;; The environment jolt's tasks run their commands in, and the two helpers
;; every task body uses: `sh` and `chez`. Loaded by deps.edn's :init.
;;
;; It matches what the Makefile gives a recipe:
;; - the Chez: an explicit CHEZ (or CHEZSCHEME) is authoritative; otherwise
;;   the pinned release patched with host/chez/chez-patches, built under
;;   .cache/local by host/chez/provision-chez.sh. A Chez on PATH is never used.
;;   Children see it as JOLT_CHEZ, which bin/jolt and host/chez/*.sh read.
;; - bash -e -o pipefail for every command line (.cache/makes/init.mk's SHELL
;;   and .SHELLFLAGS, which CI's GNU make applies; macOS's make 3.81 ignores
;;   .SHELLFLAGS, so a recipe can pass locally under make and fail here).
;; - LANG=en_US.UTF-8 and MAKES_QUIET=1 always. When the Chez is provisioned
;;   here rather than passed in, also .cache/local/bin first on PATH, TMPDIR
;;   under .cache/local/tmp and JOLT_CC, as makes' local.mk exports them; an
;;   explicit CHEZ leaves PATH and TMPDIR alone, so CI's TMPDIR=$RUNNER_TEMP
;;   holds.
(ns tasks.env
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [babashka.tasks :as t]
            [clojure.string :as str]))

(def root (str (fs/normalize (fs/absolutize "."))))

(defn getenv [k] (not-empty (System/getenv k)))

(defn sh-out
  "stdout of a command line, trimmed; nil when it fails."
  [cmd]
  (let [r (p/shell {:out :string :err :string :continue true :dir root} "sh" "-c" cmd)]
    (when (zero? (:exit r)) (str/trim (:out r)))))

(def os-name
  (let [o (str/lower-case (System/getProperty "os.name"))]
    (cond (str/includes? o "mac") "macos" (str/includes? o "win") "windows" :else "linux")))

(def explicit-chez (or (getenv "CHEZ") (getenv "CHEZSCHEME")))

(def chez-version
  (delay (or (getenv "CHEZSCHEME_VERSION")
             (sh-out "sed -n 's/^CHEZSCHEME-VERSION ?= *//p' .cache/makes/chezscheme.mk 2>/dev/null | head -n1")
             "10.4.1")))

(def chez-local (delay (str root "/.cache/local/chezscheme-" @chez-version)))

;; The compiler that builds Chez and links target/release/jolt: the host cc.
;; That is what the Makefile uses on macOS and bionic. On glibc Linux the
;; Makefile provisions makes' pinned xPack GCC instead (#788); here it is the
;; host cc there too, so pass CHEZ= and JOLT_CC= to pin both.
(def cc (delay (or (getenv "JOLT_CC") (sh-out "command -v cc") "cc")))

(defn- provision-chez! []
  (when (= "windows" os-name)
    (throw (ex-info (str "On Windows, build Chez from source with ci/apply-chez-patches.sh and pass it"
                         " as CHEZ= (see the Windows jobs in .github/workflows/tests.yml)") {})))
  (let [stamp (str @chez-local "/jolt-patches-" (sh-out "sh host/chez/provision-chez.sh --hash"))]
    (when-not (fs/exists? stamp)
      (t/shell {:dir root} "sh" "host/chez/provision-chez.sh" @chez-local @chez-version @cc))
    (str @chez-local "/bin/scheme")))

(def chez-path (delay (or explicit-chez (provision-chez!))))

(def env
  (delay
    (merge {"LANG" "en_US.UTF-8"
            "MAKES_QUIET" "1"
            "JOLT_CHEZ" @chez-path
            "JOLT_ROOT" root}
           (when-not explicit-chez
             (let [tmp (str root "/.cache/local/tmp")]
               (fs/create-dirs tmp)
               {"PATH" (str root "/.cache/local/bin:" (System/getenv "PATH"))
                "TMPDIR" tmp
                "JOLT_CC" @cc})))))

(defn gambit-env
  "GAMBIT_PREFIX, GAMBIT_GSI and GAMBIT_GSC, for the gambit tasks only: the
  default prefix asks brew, which is too slow to pay on every task."
  []
  (let [prefix (or (getenv "GAMBIT_PREFIX") (sh-out "brew --prefix gambit-scheme 2>/dev/null") "")
        abs (if (str/blank? prefix) "" (str (fs/absolutize prefix)))]
    {:env {"GAMBIT_PREFIX" prefix
           "GAMBIT_GSI" (or (getenv "GAMBIT_GSI") (str abs "/bin/gsi"))
           "GAMBIT_GSC" (or (getenv "GAMBIT_GSC") (str abs "/bin/gsc"))}}))

(defn sh
  "Run each command line in turn under bash -e -o pipefail, stopping at the
  first that fails; the task then exits with its status. An optional leading
  map's :env adds environment variables."
  [& args]
  (let [[opts lines] (if (map? (first args)) [(first args) (rest args)] [nil args])]
    (doseq [line lines]
      (t/shell {:dir root :extra-env (merge @env (:env opts))}
               "bash" "-e" "-o" "pipefail" "-c" line))))

(defn chez
  "Run the selected Chez with these arguments: (chez \"--script\" \"x.ss\")."
  [& args]
  (apply t/shell {:dir root :extra-env @env} @chez-path args))

(def jolt-bin (or (getenv "JOLT_BIN") "target/release/jolt"))
