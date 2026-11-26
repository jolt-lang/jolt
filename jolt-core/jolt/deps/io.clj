(ns jolt.deps.io
  "The host seams jolt.deps and its git procurement share: environment,
  filesystem and subprocess calls, diagnostics on stderr, and the bounded retry
  around a network step. Split out of jolt.deps so jolt.deps.git can use them
  without requiring jolt.deps."
  (:require [clojure.string :as str]))

;; --- small host seams -------------------------------------------------------
;; An env var set to the empty string reads as UNSET — `FOO= cmd` is the usual
;; way to clear a variable for one command, and treating "" as a value would
;; turn that into "set to nothing".
(defn getenv [n] (let [v (jolt.host/getenv n)] (when-not (str/blank? v) v)))
(defn file-exists? [p] (jolt.host/file-exists? p))
(defn sh
  "Run ARGV (a program and its arguments) directly, no shell between: each
  argument reaches the program as written, with nothing expanded or split. It
  runs in the user's directory and inherits stdout/stderr. -> the exit code;
  127, as a shell answers, for a program that is not there (said on stderr)."
  [argv]
  (try
    (-> (java.lang.ProcessBuilder. ^java.util.List (vec argv))
        (.inheritIO)
        (.start)
        (.waitFor))
    (catch java.io.IOException e
      (binding [*out* *err*] (println (str "[jolt.deps] " (ex-message e))))
      127)))

;; The filesystem seams. These used to be shell commands (`mkdir -p`, `mv`,
;; `rm -f`, `rm -rf`, `test -nt`, `find`), which is a POSIX assumption jolt.host/sh
;; does not carry: on Windows it runs the string through cmd.exe, where `mkdir -p
;; a/b` creates a directory named `-p` and the rest are not commands at all. Every
;; one of these is now a filesystem call, so the only subprocess left in this
;; namespace is the one real external program, git. A jar is never extracted:
;; the loader reads it in place (jolt.host/zip-archive?, zip-entry-names).
(defn mkdirs! [p] (jolt.host/mkdirs! p))     ; mkdir -p; true if p ends up a dir
(defn rm-f [p] (jolt.host/delete-file! p))   ; absent is success
(defn rm-rf [p] (jolt.host/delete-tree! p))
(defn mv! [from to] (jolt.host/rename-file! from to))
(defn touch! [p] (spit p ""))                ; the markers are all empty files

(defn tmp-dir
  "A directory for scratch files. /tmp is not universal — Windows has no such
  path, and %TEMP% is where a temporary file goes there."
  []
  (let [d (or (getenv "TMPDIR") (getenv "TEMP") (getenv "TMP") "/tmp")]
    (str/replace d #"[/\\]+$" "")))

(defn- null-device []
  (if (str/includes? (str (System/getProperty "os.name")) "Windows") "NUL" "/dev/null"))
(defn sh-out*
  "Run ARGV directly, as `sh` does, capturing stdout and stderr separately:
  {:exit n :out s :err s}, each trimmed. stderr goes to a temp file while stdout
  is read, so a child writing a lot to both never blocks on a full pipe. A
  program that is not there answers exit 127 with the reason as :err."
  [argv]
  (let [err-f (str (tmp-dir) "/jolt-deps-" (System/currentTimeMillis) "-" (rand-int 100000) ".err")]
    (try
      (let [p (-> (java.lang.ProcessBuilder. ^java.util.List (vec argv))
                  (.redirectInput (java.io.File. ^String (null-device)))
                  (.redirectError (java.io.File. ^String err-f))
                  (.start))
            out (slurp (.getInputStream p))
            code (.waitFor p)]
        {:exit code :out (str/trim out)
         :err (when (file-exists? err-f) (str/trim (slurp err-f)))})
      (catch java.io.IOException e
        {:exit 127 :out "" :err (ex-message e)})
      (finally (rm-f err-f)))))

(defn warn [& xs] (binding [*out* *err*] (println (str "[jolt.deps] " (apply str xs)))))

(defonce ^:private warned (atom #{}))
(defn warn-once
  "warn, once per process for a given message: one command can read the same
  file more than once (its tasks, then the resolution)."
  [& xs]
  (let [msg (apply str xs)]
    (when-not (contains? (first (swap-vals! warned conj msg)) msg)
      (warn msg))))

;; --- retrying the network steps ---------------------------------------------
;; A git command that talks to a remote fails transiently more often than not — a
;; reset, a rate limit, a DNS blip — and the cost of finding out is one more
;; round trip. The HTTPS fetch already works this way (jolt/mvn_http.clj); these
;; are the git-side equivalents, and they matter for the same reason: a flake
;; here fails a whole build, and a release with it.
(def net-attempts 3)
;; Deliberately short, same reasoning as the fetch's: this sits in front of a
;; developer waiting on a build, and the failures it covers clear in well under a
;; second. A long backoff trades a rare recovery for a guaranteed stall on the
;; genuinely-unreachable case.
(def ^:private net-backoff-ms [200 600])

(defn with-net-retries
  "Call ATTEMPT until `ok?` accepts its result, at most net-attempts times, with
  a short backoff between. Returns the LAST result either way — still a failing
  one when the cap is reached, so the caller reports the failure it actually got
  rather than treating the cap as a different kind of answer."
  [ok? attempt]
  (loop [n 1]
    (let [r (attempt)]
      (if (or (ok? r) (>= n net-attempts))
        r
        (do (when-let [ms (nth net-backoff-ms (dec n) nil)] (Thread/sleep ms))
            (recur (inc n)))))))

(defn- alnum? [c]
  (let [n (int c)]
    (or (and (>= n 48) (<= n 57))     ; 0-9
        (and (>= n 65) (<= n 90))     ; A-Z
        (and (>= n 97) (<= n 122))))) ; a-z
(defn sanitize
  "S as one cache path segment: every character but [A-Za-z0-9.-] becomes _."
  [s]
  (str/join (map (fn [c] (if (or (alnum? c) (= c \.) (= c \-)) c \_)) (seq s))))
