;; System-properties gate (epic jolt-of08.4) — the JVM-standard keys a library
;; can reasonably sniff. os.arch answers in the JVM's spelling (aarch64/amd64),
;; user.name from the environment, os.version from the kernel. java.version
;; stays nil deliberately (jolt has no JDK to report — known divergence);
;; unknown keys answer nil exactly as the JVM's do.
;; Run: bin/jolt run test/chez/sysprops-test.clj (smoke.sh greps for
;; "SYSPROPS-TEST OK").
(ns sysprops-test)

(def failures (atom []))

(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

;; os.arch: the JVM's spelling for the machine we are on
(check-eq "os.arch is the JVM spelling"
          (contains? #{"aarch64" "amd64"} (System/getProperty "os.arch"))
          true)

;; user.name: the login name, as the JVM reports
(check-eq "user.name matches the environment"
          (System/getProperty "user.name")
          (or (System/getenv "USER") (System/getenv "LOGNAME") (System/getenv "USERNAME")))

;; os.version: non-empty version string (Windows has no uname/sw_vers — nil there)
(check-eq "os.version is non-empty"
          (if (= "Windows" (System/getProperty "os.name"))
            true
            (pos? (count (or (System/getProperty "os.version") ""))))
          true)

;; the properties map carries the same keys with the same values
(let [m (System/getProperties)]
  (check-eq "getProperties has os.arch" (get m "os.arch") (System/getProperty "os.arch"))
  (check-eq "getProperties has user.name" (get m "user.name") (System/getProperty "user.name"))
  (check-eq "getProperties has os.version" (get m "os.version") (System/getProperty "os.version")))

;; a set property still wins over the built-in answer, and clearing restores it
(let [orig (System/getProperty "os.arch")]
  (System/setProperty "os.arch" "vax")
  (check-eq "setProperty wins over the builtin" (System/getProperty "os.arch") "vax")
  (System/clearProperty "os.arch")
  (check-eq "clearProperty restores the builtin" (System/getProperty "os.arch") orig))

;; System/mapLibraryName follows os.name, and nil is a NullPointerException
(check-eq "mapLibraryName names the platform's library"
          (System/mapLibraryName "ffitest")
          (case (System/getProperty "os.name")
            "Mac OS X" "libffitest.dylib"
            "Windows" "ffitest.dll"
            "libffitest.so"))
(check-eq "mapLibraryName of nil is a NullPointerException"
          (try (System/mapLibraryName nil) (catch NullPointerException _ :npe))
          :npe)

;; deliberate nils stay nil (documented divergence, not an accident)
(check-eq "java.version is nil" (System/getProperty "java.version") nil)
(check-eq "an unknown key is nil" (System/getProperty "no.such.property") nil)
(check-eq "an unknown key takes the default"
          (System/getProperty "no.such.property" "fallback")
          "fallback")

(if (empty? @failures)
  (println "SYSPROPS-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "SYSPROPS-TEST FAILED:" (count @failures))))
