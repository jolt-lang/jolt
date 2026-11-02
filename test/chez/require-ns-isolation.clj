;; A require on another thread must not move THIS thread's *ns* (jolt-1gk).
;;
;; a/thread conveys the spawning thread's binding frames, and conveyance SHARES
;; them (as the JVM's Frame.clone shares its TBoxes). The loader used to write
;; the loaded file's ns into the innermost *ns* binding it could find — which,
;; on a conveyed thread, was the parent's frame — so the parent's next top-level
;; form resolved in the namespace being loaded. require-contention.clj hit it
;; on two runs in three; this pins it deterministically: the target's load
;; parks on a promise, and the parent reads its *ns* while the load is
;; mid-file. Compiler.load binds *ns* for the load, so the parent's is untouched.
;;
;;   jolt run test/chez/require-ns-isolation.clj   (from smoke.sh)
(require '[clojure.core.async :as a])

(def started (promise))
(def gate (promise))

(def root (str (System/getProperty "java.io.tmpdir") "/jolt-nsiso-" (System/nanoTime)))
(.mkdirs (java.io.File. (str root "/nsiso")))
(spit (str root "/nsiso/target.clj")
      (str "(ns nsiso.target)\n"
           "(deliver user/started (str *ns*))\n"
           "(def waited (deref user/gate 10000 :timeout))\n"))
(jolt.host/set-source-roots! (vec (cons root (jolt.host/source-roots))))

(def loader (a/thread (require 'nsiso.target) (str *ns*)))
(def inside (deref started 10000 :timeout))   ; the target is mid-load now
(def mine (str *ns*))
(deliver gate :go)
(def theirs (a/<!! loader))

(doseq [p [(str root "/nsiso/target.clj") (str root "/nsiso") root]]
  (try (.delete (java.io.File. p)) (catch Throwable _ nil)))

(let [got [inside mine theirs (str *ns*) @(resolve 'nsiso.target/waited)]
      want ["nsiso.target" "user" "user" "user" :go]]
  (if (= got want)
      (println "REQUIRE-NS-ISOLATION OK")
      (do (println "REQUIRE-NS-ISOLATION FAILED: [ns-in-load parent-mid-load loader-after parent-after waited]"
                   (pr-str got) "want" (pr-str want))
          (System/exit 1))))
