;; clojure.stacktrace acceptance gate — the surface real test runners reach for
;; (kaocha's kaocha.stacktrace calls print-cause-trace to report an errored test;
;; clojure.test's own default reporter calls print-stack-trace). It loads on
;; require, so it cannot be a corpus row: the corpus runner loads no loader.
;;
;; Every expectation is checked against reference Clojure 1.12.5. The one place
;; jolt cannot match is the frame list: jolt compiles tail calls, so
;; (.getStackTrace tr) is empty where the JVM has frames, and print-stack-trace
;; reports "[empty stack trace]". That is the :trace divergence already recorded
;; in test/conformance/known-divergences.edn. Everything the runners actually
;; print — the throwable line, the ex-data, the Caused by chain — is identical,
;; and that is what these assertions pin.
;;
;; Prints the `STACKTRACE OK` / `STACKTRACE FAIL` sentinel smoke.sh greps.
(ns stacktrace-gate
  (:require [clojure.stacktrace :as st]
            [clojure.string :as str]))

(def ^:private fails (atom []))
(def ^:private passes (atom 0))

(defn- ok= [got want label]
  (if (= got want)
    (swap! passes inc)
    (swap! fails conj (str label ": want " (pr-str want) " got " (pr-str got)))))

;; --- root-cause -------------------------------------------------------------------
(ok= (.getMessage (st/root-cause (ex-info "o" {} (ex-info "m" {} (ex-info "i" {})))))
     "i" "root-cause walks to the innermost cause")
(ok= (.getMessage (st/root-cause (ex-info "only" {})))
     "only" "root-cause of an uncaused throwable is itself")

;; --- print-throwable --------------------------------------------------------------
(ok= (with-out-str (st/print-throwable (ex-info "boom" {:a 1})))
     "clojure.lang.ExceptionInfo: boom\n{:a 1}"
     "print-throwable prints class, message and ex-data")
(ok= (with-out-str (st/print-throwable (Exception. "plain")))
     "java.lang.Exception: plain"
     "print-throwable omits ex-data when there is none")
(ok= (with-out-str (st/print-throwable (Exception.)))
     "java.lang.Exception: null"
     "print-throwable renders a nil message as null")

;; --- print-stack-trace ------------------------------------------------------------
;; The frame list is empty here (tail calls); the throwable line above it is not.
(let [out (with-out-str (st/print-stack-trace (ex-info "boom" {:a 1})))]
  (ok= (first (str/split-lines out))
       "clojure.lang.ExceptionInfo: boom"
       "print-stack-trace leads with the throwable line")
  (ok= (boolean (re-find #"\[empty stack trace\]" out))
       true
       "print-stack-trace reports an empty frame list rather than throwing"))

;; --- print-cause-trace ------------------------------------------------------------
;; Drop the frame lines (indented, or the " at …" line) and what is left is the
;; cause chain, which matches the JVM exactly.
(let [out (with-out-str (st/print-cause-trace (ex-info "outer" {} (ex-info "inner" {:x 1}))))
      lines (remove #(re-find #"^\s+(at )?[a-zA-Z\[]" %) (str/split-lines out))]
  (ok= (vec lines)
       ["clojure.lang.ExceptionInfo: outer" "{}"
        "Caused by: clojure.lang.ExceptionInfo: inner" "{:x 1}"]
       "print-cause-trace walks the whole cause chain"))

(let [n @passes f @fails]
  (doseq [m f] (println "stacktrace FAIL " m))
  (println "STACKTRACE-RESULT pass" n "fail" (count f))
  (println (if (zero? (count f)) "STACKTRACE OK" "STACKTRACE FAIL"))
  (flush))
