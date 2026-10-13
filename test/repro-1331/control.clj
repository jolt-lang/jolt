;; Control: an UNMARKED Sleep parks a thread in C for 8 s, so a collection must
;; stall and the watchdog must catch KERNELBASE!SleepEx in the native stacks.
(load-file "repro/harness.clj")
(require '[jolt.ffi :as ffi])
(when (str/starts-with? (System/getProperty "os.name") "Windows")
  (ffi/load-library "kernel32.dll"))
(if (str/starts-with? (System/getProperty "os.name") "Windows")
  (ffi/defcfn c-sleep "Sleep" [:uint] :void)
  (ffi/defcfn c-sleep "usleep" [:uint] :int))
(run-with-heartbeat 30
  (fn [] (step "unmarked-sleep"
           (if (str/starts-with? (System/getProperty "os.name") "Windows")
             (c-sleep 8000) (c-sleep 8000000)))))
