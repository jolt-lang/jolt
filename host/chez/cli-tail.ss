;; cli-tail.ss — shared CLI entry tail, loaded by cli.ss (source mode) and by
;; cli-devcache.ss (dev boot cache) so the two entry paths cannot drift. The
;; devcache launcher was once a heredoc that duplicated these forms and missed
;; set-source-roots! — every project command then failed to resolve sources.
(define cli-args (cdr (command-line)))   ; drop the script name, and below --no-trace

;; jolt.main + jolt.deps live under jolt-core; keep them (and stdlib) on the
;; roots so the CLI's own namespaces — and any jolt.* an app pulls in — resolve.
;; A project's resolved deps roots are prepended to these by jolt.main.
(set-source-roots! ldr-install-roots)

;; The stack depth limit (rt.ss jolt-install-stack-limit!), read from this
;; process's environment and rlimits. Here rather than beside the collector
;; policy in cli-devcache.ss so source mode, which installs no policy, has it too.
(jolt-install-stack-limit!)

;; Tracing on (JOLT_TRACE=0 or a leading --no-trace turns it off), at runtime
;; before any app ns compiles, so the app is traced.
(set! cli-args (jolt-trace-init! cli-args))

(jolt-cli-run cli-args
  ;; `build` AOT-compiles an app to a standalone binary — load the build driver
  ;; (the cross-compiler emitter) on demand so a normal run never pays for it.
  ;; It defines jolt.host/build-binary, which jolt.main's build cmd calls.
  (lambda () (load "host/chez/build.ss")))
