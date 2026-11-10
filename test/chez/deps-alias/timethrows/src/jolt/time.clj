(ns jolt.time
  "Test stand-in for a jolt-lang/time install namespace that compiles but throws
  as it loads. The load error that reaches the provider is the CompilerException
  the top-level throw arrives in, whose message names only the form; the gate
  asserts the later miss carries the root cause, class and message, too.")

(throw (RuntimeException. "boom-rte"))
