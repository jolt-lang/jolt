(ns jolt.time
  "Test stand-in for a jolt-lang/time install namespace that IS on the source
  roots but does not compile — the shape honeysql hit when its deps.edn pinned a
  time sha older than the fix for jolt/time/zoned.clj's forward reference. The
  autoload fires, the load raises, and the class stays unregistered. What the
  gate asserts is the message: a provider that is present but broken must not be
  reported as a dependency the user forgot to declare.")

(def registered (undefined-helper-var 1))
