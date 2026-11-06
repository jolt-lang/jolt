(ns jolt.debug
  "Frames with their locals for a throwable, and a hook that runs before the
  stack unwinds.

  `frames` reads a throwable's stack, the one .getStackTrace reports, innermost
  frame first, as maps of :fn (the fn's symbol, ns/name), :file, :line (the
  line the frame was at) and :locals (a map of each local's symbol to its
  value):

    (try (my.app/run)
      (catch Throwable t (jolt.debug/frames t)))
    ;; => [{:fn my.app/inner :file \"my/app.clj\" :line 4 :locals {x 21 y 42}}
    ;;     {:fn my.app/outer ...} ...]

  A frame has every param and local in scope where it was paused when its code
  was compiled under jolt.profile's debug mode. Like the counters, the mode is a
  compile-time switch, so turn it on and then load what you want to debug:

    (require '[jolt.profile :as prof])
    (prof/instrument! :debug)
    (require 'my.app :reload)

  or set JOLT_PROFILE=debug for a whole run. Other code has only what Chez kept
  live in the frame: the locals it still reads after the call it is paused in
  returns, under their Clojure names. A frame a tail call erased is not on the
  stack, so it has none.

  Debug-compiled code makes every call except recur a real call, as the JVM
  does, which is what keeps its frames on the stack: recursion that is not
  recur runs out of stack there where it would not otherwise. The code is also
  slower, and while the mode is on namespaces load from source rather than from
  the AOT cache.

  `with-break-handler` runs its body, and when a throw escapes it, calls
  (handler throwable frames) on the throwing thread before the stack unwinds,
  then rethrows. The handler may block, for instance while a tool shows the
  frames and waits for the user. It cannot resume the throw or change a local.
  A throw a catch inside the body handles never reaches it. One that a catch
  clause there does not match does, after that try has unwound to rethrow it,
  with the frames from where it was thrown. If the handler throws, that throw
  propagates instead."
  (:require [jolt.host :as host]))

(defn- frame [[f file line locals]]
  {:fn (symbol f)
   :file file
   :line line
   :locals (into {} (map (fn [[n v]] [(symbol n) v])) locals)})

(defn frames
  "The frames of throwable t, innermost first, as maps of :fn, :file, :line and
  :locals. Empty for a throwable that has no stack."
  [t]
  (mapv frame (host/debug-frames t)))

(defn call-with-break-handler
  "Call f with no arguments and return its value. When a throw escapes f, call
  (handler throwable frames) on the throwing thread before the stack unwinds,
  then rethrow it. frames is as `frames` returns."
  [handler f]
  (host/debug-call-with-break (fn [t rows] (handler t (mapv frame rows))) f))

(defmacro with-break-handler
  "Evaluate body. When a throw escapes it, call (handler throwable frames) on
  the throwing thread before the stack unwinds, then rethrow it."
  [handler & body]
  `(call-with-break-handler ~handler (fn [] ~@body)))
