(ns jolt.image
  "Write program state to a file and read it back, on another machine or another
  CPU architecture.

  This is a STATE image, not a process image. The value graph travels; execution
  does not. There are no thread stacks, no continuations, and nothing suspended
  mid-call comes back. See RFC 0009 for the design.

  Functions travel by NAME: a fn that is some var's root is written as the var's
  name and resolved through the var table on the way back in, so it stays
  callable. An anonymous closure has no name to write, so `dump!` refuses it and
  tells you where in the graph it lives:

      (jolt.image/dump! \"app.jimg\" {:handlers {:go (fn [x] x)}})
      ;; ExceptionInfo: image: cannot write #<procedure> at :handlers -> :go

  Use `scan` first to find those without writing anything."
  (:require [jolt.host :as host]))

(defn dump!
  "Write V to PATH as a jolt image. Returns nil.

  Throws if the graph holds anything that cannot be written — an anonymous
  closure, an open resource with no registered handler, a sorted map or set
  (their comparator machinery is not encodable in this format version). The
  message names the path through the graph to the offending object, and no file
  is written."
  [path v]
  (host/image-write! path v))

(defn read-image
  "Read the value written by `dump!` at PATH.

  Throws if PATH is missing, is not a jolt image, or was written by a different
  runtime — an image does not survive a Chez upgrade, though it does survive a
  change of machine and architecture."
  [path]
  (host/image-read path))

(defn scan
  "Dry run over V. Returns a vector of {:path :object} maps, one per object that
  `dump!` would refuse — empty when V is writable. Prefer this to a speculative
  dump when you want to know what in your state is not data."
  [v]
  (host/image-scan v))

(defn dumpable?
  "True when V can be written by `dump!`."
  [v]
  (zero? (count (scan v))))

(defn register-handler!
  "Teach the encoder about a resource it would otherwise refuse.

  PRED decides whether a value is yours. DUMP-FN turns it into plain data.
  RESTORE-FN turns that data back into a live object, and should throw if the
  data is not its own — handlers are tried in registration order and the first
  that accepts wins. The data DUMP-FN returns must be plain data; it rides in a
  part of the file that cannot carry code."
  [pred dump-fn restore-fn]
  (host/image-register-handler! pred dump-fn restore-fn))

(defn runtime-version
  "The runtime identity images are pinned to. `read-image` refuses a file whose
  recorded version differs from this. Architecture is deliberately not part of
  it."
  []
  (host/image-runtime-version))
