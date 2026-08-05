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

;; --- the whole world ----------------------------------------------------------
;; The Smalltalk / Common Lisp shape: instead of naming the one value you
;; remembered to save, save the program. `dump-world!` walks the var table and
;; writes every var's root, so a restore brings back the whole of your state.
;;
;; Code does not travel. A var whose root is a function is skipped, because the
;; restoring process is the same build and already has it — every `defn`,
;; protocol impl and multimethod is there before the image is read. Only data
;; moves, which is what keeps this affordable on a runtime with no heap dump.

(defn dump-world!
  "Write every data var in the application's namespaces to PATH.

  With no NAMESPACES, dumps every namespace that is not the language's own —
  clojure.*, jolt.* and user are skipped, because their vars (*ns*, printer and
  reader settings) belong to the process being restored into, not to the image.
  Pass a seq of namespace-name strings to be explicit.

  Runs the before-dump hooks first. Vars holding functions are skipped: the
  restoring build already has the code."
  ([path] (dump-world! path nil))
  ([path namespaces] (host/image-dump-world! path namespaces)))

(defn restore-world!
  "Read a world image and rebind every var it holds. Returns the number of vars
  restored, then runs the after-restore hooks.

  Throws if PATH holds a value image rather than a world image."
  [path]
  (host/image-restore-world! path))

(defn scan-world
  "What `dump-world!` would refuse, without writing anything. Same shape as
  `scan`. Use it to find the vars holding closures before you try."
  ([] (scan-world nil))
  ([namespaces] (host/image-scan-world namespaces)))

(defn add-before-dump-hook!
  "Run F before a world dump. Where an application quiesces — stop pools, park
  worker threads, flush what should be in the image."
  [f]
  (host/image-add-before-dump-hook! f))

(defn add-after-restore-hook!
  "Run F after a world restore. Where an application rebuilds what could not be
  carried — reopen resources, re-derive computed cells, restart threads."
  [f]
  (host/image-add-after-restore-hook! f))
