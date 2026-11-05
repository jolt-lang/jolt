(ns jolt.image
  "Write program state to a file and read it back, on another machine or another
  CPU architecture.

  This is a STATE image, not a process image. The value graph travels; execution
  does not. There are no thread stacks, no continuations, and nothing suspended
  mid-call comes back. See RFC 0009 for the design.

  Functions travel as data. A fn that is some var's root is written as the
  var's NAME and resolved on the way back in, so it stays callable and IS the
  live fn. An anonymous closure travels as its SOURCE — the fn* form plus the
  captured values, recovered from the live closure — and restore compiles it
  back in its defining namespace. That makes an image code: restoring one
  evaluates the fn sources it carries, so only load images you trust, the
  same way you would only load code you trust.

  Lazy sequences travel unforced, and stay lazy: an infinite one keeps
  generating after a restore, and a side effect that had not run still has not.
  A multimethod, a reify, and a namespace travel as their name and come back as
  the live object.

  What still refuses: a closure whose captured local was optimized into the
  compiled code (a let over compile-time constants — the message names the
  capture), a future that has not completed and a transient (both belong to a
  thread a restore does not have), a fn the runtime built rather than analyzed,
  and open resources with neither a handler nor stub mode. Use `scan` to find
  any of them without writing anything; `dump!` with {:unwritable :stub} writes
  placeholders instead of refusing."
  (:require [jolt.host :as host]))

(defn dump!
  "Write V to PATH as a jolt image. Returns nil.

  Throws if the graph holds anything that cannot be written — an open
  resource with no registered handler, or a closure whose captures were
  optimized away. The message names the path through the graph to the
  offending object, and no file is written. Pass {:unwritable :stub} to
  dump such objects as resolvable stubs instead."
  ([path v] (host/image-write! path v))
  ([path v opts]
   ;; {:unwritable :stub} dumps unhandled resources as stubs instead of
   ;; failing; the return value then reports {:stubbed [...]}
   (host/image-write! path v opts)))

(defn read-image
  "Read the value written by `dump!` at PATH.

  Throws if PATH is missing, is not a jolt image, or was written by a different
  runtime — an image does not survive a Chez upgrade, though it does survive a
  change of machine and architecture."
  [path]
  (host/image-read path))

(defn scan
  "Dry run over V. Returns a vector of {:path :object :disposition} maps, one
  per object a strict `dump!` would refuse — empty when V is writable.
  :disposition is :would-stub when stub mode would dump it as a placeholder,
  :unwritable when nothing can. Prefer this to a speculative dump when you
  want to know what in your state is not data."
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
  clojure.* and jolt.* are skipped, because their vars (*ns*, printer and
  reader settings) belong to the process being restored into, not to the image.
  `user` is kept: at a REPL it is where the work lives. Pass a seq of
  namespace-name strings to be explicit.

  Runs the before-dump hooks first. Vars holding functions are skipped: the
  restoring build already has the code.

  An open resource is written as a placeholder, and the return value lists
  each one as {:stubbed [{:id :kind :description :path :extra :var}]}, :var
  the \"ns/name\" of the var it sits in (the var its :path runs through, when
  two vars share it), so `stubs` names the same var after a restore. Pass
  {:unwritable :fail} as OPTS to refuse instead, as `dump!` does."
  ([path] (dump-world! path nil))
  ([path namespaces] (host/image-dump-world! path namespaces))
  ([path namespaces opts]
   ;; stubs by default; {:unwritable :fail} restores dump!'s strictness
   (host/image-dump-world! path namespaces opts)))

(defn restore-world!
  "Read a world image and rebind every var it holds. Returns the number of vars
  restored, then runs the after-restore hooks.

  Throws if PATH holds a value image rather than a world image."
  [path]
  (host/image-restore-world! path))

(defn scan-world
  "What `dump-world!` would refuse, without writing anything. Same shape as
  `scan`, plus :var, the \"ns/name\" of the var each finding sits in; an
  object two vars share is reported once, under the var its :path runs
  through. Use it to find the vars holding closures before you try."
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

;; --- resource stubs -----------------------------------------------------------
;; An open resource the encoder has no handler for — a port, a thread, a
;; closure whose capture was optimized away — can dump as a STUB: a
;; placeholder recording what stood there. `dump-world!` stubs by default
;; (a whole-program capture should not die on a logger's file port) and its
;; return value reports what was stubbed; `dump!` stays strict unless you
;; pass {:unwritable :stub}. On restore a stub with a registered resolver is
;; replaced inline; the rest come back as inert values that print as
;; #image/stub{...} and list through `stubs`.

(defn register-stub-describer!
  "Teach the dump side per-kind detail for stubs. PRED picks the objects;
  F gets the live object and returns a map that rides in the stub's :extra.
  Guarded: a describer that throws contributes nothing and never fails a
  dump. File ports (direction, name, open state) are built in."
  [pred f]
  (host/image-register-stub-describer! pred f))

(defn register-stub-resolver!
  "Supply the restore half for a stub kind. KIND-OR-PRED is a kind string or
  a predicate over the stub info map {:id :kind :description :path :extra};
  F gets that map and returns the live value. A matching stub never
  materializes — restore replaces it inline. Register before `read-image`
  for value images; world restores can also resolve after the fact with
  `resolve-stub!`."
  [kind-or-pred f]
  (host/image-register-stub-resolver! kind-or-pred f))

(defn stubs
  "The unresolved stubs from the last `restore-world!`, oldest id first:
  {:id :kind :description :path :extra :var}. Empty when everything either
  resolved or nothing was stubbed. Value images are not listed — their graph
  belongs to the caller; register resolvers before reading those."
  []
  (host/image-stubs))

(defn resolve-stub!
  "Replace unresolved stub ID from the last world restore with VALUE,
  rewriting the owning var's root (mutable cells patch in place). Returns
  the number of slots replaced."
  [id value]
  (host/image-resolve-stub! id value))
