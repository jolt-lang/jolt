(ns kondo-fixture
  "Every jolt.ffi/defcfn shape, plus the scoped-allocation and callback
  macros, linted through the config exported at
  clj-kondo.exports/jolt-lang/jolt/. Not a runnable test: jolt.ffi's
  __cfn/__ccallable special forms only exist inside the compiler, so this
  file is never loaded, only linted.

  See README.md in this directory for the one command that re-runs this
  check and the output it expects: every def/defn in \"right arity\"
  resolves and lints clean, every call in \"wrong arity\" is flagged, and
  nothing outside those two groups produces a finding."
  (:require [jolt.ffi :as ffi]))

;; -- defcfn: every shape def-cfn-form accepts ---------------------------

(ffi/defcfn c-strlen "strlen" [:string] :size_t)
(ffi/defcfn c-abs "Absolute value." "abs" [:int] :int)
(ffi/defcfn c-puts "doc" {:added "1"} "puts" [:string] :int)
(ffi/defcfn c-sleep-blocking "sleep" [:uint] :uint :blocking)
(ffi/defcfn c-fail-dc "jolt_ne_fail" [:int] :int {:capture-native-error true})
(ffi/defcfn safe-strlen "strlen" [:string] :size_t
  raw-strlen [s] (if s (raw-strlen s) 0))
(ffi/defcfn c-fcntl "fcntl" [:int :int :varargs :int] :int)
(ffi/defcfn c-bare-snprintf "snprintf" [:pointer :size_t :string :&] :int)
(ffi/defcfn multi-wrapper "strlen" [:string] :size_t
  raw-multi
  ([s] (raw-multi s))
  ([s extra] (+ extra (raw-multi s))))

;; -- other public macros --------------------------------------------------

(def my-layout (ffi/layout [:struct [[:x :int] [:y :int]]]))
(def bound-cfn (ffi/cfn "strlen" [:string] :size_t))
(def bound-foreign-fn (ffi/foreign-fn "strlen" [:string] :size_t))

(defn on-click [x y] (+ x y))
(def click-callable (ffi/foreign-callable on-click [:pointer :pointer] :void))

(defn compare-int [_x _y] 0)

(defn add [x y] (+ x y))
(ffi/export! "add" add [:int :int] :int)

;; -- right arity: every call here must resolve and lint clean -------------

(defn use-plain [] (c-strlen "hello"))
(defn use-docstring [] (c-abs -3))
(defn use-attrmap [] (c-puts "hi"))
(defn use-blocking [] (c-sleep-blocking 1))
(defn use-capture [] (c-fail-dc 1))
(defn use-wrapper [] (safe-strlen "hello"))
(defn use-varargs-declared [] (c-fcntl 0 1 0))
(defn use-varargs-bare [] (c-bare-snprintf (ffi/alloc 8) 8 "hi"))
(defn use-varargs-bare-2 [] (c-bare-snprintf (ffi/alloc 8) 8 "%d" 42))
(defn use-multi-1 [] (multi-wrapper "hi"))
(defn use-multi-2 [] (multi-wrapper "hi" 3))

(defn use-with-arena []
  (ffi/with-arena [arena-1]
    (ffi/alloc arena-1 8)))

(defn use-with-alloc []
  (ffi/with-alloc [ptr-alloc 8]
    (ffi/write ptr-alloc :int 0 42)))

(defn use-with-out []
  (ffi/with-out [ptr-out :int]
    (ffi/read ptr-out :int 0)))

(defn use-with-layout []
  (ffi/with-layout [ptr-layout my-layout]
    (ffi/read ptr-layout my-layout)))

(defn use-with-c-string []
  (ffi/with-c-string [ptr-cstring "hi"]
    (ffi/address ptr-cstring)))

(defn use-with-c-string-array []
  (ffi/with-c-string-array [ptr-cstring-array 2] ["a" "b"]
    (ffi/address ptr-cstring-array)))

(defn use-callback []
  (ffi/with-arena [arena-2]
    (ffi/callback arena-2 compare-int [:pointer :pointer] :int)))

;; -- wrong arity: every call here must be flagged --------------------------

(defn wrong-plain [] (c-strlen "a" "b"))
(defn wrong-wrapper [] (safe-strlen))
(defn wrong-varargs-declared [] (c-fcntl 0 1))
(defn wrong-varargs-declared-2 [] (c-fcntl 0 1 2 3))
(defn wrong-varargs-bare [] (c-bare-snprintf (ffi/alloc 8) 8))
(defn wrong-multi [] (multi-wrapper))
(defn wrong-multi-2 [] (multi-wrapper "hi" 3 4))
