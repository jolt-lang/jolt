(ns ffi-smoke
  (:require [jolt.ffi :as ffi]))
(ffi/load-library "libglib-2.0.dylib")
(prn :sizeof-pointer (ffi/sizeof :pointer))
(prn :null (ffi/null))
(prn :null? (ffi/null? (ffi/null)))
(ffi/defcfn g-get-monotonic-time "g_get_monotonic_time" [] :int64)
(prn :monotonic-time (g-get-monotonic-time))
(let [p (ffi/alloc 16)]
  (ffi/write p :int64 0 42)
  (prn :wrote-and-read (ffi/read p :int64 0))
  (ffi/free p))
(prn :done)
