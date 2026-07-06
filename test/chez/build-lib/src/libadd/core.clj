(ns libadd.core
  (:require [jolt.ffi :as ffi]))

(defn add [x y] (+ x y))

;; Publish `add` as a C-callable entry point named "add". An embedder resolves it
;; via jolt_lookup("add") after jolt_library_init. export! runs at the library's
;; top-level (during heap build), so it is available before jolt_library_init
;; returns.
(ffi/export! "add" add [:int :int] :int)
