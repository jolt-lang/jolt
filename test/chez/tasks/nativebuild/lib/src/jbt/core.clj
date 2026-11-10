(ns jbt.core
  (:require [jolt.ffi :as ffi]))

(ffi/defcfn answer "jbt_answer" [] :int)
