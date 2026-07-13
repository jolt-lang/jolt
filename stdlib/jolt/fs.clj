(ns jolt.fs
  "File-system utilities. This is babashka.fs re-exported under the jolt.fs name;
  see https://github.com/babashka/fs for the API. Prefer requiring babashka.fs
  directly in new code — jolt.fs stays for backward compatibility."
  (:require [babashka.fs]))

;; Re-export every public var (functions and macros) from babashka.fs, carrying
;; its metadata so macros stay macros and docstrings/arglists survive.
(doseq [[sym v] (ns-publics 'babashka.fs)]
  (intern 'jolt.fs (with-meta sym (meta v)) @v))
