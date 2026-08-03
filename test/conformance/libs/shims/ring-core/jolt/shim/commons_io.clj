;; Stand-in for the Apache Commons IO JVM jar that ring-core's suite and its
;; multipart machinery import: org.apache.commons.io.IOUtils and
;; org.apache.commons.io.FileUtils. The jar does not exist on jolt, so the two
;; classes are registered with the statics the suite actually reaches.
;;
;; IOUtils/toByteArray drains a stream — jolt's in-stream carries a Chez binary
;; port and exposes readAllBytes, which is exactly this. FileUtils/writeStringToFile
;; writes a string to a file, which jolt's spit already does.
;;
;; A shim rather than an edit: changing ring's source to stop using these would
;; silently turn the recorded tally into a measure of our own code. The library
;; under test is unmodified.
(ns jolt.shim.commons-io)

;; Registering the statics is also what makes the class names resolve, so the
;; suite's (:import [org.apache.commons io IOUtils]) and
;; [org.apache.commons io FileUtils] imports succeed.
(__register-class-statics! "org.apache.commons.io.IOUtils"
  {"toByteArray" (fn [input] (.readAllBytes input))})

(__register-class-statics! "org.apache.commons.io.FileUtils"
  {"writeStringToFile" (fn [f s] (spit f s) nil)})
