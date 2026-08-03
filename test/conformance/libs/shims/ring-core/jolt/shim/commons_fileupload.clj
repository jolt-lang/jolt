;; org.apache.commons.fileupload2.core over jolt-lang/multipart, for conformance
;; runs of ring.middleware.multipart-params.
;;
;; ring's multipart middleware is written directly against commons-fileupload2:
;; it proxies AbstractFileUpload, reifies RequestContext and ProgressListener,
;; iterates FileItemInput, and catches FileUploadException. None of that is
;; parsing — the parsing is RFC 7578, which jolt-lang/multipart implements. So
;; this registers the class surface ring reaches for and hands the bytes to that
;; library, leaving ring's own code paths entirely its own.
;;
;; The distinction that matters: this shim is glue, not a parser. It decides
;; nothing about multipart syntax. That is the same relationship commons-fileupload
;; has to ring on the JVM, which is what makes the suite still worth running.
(ns jolt.shim.commons-fileupload
  (:require [multipart.core :as mp]
            [clojure.string :as str]))

;; --- FileUploadException ----------------------------------------------------
;; ring catches this to turn an oversized upload into a 413, and its test asserts
;; the class by name, so it has to be a real throwable whose class reads back.
(def ^:private fue-class "org.apache.commons.fileupload2.core.FileUploadException")

(defn- file-upload-exception [msg]
  (jolt.host/throwable fue-class msg))

(jolt.host/register-class-supers! fue-class ["java.lang.Exception"])
(jolt.host/register-class-supers!
 "org.apache.commons.fileupload2.core.FileUploadByteCountLimitException"
 [fue-class])

;; --- FileItemInput ----------------------------------------------------------
;; One parsed part. multipart hands back {:name :filename :content-type :charset
;; :headerlist :bytes}; this is the getter surface ring reads off it.
(defn- make-item [part]
  (let [t (jolt.host/tagged-table :fileupload/item)]
    (jolt.host/ref-put! t :jolt.shim/type :fileupload/item)
    (jolt.host/ref-put! t :part part)
    t))

(defn- item? [v]
  (and (jolt.host/table? v)
       (= :fileupload/item (jolt.host/ref-get v :jolt.shim/type))))

(defn- item-part [v] (jolt.host/ref-get v :part))

(defn- raw-content-type [part]
  (or (some (fn [[k v]] (when (= "content-type" (str/lower-case k)) v))
            (:headerlist part))
      (:content-type part)))

;; A part is a FORM FIELD when it carries no filename — RFC 7578 §4.2, and what
;; commons-fileupload reports. ring branches on this to decide params vs files.
(defn- form-field? [part] (nil? (:filename part)))

;; keyed by the table's tag, which is what the htable method arm dispatches on
(__register-class-methods! :fileupload/item
 {"isFormField"   (fn [self] (form-field? (item-part self)))
  "getFieldName"  (fn [self] (:name (item-part self)))
  "getName"        (fn [self] (:filename (item-part self)))
  ;; The RAW header, parameters and all — commons-fileupload returns that, and
  ;; ring reads the per-part charset back out of it. multipart splits the media
  ;; type from :charset, so reading :content-type alone drops "; charset=…" and
  ;; the part decodes as the request default.
  "getContentType" (fn [self] (raw-content-type (item-part self)))
  "getInputStream" (fn [self] (java.io.ByteArrayInputStream. (:bytes (item-part self))))
  "getHeaders"     (fn [self] (:headerlist (item-part self)))})

(__register-class!
 item?
 (fn [_] "org.apache.commons.fileupload2.core.FileItemInput")
 (fn [_] ["FileItemInput" "org.apache.commons.fileupload2.core.FileItemInput"
          "java.lang.Object" "Object"]))
(jolt.host/register-class-supers!
 "org.apache.commons.fileupload2.core.FileItemInput" ["java.lang.Object"])

;; --- AbstractFileUpload -----------------------------------------------------
;; ring builds this with (proxy [AbstractFileUpload] []), so it needs a real
;; constructor: a proxy over a concrete class delegates what it does not override
;; to a base instance, and with no registered ctor there is no base and
;; .setMaxFileSize has nothing to reach.
(defn- make-upload []
  (let [t (jolt.host/tagged-table :fileupload/upload)]
    (jolt.host/ref-put! t :jolt.shim/type :fileupload/upload)
    (jolt.host/ref-put! t :state (atom {:max-file-size -1 :progress-listener nil}))
    t))

(defn- upload? [v]
  (and (jolt.host/table? v)
       (= :fileupload/upload (jolt.host/ref-get v :jolt.shim/type))))

(defn- upload-state [v] (jolt.host/ref-get v :state))

;; The boundary comes off the Content-Type, which is the context's to report.
(defn- boundary-of [content-type]
  (when content-type
    (some (fn [seg]
            (let [seg (str/trim seg)]
              (when (str/starts-with? (str/lower-case seg) "boundary=")
                (let [v (subs seg (count "boundary="))]
                  (if (and (> (count v) 1) (str/starts-with? v "\""))
                    (subs v 1 (dec (count v)))
                    v)))))
          (rest (str/split content-type #";")))))

;; The body arrives as whatever ring was handed: an InputStream, a byte array, or
;; a string. multipart wants bytes.
(defn- body-bytes [body]
  (cond
    (nil? body) (byte-array 0)
    (string? body) (.getBytes body "UTF-8")
    (instance? java.io.InputStream body) (.readAllBytes body)
    :else body))

;; commons-fileupload enforces maxFileSize DURING parsing and raises its own
;; exception; ring turns that into a 413. -1 means unlimited, and the limit is
;; inclusive — ring's suite asserts a 9-byte file passes at :max-file-size 9 and
;; fails at 8.
(defn- check-size! [parts limit]
  (when (and limit (>= limit 0))
    (doseq [p parts]
      (when (and (not (form-field? p)) (> (alength (:bytes p)) limit))
        (throw (file-upload-exception
                (str "The field " (:name p) " exceeds its maximum permitted size of "
                     limit " bytes."))))))
  parts)

(defn- get-item-iterator [self context]
  (let [content-type (.getContentType context)
        boundary (boundary-of content-type)
        _ (when-not boundary
            (throw (file-upload-exception
                    (str "the request was rejected because no multipart boundary was found"))))
        bytes (body-bytes (.getInputStream context))
        ;; :check-complete false because commons-fileupload accepts a body that
        ;; stops at the final delimiter without the closing "--", and ring's
        ;; suite asserts that tolerance. The part's bytes are complete either way.
        parts (-> (mp/parse-multipart boundary bytes {:check-complete false})
                  (check-size! (:max-file-size @(upload-state self))))
        remaining (atom (seq parts))]
    ;; A progress listener, when set, is told the total once — ring only asserts
    ;; that its fn is called, not the granularity commons-fileupload happens to use.
    (when-let [pl (:progress-listener @(upload-state self))]
      (.update pl (alength bytes) (alength bytes) (count parts)))
    (reify java.util.Iterator
      (hasNext [_] (boolean (seq @remaining)))
      (next [_]
        (if-let [s (seq @remaining)]
          (let [item (make-item (first s))]
            (reset! remaining (next s))
            item)
          (throw (java.util.NoSuchElementException. "no more items")))))))

(__register-class-ctor! "org.apache.commons.fileupload2.core.AbstractFileUpload"
                        (fn [& _] (make-upload)))
(__register-class-ctor! "AbstractFileUpload" (fn [& _] (make-upload)))

(__register-class-methods! :fileupload/upload
 {"setMaxFileSize"     (fn [self n] (swap! (upload-state self) assoc :max-file-size n) nil)
  "getMaxFileSize"     (fn [self] (:max-file-size @(upload-state self)))
  "setProgressListener" (fn [self pl] (swap! (upload-state self) assoc :progress-listener pl) nil)
  "getItemIterator"    (fn [self context] (get-item-iterator self context))})

(__register-class!
 upload?
 (fn [_] "org.apache.commons.fileupload2.core.AbstractFileUpload")
 (fn [_] ["AbstractFileUpload" "org.apache.commons.fileupload2.core.AbstractFileUpload"
          "java.lang.Object" "Object"]))
(jolt.host/register-class-supers!
 "org.apache.commons.fileupload2.core.AbstractFileUpload" ["java.lang.Object"])

;; The two interfaces ring reifies. Declaring them puts the names in the class
;; graph, which is what a reify's protocol/class dispatch canonicalizes against.
(jolt.host/register-class-supers!
 "org.apache.commons.fileupload2.core.RequestContext" ["java.lang.Object"])
(jolt.host/register-class-supers!
 "org.apache.commons.fileupload2.core.ProgressListener" ["java.lang.Object"])
(__register-class-statics! "org.apache.commons.fileupload2.core.RequestContext" {})
(__register-class-statics! "org.apache.commons.fileupload2.core.ProgressListener" {})
(__register-class-statics! fue-class {})
(__register-class-statics! "org.apache.commons.fileupload2.core.FileItemInput" {})
(__register-class-statics! "org.apache.commons.fileupload2.core.AbstractFileUpload" {})
