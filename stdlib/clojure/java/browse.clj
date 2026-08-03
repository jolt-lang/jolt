;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns
  ^{:author "Christophe Grand",
    :doc "Start a web browser from Clojure"}
  clojure.java.browse
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import (java.io File)
           (java.lang ProcessBuilder)))

;; Ported from Clojure's own clojure/java/browse.clj. Clojure tries the
;; platform's open-url script first and falls back to java.awt.Desktop and then
;; to a Swing window; jolt has no AWT or Swing, so browse-url is the script path
;; alone and returns nil instead of opening a window when no script is found.
;; Every platform Clojure's own fallbacks cover in practice (macOS /usr/bin/open,
;; xdg-open elsewhere) is reached by the script path, so this differs only on a
;; headless-JVM-with-a-display corner that does not exist here.

(defn- macosx? []
  (-> "os.name" System/getProperty .toLowerCase
      (.startsWith "mac os x")))

(defn- xdg-open-loc []
  ;; try/catch needed to mask exception on Windows without Cygwin
  (let [which-out (try (:out (sh/sh "which" "xdg-open"))
                       (catch Exception _ ""))]
    (if (= which-out "")
      nil
      (str/trim-newline which-out))))

(defn- open-url-script-val []
  (if (macosx?)
    "/usr/bin/open"
    (xdg-open-loc)))

;; Resolved on first use rather than at load, as in Clojure — probing for
;; xdg-open spawns a subprocess and load time should not pay for it.
(def ^:dynamic *open-url-script* (atom :uninitialized))

(defn browse-url
  "Open url in a browser"
  {:added "1.2"}
  [url]
  (let [script @*open-url-script*
        script (if (= :uninitialized script)
                 (reset! *open-url-script* (open-url-script-val))
                 script)]
    (when script
      (try
        (let [command [script (str url)]
              null-file (File. (if (.startsWith (System/getProperty "os.name") "Windows")
                                 "NUL"
                                 "/dev/null"))
              pb (doto (ProcessBuilder. command)
                   (.redirectOutput null-file)
                   (.redirectError null-file))]
          (.start pb) ;; do not wait for the process
          true)
        (catch Throwable _ false)))))
