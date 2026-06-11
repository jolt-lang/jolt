; Jolt Standard Library: jolt.http
; HTTP client over spork/http (janet.spork.http/*; requires `jpm install spork`).
; Responses come back as {:status int :headers map :body string}.

(defn- response->map [r]
  ;; clojure.core/get explicitly: this ns defines an http `get` that shadows it
  {:status  (clojure.core/get r :status)
   :body    (str (or (janet.spork.http/read-body r) ""))
   :headers (reduce (fn [m kv] (assoc m (str (nth kv 0)) (str (nth kv 1))))
                    {}
                    (janet/pairs (or (clojure.core/get r :headers) (janet/struct))))})

(defn- header-struct [headers]
  (apply janet/struct
         (mapcat (fn [kv] [(str (key kv)) (str (val kv))]) (seq (or headers {})))))

(defn get
  [url & {:keys [headers]}]
  (response->map
    (janet.spork.http/request "GET" url :headers (header-struct headers))))

(defn post
  [url body & {:keys [headers]}]
  (response->map
    (janet.spork.http/request "POST" url :body body :headers (header-struct headers))))
