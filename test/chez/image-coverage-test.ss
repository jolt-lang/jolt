;; Every java.util collection shim travels in a state image, or is refused by
;; name. A shim whose state an image cannot write as it is (a jolt-keyed
;; hashtable, a node chain, a comparator procedure) needs a codec
;; (java/image-codecs.ss); one that is a view or an entry of another collection
;; is refused. Without this, a new shim registered with the java.util registry
;; (jutil-colls.ss) would fail an image dump with "cannot write #<hashtable> at
;; state -> 0", naming neither the class nor what to do.
;;
;; What this pins:
;;   - every tag in the java.util registry has a SAMPLE below that round-trips
;;     (same class, same contents in order), or a REFUSED sample whose dump
;;     raises naming the class
;;   - every sample's value really carries the tag it is listed under, so the
;;     tables cannot drift from the shims
;;   - every codec and refusal registered names a tag listed here
;;   chez --script test/chez/image-coverage-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (ev s) (jolt-compile-eval (string-append "(do " s ")") "user"))
(define tmp (string-append (host-temp-dir) "/jolt-image-coverage-" (number->string (get-process-id)) ".jimg"))

(ev (string-append
  "(defn image-content [v]"
  "  (cond (instance? java.util.Map v) [:map (mapv (fn [e] [(key e) (val e)]) v)]"
  "        (instance? java.util.Collection v) [:coll (vec v)]"
  "        (instance? java.util.Map$Entry v) [:entry (key v) (val v)]"
  "        :else (str v)))"
  "(defn image-round-trips? [v]"
  "  (jolt.host/image-write! \"" tmp "\" v)"
  "  (let [r (jolt.host/image-read \"" tmp "\")]"
  "    (and (= (class v) (class r)) (= (image-content v) (image-content r)))))"
  "(defn image-refuses-naming? [v cls]"
  "  (try (jolt.host/image-write! \"" tmp "\" v) false"
  "       (catch Exception e (clojure.string/includes? (ex-message e) cls))))"
  "(defn image-refuses? [v]"
  "  (try (jolt.host/image-write! \"" tmp "\" v) false"
  "       (catch Exception e (clojure.string/includes? (ex-message e) (.getName (class v))))))"))

;; tag -> a Clojure expression whose value carries the tag and must round-trip
(define samples
  '(("arraylist" . "(java.util.ArrayList. [1 :a \"s\"])")
    ("arrays-aslist" . "(java.util.Arrays/asList (object-array [1 2]))")
    ("immutable-list" . "(.toList (.stream (java.util.ArrayList. [1 2 3])))")
    ("linkedlist" . "(java.util.LinkedList. [3 1 2])")
    ("arraydeque" . "(java.util.ArrayDeque. [1 2 3])")
    ("abq" . "(doto (java.util.concurrent.ArrayBlockingQueue. 4) (.add 1) (.add 2))")
    ("lbq" . "(doto (java.util.concurrent.LinkedBlockingQueue.) (.add :x))")
    ("hashmap" . "(doto (java.util.HashMap.) (.put :b 1) (.put \"a\" [2]) (.put nil 3))")
    ("properties" . "(doto (java.util.Properties. (doto (java.util.Properties.) (.setProperty \"d\" \"0\"))) (.setProperty \"a\" \"1\"))")
    ("chm" . "(doto (java.util.concurrent.ConcurrentHashMap.) (.put :b 1) (.put \"a\" [2]))")
    ("jx-weak-hashmap" . "(doto (java.util.WeakHashMap.) (.put :b 1) (.put :a [2]))")
    ("jx-identity-hashmap" . "(doto (java.util.IdentityHashMap.) (.put :b 1) (.put :a [2]))")
    ("hashset" . "(java.util.HashSet. [3 1 :k])")
    ("linkedhashmap" . "(doto (java.util.LinkedHashMap. 16 0.75 true) (.put :b 1) (.put :a 2) (.get :b))")
    ("linkedhashset" . "(java.util.LinkedHashSet. [3 1 2])")
    ("jar-attributes" . "(doto (java.util.jar.Attributes.) (.putValue \"B\" \"two\") (.putValue \"A\" \"one\"))")
    ("treemap" . "(doto (java.util.TreeMap. (java.util.Collections/reverseOrder)) (.put 1 :a) (.put 2 :b))")
    ("treeset" . "(java.util.TreeSet. [3 1 2])")
    ("enum-set-regular" . "(java.util.EnumSet/of java.util.concurrent.TimeUnit/DAYS java.util.concurrent.TimeUnit/SECONDS)")
    ("enum-map" . "(doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/HOURS [1]))")
    ("immutable-entry" . "(.firstEntry (java.util.TreeMap. {:k 1}))")
    ("lhm-snapshot-entry" . "(.firstEntry (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("jx-unmod-coll" . "(java.util.Collections/unmodifiableCollection (java.util.ArrayList. [1 2]))")
    ("jx-unmod-seqcoll" . "(java.util.Collections/unmodifiableSequencedCollection (java.util.ArrayList. [1 2]))")
    ("jx-unmod-set" . "(java.util.Collections/unmodifiableSet (java.util.HashSet. [1 2]))")
    ("jx-unmod-seqset" . "(java.util.Collections/unmodifiableSequencedSet (java.util.LinkedHashSet. [1 2]))")
    ("jx-unmod-sortedset" . "(java.util.Collections/unmodifiableSortedSet (java.util.TreeSet. [2 1]))")
    ("jx-unmod-navset" . "(java.util.Collections/unmodifiableNavigableSet (java.util.TreeSet. [2 1]))")
    ("jx-unmod-list" . "(java.util.Collections/unmodifiableList (java.util.LinkedList. [1 2]))")
    ("jx-unmod-ralist" . "(java.util.Collections/unmodifiableList (java.util.ArrayList. [1 2]))")
    ("jx-unmod-map" . "(java.util.Collections/unmodifiableMap (doto (java.util.HashMap.) (.put :a 1)))")
    ("jx-unmod-seqmap" . "(java.util.Collections/unmodifiableSequencedMap (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("jx-unmod-sortedmap" . "(java.util.Collections/unmodifiableSortedMap (java.util.TreeMap. {1 :a}))")
    ("jx-unmod-navmap" . "(java.util.Collections/unmodifiableNavigableMap (java.util.TreeMap. {1 :a}))")
    ("jx-sync-coll" . "(java.util.Collections/synchronizedCollection (java.util.ArrayList. [1 2]))")
    ("jx-sync-set" . "(java.util.Collections/synchronizedSet (java.util.HashSet. [1 2]))")
    ("jx-sync-sortedset" . "(java.util.Collections/synchronizedSortedSet (java.util.TreeSet. [2 1]))")
    ("jx-sync-navset" . "(java.util.Collections/synchronizedNavigableSet (java.util.TreeSet. [2 1]))")
    ("jx-sync-list" . "(java.util.Collections/synchronizedList (java.util.LinkedList. [1 2]))")
    ("jx-sync-ralist" . "(java.util.Collections/synchronizedList (java.util.ArrayList. [1 2]))")
    ("jx-sync-map" . "(java.util.Collections/synchronizedMap (doto (java.util.HashMap.) (.put :a 1)))")
    ("jx-sync-sortedmap" . "(java.util.Collections/synchronizedSortedMap (java.util.TreeMap. {1 :a}))")
    ("jx-sync-navmap" . "(java.util.Collections/synchronizedNavigableMap (java.util.TreeMap. {1 :a}))")
    ("jx-listn" . "(java.util.List/of 1 2 3)")
    ("jx-list12" . "(java.util.List/of 1 2)")
    ("jx-imm-sublist" . "(.subList (java.util.List/of 1 2 3) 0 2)")
    ("jx-setn" . "(java.util.Set/of 1 2 3)")
    ("jx-set12" . "(java.util.Set/of 1)")
    ("jx-mapn" . "(java.util.Map/of :a 1 :b 2)")
    ("jx-map1" . "(java.util.Map/of :a 1)")
    ("jx-kvholder" . "(java.util.Map/entry :k 1)")
    ("jx-empty-list" . "(java.util.Collections/emptyList)")
    ("jx-singleton-list" . "(java.util.Collections/singletonList nil)")
    ("jx-copies-list" . "(java.util.Collections/nCopies 3 :a)")
    ("jx-empty-set" . "(java.util.Collections/emptySet)")
    ("jx-empty-map" . "(java.util.Collections/emptyMap)")
    ("jx-vector" . "(doto (java.util.Vector. 3 2) (.add :a) (.add 1) (.add 2) (.add 3))")
    ("jx-stack" . "(doto (java.util.Stack.) (.push 1) (.push :b))")
    ("jx-hashtable" . "(let [h (java.util.Hashtable. 5)] (doseq [i (range 12)] (.put h (str \"k\" i) i)) h)")
    ("jx-priority-queue" . "(doto (java.util.PriorityQueue. (java.util.Collections/reverseOrder)) (.addAll [3 9 1 4]))")
    ("jx-clq" . "(java.util.concurrent.ConcurrentLinkedQueue. [1 :a 2])")
    ("jx-cld" . "(doto (java.util.concurrent.ConcurrentLinkedDeque. [2 3]) (.addFirst 1))")
    ("jx-cow-list" . "(java.util.concurrent.CopyOnWriteArrayList. [3 1 3])")
    ("jx-cow-set" . "(java.util.concurrent.CopyOnWriteArraySet. [3 1 2])")
    ("jx-cslm" . "(doto (java.util.concurrent.ConcurrentSkipListMap. (java.util.Collections/reverseOrder)) (.put 1 :a) (.put 2 :b))")
    ("jx-csls" . "(java.util.concurrent.ConcurrentSkipListSet. [3 1 2])")
    ("jx-simple-entry" . "(java.util.AbstractMap$SimpleEntry. :k [1])")
    ("jx-simple-imm-entry" . "(java.util.AbstractMap$SimpleImmutableEntry. :k [1])")))
;; a JumboEnumSet needs an enum of more than 64 constants, which the host has
;; none of; its codec is the RegularEnumSet's (one registration, both tags)
(define same-codec '(("enum-set-jumbo" . "enum-set-regular")))

;; tag -> a Clojure expression whose value the image refuses, naming its class
(define refused
  '(("lhm-keyset" . "(.keySet (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("lhm-values" . "(.values (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("lhm-entryset" . "(.entrySet (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("lhm-reversed" . "(.reversed (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("lhm-entry" . "(first (.entrySet (doto (java.util.LinkedHashMap.) (.put :a 1))))")
    ("lhs-reversed" . "(.reversed (java.util.LinkedHashSet. [1]))")
    ("enum-map-keyset" . "(.keySet (doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/DAYS 1)))")
    ("enum-map-values" . "(.values (doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/DAYS 1)))")
    ("enum-map-entryset" . "(.entrySet (doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/DAYS 1)))")
    ("enum-map-entry" . "(first (.entrySet (doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/DAYS 1))))")
    ("treemap-asc-sub" . "(.headMap (java.util.TreeMap. {1 1 2 2}) 2)")
    ("treemap-desc-sub" . "(.descendingMap (java.util.TreeMap. {1 1}))")
    ("treemap-keyset" . "(.keySet (java.util.TreeMap. {1 1}))")
    ("treemap-values" . "(.values (java.util.TreeMap. {1 1}))")
    ("treemap-submap-values" . "(.values (.headMap (java.util.TreeMap. {1 1 2 2}) 2))")
    ("treemap-entryset" . "(.entrySet (java.util.TreeMap. {1 1}))")
    ("treemap-asc-entryset" . "(.entrySet (.headMap (java.util.TreeMap. {1 1 2 2}) 2))")
    ("treemap-desc-entryset" . "(.entrySet (.descendingMap (java.util.TreeMap. {1 1})))")
    ("treemap-entry" . "(first (.entrySet (java.util.TreeMap. {1 1})))")
    ("jx-unmod-entryset" . "(.entrySet (java.util.Collections/unmodifiableMap (doto (java.util.HashMap.) (.put :a 1))))")
    ("jx-unmod-entry" . "(first (.entrySet (java.util.Collections/unmodifiableMap (doto (java.util.HashMap.) (.put :a 1)))))")
    ("jx-absmap-keyset" . "(.keySet (java.util.Map/of :a 1))")
    ("jx-absmap-values" . "(.values (java.util.Map/of :a 1))")
    ("jx-imm-mapn-entryset" . "(.entrySet (java.util.Map/of :a 1 :b 2))")
    ("cd-arraylist-sublist" . "(.subList (java.util.ArrayList. [1 2 3]) 0 2)")
    ("cd-ra-sublist" . "(.subList (java.util.Arrays/asList (object-array [1 2 3])) 0 2)")
    ("cd-sublist" . "(.subList (java.util.LinkedList. [1 2 3]) 0 2)")
    ("jx-reverse-list" . "(.reversed (java.util.Vector. [1 2]))")
    ("jx-hashtable-entry" . "(first (.entrySet (java.util.Hashtable. {:a 1})))")
    ("jx-reverse-deque" . "(.reversed (java.util.concurrent.ConcurrentLinkedDeque. [1 2]))")
    ("jx-cow-sublist" . "(.subList (java.util.concurrent.CopyOnWriteArrayList. [1 2 3]) 0 2)")
    ("jx-cslm-sub" . "(.headMap (java.util.concurrent.ConcurrentSkipListMap. {1 1 2 2}) 2)")
    ("jx-cslm-dsub" . "(.descendingMap (java.util.concurrent.ConcurrentSkipListMap. {1 1}))")
    ("jx-cslm-keyset" . "(.keySet (java.util.concurrent.ConcurrentSkipListMap. {1 1}))")
    ("jx-cslm-values" . "(.values (java.util.concurrent.ConcurrentSkipListMap. {1 1}))")
    ("jx-cslm-subvalues" . "(.values (.headMap (java.util.concurrent.ConcurrentSkipListMap. {1 1 2 2}) 2))")
    ("jx-cslm-entryset" . "(.entrySet (java.util.concurrent.ConcurrentSkipListMap. {1 1}))")
    ("jx-cslm-subentryset" . "(.entrySet (.headMap (java.util.concurrent.ConcurrentSkipListMap. {1 1 2 2}) 2))")
    ("jx-cslm-dsubentryset" . "(.entrySet (.descendingMap (java.util.concurrent.ConcurrentSkipListMap. {1 1})))")
    ("jx-seq-keyset" . "(.sequencedKeySet (java.util.TreeMap. {1 1}))")
    ("jx-seq-values" . "(.sequencedValues (java.util.TreeMap. {1 1}))")
    ("jx-seq-entryset" . "(.sequencedEntrySet (java.util.TreeMap. {1 1}))")
    ("jx-reverse-linkedlist" . "(.reversed (java.util.LinkedList. [1 2]))")
    ("jx-cow-reversed" . "(.reversed (java.util.concurrent.CopyOnWriteArrayList. [1 2]))")
    ("jx-rev-sortedmap" . "(.reversed (java.util.Collections/unmodifiableSortedMap (java.util.TreeMap. {1 1})))")
    ("jx-rev-sortedset" . "(.reversed (java.util.Collections/unmodifiableSortedSet (java.util.TreeSet. [1])))")
    ("jx-hm-keyset" . "(.keySet (doto (java.util.HashMap.) (.put :a 1)))")
    ("jx-hm-values" . "(.values (doto (java.util.HashMap.) (.put :a 1)))")
    ("jx-hm-entryset" . "(.entrySet (doto (java.util.HashMap.) (.put :a 1)))")
    ("jx-hm-node" . "(first (.entrySet (doto (java.util.HashMap.) (.put :a 1))))")
    ("jx-chm-keyset" . "(.keySet (doto (java.util.concurrent.ConcurrentHashMap.) (.put :a 1)))")
    ("jx-chm-values" . "(.values (doto (java.util.concurrent.ConcurrentHashMap.) (.put :a 1)))")
    ("jx-chm-entryset" . "(.entrySet (doto (java.util.concurrent.ConcurrentHashMap.) (.put :a 1)))")
    ("jx-chm-entry" . "(first (.entrySet (doto (java.util.concurrent.ConcurrentHashMap.) (.put :a 1))))")
    ("jx-props-keyset" . "(.keySet (doto (java.util.Properties.) (.setProperty \"a\" \"1\")))")
    ("jx-props-values" . "(.values (doto (java.util.Properties.) (.setProperty \"a\" \"1\")))")
    ("jx-props-entryset" . "(.entrySet (doto (java.util.Properties.) (.setProperty \"a\" \"1\")))")
    ("jx-whm-keyset" . "(.keySet (doto (java.util.WeakHashMap.) (.put :a 1)))")
    ("jx-whm-values" . "(.values (doto (java.util.WeakHashMap.) (.put :a 1)))")
    ("jx-whm-entryset" . "(.entrySet (doto (java.util.WeakHashMap.) (.put :a 1)))")
    ("jx-whm-node" . "(first (.entrySet (doto (java.util.WeakHashMap.) (.put :a 1))))")
    ("jx-ihm-keyset" . "(.keySet (doto (java.util.IdentityHashMap.) (.put :a 1)))")
    ("jx-ihm-values" . "(.values (doto (java.util.IdentityHashMap.) (.put :a 1)))")
    ("jx-ihm-entryset" . "(.entrySet (doto (java.util.IdentityHashMap.) (.put :a 1)))")
    ("jx-ihm-node" . "(first (.entrySet (doto (java.util.IdentityHashMap.) (.put :a 1))))")))
;; tag -> a Clojure expression whose value WRAPS a value with that tag, which a
;; program only ever reaches through the wrapper (a Hashtable's keySet is a
;; Collections$SynchronizedSet over Hashtable$KeySet): the wrapper's dump is
;; refused naming the inner class, as the JDK's NotSerializableException does
(define wrapped-refused
  '(("jx-hashtable-keyset" . "(.keySet (java.util.Hashtable. {:a 1}))")
    ("jx-hashtable-values" . "(.values (java.util.Hashtable. {:a 1}))")
    ("jx-hashtable-entryset" . "(.entrySet (java.util.Hashtable. {:a 1}))")))

(define (tag-of v) (and (jhost? v) (jhost-tag v)))

;; every collection tag is covered
(let-values (((ks vs) (hashtable-entries jutil-colls-tbl)))
  (vector-for-each
    (lambda (t)
      (ok (format "java.util shim tag ~a has no image sample: add one to image-coverage-test.ss that round-trips (with a codec in java/image-codecs.ss if its state cannot travel), or a refused sample" t)
          (or (assoc t samples) (assoc t refused) (assoc t same-codec) (assoc t wrapped-refused))))
    ks))

;; the samples round-trip
(for-each
  (lambda (p)
    (let ((v (ev (cdr p))))
      (ok (format "sample for ~a carries that tag (got ~a)" (car p) (tag-of v)) (equal? (tag-of v) (car p)))
      (ok (format "~a round-trips through a state image" (car p))
          (guard (e (#t (printf "  ~a: ~a\n" (car p) (guard (_ (#t e)) (jolt-final-str (jolt-invoke (var-deref "clojure.core" "ex-message") e)))) #f))
            (jolt-truthy? (jolt-invoke (var-deref "user" "image-round-trips?") v))))))
  samples)

;; the refused samples are refused, by class name
(for-each
  (lambda (p)
    (let ((v (ev (cdr p))))
      (ok (format "refused sample for ~a carries that tag (got ~a)" (car p) (tag-of v)) (equal? (tag-of v) (car p)))
      (ok (format "~a is refused naming its class" (car p))
          (jolt-truthy? (jolt-invoke (var-deref "user" "image-refuses?") v)))))
  refused)

;; the wrapped ones: the wrapper holds a value with the tag, and is refused naming its class
(for-each
  (lambda (p)
    (let ((v (ev (cdr p))))
      (ok (format "wrapped refused sample for ~a holds that tag" (car p))
          (and (jhost? v) (equal? (tag-of (vector-ref (jhost-state v) 0)) (car p))))
      (ok (format "~a is refused naming its class" (car p))
          (jolt-truthy? (jolt-invoke (var-deref "user" "image-refuses-naming?") v
                                     (jolt-class-name (vector-ref (jhost-state v) 0)))))))
  wrapped-refused)

;; every codec and refusal registered is one this file knows about
(let-values (((ks vs) (hashtable-entries image-codec-by-tag)))
  (vector-for-each
    (lambda (t) (ok (format "codec tag ~a has a sample here" t)
                    (or (assoc t samples) (assoc t same-codec)
                        (member t '("string-builder" "string-buffer")))))
    ks))
(let-values (((ks vs) (hashtable-entries image-refusal-by-tag)))
  (ok "every refused collection view or entry has a refused sample"
      (for-all (lambda (t)
                 (or (assoc t refused) (assoc t wrapped-refused) (not (hashtable-ref jutil-colls-tbl t #f))))
               (vector->list ks))))

;; the values that are not collections but carry host state
(for-each
  (lambda (s)
    (ok (format "~a round-trips" s)
        (jolt-truthy? (jolt-invoke (var-deref "user" "image-round-trips?") (ev s)))))
  '("(StringBuilder. \"ab\")" "(StringBuffer. \"cd\")" "(doto (java.util.BitSet. 10) (.set 3) (.set 70))"))
(ok "a thread is refused naming its class"
    (jolt-truthy? (jolt-invoke (var-deref "user" "image-refuses?") (ev "(Thread. (fn []))"))))
;; a reify or proxy no var roots: its methods are this process's code
(for-each
  (lambda (s)
    (ok (format "~a is refused naming its class" s)
        (jolt-truthy? (jolt-invoke (var-deref "user" "image-refuses?") (ev s)))))
  '("(reify Runnable (run [_]))" "(proxy [Object] [] (toString [] \"p\"))" "(proxy [java.util.AbstractList] [] (get [i] i) (size [] 1))"))
(ok "a var-rooted reify still travels as its var"
    (begin
      (ev "(def image-coverage-rooted (reify Runnable (run [_])))")
      (ev (string-append "(jolt.host/image-write! \"" tmp "\" image-coverage-rooted)"))
      (jolt-truthy? (ev (string-append "(identical? image-coverage-rooted (jolt.host/image-read \"" tmp "\"))")))))
(ok "a collection holding itself restores as one"
    (begin
      (ev (string-append "(let [m (java.util.HashMap.)] (.put m :me m) (jolt.host/image-write! \"" tmp "\" m))"))
      (jolt-truthy? (ev (string-append "(let [r (jolt.host/image-read \"" tmp "\")] (identical? r (.get r :me)))")))))

(when (file-exists? tmp) (delete-file tmp))
(printf "image coverage gate: ~a/~a passed\n" (- total fails) total)
(exit (if (zero? fails) 0 1))
