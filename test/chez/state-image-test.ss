;; State image round-trip regression. Run:
;;   chez --script test/chez/state-image-test.ss
;;
;; Pins the two things the image format rests on: that a jolt value graph
;; survives write -> read unchanged (including sharing, cycles and every numeric
;; type), and that everything Chez's fasl refuses is either encoded as data or
;; refused with the path to it. Also pins the Chez behaviour the design assumes,
;; so a Chez upgrade that changes fasl fails here rather than in someone's image.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (ev s) (jolt-final-str (jolt-compile-eval (string-append "(do " s ")") "user")))
(define (is name s expect)
  (let ((got (ev s)))
    (set! total (+ total 1))
    (unless (string=? got expect)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a\n  expected: ~a\n  actual:   ~a\n" name expect got))))

(define tmp (string-append "/tmp/jolt-image-test-" (number->string (fx+ 1 (random 100000))) ".jimg"))
(define (cleanup!) (when (file-exists? tmp) (delete-file tmp)))

;; --- Chez substrate the format depends on ------------------------------------
;; If any of these change, the encoding assumptions change with them.
(define (fasl-bytes obj . pred)
  (call-with-bytevector-output-port
    (lambda (p) (if (null? pred) (fasl-write obj p) (fasl-write obj p (car pred))))))

(ok "data-only fasl is machine-independent (byte 12 = 0)"
    (fx=? 0 (bytevector-u8-ref (fasl-bytes (list 1 "two" 'three 4.0)) 12)))
(ok "records/cycles/bignums stay machine-independent"
    (and (fx=? 0 (bytevector-u8-ref (fasl-bytes (list (expt 2 200) -0.0 +nan.0 1/3)) 12))
         (fx=? 0 (bytevector-u8-ref (fasl-bytes (let ((v (vector 1))) (list v v))) 12))))
(ok "fasl-write refuses procedures"
    (call/cc (lambda (k) (with-exception-handler (lambda (e) (k #t))
                           (lambda () (fasl-bytes car) #f)))))
(ok "fasl-write refuses non-eq hashtables"
    (call/cc (lambda (k) (with-exception-handler (lambda (e) (k #t))
                           (lambda () (fasl-bytes (make-hashtable equal-hash equal?)) #f)))))
(ok "eq hashtables fasl fine"
    (let ((h (make-eq-hashtable)))
      (hashtable-set! h 'a 1)
      (not (not (fasl-bytes h)))))
(ok "externals vector length mismatch fails loudly"
    (call/cc (lambda (k) (with-exception-handler (lambda (e) (k #t))
                           (lambda ()
                             (fasl-read (open-bytevector-input-port (fasl-bytes (list car) procedure?))
                                        'load (vector))
                             #f)))))

;; --- value round-trip ---------------------------------------------------------
(define (roundtrip-expr expr)
  ;; write EXPR's value to the image, read it back, print it
  (cleanup!)
  (ev (string-append "(jolt.host/image-write! \"" tmp "\" " expr ")"))
  (ev (string-append "(pr-str (jolt.host/image-read \"" tmp "\"))")))

(define (rt name expr expect)
  (let ((got (roundtrip-expr expr)))
    (set! total (+ total 1))
    (unless (string=? got expect)
      (set! fails (+ fails 1))
      (printf "FAIL: round-trip ~a\n  expected: ~a\n  actual:   ~a\n" name expect got))))

(rt "vector"      "[1 2 3]"                        "[1 2 3]")
(rt "map"         "{:a 1 :b 2}"                    "{:a 1, :b 2}")
(rt "nested"      "{:xs [1 {:y #{3}}] :s \"hi\"}"  "{:xs [1 {:y #{3}}], :s \"hi\"}")
(rt "set"         "#{1 2 3}"                       "#{1 3 2}")
(rt "list"        "'(1 2 3)"                       "(1 2 3)")
(rt "keywords"    "[:a :b/c]"                      "[:a :b/c]")
(rt "symbols"     "'[a b/c]"                       "[a b/c]")
(rt "nil/bool"    "[nil true false]"               "[nil true false]")
(rt "strings"     "[\"a\" \"\"]"                   "[\"a\" \"\"]")
(rt "chars"       "[\\a \\newline]"                "[\\a \\newline]")
(rt "ratio"       "(/ 1 3)"                        "1/3")
(rt "bigint"      "(* 99999999999 99999999999)"    "9999999999800000000001N")
(rt "double"      "[1.5 -0.0]"                     "[1.5 -0.0]")
(rt "empty colls" "[[] {} #{} ()]"                 "[[] {} #{} ()]")
(rt "record"      "(do (defrecord P [x y]) (->P 1 2))" "#user.P{:x 1, :y 2}")

;; Sorted maps/sets keep their comparator machinery in a Chez hashtable, which
;; this format version does not encode. The point of the assertion is that it is
;; REFUSED clearly, not that it crashes somewhere in fasl.
(is "sorted map is refused, not crashed"
    (string-append "(try (jolt.host/image-write! \"" tmp "\" (sorted-map :b 2 :a 1)) :no-throw"
                   " (catch Exception e (if (re-find #\"cannot write\" (ex-message e)) :refused :wrong-error)))")
    ":refused")

;; deep + wide, to catch anything that only shows up past the small-map cutoff
(is "large map round-trips"
    (string-append "(do (jolt.host/image-write! \"" tmp "\" (zipmap (range 200) (range 200)))"
                   " (= (zipmap (range 200) (range 200)) (jolt.host/image-read \"" tmp "\")))")
    "true")
(is "large vector round-trips"
    (string-append "(do (jolt.host/image-write! \"" tmp "\" (vec (range 5000)))"
                   " (= (vec (range 5000)) (jolt.host/image-read \"" tmp "\")))")
    "true")

;; metadata rides along
(is "metadata preserved"
    (string-append "(do (jolt.host/image-write! \"" tmp "\" (with-meta [1] {:m 1}))"
                   " (:m (meta (jolt.host/image-read \"" tmp "\"))))")
    "1")

;; structural sharing: one object referenced twice stays one object
(is "sharing preserved"
    (string-append "(do (def shared {:a 1}) (jolt.host/image-write! \"" tmp "\" [shared shared])"
                   " (let [r (jolt.host/image-read \"" tmp "\")]"
                   " (identical? (first r) (second r))))")
    "true")

;; --- functions travel as var names --------------------------------------------
(is "named core fn round-trips and stays callable"
    (string-append "(do (jolt.host/image-write! \"" tmp "\" {:f inc})"
                   " ((:f (jolt.host/image-read \"" tmp "\")) 41))")
    "42")
(is "user-defined fn round-trips"
    (string-append "(do (defn dbl [x] (* 2 x)) (jolt.host/image-write! \"" tmp "\" [dbl])"
                   " ((first (jolt.host/image-read \"" tmp "\")) 21))")
    "42")

;; a bare closure has no name to write -> refused, with the path to it
(is "anonymous closure is refused"
    (string-append "(try (jolt.host/image-write! \"" tmp "\" {:handlers {:go (fn [x] x)}}) :no-throw"
                   " (catch Exception e (if (re-find #\"cannot write\" (ex-message e)) :refused :wrong-error)))")
    ":refused")
(is "refusal names the path"
    (string-append "(try (jolt.host/image-write! \"" tmp "\" {:handlers {:go (fn [x] x)}}) \"\""
                   " (catch Exception e (if (re-find #\":handlers\" (ex-message e)) :has-path :no-path)))")
    ":has-path")

;; --- scan reports without writing ----------------------------------------------
(is "scan is empty for pure data"
    "(count (jolt.host/image-scan {:a [1 2] :b #{3}}))" "0")
(is "scan finds the closure"
    "(count (jolt.host/image-scan {:f (fn [x] x)}))" "1")
(is "scan reports a path"
    "(-> (jolt.host/image-scan {:outer {:inner (fn [x] x)}}) first :path string? )" "true")
(is "scan is empty for a named fn"
    "(count (jolt.host/image-scan {:f inc}))" "0")

;; --- header / compatibility ------------------------------------------------------
(is "reading a non-image fails with a clear message"
    (string-append "(do (spit \"" tmp ".txt\" \"not an image\")"
                   " (try (jolt.host/image-read \"" tmp ".txt\") :no-throw"
                   " (catch Exception e :threw)))")
    ":threw")
(is "reading a missing file names the file"
    "(try (jolt.host/image-read \"/tmp/definitely-not-here.jimg\") :no-throw (catch Exception e (if (re-find #\"no such file\" (ex-message e)) :named :unnamed)))"
    ":named")
(is "runtime version is reported"
    "(string? (jolt.host/image-runtime-version))" "true")

;; --- interned-identity regression -----------------------------------------------
;; A fasl copy of a keyword is a key nothing can find: the map prints and counts
;; correctly while every lookup returns nil and = is false. That shape of bug is
;; invisible to a pr-str assertion, so it gets its own block.
(define (rt-expr expr) (string-append "(do (jolt.host/image-write! \"" tmp "\" " expr ")"
                                      " (jolt.host/image-read \"" tmp "\"))"))

(is "keyword lookup works after restore"     (string-append "(:m " (rt-expr "{:m 1 :n 2}") ")") "1")
(is "get works after restore"                (string-append "(get " (rt-expr "{:m 1}") " :m)") "1")
(is "= holds after restore"                  (string-append "(= {:m 1 :n 2} " (rt-expr "{:m 1 :n 2}") ")") "true")
(is "keywords are re-interned (identical?)"  (string-append "(identical? :m (first (keys " (rt-expr "{:m 1}") ")))") "true")
(is "namespaced keywords re-intern"          (string-append "(identical? :a/b (first (keys " (rt-expr "{:a/b 1}") ")))") "true")
(is "nested keyword lookup"                  (string-append "(get-in " (rt-expr "{:a {:b {:c 7}}}") " [:a :b :c])") "7")
(is "keyword set membership"                 (string-append "(contains? " (rt-expr "#{:x :y}") " :x)") "true")
(is "record field access after restore"
    (string-append "(do (defrecord Q [a b]) (:a " (rt-expr "(->Q 5 6)") "))") "5")
(is "record = after restore"
    (string-append "(do (defrecord R [a]) (= (->R 1) " (rt-expr "(->R 1)") "))") "true")
;; symbols are not interned and compare by ns/name, so a copy must still work as a key
(is "symbol-keyed lookup works"              (string-append "(get " (rt-expr "{'a 1}") " 'a)") "1")
(is "string-keyed lookup works"              (string-append "(get " (rt-expr "{\"s\" 1}") " \"s\")") "1")
(is "integer-keyed lookup works"             (string-append "(get " (rt-expr "{7 1}") " 7)") "1")
(is "large keyword map keeps every lookup"
    (string-append "(let [m " (rt-expr "(zipmap (map #(keyword (str \"k\" %)) (range 300)) (range 300))")
                   "] (every? (fn [i] (= i (get m (keyword (str \"k\" i))))) (range 300)))")
    "true")
(is "keyword sharing collapses to one intern"
    (string-append "(let [m " (rt-expr "[{:k 1} {:k 2}]")
                   "] (identical? (first (keys (first m))) (first (keys (second m)))))")
    "true")

;; cycles: fasl handles them, so an atom pointing at itself must survive
(is "cyclic structure survives"
    (string-append "(do (def a (atom nil)) (reset! a {:self a})"
                   " (jolt.host/image-write! \"" tmp "\" @a)"
                   " (let [r (jolt.host/image-read \"" tmp "\")]"
                   " (identical? r (deref (:self r)))))")
    "true")


;; --- whole-world image -----------------------------------------------------------
;; The Smalltalk/CL shape: save the program, not one named value. Code is skipped
;; (the restoring build already has it), so only data moves.
(define world-tmp (string-append tmp ".world"))
(ev "(ns imgtest.app)")
(jolt-compile-eval "(def board {:tasks [{:id 1 :text \"a\"}] :filter :all})" "imgtest.app")
(jolt-compile-eval "(def counter 41)" "imgtest.app")
(jolt-compile-eval "(defn helper [x] (* 2 x))" "imgtest.app")

(is "world scan is clean for data-only namespaces"
    "(count (jolt.host/image-scan-world [\"imgtest.app\"]))" "0")
(ev (string-append "(jolt.host/image-dump-world! \"" world-tmp "\" [\"imgtest.app\"])"))
;; clobber the world
(jolt-compile-eval "(def board {:tasks [] :filter :done})" "imgtest.app")
(jolt-compile-eval "(def counter 0)" "imgtest.app")
(is "world restore reports how many vars it rebound"
    (string-append "(jolt.host/image-restore-world! \"" world-tmp "\")") "2")
(is "data vars come back"
    "(str (:filter imgtest.app/board) \" \" imgtest.app/counter)" ":all 41")
(is "restored data keeps working keyword lookup"
    "(count (:tasks imgtest.app/board))" "1")
(is "code vars are untouched by a restore"
    "(imgtest.app/helper 21)" "42")
(is "a value image is refused by restore-world!"
    (string-append "(do (jolt.host/image-write! \"" tmp "\" {:a 1})"
                   " (try (jolt.host/image-restore-world! \"" tmp "\") :no-throw"
                   " (catch Exception e (if (re-find #\"value image\" (ex-message e)) :named :other))))")
    ":named")
(is "a world image is refused by plain read"
    (string-append "(let [w (jolt.host/image-read \"" world-tmp "\")] (vector? w))") "false")
;; hooks fire in order around the operation
(jolt-compile-eval "(def hooklog (atom []))" "user")
(is "before-dump and after-restore hooks fire"
    (string-append "(do (jolt.host/image-add-before-dump-hook! (fn [] (swap! user/hooklog conj :before)))"
                   " (jolt.host/image-add-after-restore-hook! (fn [] (swap! user/hooklog conj :after)))"
                   " (jolt.host/image-dump-world! \"" world-tmp "\" [\"imgtest.app\"])"
                   " (jolt.host/image-restore-world! \"" world-tmp "\")"
                   " (pr-str @user/hooklog))")
    "[:before :after]")
(when (file-exists? world-tmp) (delete-file world-tmp))

(cleanup!)
(when (file-exists? (string-append tmp ".txt")) (delete-file (string-append tmp ".txt")))

(printf "~a/~a state-image assertions passed\n" (- total fails) total)
(when (> fails 0) (exit 1))
