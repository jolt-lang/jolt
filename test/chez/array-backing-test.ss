;; Array backings: the Chez vector type each element kind holds its elements in.
;;   chez --script test/chez/array-backing-test.ss
;; Semantics are certified by the corpus; this pins the REPRESENTATION — an
;; fxvector for int/long/short, a bytevector for byte, an flvector for
;; double/float, a STRING for char, a boxed vector for the rest — so a regression back to one boxed
;; vector for everything fails here even where every value test still passes.
;;
;; It also pins the two ways a kind's array is legitimately BOXED: a value past
;; Chez's fixnum range widens the backing (jolt's integers promote to bignums —
;; that is the numeric model, not an array rule), and an array restored from an
;; image written before the typed backings arrives boxed. Every accessor
;; dispatches on the backing rather than the kind so that both keep working, and
;; the rows below are what says so.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (evv s) (jolt-compile-eval (string-append "(do " s ")") "user"))
(define (ev s) (jolt-final-str (evv s)))
(define (is name s expect) (ok (string-append name " => " expect) (string=? (ev s) expect)))

(define (backing-of x)
  (let ((v (jolt-array-vec x)))
    (cond ((fxvector? v) 'fxvector)
          ((bytevector? v) 'bytevector)
          ((flvector? v) 'flvector)
          ((string? v) 'string)
          ((vector? v) 'vector)
          (else 'unknown))))
(define (backing name src want)
  (ok (string-append name " is " (symbol->string want) "-backed")
      (eq? want (backing-of (evv src)))))

;; --- one backing per element kind --------------------------------------------
(backing "int-array"     "(int-array 3)"                'fxvector)
(backing "long-array"    "(long-array [1 2 3])"         'fxvector)
(backing "short-array"   "(short-array 2)"              'fxvector)
(backing "byte-array"    "(byte-array [1 -2 3])"        'bytevector)
(backing "byte-array n"  "(byte-array 4)"               'bytevector)
(backing "double-array"  "(double-array [1.5])"         'flvector)
(backing "float-array"   "(float-array 2)"              'flvector)
(backing "char-array"    "(char-array 2)"               'string)
(backing "boolean-array" "(boolean-array 2)"            'vector)
(backing "object-array"  "(object-array 2)"             'vector)
;; every other way in reaches the same table
(backing "make-array"    "(make-array Long/TYPE 3)"     'fxvector)
(backing "into-array"    "(into-array Integer/TYPE [1 2])" 'fxvector)
(backing "aclone"        "(aclone (byte-array 2))"      'bytevector)
(backing "Arrays/copyOf" "(java.util.Arrays/copyOf (long-array [1]) 4)" 'fxvector)
(backing ".getBytes"     "(.getBytes \"hi\")"           'bytevector)
(backing "readAllBytes"  "(.readAllBytes (java.io.ByteArrayInputStream. (byte-array 4)))" 'bytevector)
;; a reference array is boxed whatever it came from
(backing "into-array untyped" "(into-array [1 2])"      'vector)
(backing "to-array"      "(to-array [1 2])"             'vector)
;; every door into a char array reaches the string backing
(backing "char-array str" "(char-array \"abc\")"         'string)
(backing "char-array seq" "(char-array [\\a \\b])"         'string)
(backing ".toCharArray"  "(.toCharArray \"hi\")"          'string)
(backing "aclone chars"  "(aclone (char-array \"ab\"))"    'string)
(backing "into-array chars" "(into-array Character/TYPE [\\a])" 'string)
;; an INTEGER element is the JVM's widening int->char store, so it keeps the
;; string backing rather than boxing — (aset chars 0 65) puts \A there
(backing "char-array of ints" "(char-array [65 66])"     'string)

;; --- the char backing's own widening -----------------------------------------
;; A Chez string holds CHARACTERS, so a char array handed something no character
;; denotes swaps in a boxed vector, exactly as an fxvector does for a bignum.
(ok "a non-character store widens the char backing"
    (let ((a (evv "(doto (char-array 2) (aset 0 :k))")))
      (and (eq? 'vector (backing-of a)) (eq? 'char (jolt-array-kind a)))))
(ok "a nil store widens too"
    (eq? 'vector (backing-of (evv "(doto (char-array 2) (aset 0 nil))"))))
(ok "an integer store does NOT widen"
    (eq? 'string (backing-of (evv "(doto (char-array 2) (aset 0 65) (aset 1 \\b))"))))
;; A LONE SURROGATE is a legal JVM char[] element (a char is a UTF-16 code unit)
;; but not a Chez string element (integer->char refuses that range), so it is
;; the one ordinary-looking char store that boxes.
(ok "a lone surrogate widens rather than raising"
    (eq? 'vector (backing-of (evv "(doto (char-array 2) (aset 0 0xD800))"))))
(is "widened chars: read back"
    "(let [a (char-array 3)] (aset a 0 :k) (aset a 1 \\b) [(aget a 0) (aget a 1) (count a) (alength a)])"
    "[:k \\b 3 3]")
(is "widened chars: String. still reads it"
    "(let [a (char-array 2)] (aset a 0 \\x) (aset a 1 \\y) (String. a))"
    "xy")

;; --- widening past the fixnum range ------------------------------------------
(ok "a bignum store widens the backing"
    (let ((a (evv "(doto (long-array 3) (aset 0 Long/MAX_VALUE))")))
      (and (eq? 'vector (backing-of a)) (eq? 'long (jolt-array-kind a)))))
(ok "a bignum INIT is born widened"
    (eq? 'vector (backing-of (evv "(long-array 2 Long/MAX_VALUE)"))))
(ok "a bignum in the seq is born widened"
    (eq? 'vector (backing-of (evv "(long-array [1 Long/MAX_VALUE])"))))
;; a store narrows to the kind before it is stored, so a value past the long
;; range wraps (Number.longValue) and a non-integer truncates: neither widens
(ok "a value past the long range wraps, and stays unboxed"
    (eq? 'fxvector (backing-of (evv "(long-array 2 (*' Long/MAX_VALUE 2))"))))
(ok "a non-integer store narrows, and stays unboxed"
    (eq? 'fxvector (backing-of (evv "(doto (int-array 2) (aset 0 1.5))"))))
(ok "a fixnum store does NOT widen"
    (eq? 'fxvector (backing-of (evv "(doto (long-array 2) (aset 0 -1) (aset 1 1152921504606846975))"))))
(ok "the hinted aset widens as the generic one does"
    (eq? 'vector (backing-of (evv "(let [a (long-array 2)] ((fn [^longs x ^long i v] (aset x i v)) a 0 Long/MAX_VALUE) a)"))))
(is "the hinted aset narrows as the generic one does"
    "(let [a (int-array 2)] ((fn [^ints x ^long i v] (aset x i v)) a 0 (* 3 1000000000)) (vec a))"
    "[-1294967296 0]")

;; a widened array is an ordinary array afterwards — every op reads it
(is "widened: read back" "(let [a (long-array 3)] (aset a 0 Long/MAX_VALUE) [(aget a 0) (vec a) (count a) (alength a)])"
    "[9223372036854775807 [9223372036854775807 0 0] 3 3]")
(is "widened: write past it" "(let [a (long-array 3)] (aset a 0 Long/MAX_VALUE) (aset a 1 7) (vec a))"
    "[9223372036854775807 7 0]")
(is "widened: copy / clone / sort / equals"
    "(let [a (long-array [1 2 3]) b (long-array 3)] (aset a 1 Long/MAX_VALUE) (System/arraycopy a 0 b 0 3) [(vec b) (java.util.Arrays/equals a b) (vec (aclone a)) (vec (doto (aclone a) java.util.Arrays/sort))])"
    "[[1 9223372036854775807 3] true [1 9223372036854775807 3] [1 3 9223372036854775807]]")
(is "widened: hinted aget reads it"
    "(let [a (long-array 2)] (aset a 1 Long/MAX_VALUE) ((fn [^longs x ^long i] (aget x i)) a 1))"
    "9223372036854775807")

;; --- a BOXED array of a typed kind (what a pre-backings image restores) -------
;; Built here the way the fasl reader would hand one back: the record with a
;; plain vector in it. Nothing may notice.
(define legacy-bytes (make-jolt-array (vector 1 -2 3) 'byte))
(define legacy-longs (make-jolt-array (vector 1 2) 'long))
(ok "a boxed byte array reads" (equal? '(1 -2 3) (ja->list legacy-bytes)))
(ok "a boxed byte array counts" (fx=? 3 (ja-len legacy-bytes)))
(ok "a boxed byte array writes, narrowing" (begin (na-array-set! legacy-bytes 0 200) (eqv? -56 (ja-ref legacy-bytes 0))))
(ok "a boxed byte array still answers [B" (string=? "[B" (na-array-class-name legacy-bytes)))
(ok "a boxed byte array crosses the raw-byte seam"
    (equal? '(200 254 3) (bytevector->u8-list (na-bytearray->bv legacy-bytes))))
(ok "a bytevector array copies INTO a boxed one"
    (let ((dst (make-jolt-array (vector 0 0) 'byte)))
      (ja-copy-range! (na-byte-array (jolt-vector 7 8)) 0 dst 0 2)
      (equal? '(7 8) (ja->list dst))))
(ok "equals across the two representations"
    (ja-equal? legacy-longs (na-long-array (jolt-vector 1 2))))
(ok "a boxed long array takes the hinted read" (eqv? 2 (jolt-vaget legacy-longs 1)))
;; A char array built the way a pre-string-backing image restores one: the
;; record with a plain vector in it. Every accessor dispatches on the backing,
;; so it reads, writes and renders as it always did.
(define legacy-chars (make-jolt-array (vector #\a #\b #\c) 'char))
(ok "a boxed char array reads" (equal? '(#\a #\b #\c) (ja->list legacy-chars)))
(ok "a boxed char array counts" (fx=? 3 (ja-len legacy-chars)))
(ok "a boxed char array writes" (begin (na-array-set! legacy-chars 0 #\z) (eqv? #\z (ja-ref legacy-chars 0))))
(ok "a boxed char array still answers [C" (string=? "[C" (na-array-class-name legacy-chars)))
(ok "equals across the two char representations"
    (ja-equal? (make-jolt-array (vector #\a #\b) 'char) (na-char-array "ab")))
(ok "a string-backed array copies INTO a boxed one"
    (let ((dst (make-jolt-array (vector #\space #\space) 'char)))
      (ja-copy-range! (na-char-array "hi") 0 dst 0 2)
      (equal? '(#\h #\i) (ja->list dst))))
(ok "a boxed char array copies into a STRING-backed one"
    (let ((dst (na-char-array 2)))
      (ja-copy-range! (make-jolt-array (vector #\o #\k) 'char) 0 dst 0 2)
      (equal? '(#\o #\k) (ja->list dst))))
;; the hinted accessors reach a char array only through a LYING hint (:chars is
;; not a hinted kind), and must not read a string as a vector
(ok "a lying hinted read on a string-backed char array"
    (eqv? #\b (jolt-vaget (na-char-array "ab") 1)))
(ok "a lying hinted store on a string-backed char array"
    (let ((a (na-char-array "ab")))
      (jolt-vaset a 0 #\z)
      (equal? '(#\z #\b) (ja->list a))))

;; --- byte elements stay signed, whichever door they came in -------------------
(is "byte narrowing at every entry point"
    "[(vec (byte-array [200 -1 127])) (let [a (byte-array 2)] (aset a 0 200) (aset a 1 300) (vec a)) (vec (into-array Byte/TYPE [200])) (let [a (byte-array 1)] (java.util.Arrays/fill a 200) (vec a))]"
    "[[-56 -1 127] [-56 44] [-56] [-56]]")
(is "and round-trip through a String and back"
    "(let [bs (byte-array [-1 0 127 -128 65])] (vec (.getBytes (String. bs \"ISO-8859-1\") \"ISO-8859-1\")))"
    "[-1 0 127 -128 65]")

;; --- the HINTED byte store narrows exactly as the generic one does -------------
;; (aset ^bytes a i v) has its own store target (jolt-baset): an in-range fixnum
;; goes straight into the bytevector, and everything else falls back to the
;; generic seam that narrows it. These rows are the contract that fast path has
;; to keep — a hint may not change a value, only how fast it is stored.
(is "the hinted store narrows and ANSWERS what it stored"
    "(let [a (byte-array 2) f (fn [^bytes x ^long i v] (aset x i v))] [(f a 0 200) (f a 1 -5) (vec a)])"
    "[-56 -5 [-56 -5]]")
(is "...for every value shape the generic seam accepts"
    "(let [f (fn [^bytes x ^long i v] (aset x i v)) g (fn [v] (let [a (byte-array 1)] [(f a 0 v) (aget ^bytes a 0)]))] [(g 300) (g -1.9) (g 129.7) (g (*' Long/MAX_VALUE 2))])"
    "[[44 44] [-1 -1] [-127 -127] [-2 -2]]")
(is "...and agrees with the UNHINTED store on each of them"
    "(let [h (fn [^bytes x ^long i v] (aset x i v)) g (fn [f v] (let [a (byte-array 1)] (f a 0 v) (aget ^bytes a 0)))] (mapv (fn [v] [(g h v) (g (fn [a i x] (aset a i x)) v)]) [200 300 -5 -1.9 129.7 -128 127]))"
    "[[-56 -56] [44 44] [-5 -5] [-1 -1] [-127 -127] [-128 -128] [127 127]]")
(is "a flonum INDEX still floors on the hinted store"
    "(let [a (byte-array 3)] ((fn [^bytes x i] (aset x i 9)) a 2.7) (vec a))"
    "[0 0 9]")
(is "a LYING ^bytes hint stores through the kind the array really has"
    "(let [l (long-array 2) o (object-array 2) f (fn [^bytes x ^long i v] (aset x i v))] [(f l 0 1000) (vec l) (f o 0 :k) (vec o)])"
    "[1000 [1000 0] :k [:k nil]]")
(is "an out-of-range hinted store is the ARRAY exception, either direction"
    "[(try ((fn [^bytes x ^long i] (aset x i 1)) (byte-array 2) 9) (catch ArrayIndexOutOfBoundsException e :aioobe)) (try ((fn [^bytes x ^long i] (aset x i 1)) (byte-array 2) -1) (catch ArrayIndexOutOfBoundsException e :aioobe))]"
    "[:aioobe :aioobe]")
;; a byte array whose backing is BOXED — an older image's shape — cannot take the
;; bytevector store, and must still narrow through the seam it falls back to.
(let ((boxed (make-jolt-array (vector 0 0 0) 'byte)))
  (ok "a boxed byte backing narrows through the store helper"
      (and (eqv? -56 (jolt-baset boxed 0 200))
           (eqv? 7 (jolt-baset boxed 1 7))
           (equal? '(-56 7 0) (ja->list boxed)))))

;; --- the block moves ----------------------------------------------------------
(is "an overlapping arraycopy reads pre-copy values, both directions and both backings"
    "(let [f (fn [a] (System/arraycopy a 0 a 1 4) (vec a)) b (fn [a] (System/arraycopy a 1 a 0 4) (vec a))] [(f (byte-array [1 2 3 4 5])) (b (byte-array [1 2 3 4 5])) (f (long-array [1 2 3 4 5])) (b (long-array [1 2 3 4 5]))])"
    "[[1 1 2 3 4] [2 3 4 5 5] [1 1 2 3 4] [2 3 4 5 5]]")
(is "a stream read fills a byte-array region"
    "(let [in (java.io.ByteArrayInputStream. (byte-array [10 20 30 40 50])) buf (byte-array 8)] [(.read in buf 2 4) (vec buf)])"
    "[4 [0 0 10 20 30 40 0 0]]")
(is "a ByteBuffer bulk get/put moves bytes"
    "(let [src (java.nio.ByteBuffer/wrap (byte-array [1 2 3 4])) dst (java.nio.ByteBuffer/allocate 4) out (byte-array 4)] (.put dst src) (.rewind dst) (.get dst out) (vec out))"
    "[1 2 3 4]")

;; --- an out-of-range hinted read is the ARRAY exception ----------------------
(is "the hinted reads classify their own range error"
    "[(try ((fn [^longs a ^long i] (aget a i)) (long-array 2) 9) (catch ArrayIndexOutOfBoundsException e :aioobe)) (try ((fn [^bytes a ^long i] (aget a i)) (byte-array 2) 9) (catch ArrayIndexOutOfBoundsException e :aioobe)) (try ((fn [^doubles a ^long i] (aget a i)) (double-array 2) 9) (catch ArrayIndexOutOfBoundsException e :aioobe))]"
    "[:aioobe :aioobe :aioobe]")
;; ...including the BOXED ones, which pre-check because vector-ref's condition
;; cannot be told from any other vector in the runtime. The hint must not decide
;; which exception class a program catches: the same reads without it, and a
;; widened long array, answer the same way.
(is "a boxed hinted read/write classifies too"
    "[(try ((fn [^objects a ^long i] (aget a i)) (object-array 2) 9) (catch ArrayIndexOutOfBoundsException e :aioobe)) (try ((fn [^objects a ^long i] (aset a i 1)) (object-array 2) 9) (catch ArrayIndexOutOfBoundsException e :aioobe)) (try (aget (object-array 2) 9) (catch ArrayIndexOutOfBoundsException e :aioobe)) (try (let [a (long-array 2)] (aset a 0 Long/MAX_VALUE) ((fn [^longs x ^long i] (aget x i)) a 9)) (catch ArrayIndexOutOfBoundsException e :aioobe))]"
    "[:aioobe :aioobe :aioobe :aioobe]")

;; --- (T-array size init): the scalar fill and the SEQ form (jolt-6g1) -----------
;; Numbers.T_array branches on an instanceof: an init that is the element's own
;; scalar type fills every slot, anything else is a seq that fills a PREFIX and
;; leaves the tail at the element default. jolt had only the fill, so a seq init
;; was stored AS the element — (int-array 3 [1 2]) was three slots each holding
;; the vector — and char-array read no init at all. Every row below was compared
;; against reference JVM Clojure; the backing rows say the fix did not cost the
;; typed representation the rest of this file pins.
(is "char-array fills from a scalar init" "(mapv int (char-array 3 \\.))"        "[46 46 46]")
(is "char-array takes a seq prefix"       "(mapv int (char-array 3 [\\a \\b]))"  "[97 98 0]")
(is "char-array seqs a string init"       "(mapv int (char-array 3 \"ab\"))"     "[97 98 0]")
(is "a nil init is an empty seq"          "(mapv int (char-array 3 nil))"        "[0 0 0]")
(is "a seq longer than the array stops"   "(mapv int (char-array 2 [\\a \\b \\c \\d]))" "[97 98]")
(is "char-array 1-arg is unchanged"       "(mapv int (char-array 3))"            "[0 0 0]")
(is "int-array takes a seq prefix"        "(vec (int-array 3 [1 2]))"            "[1 2 0]")
(is "int-array still fills from a scalar" "(vec (int-array 3 7))"                "[7 7 7]")
(is "long-array takes a seq prefix"       "(vec (long-array 3 [1 2]))"           "[1 2 0]")
(is "byte-array takes a seq prefix"       "(vec (byte-array 3 [1 2]))"           "[1 2 0]")
(is "byte-array nil init"                 "(vec (byte-array 3 nil))"             "[0 0 0]")
(is "byte-array truncates a long seq"     "(vec (byte-array 2 [1 2 3 4]))"       "[1 2]")
(is "boolean-array takes a seq prefix"    "(vec (boolean-array 3 [true false]))" "[true false false]")
(is "double-array takes a seq prefix"     "(vec (double-array 3 [1.5]))"         "[1.5 0.0 0.0]")
(is "float-array takes a seq prefix"      "(vec (float-array 3 [1.5]))"          "[1.5 0.0 0.0]")
;; A scalar byte init still narrows the way na-byte-of narrows everywhere else.
;; The JVM raises here instead (Numbers.byte_array wants an instanceof Byte, so a
;; Long literal falls to its seq branch) — the :permissive divergence already on
;; record for the byte store, recorded again for this door in known-divergences.
(is "a scalar byte init narrows"          "(vec (byte-array 3 200))"             "[-56 -56 -56]")
;; The seq form must not box a typed backing: it packs through na-list->backing,
;; the same door (T-array coll) uses, so the representation is the kind's own.
(backing "char-array with a scalar init"  "(char-array 3 \\.)"       'string)
(backing "char-array with a seq init"     "(char-array 3 [\\a \\b])" 'string)
(backing "byte-array with a seq init"     "(byte-array 3 [1 2])"     'bytevector)
(backing "int-array with a seq init"      "(int-array 3 [1 2])"      'fxvector)
(backing "double-array with a seq init"   "(double-array 3 [1.5])"   'flvector)

;; (T-array coll) from a vector or another array fills the backing straight from
;; the indexed elements (na-from-indexed) instead of draining a seq. Same
;; answers as the seq route: every element coerced by the kind's rule, the
;; integer kinds falling back to a boxed backing for a value past the fixnum
;; range, and a vector walked across leaves, the tail and relaxed (RRB) nodes.
(is "double-array from a long vector"     "(vec (double-array [1 2 3]))"           "[1.0 2.0 3.0]")
(is "double-array from a ratio vector"    "(vec (double-array [1/2 3/4]))"         "[0.5 0.75]")
(is "long-array from a double vector"     "(vec (long-array [1.9 -1.9]))"          "[1 -1]")
(is "int-array from a double vector"      "(vec (int-array [3.7 1e20]))"           "[3 2147483647]")
(is "short-array wraps from a vector"     "(vec (short-array [40000]))"            "[-25536]")
(is "float-array rounds from a vector"    "(= (aget (float-array [0.1]) 0) (float 0.1))" "true")
(is "double-array from a 1000 vector"     "(let [v (vec (range 1000))] (= (map double v) (vec (double-array v))))" "true")
(is "double-array from a subvec"          "(vec (double-array (subvec (vec (range 100)) 37 41)))" "[37.0 38.0 39.0 40.0]")
(is "double-array from a catvec"          "(let [v (into (subvec (vec (range 70)) 3) (range 70 140))] (= (map double v) (vec (double-array v))))" "true")
(is "long-array from a catvec"            "(let [v (into (subvec (vec (range 70)) 3) (range 70 140))] (= v (vec (long-array v))))" "true")
(is "double-array from an empty vector"   "(alength (double-array []))"            "0")
(is "double-array from a double-array"    "(vec (double-array (double-array [1.5 2.5])))" "[1.5 2.5]")
(is "double-array from a long-array"      "(vec (double-array (long-array [1 2])))" "[1.0 2.0]")
(is "double-array from a float-array"     "(= (vec (double-array (float-array [0.1]))) [(double (float 0.1))])" "true")
(is "float-array from a double-array"     "(= (aget (float-array (double-array [0.1])) 0) (float 0.1))" "true")
(is "long-array from a double-array"      "(vec (long-array (double-array [2.9 -2.9])))" "[2 -2]")
(is "double-array from a byte-array"      "(vec (double-array (byte-array [-1 2])))" "[-1.0 2.0]")
(is "long-array keeps a bignum, boxed"    "(vec (long-array [1 9223372036854775807]))" "[1 9223372036854775807]")
(is "a nil element still raises"          "(try (double-array [1.0 nil]) :no (catch Throwable _ :raised))" ":raised")
(is "the source array is not shared"      "(let [a (double-array [1.0]) b (double-array a)] (aset a 0 9.0) (aget b 0))" "1.0")
(backing "double-array from a vector"     "(double-array [1 2])"                   'flvector)
(backing "double-array from an array"     "(double-array (long-array [1 2]))"      'flvector)
(backing "long-array from a vector"       "(long-array [1.5 2])"                   'fxvector)
(backing "int-array from an array"        "(int-array (double-array [1.5]))"       'fxvector)
(backing "long-array past fixnum range"   "(long-array [1 9223372036854775807])"   'vector)

(printf "array-backing-test: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
