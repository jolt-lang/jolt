;; Guarded persistent map path edits. #f means a receiver or key failed a
;; guard. The emitter can send the last good subtree back up the same walk
;; before resuming ordinary dispatch. The walk itself never invokes user code.

;; UUID equality and hasheq normally go through the extension registries.
;; The native registrants before the UUID handler agree with its comparison
;; and hash for the admitted key kinds. If any registry changes, do not bypass
;; it. Use the native implementations inside the walk so the probe itself
;; cannot call out even if another thread installs an arm.
(define assoc-in-native-eq-arms jolt-eq-arms)
(define assoc-in-native-hash-arms jolt-hash-arms)
(define assoc-in-native-hasheq-arms jolt-hasheq-arms)
(define (assoc-in-native-uuid?)
  (and (eq? jolt-eq-arms assoc-in-native-eq-arms)
       (eq? jolt-hash-arms assoc-in-native-hash-arms)
       (eq? jolt-hasheq-arms assoc-in-native-hasheq-arms)))

(define (assoc-in-scalar-key? k)
  (or (stable-hamt-key? k) (symbol-t? k) (string? k)))

(define (assoc-in-safe-key? k)
  (or (assoc-in-scalar-key? k) (and (juuid? k) (assoc-in-native-uuid?))))

(define (assoc-in-key=? a b)
  (cond ((eq? a b) #t)
        ((juuid? a) (and (juuid? b) (string=? (juuid-s a) (juuid-s b))))
        ((juuid? b) #f)
        (else (jolt=2 a b))))

(define (assoc-in-key-hash k)
  (if (juuid? k) (hamt-and (uuid-hashcode k) hmask) (key-hash k)))

(define assoc-in-unsafe-slot -2)
(define (assoc-in-array-index root k)
  (if (juuid? k)
      ;; Other admitted keys use kind-specialized amap-index scans. UUID
      ;; compares against every earlier slot through ordinary equality.
      (let ((n (vector-length root)))
        (let loop ((i 0))
          (if (fx=? i n)
              -1
              (let ((stored (vector-ref root i)))
                (cond ((not (assoc-in-safe-key? stored)) assoc-in-unsafe-slot)
                      ((assoc-in-key=? stored k) i)
                      (else (loop (fx+ i 2))))))))
      (amap-index root (vector-length root) k)))

(define assoc-in-unsafe-entry (list 'unsafe))
(define (assoc-in-collision-entry k entries)
  (cond ((null? entries) #f)
        ((not (assoc-in-safe-key? (caar entries))) assoc-in-unsafe-entry)
        ((assoc-in-key=? (caar entries) k) (car entries))
        (else (assoc-in-collision-entry k (cdr entries)))))

(define (assoc-in-node-edit node shift h k edit value added)
  (let* ((bit (hamt-bitpos h shift)) (bm (hnode-bm node)) (arr (hnode-arr node))
         (i (hamt-arr-index bm bit)))
    (if (hamt=? 0 (hamt-and bm bit))
        (let ((v (if edit (edit jolt-nil) value)))
          (and (or (not edit) v) (node-insert bm arr bit i k v added)))
        (let ((child (vector-ref arr i)))
          (cond
            ((hnode? child)
             (let ((r (assoc-in-node-edit child (fx+ shift 5) h k edit value added)))
               (and r (node-with-child node bm arr i child r))))
            ((hcoll? child)
             (let ((entry (assoc-in-collision-entry k (hcoll-alist child))))
               (and (not (eq? entry assoc-in-unsafe-entry))
                    (let ((v (if edit (edit (if entry (cdr entry) jolt-nil)) value)))
                      (and (or (not edit) v)
                           (node-with-child node bm arr i child (hcoll-assoc child entry k v added)))))))
            ((not (assoc-in-safe-key? (car child))) #f)
            ((assoc-in-key=? (car child) k)
             (let ((v (if edit (edit (cdr child)) value)))
               (and (or (not edit) v) (node-assoc-leaf node bm arr i child v))))
            (else
             (let ((v (if edit (edit jolt-nil) value)))
               (and (or (not edit) v)
                    (node-split-leaf bm arr i child shift h k v added assoc-in-key-hash)))))))))

;; The edit closure is compiler-generated, and may itself refuse a receiver
;; or key. It can return any Clojure value, including false, so carry success
;; separately from the value through a leaf write. At a path frame a successful
;; child is always a map.
(define (assoc-in-edit m k edit value)
  (let ((m (if (jolt-nil? m) empty-pmap m)))
    (and (pmap? m) (assoc-in-safe-key? k)
         (let ((root (pmap-root m)))
           (if (hnode? root)
               (let* ((added (box #f))
                      (r (assoc-in-node-edit root 0 (assoc-in-key-hash k) k
                           edit value added)))
                 (and r (meta-carry m (pmap-hash-write-result m root r added))))
               (let ((i (assoc-in-array-index root k)))
                 ;; Promotion can rehash unrelated keys in the array.
                 (and (not (fx=? i assoc-in-unsafe-slot))
                      (or (fx>=? i 0) (pmap-array-keep? (pmap-cnt m) k))
                      (let ((v (if edit
                                   (edit (if (fx<? i 0) jolt-nil (vector-ref root (fx+ i 1))))
                                   value)))
                        (and (or (not edit) v)
                             (meta-carry m (pmap-array-assoc-at m root i k v)))))))))))

(define (jolt-assoc-in-map-edit m k edit) (assoc-in-edit m k edit #f))
(define (jolt-assoc-in-leaf-edit m k v) (assoc-in-edit m k #f v))

;; Match the linked call's dispatch using the root the caller already read.
(define (jolt-assoc-in-apply proc m path value)
  (if (procedure? proc) (proc m path value) (jolt-invoke proc m path value)))
