;; Guarded persistent map path edits. #f means a receiver or key failed a
;; guard. The emitter can send the last good subtree back up the same walk
;; before resuming ordinary dispatch. The walk itself never invokes user code.

;; UUID equality and hasheq normally go through the extension registries.
;; Capture the native lists during rt.ss, before application or library code.
;; A shim can be present here in an eager boot or add arms later in a lazy
;; boot. Any later change makes the UUID guard decline. Keep these immutable
;; snapshots outside the -arms naming convention for live dispatch lists.
;; Use native implementations inside the walk so it cannot call out for a UUID
;; comparison or hash even if another thread installs an arm.
(define assoc-in-native-eq-snapshot jolt-eq-arms)
(define assoc-in-native-hash-snapshot jolt-hash-arms)
(define assoc-in-native-hasheq-snapshot jolt-hasheq-arms)
(define (assoc-in-native-uuid?)
  (and (eq? jolt-eq-arms assoc-in-native-eq-snapshot)
       (eq? jolt-hash-arms assoc-in-native-hash-snapshot)
       (eq? jolt-hasheq-arms assoc-in-native-hasheq-snapshot)))

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

;; One level of the walk over the node layout (collections.ss hnode): an
;; entry's key/value slot pair under dmap, a branch under nmap. Each write
;; mirrors node-assoc-by's, with the stored key hashed by assoc-in-key-hash.
(define (assoc-in-node-edit node shift h k edit value added)
  (let* ((bit (hamt-bitpos h shift)) (dm (hnode-dmap node)) (nm (hnode-nmap node))
         (arr (hnode-arr node)))
    (cond
      ((not (hamt=? 0 (hamt-and dm bit)))
       (let* ((i (fx* 2 (hamt-arr-index dm bit))) (ks (vector-ref arr i)))
         (cond
           ((not (assoc-in-safe-key? ks)) #f)
           ((assoc-in-key=? ks k)
            (let* ((j (fx+ i 1)) (old (vector-ref arr j))
                   (v (if edit (edit old) value)))
              (and (or (not edit) v)
                   (if (eq? old v) node (make-hnode dm nm (vec-set arr j v))))))
           (else
            (let ((v (if edit (edit jolt-nil) value)))
              (and (or (not edit) v)
                   (let ((b (split-hashed-leaves (fx+ shift 5) (assoc-in-key-hash ks) ks
                                                 (vector-ref arr (fx+ i 1)) h k v)))
                     (set-box! added #t)
                     (make-hnode (hamt-and dm (hamt-not bit)) (hamt-ior nm bit)
                                 (vec-entry->branch arr i (fx+ (hamt-branch-slot arr nm bit) 1) b)))))))))
      ((not (hamt=? 0 (hamt-and nm bit)))
       (let* ((j (hamt-branch-slot arr nm bit)) (b (vector-ref arr j)))
         (define (with-branch nb) (if (eq? nb b) node (make-hnode dm nm (vec-set arr j nb))))
         (cond
           ((hnode? b)
            (let ((r (assoc-in-node-edit b (fx+ shift 5) h k edit value added)))
              (and r (with-branch r))))
           ;; another hash than the bucket's: the key is not in it, and the
           ;; bucket nests beside the new entry
           ((not (hamt=? h (hcoll-hash b)))
            (let ((v (if edit (edit jolt-nil) value)))
              (and (or (not edit) v)
                   (begin (set-box! added #t)
                          (with-branch (nest-bucket (fx+ shift 5) b h k v))))))
           (else
            (let ((entry (assoc-in-collision-entry k (hcoll-alist b))))
              (and (not (eq? entry assoc-in-unsafe-entry))
                   (let ((v (if edit (edit (if entry (cdr entry) jolt-nil)) value)))
                     (and (or (not edit) v)
                          (with-branch (hcoll-assoc b entry k v added))))))))))
      (else
       (let ((v (if edit (edit jolt-nil) value)))
         (and (or (not edit) v)
              (begin (set-box! added #t)
                     (make-hnode (hamt-ior dm bit) nm
                                 (vec-insert2 arr (fx* 2 (hamt-arr-index dm bit)) k v)))))))))

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
