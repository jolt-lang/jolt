;; post-prelude overrides (jolt-9ziu) — loaded AFTER the assembled clojure.core
;; prelude, so these win over the overlay's own def-var!.
;;
;; A few clojure.core predicates are implemented in the overlay by inspecting a
;; Janet-host tagged value's :jolt/type key (e.g. (get x :jolt/type)). That key
;; doesn't exist for Chez-native representations: a jolt char is a Scheme char,
;; an atom is a Chez record. The overlay's def-var! loads after rt.ss, so it
;; clobbers the correct native shims (predicates.ss / atoms.ss) with versions
;; that return false on every Chez value. Re-assert the native versions here.
;;
;; (Long-term these predicates want a host-neutral implementation that calls a
;; host primitive instead of reading :jolt/type; until then this is the Chez-host
;; override.)
(def-var! "clojure.core" "char?" jolt-char-pred?)
(def-var! "clojure.core" "atom?" jolt-atom?)
