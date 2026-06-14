# Cross-namespace ^Type field hints (jolt-3ko follow-up): a record field hinted
# with a record type defined in ANOTHER namespace — referred (:refer) or aliased
# (:as) in — must resolve to that type's HOME ctor key in the record-shapes
# registry, the same as a same-namespace hint does. That resolved key is what
# lets the inference type a field read back to the foreign record type instead of
# :any (the lever for fast nested-record code across a multi-namespace program).
# Guards both the :refer and :as spellings — for record FIELD hints and for
# fn PARAM hints (which seed the inference so a record param's reads are typed
# across a namespace boundary without whole-program). Also guards that the
# reader keeps a tag's namespace qualifier (^g/Pt -> "g/Pt", not "Pt").
(use ../../src/jolt/api)
(import ../../src/jolt/types :as ty)
(import ../../src/jolt/core :as jc)
(import ../../src/jolt/reader :as rd)

(var failures 0)
(defn- check [label got want]
  (unless (deep= got want)
    (++ failures)
    (printf "FAIL [%s] got %q want %q" label got want)))

(def dir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-xns-hints"))
(os/mkdir dir)
(os/mkdir (string dir "/geo"))
# Pt lives in geo.pt; shape records in geo.shape hint ^Pt across the boundary.
(spit (string dir "/geo/pt.clj")
      "(ns geo.pt)\n(defrecord Pt [x y z])\n")
(spit (string dir "/geo/shape.clj")
      (string "(ns geo.shape (:require [geo.pt :as g :refer [Pt]]))\n"
              "(defrecord Seg [^Pt a ^Pt b])\n"               # :refer field hint
              "(defrecord Tri [^g/Pt a ^g/Pt b ^g/Pt c])\n"   # :as field hint
              # param hints, both spellings: ^Pt (referred), ^g/Pt (aliased)
              "(defn mid [^Pt a ^g/Pt b] a)\n"))

(def ctx (init {:compile? true :direct-linking? true}))
(array/push (get (ctx :env) :source-paths) dir)
(eval-string ctx "(require '[geo.shape])")
(def rs (get (ctx :env) :record-shapes))

(check ":refer ^Pt field hint resolves to home ctor key"
       (get (get rs "geo.shape/->Seg") :tags)
       ["geo.pt/->Pt" "geo.pt/->Pt"])
(check ":as ^g/Pt field hint resolves to home ctor key"
       (get (get rs "geo.shape/->Tri") :tags)
       ["geo.pt/->Pt" "geo.pt/->Pt" "geo.pt/->Pt"])
# the foreign type's own shape is registered under its home key
(check "home type registered"
       (get (get rs "geo.pt/->Pt") :fields)
       [:x :y :z])

# --- param hints: the arity carries [name ctor-key] for each record param, both
# the :refer (^Pt) and :as (^g/Pt) spellings resolved to the home key ----------
(def shape-ns (ty/ctx-find-ns ctx "geo.shape"))
(def mid-ir (get (get (get shape-ns :mappings) "mid") :infer-ir))
(def mid-arity (first (jc/vview (get (get mid-ir :init) :arities))))
(def phints (when (get mid-arity :phints)
              (map jc/vview (jc/vview (get mid-arity :phints)))))
(check "param hints resolve cross-ns (refer + as)"
       phints
       @[@["a" "geo.pt/->Pt"] @["b" "geo.pt/->Pt"]])

# --- reader keeps a tag's namespace qualifier ---------------------------------
(check "reader preserves qualified tag ^g/Pt"
       (get (get (rd/parse-string "^g/Pt x") :meta) :tag)
       "g/Pt")
(check "reader bare tag ^Pt unchanged"
       (get (get (rd/parse-string "^Pt x") :meta) :tag)
       "Pt")

(if (= 0 failures)
  (print "cross-ns-hints: all cases passed")
  (do (printf "cross-ns-hints: %d FAILURES" failures) (os/exit 1)))
