(ns jolt.native.layout
  "Binary record layouts as native regions: a layout is data, known at runtime,
  and each function here lowers one into a region (jolt.native/native-fn) that
  reads it from a byte-array with no interpretation left: the offsets, widths,
  byte order and sign extension are constants in the region's C.

      (require '[jolt.native.layout :as layout])

      (def header [[:magic :u32] [:version :u8] [:pad 1] [:count :u16] [:size :i64]])
      (def parse (layout/parser header))
      (vec (parse buf 0))              ;=> [magic version count size]

      (def sizes (layout/column header :size))
      (vec (sizes buf))                ;=> the :size of every whole record in buf

  A layout is a vector of fields, each [name type] with type one of :u8 :i8
  :u16 :i16 :u32 :i32 :i64 (an unsigned field reads as a non-negative long; :i64
  is the full long), or [:pad n] for n bytes skipped. Options: :endian :little
  (the default) or :big. A read past the end of the array raises
  ArrayIndexOutOfBoundsException, as aget does.

  Needs the JIT (jolt.native/jit?), as native-fn does."
  (:require [jolt.native :as native]))

(def ^:private widths {:u8 1 :i8 1 :u16 2 :i16 2 :u32 4 :i32 4 :i64 8})

(defn- check-field [f]
  (when-not (and (vector? f) (= 2 (count f)))
    (throw (ex-info (str "layout: a field is [name type] or [:pad n], got " (pr-str f)) {:field f})))
  (let [[n t] f]
    (cond
      (= :pad n) (when-not (and (int? t) (pos? t))
                   (throw (ex-info (str "layout: [:pad n] takes a positive byte count, got " (pr-str t))
                                   {:field f})))
      (not (keyword? n)) (throw (ex-info (str "layout: a field's name is a keyword, got " (pr-str n))
                                         {:field f}))
      (not (widths t)) (throw (ex-info (str "layout: " n " has type " (pr-str t)
                                            "; one of :u8 :i8 :u16 :i16 :u32 :i32 :i64")
                                       {:field f})))))

(defn- layout-fields
  "The layout's named fields as [{:name :type :offset}], and its size in bytes."
  [layout]
  (run! check-field layout)
  (let [[fs size] (reduce (fn [[fs off] [n t]]
                            (if (= :pad n)
                              [fs (+ off t)]
                              [(conj fs {:name n :type t :offset off}) (+ off (widths t))]))
                          [[] 0] layout)]
    (when (empty? fs) (throw (ex-info "layout: no fields" {:layout layout})))
    (when-not (apply distinct? (map :name fs))
      (throw (ex-info "layout: two fields share a name" {:layout layout})))
    {:fields fs :size size}))

(defn record-size
  "The layout's size in bytes, padding included."
  [layout]
  (:size (layout-fields layout)))

(defn- read-form
  "A region form reading field f of the record that starts at the byte index
  form base, from the ^bytes local buf."
  [{:keys [type offset]} base endian]
  (let [w (widths type)
        at (fn [k] (list 'aget 'buf (list '+ base (+ offset k))))
        ;; byte k of the value, least significant first
        byte-at (fn [k] (at (if (= endian :big) (- w 1 k) k)))
        unsigned (reduce (fn [acc k]
                           (let [b (list 'bit-and (byte-at k) 255)
                                 b (if (zero? k) b (list 'bit-shift-left b (* 8 k)))]
                             (if acc (list 'bit-or acc b) b)))
                         nil (range w))]
    (case type
      :i8 (byte-at 0)
      (:u8 :u16 :u32 :i64) unsigned
      ;; sign-extend from the field's top bit
      (:i16 :i32) (let [s (- 64 (* 8 w))]
                    (list 'bit-shift-right (list 'bit-shift-left unsigned s) s)))))

(defn- endian-of [opts]
  (let [e (get opts :endian :little)]
    (when-not (#{:little :big} e)
      (throw (ex-info (str "layout: :endian is :little or :big, got " (pr-str e)) {:endian e})))
    e))

(defn parser-body
  "The region body parser compiles, as data."
  [layout & {:as opts}]
  (let [{:keys [fields]} (layout-fields layout)
        e (endian-of opts)]
    (list (concat (list 'let ['out (list 'long-array (count fields))])
                  (map-indexed (fn [i f] (list 'aset 'out i (read-form f 'off e))) fields)
                  ['out]))))

(defn parser
  "A fn [buf off] reading one record of layout from the byte-array buf at byte
  offset off: a long-array of its named fields' values, in layout order."
  [layout & {:as opts}]
  (native/native-fn '[^bytes buf ^long off] (apply parser-body layout (mapcat identity opts))))

(defn column-body
  "The region body column compiles, as data."
  [layout field & {:as opts}]
  (let [{:keys [fields size]} (layout-fields layout)
        f (or (first (filter #(= field (:name %)) fields))
              (throw (ex-info (str "layout: no field " (pr-str field)) {:field field})))
        e (endian-of opts)]
    (list (list 'let ['n (list 'quot '(alength buf) size)
                      'out '(long-array n)]
                (list 'dotimes ['r 'n]
                      (list 'aset 'out 'r (read-form f (list '* 'r size) e)))
                'out))))

(defn column
  "A fn [buf] scanning the byte-array buf as consecutive records of layout:
  a long-array of field's value in each whole record (trailing bytes short of
  a record are ignored)."
  [layout field & {:as opts}]
  (native/native-fn '[^bytes buf] (apply column-body layout field (mapcat identity opts))))
