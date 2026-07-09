;; Adapted from rm-hull/jasentaa (MIT).

(ns jolt.parser.collections)

(defn- not-list? [x]
  (or
   (not (coll? x))
   (record? x)))

(defn join [a b]
  (cond
    (and (string? a) (string? b))
    (str a b)

    (and (nil? a) (string? b))
    b

    (and (string? a) (nil? b))
    a

    (nil? a)
    (recur [] b)

    (nil? b)
    (recur a [])

    (not-list? a)
    (recur (list a) b)

    (not-list? b)
    (recur a (list b))

    :else
    (concat a b)))
