(ns workload.event-index)

(defn apply-event [index {:keys [account amount id]}]
  (let [previous (get index account)]
    (assoc index account {:count (inc (get previous :count 0))
                          :total (+ (get previous :total 0) amount)
                          :last-id id})))

(defn ingest [events]
  (reduce apply-event {} events))

(defn update-index [index events]
  (reduce apply-event index events))

(defn query [index accounts]
  (mapv #(get index %) accounts))
