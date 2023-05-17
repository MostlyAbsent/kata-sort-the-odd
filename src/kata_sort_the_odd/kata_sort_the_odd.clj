(ns kata-sort-the-odd.kata-sort-the-odd)

(defn sort-odds [xs]
  (let [k (sort (keys xs))
        v (sort (vals xs))]
    (zipmap k v)))

(defn index-array [a]
  (map-indexed (fn [idx itm] {idx itm}) a))

(defn sort-array [xs]
  (let [idx-array (index-array xs)]
    (as-> (filter (fn [x] (odd? (first (vals x)))) idx-array) $
      (into {} $)
      (sort-odds $)
      (merge (reduce merge idx-array) $)
      (sort $)
      (map second $))))
