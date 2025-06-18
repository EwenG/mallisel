(ns mallisel.macros)

(defn map-o-impl [body]
  (let [properties (first body)
        properties (when (map? properties) properties)
        fields (if properties
                 (rest body)
                 body)]
    `[:map ~@(when properties [properties])
      ~@(for [field fields]
          (cond (and (vector? field) (= (count field) 0))
                field
                (and (vector? field) (= (count field) 1))
                [(first field) {:optional true}]
                (and (vector? field) (> (count field) 1))
                (let [properties (get field 1)]
                  (if (or (nil? properties) (map? properties))
                    (assoc-in field [1 :optional] true)
                    (into [(first field) {:optional true}]
                          (rest field))))
                (coll? field)
                field
                :else
                [field {:optional true}]))]))
