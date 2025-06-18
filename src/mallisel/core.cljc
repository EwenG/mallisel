(ns mallisel.core
  (:require [clojure.set]
            [malli.core :as m]
            #?(:clj [mallisel.macros :as macros])))

(defn- default-properties-merger [p1 p2]
  (if (:optional p2)
    (merge p1 p2)
    (merge (dissoc p1 :optional) p2)))

(defn- lookup-selection-impl [path options k sel]
  (let [sel-k (cond (vector? sel)
                    (first sel)
                    (coll? sel)
                    (throw (ex-info (str "Invalid selection type: " (type sel) ". Invalid selection is " sel ", at path " path)
                                    {:path path
                                     :selection (:full-selection options)
                                     :invalid-selection sel}))
                    :else sel)]
    (when (or (nil? sel-k) (coll? sel-k))
      (throw (ex-info (str "Invalid selection type: " (type sel) ". Invalid selection is " sel ", at path " path)
                      {:path path
                       :selection (:full-selection options)
                       :invalid-selection sel})))
    (when (= k sel-k)
      sel)))

(defn- lookup-selection [path options selection k]
  (some (partial lookup-selection-impl path options k) selection))

(defn- throw-invalid-selection [path options invalid-selection]
  (throw (ex-info (str "Invalid selection. Subselection must be a vector of maximum 3 elements: [<selection-path> <optional-properties-map> <optional-subselection-vector>]. Invalid subselection is " invalid-selection ", at path " path)
                  {:path path
                   :selection (:full-selection options)
                   :invalid-selection invalid-selection})))

(defn- validate-subselection-size [path options sel]
  (when (vector? sel)
    (when-not (< (count sel) 4)
      (throw-invalid-selection path options sel))))

(defn- get-selection-properties [path options sel]
  (when (vector? sel)
    (let [properties (second sel)
          properties (when-not (vector? properties) properties)]
      (when-not (or (nil? properties)
                    (map? properties))
        (throw-invalid-selection path options sel))
      properties)))

(defn- get-subselection [path options sel]
  (when (vector? sel)
    (let [subselection (second sel)
          subselection (if (vector? subselection)
                         subselection
                         (get sel 2))]
      (when-not (or (nil? subselection)
                    (vector? subselection))
        (throw-invalid-selection path options sel))
      subselection)))

(defn- get-selection-keys [selection]
  (doall
   (for [sel selection
         :let [sel-k (cond (vector? sel)
                           (first sel)
                           (coll? sel)
                           nil
                           :else sel)]
         :when (not (or (nil? sel-k) (coll? sel-k)))]
     sel-k)))

(defn- maybe-deref-schema [schema]
  (if (m/-ref-schema? schema)
    (m/-deref schema)
    schema))

(declare select-impl-dispatch)

(defn- select-impl [schema options path selection]
  (let [schema (maybe-deref-schema schema)]
    (cond (m/-entry-schema? schema)
          (let [schema-options (assoc (m/-options schema) ::path path)
                children (doall
                          (for [entry (m/-children schema)
                                :let [k (first entry)
                                      selection (lookup-selection path options selection k)]
                                :when selection
                                :let [sub-selection (get-subselection path options selection)
                                      _ (validate-subselection-size path options sub-selection)
                                      selection-properties (get-selection-properties
                                                            path options selection)
                                      entry-child (get entry 2)
                                      properties (second entry)
                                      properties-merger (:properties-merger options)
                                      merged-properties (properties-merger
                                                         properties selection-properties)
                                      selected-sub-selection (select-impl-dispatch entry-child options (conj path k) sub-selection)]]
                            (if (seq merged-properties)
                              [k merged-properties selected-sub-selection]
                              [k selected-sub-selection])))]
            (when (not= (count children) (count selection))
              (let [schema-keys (into #{} (map first) (m/-children schema))
                    selection-keys (into #{} (get-selection-keys selection))
                    invalid-selection-keys (clojure.set/difference selection-keys schema-keys)]
                (throw (ex-info (str "Invalid selection keys: " invalid-selection-keys ". Valid keys are: " schema-keys ", at path " path)
                                {:path path
                                 :selection (:full-selection options)
                                 :invalid-selection-keys invalid-selection-keys
                                 :valid-selection-keys schema-keys}))))
            (m/-into-schema
             (m/-parent schema)
             (m/-properties schema)
             children
             schema-options))
          (m/schema? schema)
          (let [children (m/-children schema)
                schema-options (assoc (m/-options schema) ::path path)]
            (if (empty? children)
              (do
                (when-not (empty? selection)
                  (throw (ex-info (str "Invalid selection. Unexpected subselection " selection ", at path " path)
                                  {:path path
                                   :selection (:full-selection options)
                                   :invalid-selection selection})))
                (m/-into-schema (m/-parent schema)
                                (m/-properties schema)
                                children
                                schema-options))
              (let [children (doall
                              (for [child children]
                                (select-impl child options path selection)))]
                (m/-into-schema (m/-parent schema)
                                (m/-properties schema)
                                children
                                schema-options))))
          :else
          (do
            (when-not (empty? selection)
              (throw (ex-info (str "Invalid selection. Unexpected subselection " selection ", at path " path)
                              {:path path
                               :selection (:full-selection options)
                               :invalid-selection selection})))
            schema))))

(defn- select-impl-pick [schema options path selection]
  (let [selection-type (first selection)
        selection-type (if (m/into-schema? selection-type)
                         (m/-type selection-type)
                         selection-type)]
    (if (= selection-type ::select)
      (select-impl-dispatch schema options path (second selection))
      (let [schema (maybe-deref-schema schema)]
        (when-not (m/schema? schema)
          (throw (ex-info (str "Invalid pick of non schema. Invalid pick " schema ", at path " path)
                          {:path path
                           :selection (:full-selection options)
                           :invalid-pick schema})))
        (when (m/-entry-schema? schema)
          (throw (ex-info (str "Invalid pick of entry schema. Invalid pick " (m/-type (m/parent schema)) ", at path " path)
                          {:path path
                           :selection (:full-selection options)
                           :invalid-pick schema})))
        (let [schema-type (m/-type (m/parent schema))
              children (m/-children schema)
              schema-options (assoc (m/-options schema) ::path path)]
          (when (not= schema-type selection-type)
            (throw (ex-info (str "Pick selection does not match schema. Invalid selection is " selection ", at path " path)
                            {:path path
                             :selection (:full-selection options)
                             :pick-shema schema
                             :invalid-selection selection
                             :selection-type selection-type})))
          (let [selection (rest selection)
                pick-properties (first selection)
                pick-properties (when (map? pick-properties) pick-properties)
                selection (if pick-properties (rest selection) selection)
                properties-merger (:properties-merger options)
                properties (m/-properties schema)
                merged-properties (properties-merger properties pick-properties)
                children (loop [children children
                                selection selection
                                selected-children []]
                           (cond (and (nil? (seq children)) (nil? (seq selection)))
                                 selected-children
                                 (nil? (seq children))
                                 (throw (ex-info (str "Invalid selection. Unexpected subselection " selection ", at path " path)
                                                 {:path path
                                                  :selection (:full-selection options)
                                                  :pick-shema schema
                                                  :invalid-selection (first selection)}))
                                 (nil? (seq selection))
                                 (throw (ex-info (str "Missing subselection, at path " path)
                                                 {:path path
                                                  :selection (:full-selection options)
                                                  :pick-shema schema}))
                                 (not (m/schema? (first children)))
                                 (do
                                   (when (not= (first children) (first selection))
                                     (throw (ex-info (str "Pick selection does not match schema. Invalid selection is " (first selection) ", at path " path)
                                                     {:path path
                                                      :selection (:full-selection options)
                                                      :pick-shema schema
                                                      :invalid-selection (first selection)})))
                                   (recur (rest children)
                                          (rest selection)
                                          (conj selected-children (first children))))
                                 :else
                                 (let [selected-child (select-impl-pick (first children) options path (first selection))]
                                   (recur (rest children)
                                          (rest selection)
                                          (conj selected-children selected-child)))))]
            (m/-into-schema (m/-parent schema)
                            merged-properties
                            children
                            schema-options)))))))

(defn- select-impl-dispatch [schema options path selection]
  (cond (= ::pick (first selection))
        (select-impl-pick schema options path (second selection))
        (= ::select (first selection))
        (select-impl schema options path (second selection))
        :else
        (select-impl schema options path selection)))

(defn- assoc-selected-schema-options [selected-from-schema selection selected-schema-options]
  (assoc selected-schema-options
         ::selected-from-schema selected-from-schema
         ::selection selection))

(defn select
  ([selection]
   [::select selection])
  ([schema selection]
   (select schema selection nil))
  ([schema selection {:keys [properties-merger] :as options}]
   (let [schema (m/schema schema)
         selected-from-schema (::selected-from-schema (m/-options schema))
         selected-from-schema (or selected-from-schema schema)
         options (if properties-merger
                   options
                   (assoc options :properties-merger default-properties-merger))
         options (assoc options :full-selection selection)
         selected-schema (select-impl-dispatch schema options [] selection)]
     (m/-update-options selected-schema (partial assoc-selected-schema-options selected-from-schema selection)))))

(defn pick [selection]
  [::pick selection])

#?(:clj
   (defmacro map-o [& body]
     (macros/map-o-impl body)))
