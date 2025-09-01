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

(defn- collect-valid-keys [schema valid-keys-volatile]
  (let [schema (maybe-deref-schema schema)
        schema? (m/schema? schema)
        schema-type (when schema? (m/-type (m/-parent schema)))]
    (cond (or (= schema-type :map) (= schema-type :multi))
          (doseq [entry (m/-children schema)]
            (vswap! valid-keys-volatile conj (first entry)))
          schema?
          (let [children (m/-children schema)]
            (when (seq children)
              (doseq [child children]
                (collect-valid-keys child valid-keys-volatile))))
          :else
          nil)))

(declare select-impl)

(defn- select-map-entry [options
                         path
                         selection
                         selected-keys-volatile
                         selected-count-volatile
                         entry]
  (let [k (first entry)
        sub-selection (get-subselection path options selection)
        _ (validate-subselection-size path options sub-selection)
        selection-properties (get-selection-properties
                              path options selection)
        entry-child (get entry 2)
        properties (second entry)
        properties-merger (:properties-merger options)
        merged-properties (properties-merger
                           properties selection-properties)
        selected-sub-selection (select-impl
                                entry-child
                                options
                                (conj path k)
                                sub-selection
                                nil)]
    (vswap! selected-count-volatile inc)
    (when selected-keys-volatile
      (vswap! selected-keys-volatile conj k))
    (if (seq merged-properties)
      [k merged-properties selected-sub-selection]
      [k selected-sub-selection])))

(defn- validate-map-entries [schema options path selection selected-keys-volatile selected-count-volatile]
  (when (and (nil? selected-keys-volatile)
             (not= @selected-count-volatile (count selection)))
    (let [schema-keys (into #{} (map first) (m/-children schema))
          selection-keys (into #{} (get-selection-keys selection))
          invalid-selection-keys (clojure.set/difference selection-keys schema-keys)]
      (throw (ex-info (str "Invalid selection keys: " invalid-selection-keys ". Valid keys are: " schema-keys ", at path " path)
                      {:path path
                       :selection (:full-selection options)
                       :invalid-selection-keys invalid-selection-keys
                       :valid-selection-keys schema-keys})))))

(defn- select-impl [schema options path selection selected-keys-volatile]
  (let [schema (maybe-deref-schema schema)
        schema? (m/schema? schema)
        schema-type (when schema? (m/-type (m/-parent schema)))]
    (cond (= schema-type :map)
          (let [schema-options (assoc (m/-options schema) ::path path)
                selected-count-volatile (volatile! 0)
                children (doall
                          (for [entry (m/-children schema)
                                :let [k (first entry)
                                      selection (lookup-selection path options selection k)]
                                :when selection]
                            (select-map-entry options
                                              path
                                              selection
                                              selected-keys-volatile
                                              selected-count-volatile
                                              entry)))]
            (validate-map-entries schema
                                  options
                                  path
                                  selection
                                  selected-keys-volatile
                                  selected-count-volatile)
            (m/-into-schema
             (m/-parent schema)
             (m/-properties schema)
             children
             schema-options))
          (= schema-type :multi)
          (let [schema-options (assoc (m/-options schema) ::path path)
                selected-count-volatile (volatile! 0)
                children (doall
                          (for [entry (m/-children schema)
                                :let [k (first entry)
                                      selection (lookup-selection path options selection k)]]
                            (if selection
                              (select-map-entry options
                                                path
                                                selection
                                                selected-keys-volatile
                                                selected-count-volatile
                                                entry)
                              (let [sub-selection []
                                    entry-child (get entry 2)
                                    selected-sub-selection (select-impl
                                                            entry-child
                                                            options
                                                            (conj path k)
                                                            sub-selection
                                                            nil)]
                                [k selected-sub-selection]))))]
            (validate-map-entries schema
                                  options
                                  path
                                  selection
                                  selected-keys-volatile
                                  selected-count-volatile)
            (m/-into-schema
             (m/-parent schema)
             (m/-properties schema)
             children
             schema-options))
          schema?
          (let [children (m/-children schema)
                schema-options (assoc (m/-options schema) ::path path)]
            (if (empty? children)
              (do
                (when (and (nil? selected-keys-volatile)
                           (not (empty? selection)))
                  (throw (ex-info (str "Invalid selection. Unexpected subselection " selection ", at path " path)
                                  {:path path
                                   :selection (:full-selection options)
                                   :invalid-selection selection})))
                (m/-into-schema (m/-parent schema)
                                (m/-properties schema)
                                children
                                schema-options))
              (let [new-selected-keys-volatile (when (nil? selected-keys-volatile)
                                                 (volatile! #{}))
                    selected-children (doall
                                       (for [child children]
                                         (select-impl child
                                                      options
                                                      path
                                                      selection
                                                      (or selected-keys-volatile
                                                          new-selected-keys-volatile))))]
                (when (and new-selected-keys-volatile
                           (not= (count @new-selected-keys-volatile) (count selection)))
                  (let [selection-keys (into #{} (get-selection-keys selection))
                        invalid-selection-keys (clojure.set/difference
                                                selection-keys
                                                @new-selected-keys-volatile)
                        schema-keys-volatile (volatile! #{})]
                    (doseq [child children]
                      (collect-valid-keys child schema-keys-volatile))
                    (throw (ex-info (str "Invalid selection keys: " invalid-selection-keys ". Valid keys are: " @schema-keys-volatile ", at path " path)
                                    {:path path
                                     :selection (:full-selection options)
                                     :invalid-selection-keys invalid-selection-keys
                                     :valid-selection-keys @schema-keys-volatile}))))
                (m/-into-schema (m/-parent schema)
                                (m/-properties schema)
                                selected-children
                                schema-options))))
          :else
          (do
            (when (and (nil? selected-keys-volatile) (not (empty? selection)))
              (throw (ex-info (str "Invalid selection. Unexpected subselection " selection ", at path " path)
                              {:path path
                               :selection (:full-selection options)
                               :invalid-selection selection})))
            schema))))

(defn- assoc-selected-schema-options [selected-from-schema selection selected-schema-options]
  (assoc selected-schema-options
         ::selected-from-schema selected-from-schema
         ::selection selection))

(defn select
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
         selected-schema (select-impl schema options [] selection nil)]
     (m/-update-options selected-schema (partial assoc-selected-schema-options selected-from-schema selection)))))

#?(:clj
   (defmacro map-o [& body]
     (macros/map-o-impl body)))
