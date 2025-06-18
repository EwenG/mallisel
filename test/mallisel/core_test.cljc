(ns mallisel.core-test
  (:require [clojure.test :as t]
            [malli.core :as m]
            [mallisel.core :as ms]))

(def ID (m/-simple-schema
         {:type :ID
          :pred string?}))

(declare Proc)

(def Stage
  (ms/map-o
   [:stage-id {:global-p "global-p"} ID]
   [:proc [:ref #'Proc]]))

(def Proc
  (ms/map-o
   [:proc-id ID]
   [:stages [:sequential [:ref #'Stage]]]
   [:branching [:cat
                [:map
                 [:k1 string?]]
                [:map
                 [:k2 string?]]]]))

(t/deftest selection
  (let [selected (ms/select Proc [:proc-id
                                  [:stages {:optional true}
                                   [[:stage-id {:selection-p "selection-p"}]
                                    [:proc [[:stages [:stage-id]]]]]]])]
    (t/is (= (m/form selected)
             '[:map
               [:proc-id :ID]
               [:stages {:optional true}
                [:sequential
                 [:map
                  [:stage-id {:global-p "global-p"
                              :selection-p "selection-p"} :ID]
                  [:proc
                   [:map
                    [:stages
                     [:sequential
                      [:map
                       [:stage-id {:global-p "global-p"} :ID]]]]]]]]]]))
    (t/is (= (m/form Proc)
             (-> selected m/options ::ms/selected-from-schema m/form)))))

(comment
  (selection)
  )

(t/deftest selection-path
  (let [selected (ms/select Proc [[:stages [:stage-id]]])
        path->schemas-atom (atom {})]
    (m/walk selected (fn [schema _ _ _]
                       (let [path (-> schema m/-options ::ms/path)]
                         (swap! path->schemas-atom update (m/form schema) (fnil conj #{}) path))))
    (t/is (= @path->schemas-atom
             '{:ID #{[:stages :stage-id]}
               [:map
                [:stage-id
                 {:global-p "global-p"}
                 :ID]] #{[:stages]}
               [:sequential
                [:map
                 [:stage-id
                  {:global-p "global-p"}
                  :ID]]] #{[:stages]}
               [:map
                [:stages
                 [:sequential
                  [:map
                   [:stage-id
                    {:global-p "global-p"}
                    :ID]]]]] #{[]}}))))

(comment
  (selection-path)
  )

(t/deftest pick
  (let [selected (ms/select Proc [[:branching (ms/pick [:cat {:optional true}
                                                        (ms/select [])
                                                        (ms/select [:k2])])]])]
    (t/is (= (m/form selected)
             '[:map [:branching
                     [:cat {:optional true}
                      :map
                      [:map [:k2 string?]]]]]))))

(comment
  (pick)
  )

(t/deftest pick-path
  (let [selected (ms/select Proc [[:branching (ms/pick [:cat
                                                        (ms/select [])
                                                        (ms/select [:k2])])]])
        path->schemas-atom (atom {})]
    (m/walk selected (fn [schema _ _ _]
                       (let [path (-> schema m/-options ::ms/path)]
                         (swap! path->schemas-atom update (m/form schema) (fnil conj #{}) path))))
    (t/is (= @path->schemas-atom
             '{:map #{[:branching]}
               string? #{[:branching :k2]}
               [:map [:k2 string?]] #{[:branching]}
               [:cat
                :map
                [:map [:k2 string?]]] #{[:branching]}
               [:map
                [:branching
                 [:cat
                  :map
                  [:map [:k2 string?]]]]] #{[]}}))))

(comment
  (pick-path)
  )


