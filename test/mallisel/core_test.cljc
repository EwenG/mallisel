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
                 [:k2 string?]]]]
   [:multi-schema [:multi {:dispatch :type}
                   ["t1" [:map
                          [:type string?]
                          [:k1 string?]]]
                   [::m/default [:map
                                 [:type string?]
                                 [:k2 string?]]]]]))

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

(t/deftest selection-multi
  (let [selected (ms/select Proc [[:multi-schema [["t1" {:selection-p "selection-p"}
                                                   [:k1]]]]])]
    (t/is (= (m/form selected)
             '[:map
               [:multi-schema
                [:multi {:dispatch :type}
                 ["t1" {:selection-p "selection-p"}
                  [:map [:k1 string?]]]
                 [:malli.core/default
                  :map]]]]))
    (t/is (= (m/form Proc)
             (-> selected m/options ::ms/selected-from-schema m/form)))))

(comment
  (selection-multi)
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


