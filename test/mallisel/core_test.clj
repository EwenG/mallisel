(ns mallisel.core-test
  (:require [clojure.test :as t]
            [malli.core :as m]
            [malli.registry :as mr]
            [mallisel.core :as ms]))

(mr/set-default-registry! (mr/composite-registry
                           (mr/fast-registry
                            (ms/default-schemas))
                           (mr/var-registry)))

(def ID (m/-simple-schema
         {:type :ID
          :pred string?}))

(declare Proc)

(def Stage
  [:map-o
   [:stage-id {:global-p "global-p"} ID]
   [:proc [:ref #'Proc]]])

(def Proc
  [:map-o
   [:proc-id ID]
   [:stages [:sequential [:ref #'Stage]]]
   [:branching [:cat
                [:map-o
                 [:k1 string?]]
                [:map-o
                 [:k2 string?]]]]
   [:multi-schema [:multi {:dispatch :type}
                   ["t1" [:map-o
                          [:type string?]
                          [:k1 string?]]]
                   [::m/default [:map-o
                                 [:type string?]
                                 [:k2 string?]]]]]])

(t/deftest selection
  (let [selected (ms/select Proc [:proc-id
                                  [:stages {:optional true}
                                   [[:stage-id {:selection-p "selection-p"}]
                                    [:proc [[:stages [:stage-id]]]]]]])
        selected (ms/remove-unselected selected)]
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
             (-> selected ms/get-optionalized-schema m/form)))))

(comment
  (selection)
  )

(t/deftest selection-multi
  (let [selected (ms/select Proc [[:multi-schema [["t1" {:selection-p "selection-p"}
                                                   [:k1]]]]])
        selected (ms/remove-unselected selected)]
    (t/is (= (m/form selected)
             '[:map
               [:multi-schema
                [:multi {:dispatch :type}
                 ["t1" {:selection-p "selection-p"}
                  [:map [:k1 string?]]]
                 [:malli.core/default
                  :map]]]]))
    (t/is (= (m/form Proc)
             (-> selected ms/get-optionalized-schema m/form)))))

(comment
  (selection-multi)
  )

(t/deftest selection-path
  (let [selected (ms/select Proc [[:stages [:stage-id]]])
        selected (ms/remove-unselected selected)
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


