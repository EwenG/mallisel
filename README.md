# mallisel

A Clojure library for selecting `:map` keys from [Malli](https://github.com/metosin/malli) schemas.

`mallisel` lets you derive smaller schemas from larger ones by **selecting specific keys**, while preserving metadata and relationships. It works with nested maps, sequences, recursive schemas, and multi-schemas.

## Features

- Select keys from `:map` schemas.
- Treat selections as **required keys** (on top of optionalized schemas).
- `map-o` macro for defining maps with all keys optional (since Malli has no built-in way to do this).
- Support for nested and recursive schemas.
- Works with `:sequential`, `:multi`, and other Malli schema types.
- Preserves schema metadata (`:optional`, custom options, etc.).
- Custom **properties merger** function support when combining properties.
- Validates that selections reference only existing keys.
- Keeps track of selection paths and the originating schema.
- For branching schemas (`:multi`, `:cat`, etc.), selections are applied to all branches. For `:multi`, specific dispatch values can be targeted, while other branches are preserved.
- Provides `validator` / `validate` functions that behave like Malli’s counterparts, but require a **selected schema** and validate values against both the **optionalized schema** and the **selected schema**.

## Usage

Require the namespace:

```clojure
(ns your.ns
  (:require [malli.core :as m]
            [mallisel.core :as ms]))
```

## Defining optionalized maps

Since Malli does not have a built-in way to make all keys optional, `mallisel` provides `map-o`:

```clojure
(def ID (m/-simple-schema {:type :ID :pred string?}))

(def Proc
  (ms/map-o
   [:proc-id ID]
   [:stages [:sequential [:map [:stage-id ID]]]]))

(m/form Proc)
;; => [:map {:optional true} [:proc-id :ID] [:stages [:sequential [:map [:stage-id :ID]]]]]

```

## Basic selection

Selecting keys marks them as required:

```clojure
(ms/select Proc [:proc-id])
;; => [:map [:proc-id :ID]]
```

## Nested selection

```clojure
(def Stage
  (ms/map-o
   [:stage-id {:global-p "global-p"} ID]
   [:proc [:ref #'Proc]]))

(def Proc
  (ms/map-o
   [:proc-id ID]
   [:stages [:sequential [:ref #'Stage]]]))

(ms/select Proc
           [:proc-id
            [:stages {:optional true}
             [[:stage-id {:selection-p "selection-p"}]
              [:proc [[:stages [:stage-id]]]]]]])
```

Produces:

```clojure
[:map
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
         [:stage-id {:global-p "global-p"} :ID]]]]]]]]]]
```

## Multi-schemas

Selections work inside `:multi` dispatch schemas:

```clojure
(def Proc
  (ms/map-o
   [:multi-schema [:multi {:dispatch :type}
                   ["t1" [:map [:type string?] [:k1 string?]]]
                   [::m/default [:map [:type string?] [:k2 string?]]]]]))

(ms/select Proc [[:multi-schema [["t1" {:selection-p "selection-p"} [:k1]]]]])
```

Result

```clojure
[:map
 [:multi-schema
  [:multi {:dispatch :type}
   ["t1" {:selection-p "selection-p"}
    [:map [:k1 string?]]]
   [:malli.core/default :map]]]]
```

Note: unlike maps, all dispatch branches are kept; selections apply only to targeted branches.

## Paths

Every schema node includes its **selection path** under `::ms/path`. For example:

```clojure
(let [selected (ms/select Proc [[:stages [:stage-id]]])]
  (m/walk selected
          (fn [schema _ _ _]
            (-> schema m/-options ::ms/path))))
```

## Validation

mallisel provides `validator` and `validate` functions. They work like Malli’s, but must be given a **selected schema**, and they validate against both:

- the **optionalized schema** (ensuring structural correctness)
- the **selected schema** (ensuring selected keys are present and valid)
  
Example:

```clojure
(def selected (ms/select Proc [:proc-id]))
(def validate-proc (ms/validator selected))

;; passes: required key present
(validate-proc {:proc-id "abc"})
;; => true

;; fails: required key missing
(validate-proc {})
;; => false

;; structural correctness check (against optionalized schema)
(validate-proc {:proc-id "abc" :stages [{:stage-id 123}]})
;; => false ; :stage-id must be a string (ID schema)

(validate-proc {:proc-id "abc" :stages [{:stage-id "s1"}]})
;; => true
```