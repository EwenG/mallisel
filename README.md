# 🧩 Mallisel

**Mallisel** is a Clojure library for selecting subtrees from [Malli](https://github.com/metosin/malli) schemas. It provides a way to declaratively extract parts of a schema based on `:map` keys and `:multi` entries

---

## ✨ Features

- Select specific keys from `:map` schemas
- Navigate and extract schema branches from `:multi` types
- Navigate and extract schema branches from sum types (`:or`, `and`, `sequential`, `:cat`, `:tuple`, etc.)
- Automatically handles schema dereferencing (`:ref`, `:var`, etc.)
- Validates selections and errors on incorrect paths
- Supports schema metadata merging (e.g., `:optional`)

---

## 📦 Installation

Mallisel is not yet published on Clojars.

To use it directly via `deps.edn`:

```clojure
{eweng/mallisel {:git/url "https://github.com/EwenG/mallisel"
                 :sha "<latest-commit-sha>"}}
```

---

## 🚀 Usage

### Selecting map keys

```clojure
(require '[mallisel.core :as ms])
(require '[malli.core :as m])

(def schema
  [:map
   [:id int?]
   [:name string?]
   [:age int?]])

(def selected
  (ms/select schema
    [[:id] [:name]]))

(m/validate selected {:id 1 :name "Ada"}) ;=> true
```

---

### Selecting schema branches (e.g., `:or`)

```Clojure
(def schema
  [:or
   [:map [:id int?]]
   [:map [:name string?]]])

(def selected
  (ms/select schema
    [::ms/pick [:or
                (ms/select [])
                (ms/select [:name])]]))

(m/validate selected {:name "Mallisel"}) ;=> true
```

---

### Nested selection

```Clojure
(def schema
  [:map
   [:user [:map
           [:id int?]
           [:name string?]]]
   [:meta [:map
           [:timestamp inst?]]]])

(def selected
  (ms/select schema
    [[:user [[:name]]]]))

(m/validate selected {:user {:name "Ada"}}) ;=> true
```

---

## 🧠 Conceptual Model

Mallisel provides a declarative way to extract or focus on specific parts of a Malli schema. You can think of it as a schema-aware, safe `select` mechanism that understands the structure and semantics of Malli schemas.

- **Tree traversal**: `select` walks through `:map` schemas by matching keys provided in the selection vector.
- **Branch handling**: `pick` is used to handle branching schema types like `:or`, `:cat`, `:tuple`, and more. It allows selecting specific paths inside these schema branches.
- **Dereferencing**: Schema references like `:ref`, `:var`, or custom registry types are automatically resolved during selection.
- **Property merging**: You can attach metadata (e.g., `{:optional true}`) to selected keys, which are merged into the resulting schema entries.
- **Strict validation**: If a selection refers to a non-existent key or branch, Mallisel throws detailed, path-aware errors to prevent silent failures.

---

## 📘 API Overview

```Clojure
(ms/select schema selection)              ;; select map keys and subtrees
(ms/select selection)                     ;; builds a ::select node (for nesting)
(ms/pick selection)                       ;; builds a ::pick node (for branching)
```

### Selection syntax

```Clojure
[[:key1 [:subkey1]]             ;; nested key selection
 [:key2 {:optional true}]       ;; merge properties into key
 [::ms/pick [:or [:branch1]]]   ;; pick schema branch
]
```