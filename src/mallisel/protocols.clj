(ns mallisel.protocols)

(defprotocol SelectedSchema
  :extend-via-metadata true
  (-optionalized-schema [this] "Returns the schema used to create the selection schema")
  (-selection [this] "Returns the selection used to create the SelectedSchema")
  (-remove-unselected [this] "Returns the SelectedSchema without the unselected paths"))
