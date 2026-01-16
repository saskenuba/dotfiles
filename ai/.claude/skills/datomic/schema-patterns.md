# Datomic Schema Patterns

Patterns for defining and managing Datomic schemas.

## Schema Definition Helpers

Common helper functions for cleaner schema definitions:

```clojure
(ns myapp.model.schemas
  (:require [datomic.client.api :as d]))

;; Base field helper
(defn field [field-name field-type]
  {:db/ident field-name 
   :db/valueType field-type 
   :db/cardinality :db.cardinality/one})

;; Type-specific helpers
(defn string-field [field-name] (field field-name :db.type/string))
(defn int-field [field-name] (field field-name :db.type/long))
(defn bigdec-field [field-name] (field field-name :db.type/bigdec))
(defn boolean-field [field-name] (field field-name :db.type/boolean))
(defn date-field [field-name] (field field-name :db.type/instant))
(defn keyword-field [field-name] (field field-name :db.type/keyword))

;; UUID identity field (most common entity ID pattern)
(defn uuid-field [field-name]
  {:db/ident field-name
   :db/valueType :db.type/uuid
   :db/cardinality :db.cardinality/one
   :db/unique :db.unique/identity})

;; Unique value (not identity - for things like email)
(defn unique-value-field [field-type field-name]
  (merge (field field-name field-type) {:db/unique :db.unique/value}))

;; Relationship helpers
(defn has-one [field-name]
  {:db/ident field-name
   :db/valueType :db.type/ref
   :db/cardinality :db.cardinality/one})

(defn has-many [field-name]
  {:db/ident field-name
   :db/valueType :db.type/ref
   :db/cardinality :db.cardinality/many})

(defn has-one-component [field-name]
  {:db/ident field-name
   :db/valueType :db.type/ref
   :db/cardinality :db.cardinality/one
   :db/isComponent true})

(defn has-many-components [field-name]
  {:db/ident field-name
   :db/valueType :db.type/ref
   :db/cardinality :db.cardinality/many
   :db/isComponent true})

;; Modifier
(defn without-history [attr-map] 
  (assoc attr-map :db/noHistory true))
```

## Entity Schema Examples

```clojure
(def user-schema
  [(uuid-field :user/id)
   (string-field :user/name)
   (unique-value-field :db.type/string :user/email)
   (string-field :user/password-hash)
   (date-field :user/created-at)])

(def invoice-schema
  [(uuid-field :invoice/id)
   (int-field :invoice/number)
   (date-field :invoice/issue-date)
   (bigdec-field :invoice/total)
   (boolean-field :invoice/deleted?)
   (has-one :invoice/company)
   (has-one :invoice/customer)
   (has-many-components :invoice/line-items)])

(def line-item-schema
  [(uuid-field :line-item/id)
   (string-field :line-item/description)
   (int-field :line-item/quantity)
   (bigdec-field :line-item/unit-price)])
```

## Composite Tuple Schema

```clojure
;; Heterogeneous tuple (explicit types)
(defn tuple-field [field-name tuple-types]
  {:db/ident field-name
   :db/valueType :db.type/tuple
   :db/tupleTypes tuple-types
   :db/cardinality :db.cardinality/one})

;; Composite tuple (auto-derived from other attributes)
(defn composite-tuple-field [field-name tuple-attrs]
  {:db/ident field-name
   :db/valueType :db.type/tuple
   :db/tupleAttrs tuple-attrs
   :db/cardinality :db.cardinality/one})

;; Example: pagination tuple for invoices
(def invoice-pagination-tuple
  (composite-tuple-field :invoice/company+date+number
    [:invoice/company :invoice/issue-date :invoice/number]))
```

## Transacting Schema

```clojure
(defn install-schema! [conn]
  (d/transact conn {:tx-data (concat user-schema
                                     invoice-schema
                                     line-item-schema
                                     [invoice-pagination-tuple])}))
```

## Schema Migration Patterns

### Adding New Attributes

Simply transact the new attribute - it will apply to existing entities when they acquire values:

```clojure
(d/transact conn {:tx-data [(boolean-field :invoice/archived?)]})
```

### Establishing Composite Tuples on Existing Data

When you add a composite tuple to entities that already have values, you need to "touch" the constituent attributes:

```clojure
(defn establish-composite [conn {:keys [attr batch-size pacing-ms]}]
  (let [db (d/db conn)
        datoms (d/datoms db {:index :aevt :components [attr] :limit -1})]
    (doseq [batch (partition-all batch-size datoms)]
      (d/transact conn {:tx-data (mapv (fn [{:keys [e v]}]
                                         [:db/add e attr v])
                                   batch)})
      (Thread/sleep pacing-ms))))

;; Usage: re-assert :invoice/number to establish :invoice/company+number tuple
(establish-composite conn {:attr :invoice/number
                          :batch-size 1000
                          :pacing-ms 100})
```

## Attribute Naming Conventions

### Entity Namespace

Use the entity type as the attribute namespace:

```clojure
;; GOOD
:invoice/deleted?
:customer/deleted?

;; BAD - pollutes indexes when used in composite tuples
:entity/deleted?
```

### Composite Tuple Naming

Use `+` to join constituent attribute names:

```clojure
:invoice/company+date+number
:customer/company+name+filters
```

The `/filters` suffix often indicates a tuple designed for filtering/pagination that includes multiple filterable fields.

## Schema Introspection

```clojure
;; Find all ref attributes
(d/q '[:find ?ident
       :where
       [?e :db/ident ?ident]
       [?e :db/valueType :db.type/ref]]
  db)

;; Find all attributes for an entity type (by namespace convention)
(d/q '[:find ?ident
       :where
       [?e :db/ident ?ident]
       [(namespace ?ident) ?ns]
       [(= ?ns "invoice")]]
  db)

;; Get full attribute schema
(d/pull db '[*] :invoice/number)
;; => {:db/ident :invoice/number
;;     :db/valueType {:db/ident :db.type/long}
;;     :db/cardinality {:db/ident :db.cardinality/one}}
```

## Cardinality Considerations

### To-Many with Components

Components are automatically retracted when parent is retracted:

```clojure
;; Line items are components - delete cascade
{:db/ident :invoice/line-items
 :db/valueType :db.type/ref
 :db/cardinality :db.cardinality/many
 :db/isComponent true}

;; Retracting invoice also retracts all line-items
(d/transact conn {:tx-data [[:db/retractEntity [:invoice/id inv-id]]]})
```

### To-Many without Components

Referenced entities are NOT retracted with parent:

```clojure
;; Tags are shared, not owned
{:db/ident :invoice/tags
 :db/valueType :db.type/ref
 :db/cardinality :db.cardinality/many}

;; Retracting invoice removes tag REFERENCES but not tag entities
```
