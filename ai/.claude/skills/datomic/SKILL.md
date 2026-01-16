---
name: datomic
description: How to extract data from a Datomic database for maximum scalability and performance, with particular emphasis on leveraging standard query and indexes to get the best overall multiuser performance with emphasis on properly leveraging `d/q`, `d/index-pull`, `d/index-range`, `d/datoms` of the Clojure `datomic.client.api`.
---

# Datomic 

Datomic stores data as Datoms: A tuple containing [entity-id, attribute, value]. A to-one attribute allows only one datom per id/attribute, and a to-many attribute allows any number of values to share the same id/attribute.
These three items are generally referred to as the `E` (entity, because datoms with the same ID clump together as a bag of facts), 
`A` (for attribute) and `V` (for value). Thus, a shorthand for a Datom would be EAV. The Datom also includes a transaction ID (T) and added/retracted flag.

```clojure
(require '[datomic.client.api :as d])

;; Get current database value from connection
(def db (d/db conn))

;; Database values are immutable - safe to pass around
(:t db)  ;; basis t (transaction number)
```

**CRITICAL**: All Datomic functions (except `q`) have a **default result limit of 1000**. Include `:limit -1` for unlimited results. This is a very common source of silent bugs in production.

## Core Operations

### Pull - Entity Retrieval

Pull retrieves a tree of data starting from an entity:

```clojure
;; By lookup ref (preferred when you have a unique ID)
(d/pull db [:invoice/id :invoice/number :invoice/date] [:invoice/id some-uuid])

;; By db/id
(d/pull db '[*] 12345678)

;; With nested joins
(d/pull db [:invoice/number {:invoice/customer [:customer/name :customer/email]}] 
        [:invoice/id invoice-id])

;; Pull returns {} if entity doesn't exist (not nil!)
(d/pull db [:invoice/id] [:invoice/id non-existent-uuid])  ;; => {}
```

### Query with Pull (Common Pattern)

```clojure
;; Find entity and pull in one operation
(ffirst (d/q '[:find (pull ?e pattern)
               :in $ pattern ?value
               :where [?e :invoice/number ?value]]
          db '[*] invoice-number))

;; Multiple entities with pull
(d/q '[:find (pull ?e [:invoice/id :invoice/number])
       :where [?e :invoice/company ?c]
              [?c :company/id ?cid]]
  db)
```

### Transactions

Transactions use `:tx-data` containing maps or list forms:

```clojure
;; Map form (upsert - creates or updates based on :db/id or identity attribute)
(d/transact conn {:tx-data [{:invoice/id (random-uuid)
                              :invoice/number 42
                              :invoice/customer [:customer/id customer-uuid]}]})

;; List form for specific operations
(d/transact conn {:tx-data [[:db/add [:invoice/id inv-id] :invoice/sent? true]
                            [:db/retract [:invoice/id inv-id] :invoice/draft? true]
                            [:db/retractEntity [:invoice/id old-inv-id]]]})

;; Transaction result
(let [{:keys [db-before db-after tx-data tempids]} (d/transact conn {:tx-data [...]})]
  ;; db-after is the new database value
  ;; tempids maps "temp-1" style ids to real db/ids
  )
```

**Transaction Options**: `:timeout` (ms, default 60000).

### Speculative Transactions

Use `d/with` for what-if analysis without affecting the database:

```clojure
(let [with-db (d/with-db conn)
      result (d/with with-db {:tx-data [{:invoice/id new-id ...}]})]
  ;; result has same shape as transact result
  ;; but nothing was persisted
  (:db-after result))
```

## Indexes

Datomic creates *covering* indexes of datoms for different sorting orders:

* **EAVT**: Entity → Attribute → Value → Tx (all datoms)
* **AEVT**: Attribute → Entity → Value → Tx (all datoms)  
* **AVET**: Attribute → Value → Entity → Tx (all datoms)
* **VAET**: Value → Attribute → Entity → Tx (ONLY `:db.type/ref` attributes)

These indexes are directly accessible with `d/datoms` (any index) and `d/index-range` (AVET only).

### Direct Index Access (d/datoms)

```clojure
;; Fast lookup-ref to db/id conversion (100x faster than query)
(defn lookup-ref->id [db [attr id :as ref]]
  (-> (d/datoms db {:index :avet :components ref :limit 1}) first :e))

;; Check if entity exists
(defn lookup-ref-exists? [db ref]
  (boolean (seq (d/datoms db {:index :avet :components ref :limit 1}))))

;; Get all attributes of an entity  
(d/datoms db {:index :eavt :components [entity-id] :limit -1})

;; Get all entities with a specific attribute
(d/datoms db {:index :aevt :components [:invoice/number] :limit -1})
```

### Index Range (d/index-range)

Efficient range scans on AVET index:

```clojure
;; Find invoices numbered 100-199
(d/index-range db {:attrid :invoice/number
                   :start 100    ;; inclusive
                   :end 200      ;; exclusive
                   :limit -1})
```

## Standard Query (d/q)

Clauses are evaluated in ORDER. Each clause results in a set of facts that match "so far". Successive clauses unify on matching bindings.

```clojure
(d/q '[:find ?v
       :in $ ?x
       :where
       [?e :attr/a ?x]   ;; First clause: AVET lookup, binds ?e
       [?e :attr/b ?v]]  ;; Second clause: EAVT lookup for each ?e
  db 42)
```

**Clause Order Matters**: Put the most selective clause first:

```clojure
;; GOOD: invoice/number is likely unique per company
(d/q '[:find ?e
       :in $ ?company ?n
       :where
       [?e :invoice/number ?n]      ;; Very selective first
       [?e :invoice/company ?company]]
  db company-id 42)

;; BAD: company might have 100k invoices  
(d/q '[:find ?e
       :in $ ?company ?n
       :where
       [?e :invoice/company ?company]  ;; Could match 100k entities
       [?e :invoice/number ?n]]        ;; Then filter
  db company-id 42)
```

**Query Options**:

```clojure
(d/q {:query '[:find ...]
      :args [db value]
      :timeout 300000  ;; 5 minutes (network only - query continues on server!)
      :limit 5})       ;; Post-processing limit only, doesn't speed up query
```

## Composite Tuples

The most important feature for optimal query and pagination. A composite tuple is an auto-derived attribute whose value combines other attributes in that entity.

```clojure
;; Schema definition
{:db/ident       :invoice/company+number
 :db/valueType   :db.type/tuple
 :db/tupleAttrs  [:invoice/company :invoice/number]
 :db/cardinality :db.cardinality/one}
```

**Critical Rules**:
- Tuples have a **hard limit of 8 slots**
- Tuples **cannot be removed** once created - never experiment on production!
- A tuple value is created if *any* entity has *any* of the named attributes
- Missing attributes hold a `nil` (nil sorts first)
- Design for maximal utility across use-cases

### Using Tuples for Pagination

```clojure
;; Paginate company's invoices
(d/index-pull db {:index :avet
                  :selector [:invoice/id :invoice/number :invoice/date]
                  :start [:invoice/company+number [company-dbid nil]]
                  :end [:invoice/company+number [(inc company-dbid) nil]] ; non-inclusive, so this includes ALL of this companies entries
                  :offset 100
                  :limit 10})
```

For complex filtering/pagination patterns with composite tuples, see [performance-patterns.md](performance-patterns.md).

## Time Travel

```clojure
;; Database as of a specific point
(d/as-of db #inst "2024-01-01")
(d/as-of db tx-id)

;; Database since a specific point (only facts added after)
(d/since db #inst "2024-01-01")

;; History database (includes retractions, use :added to distinguish)
(let [hist-db (d/history db)]
  (d/q '[:find ?e ?v ?tx ?added
         :where [?e :invoice/status ?v ?tx ?added]]
    hist-db))
```

## Schema Definition

For schema definition patterns and helpers, see [schema-patterns.md](schema-patterns.md).

## Key Antipatterns

### Attribute Reuse Across Entity Types

**BAD**: Reusing attributes pollutes indexes and composite tuples:

```clojure
;; DON'T: :entity/deleted? on all entity types
{:db/id 1 :entity/deleted? true :invoice/company 99}
{:db/id 2 :entity/deleted? true :product/sku "foo"}

;; Tuple :invoice/company+deleted+number would include:
;; [2 :invoice/company+deleted+number [nil true nil]]  ;; useless product entry!
```

**GOOD**: Namespace by entity type:

```clojure
{:db/id 1 :invoice/deleted? true :invoice/company 99}
{:db/id 2 :product/deleted? true :product/sku "foo"}
```

### Using q for Pagination

**BAD**: `d/q` calculates the complete result set before limit/offset:

```clojure
;; DON'T: This calculates ALL matching invoices, then takes 10
(take 10 (drop 100 (sort (d/q '[:find ?e :where ...] db))))
```

**GOOD**: Use composite tuples with `index-range` or `index-pull`.

### Ignoring the 1000 Limit

```clojure
;; DANGEROUS: Silently truncates at 1000
(d/datoms db {:index :aevt :components [:invoice/number]})

;; SAFE: Explicit unlimited, and lazy; however, be careful this could be a lot of processing
(d/datoms db {:index :aevt :components [:invoice/number] :limit -1})
```
