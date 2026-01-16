---
name: clojure
description: Clojure programming with naming guidelines and performance best practices. Use when writing or reviewing Clojure/ClojureScript code.
---

# Clojure Programming

## Instructions

### Core Principles

1. **Referential Transparency in Naming**: Name pure queries/computations as values (nouns), avoiding imperative verbs. Name changes in program state with action verbs.
2. **Eager Over Lazy**: Use `mapv`, `filterv`, `into` for predictable performance
3. **Transducers for Pipelines**: Compose transformations efficiently without intermediate collections
4. **Batch High-Throughput Operations**: Process data in batches for DB/API/I/O instead of one-by-one processing

### Naming Conventions

**Pure Queries/Computations** - Name as resulting values:
```clojure
;; GOOD
(defn user-full-name [user]
  (str (:first-name user) " " (:last-name user)))

(defn total-revenue [invoices]
  (transduce (map :amount) + 0 invoices))

;; AVOID - Imperative verbs
(defn get-user-full-name [user] ...)
(defn calculate-total-revenue [invoices] ...)
(defn fetch-active-subscriptions [subscriptions] ...)
```

**Transformations** - Use action verbs:
```clojure
(defn normalize-user [user]
  (update user :email str/lower-case))

(defn validate-invoice [invoice]
  (assoc invoice :valid? (valid-invoice? invoice)))

(defn enrich-record [record metadata]
  (merge record metadata))
```

**Side Effects** - Use verbs with `!` suffix:
```clojure
(defn save-invoice! [db invoice] ...)
(defn send-email! [recipient message] ...)
```

### Performance: Eager Operations

Default to eager operations:
```clojure
;; GOOD - Eager, predictable
(defn process-users [users]
  (->> users
       (mapv normalize-user)
       (filterv active?)
       (mapv add-computed-fields)))

;; AVOID - Lazy sequences
(defn process-users [users]
  (->> users
       (map normalize-user)
       (filter active?)
       (map add-computed-fields)))
```

Use `into` for building collections:
```clojure
(defn user-ids [users]
  (into #{} (map :id) users))

(defn index-users [users]
  (into {} (map (juxt :id identity)) users))

(defn process-items [items]
  (into []
        (comp (filter valid?)
              (map normalize)
              (map enrich))
        items))
```

### Transducers for Pipelines

Use transducers to avoid intermediate collections:
```clojure
;; Basic pattern
(def xf
  (comp
    (filter active?)
    (map :amount)
    (map bigdec)))

(transduce xf + 0 subscriptions)
(into [] xf subscriptions)

;; Real-world example
(defn invoice-totals [invoices]
  (transduce
    (comp
      (filter :paid?)
      (map :line-items)
      (mapcat identity)
      (map :amount))
    +
    0M
    invoices))

(defn process-records [records]
  (into []
        (comp
          (filter valid-record?)
          (map normalize-record)
          (map add-timestamp)
          (map add-metadata))
        records))
```

Stateful transducers:
```clojure
(into [] (partition-all 100) large-collection)
(into [] (comp (drop 10) (take 50)) items)
(into [] (distinct) items)
```

### High-Throughput Operations

**Always batch high-volume operations:**
```clojure
;; AVOID - One operation per record
(run! save-invoice! invoices)
(doseq [user users]
  (insert-user! db user))

;; GOOD - Batched operations
(defn saved-invoices! [invoices]
  (->> invoices
       (partition-all 100)
       (run! save-invoice-batch!)))

(defn inserted-users! [db users]
  (doseq [batch (partition-all 500 users)]
    (insert-user-batch! db batch)))
```

**Common batching contexts:**
```clojure
;; Database operations
(defn import-transactions! [db transactions]
  (doseq [batch (partition-all 1000 transactions)]
    (jdbc/insert-multi! db :transactions batch)))

;; CSV/Excel imports
(defn import-spreadsheet-data! [file-path]
  (with-open [reader (io/reader file-path)]
    (->> (csv/read-csv reader)
         (drop 1)
         (partition-all 500)
         (run! (fn [batch]
                 (let [records (into [] (map row->record) batch)]
                   (save-records-batch! records)))))))

;; API bulk operations
(defn sync-records! [api-client records]
  (doseq [batch (partition-all 250 records)]
    (api/bulk-create api-client batch)))
```

**Batch size guidelines:**

If you don't have enough context, ask for:
- Database/API batch size limits
- Average record size
- Available memory
- Network latency
- Expected dataset size

General starting points:
- 100-1000 records per batch
- Smaller (50-100) for large records or high latency
- Larger (1000+) for small records and low latency

**When NOT to batch:**
- Low-volume operations (< 100 records)
- Individual error handling required per record
- Real-time processing where latency > throughput
- System doesn't support batch operations

### Threading Macros vs Transducers

**Use transducers for collection pipelines:**
```clojure
;; GOOD - No intermediate collections
(defn revenue-report [invoices]
  (transduce
    (comp
      (filter :paid?)
      (map :amount))
    +
    0M
    invoices))

;; AVOID - Creates intermediate vectors
(defn revenue-report [invoices]
  (->> invoices
       (filterv :paid?)
       (mapv :amount)
       (reduce + 0M)))
```

**Use threading for single values:**
```clojure
;; -> for single-value transformations
(defn user-summary [user-id db]
  (-> user-id
      (user-by-id db)
      user-full-name
      (str " (" (:email user) ")")))

;; as-> when threading position varies
(defn complex-transform [data]
  (as-> data $
    (assoc $ :normalized true)
    (update $ :items into [] (map normalize-item))
    (merge default-config $)))
```

### Data Structure Selection

```clojure
;; Vectors for ordered, indexed access
(defn sort-users [users]
  (vec (sort-by :name users)))

;; Sets for uniqueness
(defn unique-ids [items]
  (into #{} (map :id) items))

;; Maps for lookup
(defn users-by-id [users]
  (into {} (map (juxt :id identity)) users))
```

### Common Pitfalls

**Force lazy sequence realization with side effects:**
```clojure
;; BAD
(map save-to-db! items)

;; GOOD
(run! save-to-db! items)
(mapv save-to-db! items)
(doseq [item items] (save-to-db! item))
```

**Don't mix laziness with resources:**
```clojure
;; BAD
(with-open [rdr (io/reader "file.txt")]
  (line-seq rdr))

;; GOOD
(with-open [rdr (io/reader "file.txt")]
  (into [] (line-seq rdr)))
```

**Use `reduced` for early termination:**
```clojure
(defn first-match [pred coll]
  (reduce
    (fn [_ item]
      (if (pred item)
        (reduced item)
        nil))
    nil
    coll))
```

### Function Composition

```clojure
;; comp for rightmost-first application
(def process-user
  (comp
    add-metadata
    normalize-fields
    validate-user))

;; Transducer composition
(def xf-pipeline
  (comp
    (filter valid?)
    (map normalize)
    (mapcat expand)
    (filter ready?)))

(into [] xf-pipeline items)
```

### Pattern Matching

```clojure
(require '[clojure.core.match :refer [match]])

(defn invoice-status [invoice]
  (match [(:paid? invoice) (:overdue? invoice) (:cancelled? invoice)]
    [true _ _] :paid
    [false true _] :overdue
    [false false true] :cancelled
    [false false false] :pending))
```

### Performance Checklist

Before finalizing code, verify:
- [ ] Using `mapv`/`filterv` instead of lazy `map`/`filter`?
- [ ] Transducers for multi-step transformations?
- [ ] `into` for building collections efficiently?
- [ ] Batching high-throughput operations (DB, API, I/O)?
- [ ] Forcing realization when doing side effects?
- [ ] Appropriate data structure (vector/set/map)?
- [ ] Early termination with `reduced` when possible?

## Examples

### Example 1: Invoice Processing Pipeline

```clojure
;; Process and aggregate invoice data
(defn revenue-by-status [invoices]
  (into {}
        (comp
          (map (juxt :status :amount))
          (partition-by first)
          (map (fn [group]
                 [(ffirst group)
                  (transduce (map second) + 0M (map second group))])))
        invoices))

;; Batch save to database
(defn save-invoices! [db invoices]
  (doseq [batch (partition-all 100 invoices)]
    (jdbc/insert-multi! db :invoices batch)))
```

### Example 2: User Data Transformation

```clojure
;; Transform and validate users
(defn process-users [users]
  (into []
        (comp
          (map normalize-user)
          (filter valid-user?)
          (map add-computed-fields))
        users))

;; Helper functions using proper naming
(defn normalize-user [user]
  (-> user
      (update :email str/lower-case)
      (update :name str/trim)))

(defn user-full-name [user]
  (str (:first-name user) " " (:last-name user)))
```

### Example 3: CSV Import with Batching

```clojure
(defn import-csv! [db file-path]
  (with-open [reader (io/reader file-path)]
    (let [rows (->> (csv/read-csv reader)
                    (drop 1)
                    (into []))]
      (->> rows
           (partition-all 500)
           (run! (fn [batch]
                   (let [records (into [] (map parse-row) batch)]
                     (jdbc/insert-multi! db :records records))))))))

(defn parse-row [row]
  {:name (nth row 0)
   :amount (parse-decimal (nth row 1))
   :date (parse-date (nth row 2))})
```

### Example 4: Aggregation with Transducers

```clojure
;; Group and aggregate in one pass
(defn sales-by-region [transactions]
  (transduce
    (comp
      (filter :completed?)
      (map (juxt :region :amount))
      (partition-by first))
    (completing
      (fn [acc group]
        (assoc acc
               (ffirst group)
               (transduce (map second) + 0M group))))
    {}
    transactions))
```
