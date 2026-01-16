# Datomic Performance Patterns

Advanced patterns for high-performance data access in multi-user environments.

## Index-Pull Considerations

`index-pull` combines an index walk with a pull of facts for each entity. It's lazy and chunked.

**Warning**: Chunking can cause over-fetching:

```clojure
;; PROBLEMATIC: index-pull may process 100 items to chunk networking,
;; even if you only need 10 after filtering
(take 10
  (filter invoice-this-month?
    (d/index-pull db
      {:index :avet
       :start [:invoice/number 0]
       :selector [:invoice/number :invoice/date {:invoice/customer [:customer/name]}]})))
```

**Better**: Use `index-range` for filtering, then pull only what you need:

```clojure
(let [filtered-ids (->> (d/index-range db {:attrid :invoice/company+date
                                            :start [company-id start-date]
                                            :end [company-id end-date]
                                            :limit -1})
                        (filter my-filter?)
                        (take 10)
                        (mapv :e))]
  (d/q '[:find (pull ?e pattern)
         :in $ pattern [?e ...]]
    db selector filtered-ids))
```

## Composite Tuple Pagination Strategy

For filtered/paginated reports, design tuples strategically:

### Tuple Element Order

1. **Ownership** first (company, user) - almost always required for security
2. **Primary filter** second (date range, status) - commonly specified by user
3. **Secondary filters** (deleted?, category) - for post-filtering
4. **Display values** (name, number) - avoid additional I/O for common display fields

```clojure
;; Good tuple design for invoice report
{:db/ident :invoice/company+date+deleted+number
 :db/valueType :db.type/tuple
 :db/tupleAttrs [:invoice/company :invoice/issue-date :invoice/deleted? :invoice/number]
 :db/cardinality :db.cardinality/one}
```

### Range + Filter Pattern

```clojure
(defn paginated-invoices [db company-id {:keys [start-date end-date deleted? offset limit]}]
  (let [;; Read complete range from index (fast sequential I/O)
        datoms (d/index-range db {:attrid :invoice/company+date+deleted+number
                                  :start [company-id start-date]
                                  :end [(inc company-id)]  ;; non-inclusive
                                  :limit -1})
        ;; Post-filter and extract
        matches (->> datoms
                     (filter (fn [{:keys [v]}]
                               (let [[_ date del? _] v]
                                 (and (or (nil? end-date) (< (compare date end-date) 0))
                                      (or (nil? deleted?) (= del? deleted?))))))
                     (drop offset)
                     (take limit))]
    ;; Convert tuples to maps (no additional I/O needed!)
    (mapv (fn [{:keys [e v]}]
            {:db/id e
             :invoice/company (nth v 0)
             :invoice/issue-date (nth v 1)
             :invoice/deleted? (nth v 2 false)
             :invoice/number (nth v 3)})
      matches)))
```

### When You Need More Fields

If the tuple doesn't contain all display fields, use a batched pull:

```clojure
(let [basic-results (paginated-invoices db company-id opts)
      ids (mapv :db/id basic-results)
      full-data (d/q '[:find (pull ?e [:invoice/id :invoice/number :invoice/customer-name])
                       :in $ [?e ...]]
                  db ids)]
  full-data)
```

## Query Clause Optimization

### Understanding Clause I/O

Each clause generates index lookups. The first clause sets the candidate set size.

```clojure
;; Clause I/O analysis:
(d/q '[:find ?e
       :in $ ?company ?status
       :where
       ;; Clause 1: AVET on :invoice/company - could be 100k matches
       [?e :invoice/company ?company]
       ;; Clause 2: For EACH ?e, EAVT lookup on :invoice/status
       ;; 100k random seeks!
       [?e :invoice/status ?status]]
  db company-id :paid)
```

**Better with composite tuple** (single sequential scan):

```clojure
(d/index-range db {:attrid :invoice/company+status
                   :start [company-id :paid]
                   :end [company-id :paid-exclusive-marker]})
```

### Segment Awareness

Datomic stores indexes in segments (~2000 datoms each). Adjacent datoms are likely in memory.

```clojure
;; Good: Sequential access uses few segments
(d/index-range db {:attrid :invoice/number :start 1000 :end 2000})

;; Potentially expensive: Random access across many segments  
(doseq [id scattered-invoice-ids]
  (d/pull db '[*] [:invoice/id id]))
```

## Large Query Detection

Monitor queries that return large result sets:

```clojure
(defn monitored-q [query & args]
  (let [result (apply d/q query args)
        count (count result)]
    (when (> count 2000)
      (log/warn {:msg "LargeQuery" :count count :query query}))
    result))
```

## Refs in Tuples

When composite tuples include ref attributes, the value is the raw `:db/id`, not a lookup ref:

```clojure
;; Tuple includes :invoice/customer (a ref)
;; The tuple value will be [<db-id-of-company> <db-id-of-customer> ...]

;; For index-range, you MUST convert UUIDs to db/ids first:
(let [company-dbid (-> (d/datoms db {:index :avet 
                                      :components [:company/id company-uuid]
                                      :limit 1})
                       first :e)]
  (d/index-range db {:attrid :invoice/company+number
                     :start [company-dbid]
                     :end [(inc company-dbid)]}))
```

## Timeouts and Their Limits

```clojure
;; :timeout affects network only - query continues running on server!
(d/q {:query '[:find ...]
      :args [db]
      :timeout 5000})  ;; Client gives up after 5s, server keeps computing

;; :limit in q is post-processing only - doesn't speed up the query
(d/q {:query '[:find ...]
      :args [db]
      :limit 10})  ;; Full result calculated, then truncated
```

For true performance control, use composite tuples with `index-range` which has real offset/limit.
