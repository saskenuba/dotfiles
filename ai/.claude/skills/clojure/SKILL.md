---
name: clojure
description: Clojure programming with naming guidelines and performance best practices. Use when writing or reviewing Clojure/ClojureScript code.
---

# Clojure Programming

## Instructions

### Use the REPL

* `(require 'namespace)` once before using a namespace
* `(ns-publics 'namespace)` to find public members
* `(clojure.repl/doc symbol)` and `(clojure.repl/source symbol)` for documentation
* Run `clj-paren-repair <FILENAME>` to fix delimiter errors

### Keywords

Keywords represent distinct facts (like RDF identifiers):
* **NEVER** use two keywords for the same fact (e.g., `:device/id` and `:member-device/id`)
* Variations use namespace extension: `:member/street` vs `:member.snapshot/street`
* Enumerations extend storage key: `:member/gender` uses `:member.gender/male`

### Core Principles

1. **Naming**: Pure computations as nouns; state changes as verbs
2. **Eager Over Lazy**: Use `mapv`, `filterv`, `into`
3. **Transducers**: For multi-step transformations
4. **Batch Operations**: Process DB/API/I/O in batches, not one-by-one

### Naming Conventions

```clojure
;; Pure queries - name as values (GOOD)
(defn user-full-name [user] ...)
(defn total-revenue [invoices] ...)

;; AVOID imperative verbs for pure functions
(defn get-user-full-name [user] ...)    ; bad
(defn calculate-total-revenue [inv] ...) ; bad

;; Transformations - action verbs
(defn normalize-user [user] (update user :email str/lower-case))

;; Side effects - verbs with !
(defn save-invoice! [db invoice] ...)
```

### Docstrings

All non-private functions need docstrings:
* First sentence: what it does (e.g., "Returns the square root of `x`")
* Parameters in backticks woven into prose
* Complex args: bullet list with required/optional

```clojure
(defn write-member-data!
  "Writes `member-data` to database on `conn` using `tx-mode` (:none, :read-committed, :serializable).

   * `:member/id` - Required member UUID
   * Additional member details (optional)."
  [conn tx-mode member-data] ...)
```

### Performance: Eager Operations

```clojure
;; GOOD - eager
(->> users (mapv normalize-user) (filterv active?))

;; AVOID - lazy
(->> users (map normalize-user) (filter active?))
```

### Transducers

```clojure
;; Avoid intermediate collections
(defn invoice-totals [invoices]
  (transduce
    (comp (filter :paid?) (map :line-items) (mapcat identity) (map :amount))
    + 0M invoices))

;; Building collections
(into #{} (map :id) users)                    ; set of ids
(into {} (map (juxt :id identity)) users)     ; index by id
(into [] (comp (filter valid?) (map normalize)) items)
```

### Batching High-Throughput Operations

```clojure
;; AVOID
(run! save-invoice! invoices)

;; GOOD
(doseq [batch (partition-all 100 invoices)]
  (jdbc/insert-multi! db :invoices batch))
```

Batch sizes: 100-1000 typical. Smaller for large records/high latency, larger for small records.
Skip batching for <100 records or when individual error handling needed.

### Common Pitfalls

```clojure
;; Force realization with side effects
(map save! items)                    ; BAD - lazy, won't execute
(run! save! items)                   ; GOOD

;; Don't mix laziness with resources
(with-open [r (io/reader "f.txt")]
  (line-seq r))                      ; BAD - lazy escapes with-open
(with-open [r (io/reader "f.txt")]
  (into [] (line-seq r)))            ; GOOD

;; Early termination
(reduce (fn [_ x] (if (pred x) (reduced x) nil)) nil coll)
```
