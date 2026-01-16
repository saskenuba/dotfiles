---
name: guardrails
description: How to write Malli schemas for Guardrails function specifications in Clojure/ClojureScript. This skill teaches the schema types available, map schema patterns, and best practices for type-safe function definitions using >defn.
---

You are an expert in writing Malli schemas for Guardrails function specifications. Guardrails enables runtime validation
of function arguments and return values using Malli schemas, providing type safety without sacrificing Clojure's dynamic
nature.

## Core Concepts

Guardrails uses `>defn` to define functions with Malli type specifications. The specification follows a "gspec" syntax
that declares argument types and return types:

```clojure
(ns your.namespace
  (:require
    [com.fulcrologic.guardrails.malli.core :refer [=> >defn]]))

(>defn function-name
  "Docstring"
  [arg1 arg2]
  [schema1 schema2 => return-schema]
  (function-body))
```

**Key operators:**

- `=>` separates input schemas from output schema
- `?` macro creates nullable schemas: `(? :string)` = `[:maybe :string]`
- `|` describes side effects (rarely used)

## Primitive Type Schemas

Use these keyword schemas for basic types:

| Schema               | Matches             | Properties                                    |
|----------------------|---------------------|-----------------------------------------------|
| `:any`               | Any value           | -                                             |
| `:some`              | Any non-nil value   | -                                             |
| `:nil`               | Only nil            | -                                             |
| `:string`            | Strings             | `:min`, `:max` for length                     |
| `:int`               | Integers            | `:min`, `:max` for range                      |
| `:float`             | Float numbers       | -                                             |
| `:double`            | Double numbers      | `:min`, `:max`, `:gen/NaN?`, `:gen/infinite?` |
| `:boolean`           | true/false          | -                                             |
| `:keyword`           | Any keyword         | -                                             |
| `:qualified-keyword` | Namespaced keywords | `:namespace` property                         |
| `:symbol`            | Any symbol          | -                                             |
| `:qualified-symbol`  | Namespaced symbols  | `:namespace` property                         |
| `:uuid`              | UUIDs               | -                                             |

**Examples:**

```clojure
(>defn greet
  "Returns a greeting string."
  [name]
  [:string => :string]
  (str "Hello, " name "!"))

(>defn add
  "Adds two integers."
  [a b]
  [:int :int => :int]
  (+ a b))

(>defn calculate-ratio
  "Returns a ratio as a double."
  [numerator denominator]
  [:number :number => :double]
  (double (/ numerator denominator)))
```

## Predicate Schemas

Standard Clojure predicates work as schemas:

```clojure
;; Number predicates
number? integer? int? pos-int? neg-int? nat-int?
pos? neg? float? double? zero?

;; Type predicates
boolean? string? keyword? symbol? uuid? uri? inst?

;; Qualified identifier predicates
ident? simple-ident? qualified-ident?
simple-keyword? qualified-keyword?
simple-symbol? qualified-symbol?

;; Collection predicates
map? vector? list? seq? set? coll?
seqable? indexed? associative? sequential?

;; Function predicates
ifn? fn?

;; Other
any? some? nil? false? true? char?
```

**When to use predicates vs keyword schemas:**

- Use `:string` when you need properties like `:min`/`:max`
- Use `string?` for simple type checking
- Keyword schemas (`:int`, `:string`) are preferred for clarity

## Collection Schemas

### Vector Schema

For homogeneous vectors:

```clojure
[:vector :int]           ; Vector of integers
[:vector {:min 1} :int]  ; Non-empty vector of integers
[:vector {:min 1 :max 10} :string]  ; 1-10 strings
```

### Set Schema

For homogeneous sets:

```clojure
[:set :keyword]          ; Set of keywords
[:set {:min 1} :string]  ; Non-empty set of strings
```

### Sequential Schema

For any sequential collection:

```clojure
[:sequential :int]       ; List, vector, or seq of ints
```

### Tuple Schema

For fixed-length heterogeneous vectors:

```clojure
[:tuple :double :double]           ; [lat, lon] pair
[:tuple :keyword :string :int]     ; [:type "name" 42]
```

## Map Schemas (Critical Section)

Maps are the most important schema type for domain modeling.

### Basic Map Schema

```clojure
[:map
 [:key1 schema1]
 [:key2 schema2]]
```

Each entry is `[key-name schema]` or `[key-name properties schema]`.

### Required vs Optional Keys

```clojure
[:map
 [:name :string]                          ; Required
 [:email :string]                         ; Required
 [:nickname {:optional true} :string]]    ; Optional
```

### Closed Maps

By default, maps allow extra keys. Close them to reject extras:

```clojure
[:map {:closed true}
 [:x :int]
 [:y :int]]

;; Valid: {:x 1 :y 2}
;; Invalid: {:x 1 :y 2 :z 3}
```

### Nested Maps

```clojure
[:map
 [:user [:map
         [:id :uuid]
         [:name :string]
         [:email :string]]]
 [:order [:map
          [:id :int]
          [:total :number]]]]
```

### Qualified Keywords in Maps

For spec-like decomplected maps, use qualified keywords with a registry:

```clojure
;; Define schemas in registry
(>def :user/id :uuid)
(>def :user/name :string)
(>def :user/email [:string {:min 5}])

;; Use in map - key is both the map key and schema reference
[:map
 :user/id      ; Key ::user/id with schema from registry
 :user/name
 :user/email]

;; Data shape: {:user/id #uuid"..." :user/name "Bob" :user/email "bob@example.com"}
```

### Map-of Schema

For homogeneous key-value maps:

```clojure
[:map-of :string :int]           ; {"a" 1 "b" 2}
[:map-of :keyword [:map [:x :int] [:y :int]]]  ; {:point1 {:x 1 :y 2}}
```

## Composite Schemas

### Maybe (Nullable)

Use the `?` macro for nullable values:

```clojure
(>defn find-user
  [id users]
  [:uuid [:vector :map] => (? [:map [:id :uuid] [:name :string]])]
  (first (filter #(= id (:id %)) users)))
```

Or use `:maybe` directly:

```clojure
[:maybe :string]  ; String or nil
```

### Enum

For fixed set of values:

```clojure
[:enum :pending :active :completed]
[:enum "small" "medium" "large"]
[:enum nil 1 2 3]  ; Note: nil requires explicit nil as first value
```

### And (Intersection)

All schemas must match:

```clojure
[:and :int [:> 0] [:< 100]]  ; Integer between 1-99
[:and :string [:fn #(> (count %) 3)]]  ; String longer than 3 chars
```

### Or (Union)

Any schema may match:

```clojure
[:or :string :int]           ; String or integer
[:or :keyword :string :int]  ; Any of these
```

### Orn (Named Union)

Named alternatives for better error messages:

```clojure
[:orn
 [:by-id :uuid]
 [:by-email :string]
 [:by-name [:tuple :string :string]]]
```

### Multi (Dispatch)

For polymorphic types:

```clojure
[:multi {:dispatch :type}
 [:user [:map [:type [:= :user]] [:name :string]]]
 [:admin [:map [:type [:= :admin]] [:permissions [:set :keyword]]]]]
```

## Comparator Schemas

```clojure
[:> 0]        ; Greater than 0
[:>= 0]       ; Greater than or equal to 0
[:< 100]      ; Less than 100
[:<= 100]     ; Less than or equal to 100
[:= 42]       ; Exactly 42
[:not= 0]     ; Not zero
```

## Custom Predicate Schemas

Use `:fn` for arbitrary validation:

```clojure
[:fn pos?]                                    ; Positive number
[:fn {:error/message "must be even"} even?]   ; Even with custom error
[:fn (fn [{:keys [x y]}] (> x y))]            ; x must be greater than y
```

## Sequence Regex Schemas

For validating sequences with patterns:

```clojure
;; Concatenation
[:cat :keyword :string :int]     ; [:foo "bar" 42]

;; Named concatenation
[:catn
 [:op :keyword]
 [:name :string]
 [:value :int]]

;; Repetition
[:* :int]                        ; Zero or more ints
[:+ :int]                        ; One or more ints
[:? :int]                        ; Zero or one int
[:repeat {:min 2 :max 4} :int]   ; 2-4 ints
```

## Registry for Reusable Schemas

Define reusable schemas with `>def`:

```clojure
;; Simple type aliases
(>def :user/id :uuid)
(>def :user/email [:string {:min 5}])
(>def :money/amount [:double {:min 0}])

;; Complex domain schemas
(>def :domain/user
  [:map
   [:id :user/id]
   [:email :user/email]
   [:name :string]
   [:created-at inst?]])

(>def :domain/order
  [:map
   [:id :uuid]
   [:user-id :user/id]
   [:items [:vector [:map
                     [:product-id :uuid]
                     [:quantity pos-int?]
                     [:price :money/amount]]]]
   [:total :money/amount]
   [:status [:enum :pending :paid :shipped :delivered]]])
```

Then use in functions:

```clojure
(>defn create-order
  [user items]
  [:domain/user [:vector :map] => :domain/order]
  ...)
```

## Best Practices

### 1. Prefer Keyword Schemas Over Predicates

```clojure
;; Preferred
[:int :string => :boolean]

;; Less clear
[int? string? => boolean?]
```

### 2. Use :number for Generic Numeric Input

When a function works with any number:

```clojure
(>defn square
  [x]
  [:number => :number]  ; Not :int or :double
  (* x x))
```

### 3. Create Registry Entries for Domain Types

Don't repeat complex schemas:

```clojure
;; Bad - repeated everywhere
(>defn process-user
  [user]
  [[:map [:id :uuid] [:name :string] [:email :string]] => :boolean]
  ...)

;; Good - reusable
(>def :domain/user [:map [:id :uuid] [:name :string] [:email :string]])

(>defn process-user
  [user]
  [:domain/user => :boolean]
  ...)
```

### 4. Use Qualified Keywords for Cross-Namespace Consistency

```clojure
(>def :order/id :uuid)
(>def :order/status [:enum :pending :paid :shipped])
(>def :order/total [:double {:min 0}])
```

### 5. Mark Optional Keys Explicitly

```clojure
[:map
 [:required-field :string]
 [:optional-field {:optional true} :string]]
```

### 6. Use (?) for Nullable Return Values

```clojure
(>defn find-by-id
  [id items]
  [:uuid [:vector :map] => (? :map)]  ; May return nil
  (first (filter #(= id (:id %)) items)))
```

### 7. Avoid Over-Specification

Don't constrain more than necessary:

```clojure
;; Over-specified - breaks if data shape changes
[:map {:closed true}
 [:x [:int {:min 0 :max 1000}]]
 [:y [:int {:min 0 :max 1000}]]]

;; Appropriate for most cases
[:map
 [:x :int]
 [:y :int]]
```

### 8. Map Schema Depth: Specify Only Local Requirements

**This is the most important principle for map schemas.**

A function's map schema should describe what *that function locally needs*, not everything the map might contain. The
temptation is to make schemas "too deep" by specifying all nested structure, but this creates maintenance nightmares.

**The Principle:**

1. Specify keys the function directly accesses
2. Never go deeper - called functions may change their requirements
3. Assume functions might require *less* in the future, so transitive assertions should be sparse

```clojure
;; BAD - Too deep, specifies everything
(>defn process-person
  [person]
  [[:map
    [:id :uuid]
    [:name :string]
    [:email :string]
    [:address [:map                    ; We don't use address!
               [:street :string]
               [:city :string]
               [:zip :string]]]
    [:preferences [:map                ; We don't use preferences!
                   [:theme :keyword]
                   [:notifications :boolean]]]]
   => :boolean]
  ;; But we only use :id and :name...
  (log-action (:id person) (:name person))
  true)

;; GOOD - Specifies only what we locally need
(>defn process-person
  [person]
  [[:map [:id :uuid] [:name :string]] => :boolean]
  (log-action (:id person) (:name person))
  true)
```

**When passing maps to other functions:**

```clojure
;; If f calls g, h, and i, and all require :person/id...
;; Might include that ONE key as a hint to callers, but don't go deeper

(>defn f
  "Processes person through multiple handlers."
  [person]
  [[:map [:person/id :uuid]] => :any]  ; Hint: callees need :person/id (and probably won't change that)
  (g person)
  (h person)
  (i person))

;; DON'T try to union all requirements of g, h, i - that's brittle
;; If g changes to need :person/email, you'd have to update f's schema
```

**Why this matters:**

- Refactoring becomes easier - change a called function without updating all callers' schemas
- Functions may require *less* over time as they're simplified
- Deep schemas create false coupling between layers

**For domain types in the registry:**

```clojure
;; Registry types CAN be complete - they represent the full domain concept
(>def :domain/person
  [:map
   [:id :uuid]
   [:name :string]
   [:email :string]
   [:address [:map [:street :string] [:city :string]]]])

;; But function schemas should still be minimal
(>defn get-person-greeting
  [person]
  [[:map [:name :string]] => :string]  ; Only needs :name
  (str "Hello, " (:name person) "!"))

;; Callers with a full :domain/person can pass it - the schema is open
;; The function documents it only needs :name
```

### 9. Document Complex Schemas

```clojure
(>def :api/response
  "Standard API response wrapper.
   :data contains the payload, :meta has pagination info."
  [:map
   [:data :any]
   [:meta {:optional true} [:map
                            [:page pos-int?]
                            [:per-page pos-int?]
                            [:total nat-int?]]]])
```

## Complete Example

```clojure
(ns myapp.orders
  (:require
    [com.fulcrologic.guardrails.malli.core :refer [=> >def >defn ?]]))

;; Registry definitions
(>def :order/id :uuid)
(>def :order/status [:enum :pending :paid :shipped :delivered :cancelled])
(>def :money/amount [:double {:min 0}])

(>def :order/item
  [:map
   [:item/quantity pos-int?]
   [:product/id :uuid]
   [:product/name :string]
   [:product/unit-price :money/amount]])

(>def :domain/order
  [:map
   :order/id
   :order/status 
   [:order/customer-email :string]
   [:order/items [:vector :order/item]]
   [:order/total :money/amount]
   [:order/notes {:optional true} :string]])

;; Pure functions
(>defn calculate-item-total
  "Calculate total for a single line item."
  [item]
  ;; Explicit requirements, not the (temptation) of :order/item!!!
  [[:map
    [:item/quantity pos-int?]
    [:product/unit-price :money/amount]] => :money/amount]
  (* (:item/quantity item) (:product/unit-price item)))

(>defn calculate-order-total
  "Sum all item totals."
  [items]
  ;; This function only cares that they are maps
  [[:vector map?] => :money/amount]
  (reduce + 0 (map calculate-item-total items)))

(>defn order-can-ship?
  "Check if order is ready to ship."
  [order]
  ;; Note that order/items isn' needed because false is a possible outcome
  [[:map :order/status] => :boolean]
  (and (= :paid (:order/status order))
    (seq (:order/items order))))

(>defn find-order-by-id
  "Find order by ID, returns nil if not found."
  [order-id orders]
  ;; In this case we're working against domain items, so we could be enforcing the fact that what we get/return must
  ;; be complete.
  [:order/id [:vector :domain/order] => (? :domain/order)]
  (first (filter #(= order-id (:order/id %)) orders)))

;; Multi-arity function
(>defn create-order
  "Create a new order. Can provide custom ID or generate one."
  ([customer-email items]
   [:string [:vector :order/item] => :domain/order]
   (create-order (random-uuid) customer-email items))
  ([id customer-email items]
   [:order/id :string [:vector :order/item] => :domain/order]
   {:id             id
    :customer-email customer-email
    :items          items
    :total          (calculate-order-total items)
    :status         :pending}))

(>defn update-order-status
  "Update order status with validation."
  [order new-status]
  [:domain/order :order/status => :domain/order]
  (assoc order :status new-status))
```

## Function Schema Patterns

### Side-Effect Functions

For functions that perform side effects, return `:nil` or a result type:

```clojure
(>defn save-order!
  "Persist order to database."
  [db order]
  [:any :domain/order => :nil]
  (db/insert! db :orders order)
  nil)

(>defn send-notification!
  "Send email notification, returns success boolean."
  [email message]
  [:string :string => :boolean]
  (try
    (email/send! email message)
    true
    (catch Exception _ false)))
```

### Functions Returning Results

For functions that may fail, use `:or` or `:orn`:

```clojure
(>def :result/success [:map [:status [:= :success]] [:data :any]])
(>def :result/error [:map [:status [:= :error]] [:message :string]])
(>def :result/either [:or :result/success :result/error])

(>defn process-payment
  [order payment-info]
  [:domain/order :map => :result/either]
  (try
    {:status :success :data (charge! order payment-info)}
    (catch Exception e
      {:status :error :message (ex-message e)})))
```

## Schema Error Messages

Add custom error messages for better debugging:

```clojure
[:string {:error/message "must be a valid email"
          :error/fn      (fn [{:keys [value]} _]
                           (str "'" value "' is not a valid email"))}]

[:fn {:error/message "password must be at least 8 characters"}
 #(>= (count %) 8)]
```

## Testing with Guardrails

### Operating Modes

Guardrails has three operating modes:

| Mode       | Runtime Validation | Externs Registry | Use Case                         |
|------------|--------------------|------------------|----------------------------------|
| `:runtime` | ✓                  | ✗                | Default. Type checking only      |
| `:pro`     | ✗                  | ✓                | IDE tooling, no runtime overhead |
| `:all`     | ✓                  | ✓                | **Required for proof system**    |

Set the mode via JVM property:

```bash
-Dguardrails.mode=:all
```

**IMPORTANT:** For the fulcro-spec proof system (transitive coverage, signatures), you MUST use `:all` mode. Without it,
the externs registry won't be populated and all functions will appear as leaf functions with no callees.

### Test Configuration

```clojure
;; deps.edn alias for tests with proof system support
:test {:jvm-opts    ["-Dguardrails.enabled" "-Dguardrails.mode=:all"]
       :extra-paths ["src/test"]}
```

In test mode, guardrails should throw on validation errors:

```clojure
;; Configuration for tests (throw on errors)
{:throw?          true
 :guardrails/mcps 20}  ; Max calls per second for validation

;; Configuration for development REPL (warn but don't throw)
{:throw?          false
 :guardrails/mcps 10}
```

This enables validation to catch type errors during testing while not breaking REPL-driven development.
