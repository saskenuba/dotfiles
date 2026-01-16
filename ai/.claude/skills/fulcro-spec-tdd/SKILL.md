---
name: fulcro-spec-tdd
description: How to structure applications for optimal testability in Clojure and Clojurescript, along with how to use guardrails and fulcro-spec to do the actual testing. You MUST use this skill when writing Clojure and Clojurescript if fulcro-spec is a dependency.
---

**Requirements:** Fulcro 3.9.0+, Fulcro Spec 3.2.0+, Guardrails 1.2.16+

Core philosophy: **testability is a design quality**. Code that is hard to test needs refactoring.

## Procedure

1. Write a function with `>defn`
2. Identify all behaviors (decision points)
3. Write a test for EACH behavior using fulcro-spec
4. VERIFY each test by temporarily breaking that behavior
5. **SEAL with `:covers` metadata** (mandatory)

## Setup

```clojure
;; Source namespace
(ns your.namespace
  (:require [com.fulcrologic.guardrails.malli.core :refer [>defn >def => ? |]]))

;; Test namespace
(ns your.namespace-test
  (:require
    [fulcro-spec.core :refer [specification behavior component assertions =>]]
    [com.fulcrologic.guardrails.malli.fulcro-spec-helpers :as gsh]))
```

```bash
clojure -J-Dguardrails.enabled=true -J-Dguardrails.mode=:all -M:dev:test
```

**Test config:**
```clojure
{:throw? true :mode :all :guardrails/mcps 20}
```

## Principle 1: Side Effects to Edges

Separate pure logic from effects. Pure functions are testable without mocking.

```clojure
;; PURE - all business logic, no side effects
(>defn order-fulfillment-plan [order inventory]
  [:domain/order :domain/inventory => :domain/fulfillment-plan]
  (let [quantity (:quantity order)
        total (* quantity (:unit-price inventory))]
    (if (>= (:stock inventory) quantity)
      {:status :success :total total
       :db-updates [[:inventory (:item-id order) (- (:stock inventory) quantity)]]
       :email {:to (:customer-email order) :content (success-email quantity total)}}
      {:status :failed :reason :insufficient-stock
       :db-updates []
       :email {:to (:customer-email order) :content (failure-email)}})))

;; ORCHESTRATION - side effects at edge, minimal logic
(>defn process-order! [order-id db]
  [:order/id :any => [:map [:status keyword?]]]
  (let [order (fetch-order db order-id)
        inventory (fetch-inventory db (:item-id order))
        plan (order-fulfillment-plan order inventory)]  ;; PURE
    (doseq [[k v] (:db-updates plan)] (update-db! db k v))
    (send-email! (get-in plan [:email :to]) (get-in plan [:email :content]))
    (select-keys plan [:status :total :reason])))
```

**Testing:** Pure functions need simple assertions. Only orchestration needs mocking.

```clojure
(specification "order-fulfillment-plan"
  (behavior "creates success plan when stock sufficient"
    (let [plan (order-fulfillment-plan
                 {:id 1 :item-id 2 :quantity 5 :customer-email "a@b.com"}
                 {:stock 10 :unit-price 10.0})]
      (assertions
        (:status plan) => :success
        (:total plan) => 50.0)))
  (behavior "creates failure plan when stock insufficient"
    (let [plan (order-fulfillment-plan
                 {:id 1 :item-id 2 :quantity 10 :customer-email "a@b.com"}
                 {:stock 3 :unit-price 10.0})]
      (assertions
        (:status plan) => :failed
        (:reason plan) => :insufficient-stock))))

(specification "process-order!"
  (behavior "orchestrates with mocked dependencies"
    (gsh/when-mocking!
      (fetch-order db id) => {:id 1 :item-id 2 :quantity 5 :customer-email "a@b.com"}
      (fetch-inventory db id) => {:stock 10 :unit-price 10.0}
      (update-db! db k v) => nil
      (send-email! to content) => nil
      (assertions (:status (process-order! 1 :db)) => :success))))
```

## Principle 2: Single Level of Abstraction

Don't mix high-level orchestration with low-level details.

```clojure
;; LOW: Date operations
(>defn days-between [^java.time.LocalDate start ^java.time.LocalDate end]
  [:java.time.LocalDate :java.time.LocalDate => :int]
  (.between java.time.temporal.ChronoUnit/DAYS start end))

;; MID: Business predicates
(>defn billing-due? [last-billed current-date]
  [(? :java.time.LocalDate) :java.time.LocalDate => :boolean]
  (or (nil? last-billed) (> (days-between last-billed current-date) 30)))

;; HIGH: Orchestration
(>defn run-billing! [current-date]
  [:java.time.LocalDate => :nil]
  (doseq [user (filterv #(billing-due? (:last-billed %) current-date) (fetch-users!))]
    (process-billing! (:id user))))
```

## Principle 3: Cover Every Behavior

A behavior = any decision point (`if`, `when`, `cond`, `case`, `and`, `or`, exceptions).

```clojure
(>defn calculate-discount [user amount]
  [(? :map) :number => :number]
  (cond
    (nil? user) 0                              ;; BEHAVIOR 1
    (< amount 100) 0                           ;; BEHAVIOR 2
    (:premium? user) (* amount 0.20)           ;; BEHAVIOR 3
    (>= (:loyalty-years user 0) 5) (* amount 0.15)  ;; BEHAVIOR 4
    :else (* amount 0.10)))                    ;; BEHAVIOR 5

;; 5 behaviors = 5 test cases
(specification "calculate-discount"
  (behavior "returns 0 for nil user"
    (assertions (calculate-discount nil 500) => 0))
  (behavior "returns 0 for purchases under 100"
    (assertions (calculate-discount {:premium? false} 50) => 0))
  (behavior "returns 20% for premium users"
    (assertions (calculate-discount {:premium? true} 100) => 20.0))
  (behavior "returns 15% for loyal users"
    (assertions (calculate-discount {:loyalty-years 5} 100) => 15.0))
  (behavior "returns 10% for regular users"
    (assertions (calculate-discount {} 100) => 10.0)))
```

**Avoid combinatorial explosion:** Test each function's behaviors independently at its level.

## Principle 4: Safe Mocking with Guardrails

### What Can Be Mocked

Only `>defn` functions. **Cannot mock:**
- Protocol methods → use `-` prefix wrapper pattern
- `defresolver`/`defmutation` → use `*-impl` delegation pattern
- Fulcro client mutations → use `*` suffix helpers (`state-map -> state-map`)
- Multimethods

### Validated vs Unvalidated

```clojure
;; BAD: Unvalidated (accepts anything, no contract enforcement)
(fulcro-spec.core/when-mocking
  (process-company c) => :success ...)

;; GOOD: Validated (enforces >defn contracts)
(gsh/when-mocking!
  (process-company c) => :success ...)
```

### Mocking Patterns

```clojure
;; Pattern 1: Mock side effects
(gsh/when-mocking!
  (db-query db q) => {:theme :dark}
  (assertions (:theme (load-prefs db 1)) => :dark))

;; Pattern 2: Control branches
(gsh/when-mocking!
  (charge-card! gw card amt) => {:status :failed :error "Declined"}
  (assertions (:status (process-payment gw card 100)) => :failed))

;; Pattern 3: Scripted returns
(gsh/when-mocking!
  (api-call req) =1x=> {:status :error}
  (api-call req) =1x=> {:status :error}
  (api-call req) => {:status :success}
  (assertions (:status (retry-with-backoff #(api-call {}) 3)) => :success))
```

## Controlling External Dependencies

Static JVM methods can't be mocked. **Wrap them:**

```clojure
;; BAD: Thread/sleep is static, can't mock
(defn process-with-delay [data]
  (Thread/sleep 5000)  ;; Tests wait 5 real seconds!
  (process data))

;; GOOD: Wrap for mockability
(>defn sleep-ms [ms] [:int => :nil] (Thread/sleep ms) nil)
(>defn now [] [=> :java.time.Instant] (java.time.Instant/now))

(>defn process-with-delay [data]
  [[:map] => [:map]]
  (sleep-ms 5000)  ;; Now mockable!
  (process data))

;; Test runs instantly
(gsh/when-mocking!
  (sleep-ms ms) => nil
  (assertions (:done (process-with-delay {:x 1})) => true))
```

**Note:** Check for existing wrappers in `com.fulcrologic.rad.type-support.date-time` or `cljc.java-time.*`.

## Test Structure

```clojure
(specification "order-fulfillment-plan"
  (component "when stock sufficient"
    (behavior "sets status to success" (assertions ...))
    (behavior "includes inventory update" (assertions ...)))
  (component "when stock insufficient"
    (behavior "sets status to failed" (assertions ...))
    (behavior "generates failure email" (assertions ...))))
```

Use assertion labels for multiple checks:
```clojure
(assertions
  "status is correct" (:status result) => :success
  "total is calculated" (:total result) => 50.0)
```

## Testing Patterns

### Collections
Test: empty, single-element, order preservation, nil values within.

### Error Paths
```clojure
(behavior "throws for invalid input"
  (assertions
    (parse-int "abc") =throws=> #?(:clj NumberFormatException :cljs js/Error)))
```

### Edge Cases
Always consider: boundaries (0, empty, max), nil, invalid input.

## Transitive Coverage & Sealing

### Get Signature
```clojure
(require '[fulcro-spec.proof :as proof])
(proof/signature 'myapp.core/my-fn)
;; => "a1b2c3" (leaf) or "a1b2c3,d4e5f6" (non-leaf)
```

### Seal Specification
```clojure
(specification {:covers {`sut/my-fn "a1b2c3"}} "my-fn"
  (behavior "does X" (assertions ...)))
```

### Verify Coverage
```clojure
(proof/fully-tested? 'myapp.core/my-fn)  ;; => true/false
(proof/why-not-tested? 'myapp.core/my-fn)  ;; => {:uncovered #{...} :stale #{...}}
(proof/coverage-stats)  ;; => {:total 42 :covered 38 :coverage-pct 90.5}
```

### Reseal When Code Changes
```clojure
(proof/stale-functions)  ;; Find stale
(proof/reseal-advice)    ;; Get new signatures
```

## Anti-Patterns

| Anti-Pattern                   | Problem                            | Solution                                |
|--------------------------------|------------------------------------|-----------------------------------------|
| Mocking pure functions         | Unnecessary complexity             | Call them directly                      |
| Over-mocking                   | Tests implementation, not behavior | Mock only side effects                  |
| 10+ mocks                      | Function too coupled               | Extract pure logic, create abstractions |
| Testing implementation details | Brittle tests                      | Test observable results                 |
| One giant test                 | Hard to diagnose failures          | One behavior per test                   |
| No assertions                  | Test proves nothing                | Always assert outcomes                  |

## Challenges & Solutions

| Challenge               | Solution                                                    |
|-------------------------|-------------------------------------------------------------|
| "Too hard to test"      | Refactor: separate effects, split responsibilities          |
| "Need 10 mocks"         | Extract pure logic, use data-driven design                  |
| "Huge setup"            | Use test data builders                                      |
| "Datetime dependencies" | Wrap static methods (see Controlling External Dependencies) |

## Mandatory Workflow

1. All behaviors pass
2. Get signature: `(proof/signature 'ns/fn)`
3. Add `:covers` metadata
4. Verify tests still pass

**A specification without `:covers` is incomplete.**