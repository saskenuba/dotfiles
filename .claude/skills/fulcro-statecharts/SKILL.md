---
name: fulcro-statecharts
description: Complete reference for Fulcrologic Statecharts including state machines, transitions, events, Fulcro integration, and testing. Use when implementing statecharts, managing complex UI state, or working with state machine patterns.
---

# Fulcrologic Statecharts - Complete Reference Guide

**For AI-Assisted Development**: This guide merges conceptual understanding with real codebase examples.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Critical Concepts (Read First)](#critical-concepts)
3. [Core Architecture](#core-architecture)
4. [Defining Statecharts](#defining-statecharts)
5. [Event Processing](#event-processing)
6. [Running Statecharts](#running-statecharts)
7. [Data Model & Operations](#data-model--operations)
8. [Fulcro Integration](#fulcro-integration)
9. [Testing](#testing)
10. [Real-World Patterns](#real-world-patterns)
11. [API Reference](#api-reference)

---

## Quick Start

### Minimal Working Example
```clojure
(require '[com.fulcrologic.statecharts.chart :refer [statechart]]
         '[com.fulcrologic.statecharts.elements :refer [state transition]]
         '[com.fulcrologic.statecharts.simple :as simple]
         '[com.fulcrologic.statecharts.protocols :as sp]
         '[com.fulcrologic.statecharts.events :refer [new-event]])

;; Define
(def my-chart
  (statechart {}
    (state {:id :idle}
      (transition {:event :start :target :running}))
    (state {:id :running}
      (transition {:event :stop :target :idle}))))

;; Setup & Run
(def env (simple/simple-env))
(simple/register! env ::my-chart my-chart)
(def processor (::sc/processor env))

;; Execute
(def s0 (sp/start! processor env ::my-chart {::sc/session-id :session-1}))
(def s1 (sp/process-event! processor env s0 (new-event :start)))
```

### Fulcro Quick Start
```clojure
;; 1. Install (once at startup)
(scf/install-fulcro-statecharts! app)

;; 2. Register
(scf/register-statechart! app ::chart chart)

;; 3. Start
(scf/start! app {:machine ::chart :session-id :some-id})
```

---

## Critical Concepts

### Event Name Matching (CRITICAL!)

**This is the #1 source of bugs. Read carefully.**

Events use **hierarchical dot-separated naming with prefix matching**:

```clojure
;; Event :a.b.c matches ALL of these:
:a.b.c      ; exact match
:a.b.c.*    ; explicit wildcard
:a.b        ; prefix match
:a.b.*      ; prefix wildcard
:a          ; prefix match
:a.*        ; prefix wildcard

;; Transitions are checked IN ORDER:
(state {:id :handler}
  (transition {:event :error.network} ...)     ; Matches :error.network.timeout
  (transition {:event :error} ...)             ; Catches ALL :error.* events
  (transition {:event :timeout} ...))          ; Won't catch :error.network.timeout!
```

**Rule**: Put specific handlers BEFORE general ones.

### Configuration vs State

- **State**: A node in your statechart
- **Configuration**: The SET of ALL currently active states (includes ancestors and parallel regions)

```clojure
;; In a parallel chart with hierarchy:
(::sc/configuration session)
;; => #{:top :parallel-region-1 :region-1/child-a
;;      :parallel-region-2 :region-2/child-x}
```

### Working Memory

The complete state of a running statechart:
- Current configuration (active states)
- Data model values
- History state tracking
- **Pure EDN** - serializable, persistable

```clojure
;; Working memory IS the session
(def session (sp/start! processor env ::chart {::sc/session-id :my-id}))
;; session contains everything needed to resume
```

---

## Core Architecture

### Components of a Statechart System

```clojure
{::sc/processor              ; Algorithm implementation (SCXML)
 ::sc/event-queue            ; FIFO queue for events
 ::sc/data-model             ; Where/how data is stored
 ::sc/execution-model        ; How code expressions run
 ::sc/working-memory-store   ; Persistent storage for sessions
 ::sc/statechart-registry    ; Chart definitions by name
 ::sc/invocation-processors} ; Handlers for invoke elements
```

### Session Lifecycle

1. **Define** - Chart definition (pure data)
2. **Register** - Store in registry under keyword
3. **Start** - Create session with ID
4. **Process Events** - Transition through states
5. **Terminate** - Reach final state or cancel

---

## Defining Statecharts

### Core Elements

```clojure
(require '[com.fulcrologic.statecharts.elements :refer
           [state parallel transition on-entry on-exit
            script-fn Send cancel history final data-model]])

(statechart {}
  ;; Atomic state
  (state {:id :simple})

  ;; Compound state (with children)
  (state {:id :parent :initial :child-a}
    (state {:id :child-a})
    (state {:id :child-b}))

  ;; Parallel state (all regions active)
  (parallel {}
    (state {:id :region-1})
    (state {:id :region-2}))

  ;; Final state (terminal)
  (final {:id :done}))
```

### Transitions

```clojure
;; Full syntax
(transition {:event :trigger          ; Event pattern to match
             :target :next-state      ; Target state ID
             :cond (fn [env data] true)  ; Guard condition
             :internal false})        ; External by default

;; Eventless transition (fires immediately)
(transition {:cond (fn [env data] (pos? (:balance data)))
             :target :approved})
(transition {:target :rejected})  ; else case

;; No target = self-transition (stay in state)
(transition {:event :refresh}
  (script-fn [env data]
    [(ops/assign :last-refresh (js/Date.))]))
```

### Executable Content

```clojure
(state {:id :active}
  ;; On entry
  (on-entry {}
    (script-fn [env data]
      (println "Entering active")
      [(ops/assign :entered-at (js/Date.))]))

  ;; On exit
  (on-exit {}
    (script-fn [env data]
      (println "Exiting active")))

  ;; Transition with actions
  (transition {:event :process :target :done}
    (script-fn [env data]
      (let [result (process-data (:input data))]
        [(ops/assign :result result)]))))
```

### Real-World Example: Traffic Light with Timer

```clojure
(def nk extend-key)  ; Helper: (nk :a "b") => :a/b

(defn traffic-signal [id initial]
  (let [red (nk id "red")
        yellow (nk id "yellow")
        green (nk id "green")
        initial (nk id (name initial))]
    (state {:id id :initial initial}
      (state {:id red}
        (transition {:event :swap-flow :target green}))
      (state {:id yellow}
        (transition {:event :swap-flow :target red}))
      (state {:id green}
        (transition {:event :warn-traffic :target yellow})))))

(defn timer []
  (state {:id :timer-control}
    (state {:id :timing-flow}
      (transition {:event :warn-pedestrians :target :timing-ped-warning})
      (on-entry {}
        (Send {:event :warn-pedestrians :delay 2000})))
    (state {:id :timing-ped-warning}
      (transition {:event :warn-traffic :target :timing-yellow})
      (on-entry {}
        (Send {:event :warn-traffic :delay 500})))
    (state {:id :timing-yellow}
      (transition {:event :swap-flow :target :timing-flow})
      (on-entry {}
        (Send {:event :swap-flow :delay 200})))))

(def traffic-lights
  (statechart {}
    (parallel {}
      (timer)
      (traffic-signal :east-west :green)
      (traffic-signal :north-south :red)
      (ped-signal :cross-ew :red)
      (ped-signal :cross-ns :white))))
```

### History States

```clojure
(state {:id :parent}
  (transition {:event :leave :target :other})

  ;; Shallow history - remembers direct child
  (history {:id :parent-history}
    (transition {:target :default-child}))  ; Default if no history

  (state {:id :default-child}
    (transition {:event :next :target :another-child}))
  (state {:id :another-child}))

;; Later, to restore history:
(transition {:event :return :target :parent-history})
```

**Deep history**: Use `{:type :deep}` to remember entire hierarchy.

---

## Event Processing

### Event Types

1. **External Events**: From outside (via `process-event!` or `send!`)
2. **Internal Events**: Generated by chart itself (via `raise` or `done.invoke`)

### Event Object Structure

```clojure
;; Access in executable content via [:_event ...]
{:_event {:name :my-event
          :data {:custom "data"}
          :target :session-id
          :origin :sender-id
          :invokeid :invoke-123}}
```

### Delayed Events (Timers)

```clojure
(require '[com.fulcrologic.statecharts.elements :refer [Send cancel]])

(state {:id :waiting}
  (on-entry {}
    (Send {:event :timeout
           :delay 5000        ; milliseconds
           :id :my-timer}))   ; ID for cancellation
  (on-exit {}
    (cancel {:sendid :my-timer}))  ; Cancel on exit
  (transition {:event :timeout :target :timed-out}))
```

### Convenience Helper: send-after

```clojure
(require '[com.fulcrologic.statecharts.convenience :refer [send-after]])

(state {:id :timing}
  (send-after {:id :timer-1 :delay 2000 :event :timeout})
  (transition {:event :timeout :target :next}))

;; Expands to on-entry Send + on-exit cancel
```

### Event Name Examples

```clojure
;; Hierarchical event organization
:user.login.success
:user.login.failed
:user.logout

:error.network.timeout
:error.network.connection
:error.validation

;; Catch-all patterns
(transition {:event :user.login} ...)  ; Matches both success and failed
(transition {:event :error} ...)       ; Matches all errors
```

---

## Running Statecharts

### Synchronous/Manual Mode

```clojure
(def env (simple/simple-env))
(simple/register! env ::my-chart my-chart)
(def processor (::sc/processor env))

;; Start
(def s0 (sp/start! processor env ::my-chart {::sc/session-id :s1}))

;; Process events manually
(def s1 (sp/process-event! processor env s0 (new-event :start)))
(def s2 (sp/process-event! processor env s1 (new-event :process)))

;; Check configuration
(::sc/configuration s2)  ; => #{:running :processing}
```

### Autonomous/Async Mode with Event Loop

```clojure
(require '[com.fulcrologic.statecharts.event-queue.core-async-event-loop :as loop])

;; Setup
(def env (simple/simple-env))
(simple/register! env ::my-chart my-chart)

;; Run event loop (polls every 100ms)
(def running? (loop/run-event-loop! env 100))

;; Start chart
(simple/start! env ::my-chart :session-1)

;; Send events asynchronously
(simple/send! env {:target :session-1 :event :start})
(simple/send! env {:target :session-1 :event :process})

;; Stop when done
(reset! running? false)
```

### Custom Working Memory Store (for monitoring)

```clojure
(def wmem (let [a (atom {})]
            (add-watch a :printer
              (fn [_ _ _ n] (println "Config:" (::sc/configuration n))))
            a))

(def env (simple/simple-env
           {::sc/working-memory-store
            (reify sp/WorkingMemoryStore
              (get-working-memory [_ _ _] @wmem)
              (save-working-memory! [_ _ _ m] (reset! wmem m)))}))
```

---

## Data Model & Operations

### Initializing Data

```clojure
(statechart {}
  (data-model {:counter 0
               :user-name "Alice"
               :items []})
  ...)
```

### Operations

```clojure
(require '[com.fulcrologic.statecharts.data-model.operations :as ops])

(script-fn [env data]
  [(ops/assign :counter (inc (:counter data)))
   (ops/assign :status :active)
   (ops/assign [:nested :path] {:value 42})
   (ops/delete :temporary-field)])
```

### Accessing Data in Executable Content

```clojure
(transition {:event :update}
  (script-fn [env data]
    ;; data contains:
    ;; - All local statechart data
    ;; - :_event with event details
    (let [event-data (get-in data [:_event :data])
          current-count (:counter data)]
      [(ops/assign :counter (+ current-count event-data))])))
```

### Conditions on Data

```clojure
(defn has-positive-balance? [env data]
  (pos? (:balance data)))

(state {:id :check}
  (transition {:event :proceed
               :cond has-positive-balance?
               :target :approved})
  (transition {:event :proceed
               :target :rejected}))
```

---

## Fulcro Integration

### Installation & Setup

```clojure
(require '[com.fulcrologic.statecharts.integration.fulcro :as scf])

;; 1. Install (idempotent, once at startup)
(scf/install-fulcro-statecharts! app)

;; 2. Register charts
(scf/register-statechart! app ::my-chart my-chart)

;; 3. Start with optional initial data
(scf/start! app {:machine ::my-chart
                 :session-id :my-session
                 :data {:fulcro/actors {...}
                        :fulcro/aliases {...}}})
```

### Data Model: Actors & Aliases

**Actors** = UI components (by class + ident)
**Aliases** = Shortcuts to data paths

```clojure
;; At startup or in chart definition
(data-model {:fulcro/aliases {:username [:actor/user :user/name]
                              :balance [:fulcro/state :account :balance]}})

;; At runtime (in scf/start!)
(scf/start! app
  {:machine ::my-chart
   :session-id :my-session
   :data {:fulcro/actors {:actor/user (scf/actor UserComponent [:user/id 123])}
          :fulcro/aliases {:display-name [:actor/user :user/display-name]}}})

;; Use in operations
(script-fn [env data]
  [(fops/assign :username "Alice")])  ; Updates [:actor/user :user/name]
```

### Data Location Types

1. **Keyword** - Local data or alias
   ```clojure
   :counter  ; Local to statechart
   :username ; If in aliases, resolves to alias path
   ```

2. **`[:ROOT ...]`** - Local statechart data
   ```clojure
   [:ROOT :counter]
   ```

3. **`[:fulcro/state ...]`** - Fulcro app database
   ```clojure
   [:fulcro/state :account :balance]
   ```

4. **`[:actor/name ...]`** - Via actor ident
   ```clojure
   [:actor/user :user/email]  ; => [:fulcro/state :user/id 123 :user/email]
   ```

### Fulcro Operations

```clojure
(require '[com.fulcrologic.statecharts.integration.fulcro.operations :as fops])

;; Load data
(script-fn [env data]
  [(fops/load :user/list UserComponent
     {:target [:fulcro/state :users]
      :ok-event :load/success
      :error-event :load/failed})])

;; Remote mutation
(script-fn [env data]
  [(fops/invoke-remote [(my-mutation {:x 1})]
     {:ok-event :mutation/success
      :error-event :mutation/failed
      :target [:actor/user]  ; Auto-merge to actor
      :returning UserComponent})])

;; Assign to alias
(script-fn [env data]
  [(fops/assoc-alias :username "Bob")])
```

### React Hooks Integration

```clojure
(require '[com.fulcrologic.statecharts.integration.fulcro.react-hooks :as sch])

(defsc TrafficLight [this {:ui/keys [color]}]
  {:query [:ui/color]
   :initial-state {:ui/color "green"}
   :ident (fn [] [:component/id ::TrafficLight])
   :statechart (statechart {}
                 (state {:id :state/green}
                   (on-entry {}
                     (script-fn [_ _] [(fops/assoc-alias :color "green")]))
                   (transition {:event :next :target :state/yellow}))
                 (state {:id :state/yellow}
                   (on-entry {}
                     (script-fn [_ _] [(fops/assoc-alias :color "yellow")]))
                   (transition {:event :next :target :state/red}))
                 (state {:id :state/red}
                   (on-entry {}
                     (script-fn [_ _] [(fops/assoc-alias :color "red")]))
                   (transition {:event :next :target :state/green})))
   :use-hooks? true}

  (let [{:keys [send! local-data]}
        (sch/use-statechart this {:data {:fulcro/aliases {:color [:actor/component :ui/color]}}})]
    (dom/div {}
      (dom/div {:style {:backgroundColor color
                        :width "50px"
                        :height "50px"}})
      (dom/button {:onClick #(send! :next)} "Next"))))
```

### Useful Fulcro Helpers

```clojure
;; Get local data path (for component queries)
(scf/local-data-path session-id)

;; Get session ident (for component queries)
(scf/statechart-session-ident session-id)

;; Get current configuration
(scf/current-configuration app session-id)

;; Send event
(scf/send! app session-id :event/name {:optional :data})

;; Resolve aliases
(resolve-aliases data)  ; Returns map of alias-key -> value

;; Resolve actors
(resolve-actors data :actor/user)  ; Returns UI props
(resolve-actors data :actor/user :actor/admin)  ; Returns map

;; Get actor component class
(resolve-actor-class data :actor/user)
```

---

## Testing

### Basic Test Setup

```clojure
(require '[com.fulcrologic.statecharts.testing :as testing])

(defn is-valid? [env data] (:valid? data))
(defn is-tuesday? [env data] false)

(def test-chart
  (statechart {}
    (state {:id :start}
      (transition {:cond is-valid? :event :submit :target :success}))
    (state {:id :success})))

;; Create test environment with mocks
(let [env (testing/new-testing-env
            {:statechart test-chart}
            {is-valid? true       ; Mock to return true
             is-tuesday? false})] ; Mock to return false

  ;; Start
  (testing/start! env)

  ;; Run events
  (testing/run-events! env :submit)

  ;; Assertions
  (testing/in? env :success)       ; => true
  (testing/ran? env is-valid?)     ; => true
  (testing/not-ran? env is-tuesday?))  ; => true
```

### Mocking with Dynamic Functions

```clojure
;; Mock can be a function receiving env with :ncalls
(let [env (testing/new-testing-env
            {:statechart test-chart}
            {my-condition (fn [env]
                           (let [call-count (:ncalls env)]
                             (if (< call-count 3)
                               false
                               true)))})]
  ...)
```

### Jump to Specific State

```clojure
(defn config [] {:statechart some-statechart})

(specification "Test from specific state"
  (let [env (testing/new-testing-env (config) {})]

    ;; Set data and configuration directly
    (testing/goto-configuration!
      env
      [(ops/assign :balance 1000)      ; Set up data
       (ops/assign :user-id "u123")]
      #{:state.region1/leaf            ; Active states
        :state.region2/leaf})

    ;; Now test from this state
    (testing/run-events! env :event/expired)

    (assertions
      (testing/in? env :expected-state) => true)))
```

### Testing Event Sends/Cancels

```clojure
;; Check if Send was called
(testing/sent? env :my-event)

;; Check if cancel was called
(testing/cancelled? env :my-timer-id)

;; Get all sent events
(testing/get-sends env)
```

---

## Real-World Patterns

### Pattern: Timeout with Cancellation

```clojure
(require '[com.fulcrologic.statecharts.convenience :refer [send-after]])

(state {:id :waiting}
  (send-after {:id :timeout-timer :delay 5000 :event :timeout})
  (transition {:event :success :target :done})
  (transition {:event :cancel :target :cancelled})
  (transition {:event :timeout :target :failed}))

;; send-after automatically cancels timer on state exit
```

### Pattern: Retry with Backoff

```clojure
(state {:id :retrying}
  (on-entry {}
    (script-fn [env data]
      (let [attempt (inc (:retry-count data 0))
            delay (* 1000 (Math/pow 2 attempt))]  ; Exponential backoff
        [(ops/assign :retry-count attempt)
         (ops/assign :next-retry-delay delay)])))

  (send-after {:id :retry-timer
               :delayexpr (fn [_ data] (:next-retry-delay data))
               :event :retry})

  (transition {:cond (fn [_ data] (> (:retry-count data) 5))
               :target :failed})
  (transition {:event :retry :target :attempting})
  (transition {:event :success :target :done}))
```

### Pattern: Conditional Routing (Choice State)

```clojure
(require '[com.fulcrologic.statecharts.convenience :refer [choice]])

;; Using convenience macro
(choice {:id :routing}
  (fn [_ data] (pos? (:balance data))) :approved
  (fn [_ data] (zero? (:balance data))) :pending
  :else :rejected)

;; Expands to:
(state {:id :routing}
  (transition {:cond (fn [_ data] (pos? (:balance data)))
               :target :approved})
  (transition {:cond (fn [_ data] (zero? (:balance data)))
               :target :pending})
  (transition {:target :rejected}))
```

### Pattern: Multi-Step Wizard

```clojure
(statechart {}
  (state {:id :wizard :initial :step-1}
    (on-entry {}
      (script-fn [_ _]
        [(ops/assign :wizard-data {})]))

    (state {:id :step-1}
      (transition {:event :next :target :step-2}
        (script-fn [env data]
          (let [step-data (get-in data [:_event :data])]
            [(ops/assign [:wizard-data :step-1] step-data)]))))

    (state {:id :step-2}
      (transition {:event :next :target :step-3}
        (script-fn [env data]
          (let [step-data (get-in data [:_event :data])]
            [(ops/assign [:wizard-data :step-2] step-data)])))
      (transition {:event :back :target :step-1}))

    (state {:id :step-3}
      (transition {:event :submit :target :submitting}
        (script-fn [env data]
          ;; All wizard data available in (:wizard-data data)
          [(fops/invoke-remote [(submit-wizard (:wizard-data data))]
             {:ok-event :submit/success
              :error-event :submit/failed})]))
      (transition {:event :back :target :step-2}))

    (state {:id :submitting}
      (transition {:event :submit/success :target :done})
      (transition {:event :submit/failed :target :step-3})))

  (final {:id :done}))
```

### Pattern: Parallel Authentication + Data Loading

```clojure
(statechart {}
  (parallel {}
    ;; Authentication region
    (state {:id :auth :initial :checking}
      (state {:id :checking}
        (on-entry {}
          (script-fn [_ _]
            [(fops/load :session SessionQuery
               {:ok-event :auth/success
                :error-event :auth/failed})]))
        (transition {:event :auth/success :target :authenticated})
        (transition {:event :auth/failed :target :unauthenticated}))
      (state {:id :authenticated})
      (state {:id :unauthenticated}))

    ;; Data loading region
    (state {:id :data :initial :loading}
      (state {:id :loading}
        (on-entry {}
          (script-fn [_ _]
            [(fops/load :user-data UserDataQuery
               {:ok-event :data/loaded
                :error-event :data/failed})]))
        (transition {:event :data/loaded :target :ready})
        (transition {:event :data/failed :target :error}))
      (state {:id :ready})
      (state {:id :error}))))

;; Both regions run in parallel
;; Check if both complete: (testing/in? env :authenticated) && (testing/in? env :ready)
```

### Pattern: Invocation (Child Statechart)

```clojure
(def child-chart
  (statechart {}
    (state {:id :working}
      (transition {:event :child/done :target :complete}))
    (final {:id :complete})))

(def main-chart
  (statechart {}
    (state {:id :running}
      (invoke {:id :child-worker
               :type :statechart
               :src `child-chart
               :autoforward true  ; Forward all events
               :params {:initial-value 42}
               :finalize (fn [env data]
                          ;; Called when child sends event to parent
                          [(ops/assign :child-result data)])})
      (transition {:event :done.invoke.child-worker :target :done}))
    (final {:id :done})))

;; Start and send to child
(simple/start! env `main-chart :session-1)
(simple/send! env {:target :child-worker :event :child/done})
```

---

## API Reference

### Namespaces

```clojure
;; Core
[com.fulcrologic.statecharts.chart :refer [statechart]]
[com.fulcrologic.statecharts.elements :refer [state parallel transition
                                                on-entry on-exit script-fn
                                                Send cancel history final
                                                data-model invoke]]
[com.fulcrologic.statecharts :as sc]
[com.fulcrologic.statecharts.protocols :as sp]
[com.fulcrologic.statecharts.simple :as simple]
[com.fulcrologic.statecharts.events :refer [new-event]]

;; Data model
[com.fulcrologic.statecharts.data-model.operations :as ops]

;; Event loop
[com.fulcrologic.statecharts.event-queue.core-async-event-loop :as loop]

;; Convenience
[com.fulcrologic.statecharts.convenience :refer [on handle send-after choice]]

;; Testing
[com.fulcrologic.statecharts.testing :as testing]

;; Fulcro
[com.fulcrologic.statecharts.integration.fulcro :as scf]
[com.fulcrologic.statecharts.integration.fulcro.operations :as fops]
[com.fulcrologic.statecharts.integration.fulcro.react-hooks :as sch]
```

### Element Functions

```clojure
;; Chart
(statechart opts & children)

;; States
(state {:id :keyword :initial :child-id} & children)
(parallel {} & regions)
(final {:id :keyword})
(history {:id :keyword :type :shallow|:deep} & transitions)

;; Transitions
(transition {:event :keyword
             :target :state-id
             :cond (fn [env data] ...)
             :internal boolean} & actions)

;; Executable content
(on-entry {} & actions)
(on-exit {} & actions)
(script {:expr (fn [env data] ...)})
(script-fn [env data] ...)  ; Macro version

;; Events
(Send {:event :keyword :delay ms :id :sendid :target :session-id})
(cancel {:sendid :id})
(raise {:event :keyword})

;; Data
(data-model initial-data)

;; Invocations
(invoke {:id :invoke-id
         :type :statechart|:future|custom
         :src chart-or-fn
         :autoforward boolean
         :params data-or-fn
         :finalize (fn [env data] ...)})
```

### Protocol Functions

```clojure
;; Starting/processing
(sp/start! processor env chart-key init-data) ; => session
(sp/process-event! processor env session event) ; => new-session

;; Simple API
(simple/simple-env)
(simple/simple-env overrides-map)
(simple/register! env chart-key chart)
(simple/start! env chart-key session-id)
(simple/send! env {:target session-id :event :keyword :data {...}})

;; Event loop
(loop/run-event-loop! env poll-interval-ms) ; => running? atom
;; Stop: (reset! running? false)
```

### Operations

```clojure
(ops/assign location value)
(ops/assign :key value)
(ops/assign [:path :to :key] value)
(ops/delete location)
```

### Fulcro Operations

```clojure
(fops/assign location value)  ; Fulcro-aware paths
(fops/assoc-alias alias-key value)
(fops/load query-root component-or-actor options)
(fops/invoke-remote txn {:ok-event :kw :error-event :kw :target path})
```

### Testing API

```clojure
;; Setup
(testing/new-testing-env config mocks)
(testing/start! env)

;; State manipulation
(testing/goto-configuration! env data-ops config-set)
(testing/run-events! env & events)

;; Assertions
(testing/in? env state-id) ; => boolean
(testing/not-in? env state-id) ; => boolean
(testing/ran? env expr-fn) ; => boolean
(testing/not-ran? env expr-fn) ; => boolean
(testing/sent? env event-name) ; => boolean
(testing/cancelled? env send-id) ; => boolean

;; Inspection
(testing/get-sends env) ; => seq of sends
(testing/get-cancels env) ; => seq of cancels
```

### Fulcro Helpers

```clojure
;; Installation
(scf/install-fulcro-statecharts! app)
(scf/install-fulcro-statecharts! app extra-env-map)

;; Registration & starting
(scf/register-statechart! app chart-key chart)
(scf/start! app {:machine chart-key :session-id id :data init-data})

;; Runtime
(scf/send! app session-id event optional-data)
(scf/current-configuration app session-id)

;; Paths & idents
(scf/local-data-path session-id)
(scf/statechart-session-ident session-id)

;; Actors
(scf/actor component-class ident)
(resolve-actors data & actor-keys)
(resolve-actor-class data actor-key)

;; Aliases
(resolve-aliases data)

;; Mutation results
(scf/mutation-result data)  ; Extract from :_event

;; Hooks
(sch/use-statechart this init-opts) ; => {:keys [send! local-data]}
```

---

## Common Pitfalls & Tips

### 1. Event Name Matching Order Matters

```clojure
;; BAD - general handler first
(state {:id :bad}
  (transition {:event :error} ...)        ; Catches ALL
  (transition {:event :error.network} ...))  ; Never runs!

;; GOOD - specific first
(state {:id :good}
  (transition {:event :error.network} ...)  ; Runs first
  (transition {:event :error} ...))         ; Catches rest
```

### 2. Use `defn` for Conditions (for testing)

```clojure
;; BAD - can't mock
(transition {:cond (fn [_ data] (pos? (:x data)))})

;; GOOD - can mock in tests
(defn positive-x? [_ data] (pos? (:x data)))
(transition {:cond positive-x?})
```

### 3. Working Memory is Immutable

```clojure
;; BAD - mutates, won't persist
(script-fn [env data]
  (swap! some-atom inc)  ; Side effect!
  [])

;; GOOD - returns operations
(script-fn [env data]
  [(ops/assign :counter (inc (:counter data)))])
```

### 4. Remember `Send` vs `send`

```clojure
;; Use capital S to avoid shadowing clojure.core/send
(require '[com.fulcrologic.statecharts.elements :refer [Send]])
(Send {:event :timeout :delay 1000})
```

### 5. Session IDs Must Be Unique

```clojure
;; BAD - overwrites existing
(scf/start! app {:machine ::chart :session-id :my-id})
(scf/start! app {:machine ::chart :session-id :my-id})  ; Replaces first!

;; GOOD - unique IDs
(scf/start! app {:machine ::chart :session-id (random-uuid)})
```

### 6. History Targets History Node, Not Parent

```clojure
;; BAD
(transition {:event :return :target :parent})  ; No history!

;; GOOD
(transition {:event :return :target :parent-history})  ; Restores
```

### 7. Check Configuration, Not Just State ID

```clojure
;; In parallel charts, multiple states are active
(let [config (::sc/configuration session)]
  (and (contains? config :region-1/ready)
       (contains? config :region-2/ready)))
```

---

## Key Differences from Other State Machine Libraries

1. **SCXML Compliant**: Follows W3C standard, not xstate
2. **Pure Clojure Data**: Charts are nested maps, not classes
3. **Immutable**: Working memory is pure EDN, fully serializable
4. **Hierarchical Events**: Dot-separated with prefix matching
5. **Operations Return Values**: `script-fn` returns vector of ops, not side effects
6. **Fulcro First-Class**: Deep integration with Fulcro's architecture

---

## Resources

- **Docs**: https://fulcrologic.github.io/statecharts/
- **Source**: https://github.com/fulcrologic/statecharts
- **SCXML Spec**: https://www.w3.org/TR/scxml/
- **Fulcro**: https://fulcro.fulcrologic.com/

---

## Quick Decision Tree

**Should I use a statechart?**
- Yes: Complex UI workflows (wizards, forms)
- Yes: Network request state management
- Yes: Multi-step processes with error handling
- Yes: Timer-based behavior
- Yes: Need to persist/resume state
- No: Simple toggle or counter (use atom)
- No: Pure data transformation (use functions)

**Synchronous or Async mode?**
- Synchronous: Testing, simple scripts, full control
- Async: Production apps, timers, long-running processes

**When to use parallel states?**
- Independent concurrent processes
- Multiple loading states
- Auth + Data loading simultaneously

**When to use history?**
- Tab navigation that remembers position
- Pause/resume workflows
- "Return to where you were"
