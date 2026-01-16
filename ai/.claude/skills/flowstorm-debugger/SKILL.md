---
name: flowstorm-debugger
description: How to use the FlowStorm debugger to analyze Clojure/ClojureScript program executions. This skill teaches timeline exploration, value inspection, and debugging patterns using FlowStorm's recording API for runtime analysis.
---

# FlowStorm Debugger

FlowStorm records program execution for post-hoc analysis. This project uses **Storm mode** (with UI).

## Quick Start (Dataico)

```clojure
;; 1. Connect FlowStorm and start recording
(require '[flow-storm.api :as fs])
(fs/local-connect)
(fs/start-recording)  ;; won't record without this!

;; 2. Instrument namespace in FlowStorm UI (Browser panel)

;; 3. RELOAD the namespace (critical!)
(require '[some.namespace :as ns] :reload)

;; 4. Call your function
(ns/some-function args)

;; 5. Check recordings
(require '[flow-storm.runtime.indexes.api :as fs-api])
(fs-api/all-flows)  ;; => {0 [161]} means flow 0, thread 161
```

## Analyzing Recordings

```clojure
(require '[flow-storm.runtime.indexes.api :as fs-api])
(require '[flow-storm.runtime.values :as vals])

;; Get timeline
(def tl (fs-api/get-referenced-maps-timeline 0 161))
(count tl)  ;; number of entries

;; Find function calls
(def fn-calls (filter #(= :fn-call (:type %)) tl))

;; Inspect a call
(let [call (first (filter #(= "my-fn" (:fn-name %)) fn-calls))
      ret-entry (get tl (:ret-idx call))]
  {:fn (str (:fn-ns call) "/" (:fn-name call))
   :args (vals/deref-val-id (:fn-args-ref call))
   :result (vals/deref-val-id (:result-ref ret-entry))})
```

## Entry Types

| Type         | Key Fields                                                      |
|--------------|-----------------------------------------------------------------|
| `:fn-call`   | `:fn-name`, `:fn-ns`, `:fn-args-ref`, `:ret-idx`, `:parent-idx` |
| `:fn-return` | `:result-ref`, `:fn-call-idx`                                   |
| `:fn-unwind` | `:throwable-ref` (exception)                                    |
| `:expr`      | `:result-ref`, `:fn-call-idx`                                   |

## Value Dereferencing

```clojure
;; Use flow-storm.runtime.values (NOT mcp-runtime)
(require '[flow-storm.runtime.values :as vals])

(vals/deref-val-id (:fn-args-ref call))    ;; get arguments
(vals/deref-val-id (:result-ref ret))      ;; get return value
(vals/deref-val-id (:throwable-ref unwind)) ;; get exception

;; For large values
(binding [*print-level* 3 *print-length* 10]
  (vals/deref-val-id value-id))
```

## Common Patterns

```clojure
;; Find calls to specific function
(filter #(and (= :fn-call (:type %))
              (= "process-invoice" (:fn-name %))) tl)

;; Find exceptions
(filter #(= :fn-unwind (:type %)) tl)

;; Navigate call graph
(:parent-idx entry)   ;; go up to parent
(:ret-idx call)       ;; go to return entry
(:fn-call-idx ret)    ;; go back to call

;; Call frequency
(->> tl
     (filter #(= :fn-call (:type %)))
     (map #(str (:fn-ns %) "/" (:fn-name %)))
     frequencies
     (sort-by val >))
```

## Skipped Namespaces (Dataico)

It's a good practice to check for skipped prefixes, otherwise you may not have the traces and don't know about it.

```clojure
;; Get current config
(require '[flow-storm.debugger.runtime-api :as runtime-api])
(.get-storm-instrumentation runtime-api/rt-api)
```

## Gotchas

1. **Must call `(fs/start-recording)`** after connecting - won't record otherwise
2. **Must reload namespace** after instrumenting in UI - otherwise no recordings
3. **Use `vals/deref-val-id`** - `mcp-runtime` may not be available
4. **REPL can block** if FlowStorm UI is closed mid-recording - restart REPL if stuck
5. **`user` namespace won't record
6. **Skipped namespaces** won't record even after reload - check config above
