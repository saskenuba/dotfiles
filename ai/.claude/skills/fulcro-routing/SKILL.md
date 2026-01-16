---
name: fulcro-routing
description: Guide for Fulcro Dynamic Routing including defrouter, route targets, deferred loading, and route navigation. Use when implementing client-side routing, creating route targets, or managing navigation in Fulcro.
---

# AI System Instruction: Fulcro Routing Guide

**Role:** You are a generic Fulcro Expert.
**Objective:** Use this guide to generate, refactor, and explain routing logic using the `com.fulcrologic.fulcro.routing.dynamic-routing` namespace (Dynamic Router).
**Context:** Fulcro routing is state-driven, not URL-driven. The URL is a serialization of the UI state, not the source of truth.

---

## 1. Core Philosophy & Nomenclature

*   **Dynamic Router (`defrouter`):** A UI component that acts as a switch. It renders exactly one of its children based on the current application state.
*   **Route Target:** A standard Fulcro component (`defsc`) that is a valid destination for a router.
*   **State-First:** Changing a route is a mutation that updates the App Database. The UI reacts to this state change.
*   **Namespace Alias:** Always alias `com.fulcrologic.fulcro.routing.dynamic-routing` as `dr`.

---

## 2. Anatomy of a Router (`defrouter`)

A router is a singleton-like component in a specific part of the UI tree.

**Syntax Pattern:**
```clojure
(ns app.ui
  (:require [com.fulcrologic.fulcro.routing.dynamic-routing :as dr :refer [defrouter]]))

(defrouter MyRouter [this {:keys [current-state pending-path-segment route-factory route-props]}]
  {:router-targets [TargetA TargetB]} 
  ;; Optional: Custom rendering for state machine transitions
  (case current-state
    :pending (dom/div "Loading...")
    :failed  (dom/div "Route Failed")
    ;; Default render (if body is omitted, this happens automatically):
    (when route-factory (route-factory route-props))))
```

**Rules:**
1.  **Must have `:router-targets`:** A vector of the component classes it can switch between.
2.  **Single Usage:** A specific Router class should usually appear only once in the active DOM to avoid ident conflicts (as it uses a specific UISM ID).

---

## 3. Anatomy of a Route Target (`defsc`)

Any component can be a route target if it implements the routing protocols via options.

**Syntax Pattern:**
```clojure
(defsc UserProfile [this {:user/keys [id name]}]
  {:query [:user/id :user/name]
   :ident :user/id
   
   ;; 1. ROUTE SEGMENT (Required)
   ;; Defines the path. Strings are literals, Keywords are parameters.
   :route-segment ["user" :user-id] 

   ;; 2. ENTRY LOGIC (Required)
   ;; Determines if we can route immediately or need to fetch data.
   :will-enter 
   (fn [app {:keys [user-id] :as route-params}]
     ;; CRITICAL: Coerce parameters from string to expected types here
     (let [id (js/parseInt user-id)]
       ;; Return one of the following:
       ;; A. Immediate Entry (Data exists)
       (dr/route-immediate [:user/id id])
       
       ;; B. Deferred Entry (Need to load data)
       (dr/route-deferred [:user/id id]
         (fn []
           ;; This lambda performs the side-effects (Load)
           (df/load! app [:user/id id] UserProfile
                     {:post-mutation `dr/target-ready
                      :post-mutation-params {:target [:user/id id]}})))))

   ;; 3. EXIT LOGIC (Optional)
   ;; Return boolean. True = allowed to leave. False = stay here.
   :allow-route-change? (fn [this] (not (props-are-dirty? this)))
   }
  (dom/div (str "User: " name)))
```

---

## 4. The Deferred Routing Pattern (Standard Data Loading)

Do not route to a screen with empty data. Use the **Deferred** pattern.

1.  **`will-enter`**: Receive the route request.
2.  **Check**: Do we have the data in the specific ident?
3.  **If No**:
    *   Return `(dr/route-deferred target-ident load-fn)`.
    *   Inside `load-fn`, issue a `df/load!`.
    *   **Crucial:** The load **must** trigger the `dr/target-ready` mutation upon completion.
4.  **If Yes**:
    *   Return `(dr/route-immediate target-ident)`.

**The `target-ready` Mutation:**
When loading is finished, you must tell the router the data is ready:
```clojure
:post-mutation        `dr/target-ready
:post-mutation-params {:target [:the-target/id 123]}
```

---

## 5. Controlling the Router

### A. Changing Routes
Routing is an imperative action.

*   **Absolute:** `(dr/change-route! app ["segment" "param"])`
*   **Relative:** `(dr/change-route-relative! component TargetClass ["sub-segment"])`

**Best Practice:** Use `dr/path-to` to generate paths safely using component classes.
```clojure
(dr/change-route! this (dr/path-to UserProfile {:user-id 123}))
```

### B. Initialization
Routers are state machines. They must be initialized.
*   **App Start:** Call `(dr/change-route! app ["home"])` in the `client-did-mount` lifecycle.
*   **Initial State:** The Parent component **must** initialize the router in its `:initial-state`.

```clojure
(defsc Root [this {:root/keys [router]}]
  {:initial-state {:root/router {}} ;; Router initializes itself
   :query [{:root/router (comp/get-query MainRouter)}]}
  (ui-main-router router))
```

---

## 6. Composition & Relative Routing

*   **Nesting:** You can nest routers (e.g., SettingsRouter inside MainRouter).
*   **Consumption:** A router consumes the parts of the path defined by its active target's `:route-segment`. The "remainder" of the path is passed to the *next* router down the tree.
*   **Example:**
    *   URL: `/settings/account` -> `["settings" "account"]`
    *   MainRouter target `SettingsPage` consumes `["settings"]`.
    *   Remainder `["account"]` is passed to `SettingsPage`.
    *   `SettingsPage` contains `SettingsSubRouter`.
    *   `SettingsSubRouter` target `Account` consumes `["account"]`.

---

## 7. Legacy Routers (DEPRECATED)

If you encounter code using these namespaces/macros, treat it as **Legacy**. Do not generate this style unless explicitly asked.

*   **Namespace:** `com.fulcrologic.fulcro.routing.legacy-ui-routers`
*   **Macros:** `defsc-router` (legacy version), `defrouter` (old 2.x version).
*   **Artifacts:** `routing-tree`, `r/route-to`, `r/set-route`.

**Refactoring Advice:**
1.  Convert `routing-tree` logic into `:route-segment` on individual components.
2.  Convert `defsc-router` to `dr/defrouter`.
3.  Update mutations to `dr/change-route!`.

---

## 8. Code Generation Checklist

When generating Fulcro routing code, you must verify:
1.  [ ] Are route parameters (keywords in segments) coerced from strings to numbers/uuids in `:will-enter`?
2.  [ ] Does `:will-enter` return a value (`route-immediate` or `route-deferred`)? It must not simply side-effect and return nil.
3.  [ ] Is the Router component included in the Parent's query and initial state?
4.  [ ] Is `dr/target-ready` used as the post-mutation for deferred loads?
5.  [ ] Are you using `dr/change-route!` for navigation, not direct DOM `<a>` tags (unless hooking into history)?