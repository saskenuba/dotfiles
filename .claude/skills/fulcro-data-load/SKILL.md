---
name: fulcro-data-load
description: Guide for Fulcro data loading patterns including load!, targeting, markers, pre-merge, and post-mutations. Use when implementing data fetching, loading states, or server communication in Fulcro.
---

# AI Guide to Fulcro Data Loading

This guide defines the rules, patterns, and heuristics for handling data loading in the Fulcro framework. In Fulcro, data loading is not merely fetching JSON; it is the process of merging remote graph data into a local normalized client database.

## 1. The Core Primitive: `load!`

The primary function for fetching data is `com.fulcrologic.fulcro.data-fetch/load!`.

### Syntax Pattern
```clojure
(df/load! app-or-component server-property-or-ident ComponentClass options-map)
```

### Parameters
1.  **`app-or-component`**: Usually `this` (the component instance) or the `app` atom.
2.  **`server-property-or-ident`**:
    *   **Keyword** (e.g., `:all-friends`): A root-level property to fetch.
    *   **Ident** (e.g., `[:person/id 1]`): Fetches/refreshes a specific entity.
3.  **`ComponentClass`**: The UI component whose query defines *what* fields to fetch.
4.  **`options-map`**: Controls targeting, markers, and post-processing.

### AI Rule: Query derivation
When generating a load call, you do not manually construct the query string. You pass the `ComponentClass`, and Fulcro derives the query automatically: `(comp/get-query ComponentClass)`.

---

## 2. Targeting: Where Data Goes

By default, data loaded via a root keyword is placed at the root of the database. Targeting allows you to relocate this data to specific edges in the graph.

### 2.1 Replacement (Standard Target)
Replaces the value at the target path with the result (ident or list of idents).

```clojure
;; Logic: Fetch :all-people, normalize them, and put the pointing vector
;; at [:list/id :people :list/people]
(df/load! this :all-people Person
  {:target [:list/id :people :list/people]})
```

### 2.2 Appending (To-Many Relations)
Adds new items to an existing list of idents without overwriting existing ones.

```clojure
(require '[com.fulcrologic.fulcro.data-fetch :as df])
(require '[com.fulcrologic.fulcro.algorithms.data-targeting :as targeting])

;; Logic: Fetch person 42, normalize, and append reference to specific list
(df/load! this [:person/id 42] Person
  {:target (targeting/append-to [:list/id :people :list/people])})
```
*Also available: `targeting/prepend-to`.*

### 2.3 Multiple Targets
Places the same reference in multiple locations.
```clojure
{:target (targeting/multiple-targets
           [:root/latest-items]
           (targeting/append-to [:list/all-items]))}
```

---

## 3. Load Markers: Tracking Status

To display "Loading..." spinners, Fulcro uses normalized load markers.

### generating a Load
```clojure
(df/load! this :some-data Component {:marker :my-custom-marker})
```

### Querying the Marker
The marker is stored in a top-level table defined by `df/marker-table`.
```clojure
(defsc MyComponent [this props]
  {:query [:id :data [df/marker-table '_]] ;; Link query to get the whole table
   ...}
  (let [marker-status (get-in props [df/marker-table :my-custom-marker])]
    (cond
      (df/loading? marker-status) (dom/div "Loading...")
      (df/failed? marker-status)  (dom/div "Error")
      :else (dom/div "Content loaded"))))
```

---

## 4. Data Initialization & Manipulation

Fulcro provides two mechanisms to modify data as it enters the client DB.

### 4.1 Pre-Merge (Preferred for Component Logic)
**Use Case:** Initializing UI-only state (e.g., `ui/checked?`), handling defaults, or calculating derived data *per component instance* as data arrives.

**Mechanism:** A hook defined in `defsc`. It receives the incoming data tree and the current existing data in the DB.

**Standard Pattern:**
```clojure
(defsc Counter [this {:keys [db/id val] :ui/keys [count]}]
  {:ident [:counter/id :db/id]
   :query [:db/id :val :ui/count]
   ;; The Hook
   :pre-merge (fn [{:keys [current-normalized data-tree]}]
                (merge
                  {:ui/count 5}       ;; 1. Defaults
                  current-normalized  ;; 2. Existing State (preserves user interaction)
                  data-tree))}        ;; 3. Incoming Server Data (wins on conflict)
   ...)
```

**AI Heuristic:**
*   If the goal is to set a default value for a field that doesn't exist on the server -> **Use Pre-Merge**.
*   If the goal is to generate a random UUID for a UI component upon load -> **Use Pre-Merge**.

### 4.2 Post-Mutations (Structural Logic)
**Use Case:** Global data reshaping, legacy transforms, or logic that affects the graph structure beyond a single component's scope.

**Mechanism:** A mutation runs after the network request finishes.

```clojure
(defmutation organize-data [params]
  (action [{:keys [state]}]
    ;; Arbitrary Clojure code to reshape the app-state atom
    (swap! state ...)))

(df/load! this :data Component {:post-mutation `organize-data})
```

---

## 5. Incremental Loading & Pruning

Loading subsets of data or "filling in" missing data later.

### 5.1 Pruning (Lazy Loading)
Fetch a component but explicitly exclude heavy fields (e.g., comments on a post).
```clojure
(df/load! this :post/id PostComponent
  {:params {:id 1}
   :without #{:post/comments}}) ;; Exclude this field from the query
```

### 5.2 Filling in (Load Field)
Load a specific field (edge) for an entity that is already in the database.
```clojure
;; Triggers a query rooted at the component's ident: [{[:post/id 1] [:post/comments ...]}]
(df/load-field! this :post/comments)
```

### 5.3 Focus
Restrict the query to *only* specific fields, pruning everything else.
```clojure
(df/load! this :post/id PostComponent {:focus [:post/title :post/author]})
```

---

## 6. Parallel vs. Sequential

*   **Sequential (Default):** Requests enter a queue. Request B does not start until Request A completes. Ensures consistency.
*   **Parallel:** Requests start immediately. Use for unrelated data chunks to improve performance.

```clojure
(df/load! this :dashboard/chart ChartComp {:parallel true})
```

---

## 7. Summary of AI Decision Logic

When generating Fulcro loading code, follow this decision tree:

1.  **What is being loaded?**
    *   *New data list?* -> Use `load!` with `:target` (usually `append-to` or replacement).
    *   *Refreshing an entity?* -> Use `load!` with the entity's Ident.
    *   *A specific field on an existing entity?* -> Use `load-field!`.

2.  **Does the data need UI initialization?**
    *   *Yes (e.g., setting `ui/open?` to false)* -> Generate a `:pre-merge` block in the component.

3.  **Does the load need to be tracked?**
    *   *Yes* -> Add `{:marker :some-keyword}` to options and query `[df/marker-table '_]` in the component.

4.  **Does the data shape match the UI?**
    *   *Yes* -> Standard load.
    *   *No (Needs grouping/reshaping)* -> Use `{:post-mutation ...}`.

5.  **Performance context?**
    *   *Is it a background fetch independent of others?* -> Add `{:parallel true}`.
