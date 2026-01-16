---
name: fulcro-frontend
description: Guide for building maintainable UI with ClojureScript and Fulcro. Use when creating UI components, designing component architecture, or following frontend patterns.
---

# Building Maintainable UI with ClojureScript and Fulcro

## Core Principles

### 1. Separation of Concerns
- **UI Logic** (business rules, data transformation) should be separate from **Presentation** (rendering, styling)
- Components should be data-driven and declarative
- Keep side effects and state management separate from view code

### 2. Component Design Philosophy
- Components accept data as props and render UI
- Prefer pure presentation components that don't manage state
- Use composition over inheritance
- Keep components small, focused, and reusable

---

## Component Architecture

### Choosing Between `defsc` and `defn`

**Use `defsc` when you need:**
- Fulcro queries (`:query`)
- Idents (`:ident`)
- Initial state (`:initial-state`)
- To call `comp/transact!` or access component instance
- Lifecycle methods

**Use `defn` for:**
- Pure presentation that just renders data
- No queries needed (data comes from parent)
- Stateless, reusable UI pieces
- Better performance (no component overhead)

```clojure
;; GOOD: defn for simple presentation
(defn ui-user-card [{:keys [user/name user/email user/avatar-url]}]
  (dom/div :.user-card
    (dom/img {:src avatar-url :alt name})
    (dom/div :.user-info
      (dom/h3 name)
      (dom/p email))))

;; GOOD: defsc when you need Fulcro features
(defsc UserCardContainer [this {:user/keys [id name email avatar-url] :as props}]
  {:query [:user/id :user/name :user/email :user/avatar-url]
   :ident :user/id}
  (ui-user-card props))
```

### Presentation Components (Pure UI)

Presentation components are functions that take data and return UI elements. They have no business logic and don't manage state.

**Prefer `defn` for pure presentation:**

```clojure
(ns myapp.ui.components
  (:require [com.fulcrologic.fulcro.dom :as dom]))

;; GOOD: Simple presentation function
(defn ui-user-card [{:keys [user/name user/email user/avatar-url]}]
  (dom/div :.user-card
    (dom/img {:src avatar-url :alt name})
    (dom/div :.user-info
      (dom/h3 name)
      (dom/p email))))

;; GOOD: Button with callback
(defn ui-button [{:keys [label on-click disabled? variant]}]
  (dom/button
    {:className (str "btn btn-" (name (or variant :default)))
     :disabled disabled?
     :onClick on-click}
    label))
```

**Use `defsc` when you need queries:**

```clojure
(ns myapp.ui.components
  (:require [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
            [com.fulcrologic.fulcro.dom :as dom]))

;; GOOD: defsc when normalization/queries are needed
(defsc UserCard [this {:keys [user/name user/email user/avatar-url]}]
  {:query [:user/name :user/email :user/avatar-url]
   :ident :user/id}
  (dom/div :.user-card
    (dom/img {:src avatar-url :alt name})
    (dom/div :.user-info
      (dom/h3 name)
      (dom/p email))))
```

### Container Components (Logic & Data)

Container components handle business logic, data fetching, and state management. They compose presentation components.

```clojure
(ns myapp.ui.containers
  (:require [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
            [com.fulcrologic.fulcro.dom :as dom]
            [com.fulcrologic.fulcro.mutations :as m]
            [myapp.ui.components :as ui]))

;; GOOD: Container component that manages logic
(defsc UserProfile [this {:user/keys [id name email] :as props}]
  {:query [:user/id :user/name :user/email]
   :ident :user/id
   :initial-state (fn [params] {:user/id nil :user/name "" :user/email ""})}
  (let [can-edit? (= id (comp/shared this :current-user-id))
        handle-edit (fn [] (comp/transact! this [(edit-user {:user/id id})]))]
    (dom/div
      ;; Compose pure presentation component
      (ui/ui-user-card props)
      ;; Logic determines what to show
      (when can-edit?
        (ui/ui-button {:label "Edit"
                       :on-click handle-edit
                       :variant :primary})))))
```

---

## Data Flow Patterns

### 1. Props Down, Events Up

Components receive all data via props and communicate changes via callback props.

```clojure
;; GOOD: Clear data flow
(defsc TodoItem [this {:keys [todo/text todo/completed? on-toggle on-delete]}]
  (dom/div :.todo-item
    (dom/input {:type "checkbox"
                :checked completed?
                :onChange on-toggle})
    (dom/span text)
    (dom/button {:onClick on-delete} "Delete")))

(defsc TodoList [this {:keys [todos]}]
  {:query [{:todos (comp/get-query TodoItem)}]}
  (dom/div :.todo-list
    (map (fn [todo]
           (ui-todo-item
             (comp/computed todo
               {:on-toggle #(comp/transact! this [(toggle-todo {:todo/id (:todo/id todo)})])
                :on-delete #(comp/transact! this [(delete-todo {:todo/id (:todo/id todo)})])})))
         todos)))
```

### 2. Computed Props for Callbacks

Use `comp/computed` to pass callbacks and other non-query data to child components **when using defsc components**.

For simple presentation functions, just accept callbacks as regular function arguments:

```clojure
;; GOOD: Simple function component - no need for defsc overhead
(defn ui-item [{:keys [item/id item/name on-click highlight?]}]
  (dom/div
    {:className (when highlight? "highlighted")
     :onClick on-click}
    name))

;; ALSO GOOD: defsc when you need queries/idents
(defsc Item [this props computed]
  {:query [:item/id :item/name]}
  (let [{:keys [on-click highlight?]} computed]
    (dom/div
      {:className (when highlight? "highlighted")
       :onClick on-click}
      (:item/name props))))

;; Usage with simple function
(defsc ItemList [this {:keys [items]}]
  {:query [{:items [:item/id :item/name]}]}
  (dom/div
    (map (fn [item]
           (ui-item (assoc item
                      :on-click #(js/console.log "clicked" (:item/id item))
                      :highlight? (= (:item/id item) selected-id))))
         items)))
```

---

## State Management

### Keep Business Logic Separate

Mutations should contain business logic, not UI concerns.

```clojure
;; GOOD: Mutation contains only business logic
(m/defmutation add-todo [{:keys [text]}]
  (action [{:keys [state]}]
    (let [new-id (random-uuid)
          new-todo {:todo/id new-id
                    :todo/text text
                    :todo/completed? false
                    :todo/created-at (js/Date.)}]
      (swap! state
        (fn [s]
          (-> s
              (assoc-in [:todo/id new-id] new-todo)
              (update :todos/list conj [:todo/id new-id])))))))

;; Component just triggers the mutation
(defsc TodoForm [this {:keys [input-text]}]
  {:initial-state {:input-text ""}}
  (dom/form
    {:onSubmit (fn [e]
                 (.preventDefault e)
                 (comp/transact! this [(add-todo {:text input-text})])
                 (m/set-string! this :input-text :value ""))}
    (dom/input {:value input-text
                :onChange #(m/set-string! this :input-text :value %)})
    (dom/button "Add")))
```

---

## Reusable Component Patterns

### 1. Generic Form Components

```clojure
;; GOOD: Generic, reusable form input
(defsc FormInput [this {:keys [label value error placeholder type]}]
  (dom/div :.form-group
    (when label (dom/label label))
    (dom/input {:type (or type "text")
                :value value
                :placeholder placeholder
                :className (when error "error")
                :onChange (:on-change (comp/get-computed this))})
    (when error (dom/span :.error-message error))))

;; Usage
(defsc LoginForm [this {:keys [email password errors]}]
  (dom/form
    (ui-form-input
      (comp/computed
        {:label "Email"
         :value email
         :error (:email errors)
         :placeholder "you@example.com"}
        {:on-change #(m/set-string! this :email :value %)}))
    (ui-form-input
      (comp/computed
        {:label "Password"
         :value password
         :error (:password errors)
         :type "password"}
        {:on-change #(m/set-string! this :password :value %)}))))
```

### 2. Layout Components

```clojure
;; GOOD: Reusable layout components
(defn card [props & children]
  (dom/div :.card props children))

(defn card-header [{:keys [title subtitle]}]
  (dom/div :.card-header
    (dom/h2 title)
    (when subtitle (dom/p :.subtitle subtitle))))

(defn card-body [props & children]
  (dom/div :.card-body props children))

;; Usage in components
(defsc ProductCard [this {:keys [product/name product/price product/description]}]
  (card {}
    (card-header {:title name :subtitle (str "$" price)})
    (card-body {}
      (dom/p description))))
```

### 3. Higher-Order Component Pattern

```clojure
;; GOOD: HOC for loading states
(defn with-loading [component]
  (comp/factory
    (fn [props]
      (let [loading? (:ui/loading? props)]
        (if loading?
          (dom/div :.loading-spinner "Loading...")
          (component (dissoc props :ui/loading?)))))))

;; Usage
(def ui-user-profile-with-loading
  (with-loading ui-user-profile))
```

---

## Anti-Patterns to Avoid

### DON'T: Mix Logic and Presentation

```clojure
;; BAD: Business logic mixed with rendering
(defsc BadComponent [this props]
  (let [data (fetch-data-from-somewhere)  ; Side effect in render
        processed (complex-calculation data)  ; Logic in render
        should-show? (and (some-condition?) (another-condition?))]  ; Complex logic
    (dom/div
      (when should-show?
        (dom/div "Content")))))

;; GOOD: Separate concerns
(defn should-display? [data]  ; Pure function
  (and (some-condition? data) (another-condition? data)))

(defsc GoodComponent [this {:keys [data processed] :as props}]
  {:query [:data :processed :ui/should-show?]}  ; Data from props
  (when (:ui/should-show? props)
    (dom/div "Content")))
```

### DON'T: Direct State Manipulation in Components

```clojure
;; BAD: Direct state access
(defsc BadComponent [this props]
  (dom/button
    {:onClick #(swap! (comp/app-state this) assoc :some-key "value")}
    "Click"))

;; GOOD: Use mutations
(defsc GoodComponent [this props]
  (dom/button
    {:onClick #(comp/transact! this [(update-value {:key :some-key :value "value"})])}
    "Click"))
```

### DON'T: Deeply Nested Component Structures

```clojure
;; BAD: Too much nesting
(defsc MonolithicComponent [this props]
  (dom/div
    (dom/div
      (dom/div
        (dom/div
          ;; ... many more divs
          )))))

;; GOOD: Break into smaller components
(defsc Header [this props] ...)
(defsc Content [this props] ...)
(defsc Footer [this props] ...)

(defsc Page [this props]
  (dom/div
    (ui-header props)
    (ui-content props)
    (ui-footer props)))
```

---

## Testing Presentational Components

Pure presentation components are easy to test:

```clojure
(deftest user-card-test
  (let [props {:user/name "Alice"
               :user/email "alice@example.com"
               :user/avatar-url "/avatar.jpg"}
        component (ui-user-card props)]
    ;; Test rendering with specific props
    (is (= "Alice" (get-text component ".user-info h3")))
    (is (= "alice@example.com" (get-text component ".user-info p")))))
```

---

## Idiomatic Clojure Patterns

### Use Threading Macros for Readability

```clojure
;; GOOD: Threading for data transformations before rendering
(defn ui-user-list [{:keys [users filters]}]
  (let [filtered-users (->> users
                            (filter (apply-filters filters))
                            (sort-by :user/name)
                            (take 10))]
    (dom/div :.user-list
      (map ui-user-card filtered-users))))

;; GOOD: Thread-first for building up props
(defn ui-card-with-actions [{:keys [title content actions]}]
  (-> {:className "card"}
      (assoc :data-testid "user-card")
      (dom/div
        (dom/h2 title)
        (dom/div :.content content)
        (when (seq actions)
          (dom/div :.actions actions)))))
```

### Keep Destructuring Close to Usage

```clojure
;; GOOD: Destructure in let when doing transformations
(defn ui-user-profile [props]
  (let [{:user/keys [name email roles]} props
        admin? (contains? (set roles) :admin)
        display-name (or name "Anonymous")]
    (dom/div :.profile
      (dom/h1 display-name)
      (dom/p email)
      (when admin? (dom/span :.badge "Admin")))))

;; ALSO GOOD: Destructure in function args for simple cases
(defn ui-user-card [{:user/keys [name email avatar-url]}]
  (dom/div :.user-card
    (dom/img {:src avatar-url})
    (dom/h3 name)
    (dom/p email)))
```

### Co-locate Helper Functions

```clojure
;; GOOD: Keep helpers close to where they're used
(ns myapp.ui.dashboard)

(defn- format-currency [amount]
  (str "$" (.toFixed amount 2)))

(defn- calculate-total [items]
  (reduce + 0 (map :price items)))

(defn ui-order-summary [{:keys [items discount]}]
  (let [subtotal (calculate-total items)
        total (- subtotal discount)]
    (dom/div :.summary
      (dom/div "Subtotal: " (format-currency subtotal))
      (dom/div "Discount: " (format-currency discount))
      (dom/div "Total: " (format-currency total)))))
```

---

## Quick Reference

### Component Checklist
- [ ] Is this component doing one thing well?
- [ ] Can it be tested easily?
- [ ] Does it receive all data via props?
- [ ] Are callbacks passed appropriately (computed props for defsc, regular args for defn)?
- [ ] Is business logic extracted to mutations?
- [ ] Should this be `defn` or `defsc`? (Use defn unless you need queries/idents/lifecycle)
- [ ] Can this component be reused?
- [ ] Are helper functions co-located in the same namespace?

### When to Extract a Component
- When you have repeated UI patterns
- When a component has more than 50-75 lines
- When you want to test a piece of UI in isolation
- When you need the same UI with different data

### When NOT to Extract a Component
**Don't break down components just for the sake of it.** If a component is:
- Already readable and easy to understand
- Has clear, logical sections (even if it's somewhat long)
- Would become harder to understand if split up
- Doesn't have repeated patterns that need reuse

...then leave it as is. Readability and maintainability are the goals, not achieving some arbitrary "perfect" structure.

### Composition Over Configuration
```clojure
;; GOOD: Compose simple components
(dom/div
  (ui-card-header {:title "Products"})
  (ui-card-body {}
    (map ui-product-item products)))

;; AVOID: Mega-component with many options
(ui-mega-card {:type :products
               :show-header? true
               :header-title "Products"
               :items products
               :item-render-fn product-item
               ...}) ; Too many options
```

---

## Summary

**Key Takeaways:**
1. **Separate presentation from logic** - Components render, mutations mutate
2. **Data flows down, events flow up** - Props in, callbacks out
3. **Compose small components** - Build complex UIs from simple pieces
4. **Prefer `defn` for pure presentation, `defsc` when you need Fulcro features** - Don't add unnecessary overhead
5. **Keep helper functions close** - Co-locate related code in the same namespace
6. **Use Fulcro's patterns appropriately** - Query, ident, initial-state, mutations where needed

Following these patterns will result in a codebase that is:
- Easy to understand and navigate
- Simple to test
- Straightforward to refactor
- Pleasant to maintain and extend
