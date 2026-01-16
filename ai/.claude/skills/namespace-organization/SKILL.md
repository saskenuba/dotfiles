---
name: namespace-organization
description: Guide for Dataico project namespace organization including architecture layers, domain organization, and where to place new code. Use when adding new features, refactoring, or understanding the codebase structure.
---

# Dataico Project: Namespace Organization Guide

## Project Overview

This is a full-stack Clojure/ClojureScript application with **2,234+ source files** following a domain-driven architecture pattern. The codebase uses:
- **850 Clojure (.clj)** files - Backend/JVM code
- **535 ClojureScript (.cljs)** files - Frontend code
- **849 Cross-platform (.cljc)** files - Shared business logic

The architecture follows a layered approach with clear separation of concerns across data access, business logic, and presentation layers, organized around business domains (accounting, invoicing, payroll, etc.).

---

## High-Level Architecture Layers

### Layer 1: Data & Model Layer
Defines data structures, schemas, and domain entities
- **`model/`** - Domain entity specifications and business logic
- **`model_rad/`** - RAD (Fulcro) entity definitions with attributes and forms
- **`data_model/`** - Core data model definitions

### Layer 2: Data Access Layer
Database queries and persistence
- **`queries/`** - Read operations (Datomic queries, database reads)
- **`resolvers/`** - Pathom resolvers for GraphQL-like data fetching
- **`transactions/`** - Database write operations and side effects

### Layer 3: Business Logic Layer
Core business rules and transformations
- **`builders/`** - Entity construction and complex object assembly
- **`mutations/`** - Business operations (CRUD + domain logic)
- **`data_engine/`** - Rule engine, expressions, dynamic queries
- **`analytics/`** - Business metrics and calculations
- **`articles/`** - Reusable domain articles/helpers

### Layer 4: Integration & Service Layer
External integrations and infrastructure services
- **`services/`** - External service integrations (DIAN, email, PDF, etc.)
- **`jobs/`** - Background job processing
- **`pipeline/`** - Data transformation pipelines (import/export)
- **`server_components/`** - Infrastructure components (database, search, queues)

### Layer 5: Presentation Layer (UI)
Frontend components and user interactions
- **`ui/`** - ClojureScript UI components, forms, routes, and actions

### Layer 6: Cross-Cutting Concerns
Utilities and reusable libraries
- **`lib/`** - Generic utility libraries (non-domain-specific)
- **`rad/`** - RAD framework extensions and middleware

---

## Detailed Namespace Reference

### 1. Model Layer (`dataico.model.*`)

**Purpose**: Define domain entities, their structure, validation, and core business logic

**Contains**:
- Entity schemas (Spec/Malli)
- Domain calculations (price, tax, subtotal computations)
- Entity-specific utilities
- Constants and enums

**Examples**:
- `dataico.model.invoice-item` - Invoice line item calculations
- `dataico.model.party` - Party/customer entity definitions
- `dataico.model.accounting` - Accounting entity models
- `dataico.model.payroll.*` - Payroll domain models

**When to Use**:
- Defining new entity types
- Adding domain-specific calculations to entities
- Validation logic for domain objects
- Entity-specific constants

**Do NOT Use For**:
- Database queries (use `queries/`)
- UI logic (use `ui/`)
- External service calls (use `services/`)

---

### 2. RAD Model Layer (`dataico.model_rad.*`)

**Purpose**: Fulcro RAD attribute definitions with forms, reports, and UI metadata

**Contains**:
- RAD attribute definitions (`:rad/attributes`)
- Form configurations
- Report configurations
- Refinements (pre/post-processing hooks)
- Validations
- Synchronous after-effects

**Subdirectories**:
- `payroll/` - Payroll-specific RAD definitions
- `refinements/` - Entity refinement logic
- `validations/` - RAD validation middleware
- `synchronous_after_effects/` - Post-save hooks
- `subscriptions/` - Subscription entity RAD configs

**Examples**:
- `dataico.model_rad.invoice` - Invoice RAD attributes and forms
- `dataico.model_rad.party` - Party RAD configuration
- `dataico.model_rad.refinements.invoice` - Invoice refinement hooks

**When to Use**:
- Creating new RAD forms
- Defining entity attributes for Fulcro
- Adding pre/post-save refinements
- Configuring entity validation

---

### 3. Queries Layer (`dataico.queries.*`)

**Purpose**: Read-only database access - all data retrieval logic

**Contains**:
- Datomic queries
- Data pull patterns
- Read-specific helper functions
- Query composition utilities

**Domain Organization**:
```
queries/
├── invoice.clj           # Invoice queries
├── party.clj            # Party/customer queries
├── accounting.clj       # Accounting queries
├── payroll.clj          # Payroll queries
├── accounting/         # Accounting subdomain
├── analytics/          # Analytics queries
├── payroll/           # Payroll subdomain
```

**Examples**:
- `dataico.queries.invoice` - Fetch invoices, invoice patterns
- `dataico.queries.accounting` - Accounting records queries
- `dataico.queries.party` - Customer/supplier queries

**When to Use**:
- Fetching data from database
- Composing complex queries
- Defining data pull patterns
- Database read operations

**Do NOT Use For**:
- Write operations (use `mutations/` or `transactions/`)
- Business logic (use `builders/` or domain logic in `model/`)

---

### 4. Resolvers Layer (`dataico.resolvers.*`)

**Purpose**: Pathom resolvers for GraphQL-like data fetching and computed fields

**Contains**:
- Pathom resolver definitions
- Computed attributes
- Data aggregations
- Join resolvers

**Examples**:
- `dataico.resolvers.invoice` - Invoice data resolvers
- `dataico.resolvers.accounting` - Accounting resolvers
- `dataico.resolvers.party` - Party resolvers

**When to Use**:
- Creating computed fields
- Defining Pathom resolvers
- Aggregating data from multiple sources
- Building derived/calculated attributes

**Related To**: Works with `queries/` for data fetching

---

### 5. Mutations Layer (`dataico.mutations.*`)

**Purpose**: High-level business operations combining validation, transactions, and side effects

**Contains**:
- Pathom mutations (server-side)
- Fulcro mutations (client-side)
- Business operation orchestration
- CRUD operations with business logic

**File Extension Pattern**:
- `.clj` - Server-only mutations
- `.cljc` - Shared mutations (client + server)
- `.cljs` - Client-only mutations

**Examples**:
- `dataico.mutations.invoice` - Invoice operations (create, update, send to DIAN)
- `dataico.mutations.accounting` - Accounting record operations
- `dataico.mutations.payroll` - Payroll processing operations

**When to Use**:
- Implementing business operations
- Orchestrating multi-step processes
- Coordinating transactions with side effects
- CRUD with business validation

**Related To**:
- Uses `transactions/` for database writes
- Uses `queries/` for data fetching
- Uses `builders/` for entity construction

---

### 6. Transactions Layer (`dataico.transactions.*`)

**Purpose**: Low-level database write operations and transaction management

**Contains**:
- Direct database transactions
- Transaction helpers
- Side-effect coordination
- Validation at transaction level
- Edit/delete operations

**Key Files**:
- `crud.clj` - Generic CRUD transaction functions
- `validations.clj` - Transaction-level validations
- `side_effects.clj` - Side effect coordination
- `edit.clj` - Entity editing utilities

**Examples**:
- `dataico.transactions.invoice` - Invoice transaction logic
- `dataico.transactions.accounting` - Accounting record transactions
- `dataico.transactions.crud` - Generic CRUD helpers

**When to Use**:
- Direct database writes
- Complex multi-entity transactions
- Transaction-level validation
- Side effect management

**Do NOT Use For**:
- Business logic (use `mutations/`)
- Queries (use `queries/`)

---

### 7. Builders Layer (`dataico.builders.*`)

**Purpose**: Entity construction, complex object assembly, and test fixtures

**Contains**:
- Entity builder functions
- Complex object construction
- Default value application
- Test data builders
- Scene builders (test fixtures)

**Domain Organization**:
```
builders/
├── invoice.cljc           # Invoice builders
├── party.cljc            # Party builders
├── accounting/           # Accounting builders
├── payroll/             # Payroll builders
├── scene/              # Test fixture scenes
```

**Examples**:
- `dataico.builders.invoice` - Build complete invoice objects
- `dataico.builders.accounting.record_builder` - Build accounting records
- `dataico.builders.party` - Build party entities

**When to Use**:
- Constructing complex entities
- Applying default values
- Building test fixtures
- Assembling related entities

**Pattern**: Builders return well-formed entities ready for persistence

---

### 8. Services Layer (`dataico.services.*`)

**Purpose**: External service integrations and infrastructure services

**Contains**:
- External API integrations
- Email services
- PDF generation
- File processing
- Third-party service wrappers

**Key Integrations**:
```
services/
├── dian.clj              # Colombian tax authority (DIAN) integration
├── send_email.clj        # Email sending
├── pdf/                 # PDF generation
├── hubspot/             # HubSpot CRM integration
├── rest/               # REST API utilities
├── apis/              # Various API integrations
```

**Examples**:
- `dataico.services.dian` - DIAN electronic invoicing
- `dataico.services.send_email` - Email delivery
- `dataico.services.pdf` - PDF generation

**When to Use**:
- Integrating external services
- Wrapping third-party APIs
- Infrastructure service interaction
- File/document processing

---

### 9. Jobs Layer (`dataico.jobs.*`)

**Purpose**: Background job processing and async task execution

**Contains**:
- Background job definitions
- Batch processing
- Scheduled tasks
- Async operations

**Domain Organization**:
```
jobs/
├── accounting/              # Accounting jobs
├── payroll/                # Payroll jobs
├── numbering/              # Numbering jobs
├── bulk_send_dian_job.clj  # Bulk DIAN submission
├── pdf_job.clj             # PDF generation jobs
├── download_documents/     # Document download jobs
```

**Examples**:
- `dataico.jobs.bulk_send_dian_job` - Bulk invoice submission to DIAN
- `dataico.jobs.accounting.*` - Accounting batch jobs
- `dataico.jobs.pdf_job` - Async PDF generation

**When to Use**:
- Long-running processes
- Batch operations
- Scheduled tasks
- Async processing

---

### 10. Pipeline Layer (`dataico.pipeline.*`)

**Purpose**: Data transformation pipelines for import/export operations

**Contains**:
- Data format adapters
- Import/export formatters
- Data transformation logic
- External format converters

**Structure**:
```
pipeline/
├── adapters/                  # Input adapters (import)
│   ├── accounting/           # Accounting imports
│   ├── excel/               # Excel imports
│   ├── party/               # Party imports
│   └── shopify.clj          # Shopify integration
├── formatters/                # Output formatters (export)
    ├── accounting/           # Accounting exports
    │   ├── softland.clj     # Softland format
    │   ├── hgi.clj          # HGI format
    │   └── siewin.clj       # Siewin format
    └── excel/               # Excel exports
```

**Examples**:
- `dataico.pipeline.adapters.excel` - Import Excel data
- `dataico.pipeline.formatters.accounting.softland` - Export to Softland format
- `dataico.pipeline.adapters.shopify` - Import Shopify orders

**When to Use**:
- Importing data from external systems
- Exporting to specific formats
- Data transformation between formats
- Integration with accounting software

**Pattern**:
- **Adapters** = Input (import)
- **Formatters** = Output (export)

---

### 11. Data Engine Layer (`dataico.data_engine.*`)

**Purpose**: Dynamic rule engine, expressions, and filtering system

**Contains**:
- Expression evaluation
- Dynamic query building
- Rule processing
- Filtering engine
- Statistics aggregation

**Key Files**:
- `expressions.cljc` - Expression parser and evaluator
- `rules.cljc` - Rule engine
- `filtering.cljc` - Dynamic filtering
- `dynamic_query.clj` - Query builder
- `selection.cljc` - Selection logic

**When to Use**:
- Building dynamic queries
- Evaluating user-defined expressions
- Rule-based processing
- Advanced filtering logic

**Example Use Cases**:
- Accounting rule execution
- Dynamic report filtering
- User-defined calculations

---

### 12. Analytics Layer (`dataico.analytics.*`)

**Purpose**: Business metrics, calculations, and analytical functions

**Contains**:
- Business metric calculations
- Revenue analytics (MRR, ARR)
- Document analytics
- Tax calculations
- Retention metrics

**Files**:
- `document.cljc` - Document analytics
- `mrr.clj` - Monthly Recurring Revenue
- `mau.clj` - Monthly Active Users
- `retention.cljc` - Retention metrics
- `tax.cljc` - Tax analytics

**When to Use**:
- Computing business metrics
- Analytical calculations
- Aggregating business data
- Revenue tracking

---

### 13. Articles Layer (`dataico.articles.*`)

**Purpose**: Reusable domain-specific helper functions (like utility modules per domain)

**Contains**:
- Domain-specific utilities
- Reusable domain logic
- Cross-cutting domain concerns

**Files by Domain**:
- `accounting.cljc` - Accounting helpers
- `sales.cljc` - Sales utilities
- `purchase.cljc` - Purchase helpers
- `payroll.cljc` - Payroll utilities
- `inventory.cljc` - Inventory helpers
- `party.cljc` - Party utilities

**When to Use**:
- Domain-specific reusable functions
- Logic used across multiple features in a domain
- Domain utilities that don't fit in model/

**Difference from `lib/`**:
- `articles/` = Domain-specific utilities
- `lib/` = Generic, non-domain utilities

---

### 14. UI Layer (`dataico.ui.*`)

**Purpose**: ClojureScript frontend components, forms, and user interactions

**Structure** (per domain):
```
ui/{domain}/
├── actions.cljs          # UI actions and mutations
├── query.cljs/.cljc      # UI-specific queries
├── forms.cljs            # Form components
├── route.cljs            # Routing
├── [feature folders]/    # Feature-specific UI
```

**Major UI Domains**:
- `ui/accounting/` - Accounting UI
- `ui/invoice/` - Invoice UI
- `ui/payroll/` - Payroll UI
- `ui/document/` - Document UI
- `ui/party/` - Party/customer UI
- `ui/cartera_dashboard/` - AR/AP dashboard

**Common UI Namespaces**:
- `ui/kit/` - Reusable UI components
- `ui/components/` - Shared components
- `ui/crud/` - Generic CRUD UI
- `ui/state_machines/` - State machine definitions
- `ui/autocomplete/` - Autocomplete components

**File Patterns**:
- `*-actions.cljs` - UI-triggered actions
- `*-query.cljs` - UI data queries
- `*-form.cljs` - Form components
- `*-route.cljs` - Route definitions

**When to Use**:
- Building UI components
- Form logic
- Client-side state management
- Routing and navigation

---

### 15. Library Layer (`dataico.lib.*`)

**Purpose**: Generic, reusable utilities (non-domain-specific)

**Categories**:

#### Data & Collections
- `collections.cljc` - Collection utilities
- `sets.cljc` - Set operations
- `trees.cljc` - Tree data structure utilities
- `compressed_ranges.clj` - Range compression

#### Date & Time
- `dates.cljc` - Date utilities

#### Data Processing
- `numbers.cljc` - Number utilities
- `strings.cljc` - String utilities
- `eql.cljc` - EQL utilities
- `pathom_utils.cljc` - Pathom helpers

#### Infrastructure
```
lib/
├── datomic/           # Datomic utilities
├── fulcro/           # Fulcro extensions
├── rad/              # RAD utilities
├── security/         # Security helpers
├── sql/              # SQL utilities
├── search/           # Search utilities
├── statecharts/      # State machine helpers
```

#### External Services
- `excel.clj` - Excel utilities
- `pdf_parsing/` - PDF parsing
- `xml/` - XML processing
- `s3.clj` - S3 operations
- `redis.clj` - Redis operations

**When to Use**:
- Generic utilities needed across domains
- Infrastructure-level helpers
- Framework extensions
- Non-domain-specific logic

**Do NOT Use For**:
- Domain-specific logic (use `articles/`)
- Business logic (use appropriate domain layer)

---

### 16. RAD Framework Layer (`dataico.rad.*`)

**Purpose**: Fulcro RAD framework extensions and middleware

**Contains**:
- Custom RAD middleware
- Form extensions
- Report extensions
- Database adapters
- RAD options and configuration

**Key Components**:
```
rad/
├── middleware/              # Custom middleware
├── form/                   # Form extensions
├── report/                 # Report extensions
├── database_adapters/      # Custom DB adapters
├── ui/                    # RAD UI components
```

**When to Use**:
- Extending RAD framework
- Custom middleware
- RAD form/report enhancements
- Framework-level customization

---

### 17. Server Components Layer (`dataico.server_components.*`)

**Purpose**: Infrastructure components and system resources

**Contains**:
- Database connections
- Search engine integration
- Job queue systems
- Email processing
- Async workers

**Components**:
```
server_components/
├── datomic/           # Datomic connection
├── sql/              # SQL connection
├── search/           # Elasticsearch
├── job_queues/       # Job queue systems
├── async/            # Async workers
├── email_processing/ # Email handling
├── stores/          # Various stores
```

**When to Use**:
- System initialization
- Infrastructure setup
- Resource management
- Component lifecycle

---

### 18. Supporting Namespaces

#### Migrations (`dataico.migrations.*`)
- Database migrations
- Schema evolution
- Data migrations
- Organized by domain (accounting, payroll, party, etc.)

#### Onboarding (`dataico.onboarding.*`)
- User onboarding flows
- Feature activation
- Setup wizards
- Organized by feature (accounting, invoice, payroll, etc.)

#### Startup (`dataico.startup.*`)
- Application initialization
- System startup logic

#### Subscriptions (`dataico.subscriptions.*`)
- Subscription management
- Plan handling

---

## Namespace Naming Conventions

### Pattern Guide

```clojure
;; Domain Entity Model
dataico.model.{entity-name}

;; Domain Entity RAD Definition
dataico.model_rad.{entity-name}

;; Read Operations
dataico.queries.{entity-name}

;; Write Operations
dataico.mutations.{entity-name}
dataico.transactions.{entity-name}

;; Data Fetching
dataico.resolvers.{entity-name}

;; Entity Construction
dataico.builders.{entity-name}

;; External Services
dataico.services.{service-name}

;; Background Jobs
dataico.jobs.{job-purpose}

;; UI Components
dataico.ui.{domain}.{component-type}

;; Generic Utilities
dataico.lib.{utility-category}

;; Domain Utilities
dataico.articles.{domain}

;; Data Transformation
dataico.pipeline.adapters.{source}
dataico.pipeline.formatters.{target}
```

### File Extension Semantics

- **`.clj`** - Server-only (JVM/Clojure)
- **`.cljs`** - Client-only (Browser/ClojureScript)
- **`.cljc`** - Cross-platform (Shared between client & server)

---

## Domain Organization

### Major Business Domains

#### 1. Accounting
```
model/accounting/
queries/accounting/
mutations/accounting.cljc
transactions/accounting/
builders/accounting/
ui/accounting/
jobs/accounting/
articles/accounting.cljc
```

#### 2. Invoicing
```
model/invoice_*.cljc
queries/invoice.clj
mutations/invoice.cljc
transactions/invoice.clj
builders/invoice.cljc
ui/invoice/
services/dian.clj (electronic invoicing)
```

#### 3. Payroll
```
model/payroll/
model_rad/payroll/
queries/payroll/
mutations/payroll/
builders/payroll/
ui/payroll/
jobs/payroll/
```

#### 4. Party Management (Customers/Suppliers)
```
model/party.cljc
queries/party.clj
mutations/party.cljc
builders/party.cljc
ui/party/
articles/party.cljc
```

#### 5. Documents (General)
```
model/document/
queries/document.clj
mutations/document.clj
ui/document/
ui/document_dashboard/
```

---

## Decision Matrix: Where Does My Code Go?

### Adding New Functionality

| I Want To... | Use Namespace | Example |
|--------------|---------------|---------|
| Define entity structure | `model.*` | `dataico.model.invoice-item` |
| Create RAD form | `model_rad.*` | `dataico.model_rad.invoice` |
| Read from database | `queries.*` | `dataico.queries.invoice` |
| Write to database | `mutations.*` + `transactions.*` | `dataico.mutations.invoice` |
| Build complex objects | `builders.*` | `dataico.builders.invoice` |
| Integrate external service | `services.*` | `dataico.services.dian` |
| Background processing | `jobs.*` | `dataico.jobs.pdf_job` |
| Import/export data | `pipeline.adapters/formatters` | `pipeline.formatters.accounting.softland` |
| Add UI component | `ui.{domain}.*` | `dataico.ui.invoice.form` |
| Create utility function (generic) | `lib.*` | `dataico.lib.dates` |
| Create utility function (domain) | `articles.*` | `dataico.articles.accounting` |
| Computed fields | `resolvers.*` | `dataico.resolvers.invoice` |
| Rule engine logic | `data_engine.*` | `dataico.data_engine.rules` |
| Business metrics | `analytics.*` | `dataico.analytics.mrr` |

### Refactoring Existing Code

| Code Currently Does... | Move To | If... |
|------------------------|---------|-------|
| Business logic in query | `model.*` or `builders.*` | Pure calculation/construction |
| Database writes in service | `mutations.*` + `transactions.*` | CRUD operation |
| UI logic in mutation | `ui.*.actions` | Client-side only |
| Repeated domain logic | `articles.*` | Domain-specific utility |
| Repeated generic logic | `lib.*` | Non-domain utility |

---

## Best Practices

### 1. Separation of Concerns
- **Queries** only read, never write
- **Mutations** orchestrate, don't implement low-level DB ops
- **Transactions** handle DB writes, not business logic
- **Builders** construct, don't persist
- **Services** integrate external systems, not domain logic

### 2. Cross-Platform Code (.cljc)
Use `.cljc` for:
- Domain models
- Business calculations
- Validation logic
- Shared utilities

Use reader conditionals `#?(:clj ... :cljs ...)` sparingly.

### 3. Namespace Dependencies
Typical dependency flow:
```
UI → Mutations → Builders → Model
     ↓            ↓
  Transactions  Queries → Resolvers
     ↓
  Services
```

### 4. Naming Consistency
- Entity namespaces match entity names
- Use consistent suffixes: `-actions`, `-query`, `-form`, `-route`
- Domain folders group related functionality

### 5. File Organization
- One namespace per file
- Group related namespaces in folders
- Use subdirectories for domain sub-areas

---

## Common Patterns

### Pattern 1: CRUD Operation
```clojure
;; 1. Define entity
(ns dataico.model.widget)

;; 2. Create RAD attributes
(ns dataico.model_rad.widget)

;; 3. Query logic
(ns dataico.queries.widget)

;; 4. Mutation
(ns dataico.mutations.widget)

;; 5. Transaction
(ns dataico.transactions.widget)

;; 6. UI
(ns dataico.ui.widget.form)
(ns dataico.ui.widget.actions)
```

### Pattern 2: External Integration
```clojure
;; 1. Service wrapper
(ns dataico.services.external-api)

;; 2. Mutation to call service
(ns dataico.mutations.external-sync)

;; 3. Background job (if async)
(ns dataico.jobs.external-sync-job)
```

### Pattern 3: Import/Export
```clojure
;; Import
(ns dataico.pipeline.adapters.source-system)

;; Export
(ns dataico.pipeline.formatters.target-system)

;; Orchestration
(ns dataico.mutations.import-data)
```

### Pattern 4: Computed Data
```clojure
;; 1. Calculation logic
(ns dataico.model.invoice-item)  ; or
(ns dataico.articles.invoice)

;; 2. Resolver
(ns dataico.resolvers.invoice)
```

---

## Quick Reference: Key Namespaces

### Core Infrastructure
- `dataico.entity` - Entity utilities (id, ident, company-id)
- `dataico.model` - Model utilities and gen-uuid
- `dataico.utils` - General utilities
- `dataico.seeds` - Seed data

### Database
- `dataico.lib.datomic.utils` - Datomic utilities
- `dataico.lib.datomic.api` - Datomic API wrapper
- `dataico.server_components.datomic.*` - DB connection

### UI Framework
- `dataico.ui.crud` - Generic CRUD UI
- `dataico.ui.kit.*` - UI component kit
- `dataico.ui.state_machines.*` - State machines

### Security
- `dataico.lib.security.*` - Security utilities
- `dataico.model.permissions` - Permission model

---

## Migration Guide

### Moving Code Between Layers

#### From Query to Model
**When**: Query contains calculations
```clojure
;; Bad: Calculation in query
(ns dataico.queries.invoice)
(defn subtotal [item]
  (* (:price item) (:quantity item)))

;; Good: Calculation in model
(ns dataico.model.invoice-item)
(defn subtotal [{:keys [price quantity]}]
  (* price quantity))
```

#### From Mutation to Transaction
**When**: Mutation has low-level DB logic
```clojure
;; Bad: DB details in mutation
(ns dataico.mutations.invoice)
(defn save-invoice [env invoice]
  (d/transact conn [invoice]))

;; Good: DB in transaction layer
(ns dataico.transactions.invoice)
(defn save-invoice-tx [db invoice]
  [(assoc invoice ...)])

(ns dataico.mutations.invoice)
(defn save-invoice [env invoice]
  (transactions.invoice/save-invoice-tx db invoice))
```

#### From Lib to Articles
**When**: "Generic" utility is actually domain-specific
```clojure
;; Bad: Domain logic in lib
(ns dataico.lib.invoice-helpers)

;; Good: Domain logic in articles
(ns dataico.articles.invoice)
```

---

## Conclusion

This architecture provides:
- **Clear separation of concerns** across layers
- **Domain-driven organization** within layers
- **Consistent patterns** for common tasks
- **Scalable structure** for large codebases

### Key Principles

1. **Model First** - Start with domain model
2. **Layer Discipline** - Respect layer boundaries
3. **Domain Cohesion** - Keep related code together
4. **Consistent Naming** - Follow established patterns
5. **Shared Logic in .cljc** - Maximize code reuse

---

## Getting Help

When unsure where code belongs:

1. **Identify the concern**: Data, logic, UI, integration?
2. **Find the domain**: Invoice, accounting, payroll, etc.
3. **Choose the layer**: Model, queries, mutations, etc.
4. **Follow patterns**: Look at similar existing code
5. **Ask**: Check with team if still unclear

The architecture is designed to make the right choice obvious. When in doubt, prefer:
- More specific over generic
- Domain layer over cross-cutting
- Existing patterns over new ones
