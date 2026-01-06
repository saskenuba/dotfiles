# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Colombian electronic invoicing and accounting platform built with:
- **Backend**: Clojure with Datomic (primary database) and PostgreSQL (SQL aggregates)
- **Frontend**: ClojureScript with Fulcro (component-based UI framework)
- **Build**: deps.edn for Clojure, shadow-cljs for ClojureScript

The UI is in Spanish (Colombian locale), but source code and comments are in English.

**IMPORTANT** At every user interaction, you should check your skills to check if there is something relevant to the task.

## Build & Test Commands
- **Run single test**: `(require 'ns-test) (in-ns 'ns-test) (require '[kaocha.repl :as k]) (k/run 'ns-test)`
- **Check CLJS build**: `(require '[shadow-cljs-checker :as checker]) (checker/wait-and-report :main)`
- **Mute logs in tests**: `(helpers.test-helpers/set-log-level! :fatal) (taoensso.timbre/set-min-level! :fatal)`

## Running Tests

When running tests, you should start a repl. It's much faster than
running tests individually, because otherwise you need to keep waiting
for the REPL to be ready and this takes 2+ minutes everytime.

Start with these JVM args:
```
-Ddev
-Dtest
-Duser.timezone=UTC
-Dguardrails.enabled=true
-Dconfig=config/dev.edn
-XX:-OmitStackTraceInFastThrow
```

and the following aliases: `:dev, :cljs, :test, :myapp/storm-clj`.

At the test REPL:
```clojure
(do
  (require 'your.namespace-test)
  (in-ns 'your.namespace-test)
  (require '[kaocha.repl :as k])
  (require 'helpers.test-helpers)
  (helpers.test-helpers/set-log-level! :fatal)
  (taoensso.timbre/set-min-level! :fatal)
  (k/run 'your.namespace-test))
```

Always mute the logs when doings tests.

## Clojure REPL Evaluation (clojure-mcp-light)

When you are not testing, the `clj-nrepl-eval` command is installed for evaluating Clojure code via nREPL.

**Discover nREPL servers:**
```bash
clj-nrepl-eval --discover-ports
```

**Evaluate code:**
```bash
clj-nrepl-eval -p <port> "<clojure-code>"
```

**With timeout (milliseconds):**
```bash
clj-nrepl-eval -p <port> --timeout 5000 "<clojure-code>"
```

The REPL session persists between evaluations - namespaces and state are maintained.
Always use `:reload` when requiring namespaces to pick up changes.

## Clojure Parenthesis Repair

The `clj-paren-repair` command is installed for fixing delimiter errors.

**Usage:**
```bash
clj-paren-repair <files>
clj-paren-repair path/to/file1.clj path/to/file2.clj
```

**IMPORTANT:** Do NOT try to manually repair parenthesis errors. If you encounter unbalanced delimiters, run `clj-paren-repair` on the file instead of attempting to fix them yourself.

## ClojureScript REPL & Fulcro DB

You can directly access Fulcro's normalized DB.

**Connect to CLJS REPL (port 9001):**
```bash
clj-nrepl-eval -p 9001 "(shadow/repl :main)"  # Connect to browser
clj-nrepl-eval -p 9001 "<code>"               # Evaluate CLJS
```

**Access Fulcro DB:**
```clojure
(require '[myapp.application :as application])
(require '[com.fulcrologic.fulcro.application :as app])
(app/current-state application/SPA)           ; Full state
(keys (app/current-state application/SPA))    ; Top-level keys
```

This is useful when in doubt, debugging, or to help when designing a new feature. Use this to your benefit.

## Flow-storm Debugger

You have a powerful debugger at your disposal. It works with both CLJ and CLJS. CLJS it has some quirks, but it can also work too, but needs more interaction with the user. Have this in mind when trying to debug an error.

## Code Organization

```
src/main/myapp/
├── lib/            # Shared utilities and infrastructure
├── model/          # Domain model, business rules, data access
├── model_rad/      # RAD entity definitions
├── builders/       # Test factory functions for entities
├── ui/             # Frontend components by feature
├── dian/           # DIAN (Colombian tax authority) integration
├── resolvers/      # Pathom resolvers (backend queries)
├── mutations/      # Frontend and backend mutation handlers
├── data_engine/    # Rules engine and expression evaluation
├── queries/        # Query functions
├── server_components/ # Server infrastructure
├── services/       # Business services
├── subscriptions/  # Subscription management
└── accounting/     # Accounting reports and aggregates
```

*IMPORTANT*: Do not use myapp.services.time namespace. Use myapp.lib.time instead.

## Reference Documentation

You have many skills in your folder for project related standards. You
NEED to use them when relevant.

Also see:
- `/docs/DeveloperGuide.adoc` - Environment setup, REPL usage, deployment

## Writing code & Coding Standards

You should always load your clojure skill to write code following some guidelines.

When writing code, you need to always think like a Senior Clojure
Engineer. You focus on solutions that are readable and easy to
maintain. Facing ambiguity, you will follow industry standards and ask
the user for clarification if you can't find the answer yourself.

- **Schema**: Malli for data validation
- **Functions**: Guardrails (`>defn`) for pre/post conditions.
- **Tests**: Fulcro Spec with `:groupN` markers. 
- **Naming**: kebab-case (ClojureScript convention)
- **UI Components**: Fulcro `defsc` with co-located queries
- **Errors**: `ex-info` for structured error data

## Git

- Never mention Claude in any commits
- Follow conventional commits
