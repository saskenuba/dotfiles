---
name: datomic-optimization
description: How to extract data from a Datomic database for maximum scalability and performance, with particular emphasis on leveraging standard query and indexes to get the best overall multiuser performance with emphasis on properly leveraging `d/q`, `d/index-pull`, `d/index-range`, `d/datoms` of the Cloure `datomic.client.api`.
---

# Datomic 

Datomic stores data as Datoms: A tuple containing [id, attribute, value]. A to-one attribute allows only one datom per id/attribute, and a to-many attribute allows any number of values to share the same id/attribute.
These three items are generally referred to as the `E` (entity, because datoms with the same ID clump together as a bag of facts), 
`A` (for attribute) and `V` (for value). Thus, a shorthand for a Datom would be EAV. The Datom also includes a transaction ID (T) and added/retracted flag. These latter two elements are rarely needed/mentioned for standard queries. 

NOTE: Datomic functions (except for `q`) all have a default result limit of 1000. All functions that take a map as an argument will accept a :timeout (in ms, default 60000) and :limit option. Be sure to include these two for operations that may run long or return a lot of data. It is a *very* common problem to silently hit the limit of 1000 and accidentally and have faulty production results. Using -1 for limit allows unlimited results, which of course algorithms should be prepared to handle.

## Indexes

Datomic creates *covering* indexes of datoms for different sorting orders. We can assume that a lookup on an index will have O(log) performance, and that the branching factor is high, so that a small number of reads will be necessary against an index when we know the primary (and possible secondary) value of an entry in that index.

The covering indexes which have these items sorted in the specified orders are:

* EAVT: all datoms
* AEVT: all datoms
* AVET: all datoms
* VAET: ONLY :db.type/ref attributes (attributes whose values are the E of other entities)

These indexes are directly accessible with `d/datoms` (any index) and `d/index-range` (only AVET).

### Examples

```clojure
;; Assuming this is in the db:
{:db/id 22
 :invoice/number 96}

;; Then this returns [96]
(mapv :v
  (d/datoms db {:index :eavt 
                :components [22 :invoice/number]
                :limit 1}))
```

```clojure
;; Assuming these are in the db
{:db/id 22 :invoice/number 96}
{:db/id 23 :invoice/number 97}
{:db/id 24 :invoice/number 98}
{:db/id 24 :invoice/number 99}

;; Then this returns [[23 97] [24 98]]
(mapv (juxt :e :v)
  (d/index-range db 
    {:attrid :invoice/number
     :start  97    ; inclusive v
     :end    99})) ; non-inclusive v
```

### Index Pull

A related function is `index-pull`, which combines an index walk with a `pull`
of facts for each entity. This is the most efficient way to grab a fixed set of
entity facts where one of the indexes can be used to find the entities of
interest. Of course the `pull` aspect of the operation is high-impact, but if
you actually need the extra facts, it is hard to do better (a covering
composite tuple can do significantly better, but only for a flat and limited
set of facts).

Index pull is lazy and chunked, meaning it will do progressing network round
trips to fulfill the request. This can lead to easy timeouts (if your item
processing is slow) or even over-fetching if you commonly terminate the
sequence early.

For example, say you're using index-pull to satisfy paginated data for a UI:

```clojure
(d/index-pull db
  {:index :avet
   :start [:invoice/number 0]
   :selector [:invoice/number :invoice/date {:invoice/customer [:customer/name ...}]
   :offset page-offset
   :limit page-size})
```

The lazy nature here probably doesn't hurt anything (you're going to process
the page-size items to a client), though it isn't a bad idea to force the
realization if you can afford the memory and expect any processing delays (e.g.
`(doall (d/index-pull ...))`).

The *chunked* nature, however, *can* hurt you in *significant ways*. One is
that the chunk may be a decent amount larger than the number of items you actually use.
The other problem is the.  `:selector` is the problem: It can ask for some arbitrary large
relational graph, which will do considerable I/O. So any "extra" item you get
and discard have that much more impact. So, something like this can have a lot of 
"hidden extra" I/O:

```clojure
;; Get the first 10 invoices that were created this month (invoice numbers are assumed to be used sequentially)
(take 10 ; Even if you luck out and don't have to skip ANY, index-pull may have processed/returned 100 to chunk networking
  (filter
    invoice-this-month? ; no idea how many you "skip" (useless selectors were run)
    (d/index-pull db
      {:index :avet
       :start [:invoice/number 0]
       ;; Selector uses EAVT index to get adjacent facts, but join to customer is a random seek
       :selector [:invoice/number :invoice/date {:invoice/customer [:customer/name ...}]})))
```

As a result, avoid using `index-pull` where you cannot rely on a solid offset/limit, and not post-processing.
Composite tuples can help with this.

## Standard Query and Clause Order

A standard query uses the `q` function, which takes some number of inputs, and
where clauses. Each clause of the where in Datomic is evaluated in ORDER and
results in a set of facts from the database that have matched "so far".
Successive clauses are automatically unified so that matching bindings mean the same set of values.

```clojure
(require '[datomic.client.api :as d])

(d/q '[:find ?v
       :in $ ?x
       :where
       [?e :other ?x]   ; Use AVET to find [:other 23 ?e]. ?e is now some set of real entity IDs
       [?e :attr ?v]]   ; Use EAVT to find [<one-of-the-?e> :attr ?v]. Each e causes an index lookup to find a possible ?v
  some-db 23)
```

Thus the overal I/O of this query will cause a jump on the AVET index to the starting point where a given A/V combo starts, and then a sequential scan of the AVET index for as long as A/V matches the input, resulting in N values for ?e. EACH of those ?e values will then cause a (random) index access on EAVT (with the now known ?e and :attr) to find possible Vs (if :attr is to-many, each ?e could result in a sequential scan of adjacent index items to gather the values). 

Datomic stores indexes in *segments*, which are blocks of adjacent datoms in the index. Thus, jumping to a point in an index invoves reading *segments* not *datoms*. This means that adjacent datoms are *highly likely* to be in memory already when a sequential scan is needed.

A segment holds roughly 2000 datoms (depends on value size).

## Query Operation

Most applications have some (usually small) number of related facts that need
to be joined together in order to find the subset of data of interest. For
example, "Find the invoice owned by Foo whose number is 42". 

The problem is that such a query in Datomic can be quite inefficient in the presence of large data sets:

```clojure
(d/q '[:find ?v
       :in $ ?company ?n
       :where
       [?e :invoice/company ?company] ; Use AVET, but might find 100k invoices in a large company
       [?e :invoice/number ?n]]       ; Two possible ways: Could use AVET (bet that 42 is rare) and subset against prior set
                                      ; Hit EAVT for every (known) [?e :invoice/number 42] looking for all matches
  some-db input-company 42)
```

Either way the first clause possibly finds a large set of e's, which is then
used in the next sub-filter. The internal algorithms can do some various
optimizations to try to make this better. Reordering clauses MAY help:


```clojure
(d/q '[:find ?v
       :in $ ?company ?n
       :where
       [?e :invoice/number ?n]         ; IF the number is rare, perhaps we get very few ?e
       [?e :invoice/company ?company]] ; Sub-filter the ?e against EAVT for the known company
  some-db input-company 42)
```

but of course which wins very much depends on the statistics of the data. If 42
is extremely common and it is a small company with few invoices the first is
definitely better, but for a company with a large number of invoices and some
rare invoice number, the second will win.

In this case doing some real-world analysis of the data/statistics can help
choose the one that is better for general multi-user use; however, reordering
clauses is only highly effective if there is a *definitive* first clause that
results in a small number of matches.  Of course unrelated early clauses (where
filtering is not being applied because two unrelated lookups are going to be
related later) can affect things with additional overhead.

```clojure
(d/q '[:find ?v
       :in $ ?x ?y
       :where
       [?e :foo ?x]
       [?e2 :other ?y] ; unrelated to first clause. Straight AVET lookup
       ...] ...)
```

NOTE: It is possible to give timeouts and limits to `q`. The timeout just
affects the network (not the back-end load...it abandons the running query
which continues) and the limit affects how many things are returned, but only
after the complete set is calculated. This makes them of very limited utility in 
any kind of optimization.

```clojure
(d/q {:query '[:find ...]
      :args [db v]    ; The inputs
      :timeout 300000 ; 5 minutes
      :limit 5})      ; limit is a post-processing limit. Doesn't make query processing more efficient
```

## Pagination and Filtering

There is a direct implication for most applications on the very common task of reporting where one needs pagination
and filtering of data. By definition pagination requires a total order, the ability to remove unwanted elements
and the ability to return a subset of the facts. A datomic `q` will *always* require obtaining the complete
data set (never sorted), which must then be post-sorted and then chunked to find the desired page:

```clojure
(let [results (d/q '[:find ?x
                     :where
                     ....] db)
      page (take 10 (drop 100 (sort results)))]
  page)
```

this leads to horrific performance in a highly concurrent multiuser
environment, even on databases that fit entirely in cache. This use-case
almost always needs a purpose-specific index, which in Datomic are created via
Composite Tuples.

## Composite Tuples

The most important feature in Datomic for optimal query and pagination is Composite Tuples.

A Composite Tuple is an auto-derived (and managed) attribute whose value is a
combination of the value of other attributes in that same entity. This allows
you to create your own "entity index". Take the invoice number example. If it
is common to look up invoices by number, then a composite tuple whose value is
a combination of company/number results in the following data in the AVET
index:

VAET
```edn
[[company-1 N1] :invoice/company+number <e8471>]
[[company-1 N2] :invoice/company+number <e9284>]
[[company-1 N3] :invoice/company+number <e211>]
[[company-2 N1] :invoice/company+number <e499>]
[[company-2 N2] :invoice/company+number <e883774>]
[[company-2 N2] :invoice/company+number <e113>]
```

so that a direct lookup of a given combo of *company and invoice number* results in a single index lookup 
(which may read multiple segments, but the sorted nature ensures a minimum amount of I/O).

This same composite tuple could be used to paginate a company's invoices by number. This use case
can be done, for example, with `index-pull`:

```clojure
(d/index-pull db {:index :avet
                  :selector [:invoice/id :invoice/number ...]
                  :start [:invoice/company+number [company-dbid nil]]
                  :offset 100
                  :limit 10})
```

Such an operation can jump directly to the starting point in the index that
matches the given `:start`, skip `:offset` items, and just grab 10 results.
This is a vast improvement over `q`; however, filtering presents an additional
challenge, since more I/O is necessary to get any additional information
*about* the invoices, and if we want 10, but are going to filter some out
(using additional queries on the ones we got), then we're going to have trouble
knowing what limit to use!

However, since composite tuples can contain up to 8 entries and the index is covering,
it is possible to design a tuple that can serve multiple use cases where
the fast sequential access of related values can dramatically reduce the IO
necessary to accomplish at least the initial task of finding the items of interest, 
and subsequent queries based on those IDs can fill out the necessary details.

```clojure
(d/q '[:find (pull ?e pattern)
       :in $ pattern [?e ...]]
  db desired-pull-pattern entity-ids)
```

#### Pagination and Filtering

The goal of filtering/pagination across a decent number of filtering controls
is a great use-case. The main realization is that you can read a segment of the
index using `index-range`. This operation jumps to a particular location in the
index, and allows you to grab a sequential section of it as a sequential scan
of that range. This is as good as things get in terms of IO.

There is no query power to this operation, other than leveraging the sort order that 
is inherent to the index. This operation is *always* against the AVET index.

As a result, the filter/pagination case will often grab a complete range

```clojure
(d/index-range db {:attrid :invoice/company+number+date+deleted
                   :start  [<company-dbid>]    ; inclusive
                   :end    [(inc <company-dbid>) ; non-inclusive
                   :limit -1])
```

and then sub-filter the datoms (which have :e :a and :v keys as if they were
maps). The `:v` of course will be the tuple value. Refs in such a value will be
the raw :db/id of the target entity, and as such must be the raw entity ID in
the start/end parameters (and not domain-level things like unique lookup refs).

The *most important* criteria for this use-case is that the initial sequence of
tuple values are in the optimal order to minimize the amount of the index you
have to grab before filtering. So, for example, it is common for ownership to
be the first criteria (I probably can't query for the other company's
invoices), and if I always require the user to specify a date range, then that
is the obvious best idea for the second element. At that point the order of
additional elements doesn't probably matter, and they are there just to make
sure they are there for post-filtering without additional I/O.

The filtering/pagination would be a processing post-step on the raw data.

NOTE: Since the tuple contains a number of facts, you can turn the
post-processed data into maps of those facts, and in fact it can be
advantageous to include more things in the tuple (unrelated to filtering) which
are always of interest to the end user, possibly avoiding additional database I/O.

```clojure
(mapv 
  (fn [{:keys [e v] :as datom}] 
    {:db/id e
     :invoice/company  (nth v 0)
     :invoice/number   (nth v 1)
     :invoice/date     (nth v 2)
     :invoice/deleted? (nth v 3 false)}) 
  datoms-from-index-range)
```

Additional Notes:

* A tuple value is created if *any* entity contains *any* of the named attributes.
* Missing attributes still hold a space with a nil
* nil sorts first

### Problems with Composite Tuples

We must take the following facts into *serious* consideration:

* A tuple is only directly usable in `q` *if* you know *all* of the values (the complete V of the datom).
* Tuples have a hard limit of 8 slots for the value.
* Tuples cannot be removed once created. Experimental creation should *never*
  be done on a production database. The result is a permanent drain during
transations (which must maintain the tuple), and an increased database size.

This means you want to be *extremely* conservative on the number of composite tuples created, and when you 
*do* create one, make sure it has maximal utility for the various use-cases that might be needed for that
particular entity type.

## Antipattern - Attribute Reuse across Entity Types

*Avoid* using an attribute across multiple entity types, especially if it is
likely to be used in a composite tuple.  While convenient for some use-cases
(e.g.  `:entity/deleted?` to mark things as soft-deleted) the problem is that
the indexes get polluted with MANY values of that attribute across all other
concerns (entity type, ownership, etc.) and such attributes will lead to high
pollution rates in any composite tuple in which they are used.

```clojure
(d/q '[:find ?e 
       :where
       ;; Not sure which order is better...find all products everywhere first, or all deleted entities?
       [?e :entity/deleted? true] ; Finds all entities that have been deleted. Not constrained by type/owner/etc.
       [?e :product/sku] ; have to future constrain to products.
       ...)
```

The proper use is to include the entity type as the namespace, even for common
concepts. Using `:product/deleted?` instead of `:entity/deleted?` means that
functional algorithms may not be able to be generic for "soft delete", but the
resulting database *will* be able to create indexes that can more effectively
skip deleted things, which is always necessary.

```clojure
(d/q '[:find ?e 
       :where
       [?e :product/deleted? true] ; Finds all *products* that have been deleted. Much smaller set 
       ...)
```

In terms of Composite Tuples imagine a the following tuples on products, and
invoices. If both use ":entity/deleted?" then the composite tuples for *both*
end up with high pollution of nils in the indexes:

```
{:db/id 1
 :entity/deleted? true
 :invoice/company 99
 :invoice/number 33}

{:db/id 100
 :entity/deleted? true
 :product/company 188
 :product/sku "foo"}

;; EAVT datoms for invoice/company+deleted+number include:
[100 :invoice/company+deleted_number [nil true nil]] ; useless entry for product 100
[1   :invoice/company+deleted_number [99 true 31]]   ; for the actual invoice

;; EAVT datoms for product/company+deleted+sku
[1 :product/company+deleted+sku [nil true nil]]     ; useless invoice-related item
[100 :product/company+deleted+sku [188 true "foo"]] ; actual useful product entry
```

## Existing Library Code

We commonly use some
