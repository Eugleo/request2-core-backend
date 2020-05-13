# Revision history for Selda


## 0.5.1.0 -- 2020-01-20

* Support for raw SQL fragments. (#134)
* Expose tableName.
* Document performance drawbacks of withoutForeignKeyEnforcement.
* Fix several bugs validating auto-incrementing PKs. (#133)


## 0.5.0.0 -- 2019-09-21

* index and indexUsing now accept a Group instead of a Selector (#121)
* Custom type errors for scope mismatches.
* Provide Generic instances for ID and RowID.
* Provide To/FromJSON instances for ID and RowID (selda-json).
* Add back MonadTrans instance for SeldaT.


## 0.4.0.0 -- 2019-06-02

* Type-safe support for backend-specific functionality. Top level query definitions now require explicit type signature. (#80)
* Native UUID support. (#47)
* Support JSON columns on all backends through aeson.
* Support JSON lookups (i.e. SELECT json_column.some_property FROM ...) on PostgreSQL.
* Multi-column primary key and uniqueness constraint support. (#25, #99)
* Switch to PostgreSQL binary protocol. (#18)
* Prevent dangerous user-defined SQL result instances.
* Expose backend internals through Database.Selda.Backend.Internal. (#109)
* Expose SQLite connection handle. (#101)
* Make MonadSelda more amenable to connection pooling. (#108)
* Add weakly auto-incrementing primary keys. (#94)
* Move compile* functions to Database.Selda.Debug.
* Remove half the tuple convenience functions.
* Remove in-process cache. (#117)
* Officially support GHC 8.6, 8.8 (SQLite only until postgres dependencies catch up with 8.8).
* Drop support for GHC 7.10. (#118)
* Manual (i.e. non record label) selectors are no longer exported by default; import Database.Selda.MakeSelectors is you need them. (#118)
* Update toolchain to use v2-style cabal commands.
* Fix date/time types for PostgreSQL. (#104)
* Fix bug when migrating tables with indexes. (#107)
* Misc. smaller bug fixes.


## 0.3.4.0 -- 2018-09-29

* Added convenience functions for working with nullable columns.


## 0.3.3.1 -- 2018-09-04

* DISTINCT should now always return distinct results.
* DISTINCT can no longer produce ill-scoped queries.


## 0.3.3.0 -- 2018-09-01

* Ad hoc selectors using OverloadedLabels.
* Shorter build times.
* Minor API updates and simplifications.


## 0.3.2.0 -- 2018-08-07

* Some aggregates are now nullable.
* sum_ on an empty table doesn't crash anymore.
* Aggregating over an empty selectValues doesn't crash anymore.


## 0.3.1.0 -- 2018-08-06

* Minor API fix when defining table attributes.


## 0.3.0.0 -- 2018-08-05

* Support for Stack and GHC 8.4.
* Precedence fix for selector index (!) operator.
* Accept INT and SMALLINT columns in user-created PostgreSQL tables.
* Add combinator for turning off foreign key checking.
* Rename unsafeRowId/unsafeId to toRowId/rowId.
* Add typed row identifiers.
* More generic type for sum_.
* Table validation against current database.
* Basic migration support.
* Basic index support.
* Remove ad hoc tables; only generic tables from now on.


## 0.2.0.0 -- 2018-04-02

* Support custom column names for generic tables.
* Scope safety fix for inner queries.
* Better type errors on GHC 8+ for inner queries.


## 0.1.12.1 -- 2018-02-27

* New PPConfig hook for more flexibility when compiling types.


## 0.1.12.0 -- 2018-01-11

* Allow recursive and optional foreign keys.
* Allow arbitrary enums in tables, represented as text.
* Fix RowID issues for PostgreSQL.
* Fix auto-incrementing primary keys for generic tables.


## 0.1.11.2 -- 2017-12-14

* Fix treatment of booleans in PostgreSQL backend.


## 0.1.11.1 -- 2017-10-10

* Fix rare infinite loop bug in in-process cache.


## 0.1.11.0 -- 2017-09-08

* Fix name generation in the presence of isIn over queries.
* SELECT DISTINCT support.
* Conditional expressions and matchNull.


## 0.1.10.1 -- 2017-08-11

* Fix name generation in the presence of multiple aggregates.


## 0.1.10.0 -- 2017-08-01

* Async exception safety.
* Allow MonadSelda instances not built on SeldaT.
* Chunk very large insertions on backends that request it (i.e. SQLite).
* GHC 8.2 support.


## 0.1.9.0 -- 2017-06-16

* Properly document semantics of order.
* Export conditional inserts.
* Fix Haste build for backends.


## 0.1.8.0 -- 2017-06-10

* Move SQL pretty-printing config into a single type.
* Support for binary blobs.
* Support for prepared statements.
* Support for connection reuse across Selda computations.
* Cleaner and more robust backend API.
* Stricter type constraints on comparisons.
* Allow limit on inner queries.
* Allow inspecting row identifiers.


## 0.1.7.0 -- 2017-05-17

* Add specialized insertUnless upsert variant.
* Fix potential race condition in upserts.
* Use abstract row identifier type for auto-incrementing primary keys.
* Less strict version bounds on dependencies.


## 0.1.6.0 -- 2017-05-07

* Conditional insert ("upsert") support.
* Support `SELECT x IN (SELECT ...)` and `SELECT x IN (a, b, ...)` queries.
* Explicit inner queries.
* Rename `inner` to `innerJoin`, more intuitive behavior for `suchThat`.
* Add `from` shorthand for `\s q -> fmap (!s) q`.
* Unique and foreign key constraints for generics.


## 0.1.5.0 -- 2017-05-05

* Inner join support.
* More sensible names in backend API.
* Fix rounding and casts.


## 0.1.4.1 -- 2017-05-04

* Fix cache consistency bug in the presence of multiple databases.


## 0.1.4.0 -- 2017-05-04

* Add uniqueness constraints and foreign keys.


## 0.1.3.3 -- 2017-05-04

* Fix cache invalidation race when using transactions.


## 0.1.3.2 -- 2017-05-01

* Only throw well-documented, Selda-specific exceptions.


## 0.1.3.1 -- 2017-05-01

* More Hackage-friendly README.


## 0.1.3.0 -- 2017-04-30

* Add selectors for non-generic tables.
* Allow default insertions on all columns.
* More sensible API for LIMIT.
* Fix broken SQL being generated for pathological corner cases.
* Documentation fixes.


## 0.1.2.0 -- 2017-04-20

* Replace `¤` with `:*:` in table definitions.


## 0.1.1.1 -- 2017-04-20

* Minor documentation fixes.


## 0.1.1.0 -- 2017-04-20

* Generic tables, queries and mutation.
* Select from inline tables.
* Tutorial updates.
* Minor bugfixes.


## 0.1.0.0 -- 2017-04-14

* Initial release.
