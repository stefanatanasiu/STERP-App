# Technical debt — module `fin`

Deliberate legacy texture and genuine compromises in General Ledger and Accounts. Everything here
is **correct but dated**, or correct-but-duplicated; nothing in this list is a known defect except
where it says so explicitly. One bullet per item: file, pattern, why it is like that, how to
modernise.

Finance is the oldest ledger in the suite and the one every other module posts through, so most of
the texture below exists because the posting contract cannot be allowed to change underneath nine
other modules. `cobol\DIALECTS.cfg` pins the module to **Micro Focus COBOL** (`fin=mf`), fixed
format.

See `docs\modules\fin.md` for what the module does.

---

## 1. The staging table instead of a table-valued parameter

* **`fin.JournalStage` + `fin.usp_PostJournal` (`db\procs\fin\usp_PostJournal.sql`).** A journal
  is posted in two steps: the caller inserts its lines into a permanent staging table keyed by a
  correlation id, then calls the procedure, which passes the header as ordinary parameters.
  *Why it is like that:* COBOL cannot pass a table-valued parameter through ODBC, and the ledger
  has to be callable from COBOL as its first-class client. A permanent table was chosen over a
  temporary one because the staged rows must survive the `EXEC` boundary.
  *Why it is debt:* the staging table is shared mutable state. Two callers that generate the same
  correlation id would see each other's lines, and a caller that dies between the `INSERT` and the
  `EXEC` leaves orphan rows that nothing sweeps up. There is no foreign key from a staged line to
  anything, so a typo in `CompanyCode` is only caught by the account check.
  *Modernisation:* take the lines as a JSON parameter (`OPENJSON`) or a table-valued parameter
  from .NET callers, keep the staging path only for COBOL, and add a nightly sweep of staged rows
  older than a day whose correlation id never completed.

## 2. Five procedures that live in another procedure's file

* **`db\procs\fin\usp_TrialBalance.sql`, `usp_JournalEnquiry.sql`, `usp_VatBoxes.sql`,
  `usp_BudgetCompare.sql`.** Twelve procedures are spread over seven files.
  `usp_AccountActivity` is at the bottom of `usp_TrialBalance.sql`; `usp_CloseStatus` and
  `usp_BankOutstanding` are at the bottom of `usp_JournalEnquiry.sql`; `usp_VatBoxDrill` is at the
  bottom of `usp_VatBoxes.sql`; `usp_ControlRecon` is at the bottom of `usp_BudgetCompare.sql`.
  *Why it is like that:* each of the five is the drill-down partner of the procedure above it and
  was written in the same sitting, which is exactly how this happens in a real codebase.
  *Why it is debt:* `sys.objects` lists twelve finance procedures and `db\procs\fin` lists seven
  files, so the obvious conclusion — that five procedures exist only in the database and a clean
  rebuild would lose them — is wrong but takes a `OBJECT_DEFINITION` comparison to disprove. One
  agent already spent a session on exactly that false alarm.
  *Modernisation:* one procedure per file named after the procedure, and a build-time check that
  the set of `CREATE OR ALTER PROCEDURE fin.*` statements on disk matches the set in the database.

## 3. The line budget pushed business logic into COBOL

* **`cobol\online\fin\CTLRECON.cob` and `db\procs\fin`.** Non-seed finance T-SQL stands at **490
  lines against a ~450 budget** (CONVENTIONS §6). Control account reconciliation, which is a
  natural set operation, was written as a COBOL online program instead.
  *Why it is like that:* a hard ceiling on one language with no ceiling on the other.
  *Why it is debt:* the placement of a piece of logic now records which budget had room left when
  it was written, not where it belongs. `CTLRECON` reads subledger balances row by row where a
  single `GROUP BY` would do.
  *Modernisation:* budget by module rather than by language, and move set-shaped aggregation back
  to the database once the ceiling is lifted.

## 4. `@SuppressResult`, and the nested `INSERT EXEC` it works around

* **`db\procs\fin\usp_PostJournal.sql`, `usp_ReverseJournal.sql`.** `usp_PostJournal` normally
  returns a one-row result set describing the journal it posted. SQL Server forbids an
  `INSERT ... EXEC` inside another `INSERT ... EXEC` (msg 8164), so any procedure that wanted to
  capture that result while itself being called that way failed. The flag turns the result set
  off; `usp_ReverseJournal` was rewritten to avoid the nesting entirely.
  *Why it is debt:* a parameter that exists only to dodge an engine limitation. Callers have to
  know when to set it, and the two code paths (with and without the result set) are only ever
  exercised together by accident.
  *Modernisation:* return the journal number through an `OUTPUT` parameter and let the result set
  go, or have the procedure write to a well-known output table the caller reads afterwards.

## 5. The posting contract knows the shape of its own dimensions

* **`db\procs\fin\usp_PostJournal.sql`.** The dimension checks are written out per dimension:
  `r.DimensionCode = 'CC' AND s.DimCostCentre ...` and `r.DimensionCode = 'PROJ' AND
  s.DimProject ...`, repeated for `MANDATORY` and for `FORBIDDEN`, and again for the value
  existence checks.
  *Why it is like that:* `fin.JournalStage` and `fin.JournalLine` carry cost centre and project as
  two named columns, which is fast, indexable and exactly how a 1990s ledger was built.
  *Why it is debt:* `fin.Dimension` and `fin.DimensionValue` are a general two-dimension model,
  but a third dimension cannot be added without altering two tables, the staging contract, every
  copybook that describes a journal line and four checks in this procedure.
  *Modernisation:* a `fin.JournalLineDimension` child table keyed by dimension code, with the two
  existing columns kept as computed columns for as long as the COBOL needs them.

## 6. Control accounts are protected by a rule about the caller, not the account

* **`db\procs\fin\usp_PostJournal.sql`, `fin.Account.AllowManualJournal`.** A control account
  refuses a manual journal — but the test is `@JournalType = 'GL' AND @SourceModule = 'FIN'`.
  Anything that declares a different source module may post to it.
  *Why it is like that:* the subledgers genuinely do have to post to their own control accounts,
  and they identify themselves by `@SourceModule`.
  *Why it is debt:* the protection is an honesty box. `db\seed\91-fin-demo.sql` passes
  `@SourceModule = 'AR'` precisely so it may touch `1200` and `4000`, which is realistic but shows
  how thin the guard is.
  *Modernisation:* derive the source module from the caller's identity (a database role or a
  signed service token) rather than taking the caller's word for it.

## 7. A number series default that pointed at nothing

* **`db\procs\fin\usp_PostJournal.sql`, `usp_ReverseJournal.sql`.** `@SeriesCode` defaulted to
  `'GLJNL'`. No such series is seeded by `07-core-numbering-terms.sql` — the real ones are `JNL`
  and `JNLR` — so every caller that relied on the default died with `STERP-E-0004` from
  `core.usp_NextDocumentNumber`. The defaults are now `'JNL'` and `'JNLR'`.
  *Still outstanding, and a genuine defect in another module:* **`cobol\online\so\SOB2B.cob` line
  843 and `cobol\batch\so\REBATE.cob` lines 965 and 1189 pass `'GLJNL'` explicitly** and will keep
  failing. The `so` team has to change those three literals; `fin` cannot fix it from this side
  without seeding a decoy series.
  *Modernisation:* make `@SeriesCode` mandatory so a wrong value is a compile-time conversation
  rather than a runtime abend, and add a deploy-time check that every series code referenced in
  source exists in `core.NumberSeries`.

## 8. The demo loader opens closed periods

* **`db\seed\91-fin-demo.sql`.** FY2026 periods 1 to 5 are hard closed and period 6 is soft
  closed, but the demonstration journals have to be posted into them. The loader sets those
  periods to `OPEN`, posts, and then writes the baseline statuses back.
  *Why it is like that:* posting through `fin.usp_PostJournal` rather than inserting into
  `fin.Journal` directly is worth far more than the awkwardness — the demo data obeys exactly the
  same rules as live data, which is the whole point of it.
  *Why it is debt:* the restore repeats the baseline rule from `db\seed\09-fin-period-vat.sql`
  (periods 1–5 `HARD`, 6 `SOFT`, 7–8 `OPEN`, with the matching `SoftCloseAt`, `HardCloseAt` and
  `ClosedBy`). Two files now have to agree about what the demo baseline is. An earlier version
  snapshotted the statuses instead, which was worse: a run that failed half way left the periods
  open, and the next run then saved `OPEN` as the baseline and restored that. Deriving the
  baseline makes the loader self-healing, at the cost of the duplication.
  *Modernisation:* one `core.Parameter`-driven definition of the demo close baseline that both
  scripts read, or a `@BypassPeriodStatus` parameter on the posting procedure guarded by a
  permission that only the loader holds.

## 9. VAT periods seeded in two files under two key conventions

* **`db\seed\11-fin-eventmap.sql` and `db\seed\12-fin-menu.sql`.** The event map file also seeds
  `fin.VatPeriod` for the current year (keys `26A1`–`26A4`, `27A1`); the menu file seeds the prior
  year (`25A1`–`25A4`) and all of `fin.StatutoryObligation`.
  *Why it is like that:* the current year quarters were needed by the event map tests before the
  menu seed existed, and `11-fin-eventmap.sql` is now a shared file that must not be rewritten.
  *Why it is debt:* a file called "eventmap" owning part of the VAT calendar is not discoverable,
  and the first attempt at the menu seed duplicated every quarter under a different key
  (`2627Q1` alongside `26A1`) before the clash surfaced as a unique-key violation on
  `fin.StatutoryObligation`. The guard in `12-fin-menu.sql` now matches on the **date range** as
  well as the key for that reason.
  *Modernisation:* one `09-fin-period-vat.sql` that owns the whole fiscal and VAT calendar, and a
  unique constraint on `(CompanyCode, StartDate, EndDate)` so a second key for the same quarter is
  impossible rather than merely discouraged.

## 10. The nine box return hard-codes the boxes that are always zero

* **`cobol\online\fin\VATRET9.cob`, `db\procs\fin\usp_VatBoxes.sql`, `db\seed\09-fin-period-vat.sql`.**
  Boxes 2, 8 and 9 are always zero, and the `fin.VatCode` rows carry `NULL` box numbers to match.
  *Why it is like that:* decision D-23 puts Northern Ireland to European Union goods movements out
  of scope.
  *Why it is debt:* the decision is spread across a seed file, a procedure and a COBOL program
  instead of being one switch. A company that acquires an XI registration — and `core.Company`
  already has an `XiVatRegNo` column for `STC` — needs three files changed.
  *Modernisation:* drive the box set from `fin.VatCode` alone and let a company with an XI
  registration activate the three codes that fill boxes 2, 8 and 9.

## 11. .NET over budget

* **`dotnet\STERP.Business\Finance`, `dotnet\STERP.Desktop\Forms\Finance`,
  `dotnet\STERP.Tests\Finance` — 1,268 lines against a ~800 line budget.**
  *Why it is like that:* five hand-written screens, each with its own COMMAREA mapping written by
  hand against the copybook, plus nine harness tests. There are no designer files; the overrun is
  all mapping and layout code.
  *Why it is debt:* every COMMAREA field offset and length is restated in VB.NET. The copybook and
  the facade have to be changed together, and nothing checks that they still agree.
  *Modernisation:* generate the CommArea structs from the `.cpy` files at build time, which would
  delete most of the overrun and make a mismatch a build error instead of a corrupt field.

## 12. Two tests still skip

* **`dotnet\STERP.Tests\Finance\FinanceTests.cs`.** The VAT drill-down and close-checklist tests
  reported `# SKIP` when they were written because `fin.VatTransaction` and `fin.CloseChecklist`
  were empty. `db\seed\91-fin-demo.sql` now seeds 42 VAT transactions and 24 checklist rows, so
  they should run — but they skip rather than fail when the tables are empty, which means a seed
  that silently stops working would be reported as a pass.
  *Modernisation:* assert the precondition and fail loudly, or move the fixture into the test so
  it does not depend on seed data at all.

## 13. Defects found by the finalisation tests (fixed 2026-10-06)

* **`cobol\online\fin\FXREVAL.cob`** passed the eight digit rate date to `FXCONV` through `WS-IDX`,
  a `PIC S9(4) COMP-5` halfword, so `20261031` was silently truncated and **no held rate was ever
  found** - every CARRY/REVALUE/REALISE without an explicit rate was refused with `FXR0002`. A
  dedicated `WS-RATE-DATE PIC 9(8)` now carries it. *Modernisation:* a strongly typed date type,
  or at least a compiler warning on narrowing MOVEs, would have caught this at build time.
* **`cobol\online\fin\VATRET9.cob` `2000-DRILL`** bound only the first nine columns of
  `fin.usp_VatBoxDrill`, which returns fifteen. The party, posting date, mechanism and direction
  columns sit between the ones the program reports, so the drill-down read the mechanism as the
  VAT amount and showed every money column as zero. All thirteen reported columns are now bound.
  *Modernisation:* select the columns by name into a generated record rather than by position.
* **`VATRET9` `3000-FINALISE`** used raw `BEGN`/`COMT`, which ended the *caller's* unit of work as
  well; it now uses the `@@TRANCOUNT` guarded `9500`/`9510`/`9520` paragraphs, as `BANKREC` and
  `JNLENTRY` do. *Modernisation:* one transaction manager instead of per-program conventions.
* **`VATRET9` `3000-FINALISE`** also re-computed the boxes through `1000-PREPARE`, which demanded
  `FIN.VAT.PREPARE` from the approver - impossible under `SOD-VAT`. The internal recompute now
  sets `WS-INTERNAL` and skips the second authority check. *Modernisation:* separate the
  calculation from the user-facing transaction so that authority is checked exactly once.

## 14. Known gap: `REVALRUN` has no multi-position run

`cobol\batch\fin\REVALRUN.cob` calls `FXREVAL REVALUE` once with no `FR-CURRENCY` and then reads
`FR-LINE-CNT` / `FR-L-*` as if the service had revalued every foreign position. `FXREVAL` is a
single-position service and never fills those lines, so the run ends `STERP-E-1001 A foreign
currency is required for a revaluation` with return code 8 (the spool file is still produced).
See the `fin-revalrun-positions` sub-todo. *Modernisation:* the scan belongs in a set based query
over the open foreign balances, with the service called once per position.
