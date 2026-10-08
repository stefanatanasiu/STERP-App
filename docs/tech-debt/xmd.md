# XMD tech debt - deliberate legacy texture

One bullet per item: file, pattern, why it is debt, modernisation hint.

## COBOL

* **`cobol\copy\XMDBATPR.cpy` - a copybook of paragraphs pasted into every program.**
  Job logging, spool paths, page headings, fetch widths and exception raising are shared by
  `COPY`ing procedure-division text, so the same object code is duplicated in six executables and a
  change to the suite means rebuilding all of them. It is exactly how a 1993 batch suite was held
  together. *Modernise:* make the housekeeping a called subprogram (or a single batch runner that
  hosts the report programs) so there is one copy of it at run time.

* **`RECONCTL.cob` - the control families are a `VALUE`d table in working storage.**
  `WS-CTL-TABLE` hard-codes eight families, their descriptions, their sign and their owning role,
  and `2100-SUBLEDGER` has one `WHEN` per family with the SQL built by `STRING`. Adding a ninth
  control family is a code change, not a data change. *Modernise:* drive the families from a
  `rpt.ControlFamily` table holding the subsidiary query, sign and owner role.

* **`RECONCTL.cob` - `OCCURS 8` with no overflow handling.**
  `WS-GL-TABLE` is fixed at eight entries because the chart of accounts has eight `ControlType`
  values. A new control type in `fin.Account` is silently ignored rather than reported.
  *Modernise:* read the distinct control types and refuse the run if any is unmapped.

* **`OBLIGREG.cob` - `OCCURS 40` obligations and `XB-J > 5` on the VAT loop.**
  The register builds at most forty derived obligations and takes only the five most recent VAT
  periods. A company with a long VAT history quietly loses the older rows. *Modernise:* page the
  register from the database instead of assembling it in working storage.

* **`OBLIGREG.cob` - the confirmation-statement review period is approximated by the accounting
  reference date.** The real review period runs from incorporation (or the last statement), which
  STERP does not hold. The derived date is therefore right in shape and wrong in detail for most
  companies. *Modernise:* add `core.Company.ConfirmationReviewDate` and derive from it.

* **`OBLIGREG.cob` / `core.StatutoryObligation` vs `fin.StatutoryObligation` - two registers.**
  The database carries a generic core register and a finance-specific one with overlapping meaning.
  `OBLIGREG` uses the core table only. *Modernise:* pick one, migrate the other, and put a view
  over the loser for compatibility.

* **`EXCPAGE.cob` - the approval SLA of five days is a literal inside the SQL text.**
  Everything else in the suite comes from `core.Parameter` through `PARMGET`; this one number did
  not, because it was added late in the 2026 pass. *Modernise:* `XMD.APPROVAL.SLA.DAYS`.

* **`KPIOTIF.cob` - supplier receipts are matched to a schedule by order number only.**
  `po.ReceiptLine` has no schedule number, so a multi-schedule purchase line takes the latest
  receipt date on the order. The measure is therefore pessimistic for staged deliveries.
  *Modernise:* carry `ScheduleNo` on `po.ReceiptLine` and match exactly, as the sales side does.

* **`KPIOTIF.cob` - `1250-EARLY-LIMIT` calls `DATECALC` once per schedule row.**
  Five working days back is recomputed for every row rather than cached per promise date. The
  calendar is cached inside `DATECALC`, so it is cheap, but it is still a call per row.
  *Modernise:* precompute the tolerance window per distinct date.

* **`CONVLOAD.cob` - `0150-BASENAME` scans a 200-byte field backwards from the end.**
  The 1993 path-splitting routine has never been touched and still assumes `\` or `/` and a single
  dot. *Modernise:* use a path helper in `STRUTIL`.

* **`CONVLOAD.cob` - accepted AR, AP and stock records are staged but not posted.**
  Only the opening trial balance reaches a ledger table (`fin.OpeningBalance`). Open items and stock
  balances are validated, counted and written to `stg.ImportRow` for the owning module's own opening
  load (`OPENSTK` for stock) to pick up. A reviewer could reasonably expect COMMIT to create the
  open items. *Modernise:* call the module posting services from the output procedure under one
  `BEGN`/`COMT`, or hand the staged batch to each module's loader.

* **`CONVLOAD.cob` - `7200-CHECK-NUM` only inspects the first twenty characters.**
  Amount fields are fifteen characters, so the limit is harmless today and wrong in principle.
  *Modernise:* validate the whole trimmed field.

* **`CONVLOAD.cob` - the SQL duplicate test for `STKB` is skipped.**
  `2600-LEDGER-DUP` has no `WHEN "STKB"`, so an opening stock balance that already exists in
  `inv.StockBalance` is only caught by the in-file test. *Modernise:* add the ledger probe.

* **`CHAINTRC.cob` - `OCCURS 24` steps with a truncation flag instead of paging.**
  A long chain is cut off and `CT-TRUNCATED` is set with return code 04. The client cannot ask for
  the next page. *Modernise:* a cursor-style COMMAREA (`CT-FROM-STEP`) or a stored procedure
  returning a result set.

* **`CHAINTRC.cob` - the financial half of the chain is found by matching `fin.Journal.SourceRef`
  to the document reference as text.** There is no foreign key from a journal back to the document
  that caused it, so a module that writes a differently formatted `SourceRef` silently drops out of
  the trace. *Modernise:* a `fin.JournalSource` link table, or enforce the reference format in
  `fin.usp_PostJournal`.

* **Every report - Report Writer `CONTROL HEADING FINAL` and `SUM` are avoided.**
  This GnuCOBOL build does not emit `CONTROL HEADING FINAL` and accumulates `SUM` on every
  `GENERATE`, so the scope block of each report is an ordinary `TYPE IS DETAIL` group that the
  program `GENERATE`s itself and every total is accumulated by hand in working storage (the same
  workaround as `bom`, `cst` and `kit`). *Modernise:* a data-driven renderer; the column positions
  are hard-coded literals in the `RD` as well.

* **Every report - 132-column line-printer layout with literal `COLUMN n` positions.**
  Changing a column width means re-counting the whole line by hand. *Modernise:* describe the
  layout as data and render it.

* **Sign conventions - `WS-CTL-SIGN` flips liability controls rather than the ledger.**
  The report reads naturally (a positive creditor balance) but the number printed under "GL balance"
  is not the signed ledger balance. The heading says `Subsidiary` and `GL balance`, not `Dr/Cr`.
  *Modernise:* print both sides as debit/credit columns and drop the sign table.

## SQL and seed

* **`db\seed\97-xmd-reports.sql` - the RPT catalogue duplicates the owning module's report row.**
  `RPT-001` and `AGEDDEBT` are two rows in `core.ReportDef` pointing at the same program, because
  the module seeds were written before the catalogue and must not be edited by this module.
  *Modernise:* one row per program with a many-to-many `core.ReportRequirement` table.

* **`db\seed\97-xmd-reports.sql` - `RPT-004`, `RPT-007` and `RPT-017` point at the nearest existing
  program.** RPT-004 maps to `CASHBOOK` and RPT-007/RPT-017 to `MTDJSON`; neither is the full
  report the specification describes (a scenario-based cash forecast; a complete boxes 1-9 audit
  trail with the C79/postponed-import reconciliation). *Modernise:* build `CASHFCST` and `VATAUDIT`
  in `cobol\batch\xmd` and repoint the catalogue - tracked as sub-todos `xmd-ctl-rpt004-cashfcst`
  and `xmd-ctl-rpt017-vataudit`.

* **No `db\procs\rpt` or `db\views\rpt` objects.** The reconciliation and ageing logic lives only in
  COBOL, so a BI tool cannot reproduce it. *Modernise:* publish `rpt.vw_ControlBalance` and
  `rpt.usp_ExceptionAgeing` with the same definitions and test the two against each other.

## Data

* **`data\legacy\*.DAT` - deliberately imperfect extracts.** Each file carries a duplicate, an
  unknown master reference and a malformed field so that the preview listing has something to show.
  They are fixtures, not production data, and `TXMDCONV` depends on the exact counts in
  `CALAROPN.DAT`. *Modernise:* generate the fixtures from a script so the expected counts and the
  file cannot drift apart.

## Build

* **`build\build-cobol.ps1` - fixed while building this module (reported to the foundation).**
  `$objs = Get-ChildItem ... | ForEach-Object {...}` yields a *string* when a module has exactly one
  online source file, and `@objs` then splats it character-wise, so `ar` produced an empty archive
  and every batch executable failed to link. Wrapped in `@(...)` to force an array. Any future
  module whose first online program is its only one would have hit the same wall.
