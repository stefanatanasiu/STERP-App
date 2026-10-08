# Technical debt — module `itm`

Deliberate legacy texture and genuine compromises in Item and Part Maintenance. Everything here
is **correct but dated**, or correct-but-duplicated; nothing in this list is a known defect.
One bullet per item: file, pattern, why it is like that, how to modernise.

Stock Control is the 1996 subsystem of the suite. `cobol\DIALECTS.cfg` pins it to **RM/COBOL**
(`itm=rm`), which is the single biggest source of the texture below.

See `docs\modules\itm.md` for what the module does.

---

## 1. Dialect and source format

* **Every file under `cobol\online\itm\` and `cobol\batch\itm\` — fixed format, 72-column
  source.** RM/COBOL reads columns 8–72 and ignores 73–80 (the old card sequence area), so each
  statement is written to fit and long literals are assembled with `STRING … DELIMITED BY SIZE`
  into `CA-MESSAGE` rather than written as one literal. `AVAILQ` and `UOMCONV` build almost every
  user message that way. *Why it is debt:* a message that reads as one sentence is spread over
  four or five statements, so changing its wording is a multi-line edit and the compiler cannot
  check the result. *Modernisation:* move to free format (`>>SOURCE FORMAT IS FREE`, as modules
  `bom` and `kit` already do) and put user-facing text in a message table keyed by reason code,
  which would also make the text translatable.

* **`cobol\online\itm\*.cob` — `SECTION`-per-paragraph with `GO TO <section>-EXIT`.** The
  eligibility ladder in `AVAILQ` (`1300-ELIGIBILITY`) and the factor resolution in `UOMCONV`
  (`3000-RESOLVE-FACTOR`) are written as a sequence of `IF … GO TO …-EXIT` tests because RM/COBOL
  of the period had no in-line `EXIT PARAGRAPH`/`EXIT PERFORM` and structured programming standards
  of 1996 preferred a single exit label. *Why it is debt:* the first-match-wins ordering of the
  eligibility tests is implicit in the statement order and is not expressed anywhere the compiler
  can check. *Modernisation:* an `EVALUATE TRUE` with one `WHEN` per exclusion reason, or a decision
  table driven from `inv.Location` / `inv.Item` metadata.

* **`cobol\online\itm\UOMCONV.cob`, `AVAILQ.cob` — nine- and six-decimal fixed-point arithmetic
  with explicit `ROUNDED` on every `COMPUTE`.** Factors are `S9(9)V9(9)` and quantities
  `S9(13)V9(6)` (D-09). There is no decimal-float type in the dialect, so precision is a property
  of each `PIC` clause rather than of the calculation. *Why it is debt:* adding a step to a
  calculation can silently change the rounding, because the intermediate precision depends on the
  receiving field. *Modernisation:* nothing to do inside COBOL; if the arithmetic ever moves, use
  SQL `decimal(19,9)`/`decimal(19,6)` with the same banker's-free `ROUND(…, n)` and keep the same
  field widths so the numbers stay reproducible.

## 2. Report Writer and hard-coded column layouts

* **`cobol\batch\itm\STKSTAT.cob`, `COUNTADJ.cob` — COBOL Report Writer (`REPORT SECTION`, `RD`,
  `TYPE IS PAGE HEADING` / `DETAIL` / `CONTROL FOOTING FINAL`, `GENERATE`).** The stock status
  report and the count sheets are 132-column line-printer layouts with the column positions
  written into the `RD` as literal `COLUMN` numbers. *Why it is debt:* Report Writer is an optional
  (and in several modern compilers unsupported) module, the width is baked in at 132 characters,
  and adding a column means re-planning every `COLUMN` on the line. The reports also assume a
  monospaced device and emit form-feeds. *Modernisation:* keep the `RD` as the specification of the
  report, but emit a delimited or fixed-width data file and let a reporting layer lay it out;
  `core.ReportDef` already carries the metadata needed to drive that.

* **`cobol\batch\itm\STKSTAT.cob` — headings and totals are positioned by hand to match the
  spool paper.** Truncation of a 40-character item code into a 24-character report column is done
  by the picture clause. *Why it is debt:* an item code can be displayed truncated on a report even
  though the system refuses to *identify* anything by truncation (D-21), which is a confusing
  mixed message. *Modernisation:* wrap the code onto a continuation line rather than truncating,
  or print the code on its own line above the detail.

## 3. COMMAREA-based call interfaces

* **`cobol\copy\UOMCONVCA.cpy`, `AVAILQCA.cpy`, `STKMOVECA.cpy`, `LOTSERCA.cpy`, `STKRESVCA.cpy`,
  `SUBSTCHKCA.cpy`, `ITEMCOPYCA.cpy`, `ITMIMPCTCA.cpy`, `ITMCOMPLCA.cpy`, `ITEMENQCA.cpy`,
  `ITEMVALCA.cpy` — one flat, fixed-length record carries both the request and the reply.** This is
  the CICS `DFHCOMMAREA` convention the suite was built on in 1991 and never left: a caller fills
  the input fields, `CALL`s, and reads the output fields out of the *same* storage. *Why it is
  debt:* input and output are indistinguishable at compile time (`UC-FROM-UOM`, `UC-TO-UOM` and
  `UC-STOCK-QTY` are genuinely in/out, and `UOMCONV FROMSTK` and `UNITCOST` **overwrite
  `UC-FROM-UOM` in place** while resolving the factor), so a caller that re-uses its COMMAREA for
  a second call without re-initialising it can get a silently wrong answer. *Modernisation:*
  separate request and reply records, or at minimum a `CLEARCA` entry point; failing that, keep
  the discipline that every caller does `INITIALIZE` before each call.

* **Fixed `OCCURS` tables in the COMMAREAs: `UC-LINES OCCURS 20` (`UOMCONVCA.cpy`),
  `AV-LINES OCCURS 40` (`AVAILQCA.cpy`).** The answer is truncated when more rows exist, and
  `AVAILQ` reports that with `CA-RETURN-CODE 04` / `AVL0003`. *Why it is debt:* the limit is a
  property of the copybook, not of the business, so a large site can get a complete *summary* with
  an incomplete *detail* and the caller must remember to look at `AVL0003`. *Modernisation:* page
  the detail (pass a "from" key and return a "more" flag), or return the lines as a result set
  from `inv.usp_StockEnquiry` and leave only the summary in the COMMAREA.

* **`cobol\copy\AVAILQCA.cpy` — `AV-DEMAND-LINE PIC S9(9) COMP-5` in an otherwise `DISPLAY`
  record.** One binary field in a character record, because the demand line number came from a
  caller that held it as a 32-bit integer. *Why it is debt:* the record is no longer
  byte-for-byte printable or safely transferable as text, and its length depends on the compiler's
  binary alignment. *Modernisation:* make it `PIC 9(9)` display like every other numeric field in
  the record and convert in the caller.

* **`cobol\online\itm\ITMAUDIT.cob`, `ITMDATE.cob` — wrapper programs that exist only to avoid a
  copybook clash.** Both are thin shells over the common services `AUDITLOG` and `DATECALC`;
  their stated reason is that a stock program must never have two standard COMMAREA headers
  (`STCAHDR`) in scope at once. *Why it is debt:* two extra programs, two extra `CALL`s and two
  extra places for a field to be forgotten, all caused by a copybook that cannot be instantiated
  twice. *Modernisation:* `COPY STCAHDR REPLACING` with a per-use prefix (the suite already uses
  `COPY <X>CA REPLACING ==DFHCOMMAREA== BY ==WS-…==`), which removes the need for the wrappers.

## 4. Embedded SQL via SQLIO result sets

* **`cobol\online\itm\AVAILQ.cob`, `UOMCONV.cob`, `ITEMVAL.cob`, `ITEMENQ.cob`, `ITMIMPCT.cob` —
  SQL is built as text into `SQLIO-STMT` and read back through an untyped column array.** There is
  no precompiler and no host-variable binding: the program sets `SQLIO-COL-WIDTH(n)`, calls
  `SQLIO`, then converts each column with `FUNCTION NUMVAL(ITM-COL(n))` (for example
  `COMPUTE AV-QTY-RESERVED = FUNCTION NUMVAL(ITM-COL(1))` and
  `COMPUTE UC-QUANTUM = FUNCTION NUMVAL(ITM-COL(2))`). *Why it is debt:* column order is a
  positional contract between the COBOL and a `SELECT` written in the same file, so inserting a
  column into the query silently shifts every value; types are checked at run time, not compile
  time; and numeric text must round-trip through `NUMVAL`, which loses nothing today only because
  the widths were chosen to match. *Modernisation:* call stored procedures with named result
  columns and generate the COBOL column map from the procedure definition, or adopt an
  `EXEC SQL` precompiler so the binding is checked.

* **Statement text is assembled with `STRING … DELIMITED BY SIZE` across several lines
  (72-column limit again).** *Why it is debt:* a `WHERE` clause is spread over many statements and
  is hard to review; parameter values are concatenated rather than bound. *Modernisation:* move
  every statement into `db\procs\inv` and leave only the procedure name and parameters in COBOL –
  which also removes the concatenation entirely.

## 5. Working-storage reuse

* **`ITM-COL` / `SQLIO` buffer reused for every query inside a program.** One column array serves
  the item read, the factor read, the reservation read and the expected-supply read in the same
  program, re-initialised between calls. *Why it is debt:* the meaning of `ITM-COL(1)` depends on
  which paragraph last ran, so a mis-ordered `PERFORM` produces a plausible wrong number instead
  of a failure. *Modernisation:* one named group item per query, filled by a small paragraph that
  owns that query.

* **`cobol\online\itm\UOMCONV.cob` (`2000-FROM-STOCK`, `6000-UNIT-COST`) — the COMMAREA field
  `UC-FROM-UOM` is used as a scratch variable.** `FROMSTK` moves `UC-TO-UOM` into `UC-FROM-UOM`,
  resolves the factor, then moves the saved value back; `UNITCOST` overwrites it with
  `UC-PRICE-UOM`. *Why it is debt:* the caller's input field is modified during the call, which is
  invisible from the copybook. *Modernisation:* resolve the factor through a working-storage
  parameter instead of the COMMAREA field (documented in `docs\modules\itm.md` §1.1 in the
  meantime).

## 6. Item code format rules

* **`db\tables\20-inv\02-Item.sql` — `CONSTRAINT CK_Item_Code CHECK (ItemCode NOT LIKE
  '%[^A-Z0-9./_-]%')`.** The permitted set (`A-Z 0-9 . / _ -`) is the 1996 card-punch character
  set, chosen so a code can be printed, bar-coded and keyed on any device. *Why it is debt:*
  (a) under the database's default **case-insensitive** collation the range `A-Z` also matches
  `a-z`, so a lower-case code is accepted by the constraint even though every program and report
  assumes upper case; (b) the rule is expressed as a negated `LIKE` pattern, which is unreadable
  and cannot report *which* character was wrong; (c) `varchar(40)` is generous enough that codes
  routinely exceed the 24-character report columns in `STKSTAT`. *Modernisation:* add an explicit
  `COLLATE Latin1_General_BIN2` (or an upper-casing `INSTEAD OF` trigger) so case is enforced
  rather than assumed, and move the validation message into `ITEMVAL VALIDATE` so the user is told
  which character is not allowed.

* **`inv.Item.Category` / `ProductGroup` are free-text `varchar(20)` columns with no master
  table.** Analysis codes were a reporting convenience in 1996 and never became reference data.
  *Why it is debt:* "a category that is in use cannot be deleted" cannot be enforced by a foreign
  key; it is enforced only by the maintenance screen and by `ITMIMPCT DELCHECK` reading
  `inv.usp_ItemImpact`, and a typo creates a new category silently. *Modernisation:* add
  `inv.ItemCategory` with a foreign key from `inv.Item`, seed it from the distinct values in use,
  and let the existing generic maintenance metadata (`core.MaintDef`) drive the screen.

## 7. The same rule in two places

* **Quantum and indivisibility are enforced in `UOMCONV` (`5000-CHECK-QUANTUM`, reason `UOM0006`)
  *and* in `inv.usp_PostStockMovement` (`STERP-E-0028`).** This is deliberate — COBOL owns the
  message the user sees and the suggested rounded quantity, SQL owns the guarantee for anything
  that reaches the table by another route — but the tolerance (`0.000001` in SQL, `0.0000005` in
  COBOL) and the rounding direction are written twice. *Modernisation:* keep both checks, but
  derive the tolerance from one parameter (`core.Parameter`) so the two cannot drift.

* **Availability eligibility is implemented in `AVAILQ` (`1300-ELIGIBILITY`) *and* in
  `inv.usp_StockEnquiry` (the `IsEligible` / `NotEligibleReason` `CASE`) *and* in
  `inv.usp_ReserveStock` (the `@eligible` `SELECT`).** Three copies of one definition, with
  slightly different reason wording in each. *Why it is debt:* a change to the rule (a new location
  type, a new quality status) has to be made three times. *Modernisation:* keep `AVAILQ` as the
  single definition for the user-facing answer and have the two procedures call one inline
  table-valued function that returns the eligibility flag and reason.

## 8. Transaction handling in the stock procedures

* **`db\procs\inv\usp_ReserveStock.sql` — unconditional `BEGIN TRANSACTION` and
  `IF XACT_STATE() <> 0 ROLLBACK TRANSACTION` in the `CATCH`.** When the procedure is called from
  inside a caller's transaction and the reservation is refused, that `ROLLBACK` rolls back the
  **whole** transaction, not just the procedure's own work. `inv.usp_PostStockMovement` does the
  right thing (`IF @@TRANCOUNT = 0 BEGIN BEGIN TRANSACTION; SET @own = 1; END` and
  `IF @own = 1 AND XACT_STATE() <> 0 ROLLBACK TRANSACTION`). *Why it is debt:* a caller that wants
  to try a reservation and continue on refusal cannot do so, and `db\tests\itm-stock.sql` has to
  open a fresh transaction per assertion and tolerate `@@TRANCOUNT = 0` in its `CATCH` blocks.
  *Modernisation:* adopt the `@own` / savepoint pattern used by `usp_PostStockMovement`.

* **`db\procs\inv\usp_ReserveStock.sql` — the result set reports the free quantity measured on
  entry.** A `RELEASE` therefore returns `QtyFreeBefore` and `QtyFreeAfter` from *before* the
  release took effect; the stock only appears as free to the next caller. *Why it is debt:* the
  column names imply an after-state that is not recomputed. *Modernisation:* re-read the free
  quantity after the mutation, or rename the columns to say what they measure.
