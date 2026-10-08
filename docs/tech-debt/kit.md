# Kit module – intentional technical debt

Legacy texture that is deliberate, correct and documented. Each entry: file, pattern, why it is
debt, and how a modernisation would remove it.

## COBOL

* **`cobol\copy\KITWORK.cpy` – one flat 01 record shared by nine programs.**
  Every kit program copies the same `KIT-HEADER-REC` / `KIT-LINE-REC` into working storage and
  passes it to `KITDB` by position. A change of column means recompiling the whole module.
  *Why it is there:* this is exactly how a 1990s copybook-driven suite was built, and it keeps the
  programs small. *Modernisation:* a typed data-access layer with a mapped entity, or at minimum
  a generated copybook with a version stamp checked at start-up.

* **`cobol\online\kit\KITDB.cob` – SQL text assembled with `STRING ... DELIMITED BY SIZE`.**
  The statement text is built at run time (parameters are always `?` markers – no user data is
  ever concatenated, so there is no injection risk). It is still hard to read and impossible to
  validate at compile time. *Modernisation:* stored procedures or an ORM; the statements are
  already single-purpose, so they map almost one-for-one.

* **`cobol\online\kit\KITDB.cob` – cursors 5 and 6 reserved by convention, not by code.**
  A caller that happens to use cursor 5 will corrupt a `LINOPEN`/`LINNEXT` loop. The rule lives
  in a comment. *Modernisation:* a cursor allocator, or a result-set object per call.

* **`cobol\online\kit\KITDB.cob` – fixed-width text row areas (`Ws-Hdr-Row`, `Ws-Lin-Row`).**
  Column widths are repeated in two places: the `CONVERT(varchar(n), ...)` in the select list and
  the `SQLIO-COL-WIDTH` table. They must be kept in step by hand. *Modernisation:* describe the
  result set once and generate both.

* **`cobol\online\kit\KITCREAT.cob` – the per-unit usage is not stored, it is re-derived.**
  `KITAMEND SCALE` recovers usage-per-unit as `OriginalRequirementQty / PlannedGoodQty` instead of
  keeping the bill's usage on the kit line. It is correct for the original output but it quietly
  assumes the original planned quantity never changed. *Modernisation:* add `UsagePerUnit` and
  `UsageBasisQty` columns to `kit.ExecutionKitLine` and store what the bill said.

* **`cobol\online\kit\KITCREAT.cob` – non-material bill lines filtered by literal node type.**
  `"NOTE"`, `"RESOURCE"` and `"BYPROD"` are compared as literals against `BX-L-NODE-TYPE`. A new
  node type in the BOM module silently becomes a material requirement. *Modernisation:* ask
  `BOMEXPL` for a boolean "is material" flag rather than inferring it from a code.

* **`cobol\online\kit\KITISSUE.cob` – the backflush check pass is a second full pass.**
  To honour "cannot partially succeed" the program walks every open line in `CHECK` mode and then
  walks them again to post. Between the two passes another user could take the stock, in which
  case the second pass fails part-way and the caller gets `08` with `KS-LINES-DONE > 0`.
  *Why it is there:* COBOL owns the transaction (`BEGN`/`COMT` is the caller's job), and a single
  pass could not be rolled back cleanly from inside the loop. *Modernisation:* make the whole
  backflush one stored procedure under a single transaction, or hold `UPDLOCK` across the check.

* **`cobol\online\kit\KITISSUE.cob` – reservation consumption is arithmetic on the kit line.**
  *Resolved 2026-10-06 (CR2510).* `KITISSUE Consume-The-Reservation` now calls
  `STKRESV CONSUME` and reduces `ReservedQty` by what the service says it consumed; the old
  subtraction survives only as the fallback when the service has no reservation to consume
  (an issue may legitimately exceed a reservation). *Remaining debt:* the fallback is silent.

* **`cobol\online\kit\KITTRIAL.cob` – `MAXBUILD` re-explodes the bill up to twenty times.**
  The maximum buildable output is found by probing candidate quantities and calling `BOMEXPL`
  again for each one, because yield, fixed-per-run material, batch steps and rounding make the
  answer non-linear. It is correct and it is slow: a deep bill costs twenty explosions.
  *Modernisation:* a set-based `usp_TrialAllocate` that evaluates the candidates in one pass.

* **`cobol\online\kit\KITTRIAL.cob` / `MULTITRL.cob` – the answer lives in WORKING-STORAGE.**
  `GETLINE` / `GETKIT` / `GETCOMP` page a result that is kept between `CALL`s inside the DLL, so
  two users of the same loaded module share one scenario. It is why a trial is cheap to page and
  why `CLEAR` exists. *Modernisation:* a scenario id and a result table.

* **`cobol\online\kit\MULTITRL.cob` – selection sort over `OCCURS 12`.**
  Priority sequencing is an O(n²) selection sort re-run on every `ADDKIT`. Harmless at twelve
  kits, embarrassing at twelve hundred. *Modernisation:* order in SQL.

* **`cobol\online\kit\KITCOMPL.cob` – the stock receipt and the ledger posting are not one
  transaction.** COBOL does not own the transaction (`BEGN`/`COMT` is the caller's job), so the
  parent is received, the completion row is written and only then is `fin.usp_PostJournal`
  called. If the posting fails the receipt stands and an `ERROR` exception is raised for somebody
  to repair. *Why it is there:* the same reason `KITISSUE` has two passes. *Modernisation:* one
  stored procedure under a single transaction, or a compensating reversal.

* **`cobol\online\kit\KITCOMPL.cob` – genealogy is attributed by proportion.**
  *Mostly resolved 2026-10-06 (CR2512).* `Write-The-Genealogy` now draws from the actual issues in
  the order they were made, taking only what earlier completions left, and picks up the lot or
  serial the storeman recorded in `kit.KitPick` when the issue itself carried none. The first six
  frames therefore contain the first lot picked and the last four the second (AC-083), and a final
  completion sweeps up whatever is left so that nothing issued stays unattributed.
  *Remaining debt:* issued material that is not attached to a kit line (`LineNum` null) cannot be
  sequenced and still claims `goodQty / plannedQty` of every such issue, and the operator is never
  asked which physical unit received which lot - the order of issue is assumed to be the order of
  consumption. *Modernisation:* capture the consumed lot at the point of assembly.

* **`cobol\online\kit\KITRESV.cob` – future supply is committed on paper, not in `inv`.**
  `SUPPLY` writes a `kit.KitSupplyCommit` row and the line's outstanding requirement falls by it,
  but nothing pins the named purchase order or works order at the other end: the inbound document
  does not know it has been spoken for, so two kits can both commit to the same PO line and only
  a report would notice. `SUPSUM` is also called once per line inside the `RESERVE` loop, which is
  a round trip per line. *Why it is there:* `STKRESV` has no supply-order reservation and changing
  its COMMAREA is the Items team's call. *Modernisation:* an `inv`/`po` service that reserves
  against a supply order line, with the commitment consumed when the receipt is booked.

* **`cobol\online\kit\KITCOMPL.cob` – the ledger call is a hand-built `EXEC` string.**
  `Call-Post-Journal` concatenates the call to `fin.usp_PostJournal`, names the series code
  (`JNL`) and the journal type (`KIT`) as literals, and asks for `@SuppressResult = 1` with a
  read-back `SELECT` of its own because the procedure's own result set does not survive the
  SQLIO call interface. Three separate traps live in those few lines, all of which produced a
  completion in stock with no ledger entry until CR2514: a procedure argument may not be an
  expression (`CAST(? AS date)` is a syntax error - `cobol\online\ap\APPOST.cob` still has it);
  a correlation id may only be spent once, so the id used for the stock receipt cannot be reused
  for the journal; and a staged line must carry a real account, because the procedure validates
  accounts and does not resolve them from the event map. *Modernisation:* one posting service
  call with named parameters, or a `kit` stored procedure that stages and posts in one place.

* **`cobol\online\kit\KITCLOSE.cob` – status history is read back out of `core.AuditLog`.**
  There is no `kit.KitStatusHistory` table; `HISTORY` selects the audit rows for
  `EntityType = 'KIT.EXECUTIONKIT'`. It cannot drift from what was recorded, but it also cannot be
  indexed or constrained as history. *Modernisation:* a first-class status history table.

* **`cobol\online\kit\KITRET.cob` – `RETURNCK` runs `LISTISS` to find the returnable quantity.**
  Checking a single return therefore reads up to fifteen issue rows and recomputes every open
  quantity. *Modernisation:* a scalar function or a persisted `ReturnedQty` on `kit.KitIssue`.

* **Magic numbers.** `OCCURS 20` windows in every COMMAREA, `OCCURS 12`/`OCCURS 15` in
  `MULTITRLCA`/`DISASSYCA`, tolerance `0.000001` in `KITQTY`, the trial's `OCCURS 150` result and
  `OCCURS 80` availability cache, `MULTITRL`'s `OCCURS 60` pool. Hard limits that a client must
  discover from the copybook. *Modernisation:* variable-length result sets.

* **`cobol\copy\*CA.cpy` – prefix-per-program field naming (`KC-`, `KQ-`, `KS-`, `KA-`, `KX-`,
  `KP-`, `KM-`, `KZ-`, `KK-`, `KV-`, `KR-`, `KT-`, `MT-`, `GN-`, `DS-`).**
  Cryptic two-letter prefixes exist only so that a caller can `COPY ... REPLACING` several
  COMMAREAs into one program without a name clash. *Modernisation:* namespaced structures.

* **Duplicated rule.** `KITISSUE Update-Line-Quantities` recomputes net/remaining/excess through
  `KITQTY` *and* `KITCREAT Fill-Window-Line` recomputes them again on every read. The stored
  columns are therefore written by one program and ignored by another. *Why it is there:* a 1996
  defect where a screen showed a stale outstanding quantity. *Modernisation:* make them computed
  columns in SQL Server and delete both code paths.

## Database

* **`kit.ExecutionKitLine` stores `NetIssuedQty`, `RemainingQty` and `ExcessQty`** even though all
  three are derivable from `CurrentRequirementQty`, `GrossIssuedQty` and `ReturnedQty`. They exist
  so that a report can be written without the arithmetic. *Modernisation:* persisted computed
  columns.

* **`kit.KitIssue` carries both `Quantity`/`UomCode` and `StockQty`** with the conversion factor
  only implied. A later packaging change cannot rewrite history, which is the point, but the
  factor itself is not kept on the kit issue row (it is on the stock movement).
  *Modernisation:* copy `SM-FACTOR` onto `kit.KitIssue`.

## Process

* **Program names truncated to 8 characters** (`KITCREAT` for `KITCREATE`, `MULTITRL` for
  `MULTITRIAL`) because of the `PROGRAM-ID` rule in CONVENTIONS §3.1. Other teams' briefs use the
  long names, so `docs\modules\kit.md` carries the mapping. *Modernisation:* an alias table, or
  longer program ids once the CICS heritage is gone.

## Report Writer: `CONTROL HEADING FINAL` is never emitted

**Files:** `cobol\batch\kit\KITRDY.cob` (`CH-SCOPE`), `cobol\batch\kit\KITDOCP.cob` (`DT-HEAD`)

The reports declare `CONTROLS ARE FINAL` and a `TYPE IS CONTROL HEADING FINAL` group for the block
that describes the scope of the listing. This compiler never generates it - the block silently
prints as blank lines, which is how `KITRDY` shipped. `KITDOCP` cannot live with that, because a
picking list without its kit header is a worse document than no document at all, so its header block
is an ordinary `TYPE IS DETAIL` that the program `GENERATE`s itself before the first material line.

*Why it is here:* it is exactly the kind of quiet, compiler-specific Report Writer behaviour that a
1990s batch suite accumulates, and the two programs now disagree about how to solve it.

*Modernisation hint:* when the reports move to a reporting service, drop Report Writer control
headings altogether and render the header from the same projection the screen uses.

## `kit.KitIssue` is a single ledger with a widening type list

**File:** `db\tables\90-kit\03-KitIssue.sql` (`CK_KitI_Type`)

Every economic event on a kit - material out, material back, scrap, backflush, recovery, absorbed
labour and overhead, subcontract, and the value transferred out on completion - is written to one
table and told apart by `IssueType`. The original CHECK constraint listed only the material
dispositions, so `KITCOST` could read `LABOUR`, `OVERHEAD`, `SUBCON` and `COMPLETE` but nothing
could ever write them: `AbsorbedGBP` and `CompletedGBP` were structurally zero and the residual
could never be relieved. The constraint has been widened in place (idempotently, so an existing
database is altered rather than rebuilt).

*Why it is here:* one wide movement table with a type column is the classic 1990s design, and the
CHECK constraint is the only thing documenting what the types mean.

*Modernisation hint:* split the dispositions from the conversion postings, or move the type list to
a reference table with its own accounting treatment.

## SQLIO has eight cursor slots and nobody arbitrates them

**Files:** `cobol\online\kit\KITCOST.cob`, `GENEALOG.cob`, `DISASSY.cob`, `KITCOMM.cob`

`c\sterpsql.c` fixes `MAXCUR` at 8 and `SQLIO` rejects anything outside 1..8 with
`SQLIO: cursor number must be 1-8`. Cursor numbers are picked by hand, per program, and the only
protection against two programs in one call stack fighting over a slot is a comment. `KITDB` holds
5 and 6, so the kit enquiry programs share 3, 4, 7 and 8. Four kit programs originally declared
9, 10, 11 and 12 and abended on their first query.

*Modernisation hint:* let the data access layer allocate and release cursor handles, so callers
never name one.

## The pick trail is a ledger, not a balance

**File:** `cobol\online\kit\KITDB.cob` (`PCKUSE`), `cobol\online\kit\KITISSUE.cob`

`kit.KitPick` keeps every pick, short pick and unpick as its own row, so "what is still off the
shelf" is a running sum rather than a column anybody can read. `PCKUSE` therefore has to settle
the unpick rows against the oldest open picks itself, inside one multi-statement T-SQL batch with
a windowed running total, before it can consume what the issue actually spent. The batch is the
only place that knows the rule, and it is written in SQL rather than COBOL because a row-by-row
COBOL loop over the trail would need a cursor the module has not got.

*Modernisation hint:* give the pick its own aggregate (picked / consumed / returned) and let the
domain service keep it, so FIFO consumption is a method rather than a statement.

## Two report runs in the same second overwrite each other

**File:** `cobol\batch\kit\KITRDY.cob` (and every other batch report through `RUNPATH`)

The spool name is `<PROG>-<CO>-<yyyymmddhhmmss>.lst`. Running `KITRDY MODE=EXEC` and
`KITRDY MODE=SHORT` back to back produces one file, because the second run finishes inside the
same second and silently replaces the first.

*Modernisation hint:* add the run number from `JOBLOG` (or a sequence) to the spool name.

## A kit trial asks BOMEXPL for leaves and can get none

**File:** `cobol\online\kit\KITTRIAL.cob` (`Explode-At-Probe-Qty`)

`BX-PURPOSE` used to be `PRODUCTION`, which no bill carries, so every trial against a real bill
came back `KIT0004` "bill not found". It now sends `STANDARD` (or `JOB`/`ORDER` when the kit
carries that demand), which is the contract in `docs\modules\bom.md`. The window was then empty,
because `BOMEXPL` only *computes* on `EXPLODE` and fills the window on a separate `GETLINE`; the
trial now explodes once and pages the answer with `GETLINE` (the callee advances `BX-FROM-LINE`
itself), and `KT-USE-SUBASSY = "Y"` asks for `BX-VIEW = "LEVEL1"` at full depth instead of
truncating at depth 1. The verdict and the component detail now agree; the two-call protocol is
the debt.

*Modernisation hint:* one explosion contract with an explicit "give me the rows you costed", not a
view filter and a paging protocol the caller has to guess.

## kit.ExecutionKit has no frozen-revision columns of its own

`KIT-004` says the bill and revision a kit was exploded from must not change under it. The header
carries `BomHeaderId` and `BomRevision`, but those are the **live** engineering references: nothing
in the schema distinguishes "the revision this kit was frozen at" from "the revision this bill is
on now", so a re-released bill leaves the kit pointing at the new one. `KITCREAT` freezes the
exploded requirement in `kit.ExecutionKitLine` and `KITDB HDRFRZ` records the assumptions, which is
why the engine behaves correctly today, but the guarantee rests on programme discipline rather than
on the data model. Adding `FrozenBomHeaderId` / `FrozenBomRevision` (and comparing them in
`BOM-031` impact work) would make it structural. Documentation only - no change was made.

## Only the completion reaches the ledger, and kit WIP is mapped to the NRV allowance

**Files:** `db\seed\85-kit-eventmap.sql`, `cobol\online\kit\KITISSUE.cob`, `KITRET.cob`,
`KITSCRAP.cob`, `KITCOMPL.cob`

`85-kit-eventmap.sql` maps six business events - `KIT.ISSUE` (store to WIP), `KIT.RETURN`,
`KIT.SCRAP`, `KIT.ABSORB`, `KIT.COMPLETE` (WIP to finished stock) and `KIT.DISASSY` - but only
`KITCOMPL` ever posts one (`KIT.COMPLETE`, Dr 1120 / Cr 1150). Issues, returns and scrap move stock
and, since CR2744, carry a value on `inv.StockMovement`, `kit.KitIssue` and the kit's
`WipValueGBP`, yet post nothing. A completion therefore credits work in progress with value that no
journal ever debited to it.

The map also names **1150** as the kit's work-in-progress account. In the chart of accounts
(`db\seed\08-fin-coa.sql`) 1150 is *"Stock allowance, net realisable value"*; the WIP control
account (`ControlType = 'WIP'`) is **1110**, which costing's `CST.WIPTRANSFER` already uses. Kit
WIP is thus invisible to the `RECONCTL` WIP family, and the one-sided completion credits land on
the NRV allowance. In the demonstration dataset `fin.usp_DemoBridgeControls` levels the control
families after the drivers run, so `RECONCTL` stays green - which hides the gap rather than
closing it.

*Modernisation hint:* post `KIT.ISSUE` / `KIT.RETURN` / `KIT.SCRAP` from the programs that move the
stock (the `KITCOMPL` `Stage-Journal-Lines` / `Call-Post-Journal` pattern, with a derived
correlation id per posting), map the kit WIP legs to 1110, and add a kit WIP assertion to
`db\tests\xmd-recon.sql`. Better still, raise the journal from the stock movement itself so no
caller can move valued stock without a ledger entry.
