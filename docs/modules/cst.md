# Costing and Profitability (`cst`)

Requirement family **CST-001..CST-026**, specification `07-Costing-and-Profitability.md`,
decisions **D-04** (valuation methods), **D-05** (overhead on normal capacity), **D-13** (two job
levels), **D-22** (rounding residuals).

COBOL dialect **RM/COBOL (`-std=rm`)**, fixed format, upper case, `SECTION`s with numbered
paragraphs — the 1997 costing suite. Schema **`cst`**.

> **Status:** the costing engine, the valuation programs, the job and WIP engine, landed-cost
> attribution, NRV testing, scenario costing, the stored procedures, the reference-data seed, the
> demo transactions, the four batch reports and the WinForms client are delivered and tested
> (111 COBOL assertions, 19 SQL assertions and 9 .NET harness assertions green). Outstanding:
> `CSTWIPRF` (the WIP roll-forward report, CST-018) and the two database-backed COBOL test
> programs `TCSTVAL`/`TCSTJOB` — see the `cst-*` rows in the session `todos` table.

---

## 1. Programs

All online programs are LINKable (`PROCEDURE DIVISION USING DFHCOMMAREA`) and live in
`cobol\online\cst`. Each has a COMMAREA copybook `<PROG>CA.cpy` in `cobol\copy`.

| Program | Functions | Purpose | REQ |
|---|---|---|---|
| `COSTNORM` | `CONTEXT` `NORMCOST` `PURCOST` `ELEMENTS` | States the enquiry basis and refuses to substitute another one; converts a quoted price to a sterling cost per stock unit; builds the acquisition cost of a receipt including duty and irrecoverable VAT; proves the elements add back to the total. | CST-001..004, 024 |
| `FIFOVAL` | `RECEIPT` `ISSUE` `RETURN` `QUERY` `LANDADJ` | FIFO layers in `cst.CostLayer`. Oldest eligible layer first, exact consumed quantities retained, partial consumption conserved. | CST-005, 009, 024 |
| `MWAVAL` | `RECEIPT` `ISSUE` `RETURN` `QUERY` `REVALUE` `OPENING` | Moving weighted average on `cst.ItemCost`, one `cst.ItemCostHistory` row per recalculation. Zero denominators and negative stock refused; residual on a zero balance reported, not hidden. | CST-006, 009, 024 |
| `STDCOST` | `GETSTD` `REVIEW` `APPROVE` `ACTIVATE` `IMPACT` `RECEIPT` `INVOICE` `RETURN` `COMPARE` | Cost-set lifecycle, activation revaluation, receipt/invoice/return variances, set-to-set comparison. | CST-007, 009, 012 |
| `LABOVH` | `RATES` `LABOUR` `OVERHEAD` `ABSORB` `OVHRATE` | Setup per run, run per gross unit, overhead per labour/machine hour or on a non-circular percentage base; under-recovery against normal capacity. | CST-011, 026 |
| `COSTROLL` | `ROLLUP` `GETLINE` `SAVESET` `EXPLAIN` | BOM cost roll-up. **Calls `BOMCOST`** (which drives `BOMEXPL`/`BOMNET`) — costing never explodes a bill itself. One valuation boundary per branch. | CST-002, 010, 024 |
| `MARGIN` | `EXPECTED` `REALISED` `LINE` `ORDER` `JOB` | Margin and markup on ex-VAT net revenue, expected versus realised, explicit `UNDEFINED` states. **Sales calls this.** | CST-013 |
| `JOBCOST` | `CREATE` `BUDGET` `APPRBUDG` `POSTCOST` `REVERSE` `ENQUIRE` `FORECAST` `COMPLETE` `CLOSE` `REOPEN` | Jobs and sub-jobs (two levels, D-13), original/revised budgets, cost transactions, estimate at completion, separate operational and financial closure. | CST-014..017, 020 |
| `WIPCALC` | `CALC` `POST` `ENQUIRE` `RESIDUAL` `WRITEOFF` | WIP roll-forward into `cst.WipBalance`; residual review before a job may be closed. | CST-018, 020 |
| `VARIANCE` | `SPLIT` `MATERIAL` `LABOUR` `OVERHEAD` `RECORD` `SUMMARY` | Quantity/rate split that reconciles to the total difference, with the baseline quantity disclosed. | CST-019 |
| `LANDED` | `ESTIMATE` `ACTUAL` `SPLIT` `ENQUIRE` | Attributes an acquisition charge over the receipt lines once, on a `VALUE`/`WEIGHT`/`VOLUME`/`QUANTITY` basis through `RNDALLOC`. On actualisation only the **difference** is spread, and it follows each line's current destination — stock, WIP or already sold. | CST-004, CST-008 |
| `NRVTEST` | `TEST` `SCAN` `APPROVE` `POST` `REVERSE` | FRS 102 lower of cost and NRV. The write-down is an allowance: the quantity on hand never changes. A reversal is capped at original cost (`NR-CAPPED`) and an explicit reversal above the earlier write-down is refused (`CST1105`). The approver may not be the preparer (`CST1108`). | CST-021, CST-025 |
| `WHATIF` | `SCENARIO` `COMPARE` `MARGINWI` | A base case and up to five variants priced through `COSTROLL` with `CR-TARGET-SET` cleared — `SAVESET` is never issued, so nothing is written, reserved or activated. `MARGINWI` puts each scenario unit cost against `WI-TARGET-PRICE` through `MARGIN`. | CST-023 |

`CA-FUNCTION` is `PIC X(8)` — every function name above is eight characters or fewer. That is why
budget approval is `APPRBUDG` and the overhead recovery rate is `OVHRATE`.

### Batch reports (`cobol\batch\cst`)

Report Writer, 132 columns, `COPY STRPTWS` + `COPY STRPTHDR`, arguments through `CMDARGS`, spool path
through `RUNPATH`, `core.JobRun` through `JOBLOG`, and `STERP-I-SPOOL <path>` as the last line so the
client's report launcher knows what to open. The argument names are exactly the `ParamSpec` of the
matching `core.ReportDef` row.

| Program | Report | Run as | Requirements |
|---|---|---|---|
| `CSTINVAL` | `CSTRP010` Inventory valuation. Control break on valuation group, NRV sub-line for any written-down pool, optional FIFO layer detail, and a closing **reconciliation to the general ledger stock control account at the same cut-off** from `cst.usp_ValuationRecon` — pool value, allowance, net, ledger balance, difference and a reconciled flag. An unreconciled run ends with return code 4 and `STERP-W-0402`. | `CSTINVAL CO=STC [ASAT=yyyymmdd] [SITE=] [GROUP=] [METHOD=MWA\|FIFO\|STD] [DETAIL=LAYER] [MODE=ALL\|EXCEPTION] [USER=]` | RPT-011, CST-001, CST-002, CST-021, CST-022, CST-026 |
| `CSTJOBPR` | `CSTRP020` Job profitability. `cst.usp_JobProfitability` per contract, sub-jobs indented under their parent and counted once (D-13), estimate at completion, variance against the approved budget, closing WIP. A job with no revenue prints `n/a`, not minus one hundred per cent. | `CSTJOBPR CO=STC [PERIOD=yyyynn] [JOB=] [STATUS=] [CUSTOMER=] [MODE=ALL\|OVERSPEND] [USER=]` | RPT-014, CST-014..CST-018, CST-020 |
| `CSTVARRP` | `CSTRP030` Variance analysis. Control break on variance type with the long name, the quantity and rate halves kept apart, adverse and favourable totalled separately, and every variance with no owner **and** no disposal flagged and counted. | `CSTVARRP CO=STC [PERIOD=yyyynn] [TYPE=] [JOB=] [ITEM=] [MODE=ALL\|UNDISPOSED] [USER=]` | CST-005, CST-006, CST-010, CST-011, CST-019, CST-024 |
| `CSTSETCM` | `CSTRP040` Cost set comparison. A `FULL JOIN` of two sets banded into over threshold / under threshold / new / dropped, with the revaluation impact (change × quantity on hand) that activation *would* post. The run changes nothing (CST-023) and says so on the last page. | `CSTSETCM CO=STC COSTSET=FY2027STD [COMPARETO=] [THRESHOLD=5] [MODE=ALL\|EXCEPTION] [USER=]` | CST-007, CST-009, CST-012, CST-013, CST-023 |
| `CSTWIPRF` | `CSTRP050` Work in progress roll-forward. One band per financial status, two detail lines per job: opening + material + labour + overhead + subcontract going in, then returns, completion transfers, scrap and write-off coming out, with a `balanced` flag on every job. Each line is **WIPCALC's own answer** (`CALC`, or `POST` to write `cst.WipBalance` as it goes), never a sum written inside the report, so a completion transfer takes value *out* of WIP instead of leaving it in both balances (CST-018). Jobs WIPCALC refuses are counted and named on the console, not dropped silently. | `CSTWIPRF CO=STC [PERIOD=yyyynn] [JOB=] [MODE=CALC\|POST] [USER=]` | RPT-015, CST-014, CST-018, CST-020 |

Band subtotals are accumulated in WORKING-STORAGE and parked in `BT-*` fields at each break rather
than declared with the Report Writer `SUM` clause: these groups have a second detail line, and `SUM`
counts every `GENERATE` of the report, so it doubled every subtotal.

### Shared copybooks

* `CSTWORK.cpy` — SQL staging row, numeric edit fields and the nine cost-element buckets every
  costing program reconciles against (CST-002).
* `COSTNORMCA` `FIFOVALCA` `MWAVALCA` `STDCOSTCA` `LABOVHCA` `COSTROLLCA` `MARGINCA` `JOBCOSTCA`
  `WIPCALCCA` `VARIANCECA` `NRVTESTCA` `WHATIFCA` `LANDEDCA` — every one now has its program.

### Stored procedures, views and seed

| Object | Purpose |
|---|---|
| `cst.usp_OrderMargin` | Ex-VAT revenue and cost of a whole sales order on one row. **`MARGIN` `ORDER` calls this**; the margin rules and the `UNDEFINED` states stay in COBOL. Lines with no cost are counted (`LinesNoCost`), never valued at zero. |
| `cst.usp_CostEnquiry` | Every basis we hold for one item side by side — average, FIFO oldest layer, standard from the active set, allowance and NRV. |
| `cst.usp_JobProfitability` | Budget, committed, accrued, actual, WIP and realised margin per job, with sub-jobs under their parent and counted once (D-13). |
| `cst.usp_ValuationRecon` | CST-022. Pool carrying value less allowance against the stock control account, with the difference and a `ReconciledFlag` stated rather than left to be found. With no `@AccountCode` it reads **every** account marked `ControlType = 'INVENTORY'`, by posting date - the accounts and the date rule `RECONCTL` groups by - so the valuation report and the control-account reconciliation cannot tell two stories about stock. An explicit `@AccountCode` still reconciles that one account on its own. |
| `cst.usp_CostSetList` | The cost set worklist behind `frmCostSet`: status, item count, and preparer beside approver with a `SelfApproved` flag, so the set that activation will refuse (CST0405) is visible before anybody tries. |
| `cst.usp_JobList` | The job worklist behind `frmJobCost`: budget, actual, committed, variance and the latest period's closing WIP, sub-jobs listed under their parent (D-13). |
| `cst.vw_ItemValuation` | One row per valuation pool, net of allowance, with a `ValuationState` of `NORMAL`/`IMPAIRED`/`RESIDUAL`/`NEGATIVE`. |
| `cst.vw_JobSummary` | Budget against actual and committed for every job. |
| `cst.vw_CostSetCompare` | A set against the set it was compared with; items present in only one are shown as `ADDED`/`REMOVED`, never dropped. |

Seed: `db\seed\40-cst-elements.sql` (cost elements including the WIP treatment codes `TRANSFER`,
`MATRETURN`, `SCRAP`, `WRITEOFF`, `REVENUE`; valuation policy per D-04; work centres with
`NormalCapacityHours` per D-05; the `FY2026STD` active cost set with `FY2025STD` superseded and
`FY2027DRAFT` in preparation) and `db\seed\41-cst-menu.sql` (the Costing menu and the four
`core.ReportDef` rows `CSTRP010` valuation, `CSTRP020` job profitability, `CSTRP030` variance,
`CSTRP040` cost set comparison).

`db\seed\42-cst-eventmap.sql` adds the nine costing postings Finance had not already mapped —
`CST.NRVREVERSAL`, `CST.LANDEDADJ`, `CST.VARPPV`, `CST.VARINVPRICE`, `CST.VARLANDED`,
`CST.VARRETURN`, `CST.WIPTRANSFER`, `CST.WIPWRITEOFF` and `CST.OVHDISPOSAL` — plus the
`core.MaintDef` / `core.MaintColumn` rows behind the five generic maintenance menu entries
(`CST.ELEMENT`, `CST.POLICY`, `CST.RATES`, `CST.LANDED`, `CST.NRV`). `10-fin-eventmap.sql` is never
edited; an unmapped posting is blocked, not sent to suspense.

`db\seed\94-cst-demo.sql` builds the demonstration data: contract `CST27000` with sub-jobs
`CST27001` and `CST27002`, an original budget superseded by an approved revision, actual, committed
and accrued cost plus a stage-payment revenue line, the internal job `CST27100` that has no revenue
at all, WIP balances, seven variances (one deliberately left with no owner and no disposal), the
`FY2027STD` cost set still in `REVIEW` against the live `FY2026STD`, four costed pools — one of them
written down to net realisable value — and three FIFO layers.

### Tests

`cobol\tests\cst\TCSTCALC.cob` (42 assertions — spec C3/C5/C7, CST-011/013/026) and
`cobol\tests\cst\TCSTLAND.cob` (42 assertions — spec C4 landed cost estimate then actual,
CST-008 two-line weight allocation and refusals, CST-021 write-down and at-cost, CST-025
reversal cap, CST-023 scenario safety). Neither touches the database: every path is driven with
the commit flag `N`, which is also what CST-023 demands of a scenario.

`cobol\tests\cst\TCSTVAL.cob` (42 assertions) and `cobol\tests\cst\TCSTJOB.cob` (46 assertions)
drive the same engines against **real rows**. TCSTVAL builds the C1 FIFO pool and the C2 average
pool in `cst.CostLayer` / `cst.ItemCost`, issues through `FIFOVAL` and `MWAVAL` with the commit
flag `T`, and reads the result back with `QUERY` so that the write is proved and not only the
arithmetic; it also reads a standard out of a named cost set through `STDCOST GETSTD` and checks
that an item with no standard in that set comes back `CST0406 INCOMPLETE` rather than zero.
TCSTJOB builds a contract with a sub-job, an approved budget of 5,000.00 and a draft of 9,000.00
that must be ignored, then proves worked example C5 (actual 2,100.00 + committed 900.00 = a
forecast at completion of 3,000.00, 2,000.00 favourable), that posting 2,400.00 more turns the
variance adverse and is reported rather than blocked, and worked example C6 — opening 1,000.00
+ additions 1,300.00 − deductions 1,100.00 = closing 1,200.00 — through `WIPCALC CALC`, `POST`,
`ENQUIRE` and `RESIDUAL`. Both suites wrap everything in `SQLIO BEGN`…`ROLB` on cursor 7, so they
leave nothing behind and are repeatable; both skip cleanly (TAP `# SKIP`) when no database or no
`cst` table is there.

`db\tests\cst-valuation.sql` (10 assertions) proves worked example C1 — ten at 5.00 then ten at
8.00, an issue of twelve relieved at 66.00 leaving 64.00 — and C2 — 400.00 over a hundred and
350.00 over fifty averaging 5.00, an issue of forty at 200.00 leaving 110 units worth 550.00 —
through `cst.CostLayer`, `cst.ItemCost` and `cst.usp_CostEnquiry`, and checks the three refusals
that keep a valuation honest: a layer cannot go negative, a pool is valued FIFO/MWA/STD and
nothing else, and a self-approved set is reported as such. `db\tests\cst-jobs.sql` (9 assertions)
proves the approved budget is the baseline, that a sub-job's cost is counted once, that closing
WIP is the roll-forward rather than a plug, and that a third job level and an unknown cost nature
are both refused. Every case runs inside a transaction that is rolled back.

`dotnet\STERP.Tests\Costing\CostingTests.cs` (9 assertions) covers the client side: every costing
COMMAREA parses and carries its window, the comparison and scenario windows copy into typed
display tables, the three worklists answer from the demo data, a roll-up states its completeness,
a what-if creates no cost set (CST-023) and the WIP roll-forward balances.

### Screens (`dotnet\STERP.Desktop\Forms\Costing`)

| Form | Menu entry | What it does |
|---|---|---|
| `frmCostEnquiry` | `CST.ENQUIRY` (Ctrl+K) | `cst.usp_CostEnquiry` — every basis side by side for one item, blank site meaning every site. |
| `frmCostSet` | `CST.COSTSET` | The cost set lifecycle through `STDCOST`: Review, Approve (F10), Activate, Impact and Compare. Comparison retains its selected action target. Impact reports that set's actual lifecycle/preparer/approver and refuses an unknown set. The worklist flags a set approved by its own preparer, and an attempt to approve one comes back as the engine's `CST0405` error rather than a silent refusal. |
| `frmCostRollup` | `CST.ROLLUP` (Ctrl+R) | `COSTROLL` `ROLLUP`/`GETLINE` with the costed tree and the element breakdown, the completeness and the missing-cost count in the status line; the What-if button prices three variants through `WHATIF` and writes nothing. |
| `frmJobCost` | `CST.JOB` (Ctrl+J), `CST.WIP` | `cst.usp_JobList` worklist, then `JOBCOST` `ENQUIRE`/`FORECAST` and `WIPCALC` `CALC`. Opened from Work in Progress (`FormArgument` `WIP`) the roll-forward is what F10 runs. |

`dotnet\STERP.Business\Costing\CostingService.vb` is the only .NET code that talks to the engines;
it fills the COMMAREA, pages the windows and shapes them for a grid. No costing rule lives in VB.

### Commit convention

`FIFOVAL`, `MWAVAL`, `STDCOST`, `JOBCOST`, `WIPCALC` and `VARIANCE` take a commit flag:

| Value | Meaning |
|---|---|
| `Y` | write and own the transaction (`BEGN`…`COMT`/`ROLB`) |
| `T` | write inside the **caller's** transaction — used by batch runs and by tests that wrap themselves in `BEGN`…`ROLB` |
| `N` | simulate only — nothing is written. `WHATIF` and the valuation report use this |

---

## 2. Interfaces other modules call

### `MARGIN` — sales margin (called by Sales)

```cobol
COPY MARGINCA REPLACING ==DFHCOMMAREA== BY ==WS-MARGIN-CA==.
...
MOVE "EXPECTED" TO CA-FUNCTION OF WS-MARGIN-CA
MOVE "STC"      TO CA-COMPANY  OF WS-MARGIN-CA
MOVE 1000.00    TO MG-GROSS-REVENUE
MOVE 600.00     TO MG-COST-AMOUNT
CALL "MARGIN" USING WS-MARGIN-CA
```

Key fields (full layout in `cobol\copy\MARGINCA.cpy`):

| Field | Picture | Notes |
|---|---|---|
| `MG-DOC-TYPE` / `MG-DOC-REF` / `MG-LINE-NUM` | `X(20)` / `X(40)` / `S9(9)` | document being measured |
| `MG-ITEM-CODE`, `MG-JOB-REF`, `MG-CUSTOMER` | `X(40)` / `X(20)` / `X(20)` | |
| `MG-QUANTITY`, `MG-UNIT-PRICE` | `S9(13)V9(6)`, `S9(11)V9(6)` | used when `MG-GROSS-REVENUE` is zero |
| `MG-GROSS-REVENUE`, `MG-DISCOUNT` | `S9(13)V9(2)` | |
| `MG-VAT-RATE`, `MG-REVENUE-INCL-VAT` | `S9(5)V9(4)`, `X` | `"Y"` strips VAT before the margin |
| `MG-NET-REVENUE`, `MG-VAT-AMOUNT` | `S9(13)V9(2)` | **out** — the ex-VAT denominator |
| `MG-COST-BASIS` / `MG-COST-BASIS-USED` | `X(12)` | `STANDARD` `AVERAGE` `FIFO` `ACTUAL` `QUOTE` |
| `MG-UNIT-COST`, `MG-COST-AMOUNT`, `MG-ADJUSTMENTS` | | supply a cost, or leave zero and `COSTNORM` is asked for `MG-COST-BASIS` |
| `MG-MARGIN-VALUE`, `MG-MARGIN-PCT`, `MG-MARKUP-PCT` | **out** | |
| `MG-MARGIN-STATE`, `MG-MARKUP-STATE` | `X(12)` | **out** — `OK` or `UNDEFINED`. A zero denominator is never shown as 0% |
| `MG-RESULT-NATURE` | `X(10)` | **out** — `EXPECTED` or `REALISED`, never mixed |
| `MG-COMPLETENESS`, `MG-LINES-NO-COST`, `MG-WARNING` | **out** | incomplete results are labelled |

Return codes: `00` ok, `04` undefined/incomplete but usable, `08` invalid input
(`CST0701`..`CST0708`), `16` document not found.

### `COSTROLL` — BOM cost roll-up

```cobol
COPY COSTROLLCA REPLACING ==DFHCOMMAREA== BY ==WS-COSTROLL-CA==.
...
MOVE "ROLLUP"   TO CA-FUNCTION OF WS-COSTROLL-CA
MOVE "FG-100"   TO CR-ITEM-CODE
MOVE 100        TO CR-LOT-SIZE
MOVE "FY2026STD" TO CR-COST-SET
MOVE "EXPAND"   TO CR-BOUNDARY
CALL "COSTROLL" USING WS-COSTROLL-CA
```

| Field | Notes |
|---|---|
| `CR-ITEM-CODE`, `CR-REVISION` | parent to roll up |
| `CR-COST-SET` | cost set that prices the components (passed through to `BOMCOST`) |
| `CR-TARGET-SET` | `SAVESET` writes the result into this **draft** set |
| `CR-LOT-SIZE` | costing lot size — the denominator a fixed setup charge needs |
| `CR-BOUNDARY` | `EXPAND` (use the subassembly's components) or `STOP` (use its stated unit cost) |
| `CR-WORK-CENTRE` | optional site burden applied by `LABOVH` on a base that holds no overhead |
| `CR-MATERIAL-COST` … `CR-RECOVERY-CREDIT` | **out** — element split |
| `CR-TOTAL-COST`, `CR-UNIT-COST`, `CR-GOOD-OUTPUT` | **out** — `unit = total / good output` |
| `CR-COMPLETENESS`, `CR-MISSING-COUNT`, `CR-ERROR-PATH`, `CR-WARNING` | **out** — a missing input is flagged, never valued at zero silently |
| `CR-LINES` (`OCCURS 20`) | costed tree window; `CR-L-CONTRIB` says whether a line contributed to the total or is shown for information only |

`COSTROLL` calls `BOMCOST` with `BC-PURPOSE = "COSTING"`; `BC-REQ-QTY` is the costing lot size and
`BC-MAX-DEPTH` is forced to 1 when the boundary is `STOP`.

---

## 3. Flows

```
purchase receipt ──► COSTNORM PURCOST ──► FIFOVAL RECEIPT   (FIFO pools)
                                      └─► MWAVAL  RECEIPT   (moving average pools)
                                      └─► STDCOST RECEIPT   (standard pools, PPV)

supplier invoice ──► STDCOST INVOICE   (incremental variance only)

issue to a kit   ──► FIFOVAL ISSUE / MWAVAL ISSUE ──► JOBCOST POSTCOST
period end       ──► WIPCALC CALC/POST ──► VARIANCE RECORD
job closure      ──► JOBCOST COMPLETE ──► JOBCOST CLOSE ──► WIPCALC RESIDUAL
cost set         ──► COSTROLL ROLLUP/SAVESET ──► STDCOST REVIEW ──► APPROVE ──► ACTIVATE
sales enquiry    ──► MARGIN EXPECTED/REALISED
```

---

## 4. Tests

`cobol\tests\cst\TCSTCALC.cob` — 42 assertions, no database required, using the worked examples of
specification section 8:

| Scenario | Assertion |
|---|---|
| **C3** | receipt of 100 at standard 9.00 ordered at 10.00 → inventory 900.00, GRNI 1,000.00, PPV 100.00; invoice 1,020.00 adds **20.00** incremental so the total adverse variance is **120.00**, not 220.00 |
| **C4** | goods 1,000.00 plus estimated freight 80.00 → provisional unit cost **10.80**; 5% duty and irrecoverable VAT capitalise to **1,260.00**; recoverable VAT stays out of stock value |
| **C5** | allowed 20 at 4.00 against actual 22 at 4.50 → usage **8.00**, price **11.00**, total **19.00**, and 99.00 − 80.00 reconciles |
| **C7** | a returned unit at standard 10.00 against original COGS 12.00 → 2.00 adverse; at standard 14.00 → 2.00 favourable |
| CST-011/026 | setup charged per run (120.00) not per unit; 6,000 hours against 8,000 normal capacity under-recovers 30,000.00 as a period expense |
| CST-013 | VAT stripped from a gross 1,200.00; zero revenue → `UNDEFINED`, missing cost → `UNDEFINED` + `INCOMPLETE` |

Run it with:

```
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-cobol.ps1 -Module itm -NoDll -NoBatch -NoTests
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-cobol.ps1 -Module cst -NoDll -NoBatch
bin\tests\TCSTCALC.exe
```

The `itm` build is needed first because `COSTNORM` statically calls `UOMCONV`; test executables link
every `obj\cobol\lib\libst_*.a`, so an empty `libst_itm.a` left behind by a concurrent build of the
Items module breaks the link.

---

## 5. How to demo

```
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-db.ps1 -Only "cst"
bin\x86\CSTINVAL.exe CO=STC ASAT=20260630 DETAIL=LAYER USER=COST
bin\x86\CSTJOBPR.exe CO=STC PERIOD=202603 USER=COST
bin\x86\CSTVARRP.exe CO=STC USER=COST
bin\x86\CSTSETCM.exe CO=STC COSTSET=FY2027STD COMPARETO=FY2026STD THRESHOLD=5 USER=COST
```

`db\seed\94-cst-demo.sql` supplies the data, and each run prints its spool file as the last line
(`STERP-I-SPOOL run\spool\...`). What to point at:

* **CSTJOBPR** — `CST27000` with its two sub-jobs indented beneath it and counted once; the estimate
  at completion over the approved budget; and `CST27100`, which has no revenue and therefore prints
  `n/a` rather than a hundred per cent loss.
* **CSTVARRP** — the landed cost variance with no owner and no disposal, flagged `UNEXPLAINED` and
  counted in the footer; return code 4.
* **CSTSETCM** — `FY2027STD` against `FY2026STD` in four bands, with the revaluation impact that
  activation *would* post, and the closing note that the run changed nothing.
* **CSTINVAL** — the written-down pool with its NRV sub-line, the FIFO layer detail, and the ledger
  reconciliation footing. On a demo database seeded without matching journals this correctly reports
  `*** NOT RECONCILED`; that is the report doing its job, not a defect.

On the screens: **Costing ▸ Item Cost Enquiry** on any costed item shows the bases side by side;
**Cost Sets** lists `FY2027STD` in `REVIEW` against the live `FY2026STD` — select it, press Compare
to see the item-by-item difference and the revaluation activation would post, then Approve as the
user who prepared it to see `CST0405` refuse it; **Roll-up and What-if** on an assembly prints the
costed tree with the boundary of each branch and the completeness of the answer; **Job Cost
Enquiry** on `CST27000` shows the sub-jobs under it, and **Work in Progress** runs the roll-forward
for the open period without committing anything.

---

## 6. Tables used

`cst.CostElement`, `cst.CostPolicy`, `cst.WorkCentre`, `cst.CostSet`, `cst.StandardCost`,
`cst.StandardCostElement`, `cst.ItemCost`, `cst.ItemCostHistory`, `cst.CostLayer`, `cst.Job`,
`cst.JobBudget`, `cst.JobBudgetLine`, `cst.JobCostTransaction`, `cst.WipBalance`, `cst.Variance`,
`cst.LandedCost`, `cst.LandedCostAllocation`, `cst.Impairment`. No new tables were needed.

See [tech debt](../tech-debt/cst.md).
