# STERP - Stefan's ERP: demonstration dataset

How the demonstration database is built, what it contains and how to prove that it hangs
together. The dataset is the one used for screenshots, acceptance walk-throughs and the
modernisation demo, so **every control account in the general ledger agrees with its
subsidiary ledger** at the demonstration cutoff `2026-10-31`.

## Rebuild it

```
build\demo-data.cmd -Rebuild                 drop, recreate, seed, run the drivers, reconcile
build\demo-data.cmd                          re-run the drivers and reconcile (no drop)
build\demo-data.cmd -Company CAL             the same for Caledonian Assemblies
build\demo-data.cmd -SkipDrivers             only level the controls and print the reconciliation
build\demo-data.cmd -Preview                 run everything in PREVIEW mode (changes nothing)
```

`build\demo-data.ps1` (wrapper `build\demo-data.cmd`) runs, in order:

1. `build\build-db.ps1 -Full` when `-Rebuild` is given - drop, recreate and seed the database.
2. The COBOL demonstration batch drivers in `bin\x86`: `FINDEMO` (finance), `PODEMO`
   (purchasing) and then **every other `*DEMO.exe`** that exists, so a driver added later by
   the sales-order or kit teams is picked up without editing this script. A driver that
   returns a non-zero code is recorded in the summary and the run continues - the modules are
   developed in parallel and a half-finished driver must not stop the dataset being usable.

   The drivers, and what each one leaves behind:

   | Driver | Arguments | What it builds |
   |---|---|---|
   | `FINDEMO` | `CO= FY=2026 FROM=1 TO=9 MODE= USER=GLCLERK` | Summarised trading journals for periods 1-9 |
   | `PODEMO` | `CO= SITE= MODE= USER=BUYER1` | Orders, approvals, receipts (GRNI), service acceptances |
   | `APDEMO` | `CO= SITE= MODE= USER=` | A payment proposal, signatures and a payment run |
   | `KITDEMO` | `CO= SITE= MODE= USER=` (also `ASAT=`, `QTY=`) | One kit from definition to assembled stock: trial, create/review/release, reserve, pick, issue, completion with genealogy, kit cost and one disassembly - see `docs\modules\kit.md` §2.3 |
   | `SODEMO` | `CO= SITE= MODE= USER=` | Sales orders, allocations, despatches and invoices |
3. `fin.usp_DemoBridgeControls` - levels the control accounts against the subsidiary ledgers
   (see below), because the drivers have just changed the subsidiary side.
4. `bin\x86\RECONCTL.exe` - the control-account reconciliation, printed to the console and
   written to `rpt.ReconciliationRun` and `run\spool\RECONCTL-*.lst`.

The exit code is RECONCTL's: 0 when every control family agrees, 4 when one does not.
Everything is re-runnable; a second run posts nothing.

## What the dataset contains

| Area | Source | Content |
|---|---|---|
| Companies, calendar, users | `db\seed\00..07-core-*.sql` | Stellar Components (STC) and Caledonian Assemblies (CAL), FY2026 periods 1-9, roles and users |
| Chart of accounts | `db\seed\08-fin-coa.sql` | ~110 accounts per company; control accounts marked with `ControlType` AR, AP, INVENTORY, WIP, GRNI, VAT, BANK, ACCRUAL, DEPOSIT, RETAINED |
| Finance activity | `db\seed\91-fin-demo.sql`, `FINDEMO` | Summarised trading journals for periods 1-9 posted through `fin.JournalStage` + `fin.usp_PostJournal` |
| Sales ledger | `db\seed\91-ar-demo.sql`, `db\seed\98-demo-ar-cal.sql` | Aged debt for both companies, receipts, a write-off, disputes, collection cases, promises to pay, Direct Debit mandates |
| Purchase ledger | `db\seed\90-ap-demo.sql` | Supplier invoices, credit notes, a payment run, three-way matched invoices |
| Stock, items, BOM, kits, costing | `db\seed\2x/3x/9x-*.sql` | Items, locations, stock movements, structures, execution kits with work in progress |
| Showcase bills of materials | `db\seed\90-bom-showcase.sql` | Two released multi-level structures with complete costs in the `BOMDEMO` cost set: a celebration cake `FG-CAKE-CELEB-8IN` (cake > sponge layer > batter > ingredients; phantom decoration > sugar rose > flower paste) and the commuter bicycle `FG-BIKE-COMMUTE-M` (bike > frame > front triangle > bottom bracket > bearings; wheelset > wheels; fork; cockpit; cable, hardware and paint phantoms). See [modules/bom.md](modules/bom.md#5-how-to-demo) |
| Purchasing | `db\seed\92-po-demo.sql`, `PODEMO` | Orders, receipts (goods received not invoiced), service acceptances |
| Kits and work in progress | `db\seed\95-kit-demo.sql`, `KITDEMO` | Six seeded kits in various states, plus one kit per run date driven right through: issues, a completion into stock with genealogy, a kit cost and a disassembly |
| Consistency bridge | `db\seed\98-demo-consistency.sql` | Brings the general ledger control accounts onto the subsidiary ledgers |

## The consistency bridge (`db\seed\98-demo-consistency.sql`)

The seeds and drivers are written by different module teams, so the two sides of a control
account were seeded independently: the finance demonstration posted summarised sales and
purchase journals while the sales and purchase ledgers seeded their own open items. The
general ledger therefore showed debtors of 330,381.00 against open items of 176,541.93.

`fin.usp_DemoBridgeControls @CompanyCode, @AsAt, @UserCode, @Tolerance, @Report`

* measures each control family **exactly the way `RECONCTL` measures it** (the same subsidiary
  expressions, the same cutoff, the same `-1` sign for liability families);
* posts the residual difference through `fin.JournalStage` and `fin.usp_PostJournal`
  (`@SeriesCode 'JNL'`, one journal per family, correlation id `DEMOBRG-<co>-<family>-<date>-<stamp>`);
* puts the contra on account **3400 "Opening balance reserve, cut over differences"** - the
  account the conversion suite uses for exactly this purpose - so no profit and loss account
  and no other control family is disturbed;
* seeds a demonstration bank statement in `fin.BankRecon` for the BANK family, whose subsidiary
  side is the latest reconciliation per bank account rather than a ledger, and **restates** its own
  `DEMO-%` statement for the same cutoff instead of skipping it, so cash banked after the statement
  was first built (the batch drivers, the acceptance suite) is levelled too rather than reported as
  an unexplained bank difference for ever. A real reconciliation of the same cutoff is never touched;
* is **self-levelling**: it only ever posts what is still outstanding, so it is safe to run
  after the batch drivers, and a second run posts nothing at all;
* is run **once for every as-at the job streams reconcile at**, earliest first - the clock date
  (`NIGHTLY` `AGEDDEBT`, `MONTHEND` `REVALRUN`/`CSTINVAL`) and then the demonstration cutoff
  (`MONTHEND` `ARRECON`/`RECONCTL`, `build\demo-data.ps1`). A bridge journal is dated at its own
  as-at, so levelling the cutoff alone leaves an as-at-today reconciliation reporting the whole
  bridge as a difference; because the procedure is self-levelling the later pass posts only the
  residual. Both `db\seed\98-demo-consistency.sql` and `build\demo-data.ps1` do this.

It also repairs the `ControlType` marking on the stock and work in progress accounts
(1100/1110/1120/1130/1140) for databases that were built before the chart of accounts carried
them - without a marked control account `RECONCTL` has nothing to reconcile INVENTORY or WIP
against and prints `NO CONTROL`.

The bridge runs for every company at the end of the seed, so a plain `build\build-db.ps1`
already leaves a reconciled database.

## Reconciliation, before and after (STC at 2026-10-31)

| Family | Subsidiary | GL before | Difference before | GL after | Difference after |
|---|---:|---:|---:|---:|---:|
| AR Trade debtors | 176,541.93 | 330,381.00 | -153,839.07 | 176,541.93 | 0.00 |
| AP Trade creditors | 50,796.82 | 183,432.82 | -132,636.00 | 50,796.82 | 0.00 |
| INVENTORY Stock | 1,060,051.85 | 0.00 (no control) | -1,060,051.85 | 1,060,051.85 | 0.00 |
| WIP Work in progress | 2,482.50 | 0.00 (no control) | 2,482.50 | 2,482.50 | 0.00 |
| GRNI Goods received not invoiced | -83,388.82 | 0.00 (no control) | -83,388.82 | -83,388.82 | 0.00 |
| VAT control | 103,194.00 | 91,168.00 | 12,026.00 | 103,194.00 | 0.00 |
| BANK Bank and cash | 0.00 | 376,085.00 | -376,085.00 | 376,085.00 | 0.00 |
| ACCRUAL Service accruals | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
| **Control totals** | **249,571.89** | **981,066.82** | **-731,494.93** | **625,656.89** | **0.00** |

`RECONCTL` returned 4 with seven unreconciled families; it now returns 0 with none. Caledonian
is levelled by the same procedure (AR -145,800.00, AP +79,581.60, GRNI +17,090.00,
VAT -3,658.00 and a 237,747.00 demonstration bank statement).

`ACCRUAL` prints `NO CONTROL` while both sides are zero: there are no service accrual journals
and no unmatched service acceptances in the dataset, so the general ledger has no line on an
`ACCRUAL` account to group. The family is counted as reconciled.

## One definition per control account

A control account that two reports measure two different ways cannot be reconciled, only
argued about. The definitions below are shared, and `db\tests\xmd-recon.sql` assertions 17-20
fail if they drift apart again.

| Control | The one measure | Read by |
|---|---|---|
| AR Trade debtors | the **outstanding** sales ledger: original amount less the applied, discount and write-off amounts of active allocations at the cutoff. A settlement discount or write-off leaves the customer owing less, so it leaves the control account too | `RECONCTL` 2110, `ARRECON` 1000/3000, `AGEDDEBT` 1000/1900, `fin.usp_DemoBridgeControls` |
| INVENTORY Stock | the **costing pool**: `cst.ItemCost.CarryingValueGBP` less `AllowanceGBP`, against every account marked `ControlType = 'INVENTORY'` by posting date | `RECONCTL` 2130, `CSTINVAL` through `cst.usp_ValuationRecon`, `fin.usp_DemoBridgeControls` |

Stock used to be measured from the running total of `inv.StockMovement`. The movement history
holds only the movements the demonstration drivers made - the seeded opening balances have no
valued `OPENING` movement - so issuing stock that was never received made the "valuation"
negative (-54.54, then -154.04). The stock on hand at cost is the costing pool, which is what
`CSTINVAL` has always reconciled and what `RECONCTL` now reconciles as well.

## Checks

```
build\build-db.cmd -Full                     clean build (seeds leave the ledgers reconciled)
build\run-tests.cmd -Layer sql               SQL suite
build\demo-data.cmd                          drivers, bridge, reconciliation (exit code 0)
bin\x86\RECONCTL.exe CO=STC ASAT=20261031 MODE=COMMIT USER=FINCTRL
```

## Known gaps

* **The bridge is a work-around, not a design.** `db\seed\91-fin-demo.sql` posts summarised trading
  journals that are not derived from the sales and purchase ledger demonstrations
  (`91-ar-demo.sql`, `90-ap-demo.sql`), so `fin.usp_DemoBridgeControls` posts the residual against
  3400. The lasting fix is to derive one side from the other - generate the open items through
  `ARINVPST` and the AP posting programs, or reduce the aggregate journals by what the ledgers
  already carry. The bridge only ever posts what is still outstanding, so it then simply stops
  posting.
* **`db\seed\98-demo-ar-cal.sql`** adds Caledonian's dispute, collection case and promise to pay.
  Fold it into `91-ar-demo.sql` when that file is next touched, and delete the 98 file.
* **Driver naming.** `build\demo-data.ps1` runs `FINDEMO`, `PODEMO`, then every other
  `bin\x86\*DEMO.exe` with `CO= SITE= MODE=COMMIT USER=`; a new driver that takes those arguments is
  picked up with no change to the script.
