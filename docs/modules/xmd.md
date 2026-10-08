# XMD - cross-module controls and reporting

Module code `xmd`, requirement families **XMD-001..XMD-013** and **RPT-001..RPT-019**, plus the
cross-cutting COM requirements that only show up between modules (COM-018, COM-020, COM-022,
COM-023, COM-025). COBOL dialect **`mvs`**, fixed format, 1993 batch-suite style: numbered
paragraphs, `PERFORM ... THRU ... EXIT`, `GO TO <para>-EXIT`, `SORT` and Report Writer.

The module owns no masters of its own. It reads what the other modules post and says whether the
answers agree.

## Programs

| Program | Kind | Purpose | Requirements |
|---|---|---|---|
| `RECONCTL` | batch, Report Writer | Control-account reconciliation at one shared cutoff: AR, AP, inventory, WIP, GRNI, VAT, bank, accruals. Writes `rpt.ReconciliationRun` and raises an owned exception per difference. | XMD-005, XMD-006, XMD-010, XMD-012, FIN-021, RPT-015 |
| `OBLIGREG` | batch, Report Writer | Statutory obligations register per company with derived due dates, owner, completion evidence and escalation. Maintains `core.StatutoryObligation`. | XMD-013, XMD-009, COM-022 |
| `EXCPAGE` | batch, Report Writer | Exception ageing and ownership, escalation, and the RPT-016 audit / approvals / data-quality sections. | XMD-006, XMD-009, COM-017, COM-022, RPT-016 |
| `KPIOTIF` | batch, Report Writer | Customer and supplier on-time-in-full on the published `rpt.MetricDefinition` definitions (D-19). | XMD-010, XMD-001, XMD-002, RPT-008, RPT-009 |
| `CHAINTRC` | online, LINKable | Order-to-cash / procure-to-pay / assembly chain trace for one document. COMMAREA `CHAINTRCCA`, function `TRACE`. | XMD-001..XMD-004, XMD-012 |
| `CHAINRPT` | batch, Report Writer | Printed chain trace; calls `CHAINTRC` so the screen and the listing can never disagree. | XMD-001..XMD-004 |
| `CONVLOAD` | batch, `SORT` + Report Writer | Legacy conversion load from fixed-width `.DAT` extracts with PREVIEW / COMMIT, per-record outcome and restart without duplicates. | XMD-007, XMD-008, XMD-011, COM-018, COM-023, FIN-024, AR-021, AP-024, ITM-024 |
| `CASHFCST` | batch, `SORT` + Report Writer | Cash forecast: opening bank funds then dated AR, AP, payment-instruction, VAT and corporation-tax flows, kept apart as contractual / expected / confirmed and never counted twice. | RPT-004, FIN-017, FIN-018 |
| `VATAUDIT` | batch, Report Writer | VAT detail (`MODE=DETAIL`, `fin.VatTransaction` per box) and MTD audit trail (`MODE=MTD`, boxes 1-9, error corrections, postponed import VAT, submission evidence), proved against the VAT control account. | RPT-007, RPT-017, FIN-016, FIN-028 |

Reporting views and procedures: `rpt.vw_ControlBalance` (the RECONCTL GL control balance as a set),
`rpt.vw_ExceptionAgeing` (the EXCPAGE buckets), `rpt.usp_ChainTrace` (set-based CHAINTRC for the
client grid). SQL proofs: `db\tests\xmd-recon.sql`, `db\tests\xmd-reportdef.sql`.

Job streams owned by this module: `jobs\XMDCLOSE.cmd` (RECONCTL, OBLIGREG, EXCPAGE at one cutoff),
`jobs\XMDCONV.cmd` (all four `data\legacy\CAL*.DAT` extracts in PREVIEW then COMMIT) and
`jobs\OPSCON.cmd` (starts the operator console with `COB_SCREEN_EXCEPTIONS=Y` / `COB_SCREEN_ESC=Y`).
Client: menu rows `RPT.CONTROLS.*` open `frmReportLauncher` with the report code in `FormArgument`;
`core.ReportDef` rows whose code starts with `JOB.` are submitted as `jobs\<name>.cmd`.

Copybooks: `XMDBATWK.cpy` (suite working storage), `XMDBATPR.cpy` (suite housekeeping - job log,
spool path, headings, fetch widths, date editing, period resolution, parameter reading, exception
raising), `CHAINTRCCA.cpy` (chain trace COMMAREA).

## How to run

```
bin\x86\RECONCTL.exe CO=STC ASAT=20261031 MODE=COMMIT USER=FINCTRL [TOL=0.01]
bin\x86\OBLIGREG.exe CO=STC ASAT=20261031 MODE=COMMIT USER=FINCTRL [DAYS=30]
bin\x86\EXCPAGE.exe  CO=STC ASAT=20261031 MODE=COMMIT USER=FINCTRL [OWNER=APMGR] [DAYS=30]
bin\x86\KPIOTIF.exe  CO=STC FROM=20260401 TO=20261031 USER=FINCTRL [DETAIL=Y]
bin\x86\CHAINRPT.exe CO=STC TYPE=ORDER REF=<order number> USER=SALESMGR
bin\x86\CONVLOAD.exe CO=CAL FILE=data\legacy\CALAROPN.DAT CUTOVER=20260331 MODE=PREVIEW USER=FINCTRL
```

`MODE=PREVIEW` is the default everywhere and changes nothing. Every run registers in `core.JobRun`
through `JOBLOG` and finishes with `STERP-I-SPOOL <path>`; reports are 132-column `run\spool\*.lst`
written by Report Writer through `STRPTHDR`.

## Reconciliation (RECONCTL, RPT-015)

Both sides of every control family are taken at the **same event cutoff**: the general-ledger side
from `fin.JournalLine` joined to `fin.Journal` where `Status = 'POSTED'` and
`PostingDate <= cutoff`, grouped by `fin.Account.ControlType`; the subsidiary side from the owning
module at the same cutoff. Liability families (`AP`, `GRNI`, `VAT`, `ACCRUAL`) carry a sign of -1
so that both sides read as a positive obligation.

| Family | Subsidiary source |
|---|---|
| AR | the **outstanding** sales ledger: `ar.OpenItem.OriginalAmountGBP` posted on or before the cutoff and not reversed, less the applied, discount and write-off amounts of active `ar.Allocation` rows effective by the cutoff. The same expression `ARRECON` and `AGEDDEBT` use, so the three proofs of the debtors control cannot disagree |
| AP | `ap.OpenItem.OriginalAmountGBP`, same basis |
| INVENTORY | the costing pool `cst.ItemCost.CarryingValueGBP` less `AllowanceGBP` - the stock on hand at cost, which is what `CSTINVAL` reconciles through `cst.usp_ValuationRecon`. The pool is a position rather than a dated history, so the cutoff is consumed and not applied (as it is for WIP and ACCRUAL). Measuring it from `inv.StockMovement` instead reported only the movements the drivers made, and so went negative when seeded opening stock was issued |
| WIP | `kit.ExecutionKit.WipValueGBP` for kits that are neither draft nor closed |
| GRNI | `po.Receipt.GrniValueGBP` less `ap.InvoiceMatch.GrniClearedGBP` |
| VAT | `fin.VatTransaction`: output VAT less recoverable input VAT |
| BANK | latest `fin.BankRecon` on or before the cutoff, statement balance adjusted for outstanding items |
| ACCRUAL | `po.ServiceAcceptance` accepted less matched value |

A difference larger than `XMD.RECON.TOLERANCE` (baseline £0.01) is printed, written to
`rpt.ReconciliationRun` with status `OPEN` and raised against the named control-account owner
(`CREDMGR`, `APMGR`, `COSTACC`, `PURMGR`, `VATOWNER`, `TREASURY`, `FINCTRL`). Re-running the same
cutoff deletes only its own `OPEN` rows first, so the run is repeatable. Return code 4 when any
family differs.

## Statutory obligations (OBLIGREG, XMD-013)

Due dates are derived from `core.Company` (accounting reference date, legal form, VAT stagger,
payment-practices and packaging flags) using `DATECALC`:

| Obligation | Derived due date |
|---|---|
| `VATRETURN` | `fin.VatPeriod.FilingDueDate`, or period end + 1 calendar month + 7 days |
| `ANNUALACCTS` | ARD + 9 months (private) or + 6 months (`LegalForm = 'PLC'`) |
| `CORPTAXPAY` | period end + 9 months + 1 day |
| `CORPTAX` | period end + 12 months |
| `CONFSTMT` | review period end + 14 days |
| `PAYPRACTICES` | each half of the financial year + 30 days (in-scope companies) |
| `PACKAGINGDATA` | obligation period end + 1 month (in-scope companies) |

The register row in `core.StatutoryObligation` supplies status, owner and evidence. A row marked
`COMPLETE` with no `EvidenceRef` is reported as `NO EVIDENCE` and stays open - a tick is not a
submission reference. Anything overdue or inside `XMD.OBLIG.ESCALATE.DAYS` is escalated through
`EXCPRAIS` to the owner and the financial controller.

## Exceptions (EXCPAGE, RPT-016)

Age is `DATEDIFF(day, RaisedAt, as-at)` - from **first detection**, never from reassignment, so
passing an exception on does not make it young again and the previous owner stays in the message.
Buckets 0-7 / 8-30 / 31-60 / 61+ by count and value, holds and unowned exceptions counted
separately. Items older than `XMD.EXCEPTION.ESCALATE.DAYS` or past their due date are escalated and
stamped with `EscalatedAt` in COMMIT mode. The RPT-016 tail adds pending approvals (and those older
than five days), audited actions carrying an authority reference, the count of restricted audit rows
(values withheld from the listing and its export) and open exceptions by category.

## Measures (KPIOTIF, XMD-010, D-19)

`rpt.MetricDefinition` holds the single published definition of each shared measure, and the report
prints it above the numbers. Customer OTIF compares `so.Despatch.DespatchDate` with
`so.DeliverySchedule.OriginalPromiseDate` and requires the full scheduled quantity. Supplier OTIF
compares `po.Receipt.PhysicalDate` with `po.PurchaseSchedule.OriginalConfirmedDate`; up to
`XMD.OTIF.EARLY.DAYS` (5) **working** days early still counts as on time, earlier than that is
reported as `EARLY`. **A zero cohort prints `UNDEFINED`** - never 0% and never 100%.

## Chain trace (CHAINTRC / CHAINRPT, XMD-001..004)

`CALL "CHAINTRC" USING WS-CT-CA` with `CA-FUNCTION = "TRACE"`, `CT-DOC-TYPE` of `ORDER`, `PO` or
`KIT` and `CT-DOC-REF`. The answer is up to 24 ordered steps, each with stage, document type,
reference, date, status, quantity, value and a `CT-KIND` of `OPERATION`, `FINANCIAL` or `REVERSAL`.
Operational steps come from `so.DeliverySchedule`/`so.Despatch`, `po.PurchaseSchedule`/`po.Receipt`
or `kit.ExecutionKit`/`kit.ExecutionKitLine`; financial effects from `fin.Journal` on `SourceRef`,
with `IsReversal` rows shown as their own step. `CT-COMPLETE`, `CT-BROKEN` and `CT-TRUNCATED` state
partial completion explicitly. Return codes: 08 bad request, 16 nothing recorded for the reference,
04 chain with no posted financial effect yet.

## Conversion load (CONVLOAD, XMD-007)

Record layouts in `data\legacy\*.DAT` (fixed width, one type per record, type in columns 1-4):

| Type | Layout |
|---|---|
| `AROP` | 5-14 customer, 15-24 document number, 25-26 kind (`IN`/`CR`), 27-34 document date, 35-42 due date, 43-45 currency, 46-60 amount, 61-70 terms, 71-80 legacy reference |
| `APOP` | as `AROP` with supplier code and supplier document reference |
| `STKB` | 5-14 site, 15-24 warehouse, 25-64 item, 65-69 revision, 70-79 lot, 80-94 quantity, 95-109 unit cost, 110-119 location |
| `GLTB` | 5-16 account, 17-24 balance date, 25-39 debit, 40-54 credit, 55-84 narrative, 85-94 cost centre |

The file is `SORT`ed on type plus natural key plus input sequence, so in-file duplicates arrive
adjacent and only the first is accepted. Each record then gets one of four outcomes:

* `ACCEPTED` - format, calendar date, cut-over date, numeric amount and master existence all pass.
* `REJECTED` - with the reason printed under the line (unknown customer/supplier/item/account,
  impossible date such as 30 February, non-numeric amount, negative opening stock, nil open item).
* `DUPLICATE` - the same natural key earlier in the file, or already in the ledger.
* `LOADED` - already loaded by an earlier run of the **same** batch.

`BatchRef` is derived as `CONV-<file base name>-<cut over>`, so a restart finds its own
`stg.ImportBatch` and skips the rows it already wrote (COM-023). PREVIEW writes nothing. COMMIT
writes `stg.ImportBatch`/`stg.ImportRow` and, for `GLTB`, `fin.OpeningBalance`. The accepted opening
trial balance is proved: out of balance ends the run with return code 8 and an owned exception.

Supplied cut-over extract for Caledon Assemblies at 31 March 2026 (`CAL`, deliberately imperfect):

| File | Records | Accepted | Rejected | Duplicate |
|---|---|---|---|---|
| `CALAROPN.DAT` | 8 | 4 | 3 | 1 |
| `CALAPOPN.DAT` | 6 | 4 | 1 | 1 |
| `CALSTOCK.DAT` | 6 | 3 | 2 | 1 |
| `CALGLTB.DAT` | 7 | 5 | 2 | 0 |

The accepted `GLTB` rows balance at £173,250.75 debit and credit; the two rejected rows (an unknown
suspense account and a row dated 32 March) cancel each other, which is exactly the situation the
trial-balance proof is there to notice.

## Report catalogue (RPT-001..RPT-019)

`db\seed\97-xmd-reports.sql` registers the whole catalogue of specification 11 section 8 in
`core.ReportDef`, pointing each id at the program that produces it. Where a module already owns the
program the catalogue entry points at that program (AGEDDEBT, APAGED, DUNNING, CASHBOOK, TBRPT,
PLRPT, MTDJSON, STKSTAT, BOMRP010) and where the module registered its own report code the RPT id is
appended to its `RequirementRefs` (POPIPE, CSTRP010, CSTRP020, KITRP010, APPPR, SOANAL, PKGRPT).
RPT-015, RPT-016 and the xmd reports are produced here. The same seed publishes
`rpt.MetricDefinition`, the `XMD.*` tuning parameters and the `RPT.CONTROLS` menu.

Proof that nothing is missing:

```sql
SELECT ReportCode, ProgramName, RequirementRefs FROM core.ReportDef
WHERE RequirementRefs LIKE '%RPT-%' ORDER BY ReportCode;
```

## Tests

`build\run-tests.ps1 -Layer cobol -Filter TXMD`

| Suite | Covers |
|---|---|
| `TXMDCONV` | Runs `CONVLOAD` in COMMIT mode over the supplied extract and proves 8 read / 4 accepted / 3 rejected / 1 duplicate, the rejection reason for the impossible date, one staging row per source record, and that a **restart** adds nothing and accepts nothing a second time. Cleans up its batch. |
| `TXMDCTL` | `CHAINTRC` input control (08 for missing company, missing reference, unsupported document type and unknown function, with stable reason codes), 16 plus `CT-BROKEN` for a reference that does not exist, a real order producing an ordered `O2C` chain, and the published OTIF measure definitions with their zero-denominator rule and decision reference. |

## How to demo

1. `bin\x86\CONVLOAD.exe CO=CAL FILE=data\legacy\CALAROPN.DAT CUTOVER=20260331 MODE=PREVIEW USER=FINCTRL`
   then the same line with `MODE=COMMIT`, then run it **again** - the third listing shows every row
   as `LOADED`, nothing loaded twice.
2. `bin\x86\RECONCTL.exe CO=STC ASAT=20261031 MODE=COMMIT USER=FINCTRL` - eight control families,
   the differences owned by name, and `SELECT * FROM rpt.ReconciliationRun` to show the stored run.
3. `bin\x86\OBLIGREG.exe CO=STC ASAT=20261031 MODE=COMMIT USER=FINCTRL` - the statutory diary with
   overdue filings escalated.
4. `bin\x86\EXCPAGE.exe CO=STC ASAT=20261031 MODE=PREVIEW USER=FINCTRL` - everything raised by every
   module, aged and owned, with the approvals and data-quality tail.
5. `bin\x86\CHAINRPT.exe CO=STC TYPE=ORDER REF=<order> USER=SALESMGR` - one order from schedule to
   despatch to posted journals, with reversals on their own lines.

## Conversion load: what COMMIT now creates (2026-10-07)

`CONVLOAD` no longer stops at `stg.ImportRow`. In COMMIT mode the whole record batch runs
inside one `BEGN`/`COMT` unit of work (`0400-BEGIN-WORK` / `3400-END-WORK`); anything
worse than a warning rolls the conversion back so a batch is never half loaded.

| Record | What an accepted row creates | `stg.ImportRow.CreatedDocumentType` / `Ref` |
| --- | --- | --- |
| `AROP` | `ar.OpenItem` (`INVOICE`/`CREDIT`, `OPEN`, residual = original) | `AROPENITEM` / document number |
| `APOP` | `ap.OpenItem` (with `SupplierDocRef`) | `APOPENITEM` / document number |
| `STKB` | an `OPENING` movement through `STKMOVE` -> `inv.usp_PostStockMovement` | `STOCKMOVE` / movement id |
| `GLTB` | `fin.OpeningBalance` (`SourceType` `LEDGER`) | `OPENINGBAL` / account code |

Foreign currency is converted with the latest `core.FxRate.UnitsPerGBP` on or before the cut
over; the rate used is kept on the open item.

Duplicate control, in the order it is applied (`2100-ONE-RECORD`):

1. the same natural key earlier in the same file -> `DUPLICATE`;
2. `2700-ALREADY-LOADED` - this batch already processed the key -> `LOADED` (a restart
   therefore skips its own work rather than reporting it as a ledger duplicate);
3. `2600-LEDGER-DUP` - the live ledger already holds the reference (`ar.OpenItem`,
   `ap.OpenItem`, `fin.OpeningBalance` and now `inv.StockMovement` for `STKB`) ->
   `DUPLICATE`.

Opening stock is dated at the cut over but **booked** in the period the batch runs in, because
`inv.usp_PostStockMovement` raises on a closed period and that would abandon the batch
transaction. `2400-VALID-ST` refuses the row with "No open period to book the opening stock
in" before the contract is ever called.

`data\legacy\CALSTOCK.DAT` was re-cut onto lot and non traced CAL items (the previous fixture
used the four serial controlled bikes, which cannot be loaded without serial numbers).

## Chain trace screen

* `dotnet\STERP.Business\Xmd\XmdService.vb` - `ChainTrace` links `CHAINTRC` (24 step
  COMMAREA window) and `ChainTraceList` runs the set based twin `rpt.usp_ChainTrace`
  (`ORDER`, `PO` or `KIT` only). `Reports` returns the XMD catalogue rows.
* `dotnet\STERP.Desktop\Forms\Xmd\frmChainTrace.vb` (+ `.Designer.vb`) - a `ListForm`
  with a document type list and a reference, F10 to trace, and a status line that calls out a
  broken or truncated chain. Menu row `RPT.CONTROLS.CHAIN` opens it; the old report form is
  still reachable as `RPT.CONTROLS.CHAINRPT`.
* Harness tests: `dotnet\STERP.Tests\Xmd\XmdTests.cs` (8 tests).
