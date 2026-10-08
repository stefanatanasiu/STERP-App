# Purchase order management (`po`)

Module code `po`, requirement family **PO-001..PO-027**, specification
[06-Purchase-Order-Management.md](https://github.com/stefanatanasiu/STERP-Supply-Chain-Functional-Specification/blob/main/06-Purchase-Order-Management.md),
acceptance scenarios AC-050..AC-058, decisions **D-07, D-08, D-25, D-29**.

COBOL dialect: **ACUCOBOL-GT (`acu`)**, fixed format, upper case, numbered paragraphs — the 1998
distribution suite. Schema `po` (plus `cst.LandedCost*` for charges and `fin.Budget*` for budgets).

> **Status: partially delivered.** Sourcing, requisitions, order capture/approval/issue, pricing,
> budget and commitment control, tolerance, landed cost, due diligence, payment terms and the
> nominal-posting engine are built. Receipt, inspection, service acceptance, returns, import
> entries, closure, the batch reports, the .NET layer and the seeds are still open — see the
> `po-*` rows in the session `todos` table for a self-contained description of each.

## Programs delivered

| Program | Kind | Purpose | Requirements |
|---|---|---|---|
| `POPOST` | internal service (`POPOSTWK.cpy`) | The purchasing suite's only route into the nominal ledger. Stages lines in `fin.JournalStage` under the caller's correlation id and lets `fin.usp_PostJournal` own the transaction, so a procedure that throws can never doom an open SQLIO transaction. The same correlation id posted twice returns the original journal with `WasDuplicate`, so a retried goods receipt cannot accrue GRNI twice. | PO-009, PO-014, PO-017, PO-025, COM-014, COM-015 |
| `POREQ` | LINKable (`POREQCA.cpy`) | Requisition lifecycle `CREATE`/`ADDLINE`/`SUBMIT`/`APPROVE`/`CANCEL`/`CONVERT`/`ENQUIRE`. Every requisition carries demand ownership — who asked, why, and which demand (`MANUAL`, `SHORTAGE`, `REPLEN`, `MRP`, `SALESORD`, `KIT`) it answers. Nothing here writes to `po.Commitment`: a requisition is a need, not a supplier commitment. | PO-001, PO-005 |
| `SOURCSEL` | LINKable (`SOURCSELCA.cpy`) | Approved source register and quotation comparison: `CHECK`, `OPEN`, `QUOTE`, `RANK`, `SELECT`. The cheapest quotation is never selected automatically — choosing above the cheapest requires a recorded reason, and an unapproved, suspended or out-of-date source needs a named authoriser. | PO-002, PO-004, PO-023 |
| `POPRICE` | LINKable (`POPRICECA.cpy`) | Purchase price determination from the effective-dated agreement, quotation or list terms that applied **on the pricing date**, with quantity breaks, discounts and price-per units. Contract beats quote beats list; an explicit item agreement beats an item-group one. Functions `PRICE`, `BREAKS`, `HISTORY` (last price actually received, which feeds source selection). | PO-004, PO-002 |
| `POORDER` | LINKable (`POORDERCA.cpy`) | Order capture and issue: `CREATE`, `ADDLINE`, `SCHEDULE`, `ISSUE`, `ENQUIRE`; delegates `ACKNOWL`, `AMEND`, `CALLOFF` and `COPY` to `POAMEND`. `ISSUE` passes three gates — due diligence, payment terms, exact-content approval — and only then writes the `COMMITTED` movements. | PO-003, PO-004, PO-005, PO-006, PO-008 |
| `POAMEND` | internal (shares `POORDERCA.cpy`) | Supplier acknowledgements, buyer amendments, blanket call-off draw-down and order copying. Differences become `po.OrderAmendment` original/revised pairs, never silent overwrites. | PO-007, PO-008, PO-021, PO-023 |
| `BUDGCHK` | LINKable (`BUDGCHKCA.cpy`) | Commitment accounting and budget authority: `CHECK`, `COMMIT`, `RELEASE`, `OVERRIDE`. Posted actual plus open commitment plus the proposed value against the approved annual budget; warns at 90%, blocks at 100%, aggregates related requests inside the splitting window. | PO-005, PO-017, D-07, D-08 |
| `POAPPRV` | LINKable (`POAPPRVCA.cpy`) | Approval for the **exact** value, supplier, terms and demand: `HASH`, `REQUEST`, `DECIDE`, `CHECK`, `INVALID`. The content hash (eight 32-bit accumulators rendered as 64 hex characters) ties an approval to one exact order content, so a material change loses the old authority. | PO-006, PO-007, D-07 |
| `POSUPCHK` | LINKable (`POSUPCHKCA.cpy`) | Supplier due diligence: `CHECK`, `RECORD`, `EXCEPTION`, `ENQUIRE` over `po.SupplierDiligence`. A blocking sanctions result cannot be excepted by anybody; expired or missing mandatory evidence needs a named approver and a reason. | PO-026, D-29 |
| `PAYTERMC` | LINKable (`PAYTERMCCA.cpy`) | Payment terms against the company policy and the statutory maximum for the supplier's size, the agreed-terms snapshot and the invoice due date. | PO-027, D-25 |
| `TOLCHK` | LINKable (`TOLCHKCA.cpy`) | Over/under delivery, price, date and charge exceptions against the D-08 tolerances. Functions `QTY`, `PRICE`, `DATE`, `CHARGE`. | PO-011, PO-022, D-08 |
| `LANDCOST` | LINKable (`LANDCOSTCA.cpy`) | Landed-cost estimate, allocation and actual true-up. Functions `CALC` (no database effect), `ESTIMATE`, `ALLOCATE`, `ACTUAL`, `ENQUIRE`. | PO-015, PO-016, PO-025 |
| `RECEIPT` | LINKable (`RECEIPTCA.cpy`) | Goods inward confirmation. Records what arrived, when, and **who owns it**; calls `TOLCHK`, then the item suite's `STKMOVE`, then `POPOST` for the GRNI accrual. Ownership and not the gate decides the accounting date: an `INCOTERMS` receipt is taken into stock in transit against shipment evidence, a `DROPSHIP` line never becomes our stock. Functions `VALIDATE` (no database effect), `CONFIRM`, `TRANSIT`, `REVERSE`. | PO-009, PO-011, PO-014, PO-017, PO-018, PO-019 |
| `INSPECT` | LINKable (`INSPECTCA.cpy`) | Goods inward inspection and disposition of the rejected quantity (`RETURN`, `SCRAP`, `REWORK`, `CONCESSION`, `HOLD`). Quarantine is a quality condition of real stock, not a separate ledger. A concession must name its authority. Functions `RECORD`, `ENQUIRE`. | PO-010, PO-023 |
| `SVCACC` | LINKable (`SVCACCCA.cpy`) | Service and milestone acceptance with its accrual — acceptance, not the invoice, recognises the cost, and no stock quantity is ever created. Bases `QUANTITY`, `MILESTONE`, `VALUE`. Functions `CALC` (no database effect), `ACCEPT`, `REVERSE`, `ENQUIRE`. | PO-012, PO-017 |
| `SUPPRET` | LINKable (`SUPPRETCA.cpy`) | Return of goods to the supplier with an **expected** credit. Before the invoice the return clears GRNI; after it, a return clearing balance the credit note settles. A return never automatically reopens the order. Functions `CREATE`, `DESPATCH`, `CREDIT`, `ENQUIRE`. | PO-013, PO-017, PO-020 |
| `IMPVAT` | LINKable (`IMPVATCA.cpy`) | Customs import entry: duty capitalised into the cost of the goods (with the share on units already sold going to cost of sales), postponed import VAT posted on both sides of one journal. Functions `CALC` (no database effect), `DECLARE`, `POST`, `RECONCILE`. | PO-025, PO-015, PO-017 |
| `POCLOSE` | LINKable (`POCLOSECA.cpy`) | Cancellation, short close, closure and reopening of an order line. Releases the remaining commitment, never clears GRNI, and leaves expected supplier credits visible. Functions `CHECK` (no database effect), `CANCEL`, `SHORTCLS`, `CLOSE`, `REOPEN`. | PO-020, PO-017, PO-013 |
| `POPIPE` | batch report (RPT-009) | Purchasing pipeline and GRNI. Every open order line with its ordered, cancelled, received, invoiced and outstanding quantities beside the commitment still to come, the GRNI already created and the invoiced value. The three stages are printed side by side and never added together for the same portion. 132 columns through `STRPTHDR`, control break by supplier, spool to `run\spool`. | PO-017, PO-020, PO-022, RPT-009 |
| `OVERDUE` | batch report | Overdue purchase orders and delivery exceptions, broken on the **owning buyer** — an exception with nobody's name on it gets ignored, so a line with no buyer is listed under `(unassigned)` and that is itself an exception. Lateness is measured against the original *and* the current promise and both are printed, so re-dating a late line moves only the current figure. Each line carries the last amendment note and the sales order or job that is waiting for it. Flags `NOACK`, `REDATED`, `RESCHED`, `LATE`. Arguments `CO SITE SUPP BUYER DAYS ASAT USER`. | PO-021, PO-022 |
| `SUPPPERF` | batch report + measure store | Supplier performance. On time, in full, OTIF, quality acceptance, price variance and lead time are kept as five separate measures and never blended into one score. D-19 fixes the cohort: supplier on time is the **original** confirmed date, with a delivery up to five *working* days early (the `po.ToleranceRule` early window, resolved through `DATECALC ADDBUS`) still counting as on time. The same figures are produced on the current-promise basis as a second row so that re-dating is visible; the two bases are stored separately in `po.SupplierPerformance` and never averaged. A measure with no denominator prints `n/a` and is stored with a zero receipt count — never as 0% or 100%. `MODE=PREVIEW` reports without storing. Arguments `CO SUPP FROM TO MODE USER`. | PO-023, D-19 |
| `PODOC` | batch document | The purchase order document that goes to the supplier, plus the status appendix that keeps a **closed** order explainable: receipts, returns, import entries, landed cost and every amendment. What prints is the approved version — the content hash, approver and approval timestamp are on the face of the document, and an amendment still waiting for re-approval turns it into a marked draft rather than quietly printing changed text under an old approval. A reprint is never passed off as an original: earlier successful runs for the same order are counted in `core.JobRun` and the document is headed `*** REPRINT No n - first issued dd/mm/yyyy ***`. Arguments `CO ORDER USER`. | PO-024 |
| `REQGEN` | batch generator | Replenishment and shortage requisition generation. Finds items where free stock plus goods already on order has fallen to the site re-order point (`REPLEN`), or where more is reserved than is physically held (`SHORTAGE`), orders up to the maximum, applies the minimum order quantity and the purchase multiple, and raises each proposal **through `POREQ`** — never by writing `po.Requisition` directly. It writes nothing to `po.PurchaseOrder` and nothing to `po.Commitment`: a requisition is a justified need, not a purchase. `MODE=PREVIEW` (the default, and anything that is not exactly `COMMIT`) proposes only. Items that already have an open requisition line are skipped, which is what makes the run repeatable. Arguments `CO SITE ITEM MODE USER`. | PO-001 |

| `PODEMO` | batch demonstration driver | Builds the purchasing demonstration transactions for one company by **driving the online services**, never by inserting rows: `POREQ` (CREATE/ADDLINE/SUBMIT/APPROVE/CONVERT), `SOURCSEL` (OPEN/QUOTE/RANK/SELECT), `POORDER` (CREATE/ADDLINE/SCHEDULE/ISSUE/ACKNOWL/AMEND/CALLOFF), `POAPPRV`, `RECEIPT` (twice on one line for a partial delivery), `INSPECT`, `SUPPRET`, `SVCACC`, `IMPVAT` and `POCLOSE`. Thirteen scenarios leave orders in `DRAFT`, `AWAITAPPROVAL`, `APPROVED`, `ISSUED`, `ACKNOWLEDGED`, `AMENDED`, `CLOSED` and `CANCELLED`, plus blanket/call-off, service and subcontract orders. **Idempotency:** every document carries a correlation id `PODEMO-<CO>-<nnn>`; a second run finds its own orders (`CorrelationId LIKE 'PODEMO-<CO>-%'`) and builds nothing new, so the job is safe to repeat. Each service call gets its own correlation id because `NUMGEN` replays an already-issued number for a repeated one (COM-015). `MODE=PREVIEW` reports what it would build. Spool `run\spool\PODEMO-<CO>-<stamp>.lst`. `RUN AS: .\bin\x86\PODEMO.exe CO=STC MODE=COMMIT USER=BUYER1` (from the application root). To rebuild from nothing, clear the demonstration data first with `sqlcmd -S . -d STERP -E -C -i run\podemo-reset.sql`. | PO-001..PO-027 |

### Rules worth knowing

* **Issue is the only commitment.** `POORDER CREATE` leaves the order `DRAFT`. A printed draft is
  never represented as an authorised commitment (PO-006). `ISSUE` refuses unless `POSUPCHK` says
  the supplier may be traded with, `PAYTERMC` says the terms are inside the statutory maximum, and
  `POAPPRV CHECK` finds an `APPROVED` row for the hash of the order **as it stands right now**.
* **Budget (D-08).** Warn at 90%, block at 100%, measured *after* the proposed purchase — otherwise
  the order that breaks the budget is always the one that passes. The commitment sum counts only
  `COMMITTED` and `RELEASED`/`CANCELLED` movements: `ACCRUED` and `INVOICED` are the same money
  moving on and are already in the nominal ledger. Related requests from the same requester and
  cost centre inside a 30-day window are aggregated, so three £4,000 requisitions cannot walk under
  a threshold that one £12,000 requisition would hit.
* **Payment terms (PO-027, D-25).** Two limits, and they are not the same thing. Longer than the
  company policy (baseline 30 days) warns and is reported in payment practices data; longer than the
  statutory maximum (baseline 60 days from receipt of a valid invoice, tighter for a small supplier)
  is a breach that needs an authorised exception — an approver **and** a reason, not one or the other.
* **Due diligence (PO-026, D-29).** Evidence is never overwritten: each check is a new row with its
  own `CheckedAt`, so what we knew and when we knew it can always be reconstructed. The mandatory
  baseline is sanctions, Companies House, VAT number and business size — business size is in the set
  because the statutory payment-terms maximum depends on it.
* **Tolerances (D-08).** Over delivery is 0% by default with an item/supplier override capped at

  5%; price variance must fail **both** 2% and £50 before it blocks; charge variance £25; more than
  five *working* days early is flagged (counted through `DATECALC` `ADDBUS` on the site calendar).
  Quantities follow section 6 of the spec:
  `outstanding = max(0, ordered - cancelled - counted)` and
  `overdelivery = max(0, counted - (ordered - cancelled))`, so an accepted tolerance leaves zero
  outstanding **and** a separately visible positive over-delivery — never a negative commitment and
  never a hidden excess.
* **Landed cost.** Only charges necessary to bring the goods to their present location and
  condition are capitalised (FRS 102 s13); `OTHER` is expensed. Recoverable VAT is never landed
  cost. A missing or zero aggregate basis **blocks** allocation. Allocation goes through `RNDALLOC`
  so the parts always add back to the approved charge. The actual true-up allocates only
  *actual minus estimate*, so an actual freight invoice cannot capitalise both. A share attributable
  to units already sold or consumed goes to cost of sales, not back into stock.
* **Pricing.** The pricing date of an issued order is its order date, so a later supplier price
  change cannot silently reprice it. `price per` means a price of 12.500000 per 100 EA is an
  effective unit price of 0.125000; the line is valued at six decimals and rounded once.

## Database objects added

`db\tables\80-po\05-SourcingPricing.sql` — `po.SupplierSource`, `po.PurchasePrice`,
`po.PurchasePriceBreak`, `po.SourcingEvent`, `po.SourcingQuote`.

`db\tables\80-po\06-CommitmentControl.sql` — `po.Commitment` (one row per **stage movement**, so
commitment, accrual and payable are never added together for the same portion), `po.BlanketBalance`,
`po.OrderAmendment` (original versus revised), `po.ToleranceRule` (D-08 policy),
`po.SupplierDiligence` (D-29), `po.SupplierPerformance` (original and current promise bases), plus
`LandedEstimateGBP`/`NonRecoverableVatGBP` on `po.PurchaseOrderLine` and `SubcontractJobRef` on
`po.Receipt`.

The pre-existing tables (`po.Requisition`, `po.PurchaseOrder`, `po.PurchaseOrderLine`,
`po.PurchaseSchedule`, `po.Receipt`, `po.ReceiptLine`, `po.ServiceAcceptance`,
`po.SupplierReturn`, `po.ImportEntry`) are used as they stand.

`db\procs\po\usp_PoEnquiry.sql` — `po.vw_CommitmentBalance` (the one supported read path over the
append-only `po.Commitment` movements), `po.usp_OpenOrders` (open order enquiry with outstanding
quantity, outstanding value and received-not-invoiced), `po.usp_CommitmentEnquiry` (commitment,
accrual and payable side by side, grouped by order, budget or supplier) and
`po.usp_OverdueSchedules` (late schedules measured against **both** the original and the current
promised date, with the value at risk).

## Tests

| Suite | Assertions | Covers |
|---|---|---|
| `cobol\tests\po\TTOLCHK.cob` | 25 | AC-053 (order 30, receive 20, ten outstanding; receipt exactly at the approved limit accepted with the excess separately visible; one unit above blocks with `NEEDS-AMEND`), D-08 price tests (5% on 200 units = £100 blocks; the same 5% on 50 units = £25 only warns), charge variance £20 within / £30 above, 5% override cap. |
| `cobol\tests\po\TLANDCOS.cob` | 19 | AC-056 (£600/£400 groups and a £75 freight charge give **£45.00 and £30.00**, totalling £75.00 with no residual), zero aggregate basis blocks, actual-minus-estimate delta only (£90 actual on a £75 estimate allocates £15 → £9/£6), consumed units split to cost of sales, three-way uneven rounding still totals the charge, non-capitalisable category warns. |
| `cobol\tests\po\TPAYTERM.cob` | 29 | D-25 payment terms: 20 and 30 days within a 30-day policy; 45 days warns and becomes reportable with a 15-day excess; **60 days exactly at the statutory maximum only warns**; 75 days breaches with `PTC0004` and return code 06; an approver *and* a reason turn the breach into an authorised `EXCEPTED` that is still reported; an approver without a reason is still a breach (`PTC0005`); a 30-day small-supplier maximum bites at 45 days; the agreed-terms snapshot reads `30 days from receipt of a valid invoice`. |
| `cobol\tests\po\TPORCV.cob` | 49 | Receipt, acceptance, import and closure. **AC-053** (order 30, receive 20, ten outstanding, nothing over delivered, valued at the order price £100.00); a lot-traceable line without a lot number fails `RCP0004` and passes once the lot is recorded; a serial line of twenty units is refused and one unit with one serial passes; D-08 over delivery of 31 against 30 is **held** (RC 06) and with an agreed 5% tolerance passes leaving **one unit of visible excess and zero outstanding**; goods for inspection are quarantined, not accepted, and are **still valued and accrued**; `INCOTERMS` ownership without shipment evidence fails `RCP0009` and with a bill of lading becomes stock in transit needing no put-away location. **AC-054** (40% of £2,000.00 accrues **£800.00** and leaves **£1,200.00**; sixteen hours at £50.00 reaches the same £800.00; excess service billing is refused with the remaining £200.00 stated; missing evidence, unnamed milestone and self-acceptance all refused). **PO-025** (5% duty on £1,000.00 is **£50.00**; import VAT at 20% of value *plus* duty is **£210.00**; postponed VAT nets to **nil**; a 40% consumed share splits the duty £20.00/£30.00; a preference claim without proof of origin fails `IMP0009` and with an EUR1 gives zero duty and £200.00 VAT). **AC-055** (short close releases £50.00 of commitment and **retains £100.00 of GRNI**; an expected credit survives closure; cancelling a received line is refused in favour of a short close; cancel without a reason, short close without an authority and reopen without an authority are all refused). |

| `cobol\tests\po\TPOPRICE.cob` | 20 | PO-004 pricing against the database. The suite makes its own `ZZ-TEST-PRC-OLD` (125.00 per 100, 01/01/2026 - 30/06/2026) and `ZZ-TEST-PRC-CUR` (140.00 per 100 from 01/07/2026, breaks at 500 = 130.00 and 1000 = 120.00) agreements through SQLIO and deletes them again, so it is repeatable on a live database: **250 priced today is 350.00** on the current contract (price-per 100, basis `CONTRACT`, agreement named); **500 takes the first break (130.00, 650.00)**; **1200 takes the second (120.00, 1,440.00)**; **the same 250 priced on 01/03/2026 is 312.50** on the expired agreement; the `BREAKS` ladder returns both steps in quantity order; and **the price source is never returned blank** - the text names the agreement, and `HISTORY` reports a named basis (`LASTPAID` or `NONE`) with an explanation of its own. Without a database connection the suite skips. |
| `cobol\tests\po\TPOSOD.cob` | 9 | COM-003 and PO-006. The suite raises its own requisition through `POREQ` (`CREATE`/`ADDLINE`/`SUBMIT`) and deletes it again. **The requester may not approve their own requisition**: return code 08, reason `REQ0013`, outcome `REFUSED`, and the requisition is still `SUBMITTED` afterwards - both when the raiser is named in `RQ-APPROVER` and when only the signed-on `CA-USER` gives it away. An independent approver is accepted and the requisition becomes `APPROVED`. `POAPPRV` **folds a punctuated approval type** (`PURCHASE-ORDER`) onto the `PURCHASEORDER` the approval matrix is keyed on, and still defaults an unstated one. |

### .NET and SQL tests

| Suite | Assertions | Covers |
|---|---|---|
| `db\tests\po-enquiry.sql` | 7 | The commitment enquiry refuses a grouping other than ORDER/BUDGET/SUPPLIER (`STERP-E-0801`); `po.vw_CommitmentBalance` arithmetic from fixture movements rolled back afterwards - **committed 1,050.00 less released 400.00 less cancelled 100.00 is 550.00**, with invoiced 400.00 and accrued 200.00 reported *beside* it and never inside it; the budget grouping reports the same 550.00; the open order enquiry still carries `OutstandingQty`, `OutstandingGBP`, `ReceivedNotInvoicedGBP`, `CommitmentGBP`, `EarliestDueDate` and `IsCommitment`; overdue schedules still report both promise bases (D-09); no open order reports a negative outstanding quantity. |
| `dotnet\STERP.Tests\Purchasing\PurchasingTests.cs` | 7 | Screen field conversion (UK dates, `£` and thousands separators, a missing field is zero); the three buyer enquiries and their contract columns; `POPRICE` answers a price explanation with a basis and a price-per; `BUDGCHK` answers with one of the four outcomes the screen knows how to show; a receipt cannot be confirmed before it has been validated; validating a delivery lists the outstanding lines; the requisition facade carries the budget verdict and the sterling value of every line for the grid. |

## .NET client

| File | Purpose |
|---|---|
| `dotnet\STERP.Business\Purchasing\PoMap.vb` | Turns a screen's name/value dictionary into typed COMMAREA values (code, text, decimal, UK date) and builds the working grids. |
| `dotnet\STERP.Business\Purchasing\RequisitionService.vb` | `POREQ` CREATE / ADDLINE / SUBMIT / APPROVE / CANCEL / CONVERT / ENQUIRE and the `SOURCSEL` approved-source check. |
| `dotnet\STERP.Business\Purchasing\PurchaseOrderService.vb` | `POORDER` CREATE / ADDLINE / SCHEDULE / ISSUE / ACKNOWL / AMEND / CALLOFF / COPY, `POPRICE` PRICE and BREAKS, `BUDGCHK` CHECK, `POAPPRV` CHECK / REQUEST / DECIDE, `POSUPCHK`, `PAYTERMC` and the three enquiry procedures. |
| `dotnet\STERP.Business\Purchasing\GoodsReceiptService.vb` | `RECEIPT` VALIDATE / CONFIRM / TRANSIT / REVERSE and `INSPECT` RECORD. VALIDATE reads the order's outstanding lines into the COMMAREA (`RECEIPT` itself reads nothing), the screen writes the counted quantities back into the same buffer and CONFIRM posts it - the old pseudo-conversational habit. `RC-L-COUNTED` is what the line has already had, `RC-L-RECEIVED` is this delivery. |
| `dotnet\STERP.Desktop\Forms\Purchasing\frmRequisition.vb` | Requisition capture with the budget verdict on the status line, and **Check this source** - `SOURCSEL` answers for a *named* supplier, so an unapproved, suspended or out-of-date source comes back as its own refusal. F5 opens an existing requisition by number (or by the menu row's `FormArgument`). |
| `dotnet\STERP.Desktop\Forms\Purchasing\frmPurchaseOrder.vb` | Order entry and approval: lines grid, **Explain price** (basis, break used, source text), **Check budget** (percentage used *after* the purchase, warn/block), supplier due diligence and payment terms policy, approval request/decision, and an amendment that reports when the approval has been invalidated. |
| `dotnet\STERP.Desktop\Forms\Purchasing\frmGoodsReceipt.vb` | Goods receipt and inspection: validate against the order, count each line, F10 confirms (accepted / quarantined / held, GRNI, price variance, journal), inspection disposition and receipt reversal. |

The forms are built in code (no designer files) with `UiStyle`, as the other modules do. They hold
no rules: every verdict on the status line is the one the COBOL engine returned.

```
build\build-cobol.ps1 -Module po -NoDll
bin\tests\TTOLCHK.exe      # 25/25
bin\tests\TLANDCOS.exe     # 19/19
bin\tests\TPAYTERM.exe     # 29/29
bin\tests\TPORCV.exe       # 49/49
bin\tests\TPOPRICE.exe     # 16/16  (needs the database)
build\build-db.ps1 -Only "80-po"
build\build-db.ps1 -Only "80-po-eventmap"
build\build-db.ps1 -Only "usp_PoEnquiry"
build\build-db.ps1 -NoDeploy -Tests -Only "po-"     # db\tests\po-enquiry.sql  7/7
build\build-net.ps1
build\run-tests.ps1 -Layer net -Filter Purchasing   # 7/7
```

## How to demo (once the remaining work lands)

1. Deploy `db\seed\80-po-eventmap.sql` so the receipt, return, inspection scrap, landed-cost and
   import events have account mappings and `po.ToleranceRule` has its D-08 baseline row, then
   `db\seed\81-po-menu.sql` for the menu, the five report definitions and the generic maintenance
   screens, then `db\seed\92-po-demo.sql` for the demonstration reference data.
2. Run `REQGEN` to raise replenishment requisitions, convert one through `POORDER`, price it with
   `POPRICE`, budget-check it with `BUDGCHK` and approve it with `POAPPRV`.
3. Receive it with `RECEIPT` (GRNI posts through `POPOST`), inspect with `INSPECT`, then allocate a
   freight charge with `LANDCOST`; declare the import with `IMPVAT` and accept a service with
   `SVCACC`.
4. Send two units back with `SUPPRET` and short close the balance with `POCLOSE`; the GRNI for what
   was kept stays on the ledger and the expected credit stays chased.
5. Report with `POPIPE` (RPT-009), `OVERDUE`, `SUPPPERF`, and print one order with `PODOC` twice —
   the second copy is headed as a reprint.

### Running the batch programs

Run them from the application root, not from `bin\x86`; the spool path is relative to the root.

```
.\bin\x86\REQGEN.exe   CO=STC MODE=PREVIEW USER=BUYER1
.\bin\x86\POPIPE.exe   CO=STC USER=BUYER1
.\bin\x86\OVERDUE.exe  CO=STC DAYS=1 USER=BUYER1
.\bin\x86\SUPPPERF.exe CO=STC FROM=20260401 TO=20270331 MODE=COMMIT USER=BUYER1
.\bin\x86\PODOC.exe    CO=STC ORDER=<order number> USER=BUYER1
```

Each one prints `STERP-I-SPOOL <path>` as its last line and registers a `core.JobRun` row.

### Demonstration data

`db\seed\92-po-demo.sql` is deliberately only the **reference** half of the demonstration data:
approved supplier sources (including an unapproved third source on one item in seven, so that
`SOURCSEL` has something to refuse), purchase prices with quantity breaks and a superseded
contract price, due-diligence evidence with one lapsed insurance certificate per company, a
supplier-scoped tolerance override, and re-order points so `REQGEN` finds demand. It is data
driven and re-runnable.

The **transactions** are not seeded as rows. A receipt has to move stock and post to the nominal
ledger, and only `RECEIPT` does both consistently; inserting `po.Receipt` rows from a script would
produce a database that reads correctly on the purchasing screens and is wrong in the ledger. The
transactional half is therefore produced by driving the services (see the `po-demo-txn` sub-todo).

## Verification sweep, 2026-10-07 (po-verify)

Requirement tags: `build\trace-gen.ps1` reports 316 of 316 requirements tagged in implementation
code, PO-001..PO-027 included. PO-013, PO-018, PO-019 and PO-023 were tagged in implementation
code only; they are now test-tagged too (sub-todo `po-test-tags`, see below).

## Unit tests for PO-013, PO-018, PO-019 and PO-023, 2026-10-07 (po-test-tags)

Three new COBOL drivers, each building and removing its own `ZZ-TEST` fixtures so that they may be
re-run against a live database, and each skipping cleanly when the database is not deployed:

| Driver | Covers | Notes |
|---|---|---|
| `cobol\tests\po\TPOSRET.cob` | PO-013, PO-017, PO-020, AC-055 | `SUPPRET` `CREATE`, `CREDIT` and `ENQUIRE`: the credit is expected and chased rather than taken, GRNI before the invoice and a return clearing balance after it, a return never reopens the order by itself, a credit note that disagrees raises a variance instead of settling, and two returns that draw their own number get two references (and therefore two posting correlations). `DESPATCH` is deliberately not exercised - it moves stock and posts a journal, and a unit test has no business leaving ledger entries behind |
| `cobol\tests\po\TPODROP.cob` | PO-018, PO-019, PO-009 | `RECEIPT` `VALIDATE` (no database effect) and `CONFIRM`. A drop-ship line needs no put-away location where an owned line still does (`RCP0005`), is still valued and still reduces the outstanding quantity, and on confirmation writes `OwnershipBasis = 'DROPSHIP'` with **no** stock movement and `IsStockInTransit = 0`. A subcontract receipt carries `RC-SUBCONTRACT-JOB` through to `po.Receipt.SubcontractJobRef`, and a drop-ship receipt is shown not to be one. Both confirmations are made at nil line price so that the GRNI accrual is not raised and no journal is left behind |
| `cobol\tests\po\TPOPERF.cob` | PO-023, D-19, RPT-008 | `SUPPPERF` is a batch EXE ending in `STOP RUN`, so it is spawned (`SQLIO` `ROLB` + `DISC` first, then `Start-Process` under a sixty-second `WaitForExit` watchdog that kills the job and returns 99). Run for the whole supplier book and for one supplier, on the parameters its `core.ReportDef` row advertises; asserts return code 0, the `STERP-I-SPOOL` line and the listing on disk, and that `MODE=PREVIEW` left the `po.SupplierPerformance` row count exactly where it found it |

Defects found by those tests and fixed at source:

| File | Defect | Fix |
|---|---|---|
| `cobol\online\po\SUPPRET.cob` | `1000-INIT` builds the posting correlation from `SR-RETURN-REF`, but `1100-NUMBER` allocates that reference afterwards. Every return that drew its own number therefore shared the one correlation `SRT-<CO>-`, so `POPOST` read the second return's posting as a replay of the first and dropped it. Correlation ids must be unique per idempotent call | Rebuild the correlation in `1100-NUMBER` once the reference is known, exactly as `RECEIPT.cob` already does in `1050-CORRELATION`; a caller-supplied `CA-CORRELATION-ID` still wins |
| `cobol\online\po\SUPPRET.cob` | `4000-CREDIT` wrote `Status = 'DESPATCHED'` whenever the credit note disagreed with the expected credit, even for a return whose goods had never left - a statement about a physical despatch that never happened, which also pulled an open return into the despatched cohort | Read the current `Status` alongside the expected credit and leave it untouched on a mismatch; only the credit note reference, `ModifiedBy` and `ModifiedAt` are updated. `SRT0009` and return code 4 are unchanged |

Defects found and fixed in the earlier verification sweep:

| File | Defect | Fix |
|---|---|---|
| `cobol\online\po\RECEIPT.cob` | `3100-WRITE-HEAD` leaves `SQLIO-PARM-NULL` set on its own blank parameters (5..14). `3300-WRITE-LINE` never cleared them, so a blank delivery note nulled `QuarantineQty` and every `po.ReceiptLine` INSERT failed - silently, because the result was not checked. `po.ReceiptLine` was empty, which starved SUPPPERF, INSPECT and SUPPRET | Clear all 40 null flags first, then check `SQLIO-SQLCODE` and reject with `RCP0011` |
| `run\podemo-reset.sql` | Deleted `inv.StockMovement` but not the `core.IdempotencyKey` rows, so every later PODEMO run replayed as a duplicate and posted no stock at all; the goods-in balances from the first run were left behind | Also delete `core.IdempotencyKey`/`inv.Lot`/`inv.StockBalance` rows the run created, and any goods-in balance with no backing movement |
| `cobol\batch\po\PODEMO.cob` | The supplier return always asked for `ACCEPTED` stock at the goods-in location, which the CAL demonstration item is not in until it has been inspected (`DESPATCH SRT0005`) | `7600` remembers the condition and location the goods actually landed in; `7700-INSPECT` moves it back to `ACCEPTED`/goods-in once the inspection releases the good part; `3100-RETURN-SOME` returns from whichever it is |

`SUPPPERF CO=STC MODE=COMMIT` and `CO=CAL` now return RC 0 and store 2 measure rows each
(ORIGINAL and CURRENT basis) from the PODEMO receipt lines. `PODEMO CO=STC` and `CO=CAL` both
return RC 0 with 81 service calls, 67 accepted, 14 expected warnings and 0 refused.

Tests: COBOL `TPO|TBUDG|TLAND|TPAY|TTOL|TSTK|TLOT|TAVAIL` 298 passed 0 failed; SQL 247 passed
0 failed; .NET `-Filter po` 38 passed 0 failed; `build\demo-data.ps1` ends RECONCTL RC 0.

## Acceptance defect sweep, 2026-10-07 (po-acc-fix)

Defects 1-3 found by the acceptance test layer (`dotnet\STERP.Tests\Acceptance`) fixed at the
source.

| File | Defect | Fix |
|---|---|---|
| `cobol\online\po\POREQ.cob` | `5000-APPROVE` took the approver from the COMMAREA and never looked at who raised the requisition, so the requester could approve their own demand (COM-003, AC-098) | The stored `RequesterUser` is always used, and new `5100-SOD` refuses an `APPROVE` whose named approver *or* signed-on user is the requester: RC 08, reason `REQ0013`, outcome `REFUSED`, status unchanged. A `REJECT` is not caught - withdrawing your own request is not a control failure |
| `cobol\online\po\POPRICE.cob` | The price basis could come back blank, and `HISTORY` filled `CA-MESSAGE` but not `PR-SOURCE-TEXT` (PO-004, AC-051) | An unstated basis reads `MANUAL`, `HISTORY` reports `LASTPAID` with its own source text, and "no price"/"no history" report `NONE` with the reason |
| `dotnet\STERP.Business\Purchasing\PurchaseOrderService.vb` | `Explain` linked to POPRICE but never read the answer back, so `PriceSource` stayed empty; `Approval` passed approval type `PURCHASE-ORDER` while `core.ApprovalRule` is keyed on `PURCHASEORDER`, so no rule matched, no approval row was created and a new order could never be issued (`ORD0012`) | New `ReadPrice` fills `PriceSource`/`PriceSourceText` after `Explain`, `Breaks` and the new `LastPaid`; the approval type is spelt `PURCHASEORDER` |
| `cobol\online\po\POAPPRV.cob` | A caller's punctuated approval type missed every rule in the approval matrix | New `1100-FOLD-TYPE` upper-cases the type and folds out `-`, `_`, `.` and spaces before it is used anywhere |
| `dotnet\STERP.Business\Purchasing\GoodsReceiptService.vb` | An order line with no destination location left the receipt with no put-away location, so `RECEIPT VALIDATE` refused every stocked line (`RCP0005`); a lot-traceable line could never be validated before it was counted | The outstanding-line query falls back to the site's goods-in location (then any stock location); `LoadLines` accepts `LOT`/`SERIAL` for a caller that already knows them |

Two findings were left open by this sweep and fixed in the next one (below): a receipt confirmed
against a brand new order answered `RCP0011`/`POP0006` because `fin.usp_PostJournal` returned no
row, and `BUDGCHK COMMIT` wrote no `COMMITTED` row to `po.Commitment` because `POORDER
5350-ONE-LINE` only treated a return code of 12 or more as fatal.

Tests: COBOL `TPO|TBUDG|TLAND|TPAY|TTOL` 261 passed 0 failed (new `TPOSOD` 9, `TPOPRICE` 16 to
20); .NET `-Filter po` 46 passed 0 failed; .NET `-Filter acc:` 39 passed 1 failed (the failure is
the `so` SORESV defect); `PODEMO CO=STC MODE=COMMIT` RC 0.

## GRNI posting and commitment defects, 2026-10-07 (po-acc-fix-2)

The two findings left open by `po-acc-fix`, both fixed at the source.

| File | Defect | Fix |
|---|---|---|
| `cobol\online\po\RECEIPT.cob` | A receipt confirmed against a brand new order answered `RCP0011`/`POP0006` *"The nominal ledger returned no posting result"*. `CA-CORRELATION-ID` is `PIC X(36)` and the client sends a 36 character GUID, so the per line stock movement id `<corr>-L<n>` truncated straight back to `<corr>`. `inv.usp_PostStockMovement` and `fin.usp_PostJournal` share `core.IdempotencyKey`, so the movement completed the key first and the GRNI posting looked like a replay of it - `fin.usp_PostJournal` returned the (non existent) journal of that key and POPOST saw no row. Every line of a multi line receipt also shared one movement key | New `1050-CORRELATION` derives three ids from the receipt's own correlation: the receipt keeps `WS-CORR`, the stock movements use `WS-CORR-SHORT` (30 characters) plus `-L<n>` so the suffix always survives, and the GRNI journal uses `WS-POST-CORR` = stem + `-GRNI`. `3600-STAMP` looks the journal up under the posting id. A caller that supplies no correlation id now gets one rebuilt around the GRN number after `1100-NUMBER` has drawn it, instead of the bare `RCP-<company>-` stem |
| `cobol\online\po\BUDGCHK.cob`, `cobol\copy\BUDGCHKCA.cpy` | `po.Commitment` held no `COMMITTED` row anywhere. An order line with no budget code and no cost centre was refused by `1000-INIT` with RC 08, and a coded line whose budget does not exist was refused by `2000-CHECK` with RC 16, so `COMMIT` never reached `5000-MOVEMENT` | An encumbrance is a fact, not a permission (PO-005, PO-017). `COMMIT` and `RELEASE` treat "nothing to measure against" as the new outcome `NOBUDGET`, RC 04, reason `BUD0008`: the movement is written and reported as unbudgeted spend. `CHECK` and `OVERRIDE` are unchanged, so `BUD0001`/`BUD0002` still refuse a check with nothing to check. A real budget that would be broken still blocks and still writes nothing |
| `cobol\online\po\POORDER.cob` | `5350-ONE-LINE` only treated a budget return code of 12 or more as fatal, so a blocked budget was silently ignored and the order issued anyway | Any return code of 6 or more now holds the issue with the budget's own reason code and message, and `OR-OUTCOME` is `HELD` |

Tests: `cobol\tests\po\TBUDGCHK.cob` gains `2000-COMMIT-CODED`, `2100-COMMIT-UNCODED` and
`2200-COMMIT-BLOCKED` (10 assertions: the coded commit returns its `po.Commitment` row, the uncoded
commit warns `BUD0008`/`NOBUDGET` and is still recorded and releasable, and a blocked purchase
encumbers nothing). `cobol\tests\po\TPORCV.cob` gains `1600-RCV-CORR` (3 assertions: a caller's
36 character id is left alone and accepted, and a derived id names the receipt).
`ProcureToPayTests.ProcureToPayChain` now asserts the `COMMITTED` movement and its value, that the
receipt confirms, that exactly one balanced `PO.RECEIPT` journal is posted with stock debited and
`2010` credited, and that `po.Receipt.JournalId`/`GrniValueGBP` agree with it.

COBOL `TPO|TBUDG|TLAND|TPAY|TTOL` 274 passed 0 failed; .NET `-Filter po` 46 passed 0 failed;
`-Filter "acc:"` 39 passed 1 failed (`OrderToCashChain`, defect 6, owned by `so`).
`build\demo-data.ps1` leaves `RECONCTL` at RC 0 with the GRNI family **AGREED** at 0.00.
