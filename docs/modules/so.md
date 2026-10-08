# Sales order management (`so`)

Module code `so`, requirement family **SO-001..SO-029**, specification
[05-Sales-Order-Management.md](https://github.com/stefanatanasiu/STERP-Supply-Chain-Functional-Specification/blob/main/05-Sales-Order-Management.md),
acceptance scenarios AC-039..AC-049, decisions **D-03, D-11, D-12, D-14, D-21, D-23**.

COBOL dialect: see `cobol\DIALECTS.cfg`. Schema `so`, with `inv` for stock and reservations,
`ar` for the customer and the sales ledger, `fin` for the nominal ledger and `core` for numbering,
permissions, approvals and parameters.

The engine is COBOL. Prices, discounts, VAT, margins, acceptance blockers, reservations, picking,
despatch, billing, returns and rebates are all decided by the programs below; the database layer
holds the data and answers enquiries, and the .NET layer is a thin terminal over both.

## Programs

| Program | Kind | Purpose | Requirements |
|---|---|---|---|
| `PRICING` | LINKable (`PRICINGCA.cpy`) | Deterministic effective-dated price selection: manual (1), contract (2), customer (3), group (4), standard (5), with quantity breaks and price-per units. `SELECT` returns the price, `EXPLAIN` returns the candidate list and why each was or was not taken. Two live candidates of equal standing are refused, never guessed. | SO-004 |
| `SODISC` | LINKable (`SODISCCA.cpy`) | Discount, allowance and charge sequence at line and document level, allocated with `RNDALLOC` so the parts always add back to the whole. | SO-005 |
| `SOVAT` | LINKable (`SOVATCA.cpy`) | UK VAT liability per line: standard, zero, exempt, outside scope, export with evidence, reverse charge. | SO-025, SO-026, D-23 |
| `SOMARGN` | LINKable (`SOMARGNCA.cpy`) | Margin and markup against the captured cost basis, and the below-policy approval request. | SO-006 |
| `SOQUOTE` | LINKable (`SOQUOTECA.cpy`) | Quotation lifecycle: issue, revise, accept, decline, expire, convert in full or in part; templates, copies and blanket call-off limits. | SO-001, SO-024 |
| `SOACCEPT` | LINKable (`SOACCEPTCA.cpy`) | Order acceptance: validates the draft and names **every** blocker rather than the first, then accepts, holds or releases. | SO-002, SO-003, SO-007, SO-011 |
| `SOCREDIT` | LINKable | Credit status and the credit decision behind a hold or a release. | SO-007 |
| `SOSCHED` | LINKable (`SOSCHEDCA.cpy`) | Delivery schedules: requested, original promise and current promise held side by side so a re-promise cannot improve the service measure. | SO-008 |
| `SOAMEND` | LINKable (`SOAMENDCA.cpy`) | Amendments and cancellations; invalidates approvals through `core.usp_InvalidateApprovals` when the content changes. | SO-015, SO-016 |
| `SOAVAIL` | LINKable (`SOAVAILCA.cpy`) | Availability and promise enquiry, keeping free stock, reservations and expected receipts apart. | SO-010 |
| `SORESV` | LINKable (`SORESVCA.cpy`) | Reservation and allocation through `inv.usp_ReserveStock`, where the free-quantity race is settled once under `UPDLOCK, HOLDLOCK`. | SO-009 |
| `PICKGEN` | batch/online | Pick list generation, short picks and pick confirmation. | SO-012 |
| `DESPATCH` | LINKable | Despatch confirmation — the revenue and control event. Reduces stock, recognises COGS once through `fin.JournalStage` / `fin.usp_PostJournal` under the correlation id. | SO-013, D-03 |
| `KITSOAL` | LINKable | Kit revenue and VAT allocation on standalone selling prices; no partial kit despatch. | SO-020, SO-028, D-11 |
| `BACKORD` | LINKable | Back orders and short-supply handling. | SO-015, SO-019 |
| `RETURNS` | LINKable | Return authorisation, receipt, inspection and disposition. | SO-017, SO-018 |
| `EXPEVID` | LINKable | Export evidence: proof type, deadline and the overdue chase. | SO-026 |
| `SOROT` | LINKable (`SOROTCA.cpy`) | Retention of title list. | SO-029 |
| `SOB2B` | LINKable (`SOB2BCA.cpy`) | Back-to-back and drop-shipment supply, including the drop-ship COGS posting. | SO-021 |
| `SODOCVR` | LINKable (`SODOCVRCA.cpy`) | Document versions and reprints; a reprint is evidence, never a release. | SO-022 |
| `SOINVRUN` | batch | Invoice run over confirmed, unbilled despatches (`PREVIEW` / `COMMIT`), billing through the sales ledger. | SO-014 |
| `REBATE` | batch | Retrospective rebate accrual as a reduction of revenue. | SO-027, D-12 |
| `SOBACKLG` | batch report | Sales backlog and fulfilment (RPT-008). | RPT-008, SO-022 |
| `SOANAL` | batch report | Sales analysis by customer, item, salesperson and period with margin. | SO-023 |
| `SOACKDOC` | batch | Acknowledgement, pick note and despatch note documents. | SO-011, SO-013 |
| `SODEMO` | batch | Demonstration driver: quote → order → acceptance and credit decision → delivery promise → reservation → pick → despatch → invoice run → customer return, for every company that has sales orders. Re-runnable; picked up automatically by `build\demo-data.ps1` (`bin\x86\*DEMO.exe`). | SO-001, SO-013, SO-014 |

COBOL unit tests live in `cobol\tests\so` (`TSOPRICE`, `TSODISC`, `TSOVAT`, `TSOACCPT`, `TSOSCHED`,
`TSORESV`, `TSODESP`, `TSOBKORD`, `TSOKIT`, `TSORETRN`, `TSOEXPEV`, `TSOINVRN`, `TSOREBAT`, `TSOB2B`,
`TSOQUOTE`, `TSOBATCH`). `TSOBATCH` is the batch smoke suite: it spawns `SOBACKLG`, `SOANAL`,
`SOINVRUN` (PREVIEW), `SOACKDOC` and `REBATE` (PREVIEW) on the parameters their `core.ReportDef`
rows advertise and asserts return code 0, a `STERP-I-SPOOL` line and the listing on disk. The jobs
run under a 60-second watchdog, so a job that does not terminate fails the suite instead of
hanging the test run. (All five once slept after writing their listing instead of returning from
`STOP RUN`; that no longer reproduces, and the watchdog now guards against it coming back.)

## Database

Tables `db\tables\70-so\` — price lists, quotes, orders and schedules, picking, despatch, returns,
rebates and document versions (see `docs\DATA-MODEL.md`).

| Object | Purpose |
|---|---|
| `so.vw_SalesOrderLineProgress` | Line-level accepted / cancelled / despatched / billed / returned, outstanding and unbilled quantities, net unit price and the promise triplet. |
| `so.vw_OpenOrderBook` | The order book: open lines that still owe goods, valued in document currency and in GBP, flagged late and re-promised. |
| `so.vw_SalesOrderStatus` | Header enquiry with the customer name, outstanding value, earliest promise and a readable `Blocker`. |
| `so.vw_DespatchUninvoiced` | The invoice run queue: confirmed despatch lines whose billed quantity has not caught up. |
| `so.vw_RebateStatus` | Accrued less settled rebate per agreement, with the threshold test. |
| `so.vw_PriceListEffective` | Price list lines flattened with level, scope and net unit price. |
| `so.usp_SalesOrderEnquiry` | Four result sets — header, lines, schedules, despatches — for one order or a customer's orders. Filters `ALL/OPEN/HELD/UNBILLED/CANCELLED`. |
| `so.usp_SalesBacklog` | Backlog aggregation by `ORDER`, `CUSTOMER`, `ITEM`, `SITE` or `PERIOD`; the arithmetic behind RPT-008. |
| `so.usp_PriceEnquiry` | The candidate list behind F2 and the price explanation panel: every list that could apply, which one applies, and which set-ups are ambiguous. It explains, it never decides. |

Reservation concurrency is **not** re-implemented here: `inv.usp_ReserveStock` already settles the
free-quantity race under `UPDLOCK, HOLDLOCK`, and `SORESV` calls it.

Errors raised by the sales procedures: `STERP-E-1901` (company missing), `1902` (unknown status
filter), `1903` (not permitted), `1904` (unknown backlog grouping), `1905` (item missing).
The COBOL programs use `STERP-E-14xx`..`18xx`.

Seeds: `70-so-pricing.sql` (price lists, breaks and rebate agreements), `71-so-eventmap.sql`
(`SO.DESPATCH.COGS`, `SO.REBATE.ACCRUAL`, `SO.DROPSHIP.COGS`), `72-so-menu.sql` (permissions, menu,
`core.ReportDef`, returns maintenance), `73-so-maintdef.sql` (price list, price break and rebate
maintenance with their permissions and menu entries), `93-so-demo.sql` (demonstration quotes and
orders). SQL tests: `db\tests\so-pricing.sql`, `db\tests\so-orderbook.sql` (21 assertions).

## .NET

`dotnet\STERP.Business\Sales\SalesOrderService.vb` wraps the COBOL programs through
`CobolGateway`/`CommArea`; no sales rule is re-implemented in VB.

* `frmSalesOrderEntry` — header and lines grid, F2 availability, price explanation from
  `PRICING EXPLAIN`, release/fulfilment/billing state and hold reason in words, F10 accept and
  release listing every blocker from `SOACCEPT VALIDATE`.
* `frmDespatchConfirm` — pick list, confirm despatch through `DESPATCH`.
* `frmSalesInvoiceRun` — `SOINVRUN` in `PREVIEW` or `COMMIT`.
* Generic maintenance: `SO.RETURN`, `SO.PRICELIST`, `SO.PRICELINE`, `SO.REBATE`.
* Harness tests in `dotnet\STERP.Tests\Sales\SalesTests.cs`.

## Flows

1. **Quote to order** — `SOQUOTE ISSUE` → customer accepts → `SOQUOTE CONVERT` → `SOACCEPT
   VALIDATE` (blockers) → `SOACCEPT ACCEPT` → credit decision (`SOCREDIT`) → `RELEASE`.
2. **Price** — `PRICING SELECT` → `SODISC LINE`/`DOCUMENT` → `SOVAT` → `SOMARGN`; a below-policy
   margin raises an approval through `core.usp_RequestApproval`.
3. **Supply** — `SOAVAIL` → `SORESV` (`inv.usp_ReserveStock`) → `PICKGEN` → `DESPATCH` (stock
   movement + COGS journal) → `SOINVRUN` (sales ledger invoice) → `REBATE` accrual.
4. **After the event** — `RETURNS`, `BACKORD`, `SOAMEND`, `EXPEVID`, `SOROT`, `SODOCVR`.

### Drop shipment posting (`SOB2B DIRECTDL`, D-03, SO-021)

A confirmed direct delivery writes the evidence despatch (`so.Despatch` / `so.DespatchLine`, cost
method `DROP`, no `MovementId` because no warehouse stock ever existed), updates the demand, and
then accrues the cost **outside** the SQLIO unit of work: DR cost of sales, CR goods received not
invoiced, under business event `SO.DROPSHIP.COGS`. The accounts **and the analysis dimensions**
come from `fin.EventAccountMap` (`SOB2B` section 4060/4070); `fin.AccountDimRule` makes a cost
centre mandatory on 5010 and forbidden on 2010, so the legs must not be stamped from the program.
`PARMGET` (`SO.DROPSHIP.COGS.ACCOUNT`, `SO.DROPSHIP.GRNI.ACCOUNT`) is only the fallback when the
event map has no row.

### Function codes are eight characters

`CA-FUNCTION` in `STCAHDR.cpy` is `PIC X(8)`. Every SO `WS-FUNC` is `PIC X(8)` and every function
literal is eight characters or fewer: `CREDNOTE`, `CONCESS`, `UNPAIDLS`, `SUPPCHG`, `DIRECTDL`,
`RECORDPF`. A longer name is silently truncated by the caller and its `WHEN` branch becomes dead
code. No .NET facade or seed row passes any of the old long names.

## How to demonstrate

```powershell
build\build-db.ps1 -Only "so"            # deploy; run twice to prove re-runnability
build\build-db.ps1 -NoDeploy -Tests -Only "so"
build\build-cobol.ps1 -Module so
build\run-tests.ps1 -Layer cobol -Filter TSO
build\run-tests.ps1 -Layer net -Filter so
```

Seeded demonstration data (document numbers use the 900000 band so they can never collide with a
number the live series issues):

* Quotations `SQS900001`..`SQS900008` and `SQC900001`..`SQC900002` cover every quote state —
  draft, issued, revised (two versions), accepted, declined, expired, converted, part converted.
* Orders `SOS900001`..`SOS900012` (STC) and `SOC900001`..`SOC900005` (CAL) cover draft, accepted,
  amended and cancelled; awaiting approval, credit hold, commercial hold (deposit outstanding),
  supply hold and released; unallocated, part reserved, reserved, part picked and picked; plus a
  blanket order and its call-off, a euro export order and a drop shipment.
* `SOS900008` (STC) and `SOC900004` (CAL) are deliberately left **picked and billing-eligible**:
  confirm a despatch against them in `frmDespatchConfirm`, then run `frmSalesInvoiceRun` in
  `PREVIEW` and `COMMIT` to produce a real despatch, COGS journal and sales invoice.
* Price lists `STD26`, `TRADE26`, `DIST26`, `OEM26`, `CUSTC0001`, `CONTC0007`, `EURC0021`,
  `USDC0027`, `CALSTD26`, `CALTRD26`, `CONTK0001` demonstrate all four list levels on one item.
* Rebate agreements `REB-2026-001`..`004` and `REB-2026-101`.

> Despatches, COGS postings and sales invoices are **not** seeded. They are accounting events and
> must be produced by running `DESPATCH` and `SOINVRUN`, so that stock, the nominal ledger and the
> sales ledger agree. `cobol\batch\so\SODEMO.cob` does exactly that for every company and is run
> by the cross-module demonstration phase (`build\demo-data.ps1`), which then levels the control
> accounts and reconciles them (`RECONCTL` RC 0). It sells to the demonstration account below,
> because every other seeded account with a picked order is deliberately on credit hold.

## Quote conversion completes the order header (CR2601, 2026-10-07)

A quotation prices goods; it never re-states the account. `SOQUOTE CONVERT` therefore completes the
order header from the sales ledger before it writes `so.SalesOrder`, exactly as an order clerk
would take the details off the account card:

| Order field | Source |
|---|---|
| `InvoiceCustomerCode` | `ar.Customer.InvoiceCustomerCode`, else the ordering account |
| `DeliveryAddressRef` / `DeliveryAddressSnapshot` | `ar.CustomerAddress` role `DELIVERY`, `IsDefault` first, validity honoured |
| `BillToAddressSnapshot` | role `BILLING`, else `REGISTERED`, else the delivery snapshot |
| `TermsCode` / `TermsSnapshot` | the quote's terms, else `ar.Customer.TermsCode` |
| `TermsAndConditionsVersion` | parameter `SO.TCS.VERSION` (default `STERP-TCS-2026-04`) |
| `VatTreatmentBasis` | from `ar.Customer.VatLocation`: GB *UK standard rated*, XI *NI protocol goods*, EU/ROW *Export zero rated* |
| `IncotermsRule` / `IncotermsNamedPlace` | `ar.Customer.DeliveryTermsCode` (or DAP) and the delivery address, for non-GB VAT locations only |

Anything the caller states in `SQ-ORDER-DEFAULTS` is kept; the values actually written are returned
in the same fields with `SQ-DEFAULTS-SRC`. When the master cannot complete the header the order is
still raised and `SOQ0014` (04) says so - `SOACCEPT` then blocks it until the terms are stated.
Programs: `SOQUOTE` sections `6700`-`6780`. Tests: `cobol\tests\so\TSOQUOTE.cob` (16 assertions),
including `SOACCEPT VALIDATE` finding **no blockers** on a converted order.

## The demonstration account

Every seeded account that carries a picked order is deliberately in difficulty (over limit,
overdue, newly opened), so `db\seed\93-so-demo.sql` adds one account per company in good standing -
`C0090` Thameside Demonstration Cycles (STC) and `K0090` Clyde Demonstration Cycles (CAL),
`Classification = 'SODEMO'`, no open items, reviewed limit, no hold - and re-points the two picked
orders at it. `SODEMO` finds it through `3150-DEMO-CUSTOMER` and still puts it through `CREDCHK`
before quoting against it. `C0010` and `K0003` stay held, so the credit controls still demonstrate.

## Correlation ids inside DESPATCH (CR2604)

A correlation id names **one** business instruction (COM-005/COM-015). `DESPATCH CONFIRM` is
several instructions, so `9700-SUB-CORR` derives one id per instruction from the caller's:

| instruction | correlation id |
| --- | --- |
| the despatch itself (`IDEMPOT`, `NUMGEN`, `CREDCHK`) | `<corr>` |
| stock issue of line *n* | `<corr>-Mnnn` |
| stock reversal of line *n* | `<corr>-Rnnn` |
| cost-of-sale journal (stage + `fin.usp_PostJournal`) | `<corr>-COGS` |

Sharing one id let `inv.usp_PostStockMovement` complete `core.IdempotencyKey` with a movement
id, after which `fin.usp_PostJournal` treated the cost of sale as already posted and returned an
empty result set (`DSP0007` *The cost of sale posting returned no result*), and the second and
later lines of a despatch were never moved. Any new service call added to `DESPATCH` must take a
derived id unless it genuinely belongs to the despatch instruction itself.

## Change log 2026-10-07 (returns, demo gate)

* **CR2605 RETURNS.cob** - `9700-SUB-CORR` derives one correlation id per stock movement of a
  return (`<corr>-RCVnnn` receipt, `-HLDnnn` quality hold, `-RELnnn` release, `-SCRnnn` scrap).
  One id may only name one idempotent instruction: `inv.usp_PostStockMovement` completes
  `core.IdempotencyKey` for the id it is given, so the receipt used to complete the key shared by
  the whole return and every later movement was short-circuited.
* **CR2605 STKMOVE.cob (itm, shared)** - `0100-INITIALISE` no longer clears `SM-HOLD-ID` for
  `RELEASE`; it is an output of `HOLD` but an input of `RELEASE`, and clearing it made every
  return disposal fail with `STK0014` / `RET0007`.
* **CR2606 PICKGEN.cob** - `CONFIRM` with `PK-LINE-NUM = 0` now confirms the first line of the
  pick list still `OPEN`, in full when no picked or short quantity is stated, so the desktop
  facade can confirm a pick without restating its lines.
* **93-so-demo.sql** - seeds `DNS900001`, a delivery of `SOS900006` line 2 made in full on the day
  originally promised, so the customer OTIF KPI (`bin\x86\KPIOTIF.exe`) has a numerator.
* `SODEMO CO=STC MODE=COMMIT USER=SALESREP` ends **RC 0** and is repeatable;
  `build\demo-data.ps1` ends `RECONCTL` **RC 0**.

## Change log 2026-10-07 (order-to-cash acceptance chain)

`acc: order to cash` (`dotnet\STERP.Tests\Acceptance\OrderToCashTests.cs`) now runs every step -
quote, order, reservation, pick, despatch, invoice, receipt, allocation and aged debt - with no skips.

* **CR2607 DESPATCH.cob** - a despatch follows the pick, so it no longer has to restate the bin.
  `2425-DEFAULT-BIN` (performed from `2420-ONE-LINE-CHECK` right after `2430-READ-ORDER-LINE`)
  fills site, warehouse, location, lot and serial only when the caller left warehouse or location
  blank: `2426-BIN-FROM-PICK` takes the confirmed `so.PickListLine` of this order line when
  `DS-PICKLIST-REF` is given, otherwise `2427-BIN-FROM-STOCK` takes the owned, accepted, available
  `inv.StockBalance` that can best supply the quantity. A pick bin is used only when it gives both
  a warehouse and a location. Tested by `TSODESP` `2400-DESPATCH-DEFAULTS-BIN`.
* **CR2608 SOINVRUN `1300-BUILD-KEY`** - `ARINVPST` carries one despatch reference and refuses
  (`ARI0042`) a despatch it has already invoiced, so the despatch reference is now part of the
  grouping key (`G-KEY X(96)`). Before, a new despatch grouped behind an invoiced one was refused
  with it and silently never billed.
* **CR2609 ARINVPST (ar, shared)** - a permission hold (RC 06) now stops the post; see
  `docs\modules\ar.md`. Billing therefore runs as `ARCLERK`, not `SALESREP`.
* **CR2610 SOINVRUN `1000-SELECT-LINES`** - SQL Server will not start a transaction while a fetch
  cursor is open (`STERP-E-1404`), so the selection is read to the end into `WS-SEL-TABLE`
  (400 rows), the cursor closed, and only then are the groups posted. Overflow is reported as
  `STERP-W-1413`, never ignored.

Rules these fixes taught: one correlation id names one idempotent service; a COBOL batch may not
open a transaction while a fetch cursor is open; a service that answers 06 must have had no
business effect; in the `acu` dialect every `DISPLAY` in `so`/`po` must say `UPON SYSOUT`.
