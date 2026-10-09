# STERP - demo workflows by company and site

The sign-on dialog offers four company/site combinations. They are **not** equally populated: the
seed scripts and demo drivers put most of the transactional story in Stellar Components, Reading.
Pick the combination that matches the story you want to tell.

| Sign-on | Who they are | Data depth | Best for |
|---|---|---|---|
| **STC / RDG** Reading Works | Stellar Components' manufacturing site (bicycle components) | **Rich** - 22 sales orders, 304 stock balances, all 7 kits, BOMs, costing, returns, quality holds | Order to cash, credit control, kits, bills of materials, costing, finance close |
| **STC / BHM** Birmingham Distribution Centre | Stellar's distribution and buying site; the buyers sit here | **Purchasing hub** - all 16 purchase orders, receipts, requisitions, supplier returns, quarantine stock | Procure to pay, goods-in and inspection, approvals, segregation of duties |
| **CAL / GLA** Glasgow Assembly Plant | Caledon Assemblies, a second, smaller legal entity | **Light** - 4 sales orders, 6 customers, 5 suppliers, 14 items, legacy files to load | Multi-company, data migration and cut-over, a compact order to cash |
| **CAL / LIV** Livingston Finishing Unit | Caledon's non-manufacturing finishing unit | **Thin** - 1 sales order (on credit hold), 4 item sites | Credit hold and override, site-scoped security; use it as a cameo, not a main act |

Finance, debtors and creditors are held **per company**, not per site, so the finance workflows
below run from either STC site. Stock, sales orders, purchase orders and kits are **per site**.

> **Before a demo:** `build\demo-data.cmd` (or `-Rebuild` for a clean slate). Several steps
> below are one-shot - for example, a picked order can only be despatched once - so rebuild
> between rehearsal and the real thing. Close `STERP.exe` before rebuilding; it locks
> `STERPCOB.dll`.

Users are switched with **Continue as** on the sign-on dialog (signed on as `SYSADM`), or from the
running client with *File > Continue as User* (user) and *File > Select Company / Site* (company,
site and business date). A real change of user, company, site or date closes every open window,
because their data and permissions belong to the old context; *Cancel* leaves the session exactly
as it was. All batch commands below run from the `STERP-App` folder.

---

## 1. STC / RDG - "Order to cash on a legacy core" (20 min)

**Story:** a cycle-parts manufacturer takes an order, checks credit, despatches, invoices and
collects the cash, and the ledger stays in balance throughout.

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `SALESREP` | *Sales > Sales Order Entry* | Open `SOS900008` (Thameside Demonstration Cycles, `C0090`) | Accepted, released, **picked** and billing-eligible; the customer is in good standing. Its pick list is made by `demo-data.cmd` (SODEMO reserves and picks it through SORESV/PICKGEN): the 16 tyres in full, 6 of the 8 frames from one lot, with the other 2 on back order because one despatch line issues from one lot |
| 2 | `SALESREP` | same | Flick through `SOS900003` (Solent Cycle Centre, `C0008`) and `SOS900004` (Fenland Cycle Repairs, `C0011`) | Credit hold (invoice > 30 days overdue) versus commercial hold (1,500.00 deposit not received): two different reasons an order cannot ship |
| 3 | `WHSE` | *Sales > Despatch Confirmation* | Press **F2**, pick the pick list for `SOS900008` (or key `SOS900008` in *Order number* and press F5), then F10 | Stock is issued from the bin and lot named on the pick, and the cost-of-sale journal is posted in the same unit of work |
| 4 | `ARCLERK` | *Sales > Sales Invoice Run* | Run `PREVIEW`, then `COMMIT` | Preview changes nothing; commit raises the invoice, the AR open item and the VAT. Billing is the AR clerk's job: posting an invoice needs `AR.INVOICE.POST`, so `SALESMGR` can open the run and preview the billing queue, but its `COMMIT` is refused (`ARI0050`) |
| 5 | `ARCLERK` | *Debtors > Customer Enquiry* (F5) | `C0001` Pennine Cycles | One invoice, `SI-26-1001`, 120+ days overdue and part-paid (receipt `RC-26-0002` and credit note `CN-26-0001` leave 8,200.00), with an open collection case and a late-payment charge. For invoices in every ageing bucket run `AGEDDEBT` (*Debtors > Debtors Reports*); the disputed invoice is `SI-26-1008` on `C0009` |
| 6 | `ARCLERK` | *Debtors > Receipt Entry* (F6) | Receipt for `C0001`, bank `GBP1`, gross **8,196.00**: allocate 8,196.00 to `SI-26-1001`, key **4.00** in *Write off* with a reason, *Preview*, then F10. Then key a receipt for `C0004` of 5,081.00 against `SI-26-1002` with **19.00** to write off and press *Preview* only | 4.00 is inside the 5.00 write-off tolerance and posts; for 19.00 the preview warns that posting would be held (`RCP0029`) because only `CREDMGR` holds `AR.WRITEOFF.APPROVE` (decision D-22). Preview values the receipt being keyed and posts or requests nothing |
| 7 | `CREDMGR` | *Debtors > Credit Control Worklist* (F7) | Raise a dispute and a promise to pay on an overdue account | The account drops out of the next dunning run |
| 8 | `FINCTRL` | *Reports > Controls and reconciliation > Control account reconciliation* | Run `RECONCTL` as at `20261031` | All eight control families still agree after everything above |

**Optional extras on the same site:** the euro export order `SOS900011` (Velo Atlantique, `C0021`,
EUR), the blanket order `SOS900009` and its call-off `SOS900010` (Mercia Frameworks, `C0007`), and
the drop shipment `SOS900012` to Causeway Frame Works (`C0020`, VAT location **XI**, Northern
Ireland protocol). *Debtors > Debtors Reports* runs `AGEDDEBT`, `STATEMNT`, `DUNNING` and
`DDCOLL` (Bacs Standard 18 file in `run\bacs`).

---

## 2. STC / RDG - "Engineer to build: BOM, kit and cost" (20 min)

**Story:** engineering defines what a product is made of, the shop floor builds it with full
traceability, and costing shows what it really cost.

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `ENGINEER` | *Bills of Material > Explosion Enquiry* | `FG-BIKE-COMMUTE-M`, qty 10, cost set `BOMDEMO` | 17 bills, 5 levels, phantoms (cable set, hardware, paint), 97 % frame yield (10 bikes need 11 frames); unit cost **568.25** against a list price of 780 |
| 2 | `ENGINEER` | same | `FG-CAKE-CELEB-8IN`, qty 12 | The non-engineering example: grams in the recipe, kilograms in stock, 96 % sponge yield, sponge offcuts as a by-product; **50.20** a cake |
| 3 | `ENGINEER` | *Bills of Material > Where Used Enquiry* | `DEMO-PIN` | Six uses, three levels deep, with open demand |
| 4 | `ENGINEER` | *Bills of Material > Engineering Changes* | Preview `ECO-BOMDEMO-001` (replace `DEMO-PIN` with `DEMO-PIN-HD`) | Released bills get a new draft revision; the run is refused (`BOM1411`) because `BOM-DEMO-OPT` has an unconfigured alternate group |
| 5 | `ENGINEER` | *Bill Maintenance* | Try to edit a released revision; release your own bill | `BOM0413` (released is immutable) and `BOM0410` (author cannot approve) |
| 6 | `WHSE` | *Kits > Kit Execution* | Walk the kits: `KIT0001004` draft, `KIT0001002` released, `KIT0001003` part-issued, `KIT0001001` part-complete (4 of 10, WIP 1,842.50), `KIT0001005` financially closed | One screen shows the whole kit lifecycle |
| 7 | `QUALITY` | *Kits > Genealogy Enquiry* | The completed kit from the latest `KITDEMO` run | Forward and backward trace from component lots to finished serials |
| 8 | `COSTACC` | *Costing > Cost Sets* | Select `FY2027STD` (in review), Compare against `FY2026STD`, then Approve as the preparer | The revaluation activation *would* post; `CST0405` refuses self-approval |
| 9 | `COSTACC` | *Costing > Job Cost Enquiry* | `CST27000` | Two sub-jobs rolled up once; estimate at completion over budget |

**Quality cameo:** *Stock Control > Stock Enquiry*, `NUT-M6-NYLOC` at RDG - 180 units on an
open quarantine hold (thread gauge failure): physical but not available.

---

## 3. STC / BHM - "Procure to pay with controls" (20 min)

**Story:** a buyer raises demand, a manager approves it, goods arrive and are inspected, the
invoice is matched and two independent people authorise the payment.

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `BUYER` | *Purchasing > Purchasing Reports > Requisition Generation* | Run `REQGEN` in `PREVIEW` | Re-order points generate requisitions from demand |
| 2 | `BUYER` | *Purchasing > Purchase Orders* | Walk the status ladder: `PO00000001` draft, `PO00000002` awaiting approval, `PO00000003` approved, `PO00000004` issued, `PO00000005` acknowledged | One order per lifecycle state |
| 3 | `BUYER`, then `PURMGR` | *System > Approvals Inbox* | `PO00000002` (Titan Light Metals, 1,985.80) is pending for `PURMGR`. Try to approve as `BUYER` first, then approve as `PURMGR` | Segregation of duties: the raiser cannot approve; approval is bound to a content hash |
| 4 | `WHSE` | *Purchasing > Goods Receipt and Inspection* | Receive `PO00000004` | GRNI posts on receipt; a lot is registered |
| 5 | `QUALITY` | same / *Stock Enquiry* | `TUBE-AL7005-34.9X1.4` in `QUAR01` (85) and `BAR-AL6082-25MM` in `INSP01` (55) | Stock that is physically there but not usable until inspected |
| 6 | `BUYER` | *Purchase Orders* | Show the variety: `PO00000009` blanket in **USD** (Puget Drive Systems), `PO00000010` service order (Cheriton Calibration), `PO00000011` subcontract (Castleford Fastenings) | Blanket, service-acceptance and subcontract flows |
| 7 | `APCLERK` | *Creditors > Invoice Register and Matching* | Register `INV-55010` for `S0001`, re-key the same reference, then key `inv 55010`, then **Match all** | Hard duplicate refusal, "similar document" warning; three-way match with a price variance inside tolerance and a line blocked for approval |
| 8 | `APCLERK` | *Creditors > Supplier and Bank Verification* | `S0002` Pennine Tube and Section: change the bank account | Stays `PENDING` until confirmation of payee, call-back and independent approval are all recorded; the payment proposal refuses to pay it |
| 9 | `APCLERK` / `TREASURY` / `TREASURY2` | *Creditors > Payment Proposal and Run* | Build, reserve, request approval, approve twice, create the run, cut the Bacs file | Two independent authorisers (D-07); a second Bacs cut is labelled a reproduction |

**Link to sales:** `SOS900005` (Wheelcraft, `C0005`) is the only BHM sales order - on **supply
hold** because a back-to-back line is waiting for supplier confirmation. It shows sales and
purchasing depending on each other.

**Batch equivalent:** `jobs\PAYRUN.cmd` and `bin\x86\APDEMO.exe CO=STC USER=APCLERK MODE=COMMIT`
do the payment run without a screen, which is a good modernisation talking point.

---

## 4. CAL / GLA - "Second company and data migration" (15-20 min)

**Story:** the group has acquired Caledon Assemblies. Its balances are loaded from the old system,
it runs on the same platform under its own ledger and Scottish calendar, and it trades with
Stellar.

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `SYSADM` | *System > Companies / Sites* | Compare STC and CAL | Separate VAT number, registered office in Manchester, Scottish bank-holiday calendar on the sites |
| 2 | `FINCTRL` | *Reports > Controls and reconciliation > Legacy conversion load* (or command line) | `CONVLOAD CO=CAL FILE=data\legacy\CALAROPN.DAT CUTOVER=20260331` - `PREVIEW`, then `COMMIT`, then `COMMIT` again | Preview proves the file; commit creates real open items; the restart loads nothing twice. Repeat with `CALAPOPN.DAT` (creditors), `CALGLTB.DAT` (trial balance) and `CALSTOCK.DAT` (opening stock) |
| 3 | `SALESREP` | *Sales > Sales Order Entry* | `SOC900001` draft awaiting approval (Highland Trail Bikes), `SOC900002` reserved (Firth of Forth Cycles), `SOC900005` amended and part-reserved | Same lifecycle as STC, smaller dataset - easier to follow |
| 4 | `WHSE`, `ARCLERK` | *Despatch Confirmation*, then *Sales Invoice Run* | Despatch (F2 for the pick list) and invoice `SOC900004` (Clyde Demonstration Cycles, `K0090`) | The STC order-to-cash flow, shown in about five minutes. The pick issues the serial `SN26RAVELM001`, which becomes `ISSUED` |
| 5 | `ARCLERK` | *Debtors > Customer Enquiry* | `K0005` Stellar Components Ltd and `K0003` Lothian Commuter Supply | **Intercompany:** Stellar is a Caledon customer (`K0005`) and a supplier (`T0001`); Caledon is customer `C0040` in STC |
| 6 | `FINCTRL` | *Finance > VAT Return* | Period `27A1` (draft) | Each company has its own VAT return |
| 7 | `FINCTRL` | `RECONCTL` | Run for `CO=CAL` as at `20261031` | The newly loaded entity reconciles like the established one |

`jobs\XMDCONV.cmd` runs the whole conversion as a job stream - useful when the audience is planning
a cut-over.

---

## 5. CAL / LIV - "Credit hold and site security" (5 min cameo)

Use it as an add-on to workflow 1 or 4, not on its own.

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `SALESREP` | *Sales > Sales Order Entry* | `SOC900003` for Borders Cycle Hire (`K0004`), 7,128.00 | On **credit hold**: a new account's first order is over the provisional limit |
| 2 | `SALESREP` | *Debtors > Credit Overrides* | Try to override | Refused - the sales rep holds `AR.CREDIT.VIEW` only; `AR.CREDIT.OVERRIDE` belongs to the credit manager |
| 3 | `CREDMGR` | same | Grant a scoped override; the order releases | Who did it, when and why is recorded in *System > Audit Enquiry* |
| 4 | `RESTRICT` | Sign on | Try to sign on to CAL / LIV | Restricted user is scoped to STC / RDG only: another company or site is refused |
| 5 | any | *Stock Control > Stock Enquiry* | An item at GLA, then the same item at LIV | Same company, two sites, separate stock |

---

## 6. Either STC site - "Close the month" (15 min, finance audience)

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `GLCLERK` | *Finance > Journal Entry* / *Journal Enquiry* | Post a journal; try one into a closed period (May 2026) | Year hard-closed to August 2026; draft journals are waiting |
| 2 | `FINCTRL` | *Finance > VAT Return* | `26A2` (finalised): box 1 **94,500.00**, box 4 **47,670.00**, box 5 **46,830.00**; drill into box 1. Then open draft `26A3` and *Submit to file* | Box drill-down to source transactions; the MTD payload lands in `run\mtd` |
| 3 | `FINCTRL` | *Finance > Bank Reconciliation* | `GBP1`, statement `BS-2026-10` | Matched cash book, plus two real outstanding items: interest 318.44, insurance direct debit 1,265.00 |
| 4 | `FINCTRL` | *Finance > Period Close Checklist* | Period 7 | Checklist-driven close with owners |
| 5 | `FINCTRL` | *Reports > Controls and reconciliation* | `RECONCTL`, *Exception ageing and escalation* (`EXCPAGE`), *Statutory obligations register* (`OBLIGREG`), all as at `20261031` | Eight control families agree; every module's exceptions are aged and owned; the statutory diary escalates overdue filings |
| 6 | `OPS` | `bin\x86\STERPOPS.exe` | Submit `MONTHEND`, watch it in the job monitor, browse the spool | The green-screen operations side: JCL-style streams, condition codes, restart |

---

## Suggested running orders

| Audience | Running order |
|---|---|
| Business / ERP buyers (45 min) | 1 > 3 > 5 |
| Manufacturing / operations (40 min) | 2 > 3 (steps 4-6) |
| Finance (35 min) | 6 > 1 (steps 4-8) > 4 (steps 2, 6, 7) |
| Modernisation / architects (30 min) | 1 (steps 1-4) > 4 (step 2) > 6 (step 6), then [ARCHITECTURE.md](ARCHITECTURE.md) and [TECH-DEBT-REGISTER.md](TECH-DEBT-REGISTER.md) |

See [DEMO-DATA.md](DEMO-DATA.md) for how the dataset is built and the `docs\modules\*.md` "How to
demo" sections for the per-module detail behind each step.
