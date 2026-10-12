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
>
> For QA, use an isolated database/runtime and preserve the post-transaction evidence.
> Do not rerun the demo control bridge after GUI postings: it restates demonstration
> positions and can conceal the discrepancies the control proof is supposed to expose.
> See [FUNCTIONAL-QA-LEDGER.md](FUNCTIONAL-QA-LEDGER.md) for the 12 October verification
> results, repaired defects and the deliberately unresolved bank/cut-over control positions.
> The final source-deployed QA run proves the GUI paths; an expected permission, immutable
> revision, hard-closed period or blocked-close refusal is a passing control, not a failed demo.

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
| 1 | `SALESREP` | *Sales > Sales Order Entry* | Open `SOS900008` (Thameside Demonstration Cycles, `C0090`) | Accepted, released, **picked** and billing-eligible; the customer is in good standing. SODEMO reserves/picks through SORESV/PICKGEN. Inspect the actual pick: the final fresh QA run picked 16 tyres and all 8 frames. A depleted single lot can leave frames on back order; do not assume the historical six-frame pick |
| 2 | `SALESREP` | same | Flick through `SOS900003` (Solent Cycle Centre, `C0008`) and `SOS900004` (Fenland Cycle Repairs, `C0011`) | Credit hold (invoice > 30 days overdue) versus commercial hold (1,500.00 deposit not received): two different reasons an order cannot ship |
| 3 | `WHSE` | *Sales > Despatch Confirmation* | Press **F2**, pick the pick list for `SOS900008` (or key `SOS900008` in *Order number* and press F5), then F10 | Stock is issued from the bin and lot named on the pick, and the cost-of-sale journal is posted in the same unit of work |
| 4 | `ARCLERK` | *Sales > Sales Invoice Run* | Run `PREVIEW`, then `COMMIT` | Preview changes nothing; commit raises the invoice, the AR open item and the VAT. Billing is the AR clerk's job: posting an invoice needs `AR.INVOICE.POST`, so `SALESMGR` can open the run and preview the billing queue, but its `COMMIT` is refused (`ARI0050`) |
| 5 | `ARCLERK` | *Debtors > Customer Enquiry* (F5) | `C0001` Pennine Cycles | One invoice, `SI-26-1001`, 120+ days overdue and part-paid (receipt `RC-26-0002` and credit note `CN-26-0001` leave 8,200.00), with an open collection case and a late-payment charge. For invoices in every ageing bucket run `AGEDDEBT` (*Debtors > Debtors Reports*); the disputed invoice is `SI-26-1008` on `C0009` |
| 6 | `ARCLERK` | *Debtors > Receipt Entry* (F6) | Receipt for `C0001`, bank `GBP1`, gross **8,196.00**: allocate 8,196.00 to `SI-26-1001`, key **4.00** in *Write off* with a reason, *Preview*, then F10. Then key a receipt for `C0004` of 5,081.00 against `SI-26-1002` with **19.00** to write off and press *Preview* only | 4.00 is inside the 5.00 write-off tolerance and posts; for 19.00 the preview warns that posting would be held (`RCP0029`) because only `CREDMGR` holds `AR.WRITEOFF.APPROVE` (decision D-22). Preview values the receipt being keyed and posts or requests nothing |
| 7 | `CREDMGR` | *Debtors > Credit Control Worklist* (F7) | Select `SI-26-1003` / `C0008`, **Raise dispute**, reason `PRICE` and an explanatory note. Select `SI-26-1008` / `C0009`, **Promise to pay (F10)**, 3,000.00 due `19/10/2026`. Run **Dunning letters** as at `20261012` | The debt remains. The report explicitly explains suppression; neither customer receives a letter while the whole-balance dispute / live promise applies. The fresh QA run produced two letters for other customers |
| 8 | `FINCTRL` | *Reports > Controls and reconciliation > Control account reconciliation* | Run `RECONCTL` as at `20261031` | Fresh GUI despatch/receipt valuation now agrees: AR, AP, INVENTORY, WIP, GRNI, VAT and ACCRUAL agree. BANK has an owned 8,196.00 difference because the saved bank position predates the cash receipt. Show the live bridge and unresolved statement items; never rerun a demo bridge to manufacture agreement |

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
| 1 | `ENGINEER` | *Bills of Material > Explosion Enquiry* | `FG-BIKE-COMMUTE-M`, qty 10, cost set `BOMDEMO` | 95 displayed occurrences, depth 4 below the root (5 levels including it), phantoms (cable set, hardware, paint), 97 % frame yield (10 bikes need 11 frames); unit cost **568.25** against a list price of 780 |
| 2 | `ENGINEER` | same | `FG-CAKE-CELEB-8IN`, qty 12 | The non-engineering example: grams in the recipe, kilograms in stock, 96 % sponge yield, sponge offcuts as a by-product; **50.20** a cake |
| 3 | `ENGINEER` | *Bills of Material > Where Used Enquiry* | `DEMO-PIN` | Six direct uses, nine total parent rows and two upward levels |
| 4 | `ENGINEER` | *Bills of Material > Bulk Change Preview* | `ECO-BOMDEMO-001`, old `DEMO-PIN`, replacement `DEMO-PIN-HD`, effective `01/11/2026`, scope `ITEM`, blank scope key, include released/draft; **Preview (F10)** | Seven targets, six proposed changes (four new revisions/two draft amendments), one failed validation: BOM1411 refuses the entire change, no bills applied. The option bill explains missing resource rate BOM0326 and unselected FINISH alternate group BOM0331. F5 clears the answer/grid. For a second refusal, replacement `DEMO-NOT-AN-ITEM`, scope `BOMREF`, key `BOM-DEMO-FRAME` shows BOM1435. Preview is available under existing BOM.VIEW, not an approval/APPLY grant |
| 5 | `ENGINEER` | *Bill Maintenance* | Try to edit a released revision; try its release action | Header and component content are immutable (`BOM0413`). ENGINEER does not hold release authority and is refused `BOM0403`; do not describe that as author self-approval. The separate authorised-author control (`BOM0410`) remains enforced and regression-tested |
| 6 | `WHSE` | *Kits > Kit Execution* | Walk the kits: `KIT0001004` draft, `KIT0001002` released, `KIT0001003` part-issued, `KIT0001001` part-complete (4 of 10, WIP 1,842.50), `KIT0001005` financially closed | One screen shows the whole kit lifecycle |
| 7 | `QUALITY` | *Kits > Genealogy Enquiry* | The completed kit from the latest `KITDEMO` run | Forward and backward trace from component lots to finished serials |
| 8 | `COSTACC` | *Costing > Cost Sets* | Select `FY2027STD` (in review), Compare against `FY2026STD`, then inspect Impact; cancel posting actions unless separately authorised | Comparison/Impact retain FY2027STD. Impact shows **REVIEW**, preparer **KWATSON**, no approver, seven items and 16,094.69 projected revaluation. KWATSON is not an active demo user: do not pretend COSTACC is its preparer or weaken self-approval controls |
| 9 | `COSTACC` | *Costing > Job Cost Enquiry* | `CST27000` | Two sub-jobs rolled up once: budget **56,500**, actual **32,345**, committed **4,380**, estimate at completion **38,365** and **18,135 favourable** variance |

**Quality cameo:** *Stock Control > Stock Enquiry*, `NUT-M6-NYLOC` at RDG - inspect the actual
QUAR01 balance (60 units in the verified fixture, not the historical 180) on an
open quarantine hold (thread gauge failure): physical but not available.

---

## 3. STC / BHM - "Procure to pay with controls" (20 min)

**Story:** a buyer raises demand, a manager approves it, goods arrive and are inspected, the
invoice is matched and two independent people authorise the payment.

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `BUYER` | *Purchasing > Purchasing Reports > Requisition Generation* | Run `REQGEN` in `PREVIEW` | Re-order points generate requisitions from demand |
| 2 | `BUYER` | *Purchasing > Purchase Order Entry* | Walk the status ladder: `PO00000001` draft, `PO00000002` awaiting approval, `PO00000003` approved, `PO00000004` issued, `PO00000005` acknowledged | One order per lifecycle state |
| 3 | `BUYER`, then `PURMGR` | Buyer refusal in Purchase Order Entry; manager decision in *System > Approvals Inbox* | Try to approve `PO00000002` as BUYER, then select its pending request as PURMGR, **Approve (F10)** and enter an audit comment | BUYER is refused APV0007. The independent inbox decision now atomically updates the matching order to APPROVED with PURMGR as approver; stale-content decisions cannot approve a changed order |
| 4 | `WHSE` | *Purchasing > Goods Receipt and Inspection* | Load/validate `PO00000004`; acknowledge any required-lot notification. Count its 40 metres of `BAR-AL6082-25MM`, supply `GOODSIN` and a new lot, **Count line**, then F10 with ownership **RECEIPT** | Fresh QA: CONFIRMED receipt, exact order-line FK, received/accepted 40, movement/valuation 410.83, GRNI journal 410.83 and FULLRECEIVED delivery state, all in one unit of work. After loading, changing the keyed header requires revalidation |
| 5 | `QUALITY` | same / *Stock Enquiry* | `TUBE-AL7005-34.9X1.4` in `QUAR01` (85) and `BAR-AL6082-25MM` in `INSP01` (55) | Stock that is physically there but not usable until inspected |
| 6 | `BUYER` | *Purchase Order Entry* | Show the variety: `PO00000009` blanket in **GBP** (Puget Drive Systems), `PO00000010` service order (Cheriton Calibration), `PO00000011` subcontract (Castleford Fastenings) | Blanket, service-acceptance and subcontract flows |
| 7 | `APCLERK` | *Creditors > Invoice Register and Matching* | Filter to `PI-26-0001` (S0001, `AAS/118220`) and click **Duplicate check**; repeat with credit note `PC-26-0001`. Matching is a separate action | APR0010 identifies the existing invoice or credit note. Executable normalisation uppercases ASCII letters/digits, removes other characters and retains leading zeros. This screen enquires/matches/checks duplicates; it is **not invoice capture**, so filtering does not register a new invoice |
| 8 | `APMGR`, `SYSADM`, `TREASURY` | *Creditors > Supplier and Bank Verification*; treasury's *Approve Supplier Bank Changes* | Load `S0002`/Refresh first. APMGR **Change bank** (BANKREF at most 10 characters; fictional details for QA). SYSADM **Bank records**, select that reference, **CoP check** MATCH, then **Record callback** using a pre-held contact. TREASURY selects it and **Approve bank** | PENDING -> VERIFIED -> APPROVED requires independent evidence/authority. Eleven-character references are explicitly refused with no write, never silently truncated. APCLERK is not granted bank-maintenance authority |
| 9 | `APCLERK` / `TREASURY` / `TREASURY2` | *Payment Proposal and Run*, *Authorise Payment Run*, *Release Payment Run* | Keep the existing released example `PPS00000001`. Build a **new** proposal: cutoff `30/11/2026`, payment `02/12/2026`, BACS/GBP1. Reserve; acknowledge exclusions; Request approval. TREASURY Approve, acknowledge first-signature warning; TREASURY2 Approve. TREASURY Create instructions, acknowledge notice, **Bacs file > Run** | APDEMO leaves matched/unreserved `S0002 / PIS00000001`, reference `APDEMO-GUI-UTILITY-2026`, due 15 November: one 148.14 item. Fresh GUI run PPS00000002 has two independent signatures and a local Standard18 file: nine records, 778 bytes (80-byte control/100-byte data records), one credit/one contra of 14,814 pence. Stored payment date determines processing day 1 December. Reproduction returns RC4 without re-paying or changing original file references. Nothing is transmitted |

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
| 6 | `VATOWNER` | *Finance > VAT Return* | Prepare period `27A1` (draft) | Each company has its own VAT return; FINCTRL is not granted the preparer's PREPARE action |
| 7 | `FINCTRL` | `RECONCTL` | Run for `CO=CAL` as at `20261031` | Cut-over files intentionally include rejected/duplicate rows and are loaded on top of an already populated demonstration company. Show the actual AR/AP/inventory opening differences rather than claiming a clean empty-ledger cut-over. GRNI/VAT/BANK agree; CAL WIP/ACCRUAL have no configured control account |

`jobs\XMDCONV.cmd` runs the whole conversion as a job stream - useful when the audience is planning
a cut-over.

---

## 5. CAL / LIV - "Credit hold and site security" (5 min cameo)

Use it as an add-on to workflow 1 or 4, not on its own.

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `SALESREP` | *Sales > Sales Order Entry* | `SOC900003` for Borders Cycle Hire (`K0004`), 7,128.00 | On **credit hold**: a new account's first order is over the provisional limit |
| 2 | `SALESREP` | *Sales > Request Credit Override* | Request for customer `K0004`, order `SOC900003`, amount 7,128.00 and an audit reason | Request is PENDING/CRD0052; SALESREP has no authority-only Credit Overrides menu and cannot approve it |
| 3 | `CREDMGR`, then `SALESREP` | *Debtors > Credit Overrides*; *Sales Order Entry* | CREDMGR approves the selected pending scoped request. SALESREP loads `SOC900003` and **Accept / release (F10)** | Fresh source fixture includes its invoice customer. Normal validation now succeeds: ACCEPTED / RELEASED, K0004 invoice customer, no hold. Override does not bypass mandatory-field, sanctions/insolvency or independent-authority checks |
| 4 | `RESTRICT` | Sign on | Try to sign on to CAL / LIV | Restricted user is scoped to STC / RDG only: another company or site is refused |
| 5 | `WHSE` | *Stock Control > Stock Enquiry* | Sign on to CAL/GLA and enquire `CAL-BIKE-GRAVEL-M`; repeat under CAL/LIV | Three remaining serials, one each, at GLA after despatch; no matching balance at LIV. RESTRICT cannot select either CAL site |

---

## 6. Either STC site - "Close the month" (15 min, finance audience)

| # | User | Where | Do this | Point out |
|---|---|---|---|---|
| 1 | `GLCLERK` | *Finance > Journal Entry* / *Journal Enquiry* | Validate/post a balanced journal in October; try 15 May 2026 | May maps to hard-closed 202602 and is refused PER0003; October 2026/07 is open. QA posts 25.00 debit account 6500 / credit 7000, each with cost centre ADMIN/project GEN; supply dimensions on **each** new line |
| 2 | `VATOWNER`, then `FINCTRL` | *Finance > VAT Return*; *VAT Review / Finalise* | VATOWNER prepares `26A2`: box 1 **94,500.00**, box 4 **47,670.00**, box 5 **46,830.00**; drill box 1/4 (three/six records). Inspect draft `26A3` before finalising 26A2. FINCTRL **Load saved return**, **Finalise**, **Local file (F10)**, confirm and acknowledge local-only notice | PREPARE/APPROVE remain separate. Frozen output is STC, VRN GB412771903, period 1 July-30 September 2026, nine exact boxes; recorded local file in `run\work\vat`. It is explicitly **NOT transmitted to HMRC**, not an HMRC receipt |
| 3 | `FINCTRL` | *Finance > Bank Reconciliation* | Enquire on `GBP1`, statement `BS-2026-10`; inspect book, statement and outstanding items | Do not call the fixture matched: the QA enquiry had five statement rows, 23 outstanding book rows and a **68,973.44 unexplained difference**. Statement plus outstanding receipts minus payments is the bridge, not automatic proof of agreement |
| 4 | `FINCTRL` | *Finance > Period Close Checklist* | Period 7 | The verified fixture has two of six steps done, two blocking and readiness **BLOCKED**. Demonstrate owners/blockers; do not close it merely for the demo |
| 5 | `FINCTRL` | *Reports > Controls and reconciliation* | `RECONCTL`, *Exception ageing and escalation* (`EXCPAGE`), *Statutory obligations register* (`OBLIGREG`), all as at `20261031` | Keep differences explicit and owned. Final STC inventory/AR/AP/GRNI/VAT/WIP/accrual agree; the saved BANK snapshot differs by the 8,196 cash receipt. CAL has mixed-fixture cut-over differences; neither case justifies forcibly completing the close |
| 6 | `OPS` | `bin\x86\STERPOPS.exe`; desktop *System > Jobs* | Native sign-on OPS/STC; option 1, MONTHEND, YEAR=2026/PERIOD=7/MODE=PREVIEW, Enter. Inspect native option 2; in Windows Jobs select MONTHEND/View spool for SYSOUT and BSRPT/View spool for report | Real submission, completed child runs, readable completion, RC4/WARNING for owned reconciliation differences; PREVIEW leaves journal count unchanged. Windows status choices match RUNNING/OK/WARNING/FAILED/ABENDED. All nine streams preserve their exact maximum condition code on normal and failure exits |

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
