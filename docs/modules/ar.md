# Debtors and Credit Control (module `ar`)

Sales Ledger. Ported from MVS in 1991, so the COBOL dialect is **IBM Enterprise** (`ar=ibm` in
`cobol\DIALECTS.cfg`), fixed format (code area columns 8–72), `*>` comments, `SECTION`-per-paragraph
batch style.

* Spec: `03-Debtors-and-Credit-Control.md`, requirements **AR-001 .. AR-029**
* Schema: `ar` (postings always go through `fin.JournalStage` + `fin.usp_PostJournal`)
* Online programs: `cobol\online\ar\*.cob` – every one is LINKable (`DFHCOMMAREA` + `<PROG>CA.cpy`)
* Batch programs: `cobol\batch\ar\*.cob`
* Stored procedures: `db\procs\ar\*.sql`, view `db\views\ar\vw_ArDebtors.sql`
* Tests: `cobol\tests\ar\T*.cob`, `db\tests\ar-*.sql`, `dotnet\STERP.Tests\Debtors\DebtorsTests.cs`
* Seed: `db\seed\50-ar-customers.sql`, `51-ar-eventmap.sql`, `52-ar-menu.sql`, `91-ar-demo.sql`

---

## 1. Interfaces other teams need (read this first)

Both programs below are pure LINK programs. The COMMAREA is the standard STERP header `STCAHDR`
(`CA-FUNCTION`, `CA-COMPANY`, `CA-SITE`, `CA-USER`, `CA-BUSINESS-DATE`, `CA-CORRELATION-ID`,
`CA-RETURN-CODE`, `CA-REASON-CODE`, `CA-MESSAGE`, `CA-MSG-COUNT`, `CA-REQ-REF`) followed by the
module body. Return codes follow the AR convention in `cobol\copy\ARWORK.cpy`:

| `CA-RETURN-CODE` | Meaning |
|---|---|
| `00` | accepted |
| `04` | accepted with a warning (the result is still valid) |
| `06` | **held** – needs authority/approval; nothing was posted |
| `08` | rejected (business rule) |
| `12` | severe / infrastructure failure |
| `16` | not found |

> A hold (`06`) is as final as an error for anything that would otherwise post. Inside AR this is the
> `AR-RC-STOP` condition (`06 THRU 16`); callers must treat `06` the same way.

### 1.1 `ARINVPST` – sales invoice posting (copybook `ARINVPSTCA.cpy`)

**This is the interface the Sales (`so`) team calls to turn a despatch or a direct invoice into a
receivable.** `CALL "ARINVPST" USING WS-AI-CA` where `WS-AI-CA` is
`COPY ARINVPSTCA REPLACING ==DFHCOMMAREA== BY ==WS-AI-CA==.`

| `CA-FUNCTION` | Does | Needs on input | Gives back |
|---|---|---|---|
| `VALIDATE` | VAT content and commercial checks only. **No database writes** – safe to call from order entry. | header + `AI-LINE-CNT` lines | `AI-VAT-TAB`, `AI-NET/VAT/GROSS(-GBP)`, `AI-MISSING-TAB`, `AI-L-MESSAGE(n)` |
| `POST` | Posts one receivable + revenue + VAT; creates the open item and the GL journal. | everything `VALIDATE` needs plus `CA-CORRELATION-ID` | `AI-INVOICE-ID`, `AI-OPEN-ITEM-ID`, `AI-JOURNAL-NO`, `AI-DUPLICATE` |
| `PROFORMA` | Produces a pro forma: no AR open item, no VAT record, no journal. | as `VALIDATE` | totals only |

Input the caller **must** supply (REQ: AR-003, AR-004, AR-005, AR-023, AR-029):

* `AI-DOC-TYPE` (`INVOICE` / `PROFORMA`), `AI-CUSTOMER`, `AI-SOURCE`
  (`DESPATCH` | `DIRECT` | `SERVICE`), `AI-DOC-DATE`, `AI-TAX-POINT`, `AI-POSTING-DATE`.
* `AI-INVOICE-CUST` when the invoice goes to a different account from the delivery account
  (pay-for relationships are set up through `CUSTVAL RELATE`).
* **Terms**: either `AI-TERMS-CODE` (AR resolves and snapshots it) or a complete
  `AI-TERMS-SNAPSHOT` / `AI-DUE-DATE` / `AI-DISCOUNT-PCT` / `AI-DISCOUNT-DATE`.
  `AI-TERMS-SOURCE` tells you where the terms came from. **Terms are snapshotted onto the invoice**
  – later changes to the customer master never move an existing due date.
* **Currency**: `AI-CURRENCY` plus `AI-FX-RATE`/`AI-FX-RATE-DATE` for non-GBP. Leave the rate at zero
  and AR fetches it through `FXCONV` for `AI-FX-RATE-DATE` (defaults to the tax point).
* **VAT invoice content** (AR-023, spec section 10) is snapshotted, not re-derived at print time:
  `AI-BILL-NAME`, `AI-BILL-ADDRESS`, `AI-DELIVERY-ADDR`, `AI-CUST-VAT-NO`, `AI-OWN-VAT-NO`,
  `AI-VAT-LOCATION` (`GB` | `XI` | `EU` | `ROW`), `AI-SPECIAL-WORDING`, `AI-EVIDENCE-REF`.
  `AI-SIMPLIFIED` is set by AR when the gross is at or below the simplified-invoice limit
  (GBP 250, `core.Parameter` `AR.VAT.SIMPLIMIT`).
* **Lines** (up to 25 per call): `AI-L-ITEM`, `AI-L-DESC`, `AI-L-QTY`, `AI-L-UOM`, `AI-L-PRICE`,
  `AI-L-DISC-PCT`, `AI-L-CHARGE`, `AI-L-VAT-CODE`, `AI-L-REV-ACCOUNT`, `AI-L-COST-CENTRE`,
  `AI-L-PROJECT` and, from despatch, `AI-L-SO-LINE`. AR computes `AI-L-NET`, `AI-L-VAT-RATE` and
  `AI-L-VAT-AMT` through `ARVATWRT` – **do not pre-compute VAT in Sales**.
* `AI-DEPOSIT-APPLY` applies an existing customer deposit (`ARDEPOS`) to the new invoice.

Rules Sales must honour:

* **Rounding is per VAT code, not per line** – the per-code analysis in `AI-VAT-TAB` is authoritative
  and is what reaches `fin.VatTransaction`. Compare against `AI-VAT-GBP`, never a line sum.
* **Zero-rated, exempt and reverse-charge supplies need wording.** If `AI-SPECIAL-WORDING` is blank
  for a liability that requires it, `POST` is rejected with `AI-MISSING-TAB` naming the gap.
* **`CA-CORRELATION-ID` is the idempotency key.** Re-posting the same despatch returns `00` with
  `AI-DUPLICATE = "Y"` and the original `AI-INVOICE-ID`; it never creates a second receivable.
* Credit notes are **not** negative invoices – use `ARCRNOTE` (section 2.1).

### 1.2 `CREDCHK` – credit exposure and release decision (copybook `CREDCHKCA.cpy`)

**This is the interface Sales calls at order entry, order amendment and despatch release.**
`CALL "CREDCHK" USING WS-CK-CA` where `WS-CK-CA` is
`COPY CREDCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-CK-CA==.`

| `CA-FUNCTION` | Does | Gives back |
|---|---|---|
| `EXPOSURE` | Computes exposure only, **no decision and no side effect**. | the five legs, `CK-EXPOSURE`, `CK-LIMIT`, `CK-HEADROOM`, `CK-UTILISED-PCT` |
| `CHECK` | Exposure **plus** the release decision; writes a credit exception when held or blocked. | `CK-DECISION`, `CK-DECISION-TEXT`, `CK-EXCEPTION-ID` |
| `OVERRIDE` | Requests a scoped, expiring override. | `CK-OVERRIDE-ID`, `CK-OVERRIDE-STATUS`, `CK-APPROVER-PERM` |
| `APPROVE` | Approves an override. The approver may **not** be the requester. | `CK-OVERRIDE-STATUS`, `CK-OVERRIDE-ACTIVE` |
| `RELEASE` | Re-tests after an override was granted. | as `CHECK` |

Exposure (all sterling, never below zero) – REQ: AR-007, AR-008, SO-007, D-06:

```
CK-EXPOSURE = CK-OPEN-DEBT        open invoice residuals
            - CK-OPEN-CREDITS     eligible unapplied AR credits
            + CK-FIRM-ORDERS      firm unshipped orders
            + CK-SHIPPED-UNBILL   shipped but not yet invoiced
            - CK-DEPOSITS         eligible unapplied deposits
```

* Set `CK-ORDER-VALUE` (+ `CK-CURRENCY`/`CK-FX-RATE`, or let AR convert) to the **candidate**
  commitment. It is evaluated **once** – when re-checking an order that is already in
  `CK-FIRM-ORDERS`, pass its reference in `CK-EXCLUDE-REF` so it is not counted twice.
* `CK-CHECK-POINT` (`ORDER` | `AMEND` | `DESPATCH`) selects the parameter set; `CK-EVAL-DATE`
  defaults to `CA-BUSINESS-DATE`.
* **D-06 hold rule**: an account is held when the limit is exceeded **or** when overdue debt over
  `CK-OVERDUE-THRESH` is more than `CK-OVERDUE-TRIGGER` days past due. `CK-OVERDUE-GBP`,
  `CK-OVERDUE-DAYS` and `CK-OLDEST-DUE-DATE` explain which one bit. Disputed amounts are reported
  separately in `CK-DISPUTED-GBP` and are excluded from the overdue trigger.
* `CK-DECISION` is `RELEASE`, `HOLD` (releasable by an override) or `BLOCK` (account status,
  sanctions or insolvency – an override cannot clear it). `CK-DECISION-TEXT` is the sentence to show
  the user. `CK-PROFORMA-ONLY = "Y"` means the account may only be supplied against a pro forma.
* Overrides are **scoped, limited and expiring**: `CK-OVERRIDE-SCOPE` (`ORDER` / `ACCOUNT`),
  `CK-OVERRIDE-AMT`, `CK-OVERRIDE-FROM`, `CK-OVERRIDE-EXPIRY`, mandatory `CK-OVERRIDE-REASON`.
  There is no permanent, unlimited override.

---

## 2. Programs

### 2.1 Online (LINKable, `cobol\online\ar`)

| Program | Functions | What it is for | REQ |
|---|---|---|---|
| `CUSTVAL` | `VALIDATE` `ACTIVATE` `SUSPEND` `RETIRE` `RELATE` `SCREEN` | Account lifecycle: an account may not trade before it is complete and screened; retiring leaves outstanding items payable. | AR-001, AR-002 |
| `CREDCHK` | `EXPOSURE` `CHECK` `OVERRIDE` `APPROVE` `RELEASE` | Credit exposure, release decision, scoped overrides (section 1.2). | AR-007, AR-008 |
| `ARINVPST` | `VALIDATE` `POST` `PROFORMA` | Sales invoice posting (section 1.1). | AR-003, AR-004, AR-005, AR-023, AR-029 |
| `ARCRNOTE` | `CHECK` `POST` | Credit notes. A credit always identifies the original supply and can never exceed what is left creditable on it. | AR-006 |
| `RCPTALOC` | `RECEIPT` `CONFIRM` `PREVIEW` `ALLOCATE` `UNALLOC` | Receipts and allocation (section 3.2). | AR-009, AR-010, AR-011, AR-019 |
| `ARDEPOS` | `DEPOSIT` `APPLY` `REFUND` `ENQUIRE` | Customer deposits/advances – a deposit creates a tax point. | AR-013 |
| `DISHONOR` | `ASSESS` `DISHONR` | Dishonoured/returned receipts: reverses the receipt **and** its allocations, reinstating the debts. | AR-014 |
| `ARADJUST` | `ASSESS` `POST` | Balance adjustments. Balances are never edited: every movement is a reasoned, classified, authorised document. | AR-012, AR-020 |
| `ARDISPUT` | `DISPUTE` `PROMISE` `ACTION` `RESOLVE` `REVIEW` | The query log: disputes, promises to pay, contacts, dunning suppression. | AR-022, AR-024 |
| `ARINSOLV` | `RECORD` `ROTLIST` `PAPSENT` `HANDOFF` | Insolvency, retention of title, Pre-Action Protocol, agency hand-off. | AR-027, AR-028 |
| `LATEINT` | `CALC` `CLAIM` `APPROVE` `WAIVE` `ISSUE` | Statutory interest (base + 8%) and the fixed sum compensation (GBP 40/70/100). The claim invoice carries no VAT. | AR-019 |
| `BADDEBT` | `ASSESS` `CLAIM` `REPAY` | VAT bad debt relief: six months past the later of supply and due date, written off in the accounts, not sold on; repayment when the debt is later paid. | AR-026 |
| `DDMAND` | `LODGE` `ACTIVATE` `NOTICE` `SCHEDULE` `CANCEL` | Bacs Direct Debit mandates, advance notice and collection planning, AUDDIS codes. | AR-025 |
| `ARENQ` | `BALANCE` `METRICS` | Balance, ageing, credit position; turnover, DSO, receipt effectiveness. | AR-017, AR-021 |
| `ARVATWRT` | `LOOKUP` `WRITE` | VAT code lookup (rate, liability, mechanism, boxes) and the `fin.VatTransaction` digital record. | AR-029 |
| `ARPOSTGL` | `ACCOUNT` `POST` | The sales ledger interface into the nominal: resolves each event leg to an account, stages and posts the journal. | AR-020 |

### 2.2 Batch (`cobol\batch\ar`)

| Program | Output | What it does | REQ |
|---|---|---|---|
| `AGEDDEBT` | 132-column Report Writer spool | Aged receivables as at a selected date, current/30/60/90/120+ buckets, per company and per collector. | AR-017, AR-021 |
| `STATEMNT` | one 132-column spool per account | Customer statements, transaction style or open-item style, suppressed for accounts in dispute-only state. | AR-018 |
| `DUNNING` | spool + `ar.DunningCase` rows | Collection run: walks the overdue population, applies the escalation stages, honours promises and suppressions. | AR-015, AR-016 |
| `DDCOLL` | Bacs **Standard 18** collection file + spool | Selects mandates whose advance notice has expired, flags first (`01`) vs recurring (`17`) collections. | AR-025 |
| `EINVUBL` | one UBL 2.1 XML file per invoice | Peppol-style structured e-invoice extract. | AR-023, AR-029 |
| `ARRECON` | 132-column spool | Sales ledger control account reconciliation: ledger vs nominal, with the reconciling items named. The sales ledger balance proved against the control account is the **outstanding** amount - documents as raised less the applied, discount and write-off amounts of active allocations - which is what `AGEDDEBT` and `RECONCTL` measure. | AR-020, AR-021 |

Run them the normal way (`CMDARGS` style arguments, `JOBLOG` run record, `RUNPATH` for the spool
directory), e.g.:

```
.\bin\x86\AGEDDEBT.exe COMPANY=STC ASOF=2026-10-31
.\bin\x86\STATEMNT.exe COMPANY=STC ASOF=2026-10-31
.\bin\x86\DUNNING.exe  COMPANY=STC RUNDATE=2026-10-31
.\bin\x86\DDCOLL.exe   COMPANY=STC PROCDATE=2026-11-03
.\bin\x86\ARRECON.exe  COMPANY=STC ASAT=20261031
.\bin\x86\EINVUBL.exe  COMPANY=STC FROM=20260901 TO=20261031
```

Spools land in `run\spool\*.lst`, the Bacs Standard 18 file in `run\bacs\`, UBL documents in
`run\einv\`.

### 2.3 Stored procedures and views (`db\procs\ar`, `db\views\ar`)

| Object | Used by | Purpose |
|---|---|---|
| `ar.usp_CustomerEnquiry` | `frmCustomerEnquiry` | Five result sets: header, open items, ageing, recent activity, credit position. |
| `ar.usp_OpenItemsForAllocation` | `frmReceiptEntry` | Debit items, available credits and the write-off tolerance for one customer/currency. |
| `ar.usp_AgedDebtSummary` | `frmCreditControl`, report launcher | Aged buckets at a date, by company and collector. |
| `ar.usp_CreditControlWorklist` | `frmCreditControl` | Worklist with owner, dunning stage, promise and dispute state. |
| `ar.usp_CustomerStatementLines` | statement preview | Statement lines in transaction or open-item style. |
| `ar.vw_ArDebtors` | enquiries, cross-module reporting | One row per open item with customer, ageing bucket and dispute flag. |

---

## 3. Key flows

### 3.1 Order to cash (what Sales sees)

```
SO order entry ──► CREDCHK CHECK ──► RELEASE / HOLD / BLOCK
                       │ HOLD
                       └──► CREDCHK OVERRIDE ──► CREDCHK APPROVE ──► CREDCHK RELEASE
despatch ──► ARINVPST VALIDATE ──► ARINVPST POST ──► ar.OpenItem + ARVATWRT WRITE
                                        └──► ARPOSTGL POST ──► fin.JournalStage ──► fin.usp_PostJournal
bank ──► RCPTALOC RECEIPT ──► RCPTALOC CONFIRM ──► RCPTALOC ALLOCATE ──► settled
```

### 3.2 Receipts and allocation (`RCPTALOC`)

* `RECEIPT` records the money and its cash effect; `CA-CORRELATION-ID` (the bank reference) stops a
  repeated bank file from increasing cash twice. A confirmed receipt writes the credit open item it
  creates back into the commarea **whole** - `RA-SOURCE-ITEM-ID`, `RA-SOURCE-DOC`, `RA-SOURCE-TYPE`
  and `RA-SOURCE-AVAIL` - so the very next `ALLOCATE` settles from the money just banked
  (CR2611; a half-stated source used to leave the area naming one document by id and another by
  number, and the allocation was refused with `RCP0020`).
* **Money is named before it is applied.** `ALLOCATE` and `UNALLOC` need a source: a call
  that states neither `RA-SOURCE-ITEM-ID` nor `RA-SOURCE-DOC` is refused with `RCP0023` rather than
  adopting the first open item of the company - a stranger's credit (CR2612). `PREVIEW` with no
  named source values the receipt still being keyed - `RA-GROSS` in `RA-CURRENCY` for `RA-CUSTOMER`
  - as its source, so *Receipt Entry* can show the residual before anything is recorded; it reads
  and writes nothing for it (CR2616). With no amount or customer either, `PREVIEW` is still refused
  with `RCP0023`.
* **Unconfirmed evidence is not funds.** A receipt stays unusable until `CONFIRM`; allocating from an
  unconfirmed source is refused (`RCP0021`).
* `PREVIEW` shows the residual effect with no business effect at all.
* `ALLOCATE` applies up to 20 target items in one call. Each one is classified in `RA-A-KIND`:
  `EXACT`, `PARTIAL`, `DISCOUNT`, `WRITEOFF` or `CROSSCCY`.
* **D-22 tolerance**: up to **GBP 5.00** per receipt may be written off by the AR clerk **with a
  reason** (`RA-A-WO-REASON`); a short payment is never silently written off (`RCP0028`), and a
  larger difference is **held** at `06` (`RCP0029`) until someone with `AR.WRITEOFF.APPROVE`
  authorises it. A held allocation posts nothing.
* **Settlement discount** is only valid inside the stated terms plus the approved grace
  (`RCP0026`/`RCP0027`), and `ARADJUST` reclassifies a difference that falls inside the terms as
  `SETTLEDISC` (`ADJ0020`) rather than a write-off.
* **Cross currency** needs an explicit conversion instruction – equal-number matching across
  currencies is blocked (`RCP0025`). A same-currency receipt at another rate realises an exchange
  difference in `RA-A-FX-REALISED`.
* `UNALLOC` is the **authorised correction**: it needs a reason (`RCP0041`), reverses exactly one
  allocation, restores the invoice residual, leaves the original row in history with a linked contra
  row carrying the correction date, never touches the receipt itself, and refuses a second attempt
  (`RCP0043`).

### 3.3 Collections

```
AGEDDEBT ──► DUNNING ──► ar.DunningCase (stages) ──► ARDISPUT PROMISE / ACTION / RESOLVE
                              │ no payment
                              ├──► LATEINT CALC/CLAIM/APPROVE/ISSUE   (base + 8%, GBP 40/70/100)
                              ├──► ARINSOLV PAPSENT / HANDOFF
                              └──► ARADJUST POST (write off) ──► BADDEBT ASSESS/CLAIM
```

---

## 4. .NET client

* Facade: `dotnet\STERP.Business\Debtors\DebtorsService.vb` – `Enquiry`, `AgedDebt`, `Worklist`,
  `Allocatable`, `Statement`, `Metrics`, `CreditCheck`, `NewReceipt`, `AddAllocation`, `RunReceipt`,
  `CorrectAllocation`, `RaiseDispute`, `RecordPromise`. Enquiries go to the stored procedures,
  everything that decides or posts goes through `CobolGateway`/`CommArea` to the programs above.
* Forms: `dotnet\STERP.Desktop\Forms\Debtors\`
  * `frmCustomerEnquiry` – account, open items, ageing, activity, credit position (F5).
  * `frmReceiptEntry` – receipt entry, allocation grid, write-off tolerance, allocation correction (F6).
  * `frmCreditControl` – credit control worklist with owner, stage, promise and dispute state (F7).
  * Simple masters (credit limits/holds, overrides, DD mandates) use `frmGenericMaint` with the
    definitions seeded in `52-ar-menu.sql` (`AR.CREDIT`, `AR.OVERRIDE`, `AR.DDMANDATE`).
* Menu and report rows are seeded in `db\seed\52-ar-menu.sql` (menu bar `AR`, report launcher entries
  for `AGEDDEBT`, `STATEMNT`, `DUNNING`, `ARRECON`).

---

## 5. How to demo

1. Deploy and seed: `build\build-db.ps1 -Only "ar-"` (run it twice – the scripts are idempotent).
2. Build: `build\build-cobol.ps1 -Module ar`, then `build\build-net.ps1`.
3. Start the client, log on as `ARCLERK` (clerk) or `CREDMGR` (credit manager).
4. **Customer enquiry** (`AR ▸ Customer Enquiry`, F5): `C0001` has invoices across every ageing
   bucket, a dispute and a part-paid invoice; `C0007` is over its limit and on hold.
5. **Receipt entry** (F6): enter a receipt for `C0001`, allocate it over two invoices, leave a 4.00
   difference and write it off with a reason (accepted), then try 19.00 (held for approval).
   Use *Correct allocation* to reverse one allocation and watch the residual come back.
6. **Credit control worklist** (F7): the overdue population with owners and dunning stages; raise a
   dispute and a promise to pay and see the account drop out of the next dunning run.
7. **Reports**: `AR ▸ Debtors Reports` runs `AGEDDEBT`, `STATEMNT`, `DUNNING` and `ARRECON`; the
   spools appear in `run\spool`. `DDCOLL` writes a Bacs Standard 18 file to `run\bacs`.

---

## 6. Tests

| Suite | Covers |
|---|---|
| `cobol\tests\ar\TARINV.cob` | `ARINVPST`: VAT content, terms snapshot, idempotent posting. |
| `cobol\tests\ar\TARCRED.cob` | `CREDCHK`: the five exposure legs, D-06 hold rule, scoped overrides. |
| `cobol\tests\ar\TARCASH.cob` | `RCPTALOC` + `ARADJUST`: confirmation, allocation, D-22 write-off tolerance, settlement discount, cross currency, allocation correction. |
| `cobol\tests\ar\TARLATE.cob` | `LATEINT`: base + 8% statutory interest and the GBP 40/70/100 compensation bands. |
| `cobol\tests\ar\TARCOLL.cob` | `BADDEBT` + `DDMAND`: bad debt relief eligibility and repayment, Bacs mandate and advance notice rules. |
| `db\tests\ar-ageing.sql`, `ar-credit.sql`, `ar-enquiry.sql` | The procedures behind the screens. |
| `dotnet\STERP.Tests\Debtors\DebtorsTests.cs` | Facade, forms, menu rows and the seeded demo data. |

Run them with `build\run-tests.ps1 -Layer cobol -Filter TAR`, `-Layer sql -Filter ar`,
`-Layer net -Filter Debtors`.

## Acceptance defect fixes (2026-10-07)

* **No active number series for a collection case (AC-030).** `ARDISPUT` and `ARINSOLV` ask `NUMGEN`
  for series `ARCASE`, and `DDMAND` asks for `DDMAN` and `DDCOL`; none were seeded, so a dispute was
  refused with `STERP-E-0004 / DSP0061`. `db\seed\52-ar-menu.sql` now seeds all three for every
  company. Covered by `db\tests\ar-numbering.sql`.
* **Collection action insert (AC-024).** `ARDISPUT` wrote the promise amount into
  `NULLIF(?,0)`; the int zero made SQL Server convert the edited money value `"0.00"` to `int` and
  the dispute was rolled back. The parameter is now cast to `decimal(18,2)` before `NULLIF`.
* **Owned exceptions (COM-022).** `CREDCHK` names `CREDMGR` as the owning user, not only the role.
* **ARRECON report catalogue.** The `core.ReportDef` row now carries `CO,PERIOD,ASAT,USER`, the
  arguments `ARRECON.exe` actually parses (`db\seed\97-xmd-reports.sql` no longer has to patch it).
* **A permission hold must stop the post (CR2609).** `ARINVPST 1000-POST-INVOICE` tested only
  `AR-RC-BAD` (`>= 8`) after `1050-CHECK-PERMISSION`, so a user without `AR.INVOICE.POST` got
  `AR-RC 6` **and the invoice posted anyway** - `SOINVRUN` then rolled back, reported nothing posted
  and never wrote `BilledQty` back. The test is now `AR-RC-BAD OR AR-RC = 6`: a service that answers
  06 has had no business effect. `AR.INVOICE.POST` is held by `ARCLERK` and `SYSADM` only.
* **Residual after a dishonour (AR-014).** `DISHONOR` returned `DH-I-RESID-AFTER` as a hard zero.
  Each listed item now reports what the customer is left owing: on `ASSESS` the residual the item
  carries plus the allocation that would come off it, and on `DISHONR` the residual re-read from
  `ar.OpenItem` once the reversal is committed. Covered by `TARCASH` (a real dishonour, built and
  cleaned up on `ZZCASH` keys) and `TARGAP2`.
* **Dividend received in sterling (AR-027).** `IS-DIVIDEND-GBP` was never populated. The caller now
  states `IS-DIVIDEND-AMT` with `IS-DIVIDEND-CCY` / `IS-DIVIDEND-DATE` (the currency defaults to the
  account's, the date to the event date); `ARINSOLV` converts a foreign dividend through `FXCONV`
  `TOGBP` at the spot rate, passes sterling through untouched, refuses a negative amount
  (`INS0041`), and writes the amount into the insolvency case narrative. The dividend never writes
  debt off: it is reported alongside the outstanding balance. Covered by `TARGAP2`.
* **Party relationships read back (AR-002).** `CUSTVAL` wrote `InvoiceCustomerCode`,
  `PayerCustomerCode` and `StatementCustomerCode` on `RELATE` but never returned them.
  `0300-READ-ACCOUNT` now reads all three into `CV-INVOICE-CUST` / `CV-PAYER-CUST` /
  `CV-STATEMENT-CUST` on every function, and `RELATE` reports the role it has just set.
  Covered by `TARGAP1`.
