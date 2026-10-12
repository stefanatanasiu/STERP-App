# Creditors and Supplier Settlement (module `ap`)

Purchase Ledger. Ported from MVS in 1991, so the COBOL dialect is **IBM Enterprise** (`ap=ibm` in
`cobol\DIALECTS.cfg`), fixed format, `*>` comments, `SECTION`-per-paragraph batch style.

* Spec: `04-Creditors-and-Supplier-Settlement.md`, requirements **AP-001 .. AP-031**
* Schema: `ap` (plus `fin.JournalStage` / `fin.usp_PostJournal` for all postings)
* Online programs: `cobol\online\ap\*.cob` – every one is LINKable (`DFHCOMMAREA` + `<PROG>CA.cpy`)
* Batch programs: `cobol\batch\ap\*.cob`
* Tests: `cobol\tests\ap\T*.cob`, `db\tests\ap-*.sql`

---

## Desktop payment preparation

Supplier Verification now offers **Change bank** (GB/GBP Bacs): key a new unique bank reference,
beneficiary, six-digit sort code, eight-digit account, bank name and request source.
The bank reference is at most **10 characters**; overlength values are refused by the
dialog and facade before calling the engine, rather than truncated into a different key.
`APBANKVR PREPARE` creates a **PENDING** record superseding the current destination only
after approval, never an immediately usable destination.
**CoP check** records the result, evidence reference and authorised decision (mandatory for
anything other than MATCH). **Record callback** requires the pre-existing contact source,
an explicit Y/N on-file declaration and the independent callback evidence. Obtain an
independent **Approve bank** decision afterwards. Maintenance, verification and approval
retain `AP.BANK.MAINT`, `AP.BANK.VERIFY` and `AP.BANK.APPROVE`; APCLERK has no bank-maintenance
grant. Use separate authorised people for change preparation and callback: the seeded
SYSADM may record CoP/callback after APMGR prepares the change, without extra grants;
TREASURY still makes the bank approval decision.

Payment Run offers **Build proposal**: cutoff/payment dates (`dd/MM/yyyy`), method and
company bank account (for example BACS / GBP1). APPAYPRP assigns the new proposal reference;
the screen displays that returned reference. This is the APPAYPRP SELECT
preview; **Reserve** uses its existing header/dates and APPAYPRP RESERVE before
**Request approval**. **Release reservations** frees the reservation; it is not authority
to transmit a payment. APCLERK prepares, then TREASURY and TREASURY2 independently approve
the exact content; preparer/self/duplicate approval refusals remain engine errors.
**Create instructions** retains APPAYRUN controls. **Bacs file** opens the existing APBACS
report launcher with RUN populated and the signed-on USER passed to the batch program.
Run produces the local Standard 18 file under `run\bacs`, not an external transmission.
The report and instruction operations still require `AP.PAYMENT.RELEASE`.
The Creditors menu has three permission-specific routes to the same payment form:
**Payment Proposal and Run** (`AP.PAYMENT.PROPOSE`), **Authorise Payment Run**
(`AP.PAYMENT.APPROVE`) and **Release Payment Run** (`AP.PAYMENT.RELEASE`).
Reapply `db\seed\61-ap-menu.sql` to deploy these routes; no role grants change and opening
the form does not confer authority for any of its actions.
**Approve Supplier Bank Changes** similarly reaches Supplier Verification using
`AP.BANK.APPROVE`, so treasury need not acquire supplier-maintenance/view rights to
approve independently verified bank changes.

### Normal demo baseline: a fresh interactive run

`APDEMO MODE=COMMIT` now leaves one STC/S0002 utility invoice **after** its example
released proposal: supplier reference `APDEMO-GUI-UTILITY-2026`, gross GBP 148.14.
Deploy `db\procs\ap\usp_ReserveProposal.sql` as well: credit-only suppliers are listed
as exclusions rather than subtracting their credits from another supplier's cash
requirement. This keeps the approved outflow equal to the fresh S0002 instruction.
APINVREG validates the allowed UTILITIES non-PO category; matching is legitimately
NOTREQUIRED. APPOST posts expense 6800/ADMIN, recoverable VAT 1250 and creditors 2000;
APMGR releases the invoice block through APBLOCK. The original payment reservations
are not released, changed or reused. Re-running APDEMO reuses the register/journal and
does not replenish a payable already selected by an interactive run.

Rebuild the AP batch programs, then run the normal `build\demo-data.ps1` preparation
against the intended isolated demo database (or re-run APDEMO COMMIT after an existing
baseline). In Payment Run, APCLERK uses **Build proposal**, cutoff `30/11/2026`,
payment date `02/12/2026`, method BACS, company bank GBP1; keep the returned PPS reference.
Reserve, request approval, obtain distinct TREASURY/TREASURY2 signatures, create
instructions, then **Bacs file / Run** under TREASURY. The due date comes from S0002's
N45 terms; the native Bacs calendar owns processing dates. The local file does not
transmit a payment. `ApDemoFixtureTests` checks normal-baseline reconciliation,
idempotency and this separate payment/Bacs route in its dedicated AP demo test database.

### Local Bacs Standard 18 layout

APBACS emits the single-processing-day variant: VOL1, HDR1, HDR2, UHL1,
one 100-byte credit record per payee, one 100-byte debit contra, EOF1, EOF2,
then UTL1. Every label is exactly 80 bytes, including trailing spaces, and
every physical record ends in CRLF. HDR2 declares `00100`; there is no
multi-day date appended to data. UHL1 contains the native-calendar processing
day (the banking day **before** value date), not the input/submission date.
The catalogue/launcher prompts only for CO and RUN. There is deliberately no
ENTRYDATE override: the approved run's stored PayDate is authoritative.
The launcher supplies USER from the signed-on session, not an editable prompt.
Reapply `db\seed\61-ap-menu.sql` to refresh existing catalogue definitions.

For the £148.14 utility payment, lengths are
`80,80,80,80,100,100,80,80,80` (778 bytes including CRLF). Both data amounts
are `00000014814` at columns 36–46. UTL1 has 13-digit debit total at columns
5–17, equal credit total at 18–30, debit count `0000001` at 31–37, and
credit count at 38–44. A multi-payee file still has **one** debit contra,
not one debit per credit.

`ApBacsFormatTests.Standard18PhysicalRecords` validates actual native bytes,
field positions, label order, dates, padding, balanced amounts and asymmetric
multi-payee counts using PREVIEW in an isolated AP demo database; it checks
that instruction statuses/file references remain unchanged. No transmission
is performed. The format reference is the
[HSBC Standard 18 implementation guide](https://www.hsbcnet.com/-/media/hsbcnet/client-transition/bacs-standard-18-mig-feb-2022.pdf),
pages 2–5 and 10–11. Sponsor-specific bank acceptance is not implied.
Native APBACS retains its START run ID through END: normal completion is OK/0,
reproduction is WARNING/4, and failures are FAILED. JOBLOG failures are explicit
`STERP-E-0201`/`STERP-E-0501` errors, not ignored successes. The format regression
also verifies both launcher/native runs are ended with matching severity and the
native file/spool references. Existing historical RUNNING rows are not repaired.

## 1. Interfaces other teams need (read this first)

### 1.1 MATCH3W – three-way matching (`APMATCH`, copybook `APMATCHCA.cpy`)

**This is the interface the Purchasing (`po`) team calls.** `APMATCH` is a pure LINK program:
`CALL "APMATCH" USING WS-MATCH-CA` where `WS-MATCH-CA` is
`COPY APMATCHCA REPLACING ==DFHCOMMAREA== BY ==WS-MATCH-CA==.`

The COMMAREA is `STCAHDR` (the standard STERP header: `CA-FUNCTION`, `CA-COMPANY`, `CA-SITE`,
`CA-USER`, `CA-BUSINESS-DATE`, `CA-CORRELATION-ID`, `CA-RETURN-CODE`, `CA-MESSAGE`) followed by
`MT-BODY`.

| `CA-FUNCTION` | Does | Needs on input | Gives back |
|---|---|---|---|
| `TOLCHK` | **Tolerance arithmetic only, no database.** Safe to call from any module. | the `MT-INV-*`, `MT-ORD-*`, `MT-RCV-*` and `MT-TOL-*` fields | `MT-*-VAR-*`, `MT-QTY-OK`/`MT-PRICE-OK`/`MT-CHARGE-OK`/`MT-VAT-OK`, `MT-WITHIN-TOLERANCE`, `MT-TOLERANCE-NOTE` |
| `MATCH` | Propose a match for **one** invoice line; reads order, receipt and previously matched quantities. | `MT-SUPPLIER`, `MT-INVOICE-ID` or `MT-REGISTER-NUMBER`, `MT-LINE-NUM` | everything `TOLCHK` gives plus `MT-AVAILABLE-QTY`, `MT-PREV-MATCHED`, `MT-MATCHED-QTY`, `MT-GRNI-CLEARED`, `MT-MATCH-STATUS` |
| `MATCHALL` | Same for every line of the document. | `MT-SUPPLIER`, `MT-INVOICE-ID` | `MT-LINES-MATCHED`, `MT-LINES-VARIANCE`, `MT-LINES-UNMATCHED`, `MT-INVOICE-STATE` |
| `CONFIRM` | Writes the accepted `ap.InvoiceMatch` rows and clears GRNI. | the proposal returned by `MATCH`/`MATCHALL` | `MT-MATCH-ID`, `MT-INVOICE-STATE` |

Key rules the caller must honour (REQ: AP-006, AP-007, decision D-08):

* **Four separate tolerances**, never one combined figure:
  `MT-TOL-QTY-PCT`, `MT-TOL-PRICE-PCT` / `MT-TOL-PRICE-UNIT` / `MT-TOL-PRICE-GBP`,
  `MT-TOL-CHARGE-GBP` and `MT-TOL-VAT-GBP` (plus `MT-TOL-ROUND-GBP` for pure rounding).
  If they are left at zero `APMATCH` loads them from `core.Parameter`
  (`AP.TOL.QTYPCT`, `AP.TOL.PRICEPCT`, `AP.TOL.PRICEGBP`, `AP.TOL.CHARGEGBP`,
  `AP.TOL.VATGBP`, `AP.TOL.ROUNDGBP`) via `PARMGET`.
* **Matchable quantity is the accepted receipt quantity only.** Quarantined
  (`MT-RCV-QUARANTINE`) and returned (`MT-RCV-RETURNED`) quantities are *not* available, and
  `MT-AVAILABLE-QTY = MT-RCV-ACCEPTED - MT-RCV-RETURNED - MT-PREV-MATCHED`.
* **Units of measure are converted before comparison.** Give the invoice UOM in `MT-INV-UOM` and
  the factor to stock UOM in `MT-INV-CONV`; `APMATCH` fills `MT-INV-STOCK-QTY`.
* **Price is compared per `MT-INV-PRICE-PER`** (price per 1 / 100 / 1000). A mismatch of
  `MT-INV-PRICE-PER` against `MT-ORD-PRICE-PER` is a price variance, not an error.
* `MT-ZERO-REFERENCE = "Y"` means the order or receipt could not be found at all – that is a
  *failure to match*, not a within-tolerance match, and `MT-WITHIN-TOLERANCE` stays `"N"`.
* `MT-BASIS` picks the comparison: `QUANTITY` (goods), `VALUE` (services/charges) or
  `MILESTONE` (contract stage). Services have no receipt, so pass `MT-SERVICE-REF`.
* `MT-NEEDS-APPROVAL = "Y"` means out of tolerance but inside the auto-approval ceiling –
  the invoice is blocked (`APBLOCK`) rather than rejected.

`CA-RETURN-CODE`: `0` ok, `6` matched with variance / needs approval, `8` cannot match
(reason in `CA-MESSAGE`), `12` data or permission error.

### 1.2 Invoice register (`APINVREG`, copybook `APINVREGCA.cpy`)

`CALL "APINVREG" USING WS-INVREG-CA`. Header `STCAHDR` then `IR-BODY` (max **40** coded lines,
`IR-LINE OCCURS 40`, and up to **10** similar documents in `IR-SIMILAR OCCURS 10`).

| `CA-FUNCTION` | Does |
|---|---|
| `DUPCHK` | Exact-duplicate and similar-document scan only (no write). |
| `VALIDATE` | Validate header + coded lines, calculate VAT and sterling amounts, no write. |
| `REGISTER` | `VALIDATE` then write `ap.PurchaseInvoice` / `...Line` / `...Vat` and the open item. |
| `SETSTATE` | Change registration / owner / block state. |
| `GET` | Read a registered document back into the COMMAREA. |

What Purchasing needs to know:

* **Duplicate control (REQ: AP-004).** `IR-SUPPLIER-REF` is normalised into `IR-REF-NORMALISED`
  (upper-cased, punctuation and leading zeros stripped) before comparison. An exact hit on
  supplier + normalised reference sets `IR-DUP-FOUND = "Y"`, `IR-DUP-REGISTER` to the earlier
  register number and `IR-REG-STATE` to `DUPLICATE`; `REGISTER` then refuses with
  `CA-RETURN-CODE = 8`. **Similar** documents (same supplier and gross within the rounding
  tolerance, or same reference for a different supplier, or same gross and date) are returned in
  `IR-SIMILAR(1..IR-SIMILAR-CNT)` with the reason in `IR-S-REASON` – these are a *warning*
  (`CA-RETURN-CODE = 4`), the clerk may proceed.
* **Stated versus calculated.** The supplier's own figures go in `IR-STATED-NET/VAT/GROSS`;
  `APINVREG` computes `IR-CALC-*` from the lines and reports the difference in
  `IR-NET-DIFF` / `IR-VAT-DIFF`. A difference outside `AP.TOL.ROUNDGBP` is a validation failure.
* **Line coding** is done by `APINVLIN` (copybook `APLINWRK.cpy`, functions `CODECHK`, `VATCALC`,
  `BOTH`) which `APINVREG` calls per line – PO lines take their coding from the order,
  non-PO lines (`IR-IS-NON-PO = "Y"`) must carry `IR-L-ACCOUNT` and a `IR-NONPO-REASON`.
* **Matching hand-off.** After `REGISTER`, `IR-MATCH-STATE` is `UNMATCHED` for any line that
  carries `IR-L-ORDER`; the caller then drives `APMATCH MATCHALL`. Lines with no order reference
  never enter matching.
* `IR-SOURCE-CHANNEL` (`PAPER`/`PDF`/`EINVOICE`/`BULK`) controls which evidence is demanded by
  `APVATEVD` – `EINVOICE` documents are loaded by the `APEINV` batch from
  `run\einvoice\in` (REQ: AP-031).

---

## 2. Online programs

| Program | Copybook | Requirements | Purpose |
|---|---|---|---|
| `APSUPVAL` | `APSUPVALCA` | AP-001, AP-024, AP-030 | Supplier lifecycle, due diligence, purchasing/payment holds, `TERMCHK` against the D-25 maximum, `CANPAY`. |
| `APBANKVR` | `APBANKVRCA` | AP-002, AP-027 | **BANKVER.** Bank-detail change: `PREPARE`, `COP` (Confirmation of Payee), `CALLBACK` (independent call-back on a previously held number), `APPROVE` (independent approver, never the requester), `CHECK`. |
| `APINVREG` | `APINVREGCA` | AP-003, AP-004, AP-005, AP-008, AP-009, AP-023, AP-031 | Invoice/credit register, duplicate and similar detection. |
| `APINVLIN` | `APLINWRK` | AP-005, AP-021, AP-025 | Line coding, extension, charge, VAT and sterling amounts. |
| `APVATEVD` | `APVATEVDCA` | AP-021, AP-025 | `EVIDENCE`, `SPLIT` (recoverable/non-recoverable), `RCHARGE` (reverse charge), `SIXMONTH`, `REPAY`, `RECLAIM`, import VAT (C79) evidence. |
| `APMATCH` | `APMATCHCA` | AP-006, AP-007 | Three-way match, four tolerances. |
| `APBLOCK` | `APBLOCKCA` | AP-008, AP-009 | `BLOCK`, `RELEASE`, `DISPUTE`, `DUEDATE`, `PENDING`. |
| `APCRDLNK` | `APCRDLNKCA` | AP-010, AP-017 | Credit notes linked to returns, `ADVANCE`/`APPLYADV`/`REFUND` prepayments, `SETOFF`. |
| `APPAYPRP` | `APPAYPRPCA` | AP-011, AP-020 | Payment proposal: `SELECT` (preview), `RESERVE`, `RELEASE`, `TOTALS`, `METHOD` routing. |
| `APPAYAPR` | `APPAYAPRCA` | AP-012 | Exact-content approval: `HASH`, `REQUEST`, `DECIDE`, `VERIFY`, `INVALID`. |
| `APPAYRUN` | `APPAYRUNCA` | AP-013, AP-016 | Run statuses `CREATE`/`SUBMIT`/`OUTCOME`/`SETTLE`/`RETURN`. |
| `APALLOC` | `APALLOCCA` | AP-014, AP-018 | Allocation, `UNALLOC`, `RESIDUAL`, `FXREAL` realised exchange difference. |
| `APSTMREC` | `APSTMRECCA` | AP-019 | Supplier statement `LOAD`/`LINE`/`RESOLVE`. |
| `APLATEPY` | `APLATEPYCA` | AP-030 | Late payment interest and compensation `CALC`/`CLAIM`/`AGREE`/`TERMCHK`. |
| `APPOST` | `APPOSTWK` | AP-021, AP-022 | `STAGE` / `POST` / `CLEAR` through `fin.JournalStage` + `fin.usp_PostJournal`. |
| `APHASH` | – | AP-012 | Content hash subroutine used by `APPAYAPR`. |
| `APSVC` | `APSVCARE` | – | Thin wrapper over `PERMCHK`, `PARMGET`, `AUDITLOG`, `EXCPRAIS`, `NUMGEN`, `PERIODCK`. |

## 3. Batch programs (`cobol\batch\ap`)

| Program | Requirements | Output | Run as |
|---|---|---|---|
| `APBACS` | AP-015, AP-016 | **Bacs Standard 18** direct-credit file in `run\bacs\` | `APBACS CO=STC RUN=<payrun> USER=APMGR` |
| `APREMIT` | AP-016 | Remittance advices, 132-col Report Writer, `run\spool\` | `APREMIT CO=STC RUN=<payrun> USER=APMGR` |
| `APAGED` | AP-026 | Aged creditors + cash requirement, `run\spool\` | `APAGED CO=STC ASAT=yyyymmdd USER=APMGR [MODE=DUE|INVOICE] [SUPP=S0011]` |
| `APPPR` | AP-028 | Payment practices report **RPT-018**, `run\spool\` + `ap.PaymentPracticesReport` | `APPPR CO=STC FROM=yyyymmdd TO=yyyymmdd USER=APMGR` |
| `APVAT6M` | AP-025 | 6-month unpaid input VAT repayment list | `APVAT6M CO=STC ASAT=yyyymmdd USER=APMGR` |
| `APEINV` | AP-031 | UBL e-invoice receipt from `run\einvoice\in` into `ap.EInvoiceInbox` then `APINVREG` | `APEINV CO=STC USER=APBATCH` |
| `APBULK` | AP-029 | Bulk invoice / opening-balance load | `APBULK CO=STC FILE=<name> MODE=INVOICE\|OPENING USER=APBATCH` |
| `APCLOSE` | AP-022 | Period-close reconciliation of the AP control account | `APCLOSE CO=STC PERIOD=yyyymm USER=APMGR` |
| `APDEMO` | AP-011, AP-012, AP-013 | A live, approved and released demonstration payment run | `APDEMO CO=STC USER=APCLERK MODE=COMMIT` |

**Payment routing (REQ: AP-015, decision D-14).** `APPAYPRP METHOD` and `APBACS` agree on:
Bacs is the default; a payment **over GBP 250,000** is routed to **CHAPS**; a payment marked
urgent and at or under the FPS limit goes by **Faster Payments**. Only the Bacs-routed
instructions reach the Std-18 file; CHAPS/FPS instructions are listed for manual release.

## 4. Stored procedures and views

| Object | Purpose |
|---|---|
| `ap.usp_ReserveProposal` | Reserves open items into a proposal **under `UPDLOCK, READPAST`** so two concurrent proposals cannot select the same invoice (REQ: AP-011). |
| `ap.usp_InvalidatePaymentApprovals` | Invalidates approvals whose content hash no longer matches (REQ: AP-012). |
| `ap.usp_AgedCreditors` | Aged creditor buckets and cash requirement for the enquiry form and `APAGED`. |
| `ap.usp_PaymentPracticesData` | Aggregates the RPT-018 figures for `APPPR`. |
| `ap.vw_OpenItemAge` | Open items with age, due date and block state. |
| `ap.vw_SupplierBalance` | Per-supplier balance, unapplied credits and advances. |
| `ap.vw_InvoiceMatchState` | Per-invoice match / block / payment eligibility state. |

## 5. Forms and menu

* **Supplier and bank verification** (`SupplierBankForm`) – supplier holds plus the BANKVER
  CoP / call-back / independent-approval sequence.
* **Invoice register and matching** (`InvoiceRegisterForm`) – register a document, see duplicates
  and similar documents, drive `MATCHALL`, block / dispute.
* **Payment proposal and run** (`PaymentRunForm`) – build and reserve a proposal, request
  approval, create the run, produce the Bacs file and remittances.
* Menu rows and `core.ReportDef` rows are in `db\seed\61-ap-menu.sql`.

## 6. How to demo

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-db.ps1 -Only "60-ap|61-ap|62-ap|90-ap|procs\\ap|views\\ap"
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-cobol.ps1 -Module ap
powershell -NoProfile -ExecutionPolicy Bypass -File build\run-tests.ps1 -Layer cobol -Filter TAP
```

1. **Supplier and bank** – open *Creditors ▸ Supplier and bank verification*, pick `S0002`,
   change the account number: the destination stays `PENDING` until CoP, call-back and an
   independent approval are all recorded, and `APPAYPRP` refuses to pay it meanwhile.
2. **Invoice register** – *Creditors ▸ Invoice register and matching*. Register `INV-55010`
   for `S0001`; re-enter the same reference and watch the duplicate refusal, then enter
   `inv 55010` and watch the *similar document* warning.
3. **Matching** – the same form, press **Match all**: line 1 matches, line 2 shows a price
   variance inside tolerance, line 3 is blocked for approval.
4. **Payment** – *Creditors ▸ Payment proposal and run*. Build, reserve, approve, create the run,
   then `APBACS` writes `run\bacs\` and `APREMIT` writes the remittances to `run\spool\`.
5. **Reports** – `APAGED`, `APPPR` (RPT-018) and `APVAT6M` all land in `run\spool\`.

### The same payment run without a screen (`APDEMO`)

`build\demo-data.ps1` runs `bin\x86\APDEMO.exe` automatically. It does by batch exactly what the
Payment proposal and run form does by hand, through the ordinary COMMAREAs:

| Step | Program / function | User | Outcome |
|---|---|---|---|
| 1 | `APPAYPRP RESERVE` | `APCLERK` | proposal built, due invoices reserved, exclusions listed |
| 2 | `APPAYAPR REQUEST` | `APCLERK` | `APHASH` content hash stored, approval raised |
| 3 | `APPAYAPR DECIDE` | `TREASURY` | first authorisation – rc 04, a second is still needed |
| 4 | `APPAYAPR DECIDE` | `TREASURY2` | run `APPROVED` (D-07: two independent authorisers) |
| 5 | `APPAYRUN CREATE` | `TREASURY` | payment instructions created, proposal `RELEASED` |

Four duties, four signatures: `SYSADM` never appears and `FINCTRL` cannot originate a run (a
`USER=` of either is treated as the operator and the preparer falls back to `APCLERK`).
`db\seed\91-ap-paydemo.sql` seeds `TREASURY2` (deputy treasury authoriser) because only the
`TREASURY` role carries `AP.PAYMENT.APPROVE` and `APR.PAYRUN.AUTHORISERS` is 2.

The driver is re-runnable: if a proposal prepared by the same user already exists for the pay
month it reports it and stops at rc 0, so nothing is reserved twice. Arguments
`PAYDATE=`, `CUTOFF=`, `PREPARER=`, `APPROVER=`, `APPROVER2=` and `RELEASER=` override the
defaults. It posts nothing to the ledger – only reservations – so `RECONCTL` stays at rc 0.

```powershell
bin\x86\APDEMO.exe  CO=STC USER=APCLERK MODE=COMMIT
bin\x86\APBACS.exe  CO=STC RUN=<ref from the listing> ENTRYDATE=20261009 USER=TREASURY
bin\x86\APREMIT.exe CO=STC RUN=<ref> USER=TREASURY
```

**First cut or reproduction (REQ: AP-013).** `APPAYAPR VERIFY` answers `APA0020` for a `RELEASED`
run because the approval covers an *approved* proposal, so `APBACS` decides for itself: when the
run was never cut (every instruction still `APPROVED` with no `BacsFileRef`), the content hash
still matches the authorised hash and two authorisers signed it, the file is an **original** and
the job ends rc 0 (`STERP-I-0253`). Once the file has been cut – `APBACS` stamps the instructions
through `APPAYRUN SUBMIT` – any further cut is a **reproduction**: rc 4 and `STERP-W-0252`, which
is what the seeded, already-settled run `PP-26-0007` gives.

`jobs\PAYRUN.cmd` needs no change: STEP020 already lets `APPROVED` and `RELEASED` through to
`APBACS`, and with `RUN=` omitted it picks the most recent proposal, which is the `APDEMO` one.

## Six month input VAT, e-invoicing and the AP test suites (AP-029, AP-031)

| Program | Kind | Requirement | What it does |
| --- | --- | --- | --- |
| `AP6MDATE` | callable, `cobol\online\ap` | AP-029 | The six month rule in one place: later of supply/due date, a calendar month shift clamped to month end (31 Aug + 6 = 28/29 Feb), the repayable proportion of a part paid invoice (half a penny rounds up) and the reclaim once paid. No database. Copybook `AP6MDTCA`. |
| `APXMLGET` | callable, `cobol\online\ap` | AP-031 | Takes one record of a UBL e-invoice apart: tag, value, attributes, container open/close, the five XML entities, `yyyy-mm-dd` dates and UBL numbers. No database. Copybook `APXMLGTC`. |
| `APVAT6M` | batch, Report Writer | AP-029, AP-025 | `APVAT6M CO=STC ASAT=yyyymmdd USER=APMGR [MODE=PREVIEW\|COMMIT]`. Pass one lists invoices whose reclaimed input VAT must go back on the return covering the six month date; pass two reclaims, once, the VAT on invoices repaid earlier and since paid. COMMIT marks `ap.VatEvidence` (`REPAID` / `RECLAIMED`) and posts `AP.VATREPAY` / `AP.VATRECLAIM` through `APPOST` under a per invoice correlation id, so a restarted run cannot repay twice. |
| `APEINV` | batch, Report Writer | AP-031, AP-003/004/023 | `APEINV CO=STC USER=APCLERK [MODE=PREVIEW\|COMMIT]`. Reads Peppol BIS Billing 3.0 UBL from `run\einvoice\in`, registers each document through the ordinary `APINVREG` COMMAREA (same duplicate, validation and coding rules as a keyed invoice), writes `ap.EInvoiceInbox`, gives every document an accepted/rejected result line and moves the file to `einvoice\processed` or `einvoice\rejected`. PREVIEW parses and runs `DUPCHK` only - nothing is written and nothing is moved. |

Sample documents live in `data\einvoice-samples\`: `INV-2026-5541.xml` and
`INV-2026-5541-RESENT.xml`, the same invoice with a different case and padding, which is the
AC-125 point - the two copies are one document, not two. Copy them into `run\einvoice\in` and run
`APEINV`.

### Seeds

* `db\seed\62-ap-eventmap.sql` - ten AP business events (`ADVANCE`, `ADVAPPLY`, `ADVREFUND`,
  `SETOFF`, `LATEPAY`, `REVCHARGE`, `VATREPAY`, `VATRECLAIM`, `GRNICLEAR`, `PAYRETURN`) and six
  `core.Parameter` rows. `AP.VATREPAY` is DR 2050 / CR 1250 and `AP.VATRECLAIM` the reverse.
* `db\seed\61-ap-menu.sql` - three form menu rows, five report launcher rows and the
  `core.ReportDef` rows for `APAGED`, `APREMIT`, `APBACS`, `APPPR` and `APVAT6M`.

### Tests

| Suite | Assertions | Database | Covers |
| --- | --- | --- | --- |
| `cobol\tests\ap\TAPMATCH.cob` | 45 | no | `APMATCH` function `TOLCHK`, AC-030 worked numbers |
| `cobol\tests\ap\TAPINVREG.cob` | 30 | yes | register, duplicates and similar documents, AC-029 |
| `cobol\tests\ap\TAPVAT6.cob` | 34 | no | `AP6MDATE`, AC-126 worked numbers (due 31/01/2027, repaid on the 2027Q3 return, reclaimed in September 2027) |
| `cobol\tests\ap\TAPEINV.cob` | 27 | no | `APXMLGET` against fragments of the sample UBL files, AC-125 |
| `cobol\tests\ap\TAPDEMO.cob` | 17 | yes | the whole payment run: `APPAYPRP RESERVE`, `APPAYAPR REQUEST/DECIDE` (preparer refused, two authorisers), `APPAYRUN CREATE`, and everything `APBACS` needs for a clean first cut. Plants its own payable and removes everything it created |

Run them with `build\run-tests.ps1 -Layer cobol -Filter TAP` (153 assertions, all green).
`TAPMATCH`, `TAPVAT6` and `TAPEINV` run with the database down.

### Period close and bulk processing (AP-022, AP-023)

| Program | Requirement | What it does |
| --- | --- | --- |
| `APCLOSE` | AP-022, AP-010, AP-021 | `APCLOSE CO=STC PERIOD=yyyymm [ASAT=yyyymmdd] USER=APMGR [MODE=PREVIEW\|COMMIT]`. Five proofs before a period closes: creditors control (2000) against the open items behind it; GRNI (2010); invoices registered but not posted; invoices still waiting on a match; supplier returns held in return clearing. COMMIT reclassifies the residual GRNI to accruals through `AP.GRNICLEAR` under correlation `APCLOSE-GRNI-<CO>-<period>`, so a restarted close cannot post twice. RC 4 when any line is out of balance. |
| `APBULK` | AP-023, AP-003/004, AP-012 | `APBULK CO=STC USER=APCLERK [MODE=PREVIEW\|COMMIT]`. Reads `run\work\APBULK-<CO>.txt` (pipe delimited, `*` comments). `INV`/`CRN` lines go through the ordinary `APINVREG` COMMAREA so each document keeps its own duplicate check; `PAY` lines call `APPAYAPR VERIFY` so a bulk run proves a payment run's authority but never grants it. Every line gets its own result row (REGISTERED / DUPLICATE / PREVIEW / AUTHORITY / REJECTED) with the reason, so reprocessing a failed file cannot register an accepted document twice. Sample: `data\bulk-samples\APBULK-STC.txt`. |

### Running the batch suite

```
bin\x86\APAGED.exe   CO=STC ASAT=20261006 USER=APMGR
bin\x86\APPPR.exe    CO=STC FROM=20260401 TO=20260930 USER=APMGR
bin\x86\APREMIT.exe  CO=STC RUN=PP-26-0007 USER=APMGR
bin\x86\APBACS.exe   CO=STC RUN=PP-26-0007 ENTRYDATE=20261009 USER=APMGR
bin\x86\APVAT6M.exe  CO=STC ASAT=20270930 USER=APMGR MODE=PREVIEW
bin\x86\APCLOSE.exe  CO=STC PERIOD=202609 ASAT=20260930 USER=APMGR
bin\x86\APBULK.exe   CO=STC USER=APCLERK MODE=PREVIEW
bin\x86\APEINV.exe   CO=STC USER=APCLERK MODE=PREVIEW
```

Every program ends with a `STERP-I-SPOOL <path>` line naming its listing. Against the demo data
`APAGED` ages 12 items, `APPPR` counts 3 invoices, `APREMIT` prints 3 advices, `APBACS` cuts 3 Bacs
items, `APVAT6M` finds 8 repayments at 30/09/2027 and `APCLOSE` runs 5 reconciliations.

`APBACS` on a run that has already been RELEASED produces a **reproduction**: the approval is still
checked, the instructions are read from `ap.vw_BacsInstructionAll` (the same columns as
`ap.vw_BacsInstruction` without the APPROVED/SUBMITTED filter) and the run keeps its status.

## Bulk and e-invoice registration (AP-023, AP-031) - coding rules

`APBULK` and `APEINV` register through the ordinary `APINVREG` COMMAREA, so a line they
build must satisfy `APINVLIN` exactly as a keyed line does:

* a line with no order, receipt or service link is **non-PO** and must carry an approved D-08
  category in `IR-NONPO-REASON` (`AP.NONPO.CATEGORIES`), otherwise `APL0005` blocks it;
* `IR-L-NATURE` feeds `ap.PurchaseInvoiceLine.CostNature`, constrained by `CK_PInvL_Cn` to
  STOCK / EXPENSE / ASSET / WIP / LANDED / DUTY - `GOODS` is not a value.

The bulk file `run\work\APBULK-<CO>.txt` therefore takes two optional trailing fields after the
account: `|order|nonporeason`. A line quoting an order is registered as STOCK, otherwise as
EXPENSE with the category given, defaulting to `SUBSCRIPTION`.

A UBL document carries `cac:OrderReference` once, on the document; `APEINV` gives it to every
line. `APEINV` clears the register COMMAREA for each document - a stale `IR-INVOICE-ID` makes
`APINVREG`'s exact-duplicate check exclude the row it is looking for, so a resent document was
registered twice instead of once (AC-125).

### Demonstrating it

```
copy data\bulk-samples\APBULK-STC.txt run\work\APBULK-STC.txt
bin\x86\APBULK.exe CO=STC USER=APCLERK MODE=COMMIT   -> 3 registered, 1 duplicate, 2 rejected
bin\x86\APBULK.exe CO=STC USER=APCLERK MODE=COMMIT   -> 0 registered, 4 duplicate (AP-023 restart)
copy data\einvoice-samples\*.xml run\einvoice\in
bin\x86\APEINV.exe CO=STC USER=APCLERK MODE=COMMIT   -> 2 documents, 1 registered, 1 duplicate
```

## Acceptance defect fixes (2026-10-07)

* **Payment proposal date conversion (AC-034).** `APPAYPRP` asked `APSVC`/`DATECALC` to adjust a
  non-processing Bacs entry date with direction `FWD`. `DATECALC` accepts `NEXT`, `PREV` or `NONE`
  only, refused the request and left a zero result date, which reached SQL Server as the text `"0"`
  and failed the proposal insert with *"Conversion failed when converting date and/or time from
  character string"*. `APPAYPRP` now asks for `NEXT`, refuses the run with `APP1014` if the Bacs
  calendar cannot answer, and never carries a zero date into the header; `APSVC` defaults an
  unstated adjustment direction to `NEXT` as well. Covered by `cobol\tests\ap\TAPPAYDT.cob`.
* **Reverse charge VAT analysis.** `db\seed\90-ap-demo.sql` now reports a reverse charge in VAT box
  4 as well as net box 7, so the purchase side of a reverse charge is declared on both sides.
