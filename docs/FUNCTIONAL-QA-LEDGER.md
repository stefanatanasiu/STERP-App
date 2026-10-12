# Functional GUI QA and defect ledger

Test started: 8 October 2026.

## Scope and method

Start with all six stories in [DEMO-WORKFLOWS.md](DEMO-WORKFLOWS.md), then extend
coverage to related entry, enquiry, reporting, validation and cross-module controls.
Exercise the native WinForms client using visible mouse/keyboard input and inspect
the live control and status state. Use database assertions and existing tests to verify
the effects of GUI actions, not as a substitute for exercising the screens.

The deliberately legacy WinForms, VB.NET, COBOL, C and SQL architecture is retained.
Expected business refusals (permissions, segregation of duties, immutable released
documents and closed periods) are passing controls, not bugs.

## Isolation and evidence

- Original database: `STERP`, not used for test transactions.
- Test database: `STERP_QA_FRESH_20261008`, deployed from source as an isolated fresh database.
- Isolated runtime and evidence: `run\work\functional-qa-20261008`.
- Both ADO.NET and COBOL/ODBC are explicitly directed at the test database.
- The current Release/x86 client was rebuilt and copied into the isolated runtime with its
  test-database configuration preserved.
- GUI input was exercised with visible mouse/keyboard actions and Windows UI Automation. No
  external service receives app data. A reliable app-only screenshot capture was not
  established for this run, so no screenshot is cited as evidence.

## Workflow coverage

| Workflow | State | Evidence / findings |
|---|---|---|
| 1. STC/RDG order to cash | Partial | Opened `SOS900008`; verified released/picked/billing-eligible state, price explanation and stock availability. Blank and unknown order references tested. Despatch, invoicing, receipt and reconciliation not run in this pass. GUI-011. |
| 2. STC/RDG BOM, kits and costing | Not started | |
| 3. STC/BHM procure to pay | Not started | |
| 4. CAL/GLA migration and trading | Not started | |
| 5. CAL/LIV credit hold and site access | Not started | |
| 6. STC finance and period close | Not started | |
| Extended navigation, input validation and reporting | Partial | Sales-order load, explain-price, availability, and missing/blank-reference validation exercised. |
| Automated regression and final reconciliation | Partial | Release/x86 rebuild passed; GUI-011 retest passed. Full application regression and control reconciliation not run. |

## Defects

Confirmed defects will include reproducible GUI steps, expected and actual behaviour,
severity, affected source, corrective action and retest evidence. Historical issues
from the earlier UI review are not counted until reproduced in this run.

| ID | Severity | Reproduction / actual result | Expected result | Fix / retest | State |
|---|---|---|---|---|---|
| GUI-001 | Medium | Demo instructions repeatedly direct operators to `System > Sign on`, which does not exist. | Instructions identify the real user/company selection menu. | **Docs fixed:** `DEMO-WORKFLOWS.md` and `README.md` now use `File > Continue as User` and `File > Select Company / Site`. | Fixed |
| GUI-002 | High | Load an order, Explain price / Availability, then load another order or an unknown reference. Previous enquiry panes (and, for not-found, previous order data) remain actionable. | All displayed and actionable data belongs to the successfully loaded reference. | **Fixed** in `frmSalesOrderEntry.vb`: every load clears the order and enquiry panes first; Explain / Availability / Reserve / F10 work only on the order that actually loaded. Retest `gui002-01..03`: pass. | Fixed |
| GUI-003 | High | Leave Sales Order Entry open as SALESREP, then continue as WHSE. The old form and customer data remain open even though the new menu cannot open that screen. | A successful session change closes old-context forms; cancelling must restore the original session. | **Fixed:** `SterpSession.Snapshot/RestoreFrom/ContextDiffers` and `frmMain.ChangeSignOn`. Cancel restores the session; a real change closes the old forms. Opening the dialog again keeps the current user, date and company/site, including after a *Continue as* user change (`frmLogin.vb`, `LoginService.RunAs`). Retest `gui003-01..04`: pass. | Fixed |
| GUI-004 | High | Despatch screen asks for a pick reference, but F2 opens the sales-order lookup and only fills the order field. Loading a documented order gives `Key a pick list reference first`. | Operator can locate and load the appropriate pick without database access. | **Fixed** in `frmDespatchConfirm.vb` / `SalesOrderService.PickListPickSql`. F2 now lists pick lists at the site that still have picked stock to send. Keying an order number and pressing F5 opens that order's pick. F10 refuses if the keys changed after the pick loaded. Also fixed: the pick line number was being sent as the order line, and the schedule and bin were dropped, so `so.DeliverySchedule` never moved. Retest `gui004-01..03`: pass; a despatched pick no longer appears in the list. | Fixed |
| GUI-005 | High | Both documented picked orders (`SOS900008`, `SOC900004`) have no pick header/lines. Re-running the documented demo drivers does not create their picks. | The advertised ready-to-despatch demonstration orders have executable allocations and picks. | **Root cause:** the seeded items are lot- or serial-controlled, so SODEMO skipped them. SODEMO also skipped CAL when run with `CO=STC`. **Fix:** `SODEMO 2700/2710/2730` reserve the stock (SORESV) and pick it (PICKGEN) from a named bin and lot/serial. Anything that cannot be picked becomes a short pick with a reason. No transactions are faked. Result: `PKS00000002` and `PKC00000001`. Despatch from the GUI succeeds: `DNS00000003` (frames FG01 lot L26001, tyres STKB01 lot L26002) and `DNC00000001` (serial SN26RAVELM001). Retest `gui005-01..03`: pass. Despatch had also failed with `DSP0004 / E-0026`; see GUI-009. | Fixed |
| GUI-006 | High | Continue as ARCLERK: the Sales menu is absent, so the documented invoice run cannot be opened. | The invoicing clerk has the narrowly scoped `SO.INVOICE.RUN` permission. | **Fixed (seed):** `72-so-menu.sql` grants `SO.INVOICE.RUN` to ARCLERK. COMMIT requires `AR.INVOICE.POST` (ARINVPST), which only ARCLERK holds. SALESMGR can preview but COMMIT is refused with `ARI0050` (screenshot `gui006-commit`). Retest as ARCLERK: PREVIEW rc 0, COMMIT rc 0, invoice `SIS00000003` posted with its open item (`gui006-arclerk-*`). | Fixed |
| GUI-007 | High | Receipt Entry, load C0001, enter GBP1, gross/allocation 8,196 against the 8,200 invoice, click Preview residual. It refuses with `RCP0023: Name the receipt or credit ... before it is applied`. | Preview supports a new, not-yet-posted receipt and reports its proposed residual without posting. | **Fixed** in `RCPTALOC` CR2616 (`2060-NEW-RECEIPT-SOURCE`): a PREVIEW with no named source values the receipt being keyed and writes nothing. ALLOCATE still needs a named source. If PREVIEW meets rc 6, the screen says that posting would be held and that nothing was requested. Retest `gui007-c0001-preview`: applied 8,196.00, written off 4.00, residual 0.00. `gui007-c0004-preview`: RCP0029 warning, no receipt and no approval row written. | Fixed |
| GUI-008 | Medium | The documented C0001 two-invoice / every-ageing-bucket demo cannot be followed: seed contains only one outstanding invoice for that customer (8,200, part paid). | Demo fixture or instructions provide actual multi-invoice examples and accurate ageing expectations. | **Docs fixed** to match the fixture: one invoice, with `AGEDDEBT` for ageing buckets. Receipt Entry gains *Write off* and *Write off reason* columns; the engine already supported them. Retest `gui008-c0001-posted`: `RCS00000001` posted 8,196.00 with a 4.00 write-off. | Fixed |
| GUI-009 | High | Found while retesting GUI-005. Despatching a 2-line pick fails on line 2 with `STERP-E-0026 location unknown (DSP0004)`; trace shows blank warehouse and location. | Every line is issued from its picked bin. | **Root cause:** `CobolCodec.Initialise` initialised only occurrence 1 of an OCCURS group. Lines 2-30 of every COMMAREA table stayed LOW-VALUES, so COBOL treated them as "not blank" and skipped defaulting. This affected **every** program with an OCCURS table. **Fixed:** every occurrence is now initialised, with a regression test (`commarea: INITIALIZE reaches every occurrence`). | Fixed |
| GUI-010 | Medium | Found while retesting GUI-005: after a serial was despatched, `inv.Serial.Status` stayed `INSTOCK`. | An issued serial is `ISSUED` (scrapped is `SCRAPPED`). | **Root cause:** `LOTSER 0100-INITIALISE` blanked `LS-SERIAL-STATUS`, which is an input to SETSTAT/NEWSER. **Fixed:** CR2617. Retest: `SN26RAVELM001` is `ISSUED` after `DNC00000001`. | Fixed |
| GUI-011 | Medium | In STC/RDG Sales Order Entry, load `SOS900008`, clear the order number, then press F5. The reference is blank but the previous customer's name, order lines and release/fulfilment/billing states remain visible. | A blank reference leaves no prior order or enquiry data displayed; order actions remain guarded until a valid order loads. | **Fixed** in `frmSalesOrderEntry.vb`: clear the loaded order and enquiry panes before handling an empty reference. Retest on the rebuilt isolated client: the reference remains blank, prior order details and lines are cleared, both grids have zero rows, availability returns to its prompt, and F5 reports that an order number is required. | Fixed |

## GUI QA run - 9 October 2026

The current source build was exercised against `STERP_QA_FRESH_20261008`. In the STC/RDG
Sales Order Entry screen, `SOS900008` loaded as released, picked and billing-eligible; the
price explanation returned the standard price and the availability enquiry reported 12 EA
free stock. Loading unknown reference `ZZZ999999` correctly cleared the previous order.
Loading a blank reference exposed GUI-011; the same steps passed after the fix and rebuild.
After the fix, UI Automation confirmed that no `SOS900008` reference, customer name, released,
picked, or billing-eligible state remained, both grids had zero rows, the availability pane
returned to its prompt, and the status line requested an order number.

The remaining five demo stories were not exercised in this pass. In particular, no invoice,
receipt, purchase, stock, journal, close, or external-file transaction was committed. Full
workflow coverage and final reconciliation remain outstanding.

**Regression run (2026-10-08, isolated scratch DB restored from the fresh baseline):**
- COBOL unit drivers: 3,132 passed, 0 failed.
- .NET harness: 163 passed, 3 skipped, 3 failed. The failures come from the isolated layout, not the code: PING expects a database called `STERP`, and `jobs\NIGHTLY.cmd` is looked for beside the copied binaries.

A first harness pass on the reused `STERP_QA_FIX_20261008` had 5 more failures. They were caused by data left by earlier GUI retests and by a test config that pointed only the SQL connection at the clone. All 5 also fail with the original codec, and all pass on the clean database.

### Baseline preparation

The documented driver sequence was run on the clone after discovering the missing picks.
FINDEMO, PODEMO, APDEMO and KITDEMO returned 0. SODEMO returned 4 because its invoice run
returned 8; this will be investigated through the invoice screen. Baseline RECONCTL at
31 October reports all eight families agreeing after the normal demo preparation bridge.
The bridge must **not** be used to conceal discrepancies introduced by the subsequent tests.

To distinguish old test fixtures from source defects, a second database,
`STERP_QA_FRESH_20261008`, was created and deployed from the current source (schemas,
tables, functions, views, procedures, triggers and seeds only). The hard-coded production
database-creation script and shared SQL Agent jobs were deliberately excluded.
The original copy remains available for before-fix evidence.

## GUI QA continuation - 9 October 2026

This continuation uses a new source-deployed database, `STERP_QA_GUI_20261009_0a5d521b`,
and an isolated application-shaped runtime at
`run\work\gui-qa-20261009T125756Z-0a5d521b`. Both SQL connection paths and all generated
files are directed there. The original database and previous QA databases are not used
for transactions. The five documented demo drivers and the initial STC/CAL reconciliation
each returned 0. Control-account bridging is baseline preparation only and will not be
rerun to conceal any discrepancy caused by GUI tests.

Native mouse/keyboard input and Windows UI Automation exercise the visible client.
Each evidence name below identifies an app-only PNG and a complete control-state JSON
under the isolated runtime's `evidence` directory. Initial input-helper failures are
not application defects; only actions with verified exact input values count.

### Confirmed defects in this continuation

| ID | Confidence | Reproduction / actual result | Expected result | Source / corrective action | State |
|---|---|---|---|---|---|
| GUI-012 | observed | ARCLERK, STC/RDG, Customer Enquiry: load `C0001`, clear Customer account and press F5. The account field is empty but Pennine's name, GBP8,200 balance, ageing and open items remain. An unknown account raises `E-0302` without clearing the prior enquiry. Evidence: `025-customer-c0001`, `026-customer-blank-stale`, `027-customer-unknown`. | Blank, unknown or failed refreshes must not present another account's results as belonging to the keyed account. | [frmCustomerEnquiry.vb](../dotnet/STERP.Desktop/Forms/Debtors/frmCustomerEnquiry.vb), `LoadData`: clear account-dependent results before validating/loading a new reference. Retest pending. | Open |
| GUI-013 | observed | Receipt Entry: load `C0001`, clear the account and press F5; the old allocation grid stays visible. After previewing GBP8,196 plus GBP4 write-off, change the account to `C0004` without F5 and preview again. The screen sends C0001's instructions under C0004's header; the engine refuses with `RCP0024` rather than the screen requiring a reload. Evidence: `031-receipt-open-items`, `032-receipt-blank-stale`, `037-receipt-preview`, `038-receipt-changed-customer`. | Clear stale allocations/results on an unsuccessful load and require the loaded account/currency to match the keyed header before previewing or posting. | [frmReceiptEntry.vb](../dotnet/STERP.Desktop/Forms/Debtors/frmReceiptEntry.vb), `LoadItems` and `Ready`: add loaded-context tracking and reset account-dependent state. No wrong-customer receipt was committed. Retest pending. | Open |
| GUI-014 | observed | On an unposted receipt with allocations, press F3 and answer Yes to "Allocations have been keyed but not posted. Abandon them?". The identical prompt appears a second time; the form closes only after a second Yes. Evidence: `046-duplicate-abandon-prompt`, `047-after-second-abandon`. | One close request asks once; No keeps the window and Yes closes it. | [BaseForm.vb](../dotnet/STERP.Desktop/BaseForm.vb), `CloseForm` and `OnFormClosing`, both call `CanClose`. Let the closing event own the check. Retest pending. | Open |

The unsaved-window session-change control passed: answering No restores ARCLERK and
keeps the old-context receipt open (`042-session-change-abandon-prompt`,
`044-session-after-abandon-no`). This is not counted as a defect.

## GUI QA rerun - 9 October 2026, 16:11 UTC

Run `gui-qa-20261009T161135Z-69b50c9a` uses the new source-deployed database
`STERP_QA_GUI_20261009_69b50c9a` and the isolated application-shaped runtime
`run\work\gui-qa-20261009T161135Z-69b50c9a`. The existing client and databases
were left untouched. Deployment excluded production database creation and shared
SQL Agent jobs. All five demo drivers and baseline STC/CAL RECONCTL returned 0.
Control bridging is baseline preparation only; it will not be rerun after GUI actions.

Evidence identifiers below refer to app-only PNGs and complete UI Automation JSON
under that runtime's `evidence` directory. All actions use visible native mouse and
keyboard input; SQL assertions check effects rather than replacing GUI workflows.
The run's preparation script, manifest and execution record are retained locally.

### Observed results before repair

- STC/RDG `SOS900008` loaded as RELEASED/PICKED/ELIGIBLE; price explanation and
  availability ran. `SOS900003` and `SOS900004` showed their distinct credit and
  commercial holds. Blank/unknown sales-order refreshes cleared prior data
  (`003` through `008`).
- F2 selected pick `PKS00000001`; confirmation produced `DNS00000002` and COGS
  journal `JNS00000083` for GBP371.79 (`010` through `013`).
- ARCLERK invoice PREVIEW returned 0 and left the invoice count at 24. COMMIT
  returned 0 and created one invoice, raising the count to 25 (`015` through `017`).
- The C0001 receipt preview applied GBP8,196 and wrote off GBP4 with zero residual;
  receipt count remained 9. No stale-context receipt was posted (`026`, `027`).
- GUI-012, GUI-013 and GUI-014 were reproduced independently on this fresh
  baseline (`019` through `031`); their earlier open findings remain valid.
- CREDMGR raised a dispute on `SI-26-1003` and recorded a GBP100 promise for
  Northgate Retail Group using F10. The worklist fell from 8 to 7 to 6 items
  (`032` through `040`). The toolbar problem below is separate from the working
  keyboard/engine path.
- Bike quantity 10 and cake quantity 12 explosions completed, with 95 and 36
  occurrences respectively (`042`, `043`).

### Additional confirmed defects

| ID | Confidence | Reproduction / actual result | Expected result | Source / corrective action | State |
|---|---|---|---|---|---|
| GUI-015 | observed | CREDMGR, STC/RDG, Credit Control Worklist: select an overdue row and click **Promise to pay (F10)**. Nothing happens (`036`, `037`). Pressing F10 on the same selection opens the amount/date dialogs and records the promise (`038` through `040`). | The advertised toolbar action and F10 invoke the same promise workflow. | [frmCreditControl.vb](../dotnet/STERP.Desktop/Forms/Debtors/frmCreditControl.vb) implements `OnConfirm` but has no `tsbPromise.Click` handler; [frmCreditControl.Designer.vb](../dotnet/STERP.Desktop/Forms/Debtors/frmCreditControl.Designer.vb) creates the button without a callback. Wire the button to `OnConfirm`. Retest pending. | Open |
| GUI-016 | observed | ENGINEER, STC/RDG, Explosion Enquiry: explode `FG-CAKE-CELEB-8IN`, quantity 12, then clear Parent item and click **Explode (F10)**. The warning requests a parent, but the previous cake explosion remains (`043`, `044`). An unknown parent raises `BOM0504` (`045`). | A failed or empty enquiry must clear results belonging to the previous parent/parameters. | Clear the explosion results before validating or executing a new enquiry. Retest pending. | Open |

Coverage of the remaining demo steps and post-repair results will be added from
observed execution; this section does not yet claim complete workflow coverage.

## GUI QA - 10 October 2026

Run `gui-qa-20261010T085122Z` uses the fresh source-deployed database
`STERP_QA_GUI_20261010_ff1a24fa` and the application-shaped isolated runtime
`run\work\functional-qa-20261010`. The current Release/x86 client was built before
the run. Deployment of 231 scripts excluded production database creation and
shared SQL Agent jobs. All five demo drivers and both initial STC/CAL RECONCTL
runs returned 0. The control bridge is baseline preparation only.

Every numbered evidence identifier below has an app-only PNG and live UI
Automation JSON in that runtime's `evidence` directory. Input is native mouse
and keyboard input, with exact field values checked before an action counts.
SQL evidence checks GUI effects. Input-helper failures are not application bugs.
No original or earlier QA database is used for transactions, and no payment or
tax payload is submitted to an external service. This is legacy-application QA,
not a modernisation stage transition or production deployment.

### Reproduction of the five outstanding findings

| Finding | Current-run evidence | Result before repair |
|---|---|---|
| GUI-012 | `019-customer-loaded`, `020-customer-blank-stale`, `021-customer-unknown-error`, `022-customer-unknown-stale` | Blank and unknown account refreshes retain Pennine's name, GBP8,200 balance and open items. |
| GUI-013 | `024-receipt-items`, `025-receipt-blank-stale`, `026-receipt-preview`, `027-receipt-changed-account-error` | Blank refresh retains allocations; changing C0001 to C0004 sends the old allocation instructions under the new header and receives RCP0024. No receipt was posted. |
| GUI-014 | `029-abandon-first-prompt`, `030-abandon-duplicate-prompt` | Answering Yes to F3's abandon prompt displays the same prompt again. |
| GUI-015 | `034-credit-toolbar-no-action`, `035-credit-keyboard-promise-dialog`, `036-credit-promise-recorded` | The promise toolbar button does nothing; F10 opens the dialogs and records the promise. |
| GUI-016 | `040-cake-explosion`, `041-explosion-blank-stale`, `042-explosion-unknown-error` | Empty explosion input leaves the preceding cake results visible. |

### Additional observed defects

| ID | Confidence | Reproduction / actual result | Expected result | Affected source | State |
|---|---|---|---|---|---|
| GUI-017 | observed | ENGINEER, Explosion Enquiry: enter cake quantity `0`, choose COSTED, then enter quantity `12` and date `32/10/2026`. Both return successful costed results. Zero is silently changed to one (total GBP67.28); the invalid date is silently changed to today (total GBP602.45). Evidence: `043-explosion-zero-quantity`, `044-explosion-invalid-date`. | Refuse non-positive/non-numeric quantities and invalid dates without running or displaying a substituted calculation. | [frmBomExplosion.vb](../dotnet/STERP.Desktop/Forms/Bom/frmBomExplosion.vb), `Qty`, `AsAt`, calculation entrypoints. | Open |
| GUI-018 | observed | Where Used Enquiry: find `DEMO-PIN`, clear Component and press F5. The previous nine parent rows remain. Evidence: `045-where-used-pin`, `046-where-used-blank-stale`. | Clear old rows and prevent a previous parent selection remaining actionable after a failed/empty enquiry. | [frmBomWhereUsed.vb](../dotnet/STERP.Desktop/Forms/Bom/frmBomWhereUsed.vb), `LoadData`, `FindClicked`. | Open |
| GUI-019 | observed | COSTACC: compare `FY2027STD` with `FY2026STD`. The status says `Compared  with FY2026STD`; F10 then requests a cost-set selection because comparison rows replaced the set list. After a permitted approval, the status names the first reloaded set (`BOMDEMO`) instead of the set actually approved (`FY2027STD`). SQL confirms FY2027STD was approved, not BOMDEMO. Evidence: `060-cost-set-comparison`, `061-cost-set-approve-after-compare`, `062-cost-set-self-approval-refused` (the identifier records the attempted expectation; the observed action was permitted because the preparer is KWATSON). | Keep the selected cost-set context through comparison and report the actual action target after refreshing the list. Do not change the engine's approval rules. | [frmCostSet.vb](../dotnet/STERP.Desktop/Forms/Costing/frmCostSet.vb), `CompareClicked`, `Act`, `Chosen`. | Open |
| GUI-020 | observed | Goods Receipt: click Counted and key `40`. The editor remains empty and keyboard focus stays on Count line. Tabbing from Reverse reaches the editor and accepts `40`. The decorative group box is in front of its sibling editors. Evidence: `082a-goods-input-state`, `082b-goods-counted-keyboard`. | Mouse and keyboard both reach the visible editors; grouping decoration must not intercept input. | [UiStyle.vb](../dotnet/STERP.Desktop/UiStyle.vb), `Box`, `ApplyPalette`; [frmGoodsReceipt.vb](../dotnet/STERP.Desktop/Forms/Purchasing/frmGoodsReceipt.vb), constructor. | Open |
| GUI-021 | observed | Validate PO00000004 and count its line, then change Order to PO00000005 without revalidating and press F10. Confirmation names PO00000005 but the service still holds PO00000004's COMMAREA. Evidence: `082-goods-receipt-counted`, `083-goods-receipt-changed-order-prompt`. The subsequent posting failed for GUI-022, so no wrong-order receipt was committed. | Require the order and receipt header to match the loaded context before counting or confirming. | [frmGoodsReceipt.vb](../dotnet/STERP.Desktop/Forms/Purchasing/frmGoodsReceipt.vb), `ValidateClicked`, `CountClicked`, `OnConfirm`; [GoodsReceiptService.vb](../dotnet/STERP.Business/Purchasing/GoodsReceiptService.vb), `Confirm`. | Open |
| GUI-022 | observed | Count the PO00000004 lot-tracked line and confirm using the screen's default `ON-RECEIPT` ownership. SQL rejects the receipt with `CK_PRcp_Ob`; both order quantities remain unchanged and no receipt is written. Evidence: `084-goods-receipt-changed-order-result`, `085-goods-receipt-order-effects.txt`. | Pass an evidenced ownership code to RECEIPT/SQL and make the normal goods-in workflow executable. | [frmGoodsReceipt.vb](../dotnet/STERP.Desktop/Forms/Purchasing/frmGoodsReceipt.vb), ownership choices; [GoodsReceiptService.vb](../dotnet/STERP.Business/Purchasing/GoodsReceiptService.vb), `Validate`. | Open |
| GUI-023 | observed | Validate PO00000004 before supplying its required lot. The loaded line has 40 outstanding units, but the modal reports `No line carries a receivable quantity / RCP0003`. The specific lot-required failure raised in the per-line validation is overwritten by the final zero-valid-lines check. Evidence: `081-goods-receipt-validated`; [RECEIPT.cob](../cobol/online/po/RECEIPT.cob), `2000-VALIDATE`, `2100-ONE-LINE`. | Keep the traceability refusal and its specific reason so the operator knows which input to correct. | [RECEIPT.cob](../cobol/online/po/RECEIPT.cob), validation result propagation. | Open |

### Initial workflow observations

- STC order SOS900008 was despatched through the GUI as DNS00000002, with COGS
  GBP371.79 and journal JNS00000083. Invoice PREVIEW left 24 invoices; COMMIT
  created exactly one invoice and its AR open item, raising the count to 25
  (`003`-`017b`). Credit and commercial hold examples remained distinct.
- The receipt preview applied GBP8,196 and wrote off GBP4 without posting.
  CREDMGR's dispute and keyboard promise reduced the worklist from eight to
  seven to six items (`026`, `033`-`036`).
- Bike and cake costed enquiries returned unit costs GBP568.25 and GBP50.20.
  All five documented kit lifecycle states loaded, including KIT0001001 WIP
  GBP1,842.50; completed kit KIT00001001 traced in both directions (`037`-`057`).
- All five purchase-order statuses and the blanket/service/subcontract examples
  loaded. BUYER's approval attempt was refused with E-0008; PURMGR independently
  approved PO00000002 (`069`-`080`). These are passing business controls.
- Some demo wording does not match the live fixture or available screens:
  where-used has six direct/nine total parents and two upward levels;
  FY2027STD's preparer is KWATSON, not COSTACC; CST27000 currently forecasts a
  favourable GBP18,135 variance; PO00000009 is GBP, not USD. Demo instructions
  will be reconciled with verified GUI paths during the continuation.

Goods receipt and the remaining demo steps continue after these repairs.
No claim of complete six-story coverage or final reconciliation is made yet.

## Limits

This is a local demonstration environment. File-based payment and tax outputs must
remain local; no bank, tax authority or external trading partner is contacted.
Coverage and final counts will be updated from observed results, not inferred from
the existing traceability tags.

## GUI QA and repairs - 12 October 2026

Run `gui-qa-20261012T082631Z-9283a339` uses the source-deployed database
`STERP_QA_GUI_20261012_9283a339` and isolated runtime
`run\work\functional-qa-20261012-9283a339`. Both ADO.NET and native ODBC use that
database. Production database creation and shared SQL Agent jobs were excluded.
Release/x86 managed and COBOL builds, all five demo drivers, and baseline
STC/CAL RECONCTL completed successfully. Control bridging is baseline-only and
will not be repeated after GUI transactions. This is user-authorised legacy
functional QA, not a modernisation stage transition.

Evidence identifiers refer to live control-state JSON and app-only PNGs below
this runtime's `evidence` directory. Native mouse/keyboard input exercises the
client; database checks verify effects rather than substitute for GUI actions.
Payment and tax files remain local.

### Defects recorded before repair

The historical findings above are preserved. The following table records this
run's independent observations and supersedes their earlier open statuses.
GUI-012 through GUI-027 have passed the focused regression suite and native GUI
retests. The final control proof exposed the additional inventory integration
finding GUI-028, which remains open.

| Finding | Confidence | Current-run reproduction | Repair / retest status |
|---|---|---|---|
| GUI-012 | observed | C0001 Customer Enquiry retains Pennine's GBP8,200 results after blank/unknown refresh (`019`-`021`). | Fixed: clear all account-dependent grids, metrics and statement caption. Loaded, blank and unknown retests passed (`101`-`103`). |
| GUI-013 | observed | Receipt Entry retains allocations after blank refresh and previews C0001 instructions under a changed C0004 header, receiving RCP0024 (`022`-`025`). No wrong-customer receipt committed. | Fixed: reset allocations/totals and require loaded account/currency identity; prevent repeat cash posting. Blank/context/preview/post/replay retests passed (`104`-`107`, `111`-`113`). |
| GUI-014 | observed | F3/Yes repeats the abandon prompt; No keeps the form and a second Yes is required (`026b`, `027`, `028`). | Fixed: the closing event performs the single unsaved-work check. No retains the form; one Yes closes it (`108`-`110`). |
| GUI-015 | observed | Promise toolbar is inert; F10 opens the promise dialog, cancelled in this run (`029`-`031`). | Fixed: toolbar invokes the existing promise workflow. Mouse retest opened the dialog (`114`); new promise/dispute commits were not exercised. |
| GUI-016 | observed | Empty parent leaves the previous cake explosion visible (`034`, `035`). | Fixed: clear results before a new/failed enquiry. Blank-parent retest passed (`118`). |
| GUI-017 | observed | Zero quantity becomes one (GBP67.28); invalid date becomes today (GBP602.45 for twelve cakes) (`036`, `037`). | Fixed: validate positive quantity and strict dates without substituting successful calculations. Refusals clear the prior result (`119`, `120`); valid bike/cake figures remain unchanged (`115`-`117`). |
| GUI-018 | observed | Blank Where Used refresh retains nine DEMO-PIN parent rows (`038`, `039`). | Fixed: clear results and stale actionable parent selection. Loaded/blank/selection retests passed (`121`-`123`). |
| GUI-019 | observed | FY2027STD comparison loses the set name/context; its Impact reports BOMDEMO after refreshing the set list (`041`-`043`). No set was activated or approved. | Fixed: retain the comparison target and capture the action target before refresh. Comparison and Impact identify FY2027STD (`124`-`126`); approval/activation were cancelled and controls were not weakened. |
| GUI-020 | observed | Mouse input cannot reach Counted, while keyboard traversal accepts 40, GOODSIN and a lot (`050`, `051`). | Fixed: decorative groups are behind sibling editors. Ordinary mouse entry succeeds (`129`). |
| GUI-021 | observed | Changing the keyed header after loading/counting PO00000004 can confirm its cached delivery (`052`, `053`). | Fixed: require the keyed header to match the loaded delivery, and discard cached state before reloading. Changed-header confirmation/count are refused (`130`, `131`). |
| GUI-022 | observed | The default ON-RECEIPT reaches SQL but violates its three-code ownership constraint (`054`). | Fixed: use RECEIPT, INCOTERMS and DROPSHIP. The repaired GUI posts RECEIPT successfully (`132`, `206`, `210`, `211`); alternate ownership posting branches were not separately GUI-tested. |
| GUI-023 | observed | A missing required lot on PO00000004 reports generic RCP0003 despite 40 outstanding units (`049`). | Fixed: retain the specific per-line traceability refusal and allow its correction without losing the loaded delivery (`128`, `129`, `132`). |
| GUI-024 | observed | GRN00000004 posts 40 units, stock movement 39 and balanced GBP639.79 journal 153, but has no PurchaseOrderLineId; order quantities remain zero, schedule OPEN and header NOTRECEIVED (`132b-receipt-database-before-fix.txt`). | Fixed: atomic receipt/order linkage, quantities and delivery status, inspection acceptance propagation, and actual-line reversal loading. Full/partial/remaining/reversal/replay GUI checks passed (`206`-`211`). SQL proves final 45 received/accepted, linked receipts, COMPLETE/FULLRECEIVED and balanced journals (`229`). Completed-order enquiry offers zero lines (`230`). The pre-fix GRN remains unchanged as evidence. |
| GUI-025 | observed | VATOWNER clicks box 1 and Drill down, but button focus loses the selected box and no records load (`145`, `146`). | Fixed: retain the last explicitly selected box independently of button focus; preparation clears selection. Actual mouse retests loaded three box-1 rows and six box-4 rows (`202`, `203`), with a real-focus regression test. |
| GUI-026 | observed | Existing PI-26-0001 falsely reports no exact duplicate (`213`, `217`). Seed keys retain punctuation excluded by executable STRUTIL DOCREF, and the screen always checks INVOICE, including PC-26-0001 (`219`). | Fixed: the seed's ASCII letter/digit key matches executable default KEEP semantics, and the screen passes DocumentType. Invoice and credit-note retests both identify their existing register with APR0010 (`225`, `226`). QA correction touched only mismatched DEMO-created keys after a collision check. |
| GUI-027 | observed | PPS00000001 Totals reports zero lines/amounts and blank status (`215`) although ten lines and stored nonzero totals exist (`216`). TOTALS requires ProposalId, but the facade supplies only a reference. | Fixed: load company-scoped identity/header totals before the native read. GUI proves six selected, GBP19,596 principal less GBP1,080 credits = GBP18,516, four exclusions and RELEASED (`227`). Unknown reference explicitly refuses APP0000 and removes prior totals/status (`228`, regression). |
| GUI-028 | observed | Final STC RECONCTL shows INVENTORY subsidiary GBP1,060,051.85 versus GL GBP1,061,983.95, a GBP1,932.10 difference. This exactly equals GUI stock movements 37-43; none has a linked cost-pool history row, and the affected cst.ItemCost positions remain unchanged (`231`). | Open: financial stock-to-valuation integration, not a receipt delivery-status regression. The stock contract does not maintain the valuation pools used by RECONCTL/CSTINVAL. A repair must resolve site versus wildcard pools, STD versus MWA/FIFO values, allowance/history and replay semantics rather than force a reconciliation adjustment. |

### Initial workflow results

- STC/RDG order-to-cash: SOS900008 despatched as DNS00000002, with GBP371.79
  COGS and JNS00000083. Invoice preview left 24 invoices; commit created exactly
  one, raising the count to 25 (`005`-`018`). Credit/commercial holds remained
  distinct; blank/unknown sales-order refreshes cleared their old data.
- Bike quantity ten: 95 occurrences, depth four, GBP568.25 unit cost. Cake
  quantity twelve: GBP50.20 unit cost (`032`-`034`).
- DEMO-PIN: nine total parent rows, six direct, two upward levels (`038`).
- All five kit lifecycle examples loaded, including KIT0001001 WIP GBP1,842.50
  (`046-kit-*`). Completed KIT00001001 traced forward and backward (`047`, `048`).
- CST27000 loaded with its two sub-jobs (`044`). Engineering Changes is not
  offered in ENGINEER's Bills of Material menu; that documented path is not
  counted as executed.

### Instrumentation limitations

A read-only UI Automation GridPattern.GetItem probe coincided with a CLR access
violation and exit of the isolated client (PID 40728). Causal attribution to
application logic is unknown: this was not an ordinary user workflow, and is
not counted as a confirmed product defect. The client was relaunched with
explicit QA-only inherited database settings; subsequent checks use ordinary
native input and do not repeat that probe. Early focus/selection attempts and
DPI capture artefacts are also excluded from product findings.

An additional bounded sign-on automation call did not return after the final
receipt enquiry; its helper and owned client were stopped. Earlier and later
successful session changes are evidenced, but this timeout is not attributed to
application logic. It is excluded from the product defect count.

### Verified repairs and durable effects

- Release/x86 managed build passed after the final AP repairs. The purchasing
  native build passed with 152 LINKable programs / 166 objects. No framework or
  dependency upgrades were made.
- The final focused TAP run has **33 tests, zero failures, zero skips**:
  15 GUI/service regressions, three receipt-state cases and 15 existing purchasing
  cases. Evidence: `final-focused-regression-tests-ap.txt`. The updated complete
  procure-to-pay acceptance chain was not run.
- C0001 receipt RCS00000001 records GBP8,196 cash and GBP4 write-off, leaving
  SI-26-1001 at zero. A repeat post is refused. C0004's GBP19 write-off was
  preview-only: no request, approval or receipt was committed.
- PO00000005: GRN00000005 received 45 and was reversed once; repeated reversal
  wrote nothing further. GRN00000006 received 20 and GRN00000007 the remaining 25.
  The final order/schedule quantities are 45 received and accepted, with
  COMPLETE/FULLRECEIVED status and real order-line links (`229`). Four balanced
  original/reversal/partial/final journals are JNS00000088-91. This repair makes
  delivery-state bookkeeping atomic; it does **not** claim that the existing
  stock, receipt and journal stages are one global transaction.
- GLCLERK's balanced GBP1 journal validates in October; 15 May 2026 is correctly
  refused as hard-closed 202602. Returning to October posts exactly one
  JNS00000092, with GBP1 on each side (`222`-`224`, `229`).

### Additional demo workflow coverage

- Purchasing: eight order status/variant fixtures loaded (`134-purchase-*`).
  BUYER's approval attempt was refused (`136`). PURMGR approved PO00000002
  through the Approvals Inbox (`153`-`155`): core approval is APPROVED with
  DecidedBy PURMGR, but the purchase-order header remained AWAITAPPROVAL.
  Header-state reconciliation by a subsequent order-engine action is not claimed.
- CAL/GLA sales: SOC900001, SOC900002 and SOC900005 loaded. CAL/LIV SOC900003
  displayed its credit hold. Accept/release first refused missing
  InvoiceCustomerCode with SOA0002, so a successful credit override/release was
  not exercised (`140`-`143`).
- CAL customer enquiries loaded K0005 (one open item) and K0003 (two), under
  the CAL company context (`220-cal-customer-*`).
- VAT: FINCTRL lacks PREPARE by design; VATOWNER is the preparer. STC 26A2
  prepared as DRAFT with boxes 1/4/5 GBP94,500 / GBP47,670 / GBP46,830.
  STC 26A3 and CAL 27A1 drafts loaded. No MTD submission was made.
- Bank GBP1: book GBP384,281; outstanding receipts GBP937,796 and payments
  GBP553,515; statement GBP68,973.44; bridged GBP453,254.44 and unexplained
  minus GBP68,973.44 (`150`). The bridge arithmetic is correct. This remains an
  unreconciled fixture, not a confirmed arithmetic defect.
- Period seven: two of six checklist steps done, two blocking and readiness
  BLOCKED (`151`). The period was not closed.

### Final control reconciliation and remaining blockers

After all GUI postings, CLI RECONCTL ran with explicit QA-only SQL/ODBC settings,
CO=STC/CAL, ASAT=20261031, MODE=COMMIT and USER=FINCTRL. The report-launcher path
was not completed because of the bounded sign-on timeout above. **No demo control
bridge was rerun, no journal was inserted to balance controls, and no pre-fix
receipt/order quantities were backfilled.**

| Company / control | Subsidiary GBP | GL GBP | Difference GBP | Outcome |
|---|---:|---:|---:|---|
| STC INVENTORY | 1,060,051.85 | 1,061,983.95 | -1,932.10 | GUI-028 open |
| STC BANK | 376,085.00 | 384,281.00 | -8,196.00 | Saved bank-reconciliation position has not been refreshed after the cash receipt |
| STC other six families | | | 0.00 each | AGREED |
| CAL all eight families | | | 0.00 each | Zero differences; WIP/ACCRUAL have no configured control account |

STC returns **4**, not a clean reconciliation. CAL returns **0**; its two
NO CONTROL rows remain explicit and must not be called configured/AGREED
accounts. Outputs are `final-reconciliation-STC.txt`,
`final-reconciliation-CAL.txt` and their named spool reports.

The BANK difference equals the GBP8,196 receipt. RECONCTL uses the latest saved
fin.BankRecon bridge, currently DEMO-20261031-GBP1 with GBP376,085 and zero saved
outstandings. The live bank enquiry correctly includes the receipt at GBP384,281
(`231`). A genuine current statement/outstanding reconciliation is required;
replacing the subsidiary proof with the GL itself or restating the demo snapshot
would hide this outstanding workflow rather than verify it.

Not executed: REQGEN preview; GUI inspection disposition; AP invoice capture/
matching and supplier bank change; full dual-authorised payment/Bacs creation;
CAL SOC900004 despatch/invoice/serial assertion; conversion preview/commit/restart;
CREDMGR override and RESTRICT refusal; local MTD submission; MONTHEND operations;
new promise/dispute commits; mixed inspection/return/standard-cost receipt
reversal scenarios. Selected paths from all six stories were exercised, not six
complete end-to-end stories. No production deployment, external payment/tax
submission or modernisation stage approval occurred.

### Scope closure

After reviewing the final control differences, the user explicitly selected
**finish this GUI QA pass with the remaining financial blockers documented**.
This pass therefore closes with **16 repaired and verified findings**
(GUI-012-027), **one open inventory integration finding** (GUI-028), and the
separate outstanding bank-reconciliation workflow. No financial-core changes or
balancing adjustments were made to force those positions to agree.

The owned QA client and bounded helpers were stopped. The isolated database,
runtime, before/after GUI evidence, test results and control spools are retained.
Execution provenance is finalised under this runtime's `docs` directory with the
same run identifier. Completion of the requested QA scope is **not** a clean STC
financial close, production-readiness certification or lifecycle gate approval.

## Complete six-workflow QA continuation - 12 October 2026

Run `gui-qa-20261012T124201Z-b5145b17` uses the fresh source-deployed database
`STERP_QA_GUI_20261012_b5145b17` and isolated application runtime
`run\work\functional-qa-20261012-b5145b17\runtime`. The human explicitly
approved isolated local builds, tests, postings, jobs and functional bug fixes.
The run-specific application profile and authorization manifest are retained
beside that runtime. This is legacy functional QA, not a lifecycle transition.
The original database, earlier QA databases and shared SQL Agent jobs are not
changed; no bank, tax authority or other external service is contacted.

The managed Release/x86 build passed. Source deployment excluded production
database creation and shared jobs. All five demo drivers and both initial
company reconciliations returned zero. The demo control bridge was used only
for this initial baseline and will not be rerun after GUI transactions.
Evidence names below identify native UI Automation JSON and app-only PNGs
under the runtime's `evidence` directory.

### Newly observed defects before repair

| ID | Confidence | Reproduction / actual result | Expected result | Affected source / retest | State |
|---|---|---|---|---|---|
| GUI-029 | observed | WHSE, STC/RDG: F2 selects pick PKS00000001 for SOS900008. Enter `32/10/2026` as Despatch date and click Confirm despatch. The confirmation dialog still opens (`014-despatch-invalid-date-prompt`). `DespatchDate` substitutes the session business date when parsing fails. The operator answered No; no despatch or stock posting was made (`015-invalid-despatch-cancelled`). | Refuse an empty or invalid despatch date before asking for confirmation or calling the posting engine; never silently substitute a different accounting date. | [frmDespatchConfirm.vb](../dotnet/STERP.Desktop/Forms/Sales/frmDespatchConfirm.vb), `DespatchDate`, `OnConfirm`, `BuildArea`. Regression and native retest pending. | Open |
| GUI-030 | observed | ENGINEER, STC/RDG: Bill Headers (list), Find `BOM-DEMO-B4TC`, Search. The sole draft row is selected but its non-key editors remain disabled. Clicking that row does not enable them; Save reports "Select a record or press New first" (`038-draft-single-result-disabled`, `039-draft-row-click-still-disabled`, `040-draft-single-result-save-refused`). The same filtered-selection failure occurs on released `BK-COMMUTE-M` (`036`, `037`), preventing an actual edit attempt from reaching the immutable-revision control. | A successful search/refresh synchronizes the selected row, edit mode and editors, including a single result and refresh from ADD mode. No results clear previous editor values. Keys/read-only columns stay protected; released content remains protected by the existing database trigger. | [frmGenericMaint.vb](../dotnet/STERP.Desktop/Forms/System/frmGenericMaint.vb), `LoadList`, selection handling. Regression and native retest pending. | Open |

Six complete workflow results and GUI-028 repair evidence will be recorded
after execution; successful preliminary enquiries are not end-to-end coverage.
