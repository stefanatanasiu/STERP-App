# Functional GUI QA and defect ledger

Test started: 8 October 2026.

## Scope and method

Start with all six stories in [DEMO-WORKFLOWS.md](DEMO-WORKFLOWS.md), then extend
coverage to related entry, enquiry, reporting, validation and cross-module controls.
Exercise the native WinForms client using visible mouse/keyboard input and inspect
screenshots. Use database assertions and existing tests to verify the effects of GUI
actions, not as a substitute for exercising the screens.

The deliberately legacy WinForms, VB.NET, COBOL, C and SQL architecture is retained.
Expected business refusals (permissions, segregation of duties, immutable released
documents and closed periods) are passing controls, not bugs.

## Isolation and evidence

- Original database: `STERP`, not used for test transactions.
- Test database: `STERP_QA_20261008`, restored from a verified copy-only backup.
- Isolated runtime and evidence: `run\work\functional-qa-20261008`.
- Both ADO.NET and COBOL/ODBC are explicitly directed at the test database.
- Build baseline: .NET Release/x86 build passed before testing.
- Screenshots are cropped to the test process; no external service receives app data.

## Workflow coverage

| Workflow | State | Evidence / findings |
|---|---|---|
| 1. STC/RDG order to cash | Not started | |
| 2. STC/RDG BOM, kits and costing | Not started | |
| 3. STC/BHM procure to pay | Not started | |
| 4. CAL/GLA migration and trading | Not started | |
| 5. CAL/LIV credit hold and site access | Not started | |
| 6. STC finance and period close | Not started | |
| Extended navigation, input validation and reporting | Not started | |
| Automated regression and final reconciliation | Not started | |

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

## Limits

This is a local demonstration environment. File-based payment and tax outputs must
remain local; no bank, tax authority or external trading partner is contacted.
Coverage and final counts will be updated from observed results, not inferred from
the existing traceability tags.
