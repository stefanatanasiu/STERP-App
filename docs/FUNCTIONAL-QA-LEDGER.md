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
| GUI-001 | Medium | Demo instructions repeatedly direct operators to `System > Sign on`, which does not exist. | Instructions identify the real user/company selection menu. | Actual paths are `File > Continue as User` and `File > Select Company / Site`. | Open |
| GUI-002 | High | Load an order, Explain price / Availability, then load another order or an unknown reference. Previous enquiry panes (and, for not-found, previous order data) remain actionable. | All displayed and actionable data belongs to the successfully loaded reference. | Screenshots `018`, `019`; sales form load/reset path. | Open |
| GUI-003 | High | Leave Sales Order Entry open as SALESREP, then continue as WHSE. The old form and customer data remain open even though the new menu cannot open that screen. | A successful session change closes old-context forms; cancelling must restore the original session. | Screenshot `021`; investigate login/session transaction and MDI lifecycle. | Open |
| GUI-004 | High | Despatch screen asks for a pick reference, but F2 opens the sales-order lookup and only fills the order field. Loading a documented order gives `Key a pick list reference first`. | Operator can locate and load the appropriate pick without database access. | Screenshots `023`, `025`. | Open |
| GUI-005 | High | Both documented picked orders (`SOS900008`, `SOC900004`) have no pick header/lines. Re-running the documented demo drivers does not create their picks. | The advertised ready-to-despatch demonstration orders have executable allocations and picks. | SQL verification plus screen `023`; fix demo preparation without fabricating posted transactions. | Open |
| GUI-006 | High | Continue as ARCLERK: the Sales menu is absent, so the documented invoice run cannot be opened. | The invoicing clerk has the narrowly scoped `SO.INVOICE.RUN` permission. | Screenshot `026`; no SO permission is seeded for ARCLERK. | Open |
| GUI-007 | High | Receipt Entry, load C0001, enter GBP1, gross/allocation 8,196 against the 8,200 invoice, click Preview residual. It refuses with `RCP0023: Name the receipt or credit ... before it is applied`. | Preview supports a new, not-yet-posted receipt and reports its proposed residual without posting. | Screenshot `033`; UI/RCPTALOC preview contract. | Open |
| GUI-008 | Medium | The documented C0001 two-invoice / every-ageing-bucket demo cannot be followed: seed contains only one outstanding invoice for that customer (8,200, part paid). | Demo fixture or instructions provide actual multi-invoice examples and accurate ageing expectations. | Screen `032` and `91-ar-demo.sql`; not caused by test transactions. | Open |

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
