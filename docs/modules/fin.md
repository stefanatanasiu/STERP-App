# General Ledger and Accounts (module `fin`)

The finance engine of the suite: the chart of accounts, the single posting contract every other
module posts through, periods and close, VAT determination and the nine box MTD return, budgets,
foreign currency revaluation, bank reconciliation and the statutory statements.

Spec: `02-Finance-General-Ledger-and-Accounts.md`. Dialect: **`mf`** (Micro Focus, fixed format)
per `cobol\DIALECTS.cfg`. Schema: `fin`.

---

## 1. Interfaces other teams need (read this first)

### 1.1 `fin.usp_PostJournal` — the single posting contract

**Every module posts to the ledger through this one procedure.** There is no second way in, and
nothing is ever sent to a suspense account to make it balance.

Because COBOL cannot pass a table-valued parameter, the lines are staged first and the header is
passed as parameters:

```sql
INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode,
                         DebitAmount, CreditAmount, CurrencyCode,
                         DimCostCentre, DimProject, Narrative, VatCode, BankAccountCode, CreatedBy)
VALUES ('PO-GRN-000412-1', 1, 'STC', '1100', 4820.00, 0, 'GBP', NULL, NULL, 'Goods received', NULL, NULL, 'PORECV');
-- ... more lines, all under the same CorrelationId ...

EXEC fin.usp_PostJournal @CompanyCode = 'STC', @CorrelationId = 'PO-GRN-000412-1',
     @UserCode = 'BUYER', @Description = 'Goods received, order 000412',
     @PostingDate = '2026-10-14', @JournalType = 'GL', @SourceModule = 'PO',
     @SourceRef = 'GRN-000412', @EventCode = 'PO.GOODSRECEIPT', @SuppressResult = 1;
```

What it enforces, in this order, before anything is written:

| Check | Error |
| --- | --- |
| Correlation id supplied | `STERP-E-0016` |
| Lines staged under that correlation id | `STERP-E-0017` |
| A period covers the posting date, and it is not `HARD` or `FUTURE` | `STERP-E-0009` |
| `SOFT` period needs `FIN.PERIOD.SOFTPOST` | `STERP-E-0009` |
| Every account known, `ACTIVE`, `POSTING`, valid on the posting date | `STERP-E-0012` |
| A manual `GL`/`FIN` journal may not touch a control account | `STERP-E-0012` |
| Mandatory dimensions present, forbidden dimensions absent, values active | `STERP-E-0013` |
| `@EventCode`, if supplied, has an approved `fin.EventAccountMap` row | `STERP-E-0014` |
| A positive exchange rate exists for a non-sterling journal | `STERP-E-0010` |
| Debits equal credits **in document currency and in sterling**, and the journal is not nil | `STERP-E-0011` |

Three things callers get wrong:

* **`@SeriesCode` defaults to `'JNL'`** (`'JNLR'` on `usp_ReverseJournal`). These are the series
  actually seeded by `07-core-numbering-terms.sql`. The default used to be `'GLJNL'`, which exists
  nowhere, so every caller that relied on the default failed with `STERP-E-0004`. The `so`
  programs that once passed `'GLJNL'` explicitly (`SOB2B.cob`, `REBATE.cob`) have been corrected;
  no caller names it any more.
* **`@SuppressResult = 1`** stops the procedure returning its result set. Set it whenever you call
  `usp_PostJournal` from inside `INSERT ... EXEC`, because SQL Server forbids a nested
  `INSERT EXEC` (msg 8164). `usp_ReverseJournal` was rewritten for the same reason.
* **Repeating a correlation id is a no-op by design.** The second call returns the original
  journal with `WasDuplicate = 1` rather than posting again. This is what makes retries, the demo
  loader and `IDEMPOT` safe.

A LINKed online program **must not commit a transaction it did not open**. See `2400-BEGIN-WORK`
in `JNLENTRY.cob` for the `@@TRANCOUNT` ownership pattern: the program notes whether it opened the
transaction and only commits or rolls back if it did. `usp_PostJournal` itself does the same with
its `@own` flag.

### 1.2 Posting without a manual-journal block

`fin.Account.AllowManualJournal = 0` marks the control accounts (debtors, creditors, stock, bank,
VAT, GRNI, retained earnings). The block only applies to `@JournalType = 'GL' AND @SourceModule =
'FIN'` — a genuine hand-keyed finance journal. Post from the subledger that owns the balance
(`@SourceModule = 'AR'`, `'AP'`, `'CB'`, `'INV'`, `'PO'`, `'SO'`) and the control account is
allowed, which is exactly the rule the demo loader follows.

---

## 2. Requirement traceability (FIN-001 .. FIN-030)

All thirty identifiers are tagged `REQ:` in implementation code. To prove it:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File build\loc-report.ps1
Select-String -Path cobol\online\fin\*.cob,cobol\batch\fin\*.cob,db\procs\fin\*.sql,db\seed\1?-fin*.sql,dotnet\STERP.Business\Finance\*.vb -Pattern 'FIN-0\d\d' |
    ForEach-Object { $_.Matches.Value } | Sort-Object -Unique | Measure-Object
```

Tests carry `REQ:` and `AC:` tags against the acceptance scenarios in `12-...` and use the spec's
worked numbers as the expected values.

---

## 3. Online programs (`cobol\online\fin`, 15 programs, ~9,700 lines)

Each one is LINKable with a `DFHCOMMAREA` described by `<PROG>CA.cpy` in `cobol\copy`.

| Program | Copybook | What it does |
| --- | --- | --- |
| `GLVALID` | `GLVALIDCA` | Validates an account, dimension combination and posting date before anything is staged. |
| `JNLENTRY` | `JNLENTRYCA` | Builds and validates a manual journal header and lines; owns the `@@TRANCOUNT` pattern. |
| `JNLPOST` | `JNLPOSTCA` | Stages the lines and calls `fin.usp_PostJournal`. |
| `ACCRREL` | `ACCRRELCA` | Accruals and prepayments, and their automatic release. |
| `BUDGCOMP` | `BUDGCOMPCA` | Budget against actual for an account, period and analysis. |
| `PERCLOSE` | `PERCLOSECA` | Soft close, hard close, reopen, and the close checklist. |
| `BANKREC` | `BANKRECCA` | Matches statement lines to cash book entries; computes the reconciling difference. |
| `VATDET` | `VATDETCA` | VAT determination: liability, mechanism and rate at the tax point. |
| `VATRET9` | `VATRET9CA` | Assembles the nine box return and finalises it. |
| `VATERRC` | `VATERRCCA` | Error correction: adjust on the next return or notify, by threshold. |
| `FXREVAL` | `FXREVALCA` | Revalues monetary balances at the period end rate. |
| `CTLRECON` | `CTLRECONCA` | Control account to subledger reconciliation (kept in COBOL, see §5). |
| `GLADJUST` | `GLADJUSTCA` | Close adjustments into a soft closed period. |
| `OPENBAL` | `OPENBALCA` | Opening balance load and cut over reserve. |
| `STATPROF` | `STATPROFCA` | Statutory statement profile and line mapping. |

## 4. Batch programs (`cobol\batch\fin`, 10 programs, ~3,600 lines)

Command line `KEY=VALUE` arguments (CONVENTIONS §3.6), `JOBLOG` writes `core.JobRun`, Report Writer
at 132 columns via `STRPTHDR`, spool to `run\spool`, last line `STERP-I-SPOOL <path>`.

| Program | `core.ReportDef` | ParamSpec |
| --- | --- | --- |
| `TBRPT` | `TBRPT` | `CO,YEAR,PERFROM,PERTO,ZEROS` |
| `PLRPT` | `PLRPT` | `CO,YEAR,PERFROM,PERTO,COMPARE` |
| `BSRPT` | `BSRPT` | `CO,YEAR,PERIOD,COMPARE` |
| `CASHBOOK` | `CASHBOOK` | `CO,BANK,FROM,TO` |
| `BUDGRPT` | `BUDGRPT` | `CO,YEAR,PERIOD,BUDGET` |
| `MTDJSON` | `MTDJSON` | `CO,VATPERIOD` — `JSON GENERATE` to `%STERP_RUN%\mtd` |
| `RECURJNL` | `RECURJNL` | `CO,YEAR,PERIOD,MODE` |
| `REVALRUN` | `REVALRUN` | `CO,ASAT,MODE,RATE,USER` — selects the open foreign-currency positions itself and calls `FXREVAL` once per position; smoke-tested in PREVIEW by `cobol\tests\fin\TREVALRN.cob` |
| `YEAREND` | `YEAREND` | `CO,YEAR` |
| `FINDEMO` | `FINDEMO` (inactive) | `CO` — demonstration data loader, deliberately off the menu |

## 5. Stored procedures and views (`db\procs\fin`, 490 lines)

Non-seed T-SQL is at **490 lines against a ~450 budget**, so no further procedures may be added;
new finance logic goes in COBOL. `CTLRECON.cob` exists for exactly that reason.

Seven files hold **twelve** procedures — five procedures live inside a sibling file rather than in
a file of their own (see `docs\tech-debt\fin.md` §2):

| File | Procedures |
| --- | --- |
| `usp_PostJournal.sql` | `usp_PostJournal` |
| `usp_ReverseJournal.sql` | `usp_ReverseJournal` |
| `usp_TrialBalance.sql` | `usp_TrialBalance`, **`usp_AccountActivity`** |
| `usp_JournalEnquiry.sql` | `usp_JournalEnquiry`, **`usp_CloseStatus`**, **`usp_BankOutstanding`** |
| `usp_VatBoxes.sql` | `usp_VatBoxes`, **`usp_VatBoxDrill`** |
| `usp_BudgetCompare.sql` | `usp_BudgetCompare`, **`usp_ControlRecon`** |
| `usp_ValidatePeriod.sql` | `usp_ValidatePeriod` |

View: `fin.vw_FinanceReporting`. Event mapping seeds: `10-fin-eventmap.sql` and
`11-fin-eventmap.sql` (which also owns the current year `fin.VatPeriod` rows, keys `26A1` onward).

## 6. Forms and menu

VB.NET facades in `dotnet\STERP.Business\Finance`, forms in `dotnet\STERP.Desktop\Forms\Finance`,
harness tests in `dotnet\STERP.Tests\Finance`.

| Form | Facade | Behind it |
| --- | --- | --- |
| `frmJournalEntry` | `JournalService` | `GLVALID`, `JNLENTRY`, `JNLPOST` |
| `frmJournalEnquiry` | `JournalService` | `fin.usp_JournalEnquiry`, `fin.usp_AccountActivity` |
| `frmPeriodClose` | `PeriodService` | `PERCLOSE`, `fin.usp_CloseStatus`, `fin.usp_ValidatePeriod` |
| `frmVatReturn` | `VatService` | `VATDET`, `VATRET9`, `VATERRC`, `fin.usp_VatBoxes`, `fin.usp_VatBoxDrill` |
| `frmBankRecon` | `BankService` | `BANKREC`, `fin.usp_BankOutstanding` |

Simple masters have no hand written form. `db\seed\12-fin-menu.sql` seeds seven generic
maintenance definitions driven by `frmGenericMaint`: `FIN.ACCOUNT`, `FIN.ACCLASS`, `FIN.VATCODE`,
`FIN.VATRATE`, `FIN.BUDGET`, `FIN.BUDGETLINE`, `FIN.BANK`. The F2 lookups on the journal and bank
forms use `FIN.ACCOUNT` and `FIN.BANK`, so those two codes must not be renamed.

The same seed adds 24 menu rows under the `FIN` bar entry (`FIN.MAINT`, `FIN.RUN` and `FIN.RPT`
branches), the ten `core.ReportDef` rows above, the prior year VAT quarters `25A1`–`25A4`, the
`fin.StatutoryObligation` rows, and the `FIN.VAT.SUBMIT` grant that `FINCTRL` was missing.

`FINCTRL` is the **approver** role: it holds `FIN.JOURNAL.APPROVE/POST/REVERSE` and
`FIN.VAT.APPROVE/SUBMIT`, and deliberately **not** `FIN.JOURNAL.ENTER` or `FIN.VAT.PREPARE`, which
belong to `GLCLERK`/`COSTACC` and `VATOWNER`. The two pairs are blocking SoD rules (`SOD-JNL`,
`SOD-VAT`, D-07), so granting `FINCTRL` either origination code makes `core.usp_DecideApproval`
refuse every journal approval with `STERP-E-0007` and breaks `db\tests\core-approvals.sql`. The
seed now carries a guard that removes any `FINCTRL` grant forming an active BLOCK pair, so drive
`GLADJUST` and `VATRET9 PREPARE` with `GLCLERK`/`VATOWNER` and `FINALISE`/`SUBMIT` with `FINCTRL`.

## 7. Main business flows

* **Manual journal** — `frmJournalEntry` → `GLVALID` per line → `JNLENTRY` builds the header →
  `JNLPOST` stages into `fin.JournalStage` → `fin.usp_PostJournal` → `fin.Journal` /
  `fin.JournalLine`. Reversal goes through `fin.usp_ReverseJournal` on series `JNLR`.
* **Period close** — `PERCLOSE` walks `fin.CloseChecklist`; blocking steps must be `DONE` or
  `ACCEPTED` before a soft close becomes a hard close. A soft closed period still accepts a close
  adjustment from `GLADJUST`, but only with `FIN.PERIOD.SOFTPOST`. Year end is `YEAREND`, which
  closes the profit and loss accounts to `3200` and carries the balance sheet forward.
* **VAT** — `VATDET` fixes liability, mechanism and rate at the tax point and writes
  `fin.VatTransaction`. `VATRET9` builds the nine boxes from those transactions plus any approved
  error corrections; `fin.usp_VatBoxDrill` gets from a box back to the transactions. `MTDJSON`
  writes the submission payload. Boxes 2, 8 and 9 are always zero (decision D-23).
* **Bank** — `CASHBOOK` lists receipts and payments with the running book balance; `BANKREC`
  matches `fin.BankStatementLine` to the cash book and reports the difference; genuinely
  outstanding items stay outstanding and are never written off to suspense.

## 8. How to demo

```powershell
# from the STERP-App repository root
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-db.ps1 -Only "fin"
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-cobol.ps1 -Module fin
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-net.ps1
```

`db\seed\91-fin-demo.sql` loads 112 posted journals (294 lines) for **both** companies across
FY2026 periods 1 to 7, every one of them through `fin.usp_PostJournal`, plus 42 VAT transactions,
the October 2026 sterling bank statement, the approved `BUD26` budget (192 lines) and the close
checklist for periods 6 and 7. It opens the closed periods, posts, and then restores the close
baseline, so the demo still shows a year that is hard closed to August 2026.

Figures to expect (company `STC`, FY2026, periods 1 to 7):

| Check | Value |
| --- | --- |
| Trial balance debits less credits | `0.00` |
| Sales (`4000`) | `1,050,000.00` Cr |
| Cost of sales (`5010`) | `464,800.00` Dr |
| Bank (`1300`) | `376,085.00` Dr |
| VAT return `26A2`, box 1 / box 4 / box 5 | `94,500.00` / `47,670.00` / `46,830.00` |
| VAT return `26A2`, box 6 / box 7 | `472,500` / `238,350` |

```sql
EXEC fin.usp_TrialBalance @CompanyCode = 'STC', @FiscalYear = 2026, @PeriodFrom = 1, @PeriodTo = 7;
EXEC fin.usp_VatBoxes     @CompanyCode = 'STC', @VatPeriodKey = '26A2';
EXEC fin.usp_VatBoxDrill  @CompanyCode = 'STC', @VatPeriodKey = '26A2', @BoxNo = 1;
EXEC fin.usp_BudgetCompare @CompanyCode = 'STC', @FiscalYear = 2026, @PeriodNo = 7, @BudgetCode = 'BUD26';
EXEC fin.usp_CloseStatus  @CompanyCode = 'STC', @FiscalYear = 2026, @PeriodNo = 7;
EXEC fin.usp_BankOutstanding @CompanyCode = 'STC', @BankAccountCode = 'GBP1';
```

Box 3 must equal box 1 plus box 2, and box 5 must equal box 3 less box 4 — `94,500 − 47,670 =
46,830`. In the client, open **Finance → VAT Return**, pick `26A2`, drill into box 1 and then use
*Submit to file*; the payload lands in `%STERP_RUN%\mtd`. **Finance → Bank Reconciliation** on
`GBP1` statement `BS-2026-10` shows the matched cash book entries and two genuinely outstanding
items (credit interest `318.44` and an insurance direct debit `1,265.00`).

Do not leave `STERP.exe` running: it locks `STERPCOB.dll` and the next COBOL build will fail.
