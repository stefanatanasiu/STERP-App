# STERP - Stefan's ERP: engineering conventions

> Binding rules for everyone (and every agent) working on STERP. Read the whole file before
> writing code. When this file and your instinct disagree, this file wins. If a rule blocks
> you, note it in your module's tech-debt or hand-off notes rather than silently ignoring it.

STERP - Stefan's ERP is a deliberately *legacy-styled* UK supply-chain ERP used to demonstrate modernisation.
It must be **realistic, working and tested**, with a **substantial majority of the code in COBOL**
(roughly 70% of hand-written source lines is the guide, measured by `build\loc-report.ps1`; it is not
a hard gate – never pad code or sacrifice quality to hit a number). Business rules belong in COBOL
first, then in stored procedures. The .NET layer is a thin, old-fashioned client.

The functional specification lives in the separate (private)
[STERP-Supply-Chain-Functional-Specification](https://github.com/stefanatanasiu/STERP-Supply-Chain-Functional-Specification)
repository, cloned next to this one as `..\STERP-Supply-Chain-Functional-Specification\` (316 requirements, acceptance scenarios in `12-...md`, decisions D-01..D-30 in `13-...md`).

**Product name.** The product is called **STERP - Stefan's ERP** wherever it identifies itself:
title bars, splash and About screens, help text, console banners, file headers and document
titles. In .NET use `SterpConfig.ProductName`; never hard-code another name. COBOL program
headers carry `SYSTEM : STERP - Stefan's ERP` followed by a `MODULE :` line. The short form
`STERP` remains correct in dialog captions, prose and identifiers (database, namespaces, EXE
names, `STERP-E-nnnn` message ids).

---

## 1. Architecture and folders

```
WinForms client (VB.NET, x86, MDI)   dotnet\STERP.Desktop       -> forms only, no business rules
Business facades (VB.NET)            dotnet\STERP.Business      -> orchestrate SP calls + COBOL LINKs
Data access (C#)                     dotnet\STERP.Data          -> SqlHelper (ADO.NET, stored procs)
COBOL interop (C#)                   dotnet\STERP.CobolInterop  -> CobolGateway.Link(prog, commarea)
COMMAREA bridge (C)                  c\sterpbrg.c               -> sterp_init / sterp_link (cdecl)
COBOL online engine                  cobol\online\<mod>\*.cob   -> linked into bin\x86\STERPCOB.dll
SQLIO (COBOL -> ODBC 18 -> SQL)      cobol\online\common\SQLIO.cob + c\sterpsql.c
COBOL batch / reports                cobol\batch\<mod>\*.cob    -> bin\x86\<NAME>.exe
COBOL unit tests                     cobol\tests\<mod>\T*.cob   -> bin\tests\T*.exe
Operator console (SCREEN SECTION)    cobol\tui\STERPOPS.cob     -> bin\x86\STERPOPS.exe
SQL Server 2025 database STERP       db\...                     -> build\build-db.ps1
JCL-style job streams                jobs\*.cmd                 -> run\logs, run\spool
Runtime folders                      run\spool extracts bacs mtd einvoice logs work
```

Module codes (lower case for folders, upper case for requirement families):
`common`, `fin` (FIN), `ar` (AR), `ap` (AP), `so` (SO), `po` (PO), `cst` (CST), `itm` (ITM),
`kit` (KIT), `bom` (BOM), `xmd` (XMD, RPT, COM cross-cutting), `tui`.

### Ownership while working in parallel
* Only create/edit files inside **your module's** folders:
  `cobol\online\<mod>`, `cobol\batch\<mod>`, `cobol\tests\<mod>`, copybooks prefixed for your module
  in `cobol\copy`, `db\tables\NN-<schema>` (new files only), `db\procs\<schema>`, `db\views\<schema>`,
  `db\functions\<schema>`, `db\triggers\<schema>`, `db\seed\NN-<mod>-*.sql`, `db\tests\<mod>-*.sql`,
  `dotnet\STERP.Business\<Module>\`, `dotnet\STERP.Desktop\Forms\<Module>\`,
  `dotnet\STERP.Tests\<Module>\`, `docs\modules\<mod>.md`, `docs\tech-debt\<mod>.md`.
* Shared files (`STCAHDR.cpy`, `SQLIOCA.cpy`, `TESTKIT*.cpy`, `c\*`, `build\*`, framework classes,
  core tables) are owned by the foundation. Need a change? Make the smallest backwards-compatible
  change and describe it in your final report.
* Never drop/recreate the database (`build-db.ps1 -Full`) while others are working. Use
  `build-db.ps1 -Only <regex>` to deploy just your scripts. All scripts must be re-runnable.
* Builds take named locks (`Invoke-WithBuildLock`) so concurrent builds are safe. If the DLL link
  fails because **another** module has not finished, rebuild your module with `-NoDll`, run
  your COBOL tests (they statically link), and retry the DLL later.

---

## 2. Build, run and test commands

```
build\build-db.ps1 [-Full] [-Only regex] [-Tests] [-NoDeploy]
build\build-cobol.ps1 [-Module ar,fin] [-NoDll] [-NoBatch] [-NoTests] [-Clean]
build\build-net.ps1 [-Debug] [-Rebuild]
build\run-tests.ps1 [-Layer cobol|sql|net] [-Filter regex]
build\loc-report.ps1 [-Min 70]      build\trace-gen.ps1 [-Strict]
build\build-all.ps1 [-Full] [-Test]
```
Every `.ps1` has a `.cmd` wrapper. Scripts dot-source `build\env.ps1`, which sets the GnuCOBOL
environment from `..\GC32-BDB-SP1` (override with `STERP_GNUCOBOL`) and `STERP_ODBC`.
Paths contain spaces: always quote them. `sqlcmd` needs `-C` (trust server certificate).

Toolchain facts: GnuCOBOL 3.2 **32-bit** (MinGW) → the .NET client is **x86 only**.
`.NET Framework 4.8`, legacy (non-SDK) `.vbproj/.csproj` built by `dotnet msbuild`.
SQL Server 2025 default instance `.`, Windows authentication, database `STERP`.

---

## 3. COBOL rules

### 3.1 Names and files
* `PROGRAM-ID` = file name = **1–8 characters, A–Z/0–9, no hyphens**, unique across the system.
* Copybooks live in `cobol\copy` (flat), file name ≤ 10 chars + `.cpy`, upper case.
  Prefix module copybooks with the module (e.g. `ARCUSREC.cpy`, `BOMEXPCA.cpy`).
* COMMAREA copybook for LINKable program `XXXXXXXX` is **`XXXXXXXXCA.cpy`** (shorter names fine:
  `PINGCA.cpy`).

### 3.2 Dialect and source format (era flavour) — see `cobol\DIALECTS.cfg`
| Module | `-std` | Format | Style |
|---|---|---|---|
| common | default | fixed | 2012 rewrite – structured, EVALUATE, inline PERFORM |
| fin | mf | fixed | 1994 Micro Focus – UPPER CASE, numbered paragraphs, PERFORM THRU, some GO TO |
| ar, ap | ibm | fixed | 1991 MVS port – UPPER CASE, SECTIONs, PERFORM THRU/EXIT, GO TO DEPENDING, 88-levels |
| so, po | acu | fixed | 1998 ACUCOBOL – UPPER CASE, numbered paragraphs, mixed old/new |
| itm, cst | rm | fixed | 1996 RM/COBOL – UPPER CASE, SECTIONs |
| bom, kit | default | **free** | 2014/2016 rewrite – first line `>>SOURCE FORMAT IS FREE`, mixed case, RECURSIVE, LOCAL-STORAGE, user FUNCTION-IDs |
| xmd (batch) | mvs | fixed | 1993 batch suite – SORT, Report Writer |
| tui | default | fixed | 2008 – SCREEN SECTION |

* **Fixed format:** code in columns 8–72 only. Never continue a literal across lines – build long
  text with `STRING ... DELIMITED BY SIZE INTO`. Comments: `*` in column 7 or `*>`.
* **`acu` (so, po) sets `console-is-crt`:** a bare `DISPLAY` goes to the curses screen, not stdout,
  and `STOP RUN` then waits for a keypress (`COB_EXIT_WAIT`) — a batch job "hangs" after writing its
  listing. Every `DISPLAY` in so/po code must say `UPON SYSOUT` (or `UPON SYSERR`). `env.ps1` and
  `jobs\SETENV.cmd` also set `COB_EXIT_WAIT=0` as a backstop.
* **Copybooks must compile in both formats:** columns 8–72, `*>` comments only, nothing in col 7.
* Dialects share sign/packed layouts, but **binary sizes and default initialisation differ**
  (`ibm`/`mvs` initialise to LOW-VALUES, others to spaces). Therefore:
  * In any record shared between programs, copybooks or with .NET use only `PIC X`, `PIC 9`
    DISPLAY, signed DISPLAY, `COMP-3`, and `PIC S9(9) COMP-5` (4 bytes). Never plain `COMP`/`BINARY`.
  * `INITIALIZE` working storage explicitly; never rely on default values without `VALUE`.
* Avoid `cobol2014` strict features not in GnuCOBOL; `JSON GENERATE`, Report Writer, `SCREEN
  SECTION`, `FUNCTION-ID`, `IS RECURSIVE`, `LOCAL-STORAGE`, `SORT`, `ROUNDED MODE` all work.
* Build with `-fstatic-call`: `CALL "LITERAL"` is a static link. Dynamic `CALL identifier` is not
  supported in the DLL – always call literals.

### 3.3 LINKable programs (callable from .NET) and inter-program calls
* A program is LINKable when its procedure division is exactly
  `PROCEDURE DIVISION USING DFHCOMMAREA.` – the build then adds it to the generated program
  directory in `STERPCOB.dll`. It must have exactly one parameter.
* Its COMMAREA copybook:
  ```cobol
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  XX-BODY.
               10 ...
  ```
  `STCAHDR` is the 400-byte standard header: program, function, company, site, user, business
  date (YYYYMMDD), correlation id (idempotency), return code, reason code, message.
* **Allowed COMMAREA field types** (parsed at run time by `CobolInterop` from the .cpy file):
  `PIC X(n)`, `PIC 9(n)[V9(m)]`, `PIC S9(n)[V9(m)]` (trailing sign, default), `... SIGN LEADING
  SEPARATE`, `PIC S9(n)[V9(m)] COMP-3`, `PIC S9(9) COMP-5`, groups, `OCCURS n TIMES` (fixed; put
  the used count in a separate field), `FILLER`, `88` levels (ignored by .NET), `REDEFINES`
  (avoid in COMMAREAs). No `OCCURS DEPENDING ON`, no `COPY REPLACING` inside COMMAREA copybooks
  (a plain nested `COPY STCAHDR.` is fine). Keep a COMMAREA below 60,000 bytes.
* Return codes: `00` OK, `04` warning, `06` hold (needs authority/approval), `08` error – no
  business effect, `12` severe (SQL/system), `16` not found. Put a stable reason code in
  `CA-REASON-CODE` (e.g. `VAT0003`, `CRD0001`) and a human message in `CA-MESSAGE`
  (British English, e.g. "Credit limit exceeded by £1,250.00"). Holds/warnings/errors must be
  distinguishable (COM-017).
* **Prefer making reusable services LINKable** so .NET and COBOL call them the same way. A COBOL
  caller uses `COPY XXXXCA REPLACING ==DFHCOMMAREA== BY ==WS-XXXX-CA==.` in WORKING-STORAGE and
  `CALL "XXXX" USING WS-XXXX-CA`. (Copybook `REPLACING` is fine in the *caller*.)
* Internal-only subprograms with several parameters are allowed (not LINKable, not in the
  directory).
* **WORKING-STORAGE persists between calls inside the DLL.** Re-initialise per call (or use
  `LOCAL-STORAGE`). Never `STOP RUN` in an online program – always `GOBACK`. No console
  `ACCEPT`/`DISPLAY` in online programs (traces `UPON SYSERR` only).
* Recursive programs: `PROGRAM-ID. BOMEXPL IS RECURSIVE.` with `LOCAL-STORAGE SECTION`.
* Online programs must not crash on bad input: validate and return `08`. (A COBOL runtime
  error terminates the hosting .NET process.)

### 3.4 Database access from COBOL (SQLIO)
* Only through `CALL "SQLIO" USING SQLIO-CA <row-area>` (copybook `SQLIOCA`). Functions:
  `EXEC`, `FTCH`, `SNGL`, `MORE`, `CLOS`, `BEGN`, `COMT`, `ROLB`, `CONN`, `DISC`.
* Always parameterise with `?` and `SQLIO-PARM-VAL(n)` / `SQLIO-PARM-CNT`; never STRING user data
  into SQL text. Numbers: `MOVE n TO SQLIO-NUM-EDIT` then `MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
  TO SQLIO-PARM-VAL(i)`. Dates as `YYYYMMDD`. Set `SQLIO-PARM-NULL(i)` to `"N"` (or `"Y"`).
* Fetch: set `SQLIO-COL-CNT` and `SQLIO-COL-WIDTH(n)`; columns arrive as left-justified text;
  convert with `FUNCTION NUMVAL`. Decimal text may look like `.50` or `-1234.500000`. Dates arrive
  as `YYYY-MM-DD` – convert with common services. Use `CONVERT(char(8), d, 112)` in the select
  list if you prefer `YYYYMMDD`.
* Cursors 1–8 allow nested loops (connection uses MARS). Close cursors you open.
* Stored procedures are called the same way: `EXEC fin.usp_PostJournal ?, ?, ?`. Procedures
  return results as **result sets** (no OUTPUT parameters for COBOL callers).
* A business posting is owned **either** by a stored procedure **or** by a COBOL program wrapped in
  `BEGN`/`COMT`/`ROLB` – never split across the .NET connection and the COBOL connection
  (they are different sessions). COM-014: all-or-nothing.
* SQLIO errors → `CA-RETURN-CODE 12`, reason `SQLnnnn`, message from `SQLIO-ERRMSG`.
* The COBOL connection string comes from env `STERP_ODBC` (set by `env.ps1` and by the client via
  `sterp_init`). Trace with env `STERP_SQLTRACE=Y`.

### 3.5 Program header, requirement tags, legacy flavour
Every program starts with a banner: program id, system, purpose, called by, interface, and a
**change log reaching back to the module's era** (fictional initials, dates, change requests:
`1996-02-11 RJH CR0412 ...`, `1999-11-30 KMP Y2K - 4 DIGIT YEARS`, `2021-01-01 ... BREXIT VAT`,
`2026-10-04 STERP ...`). Tag every requirement implemented:
```
      *> REQ: AR-024, AR-003       (any comment line containing "REQ:" followed by IDs)
```
Tests tag `REQ:` and acceptance scenario `AC: AC-005`. `build\trace-gen.ps1` builds
`docs\TRACEABILITY.md`; every one of the 316 IDs must end up tagged in implementation code.

Realistic legacy texture is **wanted** but must stay correct and be documented in
`docs\tech-debt\<mod>.md` (one bullet per item: file, pattern, why it's debt, modernisation
hint): e.g. `PERFORM THRU` with `GO TO xxx-EXIT`, a dormant `ALTER` in an old paragraph,
duplicated rule in a stored procedure and in COBOL, hard-coded `OCCURS 200` limits, magic numbers,
Y2K windowing remnants (`IF YY < 50`), commented-out code with dates, cryptic field names
(`WS-AMT2`), copy-pasted paragraphs. No real security holes (no SQL built from user text).

### 3.6 Batch programs and reports
* Main programs in `cobol\batch\<mod>\NAME.cob` → `bin\x86\NAME.exe` (statically linked with all
  online modules, so batch can `CALL` any online program).
* Arguments: `ACCEPT WS-CMD FROM COMMAND-LINE`, `KEY=VALUE` pairs separated by spaces, e.g.
  `AGEDDEBT CO=STC ASAT=20261004 USER=ARCLERK1`. Common keys: `CO`, `SITE`, `USER`, `ASAT`,
  `FROM`, `TO`, `PERIOD` (YYYYPP), `MODE` (`PREVIEW`/`COMMIT`), `OUT`.
* Output files go under `%STERP_RUN%` (env; default `..\run` relative to the exe):
  spool `run\spool\<REPORT>-<CO>-<yyyymmddhhmmss>.lst` (132 columns, Report Writer preferred,
  page headings with company name, report id, run date dd/mm/yyyy, page n), Bacs files
  `run\bacs`, MTD JSON `run\mtd`, e-invoices `run\einvoice`, extracts `run\extracts`.
* Console messages use message ids: `STERP-I-nnnn text`, `STERP-W-nnnn`, `STERP-E-nnnn`; the last
  line of a report program is `STERP-I-SPOOL <full path of spool file>`. `RETURN-CODE`
  0/4/8/12/16 like the online convention.
* Batch runs register in `core.JobRun` via `core.usp_JobRunStart` / `core.usp_JobRunEnd`
  (job monitor in the client) using the common service `JOBLOG`; status values are `RUNNING`,
  `OK`, `WARNING`, `FAILED`, `ABENDED` (see `docs\DATA-MODEL.md`).
* Common services (rounding, dates, terms, FX, parameters, numbering, audit, exceptions,
  permissions, periods, idempotency, command-line args, run paths): **reuse them, never
  re-implement** – see `docs\COMMON-SERVICES.md`.
* Database procs that `THROW` doom an open SQLIO transaction (`XACT_ABORT ON`). When a COBOL
  program needs a yes/no answer inside its own `BEGN`…`COMT`, query the table directly or use a
  proc that returns a decision row.

### 3.7 COBOL unit tests
* `cobol\tests\<mod>\T<NAME>.cob` (executable name must start with `T`), compiled with the module
  dialect, statically linked with all online modules.
* Use `COPY TESTKITW.` (working storage) and `COPY TESTKITP.` (last thing in the procedure
  division): `MOVE "name" TO TK-NAME`, `PERFORM TK-PASS/TK-FAIL`, or set `TK-EXPECTED-N`/
  `TK-ACTUAL-N` and `PERFORM TK-ASSERT-NUM`, `TK-EXPECTED-X`/`TK-ACTUAL-X` + `TK-ASSERT-STR`;
  start with `TK-BEGIN`, finish with `TK-END` then `STOP RUN`. Output is TAP (`ok n - ...`).
* **Use the specification's worked numbers** (AC scenarios, BOM examples B1–B6, VAT/FX/late
  payment examples) as expected values and tag them `REQ:`/`AC:`.
* Tests that touch the database must be repeatable: wrap in `BEGN`…`ROLB`, or use unique keys and
  clean up.

---

## 4. Database rules (SQL Server 2025, database `STERP`)

### 4.1 Schemas
`core` (companies, sites, users, roles, permissions, SoD, calendars, numbering, currencies, FX,
parameters, audit, exceptions, approvals, idempotency, sanctions, menus, jobs, reports, retention),
`fin`, `ar`, `ap`, `so`, `po`, `inv` (items & stock – module `itm`), `cst`, `bom`, `kit`,
`rpt` (reporting views), `stg` (import staging).

### 4.2 Files and order (deployed by `build-db.ps1`, every script re-runnable)
```
db\00-create-database.sql  db\01-schemas.sql
db\tables\00-core\  10-fin\  20-inv\  30-bom\  40-cst\  50-ar\  60-ap\  70-so\  80-po\  90-kit\  95-xmd\  99-fk\
          NN-TableName.sql   (IF OBJECT_ID(N'schema.Table') IS NULL CREATE TABLE ...;
                              new columns: IF COL_LENGTH(...) IS NULL ALTER TABLE ... ADD ...)
db\functions\<schema>\fn_Name.sql   db\views\<schema>\vw_Name.sql   db\procs\<schema>\usp_Name.sql
db\triggers\<schema>\tr_Name.sql    (CREATE OR ALTER)
db\seed\NN-<mod>-<topic>.sql        (MERGE / IF NOT EXISTS; NN: 00-09 core, 10 fin, 20 itm, 30 bom,
                                     40 cst, 50 ar, 60 ap, 70 so, 80 po, 85 kit, 90+ demo transactions)
db\jobs\*.sql                       (SQL Agent jobs, run against msdb)
db\tests\<mod>-<topic>.sql          (see 4.6)
```
Cross-schema foreign keys that point to a later folder go in `db\tables\99-fk\`.

### 4.3 Naming and types
* Tables PascalCase singular (`ar.SalesInvoice`, `ar.SalesInvoiceLine`). Surrogate keys
  `<Table>Id int IDENTITY` (bigint for high-volume: movements, journal lines, audit); business
  keys are `varchar` codes with unique constraints scoped by `CompanyCode` (COM-001/COM-004).
* Every company-owned row has `CompanyCode varchar(10) NOT NULL`. Site-owned rows `SiteCode`.
* Codes upper case (D-21): items up to 40 chars `[A-Z0-9-./_]`.
* Money `decimal(18,2)` (document currency and GBP base side by side: `...Amount`, `...AmountGBP`);
  unit prices/costs `decimal(19,6)`; quantities `decimal(19,6)` (D-09 six decimals);
  FX rates `decimal(18,8)` **foreign units per £1** (D-14); percentages `decimal(9,4)`.
* Dates `date`; timestamps `datetime2(3)`; status columns `varchar(12)` with CHECK constraints.
* Standard columns on mutable tables: `CreatedBy varchar(20)`, `CreatedAt datetime2(3) DEFAULT
  SYSDATETIME()`, `ModifiedBy`, `ModifiedAt`, `RowVer rowversion` (optimistic concurrency, COM-016).
* Confirmed documents keep snapshots of commercial inputs (address, terms, prices, VAT, units –
  COM-007); never update a posted document's money – reverse and replace (COM-006/FIN-006).

### 4.4 Stored procedures
* `schema.usp_VerbNoun`, `SET NOCOUNT ON; SET XACT_ABORT ON;`, explicit transactions for postings,
  `THROW 5xxxx, 'STERP-E-xxxx: message', 1;` for business errors (message is shown to users).
* Return data as result sets (COBOL reads result sets; no OUTPUT params required by COBOL callers).
* Permission-sensitive procs take `@UserCode` and call `core.usp_CheckPermission` (COM-002).
* Mutations of protected data write `core.AuditLog` (directly or via triggers) (COM-013).
* Repeatable instructions take an idempotency key (`@CorrelationId`) and use `core.IdempotencyKey`
  (COM-015). Use `UPDLOCK, HOLDLOCK` for numbering / reservation races (COM-005, SO-009).

### 4.5 Core contract objects (exact names – other layers depend on them)
| Object | Key columns / parameters |
|---|---|
| `core.Company` | `CompanyCode` PK, `Name`, `RegisteredNumber`, `VatRegNo`, `Eori`, `AccountingFramework`, `AccountingRefDate`, `VatStagger`, `VatScheme`, `BaseCurrency` ('GBP'), `IsActive` |
| `core.Site` | (`CompanyCode`,`SiteCode`) PK, `Name`, `Nation` ('ENG','SCT','NIR','WAL'), `CalendarCode` |
| `core.AppUser` | `UserCode` PK, `DisplayName`, `WindowsLogin` (nullable, unique), `IsActive` |
| `core.Role`, `core.UserRole` | `UserRole(UserCode, RoleCode, CompanyCode, SiteCode NULL = all sites, ValidFrom, ValidTo)` |
| `core.Permission`, `core.RolePermission` | `PermissionCode` like `AR.RECEIPT.POST`, `IsSensitive` |
| `core.usp_CheckPermission` | `@UserCode, @PermissionCode, @CompanyCode, @SiteCode = NULL` → row (`Allowed` bit, `Reason`) |
| `core.usp_GetUserContext` | `@WindowsLogin = NULL, @UserCode = NULL` → user row; then rows of permitted (company, site) |
| `core.Parameter` | (`ParamCode`,`CompanyCode` ('*' = all),`EffectiveFrom`) PK, `Value`, `DataType`, `DecisionRef` |
| `core.fn_Param` / `core.usp_GetParameter` | `(@ParamCode, @CompanyCode, @AsOf)` → effective value |
| `core.NumberSeries`, `core.usp_NextDocumentNumber` | `@CompanyCode, @SeriesCode, @UserCode` → row (`DocumentNumber`) |
| `core.AuditLog`, `core.usp_WriteAudit` | `EventTime, UserCode, CompanyCode, EntityType, EntityKey, Action, FieldName, OldValue, NewValue, Reason, AuthorityRef, Source` |
| `core.ExceptionItem`, `core.usp_RaiseException`, `core.usp_ResolveException` | `CompanyCode, Module, Severity ('HOLD','WARNING','ERROR'), Category, DocumentType, DocumentRef, Message, OwnerUser, OwnerRole, Priority, RaisedAt, DueAt, Status ('OPEN','ACKNOWLEDGED','RESOLVED'), ResolvedBy, ResolvedAt, ResolutionNote` |
| `core.Approval`, `core.usp_RequestApproval`, `core.usp_DecideApproval`, `core.usp_InvalidateApprovals` | `CompanyCode, DocumentType, DocumentRef, ApprovalType, Amount, ContentHash, RequestedBy, RequestedAt, Status ('PENDING','APPROVED','REJECTED','INVALIDATED'), DecidedBy, DecidedAt, Comment`; self-approval and SoD conflicts rejected (COM-003) |
| `core.IdempotencyKey`, `core.usp_IdempotentBegin/End` | `CorrelationId` PK, `Operation`, `Status`, `ResultRef` |
| `core.Currency`, `core.FxRate` | `FxRate(CurrencyCode, RateDate, RateType ('SPOT','HMRC','BOE'), UnitsPerGBP, Source)` |
| `core.Calendar`, `core.CalendarDay` / `core.Holiday` | calendars `ENG` (England & Wales), `SCT`, `NIR`, `BACS`; 2026–2028 |
| `core.PaymentTerms` | `TermsCode`, `BasisType` ('INVOICE','EOM','DAYOFNEXTMONTH'), `Days`, `DayOfMonth`, `DiscountDays`, `DiscountPct` |
| `fin.Period` | (`CompanyCode`,`FiscalYear`,`PeriodNo`) PK, `StartDate`, `EndDate`, `Status` ('FUTURE','OPEN','SOFT','HARD') |
| `core.MenuItem` | `MenuCode varchar(40)` PK (e.g. `AR.RECEIPTS`), `ParentMenuCode` (top level: `FILE`,`SYSTEM`,`FIN`,`AR`,`AP`,`SO`,`PO`,`CST`,`ITM`,`KIT`,`BOM`,`RPT`,`WINDOW`,`HELP`), `Caption` (with `&` accelerator), `FormClass` (NULL for sub-menus), `FormArgument`, `PermissionCode`, `SortOrder`, `Shortcut`, `IsActive` |
| `core.MaintDef`, `core.MaintColumn` | generic maintenance metadata (owned by the .NET framework; see `docs\NET-FRAMEWORK.md`) |
| `core.ReportDef` | `ReportCode`, `Module`, `Title`, `ProgramName` (batch EXE), `ParamSpec` (e.g. `CO,ASAT`), `PermissionCode`, `RequirementRefs` |
| `core.JobRun`, `core.usp_JobRunStart/End` | `JobRunId`, `JobName`, `CompanyCode`, `SubmittedBy`, `StartedAt`, `EndedAt`, `Status`, `ReturnCode`, `LogFile`, `SpoolFile` |

### 4.6 SQL tests
`db\tests\<mod>-<topic>.sql`: run inside `BEGIN TRAN … ROLLBACK`, print TAP lines with `PRINT 'ok 1 -
...'` / `PRINT 'not ok 1 - ...'` and tag `-- REQ: ...`. Use `BEGIN TRY … END TRY BEGIN CATCH` to
assert that a business rule raises an error.

### 4.7 Reference identities (seeded by the core seed – use these in every module)
* Fiscal year = April–March (accounting reference date 31 March). FY2026 = 1 Apr 2026–31 Mar 2027,
  periods 1–12 (April = P01). FY2025 periods are HARD closed, Apr–Aug 2026 HARD, Sep 2026 (P06)
  SOFT, Oct 2026 (P07) OPEN, later FUTURE/OPEN. "Today" for demos is the system date (≈ Oct 2026).
* Companies: **STC** – *Stellar Components Ltd* (England & Wales, sites **RDG** Reading, **BHM**
  Birmingham), **CAL** – *Caledon Assemblies Ltd* (registered in England, site **GLA** Glasgow on
  the Scottish calendar, **LIV** Livingston). Base currency GBP; foreign EUR, USD.
* Users (WindowsLogin of the installing user is mapped to `SYSADM`):
  `SYSADM`, `FINCTRL` (financial controller), `GLCLERK`, `ARCLERK`, `CREDMGR`, `APCLERK`, `APMGR`,
  `TREASURY`, `BUYER`, `PURMGR`, `SALESREP`, `SALESMGR`, `WHSE`, `ENGINEER`, `ENGMGR`, `COSTACC`,
  `VATOWNER`, `AUDITOR` (read-only), `RESTRICT` (company STC / site RDG only, no bank details).
* VAT codes: `S` 20%, `R` 5%, `Z` 0%, `E` exempt, `O` outside scope, `RC` reverse charge services,
  `DRC` domestic reverse charge, `PIV` postponed import VAT, `EXP` zero-rated export.

---

## 5. .NET rules (.NET Framework 4.8, x86, legacy projects)

* Projects (legacy non-SDK format, wildcard `Compile Include` so modules add files without editing
  project files): `STERP.Common` (C#), `STERP.Data` (C#), `STERP.CobolInterop` (C#),
  `STERP.Business` (VB.NET), `STERP.Desktop` (VB.NET WinForms, `STERP.exe`),
  `STERP.Tests` (C# console harness, `STERP.Tests.exe`). All output to `bin\x86`.
* Language level: VB 15 / C# 7.3 maximum (no newer syntax), `Option Strict Off` in VB (legacy).
* Forms: `dotnet\STERP.Desktop\Forms\<Module>\frmXxx.vb` + `frmXxx.Designer.vb`, inherit
  `STERP.Desktop.BaseForm` (classic look, F-keys). Register in the menu by seeding
  `core.MenuItem` (`FormClass` = class name). Simple masters use the generic maintenance form
  (seed `core.MaintDef`/`core.MaintColumn`) instead of a hand-written form.
* Business facades: `dotnet\STERP.Business\<Module>\XxxService.vb` – call stored procedures via
  `SqlHelper` and COBOL via `CobolGateway`. No business rule in forms or facades beyond
  orchestration and display formatting.
* See `docs\NET-FRAMEWORK.md` for the framework API (written by the framework owner).
* UI look (COM-024): classic Windows (no visual styles), Microsoft Sans Serif 8.25pt, grey forms,
  MDI child windows, toolbar + status bar, F2 lookup, F3/Esc close, F5 refresh, F10 post/confirm,
  labels on every field, status shown as text not only colour, dates `dd/MM/yyyy`, money `£#,##0.00`,
  explicit currency and unit captions.
* **Keep .NET thin**: aim for ≤ ~900 lines of VB/C# per module (designer files included).

---

## 6. Line-count guide per module (COBOL should clearly dominate – ~70% overall is a guide, not a gate)
| Layer | Target per module |
|---|---|
| COBOL (online + batch + tests + copybooks) | **≥ 5,000 lines** (Finance, Sales, BOM 6,000+) |
| T-SQL procs/views/functions/triggers (excl. seed) | ≤ ~450 lines |
| VB.NET/C# (incl. designer files) | ≤ ~800 lines – prefer generic maintenance forms, ListForm, report launcher |

The foundation layers ended up at ~55% COBOL, so modules must be strongly COBOL-weighted
(business rules, validation, calculations, batch runs, reports and tests all in COBOL).

Check with `build\loc-report.ps1 -NoWrite`.

---

## 7. Definition of done for a module
1. DB objects deploy cleanly and re-run cleanly (`build-db.ps1 -Only <your folders>` twice).
2. `build-cobol.ps1 -Module <mod>` builds with zero errors; online programs are in the DLL.
3. COBOL tests and SQL tests for the module are green (`run-tests.ps1 -Filter <prefix>`), using
   spec numbers where the spec has them.
4. Forms open from the menu, load seeded data, and run their main action end-to-end.
5. Every requirement of the family is tagged `REQ:` in implementation code; tests tag what they test.
6. `docs\modules\<mod>.md` (programs, procs, forms, flows, how to demo) and
   `docs\tech-debt\<mod>.md` written.
