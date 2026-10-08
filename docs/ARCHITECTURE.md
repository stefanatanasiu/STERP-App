# STERP - Stefan's ERP: architecture

> STERP - Stefan's ERP is a UK supply-chain ERP (general ledger, debtors, creditors, sales, purchasing, stock,
> costing, kits, bills of materials, cross-module control and reporting) built in a deliberately
> **legacy technology mix** so that it can be used as a modernisation demonstration asset.
> This document describes how the parts fit together. Engineering rules live in
> [CONVENTIONS.md](CONVENTIONS.md); this file is the map.

| Layer | Technology | Where |
|---|---|---|
| Desktop client | VB.NET WinForms MDI, .NET Framework 4.8, **x86**, legacy (non-SDK) projects | `dotnet\STERP.Desktop` |
| Business facades | VB.NET service classes (orchestration only) | `dotnet\STERP.Business` |
| Data access / interop | C# `SqlHelper` (ADO.NET), `CobolGateway` + copybook parser | `dotnet\STERP.Data`, `dotnet\STERP.CobolInterop` |
| COMMAREA bridge | C, cdecl exports, CICS `LINK` emulation | `c\sterpbrg.c` |
| Business engine | GnuCOBOL 3.2 (32-bit), five dialects, ~86% of hand-written lines | `cobol\online`, `cobol\batch` |
| COBOL data access | Home-grown `SQLIO` service over an ODBC shim | `cobol\online\common\SQLIO.cob`, `c\sterpsql.c` |
| Operator console | GnuCOBOL `SCREEN SECTION` green screen | `cobol\tui\STERPOPS.cob` |
| Job streams | JCL in Windows clothing (`.cmd`) | `jobs\*.cmd` |
| Database | SQL Server 2025, 12 schemas, procedures, triggers, SQL Agent jobs | `db\` |

Source line mix (`build\loc-report.ps1`, see [LOC-REPORT.md](LOC-REPORT.md)): 181,790 lines of COBOL
programs and 14,413 of copybooks against 11,249 VB.NET, 3,594 C#, 10,365 T-SQL, 396 C and 1,272 of
scripts — **COBOL 86.5%** of 254,335 non-blank hand-written lines.

---

## 1. System context

```
                   +--------------------------------------------------+
                   |  Stellar Components Ltd (STC) - Reading, B'ham   |
                   |  Caledon Assemblies Ltd (CAL) - Glasgow, Livi'n |
                   +--------------------------------------------------+
                       |                 |                    |
             clerks &  |        operators|           schedulers|
             managers  |   (green screen)|          (Task Sched / SQL Agent)
                       v                 v                    v
   +-------------------------------+  +-------------+  +-----------------------+
   |  STERP.exe  (WinForms MDI)    |  | STERPOPS.exe|  |  jobs\*.cmd streams   |
   |  bin\x86, .NET 4.8, x86       |  | COBOL TUI   |  |  NIGHTLY/MONTHEND/... |
   +-------------------------------+  +-------------+  +-----------------------+
                       |                     |                     |
                       +----------+----------+----------+----------+
                                  |                     |
                       +----------v----------+   +------v-----------------+
                       | STERPCOB.dll        |   | batch EXEs (bin\x86)   |
                       | online COBOL engine |   | statically linked with |
                       | + sterp_link bridge |   | the whole online engine|
                       +----------+----------+   +------+-----------------+
                                  |                     |
                                  +----------+----------+
                                             |
                                    +--------v---------+        +------------------+
                                    | SQL Server 2025  |        | run\ output tree |
                                    | database  STERP  |        | spool bacs mtd   |
                                    | 12 schemas       |        | einvoice extracts|
                                    +--------+---------+        | logs work        |
                                             |                  +------------------+
                                    +--------v---------+
                                    | SQL Agent jobs   |
                                    | (db\jobs\*.sql)  |
                                    +------------------+

   External: HMRC MTD (JSON payloads, file drop), Bacs/BACSTEL-IP (Standard 18 files),
             PEPPOL/UBL e-invoicing (XML), bank statement imports, customs/commodity data.
             STERP produces and consumes *files* — there is no outbound HTTP anywhere.
```

## 2. Component diagram

```
 +-----------------------------------------------------------------------------------+
 |  dotnet\STERP.Desktop  (VB.NET, STERP.exe)                                         |
 |   frmMain (MDI) · frmLogin · menu driven from core.MenuItem · BaseForm · ListForm  |
 |   Forms\Finance Debtors Creditors Sales Purchasing Items Bom Costing Kits System   |
 +------------------------------------+----------------------------------------------+
                                      | forms call facades only
 +------------------------------------v----------------------------------------------+
 |  dotnet\STERP.Business  (VB.NET)   ServiceBase + one service class per use case    |
 |   JournalService, ReceiptService, PaymentService, OrderService, KitService, ...    |
 +---------------+---------------------------------------------+--------------------+
                 | ADO.NET (stored procedures)                  | COMMAREA LINK
 +---------------v-------------+              +-----------------v--------------------+
 |  dotnet\STERP.Data (C#)     |              | dotnet\STERP.CobolInterop (C#)       |
 |   SqlHelper, BusinessRule-  |              |  CopybookParser (.cpy -> field map)  |
 |   Exception (STERP-E-xxxx)  |              |  CommArea (byte buffer accessor)     |
 +---------------+-------------+              |  CobolGateway.Link(prog, commarea)   |
                 |                            +-----------------+--------------------+
                 |                                              | P/Invoke, cdecl, x86
                 |                            +-----------------v--------------------+
                 |                            |  c\sterpbrg.c  -> STERPCOB.dll       |
                 |                            |   sterp_init(odbc, rundir)           |
                 |                            |   sterp_link(prog, commarea, len)    |
                 |                            |   sterp_progcount / sterp_progname   |
                 |                            +-----------------+--------------------+
                 |                                              | cob_call, static link
                 |                            +-----------------v--------------------+
                 |                            |  cobol\online\<mod>\*.cob            |
                 |                            |  PROCEDURE DIVISION USING DFHCOMMAREA|
                 |                            |  (~152 LINKable programs)            |
                 |                            +--------+---------------+-------------+
                 |                                     | CALL "SQLIO"  | CALL common
                 |                            +--------v-------+  +----v-------------+
                 |                            | SQLIO.cob      |  | common services  |
                 |                            |  EXEC/FTCH/... |  | RNDALLOC DATECALC|
                 |                            +--------+-------+  | TERMCALC FXCONV  |
                 |                                     |          | PARMGET NUMGEN   |
                 |                            +--------v-------+  | PERMCHK PERIODCK |
                 |                            | c\sterpsql.c   |  | IDEMPOT AUDITLOG |
                 |                            | ODBC Driver 18 |  | EXCPRAIS JOBLOG  |
                 |                            | 8 cursors, MARS|  | CMDARGS RUNPATH  |
                 |                            +--------+-------+  | STRUTIL STFUNCS  |
                 |                                     |          +------------------+
 +---------------v-------------------------------------v-----------------------------+
 |  SQL Server 2025 — database STERP                                                  |
 |  core fin ar ap so po inv cst bom kit rpt stg · ~179 tables · usp_ procedures ·    |
 |  audit triggers · views · SQL Agent jobs                                           |
 +-----------------------------------------------------------------------------------+

 +-----------------------------------------------------------------------------------+
 |  Batch side: jobs\*.cmd  ->  bin\x86\<PROGRAM>.exe (Report Writer / SORT / files)  |
 |  SETENV.cmd · RUNSTEP.cmd · JOBEND.cmd · COND codes · core.JobRun · run\ tree      |
 |  STERPOPS.exe (cobol\tui) submits streams, watches core.JobRun, browses spool      |
 +-----------------------------------------------------------------------------------+
```

## 3. Runtime call paths

### 3.1 Online: UI → VB facade → CobolInterop → bridge → COBOL → SQLIO → SQL Server

```
frmJournalEntry.cmdPost_Click            VB.NET form, no business rules
  JournalService.Post(header, lines)     VB.NET facade (dotnet\STERP.Business\Finance)
    CobolGateway.Link("JNLPOST", ca)     C#: resolve program, marshal CommArea
      CommArea ca = new CommArea("JNLPOSTCA")      copybook parsed at run time
      ca["CA-COMPANY"] = "STC" ...                 400-byte STCAHDR header filled
      sterp_link("JNLPOST", bytes, len)            P/Invoke into STERPCOB.dll (cdecl, x86)
        cob_call("JNLPOST", 1, &commarea)          GnuCOBOL runtime, static call
          JNLPOST  PROCEDURE DIVISION USING DFHCOMMAREA
            CALL "PERMCHK"  / "PERIODCK" / "NUMGEN" / "IDEMPOT"   common services
            CALL "SQLIO" USING SQLIO-AREA           "BEGN"
            CALL "SQLIO" ... "EXEC" INSERT fin.JournalStage ...   parameterised (?)
            CALL "SQLIO" ... "EXEC" EXEC fin.usp_PostJournal ?,?,?
            CALL "SQLIO" ... "COMT"  (or "ROLB")
              sterpsql.c -> SQLExecDirect / SQLFetch (ODBC Driver 18, MARS)
                SQL Server: fin.JournalStage -> fin.usp_PostJournal -> fin.JournalHeader/Line
            MOVE 00 TO CA-RETURN-CODE / message into CA-MESSAGE
      <- CobolResult { ReturnCode, ReasonCode, Message, CommArea }
    service maps RC 00/04/06/08/12/16 to success, warning, authority prompt or
    BusinessRuleException (the message is what the user sees)
```

The COBOL engine is loaded **once per process**: `CobolGateway` calls `sterp_init(STERP_ODBC,
run dir)` on first use, which starts the GnuCOBOL runtime, opens the ODBC connection used by
every SQLIO call and builds the directory of LINKable programs (`sterp_progcount` /
`sterp_progname`, used by the diagnostics screen).

### 3.2 Online: UI → ADO.NET → stored procedure

Enquiries, grids, aggregations, reservations and set-based postings go straight to SQL Server —
no COBOL in the path:

```
frmCustomerEnquiry.Load
  DebtorService.Enquiry(custCode)            VB.NET facade
    SqlHelper.Fill("ar.usp_CustomerEnquiry", p)   C#, ADO.NET, CommandType.StoredProcedure
      SQL Server: result sets -> DataTable -> grid
  THROW 5xxxx 'STERP-E-xxxx: ...' -> SqlException -> BusinessRuleException -> message box
```

Reports are run the same way through `core.ReportDef` rows, or by submitting a batch program.

### 3.3 Batch job streams

```
STERPOPS (or the scheduler, or a developer)
  jobs\MONTHEND.cmd CO=STC YEAR=2026 PERIOD=7 MODE=PREVIEW
    SETENV.cmd      STERP_HOME/STERP_RUN/STERP_ODBC, parse KEY=VALUE, open SYSOUT,
                    core.usp_JobRunStart -> core.JobRun row, write the //JOB card
    RUNSTEP.cmd 010 bin\x86\REVALRUN.exe CO=STC PERIOD=202607 MODE=PREVIEW
                    -> ACCEPT FROM COMMAND-LINE (CMDARGS) -> JOBLOG -> SQLIO -> SQL Server
                    -> Report Writer spool in run\spool, "STERP-I-SPOOL <path>"
                    -> RETURN-CODE becomes LASTCC; MAXCC = max(MAXCC, LASTCC)
    ... COND check after every step: 0 clean, 4 warning, >=8 stops the stream
    JOBEND.cmd      end card, core.usp_JobRunEnd (OK/WARNING/FAILED/ABENDED), exit MAXCC
```

Batch EXEs are statically linked with the whole online engine, so a batch program can `CALL`
any online program directly (no bridge, no COMMAREA marshalling). `RESTART=STEPnnn` resumes a
stream; posting steps are idempotent per document (`IDEMPOT`, `core.IdempotencyKey`).

### 3.4 Operator console

```
STERPOPS.exe (cobol\tui, SCREEN SECTION, pdcurses)
  sign on -> menu -> job monitor   : SQLIO over core.JobRun / core.JobStep
             submit a stream       : CALL "SYSTEM" jobs\<STREAM>.cmd ... (detached)
             browse SYSOUT / spool : run\logs, run\spool
             exception work list   : core.Exception via EXCPRAIS
```

It uses the same SQLIO/ODBC path as every other COBOL program; it never touches .NET.

## 4. The COMMAREA contract

Every program the client can call has a procedure division of exactly
`PROCEDURE DIVISION USING DFHCOMMAREA.` and a copybook `<PROGRAM>CA.cpy` whose first item is the
400-byte standard header `STCAHDR`:

| Field | Picture | Meaning |
|---|---|---|
| `CA-PROGRAM` | `X(8)` | program name (also the `PROGRAM-ID`, 1–8 chars) |
| `CA-FUNCTION` | `X(8)` | verb within the program (`ADD`, `POST`, `ENQ`, `VALID`, …) |
| `CA-COMPANY` / `CA-SITE` | `X(10)` | company and site context from the sign-on |
| `CA-USER` | `X(20)` | STERP user code — authority is checked in COBOL by `PERMCHK` |
| `CA-BUSINESS-DATE` | `9(8)` | `YYYYMMDD` business date, never the system clock |
| `CA-CORRELATION-ID` | `X(36)` | idempotency key (`IDEMPOT`, `core.IdempotencyKey`) |
| `CA-RETURN-CODE` | `9(2)` | `00` OK, `04` warning, `06` hold (needs authority), `08` error, `12` severe, `16` not found |
| `CA-REASON-CODE` | `X(8)` | machine-readable reason (`STERP-E-1403`, `SQL0042`, …) |
| `CA-MESSAGE` | `X(200)` | the text the user sees |
| `CA-REQ-REF`, `CA-MSG-COUNT`, `FILLER` | | document reference, message count, spare |

Rules that make the contract work across five dialects and two runtimes:

* Only `PIC X`, `PIC 9`/`PIC S9` DISPLAY, `COMP-3` and `PIC S9(9) COMP-5` are allowed in a
  COMMAREA — never plain `COMP`/`BINARY`, whose size differs between `ibm`, `mvs`, `rm`, `acu`
  and the GnuCOBOL default.
* Tables are fixed `OCCURS n` with a separate used-count field; no `OCCURS DEPENDING ON`, no
  `REDEFINES`, no `COPY REPLACING` inside a COMMAREA.
* `CobolInterop` parses the `.cpy` **at run time** and builds the field map, so the copybook is
  the single definition of the interface — there is no generated C# DTO to keep in step.
  A mismatch is a run-time `KeyNotFoundException`, not a compile error (see
  [tech-debt/net.md](tech-debt/net.md)).
* The buffer is allocated at the copybook length and passed by reference; the program may update
  any field, and the facade reads results back out of the same buffer.
* Business failures are **return codes, not exceptions**: `CA-RETURN-CODE >= 08` means "no
  business effect has occurred".

## 5. Transaction ownership rule

> A business posting is owned **either** by a stored procedure **or** by a COBOL program wrapped
> in `BEGN` … `COMT` / `ROLB` — never split across the two.

The .NET client and the COBOL engine hold **different database sessions** (ADO.NET connection vs
the single ODBC connection opened by `sterp_init`). A transaction started on one is invisible to
the other, so a unit of work must live entirely on one side (COM-014, all-or-nothing):

* **COBOL-owned**: `SQLIO "BEGN"` → validate, insert, `EXEC` procedures → `"COMT"`, or `"ROLB"`
  on any failure. The facade sees only the return code.
* **Procedure-owned**: `SET XACT_ABORT ON`, an explicit `BEGIN TRAN … COMMIT`, and
  `THROW 5xxxx, 'STERP-E-xxxx: …', 1` for business errors. The caller — COBOL or ADO.NET —
  supplies all the input in one call.
* A procedure that `THROW`s **dooms an open SQLIO transaction**. A COBOL program that needs a
  yes/no answer inside its own `BEGN … COMT` must query the table or call a procedure that
  returns a decision row rather than one that throws.
* All ledger postings funnel through `fin.JournalStage` + `fin.usp_PostJournal` with an event
  map (`db\seed\*-eventmap.sql`) deciding the accounts — that procedure owns its own transaction.
* Optimistic concurrency everywhere else: `RowVer rowversion` on mutable tables (COM-016).

## 6. Module map

Twelve families. Requirement IDs (316 of them) are tagged `REQ:` in the source and gathered into
[TRACEABILITY.md](TRACEABILITY.md) by `build\trace-gen.ps1`.

| Module | Family | Schema | Online COBOL (`cobol\online\<mod>`) | Batch (`cobol\batch\<mod>`) | Tests | .NET forms / facades |
|---|---|---|---|---|---|---|
| `common` | COM | `core` | 18: `SQLIO` `PARMGET` `NUMGEN` `PERMCHK` `PERIODCK` `IDEMPOT` `AUDITLOG` `EXCPRAIS` `JOBLOG` `RNDALLOC` `DATECALC` `TERMCALC` `FXCONV` `STRUTIL` `STFUNCS` `CMDARGS` `RUNPATH` `PING` | – | 11 | `Forms\System` (14), `Business\System` (6) |
| `fin` | FIN | `fin` | 15: `JNLENTRY` `JNLPOST` `GLVALID` `GLADJUST` `PERCLOSE` `VATDET` `VATRET9` `VATERRC` `FXREVAL` `ACCRREL` `BANKREC` `BUDGCOMP` `CTLRECON` `OPENBAL` `STATPROF` | 10: `TBRPT` `PLRPT` `BSRPT` `CASHBOOK` `BUDGRPT` `RECURJNL` `REVALRUN` `MTDJSON` `YEAREND` `FINDEMO` | 6 | `Forms\Finance` (5), `Business\Finance` (4) |
| `ar` | AR | `ar` | 16: `ARINVPST` `ARCRNOTE` `ARPOSTGL` `RCPTALOC` `CREDCHK` `CUSTVAL` `ARDISPUT` `ARADJUST` `ARDEPOS` `ARINSOLV` `ARVATWRT` `BADDEBT` `DDMAND` `DISHONOR` `LATEINT` `ARENQ` | 6: `AGEDDEBT` `STATEMNT` `DUNNING` `DDCOLL` `ARRECON` `EINVUBL` | 5 | `Forms\Debtors` (3), `Business\Debtors` (1) |
| `ap` | AP | `ap` | 19: `APINVREG` `APINVLIN` `APMATCH` `APPOST` `APALLOC` `APPAYPRP` `APPAYAPR` `APPAYRUN` `APBANKVR` `APSUPVAL` `APBLOCK` `APSTMREC` `APCRDLNK` `APVATEVD` `AP6MDATE` `APLATEPY` `APHASH` `APSVC` `APXMLGET` | 8: `APAGED` `APBACS` `APREMIT` `APVAT6M` `APEINV` `APPPR` `APBULK` `APCLOSE` | 4 | `Forms\Creditors` (3), `Business\Creditors` (3) |
| `so` | SO | `so` | 20: `SOQUOTE` `SOACCEPT` `SOAMEND` `PRICING` `SODISC` `SOVAT` `SOMARGN` `SORESV` `SOAVAIL` `PICKGEN` `DESPATCH` `BACKORD` `RETURNS` `SOCREDIT` `SOSCHED` `SOB2B` `SOROT` `SODOCVR` `KITSOAL` `EXPEVID` | 5: `SOINVRUN` `SOACKDOC` `SOANAL` `SOBACKLG` `REBATE` | 14 | `Forms\Sales` (3), `Business\Sales` (1) |
| `po` | PO | `po` | 18: `POREQ` `POORDER` `POAMEND` `POAPPRV` `POPRICE` `POPOST` `POCLOSE` `RECEIPT` `INSPECT` `SUPPRET` `SOURCSEL` `TOLCHK` `BUDGCHK` `PAYTERMC` `LANDCOST` `IMPVAT` `POSUPCHK` `SVCACC` | 6: `REQGEN` `PODOC` `POPIPE` `OVERDUE` `SUPPPERF` `PODEMO` | 7 | `Forms\Purchasing` (3), `Business\Purchasing` (4) |
| `itm` | ITM | `inv` | 13: `ITEMVAL` `ITEMENQ` `ITEMCOPY` `ITMAUDIT` `ITMCOMPL` `ITMDATE` `ITMIMPCT` `STKMOVE` `STKRESV` `AVAILQ` `LOTSER` `SUBSTCHK` `UOMCONV` | 6: `STKVALF` `STKSTAT` `OPENSTK` `COUNTADJ` `LOTTRACE` `PKGRPT` | 7 | `Forms\Items` (4), `Business\Items` (2) |
| `cst` | CST | `cst` | 13: `STDCOST` `COSTROLL` `COSTNORM` `JOBCOST` `WIPCALC` `VARIANCE` `MARGIN` `LABOVH` `LANDED` `FIFOVAL` `MWAVAL` `NRVTEST` `WHATIF` | 5: `CSTINVAL` `CSTJOBPR` `CSTVARRP` `CSTWIPRF` `CSTSETCM` | 4 | `Forms\Costing` (4), `Business\Costing` (1) |
| `bom` | BOM | `bom` | 17: `BOMEXPL` `BOMNET` `BOMCOST` `BOMVALID` `BOMCOMP` `BOMBREV` `BOMBULK` `BOMJOB` `BOMLEAF` `BOMMATH` `BOMREAD` `BOMSELCT` `BOMSUPPL` `BOMWORK` `WHEREUSD` `CYCLECHK` `ECOIMPCT` | 2: `BOMRP010` `BOMRP030` | 4 | `Forms\Bom` (3), `Business\Bom` (1) |
| `kit` | KIT | `kit` | 16: `KITCREAT` `KITAMEND` `KITTRIAL` `MULTITRL` `KITRESV` `KITISSUE` `KITCOMPL` `KITRET` `KITSCRAP` `KITCLOSE` `KITCOST` `KITQTY` `KITCOMM` `KITDB` `DISASSY` `GENEALOG` | 2: `KITRDY` `KITDOCP` | 7 | `Forms\Kits` (4), `Business\Kits` (3) |
| `xmd` | XMD, RPT | `rpt`, `stg` | 1: `CHAINTRC` | 6: `CHAINRPT` `RECONCTL` `EXCPAGE` `KPIOTIF` `OBLIGREG` `CONVLOAD` | 2 | report definitions (`core.ReportDef`) |
| `tui` | COM-018/020 | – | – | `cobol\tui\STERPOPS.cob` → `bin\x86\STERPOPS.exe` | – | – |

Schemas not owned by a single module: `core` (companies, sites, users, roles, permissions, SoD,
calendars, numbering, currencies, FX, parameters, audit, exceptions, approvals, idempotency,
sanctions, menus, jobs, reports, retention) and `stg` (import staging).

## 7. Dialect and era map

The dialect of each module is the era in which it was *notionally* first written
(`cobol\DIALECTS.cfg` drives `cobc -std=`; online, batch and test programs of a module share it):

| Module | Era / story | `-std` | Format | Visible texture |
|---|---|---|---|---|
| `ar`, `ap` | 1991 port from MVS/CICS | `ibm` | fixed | `SECTION`s, `PERFORM THRU … EXIT`, `GO TO … DEPENDING ON`, 88-levels, LOW-VALUES initialisation |
| `xmd` | 1993 batch suite | `mvs` | fixed | `SORT`, Report Writer, numbered paragraphs |
| `fin` | 1994 Micro Focus COBOL/2 on OS/2 | `mf` | fixed | UPPER CASE, numbered paragraphs, `PERFORM THRU`, some `GO TO` |
| `itm` | 1996 stock control | `rm` | fixed | RM/COBOL `SECTION`s, Report Writer columns |
| `cst` | 1997 costing | `rm` | fixed | same house style as `itm` |
| `so`, `po` | 1998 ACUCOBOL-GT distribution suite | `acu` | fixed | numbered paragraphs, mixed old/new style |
| `tui` | 2008 operator console | default | fixed | `SCREEN SECTION`, pdcurses |
| `common` | 2012 shared-services rewrite | default | fixed | structured, `EVALUATE`, inline `PERFORM` |
| `bom` | 2014 engineering rewrite | default | **free** | `>>SOURCE FORMAT IS FREE`, mixed case, `RECURSIVE`, `LOCAL-STORAGE`, `FUNCTION-ID` |
| `kit` | 2016 kitting rewrite | default | **free** | as `bom` |

All dialects are compiled by the same GnuCOBOL 3.2 with `-fstatic-call`, so every `CALL` is a
literal and the whole engine links into one DLL. Copybooks must compile in **both** source
formats (columns 8–72, `*>` comments only).

## 8. Build and deployment layout

```
build\env.ps1           dot-sourced by every script: GnuCOBOL env (..\GC32-BDB-SP1 or
                        STERP_GNUCOBOL), STERP_ODBC, bin/obj/run paths, Invoke-WithBuildLock,
                        Invoke-Sql helpers
build\build-db.ps1      00-create-database -> 01-schemas -> tables -> functions -> views ->
                        procs -> triggers -> seed -> SQL Agent jobs (every script re-runnable)
build\build-cobol.ps1   cobc -c per module  -> obj\cobol\lib\libst_<mod>.a
                        + c\sterpbrg.c, c\sterpsql.c -> bin\x86\STERPCOB.dll
                        batch -> bin\x86\<NAME>.exe, tests -> bin\tests\T<NAME>.exe
build\build-net.ps1     dotnet msbuild dotnet\STERP.sln /p:Platform=x86 -> bin\x86
build\build-all.ps1     db + cobol + net [+ tests] + LOC report
build\run-tests.ps1     COBOL TAP drivers, SQL tests, .NET harness
build\loc-report.ps1    docs\LOC-REPORT.md        build\trace-gen.ps1   docs\TRACEABILITY.md
build\techdebt-gen.ps1  docs\TECH-DEBT-REGISTER.md
```

Deployment is a **file copy**: `bin\x86` holds `STERP.exe`, the five .NET assemblies,
`STERPCOB.dll`, the GnuCOBOL runtime DLLs (`libcob-4`, `libgcc_s_dw2-1`, `libdb-18.1`, `libgmp-10`,
`libintl-8`, `libiconv-2`, `pdcurses`), `STERPOPS.exe` and ~56 batch EXEs. The client needs the
**32-bit** ODBC Driver 18 and a connection string in `STERP_ODBC`; the database is deployed by
`build-db.ps1`; `run\` is created on demand. Everything is x86 because GnuCOBOL 3.2 here is
32-bit MinGW — the single hardest constraint in the whole stack.

## 9. Test layers

`build\run-tests.ps1 [-Layer cobol|sql|net] [-Filter regex]`; the exit code is the number of
failures. All three layers print TAP (`ok n - …` / `not ok n - …`).

| Layer | What | Where | How it runs |
|---|---|---|---|
| COBOL unit | 71 drivers, module dialect, statically linked with the online engine; expected values are the specification's worked numbers | `cobol\tests\<mod>\T*.cob` → `bin\tests\T*.exe` | run each EXE, count `ok`/`not ok`; DB-touching tests wrap in `BEGN … ROLB` |
| SQL | procedure/trigger behaviour, constraints, seed integrity | `db\tests\<mod>-<topic>.sql` | `build-db.ps1 -NoDeploy -Tests`, each file inside `BEGIN TRAN … ROLLBACK` |
| .NET harness | home-grown harness (no NUnit/xUnit): facade + interop + acceptance scenarios | `dotnet\STERP.Tests\<Module>\*.cs` → `bin\x86\STERP.Tests.exe` | drives real services against the real database and the real DLL |
| Traceability | every one of the 316 requirement IDs tagged in implementation code | `REQ:` / `AC:` comments | `build\trace-gen.ps1 -Strict` |

There is no mocking anywhere: a test failure means the engine, the database or the contract
between them is wrong.

## 10. Modernisation talking points

What a modernisation team is actually up against here — every item below is real, works today and
is catalogued with a hint in [TECH-DEBT-REGISTER.md](TECH-DEBT-REGISTER.md) (~300 items).

1. **COMP-3 copybooks are the interface.** 183 copybooks define packed-decimal, fixed-length
   records that are simultaneously the COBOL record layout, the .NET interop contract (parsed at
   run time by `CopybookParser`) and the implicit shape of the database row. There is no schema
   registry, no version number and no compile-time check: rename a field and the failure appears
   at run time, in production, as a missing key. Any rewrite has to re-derive the data contract
   from the copybooks first.
2. **Eight-character program names.** `PROGRAM-ID` is 1–8 characters, unique across the whole
   system, so names such as `APVAT6M`, `RCPTALOC`, `MULTITRL` or `CHAINTRC` carry no package or
   domain structure. The namespace is flat and exhausted: `so` already has `SOROT`, `SODOCVR`,
   `KITSOAL`. Mapping 294 programs onto meaningful services is a domain-discovery exercise, not
   a rename.
3. **WORKING-STORAGE state inside a shared DLL.** The online engine is one DLL with one GnuCOBOL
   runtime, one ODBC connection and `WORKING-STORAGE` that survives between `LINK`s — several
   programs deliberately cache the last key read, reuse a COMMAREA across documents, or expect
   their caller to have primed a table. That makes the engine **stateful and single-threaded per
   process**: it cannot simply be hosted behind a web API or scaled out, and two concurrent users
   need two processes. Cloud-hosting this engine means first proving which programs are re-entrant.
4. **The same rule implemented twice.** Pricing precedence lives in `PRICING.cob` *and*
   `so.usp_PriceEnquiry`; parameter fallback in `PARMGET.cob` *and* `core.usp_GetParameter`;
   ageing, discounts and VAT appear in both COBOL and views. The duplication is intentional (the
   screen must explain what the engine charged) and it is the classic ERP trap: a change has to
   be made twice, and nothing fails when it is made once.
5. **Two transaction worlds that must not meet.** ADO.NET and SQLIO are different sessions, so the
   "unit of work" is a *convention* enforced by code review, not by the platform. Any modernisation
   that moves one posting step across the boundary silently breaks all-or-nothing.
6. **32-bit everything.** GnuCOBOL 3.2 is 32-bit MinGW, so the DLL, the bridge, the ODBC driver and
   therefore the whole .NET client are x86. The client cannot be made 64-bit without either
   rebuilding the COBOL estate on a 64-bit compiler or putting the engine behind an out-of-process
   boundary — and the .NET Framework 4.8 WinForms client cannot move to .NET 8 while it P/Invokes
   that DLL in-process.
7. **Report Writer and 132 columns.** The printed estate is COBOL Report Writer with
   `CONTROL HEADING`/`FOOTING`, hard-coded column positions and page breaks, writing `.lst` files
   to `run\spool`. There is no reporting metadata to lift: the layout *is* the program. Replacing
   it means re-specifying every report from its `01 ... TYPE IS DETAIL` paragraphs.
8. **Five dialects, one estate.** `ibm`, `mvs`, `mf`, `rm`, `acu` and GnuCOBOL default coexist, two
   of them in free format. Automated conversion tooling must handle all six flavours, including
   `GO TO … DEPENDING ON`, `PERFORM THRU` ranges with shared exits, `ALTER` remnants and dialect
   differences in default initialisation (LOW-VALUES vs spaces) that change behaviour silently.
9. **JCL by another name.** `jobs\*.cmd` reproduce job cards, `STEPnnn` labels, COND codes, SYSOUT,
   MAXCC and `RESTART=`. The operational contract (restartability, idempotency per document,
   preview vs commit) is what operations actually depend on; a scheduler migration has to preserve
   COND-code semantics, not just "run the job".
10. **Hand-rolled infrastructure.** SQLIO (eight cursor slots, positional `?` parameters, text
    columns converted with `FUNCTION NUMVAL`), the C bridge, the copybook parser, the TAP test
    harness and the green-screen console are all bespoke. There is no framework to upgrade — each
    is a small, well-understood component to be replaced by a platform service (connection pooling,
    ORM or micro-ORM, contract-first serialisation, a test framework, a web UI).
11. **Fixed `OCCURS` limits everywhere.** 40 invoice lines, 20 allocations, 200-entry explosion
    pools: the 32 KB CICS COMMAREA era is baked into the interfaces. Modern volumes need paging or
    streaming contracts, which changes every caller.
12. **Business keys, dates and money conventions.** Company/site-scoped `varchar` codes,
    `YYYYMMDD` dates crossing the bridge as display numerics, FX rates held as *foreign units per
    £1* (D-14), six-decimal quantities, snapshotted document values and "never update a posted
    document — reverse and replace". These are domain rules a rewrite must keep; they are not
    accidents of the technology.

Suggested demonstration order: run `build\build-all.ps1 -Full -Test` to show the whole estate
building and testing green, open the register ([TECH-DEBT-REGISTER.md](TECH-DEBT-REGISTER.md)) and
the traceability matrix ([TRACEABILITY.md](TRACEABILITY.md)), then pick one thread — for example
sales pricing — and follow it from `frmSalesOrder` through `SalesOrderService`, `PRICING.cob`,
`SQLIO` and `so.usp_PriceEnquiry` to see every problem above in one place.

## 11. Where to read next

* [CONVENTIONS.md](CONVENTIONS.md) — binding engineering rules (COBOL, database, .NET, DoD)
* [COMMON-SERVICES.md](COMMON-SERVICES.md) — the 18 shared COBOL services and their COMMAREAs
* [DATA-MODEL.md](DATA-MODEL.md) — 179 tables, procedures, triggers, seeded identities
* [NET-FRAMEWORK.md](NET-FRAMEWORK.md) — client framework, `CobolGateway`/`CommArea`, forms, tests
* [OPERATOR-GUIDE.md](OPERATOR-GUIDE.md) and [../jobs/README.md](../jobs/README.md) — running the batch estate
* [modules/](modules/) — one document per module; [tech-debt/](tech-debt/) — the raw debt notes
* [LOC-REPORT.md](LOC-REPORT.md), [TRACEABILITY.md](TRACEABILITY.md), [TECH-DEBT-REGISTER.md](TECH-DEBT-REGISTER.md) — generated
