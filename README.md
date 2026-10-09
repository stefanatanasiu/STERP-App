# STERP - Stefan's ERP

**UK supply-chain ERP (legacy edition)**

STERP - Stefan's ERP is a complete, working **UK supply-chain ERP** — general ledger and VAT, debtors, creditors,
sales orders, purchasing, stock, costing, kits and bills of materials, plus cross-module controls
and a printed reporting estate — built on purpose in a **legacy technology mix** of 1990s COBOL,
a C bridge, SQL Server and a .NET Framework WinForms client.

It is not a product. It is a **modernisation demonstration asset**: a realistic, tested, buildable
system that contains the problems real modernisation projects meet — packed-decimal copybooks as
interfaces, eight-character program names, state in a shared DLL, business rules implemented twice,
Report Writer output, JCL-style job streams and a 32-bit toolchain that pins the whole stack.

* **What it looks like inside:** [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md)
* **What makes it hard to modernise:** [docs/TECH-DEBT-REGISTER.md](docs/TECH-DEBT-REGISTER.md) (330 catalogued items)
* **The functional specification** it implements: the separate, private
  [STERP-Supply-Chain-Functional-Specification](https://github.com/stefanatanasiu/STERP-Supply-Chain-Functional-Specification)
  repository (316 requirements, 132 acceptance scenarios, decisions D-01..D-30). The generated
  [traceability](docs/TRACEABILITY.md) and [acceptance](docs/ACCEPTANCE.md) reports are included here;
  regenerating them needs the specification cloned next to this repository (or `STERP_SPEC` set).

> **All data is fictional.** Every company, person, address, bank account, sort code, IBAN, VAT and
> company registration number in the seed and demonstration data is invented for the demonstration;
> any resemblance to a real organisation or account is coincidental.

---

## Size and language breakdown

**Source inventory refreshed: 7 October 2026.** These figures describe the application source,
not the bundled GnuCOBOL toolchain or compiled application size.

| Language / source type | Files | Non-blank lines | Share |
|---|---:|---:|---:|
| COBOL programs (`.cob`) | 325 | 206,687 | 80.8% |
| COBOL copybooks (`.cpy`) | 183 | 14,587 | 5.7% |
| T-SQL (`.sql`) | 204 | 14,060 | 5.5% |
| VB.NET (`.vb`) | 97 | 12,106 | 4.7% |
| C# (`.cs`) | 38 | 5,895 | 2.3% |
| PowerShell / Windows command scripts (`.ps1`, `.cmd`) | 37 | 1,923 | 0.8% |
| C (`.c`) | 2 | 396 | 0.2% |
| **Total** | **886** | **255,654** | **100%** |

**COBOL accounts for 221,274 non-blank lines (86.6%).** Including blank lines, the measured
source files contain **263,717 lines**. Percentages are rounded independently.

The 325 COBOL programs comprise **166 online programs, 61 batch programs, 97 test programs
and one operator console**, supported by 183 copybooks.

**Counting scope:** comments, tests and WinForms designer source are included. Documentation,
SQL seed data, legacy data files, build outputs (`obj`, `bin`), packages and the bundled
toolchain are excluded. These are non-blank source-line counts, not comment-stripped executable
code or code-coverage measurements.

Reproduce the language counts with `build\loc-report.ps1 -NoWrite`, or run
`build\loc-report.cmd` to refresh [docs/LOC-REPORT.md](docs/LOC-REPORT.md).

## Technology mix

| Layer | Implementation |
|---|---|
| Desktop application | **VB.NET Windows Forms**, MDI interface, **.NET Framework 4.8, x86**; legacy non-SDK projects |
| Application services | VB.NET business facades; C# shared utilities, ADO.NET data access, COBOL interop and a hand-rolled TAP test harness; **six .NET projects** |
| Business engine | **GnuCOBOL 3.2, 32-bit**; five legacy dialect modes (`ibm`, `mvs`, `mf`, `rm`, `acu`) plus GnuCOBOL default; online COMMAREA programs in `STERPCOB.dll` and batch EXEs with SORT and Report Writer |
| Native bridge | **C**, called through C# P/Invoke; `sterp_link` emulates CICS `LINK` using fixed-layout COMMAREA buffers; the SQLIO service uses a C ODBC shim |
| Database | **SQL Server 2025**, database `STERP`; **12 schemas and 215 distinct table definitions in source**, with stored procedures, functions, audit triggers, views, SQL Agent jobs, seed and demonstration data; ADO.NET and **ODBC Driver 18** access |
| Operations | PowerShell build/test scripts, JCL-style Windows command-file job streams, SQL Agent scheduling, COBOL Report Writer output and a `SCREEN SECTION` green-screen operator console |

The database schemas are `core fin ar ap so po inv cst bom kit rpt stg`. There are two main
data-access paths: the .NET facades can call COBOL through the native bridge, with COBOL using
SQLIO/ODBC, or use C# ADO.NET directly. Business behaviour therefore spans COBOL and SQL rather
than living only in the desktop client.

External integrations are file-based, including Bacs payment files, MTD JSON payloads and
UBL/XML e-invoices. See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for the runtime call paths
and deployment layout.

## Prerequisites

| Need | Detail |
|---|---|
| Windows | x64 Windows 10/11 or Server; paths in this tree contain spaces, so **quote them** |
| **GnuCOBOL 3.2 (32-bit)** | Arnold Trembley's MinGW build `GC32-BDB-SP1` ([download](https://www.arnoldtrembley.com/GnuCOBOL.htm)), not included in this repository; expected at `..\GC32-BDB-SP1` next to this folder; override with the `STERP_GNUCOBOL` environment variable. `build\env.ps1` sets `COB_*` and `PATH` from it — do not run `set_env.cmd` yourself |
| **SQL Server 2025** | local default instance (`.`), **Windows authentication**; your login needs `dbcreator`/sysadmin to create the `STERP` database. Override with `STERP_SQLSERVER` / `STERP_DATABASE` |
| `sqlcmd` | the ODBC-era client (`-C` trust-server-certificate is used everywhere) |
| **ODBC Driver 18 for SQL Server** | the **32-bit** component must be installed — COBOL/SQLIO is a 32-bit process |
| **.NET Framework 4.8** | runtime to run the client, plus the targeting pack. Builds use the NuGet reference assemblies at `%UserProfile%\.nuget\packages\microsoft.netframework.referenceassemblies.net48\1.0.3` (`dotnet\STERP.Legacy.props` points `FrameworkPathOverride` at it); restore it once with `dotnet add`/`nuget install microsoft.netframework.referenceassemblies.net48 -Version 1.0.3` if the cache is empty |
| **.NET SDK** | only for `dotnet msbuild`, which builds the legacy `.vbproj`/`.csproj` files |

## Quick start

```powershell
# everything: create/upgrade the database, build COBOL + .NET, run all tests, print the LOC report
build\build-all.cmd -Full -Test

# then bring the demonstration dataset to life (re-runnable; run it again after the tests,
# which leave their own fixtures behind)
build\demo-data.cmd                 # FINDEMO, PODEMO, APDEMO, KITDEMO, SODEMO, control levelling, RECONCTL

# or incrementally
build\build-all.cmd                 # deploy DB changes, build COBOL and .NET (no drop, no tests)
build\build-all.cmd -Test           # same, then run every test layer
```

`build-all` chains the four scripts; each can be run on its own (every `.ps1` has a `.cmd` wrapper
and takes the same arguments):

```powershell
build\build-db.cmd      [-Full] [-Only <regex>] [-Tests] [-NoDeploy] [-NoSeed]
build\build-cobol.cmd   [-Module ar,fin] [-NoDll] [-NoBatch] [-NoTests] [-Clean]
build\build-net.cmd     [-Debug] [-Rebuild]
build\run-tests.cmd     [-Layer cobol|sql|net] [-Filter <regex>]
```

`-Full` means **drop and recreate** the `STERP` database, clean `obj\cobol` and rebuild the .NET
solution; without it every script is incremental and every SQL script is re-runnable.

Then run:

```powershell
bin\x86\STERP.exe        # WinForms MDI desktop client
bin\x86\STERPOPS.exe     # green-screen operator console (job monitor, submit, spool browser)
jobs\NIGHTLY.cmd CO=STC USER=OPS MODE=PREVIEW      # a JCL-style job stream
```

Start `STERPOPS.exe` from the repository root (or set `STERP_HOME` first) — it launches batch work
with paths relative to that directory, and `STERPOPS.exe SMOKE` prints every panel non-interactively.
Its operators are separate from the client's users: `OPS` (Stellar), `OPSCAL` (Caledon) and
`OPSVIEW` (read only).

### Signing on (demo users)

Sign-on is **Windows authentication** — there are no passwords anywhere in STERP. The seed
(`db\seed\03-core-roles-users.sql`) maps the Windows account that deployed the database to the
`SYSADM` user, so the sign-on dialog shows your Windows login and resolves it to `SYSADM`.

Signed on as `SYSADM` you can switch identity with the **Continue as** box on the sign-on dialog (and
from *File → Continue as User* / *File → Select Company / Site*), which is how the permission, segregation-of-duties and approval rules are
demonstrated. The seeded users (user code = role code, company `STC`, site `RDG` unless shown):

| User | Who they are | User | Who they are |
|---|---|---|---|
| `SYSADM` | System Administrator | `BUYER` | Nadia Farouk, buyer (site `BHM`) |
| `DIRECTOR` | Helen Marchetti, managing director | `PURMGR` | Colin Ashworth, purchasing manager (`BHM`) |
| `FINCTRL` | Alan Prydderch, financial controller | `SALESREP` | Jay Ellington, sales representative |
| `GLCLERK` | Priya Nandakumar, GL clerk | `SALESMGR` | sales manager |
| `ARCLERK` | Dawn Okafor, sales ledger clerk | `WHSE` | warehouse operator |
| `CREDMGR` | Martin Boyce, credit manager | `INVMGR` | inventory manager |
| `APCLERK` | Tomasz Zielinski, purchase ledger clerk | `QUALITY` | quality manager |
| `APMGR` | Sian Rutherford, AP manager | `ENGINEER` / `ENGMGR` | design engineer / engineering manager |
| `TREASURY` | Gavin Lochhead, payment authoriser | `COSTACC` | cost accountant |
| `AUDITOR` | auditor (read only) | `VATOWNER` | VAT and tax owner |
| `RESTRICT` | restricted single-site user | | |

Demonstration data is seeded for two companies — **STC** Stellar Components Ltd (sites `RDG`
Reading, `BHM` Birmingham) and **CAL** Caledon Assemblies Ltd (`GLA` Glasgow, `LIV` Livingston) —
on an April–March fiscal year.

## Tests

The dedicated test directories contain **154 source files and 55,973 non-blank lines**
(21.9% of the counted source in the inventory above), spanning COBOL drivers, SQL tests and
the .NET harness.

```powershell
build\run-tests.cmd                      # all layers; exit code = number of failures
build\run-tests.cmd -Layer cobol         # COBOL unit drivers (bin\tests\T*.exe, TAP output)
build\run-tests.cmd -Layer sql           # db\tests\*.sql, each inside BEGIN TRAN ... ROLLBACK
build\run-tests.cmd -Layer net           # bin\x86\STERP.Tests.exe harness (facades + interop)
build\run-tests.cmd -Layer cobol -Filter TSO     # only tests whose name matches
build\build-db.cmd -NoDeploy -Tests -Only ar     # SQL tests for one module
```

Supporting reports (all regenerate a file under `docs\`):

```powershell
build\loc-report.cmd  [-Min 70]     # docs\LOC-REPORT.md        language mix / COBOL share
build\trace-gen.cmd   [-Strict]     # docs\TRACEABILITY.md      316 requirements -> code -> tests
build\techdebt-gen.cmd [-Strict]    # docs\TECH-DEBT-REGISTER.md aggregated from docs\tech-debt\*.md
build\ac-coverage.cmd [-Strict]     # docs\ACCEPTANCE.md        132 acceptance scenarios -> tests
build\ui-smoke.ps1                  # opens every menu screen via STERP.exe /qa-screens, PNGs + defect log
```

Previously recorded clean verification (`build-all -Full -Test`, then `demo-data`): COBOL
3,099 assertions, SQL 389, .NET harness 165 (including 40 end-to-end acceptance tests), all
passing; RECONCTL agreed across all eight control families. These are historical results,
not a new test run: the 7 October 2026 source-inventory refresh did not rerun the application tests.

The generated [traceability report](docs/TRACEABILITY.md) and
[acceptance report](docs/ACCEPTANCE.md), dated 7 October 2026, record **316/316 requirements**
with implementation and test tags and **132/132 acceptance scenarios** mapped to automated
tests. This is tag-based traceability, not measured code coverage or evidence of a fresh passing run.

Nothing is mocked: the COBOL tests call the real engine, the SQL tests run against the real
database and the .NET harness drives the real services through the real DLL.

## Directory layout

```
bin\x86\         STERP.exe, STERPOPS.exe, STERPCOB.dll, 61 batch EXEs, GnuCOBOL runtime DLLs
bin\tests\       COBOL unit test drivers (T*.exe)
build\           env.ps1 (dot-sourced by all) + build-db / build-cobol / build-net / build-all
                 run-tests, loc-report, trace-gen, techdebt-gen  (each with a .cmd wrapper)
c\               sterpbrg.c   COMMAREA bridge (sterp_init / sterp_link, cdecl exports)
                 sterpsql.c   SQLIO ODBC shim (8 cursors, MARS, parameterised statements)
cobol\           online\<mod>\  LINKable COMMAREA programs  -> STERPCOB.dll
                 batch\<mod>\   batch and report programs    -> bin\x86\<NAME>.exe
                 tests\<mod>\   TAP unit test drivers        -> bin\tests\T<NAME>.exe
                 tui\           STERPOPS.cob operator console (SCREEN SECTION)
                 copy\          183 copybooks (records, COMMAREAs, test kit)
                 DIALECTS.cfg   module -> cobc -std (the era map)
data\            legacy fixed-format files used by the conversion/load demonstrations
db\              00-create-database.sql, 01-schemas.sql, tables\, functions\, views\, procs\,
                 triggers\, seed\, jobs\ (SQL Agent), tests\
docs\            architecture, conventions, data model, module and tech-debt notes (see below)
dotnet\          STERP.Common / STERP.Data / STERP.CobolInterop (C#),
                 STERP.Business / STERP.Desktop (VB.NET), STERP.Tests (C# harness), STERP.sln
jobs\            JCL-style streams: NIGHTLY MONTHEND PAYRUN DDCOLL VATRET STMTS YEAREND
                 + SETENV / RUNSTEP / JOBEND procedures
obj\             build intermediates (COBOL .o and module archives)
run\             spool\ bacs\ mtd\ einvoice\ extracts\ logs\ work\  — batch output tree
```

## Documentation

| Document | What is in it |
|---|---|
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | system context, component diagram, runtime call paths, COMMAREA contract, transaction ownership, module map, dialect/era map, build and test layers, **modernisation talking points** |
| [docs/CONVENTIONS.md](docs/CONVENTIONS.md) | binding engineering rules: COBOL, database, .NET, line budgets, definition of done |
| [docs/COMMON-SERVICES.md](docs/COMMON-SERVICES.md) | the 18 shared COBOL services (rounding, dates, terms, FX, parameters, numbering, permissions, periods, idempotency, audit, exceptions, batch plumbing) |
| [docs/DATA-MODEL.md](docs/DATA-MODEL.md) | every schema and table, the procedure catalogue, audit triggers, seeded identities |
| [docs/NET-FRAMEWORK.md](docs/NET-FRAMEWORK.md) | client framework: `SterpSession`, `SqlHelper`, `CobolGateway`/`CommArea`, forms, menus, reports, tests |
| [docs/OPERATOR-GUIDE.md](docs/OPERATOR-GUIDE.md) · [jobs/README.md](jobs/README.md) | running the batch estate: streams, parameters, COND codes, restart rules, output folders |
| [docs/MODULE-BRIEF.md](docs/MODULE-BRIEF.md) | the brief each module team works to |
| [docs/modules/](docs/modules/) | one document per module: `ap` `ar` `bom` `cst` `fin` `itm` `kit` `po` `so` `xmd` — programs, procedures, forms, flows and how to demonstrate them |
| [docs/tech-debt/](docs/tech-debt/) | the raw per-module debt notes that feed the register |
| [docs/TECH-DEBT-REGISTER.md](docs/TECH-DEBT-REGISTER.md) | generated: every debt item with category, why it is debt and a modernisation hint |
| [docs/TRACEABILITY.md](docs/TRACEABILITY.md) | generated: requirement → implementing source → tests |
| [docs/ACCEPTANCE.md](docs/ACCEPTANCE.md) | generated: acceptance scenario AC-001..AC-132 → automated tests |
| [docs/DEMO-DATA.md](docs/DEMO-DATA.md) | the demonstration dataset, the demo drivers and how to rebuild it |
| [docs/DEMO-WORKFLOWS.md](docs/DEMO-WORKFLOWS.md) | which company/site to sign on to for which story, with step-by-step demo scripts |
| [docs/UI-REVIEW.md](docs/UI-REVIEW.md) | screen-by-screen review of the classic WinForms client, with screenshots in `docs/img/` |
| [docs/LOC-REPORT.md](docs/LOC-REPORT.md) | generated: language mix and COBOL share |

## Licence

STERP is released under the [MIT Licence](LICENSE). GnuCOBOL and its runtime libraries are not part
of this repository and are covered by their own licences (GPL/LGPL for GnuCOBOL, and the Oracle
Berkeley DB licence for `libdb`); check them before redistributing built binaries.
