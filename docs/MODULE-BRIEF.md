# STERP - Stefan's ERP: module team brief (read this first, then CONVENTIONS.md)

You are implementing ONE functional module of STERP, a deliberately legacy-styled UK supply-chain
ERP used for modernisation demos. Several module teams work **in parallel** in the same tree.

Root: the `STERP-App` repository root (paths may contain spaces – quote).
Spec: `..\STERP-Supply-Chain-Functional-Specification\`, cloned next to `STERP-App`
(your module document, `01-...` shared rules, `11-...` cross-module, `12-...` acceptance scenarios for
your family, `13-...` decisions D-01..D-30).

## Read before coding (use view_range/grep – do not load huge files whole)
1. `docs\CONVENTIONS.md` – binding rules (sections 3 COBOL, 4 database, 5 .NET, 6 line budget, 7 DoD).
2. `docs\DATA-MODEL.md` – existing tables (179) and core procs. **Use the existing tables**; add
   columns/tables only when genuinely missing (new files in `db\tables\NN-<schema>\`, idempotent).
3. `docs\COMMON-SERVICES.md` – COBOL common services (RNDALLOC, DATECALC, TERMCALC, FXCONV, PARMGET,
   NUMGEN, AUDITLOG, EXCPRAIS, PERMCHK, PERIODCK, IDEMPOT, JOBLOG, STRUTIL, CMDARGS, RUNPATH,
   STFUNCS, copybooks STRPTHDR/STRPTWS/STBATLOG/STMONEY/STDATES). Reuse them.
4. `docs\NET-FRAMEWORK.md` – how to add services, forms, menu rows, generic maintenance definitions,
   harness tests; CobolGateway/CommArea API.
5. Example code: `cobol\online\common\*.cob`, `cobol\tests\common\TDBSVC.cob`, `db\procs\fin\usp_PostJournal.sql`.

## Avoid HTTP 408 failures (important – earlier agents crashed)
**Session budget:** stop after roughly **70 tool calls** (or sooner if a natural checkpoint is reached),
make sure what you have builds, record the remaining work as sub-todos in the SQL `todos` table
(ids prefixed with your module code, e.g. `fin-vat-return`, status `pending`, self-contained
description), and return your summary. A fresh agent will continue from the sub-todos. Long sessions
crash with HTTP 408 and lose the final report.

Keep your context small: view files with `view_range`/`grep`, pipe command output through
`Select-Object -First/-Last N` and show only errors, keep each create/edit under ~25 KB (split large
programs into several programs/copybooks – realistic anyway), never paste large outputs back.

## What to build
* **COBOL is the business engine (≥ 5,000 lines for your module, see CONVENTIONS §6).** Put
  validation, calculations, posting logic, batch runs, reports (Report Writer, 132 columns, via
  `STRPTHDR`), file outputs and unit tests in COBOL in your module's dialect (`cobol\DIALECTS.cfg`).
  LINKable online programs (DFHCOMMAREA + `<PROG>CA.cpy`) for everything the client calls.
* **Stored procedures for some business logic** (well-known pattern): e.g. enquiries, aggregations,
  reservations/concurrency-sensitive updates, postings that are pure set operations. Keep non-seed
  T-SQL ≤ ~450 lines. Postings go through `fin.JournalStage` + `fin.usp_PostJournal` (add any missing
  business events to a NEW seed file `db\seed\NN-<mod>-eventmap.sql` rather than editing `10-fin-eventmap.sql`).
* **.NET ≤ ~800 lines**: VB.NET facades in `dotnet\STERP.Business\<Module>\`, a few key forms in
  `dotnet\STERP.Desktop\Forms\<Module>\` inheriting the framework base forms, generic maintenance
  definitions for simple masters, menu rows and `core.ReportDef` rows in your seed
  (`db\seed\NN-<mod>-menu.sql`), harness tests in `dotnet\STERP.Tests\<Module>\`.
* **Demo transactions** for your module in `db\seed\9N-<mod>-demo.sql` (or produced by running your
  COBOL batch programs from a seed script) so screens and reports show meaningful data.
* Tag every requirement of your family with `REQ:` in implementation code (all IDs must appear);
  tag tests with `REQ:`/`AC:`. Use the spec's worked numbers as expected test values.
* Docs: `docs\modules\<mod>.md` (programs, procs, forms, flows, how to demo) and
  `docs\tech-debt\<mod>.md` (intentional legacy texture: file, pattern, why, modernisation hint).

## Build / test commands (all with `powershell -NoProfile -ExecutionPolicy Bypass -File ...`)
* `build\build-db.ps1 -Only "<regex for your files>"` – deploy your scripts (**never `-Full`**).
  Run it twice to prove re-runnability. `build\build-db.ps1 -NoDeploy -Tests -Only <mod>` for SQL tests.
* `build\build-cobol.ps1 -Module <mod>` (add `-NoDll` if STERPCOB.dll is locked or another module is
  mid-edit; your test EXEs link statically). Build errors from OTHER modules' sources are not yours –
  ignore them, retry later.
* `build\build-net.ps1` (if another team's file breaks the build, wait and retry; don't edit theirs).
* `build\run-tests.ps1 -Layer cobol -Filter <prefix>` / `-Layer net -Filter <mod>`.
* Do not start the desktop client and leave it running (it locks STERPCOB.dll). If you launch it for a
  screenshot, stop it by PID afterwards.

## Ownership
Only create/edit files in your module's folders and files prefixed for your module (see CONVENTIONS
§1). Shared/foundation files: smallest backwards-compatible change only, and report it.

## Final reply (≤ 50 lines)
Programs/procs/forms delivered, requirement coverage (all IDs tagged?), test results (COBOL/SQL/.NET),
line counts per language for your module, anything other teams must know.
