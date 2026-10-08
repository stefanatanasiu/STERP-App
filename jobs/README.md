# STERP - Stefan's ERP: job streams (`jobs\*.cmd`)

JCL in Windows clothing. Every stream is a `.cmd` file written the way the operations team
wrote JCL on the old box: a `//JOBNAME JOB` banner, numbered `STEPnnn` labels, a COND code
check after every step, SYSOUT to `run\logs\<job>-<yyyymmddhhmmss>.log` and a `//JOB ENDED`
card with the maximum COND code.

REQ: XMD-005, XMD-007, COM-017, COM-018, RPT-015, RPT-016

## Procedures (shared, called by every stream)

| File | JCL equivalent | What it does |
|---|---|---|
| `SETENV.cmd` | `// JOBLIB` + `//SYSIN DD *` | run-time environment (`STERP_HOME`, `STERP_RUN`, `STERP_ODBC`, `PATH`), parses the `KEY=VALUE` parameters, opens SYSOUT, writes the job card, registers the stream in `core.JobRun` via `core.usp_JobRunStart` |
| `RUNSTEP.cmd` | `//STEPnnn EXEC PGM=` | runs one batch EXE (or a T-SQL step with `@SQL`), appends its output to SYSOUT, sets `LASTCC` and `MAXCC` |
| `JOBEND.cmd` | end-of-job | writes the end card, closes `core.JobRun` (`OK` / `WARNING` / `FAILED` / `ABENDED`) and returns MAXCC to the submitter |

## Streams

| Stream | Steps | Typical submission |
|---|---|---|
| `NIGHTLY.cmd` | 010 exception ageing (`core.usp_AgeExceptions`), 020 `RECURJNL`, 030 `AGEDDEBT`, 040 `APAGED`, 050 `APVAT6M`, 060 FX staleness | `jobs\NIGHTLY.cmd CO=STC USER=OPS MODE=PREVIEW` |
| `MONTHEND.cmd` | 005 period check, 010 `REVALRUN`, 020 `RECURJNL` (accrual/prepayment release through `ACCRREL`), 030 `CSTINVAL`, 040 `ARRECON`, 050 `RECONCTL`, 060 `TBRPT`, 070 `PLRPT`, 080 `BSRPT` | `jobs\MONTHEND.cmd CO=STC YEAR=2026 PERIOD=7 MODE=PREVIEW` |
| `PAYRUN.cmd` | 010 locate proposal, 020 **approval gate**, 030 `APBACS`, 040 `APREMIT` | `jobs\PAYRUN.cmd CO=STC RUN=PP-26-0007 USER=APMGR` |
| `DDCOLL.cmd` | 010 `DDCOLL`, 020 collection summary | `jobs\DDCOLL.cmd CO=STC SUN=123456 MODE=PREVIEW` |
| `VATRET.cmd` | 005 pick the VAT period, 010 `VATRET`, 020 `MTDJSON` | `jobs\VATRET.cmd CO=STC VATPER=26A3` |
| `STMTS.cmd` | 010 `STATEMNT`, 020 `DUNNING`, 030 AR exception summary | `jobs\STMTS.cmd CO=STC ASAT=20261031 STAGE=ALL` |
| `YEAREND.cmd` | 005 period status, 010 `TBRPT`, 020 `PLRPT`, 030 `BSRPT`, 040 `YEAREND`, 050 new-year check | `jobs\YEAREND.cmd CO=STC YEAR=2026 MODE=PREVIEW` |

## Parameters

Given as unquoted `KEY=VALUE` pairs in any order. Known keys: `CO`, `SITE`, `USER`, `MODE`
(`PREVIEW` default / `COMMIT`), `ASAT` (yyyymmdd), `YEAR`, `PERIOD` (1-12; `SETENV` also builds
the packed `YYYYPP` form in `P_YYYYPP`), `RUN`, `VATPER`, `SUN`, `STYLE`, `STAGE`,
`RESTART=STEPnnn`. Anything else becomes `P_<KEY>` and is available to the stream.

Defaults: `CO=STC`, `USER=OPS`, `MODE=PREVIEW`, `ASAT`/`YEAR`/`PERIOD` from the run date on an
April-to-March fiscal year.

## COND codes

`0` clean, `4` warning (a report found something, or a step was bypassed because its program is
not installed yet), `8` or more stops the stream at that step. The stream returns MAXCC, so a
scheduler or the operator console sees `ERRORLEVEL 8` for a failure and `0` otherwise.

## Restart

`RESTART=STEPnnn` resumes at that label — the same idea as the JCL `RESTART=` parameter. Report
steps change nothing and can always be repeated; posting steps (`RECURJNL`, `REVALRUN`,
`APVAT6M`, `YEAREND`, `APBACS`) are idempotent per document, so a restart cannot double-post.
Full restart rules are in `docs\OPERATOR-GUIDE.md`.

## Where the output goes

`run\logs` SYSOUT, `run\spool` 132-column reports, `run\bacs` payment and collection files,
`run\mtd` VAT payloads, `run\extracts`, `run\einvoice`. Each run of a report program also prints
`STERP-I-SPOOL <full path>` to SYSOUT, and `core.JobRun` keeps the log and spool paths for the
job monitor.
