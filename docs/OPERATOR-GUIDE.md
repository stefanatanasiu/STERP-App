# STERP - Stefan's ERP: operator guide

For the operations shift: running the console, submitting the job streams, where the output
lands and what to do when a stream stops.

REQ: XMD-005, XMD-007, COM-017, COM-018, RPT-015

---

## 1. The operator console — `STERPOPS`

```
set STERP_HOME=<repo root>
bin\x86\STERPOPS.exe                 interactive 24x80 console
bin\x86\STERPOPS.exe SMOKE           prints every panel's data, RC=0 (used by the build)
bin\x86\STERPOPS.exe SMOKE USER=OPS CO=STC
```

Start it from the STERP root, or set `STERP_HOME` first; the console launches batch work with
paths relative to that directory. The ODBC connection comes from `STERP_ODBC`
(`build\env.ps1`, or `jobs\SETENV.cmd` for a plain command prompt).

**Sign on.** User code is validated against `core.AppUser` (active users only); the company
defaults to the user's default company. Seeded operators: `OPS` (Stellar), `OPSCAL` (Caledon),
`OPSVIEW` (read only — may look but not acknowledge). Three refusals lock the console (RC 8).

**Panels.**

| Option | Panel | Source |
|---|---|---|
| 1 | Submit batch job / job stream | `core.ReportDef` (`JOB.*` rows are the `jobs\*.cmd` streams) |
| 2 | Job monitor, newest first | `core.JobRun` |
| 3 | Exception queue, acknowledge | `core.ExceptionItem` / `core.usp_ResolveException` |
| 4 | Period status per company | `fin.Period` |
| 5 | Engine health | `CALL "PING"` + SQLIO connection |

**Keys.** `F3` back/exit, `F7` page back, `F8` page forward, `ENTER` refresh, a line number
selects a row. Function keys only reach the program when the run time is told to pass them:

```
set COB_SCREEN_EXCEPTIONS=Y
```

`jobs\OPSCON.cmd` does exactly that: it sets `COB_SCREEN_EXCEPTIONS=Y` and `COB_SCREEN_ESC=Y`,
puts `bin\x86` on the PATH and starts the console from the application root, so the function keys
work without any set-up by the operator. `jobs\OPSCON.cmd SMOKE` runs the console's self test
(no screen, return code 0) and is what the harness test *ops console: SMOKE runs clean* uses.

Without it, type the equivalent commands in `OPTION ===>`: `X` (F3), `-` or `P` (F7), `+` or `N`
(F8), a line number to select.

**Submitting.** Option 1 lists the catalogue. Type the line number, amend the `KEY=VALUE` panel
(`CO`, `USER`, `ASAT` and `MODE=PREVIEW` are pre-filled) and press ENTER. `JOB.*` rows run
`jobs\<name>.cmd`; everything else runs `bin\x86\<program>.exe` with the console's log in
`run\logs\OPS-<program>-<stamp>.log`. The console then shows the completion code and the spool
file recorded in `core.JobRun`.

---

## 2. Job streams

See `jobs\README.md` for the step list of each stream. Submission from a command prompt:

```
jobs\NIGHTLY.cmd  CO=STC USER=OPS MODE=PREVIEW
jobs\MONTHEND.cmd CO=STC YEAR=2026 PERIOD=7 MODE=COMMIT
jobs\PAYRUN.cmd   CO=STC RUN=PP-26-0007 USER=APMGR
```

Parameters are unquoted `KEY=VALUE` pairs. `MODE=PREVIEW` (the default) changes nothing — always
run a preview first and keep its SYSOUT with the night's paperwork.

**The nightly cycle.** `NIGHTLY` every working night. `STMTS` and `DDCOLL` on their calendar
days. `MONTHEND` after the last nightly of the period and before the period is soft closed.
`VATRET` after the quarter's `MONTHEND`. `YEAREND` only when every period of the year is closed
and the trial balance is signed off.

**Operators hold `VATOWNER`.** `VATRET` STEP020 (`MTDJSON`) checks `FIN.VAT.PREPARE` on the
submitting user, so `db\seed\96-xmd-jobs.sql` grants `OPS`, `OPSCAL` and `OPSVIEW` the `VATOWNER`
role alongside `BATCH`/`OPERATOR`/`GLCLERK` - otherwise the night shift's VAT stream ends COND 8.
That trades segregation of duties for an unattended stream: where SoD matters more, remove the role
there and have a VAT preparer submit `VATRET` instead.

---

## 3. Output and spool folders

| Folder | Contents | Keep |
|---|---|---|
| `run\logs` | SYSOUT of every stream and console submission | 90 days |
| `run\spool` | 132-column reports, `<REPORT>-<CO>-<stamp>.lst` | 90 days (statutory reports: 6 years) |
| `run\bacs` | Bacs Standard 18 payment and direct debit files | 6 years |
| `run\mtd` | Making Tax Digital VAT payloads | 6 years |
| `run\einvoice`, `run\extracts`, `run\work` | e-invoices, extracts, scratch | 90 days / scratch |

Nothing is deleted automatically. `core.JobRun` holds the log and spool path of every run, so
the job monitor (console option 2, or the client) is the index into these folders.

---

## 4. COND codes and restart rules

| COND | Meaning | Action |
|---|---|---|
| 0 | clean | file the SYSOUT |
| 4 | warning, or a step bypassed because its program is not installed | read the SYSOUT; a reconciliation warning goes to the exception queue for the morning |
| 8 | step failed, stream stopped | correct the cause, then restart |
| 12 / 16 | severe / abend | call the duty officer before restarting anything |

**Restart.** Add `RESTART=STEPnnn`:

```
jobs\MONTHEND.cmd CO=STC YEAR=2026 PERIOD=7 RESTART=STEP040
```

Rules:

1. Restart at the **failing step**, never before a step that has already posted.
2. Report steps (`TBRPT`, `PLRPT`, `BSRPT`, `AGEDDEBT`, `APAGED`, `STATEMNT`, `ARRECON`,
   `CSTINVAL`, `APPPR`) change nothing and may be repeated freely.
3. Posting steps are idempotent per document (`RECURJNL`, `REVALRUN`, `APVAT6M`, `DUNNING`,
   `YEAREND`, `APBACS`): a repeated run recognises what it already did and does not post twice.
   `PAYRUN` additionally refuses a proposal that is not approved and skips one already paid.
4. A stream that stopped leaves its `core.JobRun` row `FAILED`; the restarted run creates a new
   row. A row still showing `RUNNING` long after the shift means the machine died mid-step —
   check the SYSOUT before restarting.
5. Never edit a spool or Bacs file. Re-run the step; the new file carries a new timestamp.

---

## 5. Daily checks

1. Console option 5 — engine and database healthy.
2. Console option 2 — last night's streams ended `OK` or `WARNING`, nothing left `RUNNING`.
3. Console option 3 — work the exception queue; acknowledge what the shift has picked up so the
   ageing in `NIGHTLY` STEP010 can escalate the rest.
4. Console option 4 — the period the business is posting into is `OPEN`.
