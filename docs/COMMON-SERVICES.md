# STERP - Stefan's ERP: common services (module `common`)

Shared COBOL services that every other module calls instead of re-implementing money,
dates, numbering, authority, auditing and parameters. Sources live in
`cobol\online\common`, COMMAREA copybooks in `cobol\copy`, tests in `cobol\tests\common`.

Read this with `docs\CONVENTIONS.md` section 3 (binding rules) — this page only documents
what the services do and how to call them.

---

## 1. How to call a common service

Every service is **LINKable**: `PROCEDURE DIVISION USING DFHCOMMAREA.` with one COMMAREA
whose first 400 bytes are the standard header `STCAHDR` (program, function, company, site,
user, business date, correlation id, return code, reason code, message).

From COBOL:

```cobol
       WORKING-STORAGE SECTION.
       COPY TERMCALCCA REPLACING ==DFHCOMMAREA== BY ==WS-TERMCALC==.
      *> ...
           INITIALIZE WS-TERMCALC
           MOVE "DUEDATE"  TO CA-FUNCTION OF WS-TERMCALC
           MOVE "STC"      TO CA-COMPANY  OF WS-TERMCALC
           MOVE "ARCLERK"  TO CA-USER     OF WS-TERMCALC
           MOVE 20261015   TO CA-BUSINESS-DATE OF WS-TERMCALC
           MOVE "30NET"    TO TC-TERMS-CODE
           MOVE 20261015   TO TC-INVOICE-DATE
           CALL "TERMCALC" USING WS-TERMCALC
           IF CA-RETURN-CODE OF WS-TERMCALC > 4
               PERFORM REPORT-THE-REFUSAL
           END-IF
```

`INITIALIZE` the area before every call: working storage inside `STERPCOB.dll` persists
between calls. Always qualify `CA-...` fields with `OF WS-xxx` when more than one COMMAREA
is in scope.

From .NET (`STERP.CobolInterop`, see `docs\NET-FRAMEWORK.md` section 4):

```vb
Dim ca As CommArea = CommArea.For("TERMCALCCA")   ' copybook parsed and cached
ca("TC-TERMS-CODE")  = "30NET"
ca("TC-INVOICE-DATE") = 20261015
Dim r As CobolResult = CobolGateway.Link("TERMCALC", ca, "DUEDATE")
```

The .NET side parses the `.cpy` file at run time, so **a COMMAREA layout change is a
breaking change for the client** — keep field names stable and add new fields at the end of
the body.

### Return codes (CONVENTIONS 3.3)

| Code | Meaning |
|---|---|
| `00` | OK |
| `04` | warning — proceed, something was defaulted or adjusted |
| `06` | hold — refused, but an authorised route exists |
| `08` | error — invalid input or a business refusal, **no business effect** |
| `12` | severe — SQL or system failure (`SQLnnnn`, message from `SQLIO-ERRMSG`) |
| `16` | not found |

`CA-REASON-CODE` carries a stable 8-character code (`PRM0004`, `NUM0007`, …) and
`CA-MESSAGE` a British-English sentence fit to show a user. Reason codes are listed per
service below. Online programs never `STOP RUN` and never crash on bad input — a bad
request comes back as `08`, because a COBOL runtime error would terminate the hosting
.NET process.

### Service index

| Program | Copybook | Purpose | DB |
|---|---|---|---|
| `RNDALLOC` | `RNDALLOCCA` | rounding and stable penny allocation | no |
| `DATECALC` | `DATECALCCA` | date validation and arithmetic, business days | yes (calendars) |
| `TERMCALC` | `TERMCALCCA` | due dates, settlement discount, Bacs adjustment | yes |
| `FXCONV` | `FXCONVCA` | currency conversion, realised gain/loss | yes |
| `STRUTIL` | `STRUTILCA` | code validation, reference normalisation, formatting | no |
| `PARMGET` | `PARMGETCA` | effective-dated tuning parameters | yes |
| `NUMGEN` | `NUMGENCA` | document numbering and voiding | yes |
| `PERMCHK` | `PERMCHKCA` | permission check | yes |
| `PERIODCK` | `PERIODCKCA` | accounting period policy | yes |
| `IDEMPOT` | `IDEMPOTCA` | idempotent instructions | yes |
| `AUDITLOG` | `AUDITLOGCA` | audit evidence | yes |
| `EXCPRAIS` | `EXCPRAISCA` | exceptions / work list | yes |
| `JOBLOG` | `JOBLOGCA` | batch job run registration | yes |
| `CMDARGS` | `CMDARGSCA` | `KEY=VALUE` command line parsing | no |
| `RUNPATH` | `RUNPATHCA` | output file naming and folders | no |
| `SQLIO` | `SQLIOCA` | the only route to the database | — |
| `PING` | `PINGCA` | engine / database health check | yes |
| `STFUNCS` | `STFNARGS` | user-defined `FUNCTION`s (not LINKable) | no |

---

## 2. Money and numbers

### RNDALLOC — rounding and allocation (`RNDALLOCCA`, REQ COM-011, AC-006)

| Function | Does |
|---|---|
| `ROUND` | round `RA-VALUE` to `RA-PLACES`, **half away from zero** → `RA-RESULT` |
| `TRUNC` | truncate towards zero |
| `UNITPRC` | unit price arithmetic at full precision |
| `EXTEND` | extend quantity × price, rounding once at the end |
| `ALLOC` | split `RA-AMOUNT` over `RA-LINE-CNT` lines so the parts add back exactly |

Key fields: `RA-VALUE`, `RA-VALUE2`, `RA-PLACES`, `RA-RESULT`, `RA-UNROUNDED`,
`RA-AMOUNT`, `RA-MODE` (`EQUAL` / `WEIGHT`), `RA-LINE-CNT`, `RA-LINE-REF(n)`,
`RA-WEIGHT(n)`, `RA-PART(n)`, `RA-ALLOCATED`, `RA-RESIDUAL`, `RA-BUMPED`,
`RA-MAX-LINES` (500).

Rules worth knowing:

* **Half away from zero** (AC-006): `1.005` → `1.01` and `-1.005` → `-1.01`. A credit note
  therefore rounds the same distance as the invoice it reverses.
* **Allocation is exact and stable**: `1.00` over three lines gives `0.34 / 0.33 / 0.33`,
  and `-1.00` gives `-0.34 / -0.33 / -0.33`. The parts always sum to `RA-AMOUNT`
  (`RA-RESIDUAL` = 0); `RA-BUMPED` says how many lines took a penny of the remainder.
  Allocating the same input twice gives the same answer.

Reason codes: `RND0001`–`RND0006` input refusals (`RND0002` line count outside 1–500,
`RND0003` bad mode, `RND0004` negative weight, `RND0005` zero total weight, `RND0006` zero
divisor), `RND0008` unsupported function, and `RND0009` — a self-check that returns `12` if
the allocated parts ever fail to reconcile with the amount.

### FXCONV — currency conversion (`FXCONVCA`, REQ COM-010/COM-011, AC-005)

| Function | Does |
|---|---|
| `GETRATE` | fetch the effective rate for `FX-CURRENCY` / `FX-RATE-DATE` / `FX-RATE-TYPE` |
| `TOGBP` | `FX-FOREIGN-AMOUNT` → `FX-GBP-AMOUNT` |
| `TOFCY` | `FX-GBP-AMOUNT` → `FX-FOREIGN-AMOUNT` |
| `REALISED` | realised gain/loss between `FX-RATE` and `FX-RATE-2` → `FX-REALISED` |

Key fields: `FX-CURRENCY`, `FX-RATE-DATE`, `FX-RATE-TYPE` (`SPOT`/`HMRC`/`BOE`),
`FX-RATE`, `FX-RATE-ENTERED`, `FX-QUOTE-BASIS` (`FCYPERGBP` / `GBPPERFCY`),
`FX-ALLOW-INVERT`, `FX-RATE-SOURCE`, `FX-RATE-FOUND-DATE`, `FX-FOREIGN-AMOUNT`,
`FX-GBP-AMOUNT`, `FX-PLACES`, `FX-RATE-2`, `FX-GBP-AMOUNT-2`, `FX-REALISED`,
`FX-POSITION` (`ASSET`/`LIABILITY`), `FX-UNROUNDED`, `FX-RESIDUAL`, `FX-RATE-TEXT`.

**Direction is the UK market and HMRC convention: `core.FxRate.UnitsPerGBP` is foreign
currency units per one pound sterling.** A rate quoted the other way round is only accepted
when `FX-ALLOW-INVERT` is `"Y"` and `FX-QUOTE-BASIS` says `GBPPERFCY`; otherwise a reversed
rate is refused rather than silently inverted, because a reversed EUR rate is the difference
between £1,000 and £1,170. Missing, zero and negative rates are all refused (`08`/`16`), never
defaulted to 1. `FX-RATE-FOUND-DATE` reports the rate date actually used when the exact date
has no quotation. Reason codes `FX0001`–`FX0010`.

### STRUTIL — text, codes and formatting (`STRUTILCA`, REQ COM-004/COM-024, AC-003, D-21)

| Function | Does |
|---|---|
| `UPPER` | fold to upper case |
| `CODE` | validate a reference code (D-21 character set), fold to upper case |
| `DOCREF` | **normalise a reference for duplicate detection** |
| `MONEY` | format `SU-AMOUNT` as money |
| `QTY` | format a quantity |
| `SQUEEZE` | collapse runs of spaces |

Key fields: `SU-TEXT-IN`, `SU-TEXT-OUT`, `SU-MAX-LEN`, `SU-LENGTH-OUT`, `SU-VALID`,
`SU-CHANGED`, `SU-AMOUNT`, `SU-PLACES`, `SU-CURRENCY`, `SU-SYMBOL`,
`SU-ZERO-POLICY` (`KEEP`/`STRIP`), `SU-BAD-CHAR`, `SU-BAD-POS`.

* `CODE` accepts the D-21 code character set and returns `04` with `SU-CHANGED = "Y"` when
  it had to fold case; anything outside the set is `08` with the offending character in
  `SU-BAD-CHAR` and its position in `SU-BAD-POS`, so the client can point at it.
* `DOCREF` normalisation makes `INV-0012/A`, `inv 0012 a` and `INV0012A` compare equal, so
  that a supplier invoice cannot be entered twice behind a cosmetic difference.

**Pound sign encoding.** COBOL sources and COMMAREAs are single byte. The pound sign is
held as `X"A3"` (ISO-8859-1 / Windows-1252) in `STM-POUND` (`STMONEY`) and never typed
literally into source, so the files stay byte-stable whatever editor encoding is in use.
`STRUTIL MONEY` emits that byte when `SU-SYMBOL = "Y"`; when the output is going anywhere
that is not certain to be single byte, ask for `SU-SYMBOL = "N"` and `SU-CURRENCY = "GBP"`
instead. Messages produced by the other services spell out `GBP` rather than embedding the
symbol. Reason codes `STR0001`–`STR0006`, `STR0009`.

---

## 3. Dates and terms

### DATECALC — dates and business days (`DATECALCCA`, REQ COM-008)

| Function | Does |
|---|---|
| `VALIDATE` | is `DC-DATE-1` a real date? |
| `ADDDAYS` | `DC-DATE-1` + `DC-NUMBER` calendar days |
| `ADDMONTH` | add months, clamping to the month end |
| `EOMONTH` | last day of the month |
| `DOW` | day of week → `DC-RESULT-NUM` (1 Monday … 7 Sunday), `DC-DAY-NAME` |
| `DAYSBTWN` | days between `DC-DATE-1` and `DC-DATE-2` |
| `FORMAT` | `DC-DATE-1` → `DC-TEXT-OUT` in `DC-FORMAT-OUT` |
| `PARSE` | `DC-TEXT-IN` in `DC-FORMAT-IN` → `DC-RESULT-DATE` |
| `ISBUSDAY` | is it a working day on `DC-CALENDAR`? |
| `ADDBUS` | add `DC-NUMBER` **business** days |
| `ADJUST` | move onto a business day in `DC-DIRECTION` (`FWD`/`BACK`) |
| `CACHECLR` | drop the cached calendars |

Key fields: `DC-DATE-1`, `DC-DATE-2`, `DC-RESULT-DATE`, `DC-NUMBER`, `DC-RESULT-NUM`,
`DC-CALENDAR`, `DC-DIRECTION`, `DC-TEXT-IN/OUT`, `DC-FORMAT-IN/OUT`, `DC-DAY-NAME`,
`DC-MONTH-NAME`, `DC-IS-BUSINESS`, `DC-IS-HOLIDAY`, `DC-IS-WEEKEND`, `DC-ADJUSTED`,
`DC-HOLIDAY-NAME`, `DC-CAL-SOURCE`, `DC-CAL-ROWS`, `DC-CACHE-HITS`, `DC-CACHE-LOADS`.

Calendars are `ENG` (England & Wales), `SCT`, `NIR` and `BACS`, read from
`core.CalendarDay` / `core.Holiday` and **cached in working storage** on first use — a
long batch run loads each calendar once. `DC-CACHE-HITS` / `DC-CACHE-LOADS` expose that;
`CACHECLR` forces a reload after a calendar is maintained. Dates are `9(8)` `YYYYMMDD`
throughout; `FORMAT`/`PARSE` convert to and from the UK `DD/MM/YYYY` presentation.
Reason codes `DAT0001`–`DAT0010`.

### TERMCALC — payment terms (`TERMCALCCA`, REQ COM-012/COM-026, AC-005)

| Function | Does |
|---|---|
| `GETTERMS` | read `core.PaymentTerms` into the COMMAREA |
| `DUEDATE` | due date from the terms and `TC-INVOICE-DATE` |
| `DISCOUNT` | settlement discount date and amount |
| `INVOICE` | everything for one invoice in a single call |
| `EOM` | end-of-month basis on its own |

Key fields: `TC-TERMS-CODE`, `TC-BASIS-TYPE` (`INVOICE` / `EOM` / `DAYOFNEXTMONTH`),
`TC-DAYS`, `TC-DAY-OF-MONTH`, `TC-DISCOUNT-DAYS`, `TC-DISCOUNT-PCT`, `TC-INVOICE-DATE`,
`TC-SUPPLY-DATE`, `TC-RECEIVED-DATE`, `TC-DUE-DATE`, `TC-DUE-DATE-RAW`,
`TC-DISCOUNT-DATE`, `TC-NET-AMOUNT`, `TC-VAT-AMOUNT`, `TC-DISCOUNT-AMT`,
`TC-DISCOUNT-BASIS`, `TC-BACS-ADJUST`, `TC-BACS-DIRECTION`, `TC-CALENDAR`,
`TC-TERM-DAYS`, `TC-MAX-TERM-DAYS`, `TC-LONG-TERM-FLAG`, `TC-SOURCE`, `TC-TERMS-DESC`,
`TC-DUE-DATE-UK`, `TC-DISC-DATE-UK`.

* **Statutory default**: with no usable terms the due date is the statutory 30 days and
  `TC-SOURCE` says so, with return code `04` — a missing terms code never stops an invoice.
* **AC-005, day 31 of the following month**: for `DAYOFNEXTMONTH` with `TC-DAY-OF-MONTH = 31`
  an invoice dated January 2027 is due **28 February 2027**, and dated January 2028 due
  **29 February 2028**. The day is clamped to the real month end rather than overflowing
  into March.
* **Bacs adjustment**: when `TC-BACS-ADJUST = "Y"` the due date is moved onto a Bacs
  processing day in `TC-BACS-DIRECTION`; `TC-DUE-DATE-RAW` keeps the unadjusted date so the
  adjustment stays visible and auditable.
* `TC-LONG-TERM-FLAG` marks terms beyond `TC-MAX-TERM-DAYS` (late payment / reporting duty).

Reason codes `TRM0001`–`TRM0009`.

---

## 4. Control services (database backed)

### PARMGET — tuning parameters (`PARMGETCA`, REQ COM-011/COM-025)

Functions `GET` (text) and `GETNUM` (text plus number).
Key fields: `PM-PARAM-CODE`, `PM-AS-OF`, `PM-VALUE-TEXT`, `PM-VALUE-NUM`, `PM-DATA-TYPE`,
`PM-UNIT`, `PM-DECISION-REF`, `PM-OWNER-ROLE`, `PM-DESCRIPTION`, `PM-EFFECTIVE-FROM`,
`PM-SCOPE`, `PM-DEFAULT-TEXT`, `PM-USE-DEFAULT`, `PM-FOUND`, `PM-IS-NUMERIC`.

Values come from `core.Parameter`; the company-specific row wins over the `'*'` row, and
within that the latest `EffectiveFrom` not later than `PM-AS-OF` (defaulting to
`CA-BUSINESS-DATE`). **COM-025**: when nothing is held and `PM-USE-DEFAULT = "Y"` the
caller's `PM-DEFAULT-TEXT` is returned with `04` and reason `PRM0004`, so a missing tuning
parameter cannot stop a business action; without a default the answer is a plain `16`.
Reason codes `PRM0001`–`PRM0005`, `PRM0008`, `PRM0009`, `SQL0001`.

### NUMGEN — document numbers (`NUMGENCA`, REQ COM-005/COM-015, D-15)

Functions `NEXT` and `VOID`. Key fields: `NG-SERIES-CODE`, `NG-DOCUMENT-NUMBER`,
`NG-SEQUENCE`, `NG-REASON-TEXT`, `NG-STATUS`.

`NEXT` draws the next number of `core.NumberSeries` for `CA-COMPANY` and formats it as
prefix + zero-padded sequence. **COM-015**: if `CA-CORRELATION-ID` is set and that same
instruction already drew a number, the original reference is returned with `04` and reason
`NUM0007` instead of burning a second one — a retried confirmation cannot leave an
unexplained gap in a legal sequence. `VOID` records an abandoned reference with a mandatory
reason (`NG-STATUS` becomes `VOID`); a voided number stays traceable and is never reissued
(D-15). Reason codes `NUM0001`–`NUM0008`, `NUM0009`, `SQL0001`.

### PERMCHK — authority (`PERMCHKCA`, REQ COM-002/COM-019)

Function `CHECK`. Key fields: `PK-PERMISSION`, `PK-CHECK-USER` (defaults to `CA-USER`),
`PK-CHECK-SITE`, `PK-ALLOWED`, `PK-REASON-TEXT`.

Calls `core.usp_CheckPermission`. A refusal is `06` with `PK-ALLOWED = "N"` and the reason
text from the procedure — **never a silent failure**, so the user is told why and what would
authorise it. Reason codes `PRM0001`–`PRM0003`, `PRM0005`–`PRM0007`, `PRM0009`, `SQL0001`.

### PERIODCK — accounting period policy (`PERIODCKCA`, REQ COM-009)

Functions `CHECK` and `DATES`. Key fields: `PC-POSTING-DATE`, `PC-DOCUMENT-DATE`,
`PC-TAX-POINT`, `PC-ACTION`, `PC-FISCAL-YEAR`, `PC-PERIOD-NO`, `PC-PERIOD-YYYYPP`,
`PC-PERIOD-STATUS`, `PC-ALLOWED`, `PC-OVERRIDE-PERM`, `PC-REASON-TEXT`.

| Period status | Answer |
|---|---|
| `OPEN` | `00`, `PC-ALLOWED = "Y"` |
| `SOFT` | `00` if the user holds `FIN.PERIOD.SOFTPOST`, otherwise **`06`** with that permission in `PC-OVERRIDE-PERM` |
| `HARD` | `08` — post the correction in the open period or ask the controller to reopen (D-15) |
| `FUTURE` | `08` — not yet open |
| none | `08` — no period covers that date |

Backdating a document does not authorise posting into a closed period; the status is always
returned so the caller can offer the authorised route. Reason codes `PER0001`–`PER0007`,
`PER0009`, `SQL0001`.

### IDEMPOT — idempotent instructions (`IDEMPOTCA`, REQ COM-015/COM-014)

Functions `BEGIN` and `END`. Key fields: `ID-OPERATION`, `ID-STATUS`, `ID-OUTCOME`
(`STARTED` / `INPROGRESS` / `COMPLETED` / `FAILED`), `ID-RESULT-REF`, `ID-RESULT-MESSAGE`,
`ID-STARTED-AT`, `ID-ENDED-AT`. The key is `CA-CORRELATION-ID`.

| `BEGIN` outcome | Return | Meaning |
|---|---|---|
| `STARTED` | `00` | new work — carry on |
| `INPROGRESS` | `06` | another attempt is unresolved — establish its outcome first |
| `COMPLETED` | `04` | already done; `ID-RESULT-REF` is the original result |
| `FAILED` | `08` | the earlier attempt failed |

Close with `END` and `ID-STATUS` of `COMPLETED` or `FAILED`. An unresolved attempt is
deliberately distinct from a success, so a user can tell "already posted" from "we do not
know yet". Reason codes `IDM0001`–`IDM0010`, `SQL0001`.

### AUDITLOG — audit evidence (`AUDITLOGCA`, REQ COM-013)

Function `WRITE`. Key fields: `AU-ENTITY-TYPE`, `AU-ENTITY-KEY`, `AU-ACTION`,
`AU-FIELD-NAME`, `AU-OLD-VALUE`, `AU-NEW-VALUE`, `AU-REASON`, `AU-AUTHORITY-REF`,
`AU-SOURCE`, `AU-SENSITIVE`, `AU-AUDIT-ID`.

Writes `core.AuditLog` through `core.usp_WriteAudit` and returns the row identity.
Mandatory evidence (entity type, entity key, action, user) is refused with `08` rather than
stored half-complete. `AU-SENSITIVE = "Y"` marks rows only an authorised reviewer may read
(COM-019). Reason codes `AUD0001`–`AUD0003`, `AUD0008`, `AUD0009`, `SQL0001`.

### EXCPRAIS — exceptions and work lists (`EXCPRAISCA`, REQ COM-017/COM-022)

Function `RAISE`. Key fields: `EX-MODULE`, `EX-SEVERITY`, `EX-CATEGORY`, `EX-MESSAGE-TEXT`,
`EX-DOCUMENT-TYPE`, `EX-DOCUMENT-REF`, `EX-OWNER-USER`, `EX-OWNER-ROLE`, `EX-PRIORITY`,
`EX-DUE-AT`, `EX-AMOUNT-GBP`, `EX-QUANTITY`, `EX-EXCEPTION-ID`, `EX-STATUS`, `EX-RAISED-AT`.

**The severity drives the return code** so holds, warnings and errors stay distinguishable
(COM-017): `HOLD` → `06`, `WARNING` → `04`, `ERROR` → `00` with the exception recorded.
In every case the row is written to `core.ExceptionItem` with status `OPEN`. An exception
with no owner (neither user nor role) is refused — COM-022, an unowned exception never gets
worked. Reason codes `EXC0001`–`EXC0006`, `EXC0008`, `EXC0009`, `SQL0001`.

### JOBLOG — batch run registration (`JOBLOGCA`, REQ COM-014/COM-018)

Functions `START` and `END`. Key fields: `JL-JOB-NAME`, `JL-JOB-RUN-ID`, `JL-STATUS`,
`JL-RETURN-CODE`, `JL-ROWS-PROCESSED`, `JL-ARGUMENTS`, `JL-LOG-FILE`, `JL-SPOOL-FILE`,
`JL-MESSAGE-TEXT`, `JL-STARTED-AT`, `JL-ENDED-AT`.

Every batch main program calls `START` first and `END` last, so an abandoned run is visible
as one still marked `RUNNING` rather than silently missing. `JL-STATUS` uses the wording of
CONVENTIONS 3.6 — `RUNNING`, `COMPLETE`, `WARNING`, `FAILED`, `CANCELLED` — which the
service maps to the `core.JobRun` domain (`COMPLETE` ↔ `OK`, `CANCELLED` ↔ `ABENDED`) in
both directions, so callers never see the storage spelling. Reason codes `JOB0001`–`JOB0003`,
`JOB0008`, `JOB0009`, `SQL0001`.

---

## 5. Batch plumbing

### CMDARGS — command line (`CMDARGSCA`, REQ COM-018)

Functions `CMDLINE` (read the real command line), `PARSE` (parse `CM-COMMAND-LINE`) and
`GET` (look `CM-LOOKUP-KEY` up → `CM-LOOKUP-VALUE`, `CM-FOUND`).

Parses `KEY=VALUE` pairs separated by spaces, e.g.
`AGEDDEBT CO=STC ASAT=20261004 USER=ARCLERK`. The common keys are lifted into their own
fields — `CM-CO`, `CM-SITE`, `CM-USER`, `CM-ASAT`, `CM-FROM`, `CM-TO`, `CM-PERIOD`,
`CM-MODE`, `CM-OUT` — and every pair is also available in `CM-PAIR(n)` (`CM-KEY`,
`CM-VALUE`, up to 20, `CM-PAIR-CNT` used, `CM-DROPPED` beyond the limit). Keys are folded
to upper case. Reason codes `CMD0001`–`CMD0004`, `CMD0009`.

### RUNPATH — output files (`RUNPATHCA`, REQ COM-018/COM-020)

Functions `BASE` (resolve the run directory only) and `BUILD` (full path).
Key fields: `RP-FOLDER`, `RP-REPORT-ID`, `RP-COMPANY-REF`, `RP-EXTENSION`, `RP-TIMESTAMP`,
`RP-USE-TIMESTAMP`, `RP-FILE-NAME`, `RP-FULL-PATH`, `RP-BASE-PATH`, `RP-SOURCE`.

The run directory is `%STERP_RUN%`, or `run` relative to the current directory when that is
unset (`RP-SOURCE` reports `ENV` or `DEFAULT`); a trailing separator is stripped.

| `RP-FOLDER` | Sub-directory | Default extension |
|---|---|---|
| `SPOOL` | `run\spool` | `lst` (132-column reports) |
| `BACS` | `run\bacs` | `txt` |
| `MTD` | `run\mtd` | `json` |
| `EINVOICE` | `run\einvoice` | `xml` |
| `EXTRACTS` | `run\extracts` | `csv` |
| `LOGS` | `run\logs` | `log` |
| `WORK` | `run\work` | `tmp` |

The file name is `<RP-REPORT-ID>-<RP-COMPANY-REF>-<yyyymmddhhmmss>.<ext>`; the company is
dropped when blank and the stamp omitted when `RP-USE-TIMESTAMP = "N"`. Supply
`RP-TIMESTAMP` to keep all of one job's files stamped alike. Reason codes `RUN0001`,
`RUN0002`, `RUN0009`.

---

## 6. SQLIO and PING

`SQLIO` (`SQLIOCA`) is the only route from COBOL to SQL Server — see CONVENTIONS 3.4.
Functions `EXEC`, `FTCH`, `SNGL`, `MORE`, `CLOS`, `BEGN`, `COMT`, `ROLB`, `CONN`, `DISC`;
`SQLIO-SQLCODE` is `0` OK, `100` not found / end of data, `-1` error (`SQLIO-SQLSTATE`,
`SQLIO-ERRMSG`). Always bind with `?` and `SQLIO-PARM-VAL(n)`; never build SQL from user
text. Cursors 1–8 allow nested loops. `PING` (`PINGCA`) echoes a COMMAREA and reads the
database — the client's health check.

---

## 7. User-defined functions — `STFUNCS` (`STFNARGS`)

`STFUNCS` is **not** LINKable; it is a library of user-defined `FUNCTION`s used inside
COBOL expressions.

| Function | Arguments | Returns |
|---|---|---|
| `ROUND-AWAY` | value, places | value rounded half away from zero |
| `SIGN-OF` | value | `-1`, `0` or `+1` |
| `IS-LEAP` | year | `1` / `0` |
| `DAYS-IN-MONTH` | year, month | `28`–`31`, `0` if the month is impossible |
| `END-OF-MONTH` | `YYYYMMDD` | last day of that month, `0` if the month is impossible |
| `WEEKDAY-OF` | `YYYYMMDD` | `1` Monday … `7` Sunday, `0` if the date is impossible |

Declare them and pass arguments **only** through `STFNARGS` fields:

```cobol
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ROUND-AWAY
           FUNCTION SIGN-OF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY STFNARGS.
      *> ...
           MOVE WS-AMOUNT TO FNA-VALUE
           MOVE 2         TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE FNA-RESULT TO WS-AMOUNT
```

GnuCOBOL passes user-defined `FUNCTION` arguments **by reference without converting them**,
so a literal, an expression or a field whose `PICTURE` differs from the parameter is read as
raw storage and silently produces nonsense. Always move into `FNA-VALUE` / `FNA-PLACES` /
`FNA-YEAR` / `FNA-MONTH` / `FNA-DATE` first and take the answer from `FNA-RESULT` /
`FNA-DATE-RESULT`.

**Linking.** libcob resolves user-defined functions at run time (`cob_resolve_func`) even
under `-fstatic-call`, so nothing in a module archive refers to `STFUNCS` and an ordinary
link would leave it out. `build\build-cobol.ps1` therefore puts the object of every source
containing a `FUNCTION-ID` on the link line explicitly — for the test executables, for the
batch executables and for `STERPCOB.dll`. This works in all three; `TSTFUNC` proves it both
directly and through `DATECALC` and `RNDALLOC`, which are the objects the DLL publishes.

---

## 8. Tests

`build\run-tests.ps1 -Layer cobol`

| Suite | Covers |
|---|---|
| `TRNDALL` | RNDALLOC rounding and allocation, `ROUND-AWAY`, `SIGN-OF` |
| `TDATECAL` | DATECALC, calendars and caching |
| `TTERMCAL` | TERMCALC due dates, discounts, Bacs |
| `TFXCONV` | FXCONV direction, refusals, realised gain/loss |
| `TSTRUTIL` | STRUTIL codes, normalisation, formatting |
| `TCMDARG` | CMDARGS parsing |
| `TRUNPATH` | RUNPATH folders and file naming |
| `TSTFUNC` | every `STFUNCS` function, directly and through the engine |
| `TSQLIO` | SQLIO and PING |
| `TDBSVC` | PARMGET, NUMGEN, PERIODCK, PERMCHK, AUDITLOG, EXCPRAIS, IDEMPOT, JOBLOG |

`TDBSVC` needs SQL Server. It runs its writes inside one `BEGN … ROLB` transaction with its
own fixture rows (keys prefixed `ZZ` / `STERP.TEST`), so it is repeatable and leaves nothing
behind; refusal cases run in a second phase outside that transaction because several core
procedures report refusals with `THROW` under `SET XACT_ABORT ON`, which also dooms the
caller's transaction. With no database reachable the suite reports TAP `# SKIP` rather than
failing.
