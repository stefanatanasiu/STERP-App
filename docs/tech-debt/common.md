# Technical debt — module `common`

Deliberate legacy texture and genuine compromises in the common services. Everything here
is **correct but dated**, or correct-but-duplicated; nothing in this list is a known defect.
One bullet per item: file, pattern, why it is debt, how to modernise.

See `docs\COMMON-SERVICES.md` for what the services do.

---

## 1. Rules duplicated between COBOL and SQL

* **`cobol\online\common\PARMGET.cob` (`1000-READ-PARAMETER`) — the parameter selection rule
  is implemented twice.** `core.usp_GetParameter` raises `STERP-E-0003` when a parameter is
  not held, and because it runs with `SET XACT_ABORT ON` that raise also **dooms the
  caller's transaction**. COM-025 says a missing tuning parameter must never stop a business
  action, so PARMGET issues the procedure's `SELECT TOP (1) … ORDER BY` itself and treats
  "no rows" as not held. *Why it is debt:* the effective-date and `'*'` fallback rule now
  exists in two places and can drift. *Modernisation:* give the procedure a non-throwing
  twin (`core.usp_TryGetParameter`, or an output "found" column) and have PARMGET call that;
  then delete the inline `SELECT`.

* **`cobol\online\common\PERIODCK.cob` (`1000-CHECK-PERIOD`, `1100-APPLY-PERIOD-POLICY`) —
  the period policy is implemented twice.** `fin.usp_ValidatePeriod` answers only "yes": it
  reports every refusal with `THROW`, so it cannot distinguish a hard close (`08`) from a
  soft close that an authorised user may still post into (`06`), and the raise dooms the
  caller's posting transaction. COM-009 needs that distinction, so PERIODCK reads
  `fin.Period` plus `core.fn_HasPermission` and applies the policy itself. *Why it is debt:*
  the same policy lives in the procedure and in COBOL. *Modernisation:* have the procedure
  return a decision row (`Status`, `Allowed`, `OverridePermission`) instead of throwing, and
  reduce PERIODCK to calling it.

* **`cobol\online\common\NUMGEN.cob` (`1050-REPLAY-ISSUED`) — idempotency implemented in the
  caller.** `core.usp_NextDocumentNumber` stores `CorrelationId` on `core.DocumentNumber`
  but never looks one up, so COM-015 (a repeated instruction must not burn a second legal
  reference) is enforced by an extra `SELECT` in NUMGEN before the number is drawn. *Why it
  is debt:* the check and the draw are two statements, so the guarantee relies on the
  caller's transaction rather than on one atomic operation in the procedure.
  *Modernisation:* move the lookup inside `core.usp_NextDocumentNumber`, under the same
  `UPDLOCK, HOLDLOCK` that protects the sequence.

* **`cobol\online\common\JOBLOG.cob` (`WS-DB-STATUS`, `2000-END`) — two vocabularies for one
  status.** A batch program speaks the wording of `CONVENTIONS 3.6` (`COMPLETE`,
  `CANCELLED`) while `core.JobRun` constrains `Status` to `RUNNING / OK / WARNING / FAILED /
  ABENDED`; JOBLOG translates in both directions. *Why it is debt:* a reader has to know
  that `COMPLETE` is stored as `OK`, and a report written straight against `core.JobRun`
  shows the other word. *Modernisation:* agree one domain and change the check constraint
  and the convention together.

* **`cobol\online\common\IDEMPOT.cob` (`1000-BEGIN`) — outcome names translated.**
  `core.usp_IdempotentBegin` returns `NEW` / `DUPLICATE` / `INPROGRESS`; the COMMAREA
  publishes `STARTED` / `COMPLETED` / `INPROGRESS` / `FAILED`. The mapping is done once, on
  entry. *Modernisation:* align the procedure's wording with the COMMAREA so the mapping
  disappears.

## 2. Era flavour kept on purpose

* **`cobol\online\common\DATECALC.cob` (`1720-PARSE-SIX`, lines ~434–441) — Y2K windowing
  remnant.** `IF WS-Y2 < 50 → 2000 + yy ELSE 1900 + yy` survives for two-digit years in old
  interface files, as the 1999 change log entry records. *Why it is debt:* a two-digit year
  of `50`–`99` is read as the twentieth century, which will be wrong for dates after 2049.
  *Modernisation:* reject two-digit years on new interfaces; keep the window only behind an
  explicit `DC-FORMAT-IN` of `DDMMYY`.

* **`GO TO <para>-EXIT` as the early return in nearly every service** (141 occurrences
  across `cobol\online\common`). Each `SECTION` validates, then jumps to its own `-EXIT`
  paragraph instead of nesting `IF`s or using `EXIT SECTION`. *Why it is debt:* it is the
  1980s single-exit idiom and hides the control flow from modern readers and analysers.
  *Modernisation:* `EXIT SECTION` / `EXIT PARAGRAPH`, or invert the validation into a
  guard-clause paragraph that sets a flag.

* **Fixed `OCCURS` limits with no `OCCURS DEPENDING ON`.** `RA-LINE OCCURS 500`
  (`RNDALLOCCA`), `CM-PAIR OCCURS 20` (`CMDARGSCA`), `SQLIO-PARM OCCURS 40` and
  `SQLIO-COL-WIDTH OCCURS 80` (`SQLIOCA`), `CAL-ENTRY OCCURS 78` (`CALBUILT`),
  `WS-SLOT OCCURS 12` × `WS-SL-DAY OCCURS 366` (`DATECALC` calendar cache). The limits are
  deliberate: `CobolInterop` cannot marshal `OCCURS DEPENDING ON`, and a COMMAREA must have
  a fixed length. *Why it is debt:* an allocation over more than 500 lines is refused
  (`RND0002`, "Line count must be between 1 and 500") and a command line with more than 20
  pairs silently reports the excess in `CM-DROPPED`. *Modernisation:* page the input through
  the service, or move the limit into a parameter so it can be raised without a recompile of
  every caller.

* **`cobol\online\common\DATECALC.cob` — calendar cache is never invalidated by itself.**
  Calendars are loaded once per calendar/year into working storage, which persists between
  calls inside `STERPCOB.dll`; a holiday maintained during a long client session is not seen
  until `CACHECLR` is called. *Why it is debt:* correctness depends on somebody remembering
  to call `CACHECLR`. *Modernisation:* stamp the cache with `MAX(ModifiedAt)` of
  `core.CalendarDay` and reload when it moves, or have the maintenance screen call
  `CACHECLR`.

* **Reason-code prefix collision: `PRM` means two things.** `PARMGET` uses
  `PRM0001`–`PRM0005` for parameters and `PERMCHK` uses `PRM0001`–`PRM0007` for permissions,
  so a reason code is only unambiguous together with `CA-PROGRAM`. *Modernisation:* renumber
  `PERMCHK` to a `PER`-free prefix such as `AUT`, in one go across the traceability matrix.

* **`CA-MESSAGE` carries presentation text from the engine.** Services compose a finished
  British-English sentence (including number formatting) rather than returning only a code
  and arguments. *Why it is debt:* the engine cannot be localised, and the client cannot
  re-word a message without changing COBOL. *Modernisation:* return the reason code plus
  typed substitution values and let the client render; keep `CA-MESSAGE` as a fallback.

## 3. Encoding and formatting

* **`cobol\online\common\STRUTIL.cob`, `cobol\copy\STMONEY.cpy` — the pound sign is the
  single byte `X"A3"`.** COBOL sources and COMMAREAs are single byte, so the symbol is held
  as a hex literal in `STM-POUND` and never typed into source; `STRUTIL MONEY` emits it only
  when `SU-SYMBOL = "Y"`, and service messages spell out `GBP` instead. *Why it is debt:*
  the byte is only correct when the consumer reads Windows-1252/ISO-8859-1; in a UTF-8
  pipeline it is a lone invalid byte. *Modernisation:* return money unsymbolled with
  `SU-CURRENCY`, and let the .NET client apply `CultureInfo("en-GB")` formatting.

## 4. Build and linkage

* **`build\build-cobol.ps1` (`Func-Objs`) — `FUNCTION-ID` objects are forced onto the link
  line.** libcob resolves user-defined functions at run time through `cob_resolve_func` even
  under `-fstatic-call`, so no archive member refers to `STFUNCS` and the linker would never
  extract it; the build therefore adds the object of every source containing a `FUNCTION-ID`
  explicitly. *Why it is debt:* it is a build-script special case that a newcomer will not
  expect, and a missing entry fails at first call rather than at link time. *Modernisation:*
  keep it, but assert it — `TSTFUNC` now calls every function directly and through
  `DATECALC`/`RNDALLOC`, so a regression is caught by the test suite rather than in the
  client.

* **`cobol\tests\common\TDBSVC.cob` — the suite is split into two phases.** Writes run
  inside one `BEGN … ROLB`; refusal cases run afterwards outside it, because several core
  procedures report business refusals with `THROW` under `SET XACT_ABORT ON`, which dooms
  the enclosing transaction and would destroy the fixtures of every later test. *Why it is
  debt:* the test layout is dictated by the procedures' error style rather than by what is
  being tested. *Modernisation:* once the procedures return decision rows instead of
  throwing (items in section 1), the two phases can be merged.

## 5. Correlation-id reuse audit

`core.IdempotencyKey` has `CorrelationId` as its whole primary key — `Operation` is recorded
but not keyed. `core.usp_IdempotentBegin/End` (`IDEMPOT`), `inv.usp_PostStockMovement`
(`STKMOVE`) and `fin.usp_PostJournal` (`JNLENTRY`/`GLPOST`/`JNLPOST`/`ARPOSTGL`) therefore
all compete for one row. A unit of work that sends the *same* id to two of those posting
services gets exactly one effect: the first call completes the key, and the second is handed
the first one's result and posts nothing. `CA-CORRELATION-ID` is `PIC X(36)` (`STCAHDR`), so
appending `-L1` to a 36-character guid truncates the suffix straight back off and reproduces
the same collision. `NUMGEN` is **not** affected — it keys its own issue table, which is why
the reference fixes in `so\DESPATCH` (`9700-SUB-CORR`) and `po\RECEIPT` (`1050-CORRELATION`)
still hand it the caller's raw id.

The fix everywhere is the same: shorten the caller's id to a 30-character stem and append a
suffix that names the sub-instruction (`-P`, `-C001`, `-L0007`, `-RCV`, `-ALLOC`), so the
suffix can never be truncated away.

### Programs audited

| Program | Idempotent calls in one unit of work | Verdict |
|---|---|---|
| `online\kit\DISASSY` | `STKMOVE` ISSUE (parent) + `STKMOVE` RECEIPT per recovered line | **Defect — fixed.** Parent issue and every component receipt shared the caller's id, so only the issue posted; the components were reported posted but no stock was received. Now `-P` / `-Cnnn`. |
| `online\kit\KITISSUE` | `STKMOVE` ISSUE once per component line of a `BACKFLSH` pass | **Defect — fixed.** Every backflushed line carried the caller's id, so only the first component was issued. Now `-Lnnnn` from `KL-LINE-NUM`. |
| `online\kit\KITCOMPL` | `KITISSUE` backflush + `STKMOVE` RECEIPT (parent) + `fin.usp_PostJournal` | **Defect — fixed.** The journal already derived its own id (`Build-Posting-Correlation`, CR2514) but the parent receipt still used the caller's, which the backflush had already spent. Now `-RCV`. |
| `online\ar\RCPTALOC` | `IDEMPOT` + `NUMGEN` + `ARPOSTGL` (cash) + `ARPOSTGL` (allocation adjustments) | **Defect — fixed.** The second posting built its id as `TRIM(AR-CORREL) + "-ALLOC"` into `AR-SCRATCH` `X(256)` and then moved it to a `X(36)` field: a 36-character caller id truncated the suffix away, so discounts, write offs and realised FX were never posted. Now a 30-character stem + `-ALLOC`. |
| `batch\itm\OPENSTK` | `STKMOVE` OPENING once per file line | **Defect — fixed.** The per-line id was the natural key cut to 36 characters; two long item codes agreeing for 36 characters shared one key and the second line loaded nothing. The file line number is now appended to a 27-character stem. |
| `batch\xmd\CONVLOAD` | `STKMOVE` OPENING once per conversion stock row | **Defect — fixed.** The row sequence sat at the *end* of the id before it was cut to 36, so a long file name truncated the sequence away and the whole load shared one key. The sequence is now appended to a 26-character stem. |
| `batch\fin\FINDEMO` | `JNLPOST` once per period and kind | Clean on correlation (`FINDEMO-<co>-<fy>-<pp>-<kind>` is distinct per journal). Two unrelated defects were found and fixed — see section 5.1. |
| `online\ar\ARINVPST` | `IDEMPOT` + `NUMGEN` + one `ARPOSTGL` | Clean. One posting call; `IDEMPOT` only claims the key as `INPROGRESS`, which `fin.usp_PostJournal` does not treat as a replay. |
| `online\ar\ARCRNOTE` | `IDEMPOT` + `NUMGEN` + one `ARPOSTGL` | Clean, same shape. |
| `online\ar\ARADJUST` | `NUMGEN` + one `ARPOSTGL` | Clean. |
| `online\ar\ARDEPOS` | `NUMGEN` + one `ARPOSTGL` | Clean. |
| `online\ar\ARDISPUT` | `NUMGEN` only | Clean — no posting service. |
| `online\ar\ARINSOLV` | `NUMGEN` only | Clean. |
| `online\ar\DDMAND` | `NUMGEN` in two different functions | Clean — `NUMGEN` does not use `core.IdempotencyKey`, and the two draws are separate instructions. |
| `online\ar\ARPOSTGL` | one `fin.usp_PostJournal` | Clean — it is the service; the duty to supply a distinct id is the caller's. |
| `online\ap\APSVC` | `AUDITLOG` + `NUMGEN` + `IDEMPOT` in separate functions | Clean. |
| `online\ap\APPOST` | one `fin.usp_PostJournal` | Clean — service, caller's id passed straight through. |
| `online\fin\JNLPOST` | one `fin.usp_PostJournal` **or** one `fin.usp_ReverseJournal` | Clean — `POST` and `REVERSE` are separate functions, never both in one call. |
| `online\fin\JNLENTRY` | one `fin.usp_PostJournal` | Clean — service. |
| `online\itm\STKMOVE` | one `inv.usp_PostStockMovement` | Clean — service. |
| `online\kit\KITRET` | one `STKMOVE` RETURNIN per call | Clean — one movement per invocation. |
| `online\kit\KITCREAT` | `NUMGEN` + `AUDITLOG` | Clean — no posting service. |
| `batch\itm\COUNTADJ` | `STKMOVE` ADJUST once per count line | Clean *for this defect class* — it passes no correlation id at all, so every line posts. Separate debt: a rerun of an approved count would adjust twice; it is protected only by `4150-MARK-LINE`. |
| `online\common\IDEMPOT`, `online\common\NUMGEN` | the services themselves | Clean. |

Nothing in `online\cst`, `online\itm` (beyond `STKMOVE`), `online\xmd`, `batch\cst`,
`batch\ap`, `batch\ar`, `batch\kit` calls an idempotent posting service more than once in a
unit of work. `so`, `po` and `bom` were out of scope for this sweep.

*Why it is debt:* the derivation is copied into each program rather than offered as a common
service, so the next unit of work that posts twice will make the same mistake. *Modernisation:*
give `IDEMPOT` a `DERIVE` function that takes `ID-OPERATION` as the suffix and returns the
sub-correlation, and widen `CA-CORRELATION-ID` to match the `varchar(64)` the database
already allows.

### 5.1 `batch\fin\FINDEMO` — RC 4 on a healthy demonstration database

`FINDEMO CO=STC FY=2026 FROM=1 TO=9 MODE=COMMIT` ended RC 4 with 0 posted, 2 already present
and 43 refused. Two separate causes, neither of them correlation reuse:

* **Legitimate refusals reported as failures.** Periods 1–5 are `HARD`, period 6 is `SOFT`
  and period 9 is `FUTURE`, so `JNLPOST` correctly refused them with `JNP0018`/`JNP0017`. A
  demonstration generator holds no close-adjustment authority and never will, so a closed
  period is not a failure of the job. `1150-PERIOD-DATES` now reads `fin.Period.Status`,
  `2000-ONE-JOURNAL` reports a non-open period as `SKIPPED-CLOSED` without staging anything,
  and `2300-POST` treats a late `JNP0017`/`JNP0018` the same way. They are counted and
  printed separately from refusals and no longer force RC 4.
* **`SQL0001` on every trading journal of an open period — a real defect.** Accounts `4110`,
  `5030`, `6000` and `6200` carry a `MANDATORY` `CC` rule in `fin.AccountDimRule`, and
  `2200-STAGE-LINE` staged no `DimCostCentre`, so `fin.usp_PostJournal` threw
  `STERP-E-0013`. The ODBC driver prefixes the message, so `JNLPOST 9300-SQL-FAILED` could
  not recognise `STERP-E-` and reported a bare `SQL0001` with RC 12 — which is why the
  symptom looked like infrastructure rather than data. `2200-STAGE-LINE` now sets
  `DimCostCentre` from `fin.DimensionValue` for exactly those accounts whose rule is
  `MANDATORY` (a `FORBIDDEN` account must still carry none, or the same procedure refuses it
  from the other direction). A new `2150-CLEAR-STAGE` deletes any residue of an earlier
  attempt before the lines are staged, because rows left behind by a refused attempt used to
  survive the correction and be posted exactly as first written.

  *Remaining debt:* `JNLPOST 9300-SQL-FAILED` matches `CA-MESSAGE(1:8) = "STERP-E-"` on a
  message the driver has already prefixed, so every procedure `THROW` reaches a batch job as
  `SQL0001`/RC 12 instead of its own reason code. *Modernisation:* search for `STERP-E-`
  anywhere in the message rather than anchoring at position 1.

`FINDEMO` now ends RC 0 twice in a row (8 posted then 10 already present, 35 period-closed,
0 refused) and `build\demo-data.ps1` ends with `RECONCTL` RC 0 and all eight control
families `AGREED`.

## Function codes longer than CA-FUNCTION PIC X(8) (CR2614)

| Where | Pattern | Why it matters | Modernisation hint |
|---|---|---|---|
| `ECOIMPCT` (DECIDEALL), `ITEMENQ` (WHEREUSED), `APPAYRUN` (UNCERTAIN), `IMPVAT` (RECONCILE); earlier `STATPROF` (ARDCHANGE), `ITEMCOPY` (BULKAPPLY), `POSUPCHK` (EXCEPTION), `SODOCVR`/`REBATE` | `EVALUATE CA-FUNCTION WHEN "<9+ chars>"` - the MOVE into the 8-byte field truncates, so the branch is dead code and the caller gets "unknown function" or a different branch | Silent loss of a business function; compilers do not warn | Use an enumerated function code type (enum/const) shared by caller and callee; a static check that every `WHEN` literal fits the receiving field |
