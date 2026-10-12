# `ap` – intentional technical debt

Deliberate legacy texture in the Purchase Ledger. The module is notionally a 1991 MVS port, so the
debt is the debt a 35-year-old purchase ledger really accumulates. Each entry: **what**, **where**,
**why it is there**, **how a modernisation would remove it**.

| # | File / pattern | What it is | Why (the legacy story) | Modernisation hint |
|---|---|---|---|---|
| 1 | `cobol\copy\APINVREGCA.cpy` – `IR-LINE OCCURS 40` | Fixed 40-line invoice limit in the COMMAREA. | 1991 CICS COMMAREAs were capped at 32 KB, so every table was a fixed `OCCURS` sized on "the biggest invoice anyone had seen". | Replace the COMMAREA with a streamed/paged contract (or a JSON/gRPC message) and let the line count be unbounded; keep a server-side page size instead. |
| 2 | `cobol\copy\APINVREGCA.cpy` – `IR-SIMILAR OCCURS 10` | Only the first ten similar documents are returned. | Same COMMAREA budget. The clerk screen only had ten lines anyway. | Return a cursor/continuation token; let the UI page through candidates. |
| 3 | `cobol\online\ap\APINVREG.cob` – `3200-NORMALISE-REF` | Duplicate detection normalises the supplier reference by hand (upper case, strip punctuation, strip leading zeros) in COBOL. | No collation or computed-column support on the 1991 platform; the rule was written once and copied. | Persist a computed/persisted `RefNormalised` column with an index, or a proper fuzzy-match service; keep one implementation, not one per program. |
| 4 | `cobol\online\ap\APMATCH.cob` – tolerance parameters read per call | `PARMGET` is called for each of the six `AP.TOL.*` parameters on every `MATCH`, with no caching. | Parameters used to live in a VSAM file that was cheap to read; nobody revisited it after the move to SQL. | Cache the parameter set per transaction, or move tolerances into a typed configuration service with change auditing. |
| 5 | `cobol\online\ap\APMATCH.cob` / `APBLOCK.cob` | Match state is held both on `ap.PurchaseInvoice` (`MatchState`) and derived from `ap.InvoiceMatch` rows. | Denormalised in 1994 because the summary screen was too slow over the match detail. | Single source of truth: derive the state in a view (`ap.vw_InvoiceMatchState`) and drop the stored column, or keep it but maintain it in one trigger/proc. |
| 6 | `cobol\online\ap\APHASH.cob` | Home-grown content hash (positional weighted sum) for payment-approval invalidation. | Predates any available cryptographic hash on the platform; the requirement was only "detect a change". | Use SHA-256 over a canonical serialisation, computed in one place (DB `HASHBYTES` or .NET) and stored with the approval. |
| 7 | `db\procs\ap\usp_ReserveProposal.sql` – `UPDLOCK, READPAST` | Pessimistic reservation of open items. | The batch payment run was single-threaded in 1991; when it was made concurrent the cheapest fix was a lock hint. | Optimistic concurrency with a row version, or an outbox/queue that hands each open item to exactly one proposal. |
| 8 | `cobol\batch\ap\APBACS.cob` | Standard 18 single-day 80-byte labels / 100-byte data, built into `PIC X(100)` and written byte-sequentially with CRLF. | Byte writes preserve mandatory trailing spaces without relying on process-wide `COB_LS_FIXED`; `ApBacsFormatTests` verifies physical bytes, fields, totals and counts. | Keep the mandated format but isolate it behind a formatter module, retaining the output-shape regression. |
| 9 | `cobol\batch\ap\APBACS.cob` – sort code / account validated by modulus check in COBOL | Hand-coded modulus weighting table. | The VocaLink table was typed in once, in 1991, and updated by hand. | Call a bank-detail validation service (or the CoP provider) and stop shipping a static weighting table. |
| 10 | `cobol\batch\ap\APREMIT.cob`, `APAGED.cob`, `APPPR.cob`, `APVAT6M.cob` | Report Writer, 132 columns, `run\spool\*.lst`. | Everything printed on a line printer. 132 columns is the paper width. | Emit a structured result set (or the existing `core.ReportDef`) and render to PDF/HTML; keep the `.lst` only for the archive. |
| 11 | `cobol\batch\ap\APEINV.cob` | UBL e-invoices parsed by `INSPECT`/`UNSTRING` tag scanning rather than a real XML parser. | Bolted on in 2019 to a program that had no XML support; "just find the tags". | Parse with a schema-validating XML/UBL library in a sidecar service and hand COBOL a flat, validated record. |
| 12 | `cobol\batch\ap\APBULK.cob` | Bulk load reads a positional flat file whose layout is described only in the program comments. | The interface was agreed over the phone with a supplier in 1997. | Publish a versioned schema (CSV header or Avro/JSON) and validate on arrival; reject with a machine-readable error file. |
| 13 | AP-wide | Amounts are `COMP-3` `S9(13)V99`, quantities `S9(11)V9(6)`, FX `S9(9)V9(8)`. | Packed decimal was the only exact money type. | Keep exact decimals (`decimal(19,4)` / `decimal(19,6)`) – do **not** "modernise" to floating point. The lesson is to carry the scale explicitly. |
| 14 | AP-wide | Dates are `PIC 9(8)` `yyyymmdd` integers and arithmetic goes through `DATECALC`. | Y2K remediation in 1999 expanded 6-digit dates to 8 rather than introducing a date type. | Use a real date type end to end; `DATECALC` becomes a business-calendar service (working days, Bacs processing days). |
| 15 | `cobol\online\ap\APPAYPRP.cob` – payment routing thresholds | The GBP 250,000 CHAPS threshold and the FPS limit are read from `core.Parameter` but the *rule shape* (three methods, one threshold each) is hard-coded in `4400-ROUTE-METHOD`. | The rule has only ever changed by moving the number. | Express routing as data (ordered rules: condition → method) so a new rail does not need a code change. |
| 16 | `cobol\online\ap\APSUPVAL.cob` – hold reasons | Hold reasons are a fixed set of 88-levels rather than a reference table. | The list was "obviously" fixed in 1991. | Move to a reference table with effective dates, so reasons can be added without a recompile. |
| 17 | `ap.PurchaseInvoice` carries both document currency and GBP columns | Every amount is stored twice. | The 1991 ledger was sterling-only; multi-currency was added by duplicating columns rather than normalising. | Store the document amount plus the rate, and derive the base amount in a view, so a rate correction cannot leave the two out of step. |
| 18 | `cobol\online\ap\APPOST.cob` | Posting goes through `fin.JournalStage` + `fin.usp_PostJournal` but the event names are string literals in COBOL. | Event names were copied from a printed list. | Generate the event constants from `fin.EventMap` (the seed in `db\seed\62-ap-eventmap.sql`) so the two cannot drift. |
| 19 | AP-wide | No optimistic-concurrency token on `ap.PurchaseInvoice`; last write wins outside the reservation path. | Screens were single-user per invoice in CICS. | Add a `rowversion` and surface conflicts to the user. |
| 20 | `cobol\tests\ap\*` | Tests clean up with `DELETE ... WHERE CorrelationId LIKE 'ZZAPTEST%'` against the live schema. | There was no test database; tests ran against a copy of production. | Use transactional fixtures or a disposable container database per test run. |
| 21 | AP-wide – every ``INSERT`` that reads ``SCOPE_IDENTITY()`` back (`APINVREG`, `APALLOC`, `APCRDLNK`, `APLATEPY`, `APPAYPRP`, `APSTMREC`, `APVATEVD`) | The statement must start with ``SET NOCOUNT ON;`` or the ODBC shim fetches the row-count result set instead of the identity and fails with "Invalid cursor state". | `SQLIO` exposes one statement handle per cursor number and fetches the *first* result set; nobody wrote the rule down, so it was rediscovered by `TAPINVREG`. | Return the key with ``OUTPUT INSERTED.<Id>`` (one result set, no row-count trap), or move to a data-access layer that reads the generated key properly. |
| 22 | `cobol\online\ap\APINVREG.cob` – SQLIO cursor 1 | Cursor 1 is shared by the duplicate probe, the header insert and `GET`, and each read has to be closed by hand with ``CLOS`` before the next statement reuses it. | Cursor numbers were a scarce, statically allocated resource in the 1991 CICS program. | Scope a statement handle to the unit of work (``using``/``try-with-resources``) so a leaked cursor is impossible. |
| 23 | `cobol\online\ap\APMATCH.cob` – `MT-TOL-QTY-PCT` | The quantity tolerance is accepted in the COMMAREA and documented, but never applied: the matchable quantity is strictly the accepted receipt quantity (AC-030). | The percentage was added to the copybook for a 1990s over-delivery policy that was never switched on. | Either implement it as an explicit, approved over-delivery allowance or delete the field; `AP.TOL.QTYPCT` is seeded at zero in `db\seed\62-ap-eventmap.sql` so the current behaviour is at least visible. |

24. **`cobol\batch\ap\APEINV.cob` - the inbox is listed by shelling out.** This dialect has no
    directory read, so `APEINV` asks the shell for `dir /b` into `run\work\APEINV-INBOX.txt` and
    reads that, then moves each file with `move /Y`. *Why:* the 1998 EDIFACT translator did exactly
    this and nobody has had a reason to change it. *Modernisation:* a watcher or queue trigger on
    the access point drop, with the file list and the move handled by the platform.
25. **`cobol\online\ap\APXMLGET.cob` - a record scanner, not an XML parser.** It assumes the
    flat one-element-per-line layout our access point delivers and keeps a twelve deep container
    stack in the caller. A document with several elements on one line needs the `FIND` function.
    *Modernisation:* schema-validated XML deserialisation.
26. **`cobol\online\ap\AP6MDATE.cob` - VAT periods are calendar quarters.** `SM-PERIOD-KEY` is
    derived as `YYYYQn` from the date rather than read from the finance period table, so a stagger
    group would be wrong. *Modernisation:* look the return period up in `fin`.
27. **`ASSIGN TO DYNAMIC` is required in the `ibm` dialect.** Every AP batch program had
    `ASSIGN TO WS-SPOOL-NAME`, which the compiler treats as the *external name* `WS-SPOOL-NAME`:
    the reports were silently written to a file called `NAME` in the working directory. All six AP
    batch programs now say `ASSIGN TO DYNAMIC`. *Modernisation:* one shared SELECT copybook.

28. **SQLIO has no `OPEN` function.** Five AP batch programs opened their cursors with
    `MOVE "OPEN" TO SQLIO-FUNC` and abended with `INVALID FUNCTION OPEN`. A cursor is opened with
    `EXEC` and then driven with `FTCH`/`CLOS` (see `SQLIOCA`). *Modernisation:* a cursor open/fetch
    wrapper so the function codes cannot be got wrong.
29. **SQLIO cursor numbers are a global register, 1 to 8.** `PARMGET`, `PERIODCK` and `PERMCHK` all
    use cursor 6, `NUMGEN` and `IDEMPOT` cursor 7, `JOBLOG`, `AUDITLOG` and `EXCPRAIS` cursor 8, and
    `APPOST` cursors 1 and 5. A batch program that calls one of those inside its own fetch loop
    silently loses its result set, and `APBACS` used cursor 9, which does not exist. AP batch now
    keeps its own loops on cursors 2 to 4. *Modernisation:* per-statement handles, not a numbered
    register.
30. **`ap.vw_BacsInstructionAll`.** A second view that differs from `ap.vw_BacsInstruction` only by
    the missing status filter, so `APBACS` can reproduce the file of a released run. *Modernisation:*
    one view with the filter as a parameter, or a table function.

## Procedure arguments may not be expressions (APPOST)

`APPOST.cob` built `EXEC fin.usp_PostJournal ?, ?, ?, ?, CAST(? AS date), ...`. T-SQL refuses an
expression as a procedure argument, so every AP posting failed with *Incorrect syntax*;
`CONVERT(date, ?, 112)` fails for the same reason. The dates now go across as plain `YYYYMMDD`
parameters and SQL Server converts them (the unseparated ISO form is language independent).
`CAST(? AS date)` inside an ordinary INSERT/SELECT is legal and is left alone.
*Modernisation hint:* bind typed parameters in the data access layer instead of pasting casts into
statement text.

## The register COMMAREA is reused across documents

`APEINV` reuses one `APINVREGCA` area for every file in the run. `IR-INVOICE-ID` is an
in-out field (`APINVREG` 2100-EXACT excludes it so an amendment does not find itself), so a value
left over from the previous document silently switched the duplicate check off. The area is now
cleared per document. *Modernisation hint:* a per-call request object instead of a shared COMMAREA.
