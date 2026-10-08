# Kit maintenance and execution (`kit`)

Specification: `09-Kit-Maintenance-and-Execution.md` (KIT-001..KIT-024), acceptance scenarios
AC-076..AC-084, decisions **D-10** (yield/scrap/run/batch), **D-11** (commercial kit pricing, VAT,
substitution, returns), **D-12** (returns, scrap, disassembly), **D-20** (backflush for non-lot and
non-serial components only).

Dialect: `default`, **free format** (`>>SOURCE FORMAT IS FREE`), 2014/2016 rewrite style.
Schema `kit` (tables already exist – see `docs\DATA-MODEL.md` §7).

---

## 1. The five meanings of "kit"

The module refuses to let the word hide five different economic processes (KIT-001). Every
program takes the kind into account and `KITCREAT KINDCHK` answers the only question that
really matters – *may this parent ever be received into stock?*

| Kind | Parent stock | Completion | Disassembly |
|---|---|---|---|
| `COMMERCIAL` | never | never | never |
| `STOCKASSY` | yes | yes | yes |
| `EXECUTION` | not an extra balance | parent only | n/a |
| `PHANTOM` | never | never | never |
| `TRIAL` | nothing is committed at all | never | never |

## 2. Programs

| Program | Status | Function codes | Requirements |
|---|---|---|---|
| `KITQTY` | **done** | `LINEQTY`, `REQCHG`, `SCALE`, `ACCOUNT` | KIT-003, KIT-005, KIT-011, KIT-016 |
| `KITDB` | **done** (internal, 4 parameters – not LINKable) | `HDRGET/HDRINS/HDRSTAT/HDRQTY/HDRFRZ/LINOPEN/LINNEXT/LINCLOS/LINGET/LININS/LINSAVE/ITEMGET/ISSINS/GENINS/PCKINS/PCKUSE/SUPINS/SUPSUM/SUPFREE` | KIT-002, KIT-024 |
| `KITCREAT` | **done** | `CREATE`, `REVIEW`, `RELEASE`, `ENQUIRE`, `GETLINE`, `KINDCHK` | KIT-001, KIT-002, KIT-004, KIT-024 |
| `KITAMEND` | **done** | `IMPACT`, `AMEND`, `SCALE`, `ADDLINE`, `CANCELLN`, `SUBST` | KIT-003, KIT-005, KIT-012 |
| `KITISSUE` | **done** | `ISSUE`, `SUBST`, `CHECK`, `BACKFLSH` | KIT-010, KIT-011, KIT-012 |
| `KITTRIAL` | **done** | `TRIAL`, `MAXBUILD`, `GETLINE`, `SAVE` | KIT-006 |
| `MULTITRL` | **done** | `CLEAR`, `ADDKIT`, `RUN`, `GETKIT`, `GETCOMP` | KIT-007 |
| `KITRESV` | **done** | `RESERVE`, `SUPPLY`, `RELEASE`, `REVALID`, `PICKLIST`, `PICK`, `SHORTPCK`, `UNPICK` | KIT-008, KIT-009, KIT-022 |
| `KITRET` | **done** | `RETURN`, `RETURNCK`, `LISTISS` | KIT-013 |
| `KITSCRAP` | **done** | `SCRAP`, `SCRAPCK` | KIT-014 |
| `KITCOMPL` | **done** | `COMPCHK`, `COMPLETE`, `FINAL` | KIT-015, KIT-016, KIT-019 |
| `KITCLOSE` | **done** | `CLOSECK`, `CLOSE`, `FINCLOSE`, `CANCEL`, `REOPEN`, `HISTORY` | KIT-017, KIT-018, KIT-024 |
| `GENEALOG` | **done** | `FORWARD`, `BACKWARD`, `WHEREUSD` | KIT-019 |
| `KITCOST` | **done** | `COST`, `GETELEM`, `VARIANCE` | KIT-020 |
| `DISASSY` | **done** | `CREATE`, `ADDLINE`, `VALUE`, `APPROVE`, `POST`, `ENQUIRE` | KIT-021 |
| `KITCOMM` | **done** | `SETS`, `ALLOCATE`, `FULFIL`, `RETURNCK` | KIT-001, KIT-023 |

### 2.1 Batch, procedures, views and seeds

| Artefact | What it is |
|---|---|
| `cobol\batch\kit\KITRDY.cob` | RPT-013. `MODE=READY\|EXEC\|SHORT` – kit readiness to pick, execution position, shortage list. Report Writer, 132 columns, spool file through `RUNPATH`, run logged by `JOBLOG`. |
| `cobol\batch\kit\KITDOCP.cob` | RPT-014, KIT-022. The kit picking document for **one** kit (`KIT=`), `MODE=ORIGINAL\|REPRINT` with `COPY=` and `REASON=`. A reprint is stamped three times – document kind, copy line and footing band – because a second picking list on the shop floor is how material gets issued twice. The program reads and prints only: it never writes `kit.KitIssue`, `kit.KitPick`, `inv` or the ledger. |
| `cobol\batch\kit\KITDEMO.cob` | The demonstration driver. `CO= SITE= MODE=PREVIEW\|COMMIT USER=` (plus `ASAT=` and `QTY=`), picked up automatically by `build\demo-data.ps1`. It drives the whole define-to-assemble flow through the online services – see §2.3. |
| `db\procs\kit\usp_KitEnquiry.sql` | `kit.usp_KitReadiness`, `kit.usp_TrialAllocate`, `kit.usp_KitGenealogy`, `kit.usp_KitCostSummary`. |
| `db\views\kit\vw_Kit.sql` | `kit.vw_KitExecution`, `kit.vw_KitShortage`, `kit.vw_KitGenealogyLine`. |
| `db\seed\85-kit-eventmap.sql` | Business events `KIT.ISSUE` / `RETURN` / `SCRAP` / `ABSORB` / `COMPLETE` / `DISASSY`, and number series `KIT` and `DIS`. |
| `db\seed\85-kit-menu.sql` | Kit menu and `core.ReportDef` rows `KITRP010/020/030` (→ `KITRDY`, one per mode) and `KITRP040` (→ `KITDOCP`). |
| `db\seed\86-kit-maintdef.sql` | `core.MaintDef` rows `KIT.DEF` and `KIT.DISASSY` for `frmGenericMaint`. |
| `db\seed\95-kit-demo.sql` | Six demo kits across both companies. |

Running the reports (from the application root, so that `run\spool` resolves):

```
bin\x86\KITRDY.exe  CO=STC MODE=READY USER=STORES
bin\x86\KITDOCP.exe CO=STC KIT=KIT0001001 USER=STORES
bin\x86\KITDOCP.exe CO=STC KIT=KIT0001001 MODE=REPRINT COPY=3 REASON=RE-PICK USER=STORES
```

### 2.2 Tests

| Suite | Covers |
|---|---|
| `cobol\tests\kit\TKITDEMO.cob` | What the `KITDEMO` driver leaves behind (KIT-015, KIT-019, AC-083): the demonstration kit was issued, its output was received with a stock movement behind it, the completion carries a genealogy, no genealogy line floats free of its completion, nothing was consumed in quantity zero, each completion has a correlation id of its own, and `GENEALOG FORWARD` reads the kit back. 11 assertions; read only, and it skips (never fails) when no `KITDEMO` run is on file. |
| `cobol\tests\kit\TKITQTY.cob` | The quantity rules, AC-077. 22 assertions, database free. |
| `cobol\tests\kit\TKITTRL.cob` | Trial kitting and priority sequencing, AC-078. 19 assertions, database free. |
| `cobol\tests\kit\TKITMTRL.cob` | `MULTITRL` and `KITTRIAL` against the real showcase bills (AC-078, KIT-006, KIT-007): the bicycle pools more than 25 components and `GETCOMP` pages them all, no service line enters the pool, every pool entry balances (allocated + shortage = demand, allocated ≤ free), a component repeated within one kit is one claim, priority decides who is served when two kits compete, an incomplete bill is `INCOMPLETE`, and a single trial of the cake with no ingredients in stock is infeasible with a limiting item. 27 assertions; read only, and it skips when the showcase bills are absent. |
| `cobol\tests\kit\TKITCST.cob` | `KITCOST`: the seven values and the residual identity, scrap not deducted (KIT-014), and the AC-081 subassembly-charged-once check in both directions. 27 assertions; builds its own kit inside a transaction that is always rolled back. |
| `cobol\tests\kit\TKITEXEC.cob`, `TKITCLS.cob` | Execution and closure. |
| `cobol\tests\kit\TKITDIS.cob` | `DISASSY` (KIT-021, AC-084): the D-12 value rule – carrying 100.00 plus approved cost 10.00 against recovery 95.00 leaves a loss of 15.00 and `DS-BALANCED = "Y"`; recovery above 110.00 is refused with `KIT0019`; the allocated line values (71.24 / 11.88 / 11.88) add back to the recovery exactly with the rounding penny on the largest line; and the database-free refusals `KIT0002`, `KIT0003`, `KIT0008`. 46 assertions; builds a stocked parent in `inv.Item` inside a transaction that is always rolled back. |
| `cobol\tests\kit\TKITCOM.cob` | `KITCOMM` (KIT-001, KIT-023, D-11): `ALLOCATE` spreads `KV-PARENT-NET` by relative standalone selling price (66.66 / 16.67 / 16.67 of 100.00) so `KV-ALLOC-TOTAL` equals `KV-PARENT-NET` exactly, the residual going to the largest line and `KV-RECONCILED = "Y"`; `KIT0028` when no component carries an SSP; `RETURNCK` credits the allocated component value and never creates parent stock; `SETS` and `FULFIL` refuse a partial set. 41 assertions; builds its own kit definition inside a transaction that is always rolled back. |
| `cobol\tests\kit\TKITBFL.cob` | `KITISSUE BACKFLSH` (KIT-010, KIT-011, CR2742): a kit whose two untraced components sit in different bins is flushed with **no** warehouse or location in the commarea - each line is sourced from its own eligible balance (two distinct `kit.KitIssue` locations, each matching a balance of that line's item); and a flush after a manual issue takes only what the line still owes, so `GrossIssuedQty` stops at the requirement instead of adding usage x output again; and each flushed line is valued at a cost the item holds at the site, with the stock movement carrying the same value (CR2744). 14 assertions; builds its own kit, stock and bins inside a transaction that is always rolled back. |
| `db\tests\kit-enquiry.sql`, `kit-costing.sql`, `kit-views.sql` | The procedures and views, 46 assertions inside `BEGIN TRAN … ROLLBACK`. |
| `dotnet\STERP.Tests\Kits\KitTests.cs` | The VB facades and the menu/permission wiring. |

Every COMMAREA copybook listed above **is already written and compiles**, so other teams can code
against the interfaces now. Program names are 8 characters because of CONVENTIONS §3.1, so the
brief's `KITCREATE` is the program `KITCREAT` and `MULTITRIAL` is `MULTITRL`.

### 2.1 Execution flow, and the three states that are deliberately separate

```
KITCREAT CREATE -> REVIEW -> RELEASE      (freezes revisions, units, assumptions)
KITTRIAL / MULTITRL                        (feasibility only - never commits)
KITRESV  RESERVE | SUPPLY                  (cover: free stock, or a named inbound)
KITRESV  PICKLIST -> PICK                  (stock spoken for, then off the shelf)
KITISSUE ISSUE / BACKFLSH                  (stock becomes work in progress)
KITRET   RETURN      KITSCRAP SCRAP        (dispositions for what is left)
KITCOMPL COMPLETE / FINAL                  (output received, genealogy, WIP transfer)
KITCLOSE CLOSE -> FINCLOSE | CANCEL | REOPEN
```

### 2.3 The demonstration driver (`KITDEMO`)

```
bin\x86\KITDEMO.exe CO=STC SITE=BHM MODE=COMMIT USER=FINCTRL [ASAT=yyyymmdd] [QTY=2]
```

`build\demo-data.ps1` runs every `bin\x86\*DEMO.exe` with `CO= SITE= MODE= USER=`, so the kit
driver needs no entry in that script. What it does, in order:

1. reads the released standard bills whose components have eligible stock (`ACCEPTED`, available,
   owned) and keeps the ten best candidates, the requested site first;
2. `KITTRIAL TRIAL` on each candidate until one can be built today – a second scenario at the top
   level is recorded when the subassembly view builds nothing (KIT-006, never an overwrite);
3. (`COMMIT` only – a preview takes the first feasible candidate and stops here)
   `KITCREAT CREATE` for the first candidate that produces a released requirement, then `REVIEW`,
   then `RELEASE` asked for by the engineering manager (the `KIT.EXEC.RELEASE` permission is not
   seeded, so this answers `06`/`KIT0009` – the control working, recorded as a hold);
4. `KITRESV RESERVE` for the whole kit, `PICKLIST`, then `PICK` per line from the bin the component
   is really in;
5. `KITISSUE ISSUE` per picked line, then a `BACKFLSH` pass – refused with `KIT0013` because the
   demonstration kit is not flagged for backflush (D-20: backflush is never implicit);
6. `KITCOMPL COMPCHK`, then `COMPLETE` for all but the last unit and `FINAL` for the rest, posting
   `KIT.COMPLETE` through `fin.JournalStage`; a completion that cannot reach the ledger is repeated
   with `KM-COST-BASIS = "NOGL"` on a fresh id rather than abandoned;
7. `GENEALOG FORWARD`, `KITCOST COST`, and one `DISASSY CREATE → ADDLINE → VALUE → APPROVE → POST`.

Rules it keeps:

* **Segregation of duties.** `USER=` is the operator (the dataset is loaded by the financial
  controller) and is used for `JOBLOG` and the listing heading only. The actors are `ENGINEER`
  (trial, create), `ENGMGR` (release), `WHSE` (reserve, pick, issue, complete), `INVMGR` (named
  approver of the completion, raiser of the disassembly), `QUALITY` (approves the disassembly,
  reads genealogy) and `COSTACC` (kit cost).
* **One correlation id per call** (COM-015). Every call takes a fresh id derived from the 24
  character stem `KITDEMO-<CO>-<CCYYMMDD>`, so `KITISSUE BACKFLSH` and `DISASSY POST`, which cut the
  caller's id to 30 characters and append their own per-line suffix (CR2741), still fit inside the
  36 character field. No id is ever handed to two idempotent services.
* **Re-runnable.** A kit whose `SourceDemandRef` or `CorrelationId` carries the run date's tag means
  the day's demonstration is already on file: the driver says so and returns `0` without issuing,
  receiving or posting anything. Two consecutive runs both return `0`.
* **`MODE=PREVIEW`** stops after the trial – the only service that commits nothing at all.
* Steps the dataset can live without (the release hold, a pick, the backflush refusal, the
  disassembly) are recorded as warnings; a refusal in the main chain sets `RETURN-CODE 4`.
* The listing is `run\spool\KITDEMO-<CO>-<stamp>.lst`: one line per service call with the actor,
  the function, the reference, the quantity and the outcome.

`RESERVED`, `PICKED` and `ISSUED` are three different things and `KITRESV` keeps them apart:a reservation has not moved, a pick has left the shelf but is still stock and still revalidated,
and only an issue is work in progress. `KITISSUE` consumes the reservation through
`STKRESV CONSUME` so the kit line and `inv` can never drift apart.

A fourth state, **covered by future supply**, is KIT-008's other half. `KITRESV SUPPLY` pins a
line to an explicitly identified inbound (`KR-SUPPLY-TYPE` `PURCHASE`/`WORKSORDER`/`KIT`/
`TRANSFER`, `KR-SUPPLY-REF`, `KR-SUPPLY-LINE`, `KR-SUPPLY-DUE`) and writes `kit.KitSupplyCommit`.
Nothing in `inv` moves, no stock is reserved, and the line's outstanding requirement falls by the
committed quantity so that free stock is never reserved for a need a purchase order already
covers. Supply due after the line needs it is still cover, but it comes back as `04` + `KIT0010`
with `KR-SUPPLY-LATE = "Y"`. `RELEASE` releases the commitments with the reservations.

`PICK`/`SHORTPCK`/`UNPICK` write `kit.KitPick`, which is the only record of the lot or serial the
storeman actually took (an unpick is kept as a negative row rather than erased). `KITISSUE` then
spends those picks: after the issue is recorded it calls `KITDB PCKUSE`, which settles any
outstanding unpick against the oldest open picks and consumes the rest of the line's picks in
pick order, so `kit.KitPick.ConsumedQty` always says how much of each pick has gone into the kit.
Whatever is left unconsumed is material that is physically out of the store, which is exactly
what `KITCLOSE CANCEL` has to show the storeman; an issue with no pick trail consumes nothing and
is not an error. `KITCOMPL`
reads it back: each completion draws from the issues in the order they were made, taking only
what earlier completions left, so genealogy names the lot that is really in the unit instead of a
proportion of everything issued (AC-083).

`KITCOMPL` posts the work-in-progress transfer through `fin.JournalStage` +
`fin.usp_PostJournal` under business event **`KIT.COMPLETE`** (debit stock 1120, credit work in
progress 1150). The two legs, including their analysis dimensions, are read straight out of
`fin.EventAccountMap`, so moving an account is a seed change in `db\seed\85-kit-eventmap.sql`
and not a recompile. The posting carries a correlation id of its own
(`KITCOMPL/<kit ref>/<completion id>`) because the caller's id has already been spent on the
stock receipt and the ledger would otherwise treat the journal as a repeat and post nothing.
Pass `KM-COST-BASIS = "NOGL"` to record a completion without a ledger entry (used by demo data
loads); any other value posts, and a completion that cannot post comes back `08` + `KIT0016`
with an exception raised - it never claims success quietly.

## 3. Interfaces the Sales team needs

### 3.1 `KITCREAT` – create a works kit for a stocked assembly order

```cobol
COPY KITCREATCA REPLACING ==DFHCOMMAREA== BY ==WS-KITCREAT-CA==
     LEADING ==CA-== BY ==SC-==.
...
MOVE "CREATE"     TO SC-FUNCTION
MOVE "STC"        TO SC-COMPANY
MOVE "RDG"        TO SC-SITE
MOVE "SALESREP"   TO SC-USER
MOVE 20261006     TO SC-BUSINESS-DATE
MOVE "STOCKASSY"  TO KC-KIT-KIND
MOVE "FRAME-A10"  TO KC-PARENT-ITEM
MOVE 10           TO KC-PLANNED-QTY
MOVE "SALESORDER" TO KC-SOURCE-TYPE
MOVE "SO0001234"  TO KC-SOURCE-REF
MOVE 20261106     TO KC-NEED-DATE
CALL "KITCREAT" USING WS-KITCREAT-CA
```

* `KC-KIT-REF` comes back with the allocated reference (`core.NumberSeries` code `KIT`) and
  `KC-EXEC-KIT-ID` with the surrogate key.
* `SC-RETURN-CODE` `08` + `KIT0004` means the bill of material is not complete; **no kit is
  created at all**, so Sales must not treat it as a partial success (KIT-002).
* `KC-SOURCE-TYPE = "SALESORDER"` requires `KC-SOURCE-REF`, otherwise `08` / `KIT0024`.
* `KINDCHK` sets `KC-MAY-RECEIVE`. A commercial kit parent always returns `"N"` – use `KITCOMM`
  for commercial kits, never `KITCOMPL`.
* `ENQUIRE` + `GETLINE` page the requirement 20 lines at a time (`KC-FROM-LINE`, `KC-WIN-MAX`,
  `KC-MORE-LINES`). The derived quantities in the window are recomputed through `KITQTY` on every
  read, so a screen can never show arithmetic that disagrees with the rule.

### 3.2 `KITTRIAL` – non-committing feasibility (COMMAREA `KITTRIALCA`)

```cobol
MOVE "TRIAL"     TO <hdr>-FUNCTION      *> or MAXBUILD / GETLINE / SAVE
MOVE "FRAME-A10" TO KT-PARENT-ITEM
MOVE 10          TO KT-REQUESTED-QTY
MOVE 20261106    TO KT-REQ-DATE
MOVE "N"         TO KT-USE-INBOUND
CALL "KITTRIAL" USING WS-KITTRIAL-CA
```

Guarantees Sales can rely on:

* `KT-COMMITTED` is **always `"N"`**. A trial changes no stock, reservation, supply order or
  ledger balance (KIT-006).
* `KT-MAX-BUILDABLE` satisfies *every* constraint after yield, fixed requirements, batch steps,
  rounding and supply netting – it is not a minimum of component ratios.
* `KT-LIMITING-ITEM` / `KT-LIMITING-SHORT` name the limiting component.
* Per line: gross, net requirement, free, own reservation, other reservation, expected inbound and
  date, allocated, immediate shortage and time-phased shortage date are all separate fields.
* Re-running with a different priority order must be recorded as a new `KT-SCENARIO-NO`, never as
  an overwrite of the first answer.
* Demand per component is the explosion's leaf requirement (`BX-L-REQ-QTY`); service, cost-only,
  non-stock and note lines need no stock and are not assessed. The availability cache holds 200
  distinct components – a bill with more is reported infeasible with a warning rather than
  assessed against the wrong item.

For several competing kits use `MULTITRL`, which allocates the shared stock **once** in priority
order, so two kits can never both be reported complete on the same units (KIT-007). Each kit's
explosion is read in full (`GETLINE` from line 1 until exhausted) and only stocked components enter
the shared pool. The pool holds 200 components; when it is full the run stops with `KIT0025` and
verdict `ERROR` instead of dropping demand. A kit whose bill cannot be fully exploded gets verdict
`INCOMPLETE`, is counted as short and is allocated nothing. A component counts as one claim per
kit, however many times it appears in that kit's bill.

## 4. Rules that are implemented, and where

| Rule | Where |
|---|---|
| `net = gross − returns`, `remaining = max(0, req − net)`, `excess = max(0, net − req)` | `KITQTY LINEQTY` |
| Scrap is a disposition, not a return, and does not raise the requirement | `KITQTY`, `KITSCRAP` |
| Reducing a requirement shows excess and needs authority; it never rewrites an issue | `KITQTY REQCHG`, `KITAMEND AMEND` |
| An issued line can never be deleted | `KITAMEND CANCELLN` → `08` / `KIT0015` |
| Release freezes bill/item revisions, units, yield, quantum, batch plan, selection date | `KITCREAT RELEASE` → `KITDB HDRFRZ` |
| Proportional scales, fixed-per-run does not repeat, fixed-per-batch steps | `KITQTY SCALE`, `KITAMEND SCALE` |
| Backflush only for non-lot/non-serial components, on an explicitly flagged kit | `KITISSUE BACKFLSH` → `KIT0013` |
| A combined backflush cannot partially succeed | `KITISSUE` runs a full `CHECK` pass before posting anything |
| Overissue needs reason + approval, and still cannot create physical stock | `KITISSUE` → `KIT0008`, `KIT0009`, `KIT0010` |
| Substitution is kit-specific and never edits the standard bill | `KITAMEND SUBST`, `KITISSUE SUBST` |
| Customer-specified component needs written customer agreement (D-11) | `KITAMEND SUBST` → `KIT0014` |

## 5. Reason codes

`KIT0001`..`KIT0029` are catalogued in `cobol\copy\KITWORK.cpy`. Return codes follow the house
convention: `00` ok, `04` warning, `06` hold (needs authority), `08` error, `12` severe, `16` not
found. An excess is a **warning** (`04` / `KIT0022`), a missing approval is a **hold**
(`06` / `KIT0009`) and a shortage is an **error** (`08` / `KIT0010`) – they stay distinguishable
(COM-017).

## 6. How to demo (today)

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-cobol.ps1 -Module kit
powershell -NoProfile -ExecutionPolicy Bypass -File build\run-tests.ps1 -Layer cobol -Filter TKITQTY
```

`TKITQTY` runs the specification's own worked numbers: require ten, issue seven, return two →
net 5 / remaining 5; reduce the approved requirement to four → excess 1, remaining 0, net still 5
and a `06` hold for authority (AC-077). It also proves AC-081's fixed-per-run rule, AC-087's
zero-output rule and the KIT-017 accountability identity.

## 7. Still to build

Nothing planned is outstanding: every program, batch job, form and test listed above is built and
green. Deliberate legacy texture and known modernisation targets are in `docs\tech-debt\kit.md`.

## Backflush: where the material comes from, and how much (CR2742, CR2743)

`KITISSUE BACKFLSH` used to take `KS-SITE` / `KS-WAREHOUSE` / `KS-LOCATION` from the commarea and
apply them to every component, so a kit whose components live in two bins could not be flushed at
all: no location gave `STK0002` from `STKMOVE`, and one location gave a shortage on the other
component. Each line is now sourced from **its own eligible balance** through `AVAILQ ENQUIRE` -
the same eligibility test `KITRESV` reserves against (quality, expiry, location availability,
ownership) - and the caller's site and warehouse only narrow the search. The line's own bin wins
when it can carry the issue; otherwise the eligible bin with the most free stock does. Traced
balances are never candidates (D-20), and the chosen balance's **revision** comes with it, because
`STKMOVE` fills a blank revision from the item master and would otherwise post against a balance
that does not exist. Finding nothing leaves the scope alone and `KIT0010` reports the shortage.

The quantity is capped by what the line still owes: `usage per unit x good output`, limited to
`CurrentRequirementQty - NetIssuedQty`. Material the stores issued by hand (or an earlier flush)
is already in work in progress, so flushing it again was an over-issue.

The pass also lists its open lines and closes the cursor **before** anything is posted. Issuing a
line changes its status, which takes the row out of the "only open" cursor the pass was reading,
and the next fetch ended the loop with the rest of the kit unflushed.

`KITCOMPL` reports `KM-TOTAL-GOOD` including the receipt being recorded, so a `COMPCHK` caller can
tell whether the run would be finished by it; `KM-REMAINING-OUT` stays the output the kit may
*still* receive. A partial completion answers `KIT0029` (RC 4) - `KIT0018` (RC 8) now means only
that the completion exceeds the planned good output.

`KITCREAT` stores `CA-CORRELATION-ID` on `kit.ExecutionKit.CorrelationId` (`KITDB HDRINS`), so
every row a run leaves behind is traceable to the instruction that asked for it (COM-015).

## Issues are valued at the item's cost (CR2744)

`STKMOVE` never derives a cost - it values a movement only when the caller passes `SM-UNIT-COST`.
`KITISSUE` passed none, so every kit issue was posted at nil: the `inv.StockMovement` row had no
value, `kit.KitIssue.UnitCostGBP` was 0.00, the kit carried no work in progress and the completion
receipt (valued by `KITCOMPL` from that WIP) came out at zero too - which is why `KITCOST` answered
zeros on the `KITDEMO` kit.

`KITISSUE Price-The-Issue` now reads the item through `KITDB ITEMGET` at the issuing site just
before `STKMOVE` and passes the standard cost for a standard-costed item, otherwise the moving
average (the basis `DESPATCH` uses for a cost of sale). `ITEMGET` itself now prefers the cost row of
`KD-SITE` when the caller names one, then the company-wide `*` row, and the item's current revision
within a site; with `KD-SITE` blank it reads only the `*` row, exactly as before. It also returns the
cost method in the new `KI-COST-METHOD` (appended to `KIT-ITEM-REC` in `KITWORK.cpy`, so the whole
kit module must be rebuilt together). `inv.usp_PostStockMovement` only records the value; it does
not move `cst.ItemCost`, so nothing is counted twice. Proved by `TKITBFL` ("Every flushed line is
valued at the item's own cost").

Issues still post no journal: only `KITCOMPL` reaches the ledger, and the kit event map names the
NRV allowance (1150) as work in progress. Both are recorded in `docs\tech-debt\kit.md` ("Only the
completion reaches the ledger").
