# BOM - Bills of Material and nested calculations

Family **BOM-001..BOM-032**, spec `10-Bills-of-Materials-and-Nested-Calculations.md`
(worked examples B1-B6), schema `bom`, COBOL dialect `default` (free format), module code `bom`.

The engine is COBOL. The database holds the structures and the retained results; stored procedures
cover only the set operations (release gate, where-used, revision comparison) and the views the
screens read.

---

## 1. Programs

### Online (LINKable, `cobol\online\bom`)

| Program | Copybook | Functions | Purpose |
|---|---|---|---|
| `BOMMATH`  | `BOMMATHCA`  | `GROSS`, `SCALE`, `SCRAP`, `ROUND`, `CONVERT` | the arithmetic of one occurrence: nominal basis scaling, yield, scrap allowance, unit conversion, rounding at the declared boundary (BOM-006..BOM-011) |
| `BOMWORK`  | `BOMWKCTL`   | `INIT`, `ADD`, `GET`, `SORT`, `CLEAR` | the occurrence pool every view reads; one explosion, many readings |
| `BOMREAD`  | `BOMBILL`    | `HEADER`, `LINES`, `ITEM` | reads `bom.BomHeader` / `bom.BomComponent` / `inv.Item` through SQLIO |
| `BOMSELCT` | `BOMSELCTCA` | `SELECT`, `LIST` | deterministic revision/component selection for company/site/purpose/variant and effective date (BOM-005) |
| `CYCLECHK` | (in `BOMVALIDCA`) | `CHECK` | direct and indirect cycle detection over every permitted configuration (BOM-013) |
| `BOMVALID` | `BOMVALIDCA` | `VALIDATE`, `CHECK` | complete validation; writes `bom.BomValidation` + findings, the evidence release needs (BOM-004) |
| `BOMEXPL`  | `BOMEXPLCA`  | `EXPLODE`, `NODE`, `GETLINE`, `LEAFQTY`, `STORE` | **recursive nested explosion** (BOM-012..BOM-019) |
| `BOMLEAF`  | `BOMLEAFCA`  | `SINGLE`, `INDENT`, `FLATTEN`, `ITEMQTY` | the three enquiry shapes of one calculation (BOM-014, BOM-015) |
| `BOMNET`   | `BOMNETCA`   | `NET`, `MAXBUILD`, `GETLINE` | netted planning, stock consumed once, maximum buildable (BOM-020) |
| `BOMCOST`  | `BOMCOSTCA`  | `COST`, `BRANCH`, `GETLINE` | costed explosion, one value boundary per branch (BOM-021, BOM-026) |
| `BOMSUPPL` | `BOMSUPCT`   | `LOAD`, `TAKE` | eligible supply pool used by `BOMNET` |
| `WHEREUSD` | `WHEREUSDCA` | `DIRECT`, `ALL`, `TOPLEVEL`, `OPENUSE`, `GETLINE` | where-used traversal and open demand (BOM-023) |
| `BOMCOMP`  | `BOMCOMPCA`  | `COMPARE`, `BYREF`, `IMPACT`, `GETLINE` | revision comparison with cost impact (BOM-025) |
| `BOMJOB`   | `BOMJOBCA`   | `DERIVE`, `NEWREV`, `LINEAGE` | job/order bill derivation with complete lineage (BOM-022, BOM-003) |
| `ECOIMPCT` | `ECOIMPCTCA` | `ASSESS`, `GETLINE`, `DECIDE`, `DECIDEALL`, `CHECK` | engineering change impact: every affected kit, demand, job bill and parent bill needs an explicit adopt/retain/replan decision before activation (BOM-031) |
| `BOMBULK`  | `BOMBULKCA`  | `PREVIEW`, `APPLY`, `GETLINE` (`COMMIT`/`RESULT` accepted as the 2016 spellings) | **bulk component change** across many bills - `REPLACE`, `ADD`, `REMOVE`, `QTYCHG` - with a complete preview, whole-bill validation of every proposal and an all-or-nothing apply (BOM-018, BOM-024, BOM-030) |
| `BOMBREV`  | `BOMBREVC` (not LINKable) | `PROPOSE` | builds **one** proposed revision for one bulk-change target and validates it; owns no transaction, so only `BOMBULK` may call it (BOM-004, BOM-013, BOM-024) |

#### `BOMBULK` - bulk component change (`BOMBULKCA`)

| Field | Meaning |
|---|---|
| `BB-CHANGE-REF` | the engineering change reference; also the idempotency key of `APPLY` (BOM-030) |
| `BB-ACTION` | `REPLACE` (swap `BB-OLD-ITEM` for `BB-NEW-ITEM`), `ADD`, `REMOVE`, `QTYCHG` (quantity / scrap / unit / basis) |
| `BB-SCOPE` | `ITEM` every bill that uses `BB-OLD-ITEM`, `PARENT` every bill of `BB-SCOPE-ITEM`, `BOMREF` the named bill reference |
| `BB-TARGET-CNT` / `BB-APPLIED-CNT` / `BB-FAILED-CNT` / `BB-SKIPPED-CNT` | per-run outcome counts; `BB-NEWREV-CNT` and `BB-AMEND-CNT` split the changed ones |
| `BB-MAY-APPLY` / `BB-REFUSAL` | whether the previewed change may be applied, and why not |
| `BB-LINES(1..25)` | per-bill before/after: source revision, proposed revision, treatment (`NEWREV`/`AMEND`), line, old and new quantity, outcome (`PENDING`/`APPLIED`/`FAILED`/`SKIPPED`), reason and detail |

`PREVIEW` and `APPLY` run identical code inside one `BEGN`…`COMT`/`ROLB`: every proposed revision is
really built and really validated (`BOMVALID`, plus `CYCLECHK` and a `WHEREUSD` ancestry test for a
replacement that would close a loop). A preview always rolls back; an apply rolls back too unless
every proposal passed. A `RELEASED` target therefore never changes - it gets the next free draft
revision beside it - and a `DRAFT` target is amended in place. The change set is written to
`bom.BomChange` / `bom.BomChangeLine` outside that transaction, so a refused run still leaves the
evidence of what was asked for; an applied run also raises `bom.BomChangeImpact` rows for the
released bills it touched, which is the hand-over to `ECOIMPCT` (BOM-031).

### Batch (`cobol\batch\bom`)

| Program | Run as | Output |
|---|---|---|
| `BOMRP010` | `BOMRP010 CO=STC ITEM=DEMO-FRAME QTY=9 ASAT=20261001 MODE=INDENT\|COSTED\|LEAF [COSTSET=STD2026] [LEVELS=n] USER=ENGINEER` | `run\spool\BOMRP010-<CO>-<stamp>.lst`, 132 columns, Report Writer |
| `BOMRP030` | `BOMRP030 CO=STC ITEM=DEMO-PIN ASAT=20261001 LEVELS=10 MODE=WHEREUSED [DRAFT=Y] USER=ENGINEER` | `run\spool\BOMRP030-<CO>-<stamp>.lst`, report id `BOMRP030` |
| `BOMRP030` | `BOMRP030 CO=STC BOMREF=BOM-DEMO-BDY FROMREV=A TOREV=B COSTSET=BOMDEMO [QTY=n] MODE=COMPARE USER=ENGINEER` | the same program and spool name, report id `BOMRP040` |

`BOMRP010` prints the structure indented by level with the usage, its basis, the stock unit, the
**unrounded** requirement beside the rounded issue quantity, and in `MODE=COSTED` the rate from the
stated cost set, the extended value, material/conversion/recovery totals and the unit cost. The
report never calculates anything itself - it asks `BOMLEAF`/`BOMCOST`, so a printed listing and the
enquiry screen cannot disagree (BOM-014, BOM-032).

`BOMRP030` is the engineering change pair, one program with two `DETAIL` layouts chosen by `MODE=`
(the same deliberate pattern as `BOMRP010`). `MODE=WHEREUSED` prints three sections - direct and
indirect parents with level, quantity per parent, bill/revision and the full path; the top level
assemblies each path ends at; and the open execution demand that needs a decision - counting
`POSSIBLE` uses apart from confirmed ones (BOM-023, BOM-028, BOM-031). `MODE=COMPARE` prints the
added, removed and changed components between two revisions with the quantity/unit/basis difference
and the cost impact, marking a pure resequence as `SEQUENCE` (BOM-025, BOM-029). Both modes ask
`WHEREUSD` and `BOMCOMP` and calculate nothing themselves; a refusal is also raised as a worklist
exception through `EXCPRAIS`.

### Tests (`cobol\tests\bom`)

| Test | Cases | Covers |
|---|---|---|
| `TBOMEXPL` | 44 | B1 structure, yield, scrap, fixed-per-run/batch, phantom, rounding, paths, stop reasons |
| `TBOMNET`  | 18 | B2 netted plan, **nine frames** maximum buildable, shared stock consumed once |
| `TBOMCOST` | 17 | B4/B5 costed roll-up, by-product recovery, missing-rate incompleteness |
| `TBOMCHG`  | 56 | `BOMBULK` preview of all seven bills that use the B1 pin, refusal on an unconfigured alternate group, cycle and unknown-item refusals, skipped targets, B3/B5 before-and-after quantities, released original intact after an apply, repeated apply idempotent |

Run: `build\run-tests.ps1 -Layer cobol -Filter TBOM` (135 cases).

---

## 2. COMMAREA contracts for other teams

> **Kits, Costing and Sales teams: these two interfaces are frozen.** Fill the header fields of
> `STCAHDR` (`CA-FUNCTION`, `CA-COMPANY`, `CA-SITE`, `CA-USER`, optionally `CA-CORRELATION-ID`),
> then `CALL`. `CA-RETURN-CODE` 00/04 = usable answer, 08+ = refusal with `CA-REASON-CODE` and
> `CA-MESSAGE`. From .NET use `NewArea("BOMEXPLCA")` + `Link("BOMEXPL", ca, "EXPLODE")`.

### 2.1 `BOMEXPL` - gross nested explosion (`BOMEXPLCA`)

Input (function `EXPLODE`):

| Field | Picture | Meaning |
|---|---|---|
| `BX-PARENT-ITEM` | `X(40)` | parent item code |
| `BX-PARENT-REV` | `X(4)` | parent revision, spaces = selected by effective date |
| `BX-PURPOSE` | `X(10)` | `STANDARD`, `JOB`, `ORDER` |
| `BX-JOB-REF` / `BX-VARIANT` | `X(20)` | job/order bill and variant selection (BOM-003, BOM-005) |
| `BX-REQ-QTY` | `S9(15)V9(6)` | required **good** parent output |
| `BX-EFF-DATE` | `9(8)` | yyyymmdd, the selection date |
| `BX-MAX-DEPTH` | `S9(4)` | display/stop depth limit (0 = engine maximum) |
| `BX-CONSOLIDATE` | `X` | `Y` only with an approved execution consolidation (BOM-019, D-10) |
| `BX-EXPAND-PHANTOM` | `X` | `Y` expands phantoms through to usable components (BOM-017) |
| `BX-INCLUDE-NOTES` / `BX-INCLUDE-BYPROD` | `X` | include note/service lines, include by-products |
| `BX-COST-SET` / `BX-CURRENCY` | `X(20)` / `X(3)` | only used by the costed path |
| `BX-SPLIT-CNT` / `BX-SPLITS` | table of 10 | split the requirement into separate runs (fixed-per-run evidence) |
| `BX-OPT-CNT` / `BX-OPTIONS` | table of 10 | chosen alternate per option group; unchosen groups make the result `INCOMPLETE` (BOM-018) |

Output: `BX-BOM-ID`, `BX-BOM-REF`, `BX-BOM-REV`, `BX-NOMINAL-BASIS`, `BX-YIELD`, `BX-QUANTUM`,
`BX-GROSS-STARTS`, `BX-RUN-COUNT`, `BX-BATCH-COUNT`, `BX-EXCESS`, `BX-OCC-COUNT`, `BX-LEAF-COUNT`,
`BX-DEPTH-REACHED`, `BX-MAX-BUILDABLE`, the cost summary, and
`BX-COMPLETENESS` (`COMPLETE` / `INCOMPLETE` / `ERROR`) with `BX-ERROR-PATH` / `BX-CYCLE-PATH`
(BOM-029). Paging: set `BX-FROM-LINE`, `BX-WIN-MAX` (≤ 25), `BX-VIEW`
(`ALL`/`LEAF`/`INTERMED`/`LEVEL1`/`INDENT`) and call `GETLINE` until `BX-MORE-LINES` is not `Y`.
Each `BX-LINES` entry carries depth, path, item, revision, node type (`LEAF`/`INTERMED`/`PHANTOM`/
`BYPRODUCT`/`NOTE`), supply behaviour, stop reason, basis, unit, usage, unrounded quantity, rounding
increment, required quantity, runs/batches, costs and any warning.

**The result stays in the pool until the next `EXPLODE`**, so paging a large tree never re-explodes
it, and a read-only repeat has no live effect (BOM-030). `STORE` writes the retained result to
`bom.BomExplosion` / `bom.BomExplosionLine` under `BX-CALC-REF` for reproducibility (BOM-032).

### 2.2 `BOMNET` - netted planning (`BOMNETCA`)

Input (function `NET`, or `MAXBUILD` for the largest whole-parent output):
`BN-PARENT-ITEM`, `BN-REQ-QTY`, `BN-EFF-DATE`, `BN-MAX-DEPTH`, `BN-STOCK-SOURCE`
(`LIVE` reads `inv.StockBalance`/`inv.Lot` less `inv.StockReservation`; `TRIAL` uses the supplied
`BN-SUPPLY` table for what-if planning), `BN-USE-RESERVED`, `BN-USE-INBOUND`, and up to 30
`BN-SUPPLY` rows (`BN-S-ITEM`, `BN-S-FREE-QTY`, `BN-S-INBOUND`; `BN-S-USED` is returned).

Output: `BN-GROSS-STARTS`, `BN-SHORTAGE-CNT`, `BN-SHORTAGE-ITEM`, `BN-SHORTAGE-QTY`,
`BN-MAX-BUILDABLE`, `BN-BUILDABLE-NOW`, `BN-COMPLETENESS`, and paged `BN-LINES` with
`BN-L-REQ-QTY`, `BN-L-STOCK-USED`, `BN-L-INBOUND`, `BN-L-TO-MAKE`, `BN-L-SHORTAGE`, `BN-L-EXCESS`.

**Eligible stock is consumed once**: a subassembly supplied from stock is not also exploded for new
material for the same units, and a stock unit cannot satisfy two demands (BOM-020). Spec example B2
is the reference: the netted maximum is **nine frames**, and `TBOMNET` asserts it.

---

## 3. Database objects

### Bulk replacement preview (GUI)

`Bills of Material → Bulk Change Preview` (`BOM.CHANGE.PREVIEW`,
`frmBomBulkPreview`) uses existing `BOM.VIEW` authority, not change approval.
Enter a change reference, old component, replacement, effective date, scope
(`ITEM`, `BOMREF` or `PARENT`) and optional bill/parent. F10 calls native
`BOMBULK PREVIEW`; F5 clears the displayed result. The grid shows every proposed
revision, before/after quantity, outcome and native reason even when refused.
The answer panel retains the complete summary/refusal. There is no apply button.

Demo: `ECO-BOMDEMO-001`, `DEMO-PIN` → `DEMO-PIN-HD`, `01/11/2026`,
scope `ITEM`, include released and draft bills. Seven bills are previewed; the
unconfigured alternate group refuses the run (`BOM1411`), with zero applied.
`DEMO-NOT-AN-ITEM` is an invalid replacement and is also refused by the engine.
Preview persists request/outcome evidence in `BomChange`/`BomChangeLine`, but rolls
back all proposed bill revisions. The seeded change header does not store source
or replacement inputs; enter them explicitly. Native preview/paging requires
`BOM.VIEW`; native apply retains separate `BOM.CHANGE.APPROVE` authority.

| Object | Purpose |
|---|---|
| `bom.BomHeader` / `BomComponent` / `BomByProduct` | the versioned structures; effectivity is half-open `[from, to)` |
| `bom.BomValidation` / `BomValidationFinding` | validation evidence and named findings (BOM-004, BOM-029) |
| `bom.BomExplosion` / `BomExplosionLine` | retained, reproducible calculation results (BOM-032) |
| `bom.BomChange` / `BomChangeLine` | bulk change proposals with per-bill outcome (BOM-024) |
| `bom.BomLineage` | where a job/order bill came from (BOM-022) |
| `bom.BomChangeImpact` | the explicit adopt/retain/replan decision per affected document (BOM-031) |
| `bom.usp_ReleaseBom` | the release gate: validation that still matches the content (content hash), an authorised reviewer who is neither author nor releaser, supersession of the previous revision, audit, idempotency (BOM-004, BOM-028, BOM-030) |
| `bom.usp_BomWhereUsed` | direct and indirect parents + impacted demand (BOM-023) |
| `bom.usp_BomCompareRevisions` | added/removed/changed with changed-field list and cost impact (BOM-025) |
| `bom.vw_BomComponentUsage` | one readable row per occurrence; notes/services/cost-only flagged as non-stock (BOM-027) |
| `bom.vw_BomCurrentRevision` | the deterministic selection, `SelectionRank = 1` is the effective revision (BOM-005) |
| `bom.vw_BomExplosionSummary` | retained runs with completeness, stop reasons and shortage counts |
| `bom.tr_BomReleasedImmutable` | released header/component content is frozen, including NULL-safe header comparisons; identical rewrites and audit stamps are allowed, real changes are refused (BOM-004). Normal supersession/effectivity closure remains allowed |
| `bom.tr_BomHeaderRetireCheck` | a referenced revision cannot be retired (BOM-028) |

Seeds: `30-bom-items.sql` (B1-B6 items), `31-bom-structures.sql` (the bills themselves),
`32-bom-menu.sql` (menu + `core.ReportDef`), `33-bom-maintdef.sql` (`core.MaintDef` / `core.MaintColumn`
for `BOM.HEADER` and `BOM.CHANGE`), `90-bom-demo.sql` (demo transactions),
`90-bom-showcase.sql` (the cake and bicycle showcase structures, see section 5),
`99-bom-demo-planning.sql` (runs last and puts the two figures the worked examples depend on back
after the other modules' demonstration scripts have re-profiled the planning data: the B5 purchase
pack of five and the B1 item standard cost column at Reading).

SQL tests (`build\build-db.ps1 -NoDeploy -Tests -Only bom`, 25 assertions):

| File | Asserts |
|---|---|
| `db\tests\bom-release.sql` | the release gate (no validation, failed validation, stale evidence, author self-review, unauthorised reviewer/releaser, double release), the clean release stamp, idempotent repeat, the immutability trigger (update refused, delete refused, identical rewrite allowed) and supersession history |
| `db\tests\bom-whereused.sql` | direct and indirect parents of a B1 component with the usage per top unit, draft exclusion, the B6 cycle, a two-bill loop, a self-quoting item and the empty answer |

---

## 4. Forms and menu

| Menu | Form | Notes |
|---|---|---|
| `BOM.EDIT` | `frmBomEditor` | bill header/line editor and the release action |
| `BOM.HEADERS` | `frmGenericMaint` (`BOM.HEADER`) | list maintenance of bill headers |
| `BOM.EXPLODE` | `frmBomExplosion` | indented / leaf / costed / netted enquiry, `Ctrl+E` |
| `BOM.WHEREUSED` | `frmBomWhereUsed` | where-used enquiry |
| `BOM.CHANGE` | `frmGenericMaint` (`BOM.CHANGE`) | engineering changes |
| `BOM.RPT.*` | `frmReportLauncher` | `BOMRP010` indented and costed listings, `BOMRP030` where-used, `BOMRP040` revision comparison |

`dotnet\STERP.Business\Bom\BomService.vb` is the only .NET code with a BOM dependency: it fills the
COMMAREAs above, pages the results into `DataTable`s and calls the three procedures.
`frmBomExplosion` and `frmBomWhereUsed` inherit `ListForm`, `frmBomEditor` inherits `BaseForm`; the
editor's release button calls `bom.usp_ReleaseBom` and shows its refusal (for example
`STERP-E-BOM0410`, the author may not sign their own review) as a business rule error.
`dotnet\STERP.Tests\Bom\BomTests.cs` is the harness suite for the facade and the registration
(`build\run-tests.ps1 -Layer net -Filter bom`, 9 cases).

---

## 5. How to demo

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-db.ps1   -Only "bom"
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-cobol.ps1 -Module bom
powershell -NoProfile -ExecutionPolicy Bypass -File build\run-tests.ps1  -Layer cobol -Filter TBOM
bin\x86\BOMRP010.exe CO=STC ITEM=DEMO-FRAME QTY=9 MODE=INDENT USER=ENGINEER
bin\x86\BOMRP010.exe CO=STC ITEM=DEMO-FRAME QTY=9 MODE=COSTED COSTSET=BOMDEMO USER=ENGINEER
bin\x86\BOMRP030.exe CO=STC ITEM=DEMO-PIN ASAT=20261001 LEVELS=10 MODE=WHEREUSED USER=ENGINEER
bin\x86\BOMRP030.exe CO=STC BOMREF=BOM-DEMO-BDY FROMREV=A TOREV=B COSTSET=BOMDEMO MODE=COMPARE USER=ENGINEER
```

1. **B1** - explode `DEMO-FRAME` for 9: the phantom `DEMO-PACK` disappears into its components, the
   plate line converts kilograms to the stock unit, the pin line rounds up to whole pieces and the
   listing prints the unrounded value underneath.
2. **B2** - the netted enquiry for the same frame: shared stock is consumed once and the maximum
   buildable output is **nine frames**.
3. **B4/B5** - the costed listing: material, conversion, by-product recovery credit and unit cost,
   with one value boundary per branch.
4. Try to edit a released revision: `STERP-E-BOM0413`. Release without validation:
   `STERP-E-BOM0406`. Release signed by the author: `STERP-E-BOM0410`.
5. **Bulk change** - preview the seeded engineering change `ECO-BOMDEMO-001` (replace `DEMO-PIN`
   with `DEMO-PIN-HD` everywhere): seven bills are listed with the revision each one would get,
   the released bills show a new draft revision rather than a change to themselves, and the run is
   refused because `BOM-DEMO-OPT` has an unconfigured alternate group - `STERP-E-BOM1411`.
6. **Where used** - `BOMRP030 MODE=WHEREUSED ITEM=DEMO-PIN` prints the pin's six ancestor uses down
   three levels, the top level assemblies and the open demand, then
   `MODE=COMPARE BOMREF=BOM-DEMO-BDY FROMREV=A TOREV=B` prints the one changed component.

### Showcase structures: a cake and a bicycle

`db\seed\90-bom-showcase.sql` seeds two released, fully costed structures that are easier to talk
through than the synthetic B-examples. Open *Bills of Material > Explosion* (cost set `BOMDEMO` is
the default), or print them:

```powershell
bin\x86\BOMRP010.exe CO=STC ITEM=FG-CAKE-CELEB-8IN QTY=12 MODE=COSTED COSTSET=BOMDEMO USER=ENGINEER
bin\x86\BOMRP010.exe CO=STC ITEM=FG-BIKE-COMMUTE-M QTY=10 MODE=COSTED COSTSET=BOMDEMO USER=ENGINEER
bin\x86\BOMRP030.exe CO=STC ITEM=WIRE-TIG-4043-5KG ASAT=20261007 LEVELS=10 MODE=WHEREUSED USER=ENGINEER
```

| | Celebration cake `FG-CAKE-CELEB-8IN` | Commuter bicycle `FG-BIKE-COMMUTE-M` |
|---|---|---|
| Bills | 7 (`CK-*`), 36 occurrences, 4 levels | 17 (`BK-*`), 95 occurrences, 5 levels |
| Path to the deepest leaf | cake > sponge layer > batter > flour | bike > frame > front triangle > bottom bracket assembly > bearing |
| Phantom | decoration set (roses, ribbon, topper, candles) | cable set, hardware pack, paint process |
| Unit conversion | recipes in grams/millilitres, stock in kilograms/litres | - |
| Nominal basis | batter per 10 kg, buttercream per 5 kg, roses per 10 | - |
| Yield / allowance | sponge 96 % (12 cakes need 38 layers baked), roses 90 %; fondant 8 % trim, eggs 2 % | frame 97 % (10 bikes need 11 frames), wheels 99 %, fork 98 %; tubes 5-6 %, paint 15 % overspray |
| Fixed quantities | two piping bags per run | one grease cartridge per run, one argon cylinder per welding batch of 20 |
| Non-material lines | a note; decorating labour 0.75 h at 18.50 | a note; heat treatment, anodising, pre-delivery inspection |
| By-product | sponge offcuts as cake-pop crumb | aluminium swarf |
| Unit cost (BOMDEMO) | 50.20 in a run of 12 | 568.25 in a run of 10 (551 in a run of 20; list price 780) |

The bicycle is built from the commercial catalogue (`FG-FRAME-GRAVEL-M`, `SA-WHEEL-*-700C`,
`SA-BB-ASSY-BSA`, ...), which carries stock, prices and sales orders but had no bills; the cake is
entirely new (`FG-`/`SA-CAKE-*`, `ING-*`, `DECOR-*`, `PKG-CAKE-*`). Only `cst.StandardCost` rows in
`BOMDEMO` are added - `cst.ItemCost` and stock are untouched, so valuation and RECONCTL do not move.
