# Item and Part Maintenance (module `itm`)

Stock Control. The oldest surviving subsystem in the suite: the online programs are
**RM/COBOL** (`itm=rm` in `cobol\DIALECTS.cfg`), fixed format, 72-column source, `*>` comments,
`SECTION`-per-paragraph, Report Writer for the printed output.

* Spec: `08-Item-and-Part-Maintenance.md`, requirements **ITM-001 .. ITM-027**; acceptance
  scenarios in `12-...`; decisions **D-01 .. D-30** in `13-...`
* Schema: `inv` (costing lives in `cst`, postings go through `fin.JournalStage`)
* Online programs: `cobol\online\itm\*.cob` – every one is LINKable (`DFHCOMMAREA` + `<PROG>CA.cpy`)
* Batch programs: `cobol\batch\itm\*.cob`
* Tests: `cobol\tests\itm\T*.cob`, `db\tests\itm-items.sql`, `db\tests\itm-stock.sql`

**Scope.** The governed catalogue identity (code, revision, classification, attributes, units,
supply rules, partner references, compliance), and the physical stock that identity owns
(locations, lots, serials, balances, movements, reservations, quality holds, counts, valuation
evidence). Balances, costs and demand are **derived from transactions and never edited**: the only
way physical stock changes is `STKMOVE` → `inv.usp_PostStockMovement`.

---

## 1. Interfaces other teams need (read this first)

Both programs are pure `CALL` services. The COMMAREA is `STCAHDR` (the standard 400-byte STERP
header) followed by the program body:

```cobol
COPY UOMCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-UOM-CA==.
...
CALL "UOMCONV" USING WS-UOM-CA
```

`STCAHDR` input fields are `CA-PROGRAM X(8)`, `CA-FUNCTION X(8)`, `CA-COMPANY X(10)`,
`CA-SITE X(10)`, `CA-USER X(20)`, `CA-BUSINESS-DATE 9(8)`, `CA-CORRELATION-ID X(36)`; output
fields are `CA-RETURN-CODE 9(2)`, `CA-REASON-CODE X(8)`, `CA-MESSAGE X(200)`, `CA-REQ-REF X(10)`,
`CA-MSG-COUNT 9(2)`. Return codes follow the house convention: **00** OK, **04** warning
(proceed), **06** hold, **08** error (no business effect), **12** severe, **16** not found;
`CA-FAILED` is `08 THRU 16`.

### 1.1 UOMCONV – the unit of measure service (`UOMCONVCA.cpy`)

**Every module that turns an entered quantity into a stock quantity must call UOMCONV.**
There is no implied 1:1. A missing, zero or negative conversion is an error, never an assumed
factor of one (ITM-005, ITM-007, D-09).

```
stock quantity     = entered quantity x stock units per entered unit
sterling unit cost = foreign price / (price-per qty x stock units per price unit)
                                   / foreign currency units per GBP
```

#### Functions (`CA-FUNCTION`)

| Value | Does | Must supply | Gives back |
|---|---|---|---|
| `TOSTOCK` | Entered quantity → stock quantity, then the quantum check. | `UC-ITEM-CODE`, `UC-FROM-UOM`, `UC-ENTERED-QTY`, `UC-USAGE-TYPE`, `UC-EFF-DATE` | `UC-STOCK-QTY`, `UC-RESULT-QTY`, `UC-FACTOR`, `UC-TO-UOM` (= stock unit), `UC-ROUNDED-QTY`, `UC-ADJUSTED` |
| `FROMSTK` | Stock quantity → another unit (`UC-STOCK-QTY / UC-FACTOR`). | `UC-ITEM-CODE`, `UC-STOCK-QTY`, `UC-TO-UOM` | `UC-RESULT-QTY`, `UC-FACTOR`, `UC-INVERSE-FACTOR` |
| `FACTOR` | Look up the effective stock units per `UC-FROM-UOM` for `UC-USAGE-TYPE` at `UC-EFF-DATE`. | `UC-ITEM-CODE`, `UC-FROM-UOM`, `UC-USAGE-TYPE` | `UC-FACTOR`, `UC-INVERSE-FACTOR`, `UC-EFF-FROM`, `UC-EFF-TO`, `UC-SOURCE`, `UC-PACK-DESC` |
| `CONVERT` | Dimensional conversion `UC-FROM-UOM` → `UC-TO-UOM` through `inv.Uom` only – **no item involved**. | `UC-FROM-UOM`, `UC-TO-UOM`, `UC-ENTERED-QTY` | `UC-RESULT-QTY`, `UC-FACTOR`, `UC-FROM-DIM`, `UC-TO-DIM`, `UC-DECIMALS` |
| `CHECKQTY` | Test a quantity against the item quantum and indivisibility. | `UC-ITEM-CODE`, `UC-STOCK-QTY` (or `UC-ENTERED-QTY`) | `UC-ROUNDED-QTY`, `UC-ADJUSTED`, `UC-QUANTUM`, `UC-INDIVISIBLE` |
| `UNITCOST` | Sterling unit cost of a purchase price. | `UC-ITEM-CODE`, `UC-FOREIGN-PRICE`, `UC-PRICE-PER-QTY`, `UC-PRICE-UOM`, `UC-FX-PER-GBP` | `UC-UNIT-COST-GBP`, `UC-FACTOR`, `UC-STOCK-UOM` |
| `ITEMUOM` | List the item's effective conversions, newest first. | `UC-ITEM-CODE` | `UC-LINE-CNT`, `UC-LINES(1..20)` |

`UC-USAGE-TYPE` is one of `STOCK`, `PURCHASE`, `SALES`, `ISSUE`, `PRICE`, `PACK`.
`UC-EFF-DATE` is `YYYYMMDD`; **0 means the COMMAREA business date**.

#### `UC-BODY` fields

| Field | PIC | Dir | Meaning / valid values |
|---|---|---|---|
| `UC-ITEM-CODE` | `X(40)` | in | Item whose conversions apply; not used by `CONVERT`. |
| `UC-FROM-UOM` | `X(10)` | in/out | Unit the quantity is entered in. **Blank defaults to the item's stock unit.** `FROMSTK` and `UNITCOST` overwrite it while resolving. |
| `UC-TO-UOM` | `X(10)` | in/out | Target unit. `TOSTOCK` sets it to the stock unit on return. |
| `UC-USAGE-TYPE` | `X(10)` | in | `STOCK`/`PURCHASE`/`SALES`/`ISSUE`/`PRICE`/`PACK`. |
| `UC-EFF-DATE` | `9(8)` | in | Date the factor must be effective on; `0` = `CA-BUSINESS-DATE`. |
| `UC-ENTERED-QTY` | `S9(13)V9(6)` | in | Quantity as keyed, in `UC-FROM-UOM`. |
| `UC-STOCK-QTY` | `S9(13)V9(6)` | in/out | Out for `TOSTOCK`; in for `FROMSTK` and `CHECKQTY`. |
| `UC-RESULT-QTY` | `S9(13)V9(6)` | out | Converted quantity expressed in `UC-TO-UOM`. |
| `UC-FACTOR` | `S9(9)V9(9)` | out | Stock units per one `UC-FROM-UOM`. Nine decimals (D-09). |
| `UC-INVERSE-FACTOR` | `S9(9)V9(9)` | out | `1 / UC-FACTOR`. |
| `UC-STOCK-UOM` | `X(10)` | out | The item's stock unit. |
| `UC-FROM-DIM` / `UC-TO-DIM` | `X(10)` | out | Dimensions of the two units (`LENGTH`, `MASS`, `EACH`, …). |
| `UC-DECIMALS` | `9(2)` | out | Decimal places the target unit is held to. |
| `UC-QUANTUM` | `S9(13)V9(6)` | out | Permitted quantity increment. A serialised item is forced to `1`. |
| `UC-INDIVISIBLE` | `X` | out | `"Y"`/`"N"`; `88 UC-IS-INDIVISIBLE VALUE "Y"`. Serialised items are always `"Y"`. |
| `UC-TRACEABILITY` | `X(8)` | out | `NONE`, `LOT` or `SERIAL`. |
| `UC-PACK-DESC` | `X(40)` | out | Pack description of the conversion used ("box of 100"). |
| `UC-EFF-FROM` / `UC-EFF-TO` | `9(8)` | out | Validity window of the factor that was applied. |
| `UC-SOURCE` | `X(10)` | out | Where the factor came from: `88 UC-FROM-ITEM "ITEMUOM"`, `88 UC-FROM-UOMTAB "UOM"`, `88 UC-FROM-IDENTITY "IDENTITY"`. |
| `UC-FOREIGN-PRICE` | `S9(11)V9(6)` | in | `UNITCOST`: the supplier's price in its own currency. |
| `UC-PRICE-PER-QTY` | `S9(9)V9(6)` | in | `UNITCOST`: price per 1 / 10 / 100 …; **zero is treated as 1**. |
| `UC-PRICE-UOM` | `X(10)` | in | `UNITCOST`: unit the price is quoted in; overrides `UC-FROM-UOM`. |
| `UC-FX-PER-GBP` | `S9(9)V9(8)` | in | `UNITCOST`: foreign currency units per GBP; **zero is treated as 1**. |
| `UC-UNIT-COST-GBP` | `S9(11)V9(6)` | out | Sterling cost per **stock** unit. |
| `UC-ROUNDED-QTY` | `S9(13)V9(6)` | out | The quantity rounded **up** to the next whole quantum. |
| `UC-ADJUSTED` | `X` | out | `"Y"` when rounding changed the quantity; `88 UC-WAS-ADJUSTED VALUE "Y"`. |
| `UC-LINE-CNT` | `9(3)` | out | `ITEMUOM`: number of lines returned (max 20). |
| `UC-LINES(n)` | `OCCURS 20` | out | `UC-L-UOM X(10)`, `UC-L-USAGE X(10)`, `UC-L-FACTOR S9(9)V9(9)`, `UC-L-FROM 9(8)`, `UC-L-TO 9(8)`, `UC-L-PACK X(40)`. |

#### Factor resolution, precision and indivisibility

`3000-RESOLVE-FACTOR` tries, strictly in this order: **(1)** an approved item-specific
`inv.ItemUom` row for the requested usage, **(2)** an approved item-specific row for any usage,
**(3)** the entered unit *is* the stock unit → factor 1, **(4)** a dimensional conversion through
`inv.Uom`. **Anything else is an error – there is no implied 1:1.**

Factors are held to nine decimals and quantities to six (D-09); every `COMPUTE` is `ROUNDED`.
`5000-CHECK-QUANTUM` then applies the item policy: a quantum of zero or less is treated as 1; if
`UC-INDIVISIBLE` is not `"Y"` the quantity passes untouched; otherwise the quantity is divided by
the quantum and a remainder outside ±0.0000005 makes the program round **up** to
`(whole + 1) x quantum`, set `UC-ROUNDED-QTY`, set `UC-ADJUSTED = "Y"`, set `UOM0006` and reject
with `CA-RETURN-CODE 08`. The caller is expected to show `UC-ROUNDED-QTY` and ask the user to
confirm it – UOMCONV never silently alters a quantity.

#### Reason codes

| `CA-REASON-CODE` | `CA-RETURN-CODE` | Meaning |
|---|---|---|
| `UOM0001` | `16` (`08` on a blank key) | Item not known in this company. |
| `UOM0002` | `08` | Unit of measure not known. |
| `UOM0003` | `08` (`16` from `ITEMUOM`) | No effective conversion for that unit and usage at that date. |
| `UOM0004` | `08` | Conversion factor is zero or negative. |
| `UOM0005` | `08` | Units are dimensionally incompatible (`LENGTH` → `MASS`). |
| `UOM0006` | `08` | Quantity breaks the item quantum / indivisibility; see `UC-ROUNDED-QTY`. |
| `UOM0007` | `08` | Price-per quantity or exchange rate is zero. |
| `UOM0008` | `08` | Unsupported `CA-FUNCTION`. |
| `UOM0009` | `08` | Quantity out of range. |
| – | `12` | SQL failure (`9300-SQL-FAILED`); `CA-MESSAGE` carries the SQLIO text. |

#### Worked call

A buyer keys **3 boxes** of `BOLT-M5X16-SS`, bought in `PK100` (100 each per box):

```cobol
MOVE "UOMCONV"          TO CA-PROGRAM
MOVE "TOSTOCK"          TO CA-FUNCTION
MOVE "STC"              TO CA-COMPANY
MOVE "BUYER"            TO CA-USER
MOVE 20261015           TO CA-BUSINESS-DATE
MOVE "BOLT-M5X16-SS"    TO UC-ITEM-CODE
MOVE "PK100"            TO UC-FROM-UOM
MOVE "PURCHASE"         TO UC-USAGE-TYPE
MOVE 0                  TO UC-EFF-DATE
MOVE 3                  TO UC-ENTERED-QTY
CALL "UOMCONV" USING WS-UOM-CA
*> CA-RETURN-CODE 00, UC-FACTOR 100.000000000, UC-STOCK-UOM "EA",
*> UC-STOCK-QTY 300.000000, UC-SOURCE "ITEMUOM", UC-ADJUSTED "N"
```

The same part at **£75.00 per sleeve of 25** gives `UNITCOST` → `UC-UNIT-COST-GBP` **3.000000**
per `EA` (75.00 / (1 x 25) / 1.0). Keying **2.5** boxes of an indivisible item returns
`CA-RETURN-CODE 08`, `UOM0006`, `UC-ROUNDED-QTY 3`, `UC-ADJUSTED "Y"`.

---

### 1.2 AVAILQ – the availability service (`AVAILQCA.cpy`)

**This is the interface Sales (`so`) and Kitting (`kit`) call** to ask "how much may I use?".
The quantities follow section 6 of the specification and are **never double counted**: a bucket is
owned or third party, and it is in exactly one condition (ITM-012, ITM-013).

#### Functions (`CA-FUNCTION`)

| Value | Does |
|---|---|
| `AVAIL` | The quantity summary for the scope (no lines). |
| `ENQUIRE` | `AVAIL` plus one line per balance bucket in `AV-LINES`. |
| `BYLOC` | One line per location, **owned stock only**. |
| `LOTS` | One line per lot with its expiry date and condition. |

Blank scope fields mean "all". `AV-OWNER-TYPE` defaults to `OWNED`, so customer- and
supplier-owned material in our custody is never counted as ordinary available stock.

#### `AV-BODY` fields

| Field | PIC | Dir | Meaning / valid values |
|---|---|---|---|
| `AV-ITEM-CODE` | `X(40)` | in | Item asked about (required). |
| `AV-REVISION` | `X(4)` | in | Restrict to one revision; blank = all. |
| `AV-SITE` | `X(10)` | in | Site scope; blank = every site in the company. |
| `AV-WAREHOUSE` | `X(20)` | in | Warehouse scope; blank = all. |
| `AV-LOCATION` | `X(20)` | in | Location scope; blank = all. |
| `AV-LOT-CODE` | `X(30)` | in | Lot scope; blank = all. |
| `AV-SERIAL-NO` | `X(40)` | in | Serial scope; blank = all. |
| `AV-OWNER-TYPE` | `X(10)` | in | `OWNED` (default), `CUSTOMER`, `SUPPLIER`. |
| `AV-OWNER-CODE` | `X(20)` | in | The customer or supplier that owns the material. |
| `AV-CONDITION` | `X(12)` | in | Filter one quality status: `ACCEPTED`, `QUARANTINE`, `INSPECTION`, `REJECTED`, `EXPIRED`. |
| `AV-AS-AT` | `9(8)` | in | `YYYYMMDD` the expiry and shelf-life tests are made on; `0` = business date. |
| `AV-DEMAND-TYPE` | `X(12)` | in | `SALESORDER`, `KIT`, `TRANSFER`, `JOB`, `OTHER`. |
| `AV-DEMAND-REF` | `X(40)` | in | The demand asking. Stock already reserved to it is reported back to it. |
| `AV-DEMAND-LINE` | `S9(9) COMP-5` | in | Line of that demand. |
| `AV-STOCK-UOM` | `X(10)` | out | Unit every quantity below is expressed in. |
| `AV-QTY-PHYSICAL` | `S9(13)V9(6)` | out | **Owned** stock on hand in scope, every quality condition, every location. Excludes third-party material. |
| `AV-QTY-ELIGIBLE` | `S9(13)V9(6)` | out | Owned stock that satisfies quality, expiry, minimum remaining shelf life, site/revision scope and location availability. |
| `AV-QTY-RESERVED` | `S9(13)V9(6)` | out | Active reservations against the eligible stock (all demands). |
| `AV-QTY-FREE` | `S9(13)V9(6)` | out | `ELIGIBLE - RESERVED`, floored at zero. **Ignores safety stock.** |
| `AV-QTY-EXPECTED` | `S9(13)V9(6)` | out | Confirmed supply not yet received (purchase receipts, transfer receipts). Reported **separately**, never added to free. |
| `AV-QTY-PLANNED` | `S9(13)V9(6)` | out | Planned assembly output. Reported separately. |
| `AV-QTY-QUARANTINE` | `S9(13)V9(6)` | out | Owned, physical, condition `QUARANTINE`. |
| `AV-QTY-INSPECTION` | `S9(13)V9(6)` | out | Owned, physical, condition `INSPECTION`. |
| `AV-QTY-REJECTED` | `S9(13)V9(6)` | out | Owned, physical, condition `REJECTED`. |
| `AV-QTY-EXPIRED` | `S9(13)V9(6)` | out | Condition `EXPIRED`, **or** expiry before `AV-AS-AT`, **or** inside the minimum remaining shelf life. |
| `AV-QTY-INTRANSIT` | `S9(13)V9(6)` | out | Owned stock sitting in a `TRANSIT` location. |
| `AV-QTY-THIRDPARTY` | `S9(13)V9(6)` | out | Customer- or supplier-owned material in our custody. **Not** part of physical. |
| `AV-QTY-UNAVAIL-LOC` | `S9(13)V9(6)` | out | Owned stock excluded for any other reason: location not available, location holds external stock, item blocked or retired. |
| `AV-QTY-OWN-DEMAND` | `S9(13)V9(6)` | out | Of `AV-QTY-RESERVED`, the part already held for `AV-DEMAND-REF` itself. |
| `AV-QTY-FOR-DEMAND` | `S9(13)V9(6)` | out | `FREE + OWN-DEMAND` – what this demand may actually use. Never offered to another demand. |
| `AV-QTY-SAFETY` | `S9(13)V9(6)` | out | The safety stock target (a planning figure, not a block). |
| `AV-QTY-POLICY` | `S9(13)V9(6)` | out | `FREE - SAFETY`, floored at zero: the protected view. |
| `AV-EXPECTED-DATE` | `9(8)` | out | Date the earliest expected supply is due. |
| `AV-DATA-COMPLETE` | `X` | out | `"Y"` when nothing was suppressed; `88 AV-IS-COMPLETE VALUE "Y"`. |
| `AV-RESTRICTED` | `X` | out | `"Y"` when the user's permissions hid part of the answer (a labelled gap, never an invented zero). |
| `AV-LINE-CNT` | `9(3)` | out | Lines returned (max 40). |
| `AV-LINES(n)` | `OCCURS 40` | out | `AV-L-SITE X(10)`, `AV-L-WAREHOUSE X(20)`, `AV-L-LOCATION X(20)`, `AV-L-LOC-TYPE X(12)`, `AV-L-REVISION X(4)`, `AV-L-LOT X(30)`, `AV-L-SERIAL X(40)`, `AV-L-OWNER-TYPE X(10)`, `AV-L-OWNER X(20)`, `AV-L-CONDITION X(12)`, `AV-L-QTY S9(13)V9(6)`, `AV-L-RESERVED S9(13)V9(6)`, `AV-L-FREE S9(13)V9(6)`, `AV-L-EXPIRY 9(8)`, `AV-L-ELIGIBLE X`, `AV-L-WHY X(30)`. |

#### How a bucket is classified (`1300-ELIGIBILITY` / `1400-ACCUMULATE`)

A bucket whose owner is not `OWNED` goes **only** to `AV-QTY-THIRDPARTY`. Everything else is added
to `AV-QTY-PHYSICAL`, and then either to `AV-QTY-ELIGIBLE` or to exactly one exclusion bucket. The
first failing test wins, in this order, and its text is returned in `AV-L-WHY`:

1. `"Location holds external stock"` → `AV-QTY-UNAVAIL-LOC`
2. `"In transit"` (location type `TRANSIT`) → `AV-QTY-INTRANSIT`
3. `"Quarantined"` → `AV-QTY-QUARANTINE`
4. `"Awaiting inspection"` → `AV-QTY-INSPECTION`
5. `"Rejected"` → `AV-QTY-REJECTED`
6. `"Expired"` (condition `EXPIRED`, or expiry earlier than `AV-AS-AT`) → `AV-QTY-EXPIRED`
7. `"Inside minimum shelf life"` (expiry earlier than `AV-AS-AT` + minimum remaining days, computed through `ITMDATE ADDDAYS`) → `AV-QTY-EXPIRED`
8. `"Location not available"` → `AV-QTY-UNAVAIL-LOC`
9. `"Item is blocked or retired"` → `AV-QTY-UNAVAIL-LOC`

#### Reason codes

| `CA-REASON-CODE` | `CA-RETURN-CODE` | Meaning |
|---|---|---|
| `AVL0001` | `16` | Item not known in this company, or the item has no physical and no third-party stock in scope. |
| `AVL0002` | `08` | Unsupported `CA-FUNCTION`. |
| `AVL0003` | `04` | More buckets exist than `AV-LINES` can carry – the summary is complete, the lines are truncated. |
| `AVL0004` | `04` | An expiry-controlled item has stock without an expiry date; the figures are reported with a warning. |
| `AVL0005` | `08` | `AV-AS-AT` is not a valid date. |
| – | `12` | SQL failure; `CA-MESSAGE` carries the SQLIO text. |

#### Worked call

Sales needs 40 `BEARING-6902-2RS` for order `SO-10042` line 1 at Reading:

```cobol
MOVE "AVAILQ"            TO CA-PROGRAM
MOVE "AVAIL"             TO CA-FUNCTION
MOVE "STC"               TO CA-COMPANY
MOVE "RDG"               TO CA-SITE
MOVE "SALESREP"          TO CA-USER
MOVE "BEARING-6902-2RS"  TO AV-ITEM-CODE
MOVE "RDG"               TO AV-SITE
MOVE "SALESORDER"        TO AV-DEMAND-TYPE
MOVE "SO-10042"          TO AV-DEMAND-REF
MOVE 1                   TO AV-DEMAND-LINE
MOVE 20261015            TO AV-AS-AT
CALL "AVAILQ" USING WS-AVAIL-CA
```

With 180 accepted in `STKA01`, 120 quarantined in `QUAR01` and 25 already reserved (10 of them to
`SO-10042` itself), the answer is `AV-QTY-PHYSICAL 300`, `AV-QTY-ELIGIBLE 180`,
`AV-QTY-QUARANTINE 120`, `AV-QTY-RESERVED 25`, `AV-QTY-FREE 155`, `AV-QTY-OWN-DEMAND 10`,
`AV-QTY-FOR-DEMAND 165`, `CA-RETURN-CODE 00`. The 120 quarantined units are visible but are not
offered, and they are not silently lost from the physical total.

---

## 2. Requirement traceability (ITM-001 .. ITM-027)

| Req | Summary | Implemented by |
|---|---|---|
| ITM-001 | Unique stable item identities, revisions, unambiguous full-code / alias search | `ITEMVAL` (`FETCH`, `SEARCH`, `RESOLVE`), `inv.vw_ItemAlias`, `inv.usp_UiItemLookup`, `CK_Item_Code` |
| ITM-002 | Classification controls stock, sales, purchasing, manufacture, BOM and costing behaviour | `ITEMVAL CLASSCHK`, `inv.Item.ItemClass` (`CK_Item_Class`), `inv.ItemRevision` |
| ITM-003 | Activation requires the information its processes need and an authorised review | `ITEMVAL VALIDATE` / `ACTIVATE`, `inv.Item.LifecycleStatus` |
| ITM-004 | Descriptive, physical, engineering and analysis attributes with units, ownership and history | `ITEMVAL`, `ITEMCOPY`, `inv.tr_Item_Audit`, `inv.Item` attribute columns |
| ITM-005 | Unit definitions with explicit dimensions, positive factors, scope and effective dates | `UOMCONV` (`FACTOR`, `CONVERT`), `inv.Uom`, `inv.ItemUom` |
| ITM-006 | Permitted quantity precision and indivisibility per item | `UOMCONV CHECKQTY` (`5000-CHECK-QUANTUM`), `LOTSER`, `inv.Item.QuantumQty` / `IsIndivisible`, `STERP-E-0028` |
| ITM-007 | Purchase, sales, price and issue units convert reproducibly to the stock unit | `UOMCONV` (`TOSTOCK`, `FROMSTK`, `UNITCOST`), `inv.StockMovement.ConversionFactor` |
| ITM-008 | Supply rules: make/buy, lead times, minimums, purchase multiples, site-specific sourcing | `ITEMENQ SUPPLY`, `inv.ItemSite`, `inv.ItemPartner` |
| ITM-009 | Supplier / customer item references with partner, code, unit, validity, approved source | `ITEMENQ PARTNER`, `ITEMVAL RESOLVE`, `inv.ItemPartner`, `inv.vw_ItemAlias` |
| ITM-010 | Financial / VAT / valuation categories validated and protected against unsafe retrospective change | `ITMIMPCT` (`PROTECT`, `IMPACT`), `inv.usp_ItemImpact`, `ITMCOMPL VATCODE` |
| ITM-011 | Lot / serial and shelf life enforced on receipt, transfer, issue, despatch, return | `LOTSER` (`CHKMOVE`, `NEWLOT`, `NEWSER`, `SETSTAT`, `EXPCHK`, `TRACE`), `ITMDATE`, `STERP-E-0027` |
| ITM-012 | Enquiries distinguish item/revision, company, owner, site/location, lot/serial, condition | `AVAILQ` (`ENQUIRE`, `BYLOC`, `LOTS`), `ITEMENQ`, `inv.usp_StockEnquiry`, `inv.vw_StockAvailability`, `STKSTAT` |
| ITM-013 | Availability separates physical, eligible, reserved, free, expected and planned | `AVAILQ` (`1300`/`1400`), `inv.vw_StockAvailability`, `inv.usp_ReserveStock` |
| ITM-014 | Reservations demand-specific, quantity-bounded, revalidated on use and on eligibility change | `STKRESV` (`RESERVE`, `RELEASE`, `CONSUME`, `REVALID`, `ENQUIRE`), `inv.usp_ReserveStock` (`UPDLOCK, HOLDLOCK`) |
| ITM-015 | Stock changes only through authorised receipt, issue, transfer, return, adjustment, completion | `STKMOVE`, `inv.usp_PostStockMovement`, `inv.StockMovement` |
| ITM-016 | Quality / hold actions record quantity, lot, reason, owner, outcome and release authority | `STKMOVE` (`HOLD`, `RELEASE`, `ACCEPT`, `REJECT`), `inv.QualityHold` |
| ITM-017 | Transfers preserve ownership, traceability and total quantity/value, with in-transit status | `STKMOVE TRANSFER` / `CONFIRM`, `inv.Location` type `TRANSIT`, `inv.StockBalance.QtyInTransit` |
| ITM-018 | Counting: controlled scope, expected/counted quantities, recount and approved adjustment | `COUNTADJ` (`SHEET`, `ENTER`, `RECOUNT`, `APPROVE`, `POST`), `inv.StockCount`, `inv.StockCountLine` |
| ITM-019 | Substitution and equivalent-revision rules with contexts, ratios, validity and approval | `SUBSTCHK` (`LIST`, `CHECK`, `REVCHECK`, `APPLY`), `inv.ItemSubstitute` |
| ITM-020 | Blocking, obsolescence and succession by business use with impact review | `ITMIMPCT` (`BLOCK`, `OBSOLETE`, `SUCCEED`), `inv.usp_ItemImpact` |
| ITM-021 | Item copying and bulk maintenance preview attributes, exclusions and affected records | `ITEMCOPY` (`PREVIEW`, `COPY`, `BULKPREV`, `BULKAPPL`) |
| ITM-022 | Referenced items are retired, not deleted; deletion only for unused drafts | `ITMIMPCT` (`RETIRE`, `DELCHECK`), `inv.Item.LifecycleStatus = 'RETIRED'` |
| ITM-023 | Enquiries show usage, demand, supply, stock, valuation, aliases, substitutions, where-used | `ITEMENQ` (`WHEREUSED`, `USAGE`, `SUMMARY`), `inv.usp_ItemWhereUsed`, `STKSTAT` |
| ITM-024 | Opening stock with quantity, ownership, location, condition, traceability, valuation evidence | `STKMOVE OPENING`, `OPENSTK` (batch), `cst.CostLayer` |
| ITM-025 | Effective-dated UK VAT classification driving default VAT codes | `ITMCOMPL` (`VATCODE`, `VATSET`), `inv.ItemVatClass` → `fin.VatCode` |
| ITM-026 | Customs data: commodity code, origin, preference evidence, export control | `ITMCOMPL` (`CUSTOMS`, `CUSTCHK`), `inv.ItemCustoms` (`CK_ItemCus_Cn`) |
| ITM-027 | Product compliance and environmental data (UKCA/CE, dangerous goods, WEEE, batteries, packaging) | `ITMCOMPL` (`COMPLY`, `DGCHECK`, `PACKAGE`), `inv.ItemPackaging`, `PKGRPT` (RPT-019) |

---

## 3. Online programs (`cobol\online\itm`)

| Program | Copybook | Requirements | `CA-FUNCTION` values | Purpose |
|---|---|---|---|---|
| `ITEMVAL` | `ITEMVALCA` | ITM-001..004 | `FETCH`, `SEARCH`, `RESOLVE`, `CLASSCHK`, `VALIDATE`, `ACTIVATE`, `REVISION` | Item identity, full-code and alias search, the classification rules that decide what an item may take part in, and the activation review that releases a draft. Nothing is identified by truncation. |
| `UOMCONV` | `UOMCONVCA` | ITM-005, ITM-006, ITM-007 | `TOSTOCK`, `FROMSTK`, `FACTOR`, `CONVERT`, `CHECKQTY`, `UNITCOST`, `ITEMUOM` | The unit of measure service (section 1.1). |
| `AVAILQ` | `AVAILQCA` | ITM-012, ITM-013 | `AVAIL`, `ENQUIRE`, `BYLOC`, `LOTS` | Stock enquiry and availability (section 1.2). |
| `STKMOVE` | `STKMOVECA` | ITM-015, ITM-016, ITM-017, ITM-024 | `RECEIPT`, `ISSUE`, `RETURN`, `ADJUST`, `COMPLETE`, `OPENING`, `SCRAP`, `TRANSFER`, `CONFIRM`, `HOLD`, `RELEASE` | The only way physical stock changes. Every change states its source, reason, ownership, traceability and quality status; `TRANSFER` then `CONFIRM` models in-transit. |
| `LOTSER` | `LOTSERCA` | ITM-011, ITM-006 | `CHKMOVE`, `NEWLOT`, `NEWSER`, `SETSTAT`, `EXPCHK`, `TRACE` | Lot, serial and shelf-life control. A serial number is unique while it represents a unit in stock; expired material is refused. |
| `STKRESV` | `STKRESVCA` | ITM-014 | `RESERVE`, `RELEASE`, `CONSUME`, `REVALID`, `ENQUIRE` | Reservation primitives. The free-stock arithmetic runs inside `inv.usp_ReserveStock` under `UPDLOCK`. |
| `SUBSTCHK` | `SUBSTCHKCA` | ITM-019 | `LIST`, `CHECK`, `REVCHECK`, `APPLY` | Substitution and equivalent-revision rules; a substitute never silently replaces an engineering- or customer-specified item. |
| `ITEMCOPY` | `ITEMCOPYCA` | ITM-021, ITM-004 | `PREVIEW`, `COPY`, `BULKPREV`, `BULKAPPL` | Item copy and bulk maintenance with preview. A copy creates a **new identity**: never balances, movement history or approvals. |
| `ITMIMPCT` | `ITMIMPCTCA` | ITM-010, ITM-020, ITM-022 | `IMPACT`, `BLOCK`, `OBSOLETE`, `SUCCEED`, `RETIRE`, `DELCHECK`, `PROTECT` | Lifecycle change impact review before an item is blocked, obsoleted, superseded or retired, and before a protected financial attribute is changed. |
| `ITEMENQ` | `ITEMENQCA` | ITM-008, ITM-009, ITM-023 | `SUPPLY`, `PARTNER`, `WHEREUSED`, `USAGE`, `SUMMARY` | Supply rules, partner references, where-used and usage. Procurement rounding to a minimum order and a purchase multiple is reported as **excess supply**, separately from engineering usage. Restricted cost data is a labelled gap. |
| `ITMCOMPL` | `ITMCOMPLCA` | ITM-025, ITM-026, ITM-027 | `VATCODE`, `VATSET`, `CUSTOMS`, `CUSTCHK`, `COMPLY`, `DGCHECK`, `PACKAGE` | UK VAT classification by tax point, trade and customs data, and product compliance. An item without a classification cannot be invoiced. |
| `ITMAUDIT` | – | COM-013 | – | Thin wrapper over `AUDITLOG` so a stock program never has two standard COMMAREA headers in scope. Called by `SUBSTCHK`, `ITEMCOPY`, `ITMIMPCT`, `COUNTADJ`, `OPENSTK`. |
| `ITMDATE` | – | ITM-011 | `ADDDAYS`, … | Thin wrapper over `DATECALC`, same reason. Used for shelf-life arithmetic. |

## 4. Batch programs (`cobol\batch\itm`)

| Program | Requirements | Output | Run as |
|---|---|---|---|
| `STKSTAT` | ITM-012, ITM-013, ITM-023 | Stock status report, 132-column Report Writer, `run\spool\` | `STKSTAT CO=STC ASAT=yyyymmdd USER=WHSE [SITE=RDG]` |
| `COUNTADJ` | ITM-018, ITM-015 | Count sheets, recount control and the approved variance posted as an adjustment movement | `COUNTADJ CO=STC COUNT=<ref> MODE=SHEET\|POST USER=WHSE` |
| `STKVALF` | ITM-023, ITM-024 | Stock valuation file and report (carrying value by valuation group) | `STKVALF CO=STC ASAT=yyyymmdd USER=COSTACC` |
| `LOTTRACE` | ITM-011 | Lot and serial traceability report, forwards and backwards from a lot | `LOTTRACE CO=STC ITEM=<code> LOT=<lot> USER=WHSE` |
| `PKGRPT` | ITM-027 | Compliance and packaging report **RPT-019** (UKCA, dangerous goods, WEEE, batteries, packaging tax) | `PKGRPT CO=STC FROM=yyyymmdd TO=yyyymmdd USER=VATOWNER` |
| `OPENSTK` | ITM-024 | Opening stock load with ownership, condition, traceability and valuation evidence | `OPENSTK CO=STC FILE=<name> USER=WHSE` |

## 5. Stored procedures and views

| Object | Purpose |
|---|---|
| `inv.usp_StockEnquiry` | The enquiry behind the stock enquiry screen and `STKSTAT`. Returns one row per bucket with `IsEligible` and a plain-English `NotEligibleReason`, so third-party, in-transit, quarantined and expired stock can never be read as available (ITM-012, ITM-013, ITM-023). |
| `inv.usp_PostStockMovement` | The single stock contract. Validates direction, lifecycle, location, traceability, quantum and period, then updates `inv.StockBalance` under `UPDLOCK, HOLDLOCK` and writes `inv.StockMovement`. Honours `@CorrelationId` for idempotency (COM-015) and audits adjustments (ITM-015..017). |
| `inv.usp_ReserveStock` | `RESERVE` / `RELEASE` / `CONSUME` / `REVALID`. Recomputes eligible and reserved under `UPDLOCK, HOLDLOCK` inside its own transaction so two users cannot both reserve the last unit (ITM-014, COM-005). |
| `inv.usp_ItemImpact` | Counts everything that still depends on an item – stock, in-transit, reservations, movements, open sales and purchase lines, BOM lines, kits, substitutes, holds, count lines, live serials – for `ITMIMPCT` (ITM-010, ITM-020, ITM-022, ITM-023). |
| `inv.usp_ItemWhereUsed` | Where-used and usage (`BOM`, `KIT`, `SUBST`, `USAGE`, `ALL`); costs only joined when `@ShowCost = 1`, which the caller sets from the permission check. |
| `inv.usp_UiItemLookup` | F2 lookup across item codes, descriptions and partner aliases. |
| `inv.vw_StockAvailability` | One row per physical bucket with ownership, condition, location availability, expiry and traceability policy. **The arithmetic that turns these rows into physical / eligible / reserved / free lives in `AVAILQ`, not in SQL.** |
| `inv.vw_ItemAlias` | The full-code and alias search set; the internal identity is always reported with the alias that found it (D-21). |

Tables: `inv.Uom`, `inv.Item`, `inv.ItemRevision`, `inv.ItemUom`, `inv.ItemSite`,
`inv.ItemPartner`, `inv.ItemSubstitute`, `inv.ItemVatClass`, `inv.ItemCustoms`,
`inv.ItemPackaging`, `inv.Warehouse`, `inv.Location`, `inv.Lot`, `inv.Serial`,
`inv.StockBalance`, `inv.StockMovement`, `inv.StockReservation`, `inv.QualityHold`,
`inv.StockCount`, `inv.StockCountLine`. Audit trigger: `inv.tr_Item_Audit`.

## 6. Forms and menu

* **Item master** (`frmItemMaster`) – identity, revisions, classification, attributes, units,
  supply rules, partner references, VAT class, customs and compliance; activation review and the
  impact review before blocking, obsoleting, superseding or retiring.
* **Stock enquiry** (`frmStockEnquiry`) – the availability buckets with the eligibility reason per
  bucket, by site, location, lot, serial, owner and condition.
* **Stock movement** (`frmStockMovement`) – receipt, issue, transfer, return, adjustment,
  completion, scrap, quality hold and release.
* **Stock count** (`frmStockCount`) – count scope, count sheets, counted and recounted quantities,
  intervening movements, approved adjustment.
* Menu rows (`core.MenuItem`, parent `ITM`), `core.ReportDef` rows and the business event map are
  seeded by `db\seed\27-itm-menu.sql` and `db\seed\28-itm-eventmap.sql`; the demo data is
  `db\seed\91-itm-demo.sql`.

## 7. Main business flows

* **Identity, revisions and alias search (ITM-001, ITM-002).** An item code uses only
  `A-Z 0-9 . / _ -` (`CK_Item_Code`). Revisions live in `inv.ItemRevision` with their own status
  and interchangeability flag; `inv.Item.CurrentRevision` points at the live one. `ITEMVAL SEARCH`
  and `inv.vw_ItemAlias` search the internal code, the description and every partner part number,
  and always report **which alias found the item** – nothing is matched by truncation (D-21).
* **Classification and activation (ITM-002, ITM-003).** `ItemClass` is closed
  (`BOUGHT`, `MADE`, `NONSTOCK`, `SERVICE`, `PHANTOM`, `KIT`, `PACKAGING`, `COSTONLY`) and decides
  what the item may do. A `DRAFT` or `REVIEW` item cannot take a stock movement at all
  (`STERP-E-0025`); `ITEMVAL ACTIVATE` runs the review that moves it to `RELEASED`.
* **Attributes and audit (ITM-004).** Classification, units, traceability, valuation group, cost
  method and lifecycle are protected data: `inv.tr_Item_Audit` writes every insert, update and
  delete to `core.AuditLog` with the before and after values.
* **Units, precision and indivisibility (ITM-005..007).** See section 1.1. The rule is enforced
  twice on purpose: `UOMCONV CHECKQTY` for the user, and `inv.usp_PostStockMovement`
  (`STERP-E-0028`) for anything that reaches the database by another route.
* **Supply rules and partner references (ITM-008, ITM-009).** `inv.ItemSite` holds make/buy, lead
  time, minimum and multiple per site; `inv.ItemPartner` holds supplier, customer and manufacturer
  part numbers with unit, price break, validity and approved-source rank. `ITEMENQ SUPPLY`
  reports rounding to a purchase multiple as **excess supply**, never as demand.
* **Category protection (ITM-004).** Classification values in use cannot be withdrawn: the unit of
  measure and VAT code an item is classified against are enforced by foreign keys
  (`inv.Item → inv.Uom`, `inv.ItemVatClass → fin.VatCode`). **Note:** `Category` and
  `ProductGroup` are values on `inv.Item` rather than a separate master table, so "a category in
  use cannot be deleted" is enforced by the maintenance screen and by `ITMIMPCT DELCHECK` reading
  `inv.usp_ItemImpact` – the database alone will not stop the row from being removed. The SQL test
  asserts the in-use condition rather than a constraint violation.
* **Lot, serial and shelf life (ITM-011).** `TraceabilityType` `LOT` or `SERIAL` makes the lot or
  serial mandatory on every movement (`STERP-E-0027`). `MinRemainingShelfLifeDays` removes stock
  from availability before it actually expires.
* **Stock enquiry and availability buckets (ITM-012, ITM-013).** See section 1.2.
* **Reservations (ITM-014).** A reservation is demand-specific and quantity-bounded, and is **not**
  consumption – the stock stays where it is until an issue or despatch moves it. Over-reserving is
  refused outright unless `@AllowPartial = 1`, in which case the shortfall is reported.
  `REVALID` invalidates reservations whose material has become ineligible and raises a
  `core.ExceptionItem` for the demand owner; it never silently permits blocked, quarantined or
  expired material.
* **Stock movements and quality holds (ITM-015, ITM-016).** `MovementType` is closed; direction is
  derived from the type unless `@Direction` is given. A movement may never create a negative
  balance (`STERP-E-0015`, D-09) and may never enter a `HARD` or `FUTURE` period
  (`STERP-E-0009`). Adjustments, scrap and count postings are audited with before and after
  quantities. `inv.QualityHold` records the quantity, lot, reason, owner, outcome and release
  authority.
* **In-transit transfers (ITM-017).** `TRANSFER` moves stock into a `TRANSIT` location and
  `CONFIRM` receives it at the destination; ownership, traceability and total quantity are
  preserved, and transit stock is reported in `AV-QTY-INTRANSIT`, never as free.
* **Counting and approved adjustment (ITM-018).** `COUNTADJ` opens a count with an explicit
  cutoff, prints the sheets, takes counted and recounted quantities, accounts for the movements
  that happened between the cutoff and the count (`InterveningMovementQty`), and posts the
  **approved** variance as an adjustment movement. A count line never edits a balance directly.
* **Substitution (ITM-019).** A substitute is a proposal with a stated context
  (`ContextPurchase` / `Sales` / `Kit` / `Bom`), an approved ratio, validity dates and an approver.
  `RequiresCustomerAgreement` blocks the swap on a customer-specified item.
* **Obsolescence impact (ITM-010, ITM-020, ITM-022).** `ITMIMPCT IMPACT` shows the outstanding
  stock, reservations, orders, BOM lines, kits, holds, count lines and live serials before any
  lifecycle change. Referenced items are **retired, not deleted**; `DELCHECK` only permits deleting
  an unused draft.
* **Copy and bulk maintenance (ITM-021).** `ITEMCOPY PREVIEW` lists exactly which attributes would
  be carried across and which are excluded (balances, history, approvals); `BULKPREV` shows the
  affected records before `BULKAPPL` changes them.
* **Where-used and usage (ITM-023).** `ITEMENQ WHEREUSED` / `USAGE` and `inv.usp_ItemWhereUsed`
  cover BOM, kit, substitute and movement usage; cost columns appear only when the user's
  permission allows, otherwise a labelled gap.
* **Opening stock (ITM-024).** `STKMOVE OPENING` and the `OPENSTK` batch load quantity, ownership,
  location, condition, traceability **and** valuation evidence, with approval.
* **VAT class, customs and compliance (ITM-025..027).** `inv.ItemVatClass` is effective-dated per
  `Direction` (`SALE`/`PURCH`): a later classification applies from its own tax point and never
  rewrites documents already issued. `inv.ItemCustoms` holds a numeric commodity code
  (`CK_ItemCus_Cn`), country of origin, preference evidence, duty rate and export control.
  `inv.ItemPackaging` plus the UKCA, dangerous goods, REACH, WEEE and battery columns on
  `inv.Item` feed the RPT-019 report produced by `PKGRPT`.

## 8. How to demo

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-db.ps1 -Only "20-itm|21-itm|22-itm|25-itm|26-itm|27-itm|28-itm|65-itm|91-itm|procs\\inv|views\\inv"
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-cobol.ps1 -Module itm
powershell -NoProfile -ExecutionPolicy Bypass -File build\build-db.ps1 -NoDeploy -Tests -Only itm
```

1. **Item master** – *Stock Control ▸ Item master*, open `TUBE-AL6061-28X1.5`. Search for the
   supplier part `SUP-AL606128X1.5-01`: the item is found and the grid says it was found **by an
   alias**. Try to save the code `TUBE AL6061#1` – refused.
2. **Units** – on the same item press **Units**: `PK100` is 100 `EA`, and keying 2.5 boxes of
   `BOLT-M5X16-SS` returns "moves in multiples of 1 EA" with the rounded-up quantity offered for
   confirmation.
3. **Stock enquiry** – *Stock Control ▸ Stock enquiry*, item `BEARING-6902-2RS` at `RDG`:
   accepted stock in `STKA01` is eligible, the 120 units in `QUAR01` are physical but show
   *Quality condition quarantine*, and the customer-owned `DECAL-SET-ROAD` in `CUSTOWN` shows
   *Third party owned – in our custody*.
4. **Movement** – *Stock Control ▸ Stock movement*. Receive 50 into `STKA01` (balance rises by
   exactly 50 and a movement is written), then try to issue 9,999,999 – refused, *insufficient
   physical stock*. Try the same receipt dated 15 May 2026 – refused, period closed.
5. **Reservation** – reserve 10 for `SO-10042`; the free quantity drops by 10 while physical stays
   the same. Reserve more than is free – refused. Release, and the stock returns to free.
6. **Count** – *Stock Control ▸ Stock count*: open a count, print the sheets, enter a counted
   quantity, approve, and watch the variance post as an adjustment movement with its reason.
7. **Reports** – `STKSTAT`, `STKVALF`, `LOTTRACE` and `PKGRPT` (RPT-019) all land in `run\spool\`.

## Lot registration on receipt - defect fixed 2026-10-07

A goods receipt that named a lot wrote `inv.StockBalance` with that `LotCode` but created no
`inv.Lot` row, so the next issue of the same lot was refused with *"Lot <x> is not recorded for
item <y>"*. Two faults, both now fixed:

* `cobol\online\itm\LOTSER.cob` `2200-BIND-LOT` bound 17 parameters to the `NEWLOT` MERGE,
  which has 18 placeholders. `OriginCountry` and `CreatedBy` were one position out and the
  statement failed at execute time. It now binds `LS-ORIGIN-COUNTRY` to 17, `CA-USER` to 18 and
  sets `SQLIO-PARM-CNT` to 18.
* `cobol\online\itm\STKMOVE.cob` `0600-REGISTER-TRACE` called `LOTSER` and ignored the return
  code, so the failure was invisible and the balance was posted for a lot that did not exist. It
  now rejects the movement with `STK0005` (lot) or `STK0011` (serial) and reports LOTSER's own
  message, which keeps the rule that the traceability identity exists before the balance does.

`cobol\tests\itm\TLOTSER.cob` `2000-NEW-LOT-REGISTERED` covers it: `NEWLOT` for an unseen lot
returns 0 and the row is on `inv.Lot` afterwards. The test removes its own `ZZ-TEST-LOTSER%`
rows before and after. TLOTSER is 18 passed, TSTKMOVE 16 passed, TAVAILQ 16 passed.
