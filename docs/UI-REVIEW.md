# UI review - classic look and feel pass

Date: 2026-10-07.  Scope: `dotnet\STERP.Desktop` (every form reachable from `core.MenuItem`),
`build\ui-smoke.ps1`, `docs\img\`.  Governing requirement: **COM-024** (Windows Classic house
style, keyboard driven, no cue that depends on colour alone).

## 1. How the screens were reviewed

`STERP.EXE` gained an undocumented QA switch, `QaScreens.vb`:

```
STERP.EXE /qa-screens <outdir> [company]
```

It signs on as SYSADM, selects a company/site, opens **every distinct `FormClass` /
`FormArgument` pair in `core.MenuItem`** as an MDI child, writes one PNG per screen with
`Control.DrawToBitmap`, inspects the live control tree and exits.  A background watchdog closes
any message box a screen raises so an unattended sweep can never stall.

`build\ui-smoke.ps1` builds the client, runs the sweep, **always stops `STERP.exe` by PID**
(it holds `bin\x86\STERPCOB.dll` open and would block every COBOL build) and summarises
`qa-report.tsv`:

```
powershell -NoProfile -ExecutionPolicy Bypass -File build\ui-smoke.ps1
powershell -NoProfile -ExecutionPolicy Bypass -File build\ui-smoke.ps1 -OutDir run\work\screens-stc -Company STC
```

The inspector reports: controls outside their parent, overlapping siblings, fonts that are not
MS Sans Serif 8.25 / Courier New, colours outside the Windows 2000 "Standard" palette,
`FlatStyle` that is not Standard or System, grids with no column headers / no columns / no rows,
push buttons with no accelerator, a screen with no tab order, a screen that states no function
key, a window with no caption, and any form that throws on open.

### Result

| sweep | screens captured | forms opened | forms that threw | defects reported |
| --- | --- | --- | --- | --- |
| before the fixes | 140 | 139 | 0 | **529** |
| after the fixes  | 140 | 139 | 0 | **57** (all "grid is empty / has no columns", see §4) |

Two sweeps were run, one per demo company: `CAL` (Caledon Assemblies, finance and trading data)
and `STC` (Stellar Components, the costing / job demo data).  Artefacts:
`run\work\screens\` and `run\work\screens-stc\`.

## 2. Forms reviewed

All 139 menu targets opened cleanly.  By family:

| family | screens | notes |
| --- | --- | --- |
| Shell / System | frmMain, frmLogin, frmSplash, frmAbout, frmLookup, frmMaintLauncher, frmApprovals, frmExceptions, frmAudit, frmJobMonitor, frmEngineDiag, frmSpoolViewer | MDI frame, sign-on, spool viewer |
| Generic maintenance | 54 `frmGenericMaint` definitions (CORE, FIN, ITM, BOM, KIT, PO, SO, AR, CST) | one screen, 54 `core.MaintDef` keys |
| Report launcher | 21 `frmReportLauncher` entries (module and single-report rows) | catalogue + parameter panel + run log |
| Finance | frmJournalEntry, frmJournalEnquiry, frmBankRecon, frmPeriodClose, frmVatReturn | |
| Debtors / Creditors | frmCustomerEnquiry, frmReceiptEntry, frmCreditControl, frmInvoiceRegister, frmPaymentRun, frmSupplierVerify | |
| Sales / Purchasing | frmSalesOrderEntry, frmDespatchConfirm, frmSalesInvoiceRun, frmPurchaseOrder, frmGoodsReceipt, frmRequisition | |
| Stock / BOM / Kits | frmItemMaster, frmStockEnquiry, frmStockMovement, frmStockCount, frmBomEditor, frmBomExplosion (+COMPARE), frmBomWhereUsed, frmKitMaint, frmKitTrial, frmKitExecution, frmKitGenealogy | |
| Costing | frmCostEnquiry, frmCostSet, frmCostRollup, frmJobCost (+WIP), 5 CST maintenance keys, 5 CST reports | closes todo `cst-verify-ui` |
| Cross-module | frmChainTrace | |

## 3. Defects found and fixed

| # | Defect | Where | Fix |
| --- | --- | --- | --- |
| 1 | **264 toolbars, menu bars and status bars drew in Segoe UI 9** - `ToolStrip` sets its own font in its constructor, so it never inherited the form font.  The most visible break of the period look: every window had a modern menu and status line. | all forms, `frmMain` menu bar included | `UiStyle.ApplyPalette` now forces `BodyFont` on every `ToolStrip` and, recursively, on every item and drop-down item it owns. |
| 2 | **Filter rows ran off the right edge** - `ListForm` laid filters out left to right with no wrapping, so later criteria were invisible and unreachable (`frmCostRollup` lost *Target price* and *Boundary*; also `frmBomExplosion`, `frmKitGenealogy`, `frmKitTrial`). | `ListForm` | `Reserve()` starts a new filter line and grows the filter panel when the next label/field pair would not fit. |
| 3 | **Empty grids were blank grey rectangles** - nothing told the operator whether the list was empty or still loading, and the only cue was the absence of colour. | every grid | `GridHelper.Style` paints `(no records - press F5 to refresh)` over an empty grid; the wording can be overridden through the grid's `Tag`.  A text cue, not a colour one (COM-024). |
| 4 | **Status line stuck on "BUSY / Working..."** - `BaseForm.Guard` reset the cursor but not the status line, so idle screens claimed to be busy (seen on `frmPurchaseOrder`, which lost its "F5 lists the open orders, F10 issues the order" hint). | `BaseForm` | `Busy(True)` remembers the line, `Guard` restores it in its `Finally`. |
| 5 | **Splash banner cut off** - the designer set `ClientSize` *before* `FormBorderStyle`, so the frame was taken out of the client area and the sunken progress line ("Starting transaction engine...") was never visible. | `frmSplash` | `ClientSize` is set after the border style in the designer and asked for again in `OnLoad`; the blue rule now spans the full width. |
| 6 | **Overlapping controls** - a seventh inspection field was drawn on top of *Disposition*; a line field was drawn under the *Validate* button; the *Method* caption ran under its combo; two buttons sat 7px into the grid. | `frmGoodsReceipt`, `frmJournalEntry`, `frmReceiptEntry`, `frmRequisition` | inspection block wraps into two columns in a taller group box; journal actions moved to their own row above the list; caption narrowed; grid moved down. |
| 7 | **Third column of fields off the window** - *Reference* and its box were drawn beyond the right edge. | `frmStockMovement` | client widened to 960, column pitch 400 -> 310, grid widened. |
| 8 | **Single-report menu rows showed "0 report(s) available"** - `ReportService.Definitions` only treated the argument as a report code when it contained `-` or `.`, so `FormArgument` values such as `CSTRP010`, `APAGED` or `AGEDDEBT` were passed as a *module* filter and matched nothing.  21 menu rows opened an empty launcher. | `STERP.Business\System\ReportService.vb` (minimal fix, required by the form defect) | the argument is matched against `ReportCode` first, then against the module, then falls back to the whole catalogue. |
| 9 | **No function keys stated on the MDI frame** | `frmMain` | right aligned `F1 Help  F2 Lookup  F3 Close  F5 Refresh  F10 Post` label on the toolbar, visible from every screen. |
| 10 | **Buttons without an accelerator** | `frmLogin`, `frmLookup` | `Cancel` -> `&Cancel`.  (Every other push button already carried one; the 63 buttons captioned `F2` are deliberate key hints.) |

Checked and found already correct: visual styles off, MS Sans Serif 8.25 body font, the grey
3D palette, `ToolStripRenderMode.System` with `ClassicRenderer`, the Courier New spool viewer,
the `(c) 1994-2026` about box, the four-panel status bar (company / site / period / user),
CUA keys F2/F3/F5/F10 in `BaseForm`, tab order on every screen, grid column headers, and the
non-colour severity cues `OK`, `BUSY`, `!  WARNING`, `## HOLD`, `*** ERROR` on the status line.

## 4. Remaining issues

1. **57 "grid is empty / has no columns" reports.**  Most are correct behaviour: an entry screen
   opens with no criteria keyed and the grid now says so in words.  They are left in the report
   deliberately - the sweep is also a data smoke test.
2. **Costing maintenance definitions `CST.ELEMENT`, `CST.NRV`, `CST.POLICY`, `CST.RATES` do not
   load.**  `core.MaintDef` and `core.MaintColumn` both hold rows, yet `frmGenericMaint` falls
   back to the generic "File maintenance" caption with no columns, while `CST.LANDED` on the same
   screen works.  The read behind the definition fails at run time.  Costing module data, not a
   forms defect - raised as todo `cst-maintdef-columns`.
3. **Demo data is split across companies.**  `cst.Job` only holds `STC` rows, so the Costing
   screens are empty under `CAL`.  Use `-Company STC` for a costing demo; this is why the
   harness takes a company argument.
4. The MDI child title bars are painted by the OS, so they follow the desktop theme rather than
   the Windows Classic scheme the client carries internally.  Nothing the application can change
   without owner drawing every frame.

## 5. Representative screenshots (`docs\img\`)

| file | screen |
| --- | --- |
| `main-mdi.png` | the MDI shell: database driven menu bar, classic toolbar with the function key legend, Journal Enquiry open with 66 records, status bar showing user / company / site / period / engine |
| `maintenance-core-param.png` | generic file maintenance, `CORE.PARAM`, 205 effective dated rows with the detail pane |
| `journal-entry.png` | journal entry: header, line fields, action row, staged lines |
| `sales-order-entry.png` | sales order entry with the price explanation and availability panes |
| `goods-receipt.png` | goods receipt and inspection, two column inspection block |
| `report-launcher-cst.png` | report launcher opened on a single report row (`CSTRP010`) |
| `job-cost-wip.png` | work in progress under company STC |
| `cost-sets.png` | cost sets maintenance |
| `spool-viewer.png` | Courier New spool file viewer |
