# STERP - Stefan's ERP: .NET client framework

> How to add services, forms, menu entries, maintenance screens and tests to the STERP
> Windows client. Read `docs\CONVENTIONS.md` first - this file only covers the .NET layer.
> The client is deliberately thin: **business rules live in COBOL and in stored procedures**,
> the .NET code orchestrates and displays.

---

## 1. Layout

| Project | Language | Output | Contains |
|---|---|---|---|
| `dotnet\STERP.Common` | C# | `STERP.Common.dll` | sign-on context, UK formatting, settings, run log |
| `dotnet\STERP.Data` | C# | `STERP.Data.dll` | `SqlHelper`, `BusinessRuleException` |
| `dotnet\STERP.CobolInterop` | C# | `STERP.CobolInterop.dll` | copybook parser, `CommArea`, `CobolGateway` |
| `dotnet\STERP.Business` | VB.NET | `STERP.Business.dll` | `ServiceBase` + one service class per use case |
| `dotnet\STERP.Desktop` | VB.NET | **`STERP.exe`** | `BaseForm`, `ListForm`, the shell and the framework forms |
| `dotnet\STERP.Tests` | C# | `STERP.Tests.exe` | the home-grown TAP harness |

Everything builds to `bin\x86` (`build\build-net.ps1`). All projects are legacy non-SDK
projects that import `dotnet\STERP.Legacy.props`; **project files use wildcards, so you never
have to edit a `.vbproj` to add a file**:

```
dotnet\STERP.Business\<Module>\XxxService.vb       -- picked up by **\*.vb
dotnet\STERP.Desktop\Forms\<Module>\frmXxx.vb      -- picked up by Forms\**\*.vb
dotnet\STERP.Desktop\Forms\<Module>\frmXxx.Designer.vb
dotnet\STERP.Tests\<Module>\XxxTests.cs            -- picked up by **\*.cs
```

Language level is **VB 15 / C# 7.3**. `Option Strict Off` is on by design.
Budget: **≤ ~900 lines of VB/C# per module** including designer files (CONVENTIONS 6).

---

## 2. Session context - `STERP.Common`

`SterpSession.Current` is a process-wide singleton filled at sign-on:

```vb
Dim s = SterpSession.Current
s.UserCode      ' SYSADM          s.DisplayName  s.WindowsLogin  s.IsSysAdm  s.IsImpersonating
s.CompanyCode   ' STC             s.CompanyName  s.BaseCurrency
s.SiteCode      ' RDG             s.SiteName     s.Nation
s.BusinessDate  ' Date            s.FiscalYear   s.PeriodNo      s.PeriodStatus   s.PeriodText
s.Access        ' List(Of CompanySite) the user may work in
s.NewCorrelationId()               ' idempotency key for a repeatable instruction (COM-015)
```

Inside a service you simply write `Company`, `Site`, `User` (see `ServiceBase`).

**Formatting - always use `Fmt`, never `ToString()`** (COM-024):

```vb
Fmt.Date(d)                 ' 04/10/2026
Fmt.Stamp(dt)               ' 04/10/2026 14:22:01
Fmt.Money(1234.5D)          ' £1,234.50
Fmt.Money(1234.5D, "EUR")   ' 1,234.50 EUR      (never a £ sign on a foreign amount)
Fmt.Qty(12.5D, "EA")        ' 12.5 EA           (six decimals, trailing zeros trimmed)
Fmt.Rate(r)  Fmt.Pct(p)  Fmt.Period(2026, 7)    ' 2026/07
Fmt.Yyyymmdd(d)             ' 20261004          (COBOL / SQLIO exchange format)
Fmt.ParseDate("4/10/26")    ' Date? - accepts dd/MM/yyyy, ddMMyyyy, yyyyMMdd
Fmt.Str(o)  Fmt.Dec(o)  Fmt.Int(o)  Fmt.Bool(o) ' DBNull-safe conversions
```

Settings come from `STERP.exe.config` `<appSettings>`, overridden by an optional
`bin\x86\STERP.INI`: `SterpConfig.Get/GetBool/GetInt`, `SterpConfig.SqlConnectionString`,
`SterpConfig.OdbcConnectionString`, `SterpConfig.RunDir`, `SterpConfig.CopyDir`.
`SterpConfig.ProductName` (`"STERP - Stefan's ERP"`) is the product name for title bars, the
splash, About and help screens - use it rather than a literal.
`SterpLog.Info/Warn/Error/Trace` writes `run\logs\STERP-yyyyMMdd.log`.

---

## 3. Data access - `SqlHelper`

Old Data Access Application Block style: static `Execute*` methods, no connection handling in
the caller. Anything without a space is treated as a stored procedure name; anything with a
space is ad-hoc text (use it only for framework reads, always parameterised).

```vb
' name/value pairs - the usual form
Dim t As DataTable = SqlHelper.ExecuteDataTable("ar.usp_ListOpenItems",
                                                "@CompanyCode", Company, "@CustomerCode", cust)

Dim ds As DataSet  = SqlHelper.ExecuteDataSet("core.usp_GetUserContext", "@UserCode", "ARCLERK")
Dim r  As DataRow  = SqlHelper.ExecuteRow("core.usp_CheckPermission", "@UserCode", User,
                                          "@PermissionCode", "AR.RECEIPT.POST",
                                          "@CompanyCode", Company, "@SiteCode", Site)
Dim n  As Integer  = SqlHelper.ExecuteNonQuery("ar.usp_PostReceipt", "@ReceiptId", id, "@UserCode", User)
Dim o  As Object   = SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM ar.OpenItem WHERE CompanyCode = @c",
                                             "@c", Company)
Using rd = SqlHelper.ExecuteReader("ar.usp_ListOpenItems", "@CompanyCode", Company)
    Do While rd.Read() : ... : Loop
End Using

' or an explicit SqlParameter array when you need types / sizes / output parameters
Dim p() As SqlParameter = SqlHelper.Params("@CompanyCode", Company, "@AsAt", asAt)
SqlHelper.ExecuteDataTable("ar.usp_AgedDebt", p)
```

A `NULL` or `""` value becomes `DBNull.Value`.

**Errors.** A stored procedure that `THROW`s `'STERP-E-0042: Credit limit exceeded by £1,250.00'`
arrives as a `BusinessRuleException` with `Code = "E-0042"`, `Severity = "E"c` and
`Message = "Credit limit exceeded by £1,250.00"`. Everything else is a plain `SqlException`.
Inside a form, `Guard(Sub() ... End Sub)` already turns both into a message box.

`SqlHelper.ObjectExists("ar.usp_Foo")` tells you whether another module has deployed yet.

---

## 4. COBOL interop - `CobolGateway` and `CommArea`

The gateway is the `EXEC CICS LINK` replacement. It serialises every call on a process-wide
lock (the GnuCOBOL runtime is not thread safe), fills the 400 byte `STCAHDR` header from the
session, and reads the outcome back out of it.

### 4.1 Writing the COMMAREA copybook

See CONVENTIONS 3.3. For a LINKable program `ARCRED` the copybook is `ARCREDCA.cpy`:

```cobol
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AC-BODY.
               10  AC-CUSTOMER      PIC X(10).
               10  AC-ORDER-VALUE   PIC S9(11)V99 COMP-3.
               10  AC-LIMIT         PIC S9(11)V99 COMP-3.
               10  AC-EXPOSURE      PIC S9(11)V99 COMP-3.
               10  AC-LINE-CNT      PIC 9(3).
               10  AC-LINES         OCCURS 50 TIMES.
                   15  AC-L-ITEM    PIC X(40).
                   15  AC-L-QTY     PIC S9(9)V9(6).
                   15  AC-L-VALUE   PIC S9(11)V99 COMP-3.
```

`build\build-cobol.ps1` copies copybooks to `bin\x86\copy`, where `CopybookParser` reads them.

### 4.2 Calling it from a service

```vb
Imports STERP.CobolInterop

Public Class CreditService
    Inherits ServiceBase

    ''' <summary>Credit check for a sales order (AR-012).</summary>
    Public Function Check(ByVal customer As String, ByVal value As Decimal,
                          ByVal lines As DataTable) As CobolResult

        Dim ca As CommArea = NewArea("ARCREDCA")        ' layout cached after the first call
        ca.SetString("AC-CUSTOMER", customer)
        ca.SetDecimal("AC-ORDER-VALUE", value)
        ca.SetInt("AC-LINE-CNT", Math.Min(lines.Rows.Count, 50))

        For i As Integer = 0 To Math.Min(lines.Rows.Count, 50) - 1
            ca.SetString("AC-L-ITEM(" & (i + 1) & ")", Fmt.Str(lines.Rows(i)("ItemCode")))
            ca.SetDecimal("AC-L-QTY(" & (i + 1) & ")", Fmt.Dec(lines.Rows(i)("Qty")))
            ca.SetDecimal("AC-L-VALUE(" & (i + 1) & ")", Fmt.Dec(lines.Rows(i)("LineValue")))
        Next

        '  program name, COMMAREA, CA-FUNCTION  [, correlation id for idempotency]
        Dim r As CobolResult = Link("ARCRED", ca, "CHECK", Session.NewCorrelationId())

        If r.Succeeded Then                              ' RC 00 or 04
            _exposure = ca.GetDecimal("AC-EXPOSURE")
            _limit = ca.GetDecimal("AC-LIMIT")
        End If
        Return r                                         ' the form decides what to show
    End Function
End Class
```

In the form:

```vb
Dim r As CobolResult = svc.Check(cust, value, lines)
If Not ShowResult(r) Then Return        ' shows error / warning / hold with the reason code
SetStatus("Credit available: " & Fmt.Money(svc.Headroom), "OK")
```

### 4.3 `CommArea` reference

| Member | Notes |
|---|---|
| `CommArea.For("ARCREDCA")` | parses (and caches) the copybook, returns an INITIALIZEd buffer |
| `GetString / SetString(name)` | `PIC X` trimmed on the right; numeric fields are converted |
| `GetDecimal / SetDecimal(name)` | zoned, `SIGN LEADING/TRAILING SEPARATE`, `COMP-3`, `COMP-5` |
| `GetInt / SetInt`, `GetDate / SetDate` | `GetDate` reads a `PIC 9(8)` holding `YYYYMMDD` |
| `Get(name) / Set(name, value)` | weakly typed, handy for data binding |
| `GetTable("AC-LINES", n)` | the first *n* occurrences as a `DataTable`, one column per child |
| `SetRow("AC-LINES", i, "AC-L-ITEM", code, "AC-L-QTY", q)` | writes one occurrence |
| `Has(name)`, `Field(name)`, `Length`, `Buffer` | layout enquiry and the raw bytes |
| `HexDump()` | classic 16-byte-per-line dump (also written to the log when `STERP.Trace=Y`) |

Field references: `"AC-L-QTY(3)"`, `"AC-LINES(3).AC-L-QTY"`, `"AC-L-QTY OF AC-LINES"`,
`"AC-L-QTY(3)"` - subscripts are 1-based like COBOL and are range checked.

### 4.4 `CobolResult`

`ReturnCode` 00 OK / 04 warning / 06 hold / 08 error / 12 severe / 16 not found,
plus `ReasonCode`, `Message`, `MessageCount`, `CorrelationId`, `Area`, and the helpers
`IsOk`, `IsWarning`, `IsHold`, `IsError`, `IsSevere`, `IsNotFound`, `Succeeded`,
`Severity`, `Display` (message with the reason code appended).
A program that is not in the generated directory comes back as **RC 16 / `PGM0001`** - never an
exception. `CobolGateway.ProgramNames()` / `ProgramCount()` expose the directory.

---

## 5. Adding a service

`dotnet\STERP.Business\<Module>\XxxService.vb`:

```vb
Imports STERP.Common

''' <summary>What it does, and the requirement it serves.</summary>
Public Class ReceiptService
    Inherits ServiceBase

    Public Function OpenItems(ByVal customer As String) As DataTable
        Return Query("ar.usp_ListOpenItems", "@CompanyCode", Company, "@CustomerCode", customer)
    End Function

    Public Function Post(ByVal receiptId As Integer) As String
        Try
            Exec("ar.usp_PostReceipt", "@ReceiptId", receiptId, "@UserCode", User,
                 "@CorrelationId", Session.NewCorrelationId())
            Return ""
        Catch ex As STERP.Data.BusinessRuleException
            Return ex.Message & "  (" & ex.Code & ")"
        End Try
    End Function
End Class
```

`ServiceBase` gives you `Query`, `QuerySet`, `QueryRow`, `Exec`, `Scalar`, `Deployed(name)`,
`Col(row, name, default)`, `NewArea`, `Link`, and the `Session` / `Company` / `Site` / `User`
shortcuts. **No business rule may live here** - only orchestration and display shaping.

---

## 6. Adding a form

Two files in `dotnet\STERP.Desktop\Forms\<Module>\`. The code file holds the behaviour, the
`.Designer.vb` holds `InitializeComponent` exactly as the WinForms designer writes it.

`frmReceiptEntry.vb`:

```vb
Imports STERP.Business
Imports STERP.Common

''' <summary>Cash receipt entry (AR-015).</summary>
Partial Public Class frmReceiptEntry
    Inherits BaseForm                      ' classic look + F-keys + status line

    Private ReadOnly _svc As New ReceiptService()

    Public Sub New()
        InitializeComponent()
        GridHelper.Style(grdItems)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        LoadData()
    End Sub

    Private Sub LoadData()
        Guard(Sub()                                       ' busy cursor + error handling
                  GridHelper.Bind(grdItems, _svc.OpenItems(txtCustomer.Text))
                  SetStatus(grdItems.Rows.Count & " open item(s).", "")
              End Sub)
    End Sub

    Protected Overrides Sub OnRefreshKey()                 ' F5
        LoadData()
    End Sub

    Protected Overrides Sub OnLookup()                     ' F2
        If Me.ActiveControl Is txtCustomer Then
            Dim c As String = frmLookup.Pick(Me, "ar.usp_LookupCustomer", "", "Customer")
            If c <> "" Then txtCustomer.Text = c
        Else
            MyBase.OnLookup()
        End If
    End Sub

    Protected Overrides Sub OnConfirm()                    ' F10
        If Not Confirm("Post this receipt?") Then Return
        Guard(Sub()
                  Dim msg As String = _svc.Post(_receiptId)
                  If msg <> "" Then ShowError(msg, "") Else SetStatus("Receipt posted.", "OK")
              End Sub)
    End Sub

    Protected Overrides Function CanClose() As Boolean     ' F3 / Esc
        Return Not _dirty OrElse Confirm("Abandon the changes on this screen?")
    End Function
End Class
```

`frmReceiptEntry.Designer.vb`:

```vb
Partial Public Class frmReceiptEntry
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer
    Friend WithEvents lblCustomer As Label
    Friend WithEvents txtCustomer As TextBox
    Friend WithEvents grdItems As DataGridView

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.lblCustomer = New Label()
        Me.txtCustomer = New TextBox()
        Me.grdItems = New DataGridView()
        Me.SuspendLayout()
        Me.lblCustomer.Text = "&Customer:"
        Me.lblCustomer.SetBounds(8, 11, 70, 16)
        Me.txtCustomer.SetBounds(80, 8, 110, 20)
        Me.txtCustomer.BorderStyle = BorderStyle.Fixed3D
        Me.grdItems.SetBounds(8, 34, 700, 300)
        Me.grdItems.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        Me.ClientSize = New Size(720, 400)
        Me.Controls.AddRange(New Control() {Me.lblCustomer, Me.txtCustomer, Me.grdItems})
        Me.Name = "frmReceiptEntry"
        Me.Text = "Cash Receipt Entry"
        Me.ResumeLayout(False)
    End Sub
End Class
```

**Docking order matters.** `BaseForm` adds its status line last so that the docking pass gives
it the bottom edge; if you use `DockStyle.Fill`, add that control to `Me.Controls` **first** and
the docked strips afterwards (see `frmGenericMaint.Designer.vb`).

### 6.1 `BaseForm` members

| Member | Purpose |
|---|---|
| `FormArgument` | `core.MenuItem.FormArgument` of the entry that opened the form |
| `OnLookup()` F2, `OnRefreshKey()` F5, `OnConfirm()` F10, `OnHelpKey()` F1, `CanClose()` F3/Esc | override what you need |
| `SetStatus(text)` / `SetStatus(text, cue)` | status line; the cue is **text** (`OK`, `!  WARNING`, `*** ERROR`, `## HOLD`) so status is never colour-only (COM-024) |
| `Busy(True/False)` | wait cursor |
| `Info`, `ShowError`, `ShowWarning`, `ShowHold`, `Confirm` | message boxes that always print the reason code |
| `ShowResult(CobolResult)` | picks the right box from the return code; returns True when work may continue |
| `Guard(Sub() ... End Sub)` | busy cursor + `BusinessRuleException` / `Exception` to a message box |

### 6.2 `ListForm` - enquiry screens for free

Inherit `ListForm` instead of `BaseForm` and you get a toolbar, a filter row and a styled grid:

```vb
Public Class frmOpenItems
    Inherits ListForm
    Private cboStatus As ComboBox

    Public Sub New()
        Me.Text = "Open Items"
    End Sub

    Protected Overrides Sub BuildFilters()
        cboStatus = FilterList("&Status:", 110, "OPEN", "PAID", "ALL")
        AddHandler cboStatus.SelectedIndexChanged, Sub(s, e) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()          ' between Refresh and Close
        AddSeparator()
        AddTool("Allocate (F10)", "POST", AddressOf AllocateClicked)
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub() Populate(_svc.OpenItems(Company, Convert.ToString(cboStatus.SelectedItem))))
    End Sub
End Class
```

`FilterEdit(caption, width)`, `FilterList(caption, width, items...)`, `AddTool(text, glyph,
handler)`, `AddSeparator()`, `Populate(DataTable)`, `Grid`, `GridHelper.Selected(Grid, "Col")`.
Glyph names: `NEW SAVE RETIRE REFRESH FIND PRINT POST REJECT RUN CLOSE ENGINE`.

### 6.3 Grids

```vb
GridHelper.Style(grd)                               ' classic flat headers, 17px rows, read only
GridHelper.Bind(grd, table)                         ' binds and applies UK formats by column type
GridHelper.Decorate(grd, "Amount", "Net £", "MONEY", 110)
GridHelper.FillWidth(grd)                           ' after Decorate - stretch the last column
Dim id = GridHelper.Selected(grd, "ReceiptId")      ' Nothing when no current row
Dim row = GridHelper.SelectedRow(grd)
```
Data types: `TEXT CODE INT DECIMAL MONEY QTY RATE PCT DATE STAMP BOOL`.
`Bind` already stretches the last column, so you only need `FillWidth` when you have set
explicit widths with `Decorate` afterwards. `ListForm.Populate` does both for you.

---

## 7. Adding a menu entry

Seed `core.MenuItem` in `db\seed\NN-<mod>-menu.sql` (MERGE, re-runnable). The shell builds the
menu bar from `core.usp_UiMenuLoad`, which hides entries whose `PermissionCode` the user does
not hold.

```sql
MERGE core.MenuItem AS t USING (VALUES
    ('AR',             NULL,   '&Sales Ledger',        NULL,              NULL,  NULL,                 40, NULL),
    ('AR.RECEIPT',     'AR',   'Cash &Receipt Entry...','frmReceiptEntry', NULL,  'AR.RECEIPT.POST',    10, 'Ctrl+E'),
    ('AR.CUSTOMER',    'AR',   '&Customers...',        'frmGenericMaint', 'AR.CUSTOMER', 'AR.CUST.MAINT', 20, NULL)
  ) AS s (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut)
  ON t.MenuCode = s.MenuCode
WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption,
     FormClass = s.FormClass, FormArgument = s.FormArgument, PermissionCode = s.PermissionCode,
     SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
WHEN NOT MATCHED BY TARGET THEN INSERT (MenuCode, ParentMenuCode, Caption, FormClass,
     FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
     VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument,
             s.PermissionCode, s.SortOrder, s.Shortcut, 1);
```

Rules:
* `MenuCode` is `<MODULE>.<SCREEN>`; a top level bar entry has `ParentMenuCode = NULL`.
* Top level `SortOrder` is fixed by `db\seed\07-core-numbering-terms.sql`: FILE 10, SYSTEM 20,
  FIN 30, AR 40, AP 50, SO 60, PO 70, ITM 80, BOM 90, KIT 100, CST 110, RPT 120, WINDOW 130,
  HELP 140. **Never re-number these** - Window and Help must stay last on the bar. Your module
  owns the numbers inside its own drop-down only.
* `FormClass` is just the VB class name - the shell resolves `STERP.Desktop.<FormClass>` by
  reflection and re-activates an already open window instead of opening a second copy.
* `FormArgument` reaches the form as `BaseForm.FormArgument`.
* `FormClass = NULL` plus `FormArgument` starting with `$` is a **built-in shell command**:
  `$COMPANY $RUNAS $PRINTSETUP $EXIT $CASCADE $TILEH $TILEV $ARRANGE $CLOSEALL $HELP $ABOUT`.
* Use a unique `&` accelerator inside each drop-down.
* Give a `PermissionCode` only if the code really exists in `core.Permission`, otherwise the
  entry disappears for everybody. `core.usp_UiMenuLoad` also drops a branch that the permission
  filter has emptied, so a drop-down never appears with nothing under it; a top level entry
  with no children at all is kept for you to fill in and simply not drawn by the shell.

---

## 7a. Adding a report

Register the batch program in `core.ReportDef` (`db\seed\NN-<mod>-reports.sql`) and let the
user reach it from **Reports > Run Report...** (`frmReportLauncher`). You do not write a form.

```sql
MERGE core.ReportDef AS t USING (VALUES
    ('AR.AGED', 'ar', 'Aged debt analysis', 'ARAGED', 'CO,ASAT,MODE', 'RPT.RUN',
     'Aged debtors at a date, 30/60/90 day buckets.')
  ) AS s (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, Description)
  ON t.ReportCode = s.ReportCode
WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName,
     ParamSpec = s.ParamSpec, PermissionCode = s.PermissionCode, Description = s.Description,
     IsActive = 1
WHEN NOT MATCHED BY TARGET THEN INSERT (ReportCode, Module, Title, ProgramName, ParamSpec,
     PermissionCode, Description, IsActive, CreatedBy)
     VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode,
             s.Description, 1, 'SEED');
```

* `ProgramName` is the batch EXE in `bin\x86` **without** `.exe` (`ARAGED` -> `ARAGED.exe`).
* `ParamSpec` is a comma separated list of parameter keys. The launcher draws one labelled box
  per key and pre-fills the ones it knows: `CO SITE USER ASAT FROM TO PERIOD MODE`.
* The launcher runs `ARAGED.exe CO=STC ASAT=20261031 MODE=PREVIEW` - keys upper cased, blank
  values dropped, values containing a space wrapped in double quotes
  (`ReportService.BuildArguments`).
* `STERP_RUN` and `STERP_ODBC` are set in the child environment.
* The last `STERP-I-SPOOL <full path>` line of the console log names the spool file
  (`ReportService.ParseSpool`); the launcher opens it in `frmSpoolViewer` automatically.
* Every run is recorded in `core.JobRun`. Map the return code with
  `ReportService.JobStatus`: 0 `OK`, 4 `WARNING`, 8/12 `FAILED`, 16+ `ABENDED` - those are the
  only values `CK_JobRun_St` allows.

```vb
Dim svc As New ReportService()
Dim defs As DataTable = svc.Definitions("ar")        ' permission filtered
Dim args As New Dictionary(Of String, String)
args("CO") = SterpSession.Current.CompanyCode
args("ASAT") = Fmt.Yyyymmdd(SterpSession.Current.BusinessDate)
Dim rr As ReportRun = svc.Run("AR.AGED", "ARAGED", args)
' rr.ReturnCode  rr.Succeeded (<= 4)  rr.SpoolFile  rr.LogFile  rr.Output  rr.JobRunId
```

A batch program that is not installed comes back as `ReturnCode = 16` with a message - never
an exception.

---

## 8. Adding a generic maintenance screen

Simple masters do **not** get a hand written form. Register them in `core.MaintDef` /
`core.MaintColumn` (`db\seed\NN-<mod>-maintdefs.sql`) and point a menu entry at
`frmGenericMaint` with `FormArgument` = the `MaintCode`. See `db\seed\06-core-maintdefs.sql`
for a complete worked example.

```sql
-- the screen
INSERT core.MaintDef (MaintCode, Title, ModuleCode, SchemaName, TableName,
                      CompanyScoped, RetireColumn, PermissionCode, HelpText)
VALUES ('AR.CUSTOMER', 'Customers', 'ar', 'ar', 'Customer', 1, 'IsActive', 'AR.CUST.MAINT',
        'Sales ledger accounts. Retiring an account stops new invoices.');

-- its columns  (SortOrder drives both the grid order and the edit panel order)
INSERT core.MaintColumn (MaintCode, ColumnName, Caption, DataType, SortOrder, Width,
                         IsKey, IsRequired, IsReadOnly, IsInList, IsInEdit, IsSearchable,
                         LookupProc, LookupArg)
VALUES ('AR.CUSTOMER','CompanyCode','Company','CODE',10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
       ('AR.CUSTOMER','CustomerCode','Account','CODE',20,100,1,1,0,1,1,1,NULL,NULL),
       ('AR.CUSTOMER','Name','Name','TEXT',30,240,0,1,0,1,1,1,NULL,NULL),
       ('AR.CUSTOMER','CreditLimit','Credit limit','MONEY',40,110,0,0,0,1,1,0,NULL,NULL),
       ('AR.CUSTOMER','IsActive','In use','BOOL',90,60,0,0,0,1,1,0,NULL,NULL);
```

| Column | Meaning |
|---|---|
| `DataType` | `TEXT CODE INT DECIMAL MONEY QTY RATE PCT DATE BOOL` - drives the editor and the grid format |
| `IsKey` | part of the business key; only editable while adding |
| `IsInList` / `IsInEdit` | show in the grid / in the edit panel |
| `IsSearchable` | included in the Find box (text and code columns only) |
| `IsReadOnly` | displayed but never written |
| `LookupProc` / `LookupArg` | F2 lookup; `core.usp_UiLookup` with another `MaintCode` works for any registered master |
| `RetireColumn` on `MaintDef` | the bit column cleared by **Retire** - there is **no hard delete** |

Safety: `core.usp_UiMaintList` / `core.usp_UiMaintSave` build their SQL **only** from
`core.MaintColumn` (every identifier through `QUOTENAME`, every value an `sp_executesql`
parameter), skip columns the table does not have, check `core.usp_CheckPermission` and write
`core.AuditLog` for every add, change and retire.

From .NET the same contract is available as `MaintService`:

```vb
Dim svc As New MaintService()
svc.LoadDefinition("AR.CUSTOMER")             ' svc.Definition, svc.Columns
Dim rows = svc.List("AR.CUSTOMER", "ACME")
Dim msg = svc.Save("AR.CUSTOMER", "ADD", values)   ' "" = saved;  ADD / CHANGE / RETIRE
```

---

## 9. Adding a test

`dotnet\STERP.Tests\<Module>\XxxTests.cs` - public static methods tagged `[StTest]`:

```csharp
using STERP.Business;
using STERP.CobolInterop;

namespace STERP.Tests
{
    public static class CreditTests
    {
        [StTest("credit: an order over the limit is held", Reqs = "AR-012", Ac = "AC-021")]
        public static void OverLimit()
        {
            if (!STERP.Data.SqlHelper.ObjectExists("ar.Customer")) St.Skip("AR not deployed yet");
            CommArea ca = CommArea.For("ARCREDCA");
            ca.SetString("AC-CUSTOMER", "C0001");
            ca.SetDecimal("AC-ORDER-VALUE", 50000m);
            CobolResult r = CobolGateway.Link("ARCRED", ca, "CHECK");
            St.Eq(6, r.ReturnCode, "an order over the limit must come back on hold");
            St.Eq("CRD0001", r.ReasonCode, "reason code");
            St.Eq(1250.00m, ca.GetDecimal("AC-EXPOSURE"), "exposure");
        }
    }
}
```

Assertions: `St.Eq(expected, actual, what)`, `St.True(cond, what)`, `St.Contains(needle,
haystack, what)`, `St.Fail(msg)`, `St.Skip(reason)` (reported as `ok n - ... # SKIP ...`).

```
build\build-net.ps1
bin\x86\STERP.Tests.exe                 all tests, TAP output, exit code = failures
bin\x86\STERP.Tests.exe /filter:credit  regex over the test name, method or class
build\run-tests.ps1 -Layer net -Filter credit
```

---

## 10. UI style rules (COM-024)

* **No visual styles.** `Program.Main` never calls `Application.EnableVisualStyles`; the shell
  installs `ClassicRenderer` so menus and toolbars stay grey with a navy highlight.
* **Classic palette.** Windows 10/11 paint `COLOR_BTNFACE` almost white and `COLOR_HIGHLIGHT`
  bright blue, which breaks the period look, so the shell carries its own Windows 2000 scheme
  on `UiStyle`. Use these, never `SystemColors`:

  | Member | Colour | Used for |
  |---|---|---|
  | `UiStyle.Face` | `#D4D0C8` | forms, panels, group boxes, menu/tool/status bars, grid headers |
  | `UiStyle.FaceText` | `#000000` | all caption and field text |
  | `UiStyle.WindowFace` | `#FFFFFF` | editable fields, lists, grid cells, multi-line panes |
  | `UiStyle.Chosen` / `ChosenText` | `#000080` / white | selected grid row, selected menu item |
  | `UiStyle.Shadow` / `Workspace` | `#808080` | grid lines, the MDI workspace |
  | `UiStyle.Disabled` | `#808080` | greyed text |

  `BaseForm` calls `UiStyle.ApplyPalette(Me)` in `OnShown`, so anything the designer or `Load`
  created is repainted for you. Build controls in code with
  `UiStyle.Caption/Edit/Picker/Tick/Push/Box` and they are already correct. Do **not** use
  `FlatStyle.System` - the system renderer ignores `BackColor` and paints the current Windows
  theme colour; `ApplyPalette` forces `FlatStyle.Standard` on buttons, group boxes and ticks.
* A **read-only single line** box is greyed (`Face`); a **multi-line** pane (log, spool file,
  hex dump) stays a white document window.
* Microsoft Sans Serif 8.25pt (`UiStyle.BodyFont`), Courier New for listings
  (`UiStyle.FixedFont`), `BorderStyle.Fixed3D` on inputs.
* Every field has a label ending in `:` with an `&` accelerator. Grids are read only; editing
  happens in an edit panel or a dialog.
* MDI children only; one window per screen, re-activated rather than duplicated. `BaseForm`
  shrinks a child that is bigger than the workspace so nothing opens off-screen.
* F1 help, F2 lookup, F3/Esc close, F5 refresh, F10 confirm/post - wire them through the
  `BaseForm` overrides, never with a private `KeyDown` handler.
* Dates `dd/MM/yyyy`, money `£#,##0.00` (foreign currency as `1,234.50 EUR`), quantities to six
  decimals with the unit, percentages with `%`. Always via `Fmt`.
* **Status must never be colour alone** - put the state in text (`HELD`, `*** ERROR`,
  `OVERDUE 32 days`) and use colour only as reinforcement.
* `UiStyle.Glyph(name)` draws toolbar images in code - the client ships no image files.
* `run\work\qa-screens.ps1` starts the client, walks the menus and writes PNGs of every screen
  to `run\work\screens` for a look-and-feel review. It always stops the client by PID, because
  a running `STERP.exe` holds a lock on `STERPCOB.dll` and blocks `build-cobol.ps1`.

---

## 11. Build and run

```
build\build-net.ps1 [-Debug] [-Rebuild]      msbuild dotnet\STERP.sln, Release|x86 -> bin\x86
build\run-tests.ps1 -Layer net [-Filter re]  runs bin\x86\STERP.Tests.exe
bin\x86\STERP.exe                            the client (x86, .NET Framework 4.8)
```

`STERP.exe.config` keys: `STERP.Sql`, `STERP.Odbc`, `STERP.Trace` (`Y` writes COMMAREA hex
dumps to `run\logs`), `STERP.CommandTimeout`, `STERP.RunDir`, `STERP.CopyDir`. An optional
`bin\x86\STERP.INI` overrides them per workstation.

Sign-on uses `core.usp_GetUserContext`. While the security module is being installed the client
degrades to reading `core.AppUser`/`core.Company` directly and finally to a stand-alone SYSADM
context, so the shell always starts (see `docs\tech-debt\net.md`).
