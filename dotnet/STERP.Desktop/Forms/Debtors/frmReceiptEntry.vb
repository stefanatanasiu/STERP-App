Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Cash receipt entry and allocation.  ar.usp_OpenItemsForAllocation offers what the money may
''' be applied to; RCPTALOC previews the residual, records the receipt and applies the clerk's
''' instructions, and reverses an allocation when it was wrong.  The arithmetic - discount,
''' tolerance, cross currency, the residual itself - is the engine's, never the screen's.
''' REQ: AR-009, AR-010, AR-011, AR-012, AR-019, COM-015, COM-024
''' </summary>
Public Class frmReceiptEntry
    Inherits BaseForm

    Private Const AllocColumn As String = "AllocateAmount"
    Private Const WriteOffColumn As String = "WriteOffAmount"
    Private Const WriteOffReasonColumn As String = "WriteOffReason"

    Private ReadOnly _svc As New DebtorsService()
    Private _items As DataTable
    Private _posted As Boolean
    Private _loadedCustomer As String = ""
    Private _loadedCurrency As String = ""

    Public Sub New()
        InitializeComponent()
        GridHelper.Style(grdItems)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        txtReceiptDate.Text = Fmt.Date(SterpSession.Current.BusinessDate)
        cboCurrency.SelectedItem = SterpSession.Current.BaseCurrency
        If cboCurrency.SelectedIndex < 0 Then cboCurrency.SelectedIndex = 0
        cboMethod.SelectedIndex = 0
        If FormArgument <> "" Then txtCustomer.Text = FormArgument
        SetStatus("Key the receipt header, press F5 for the open items, then F10 to post.")
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadItems()
    End Sub

    Protected Overrides Sub OnLookup()
        Dim code As String = frmLookup.Pick(Me, DebtorsService.CustomerPickSql, "", "Customer account")
        If code = "" Then Return
        txtCustomer.Text = code
        LoadItems()
    End Sub

    Protected Overrides Function CanClose() As Boolean
        If _items Is Nothing OrElse _posted OrElse Allocated() = 0D Then Return True
        Return Confirm("Allocations have been keyed but not posted.  Abandon them?")
    End Function

    Private Function Currency() As String
        Return Fmt.Str(cboCurrency.SelectedItem)
    End Function

    ''' <summary>The items this customer may be credited with, plus a column for the clerk.</summary>
    Private Sub LoadItems()
        _items = Nothing
        _loadedCustomer = ""
        _loadedCurrency = ""
        _posted = False
        GridHelper.Bind(grdItems, Nothing)
        For Each label As Label In Total.Values
            label.Text = ""
        Next
        Dim customer As String = txtCustomer.Text.Trim().ToUpperInvariant()
        If customer = "" Then
            SetStatus("Key a customer account first, or press F2 for the account list.", "!  WARNING")
            Return
        End If
        txtCustomer.Text = customer
        Guard(Sub()
                  Dim ds As DataSet = _svc.Allocatable(customer, Currency())
                  _items = ds.Tables(0)
                  _items.Columns.Add(AllocColumn, GetType(Decimal)).DefaultValue = 0D
                  _items.Columns.Add(WriteOffColumn, GetType(Decimal)).DefaultValue = 0D
                  _items.Columns.Add(WriteOffReasonColumn, GetType(String)).DefaultValue = ""
                  For Each r As DataRow In _items.Rows
                      r(AllocColumn) = 0D
                      r(WriteOffColumn) = 0D
                      r(WriteOffReasonColumn) = ""
                  Next
                  GridHelper.Bind(grdItems, _items)
                  GridHelper.Decorate(grdItems, AllocColumn, "Allocate " & Currency(), "MONEY", 100)
                  GridHelper.Decorate(grdItems, WriteOffColumn, "Write off " & Currency(), "MONEY", 90)
                  GridHelper.Decorate(grdItems, WriteOffReasonColumn, "Write off reason", "TEXT", 160)
                  grdItems.Columns(AllocColumn).DisplayIndex = 0
                  grdItems.Columns(WriteOffColumn).DisplayIndex = 1
                  grdItems.Columns(WriteOffReasonColumn).DisplayIndex = 2
                  grdItems.ReadOnly = False
                  For Each c As DataGridViewColumn In grdItems.Columns
                      c.ReadOnly = (c.DataPropertyName <> AllocColumn AndAlso c.DataPropertyName <> WriteOffColumn AndAlso
                                    c.DataPropertyName <> WriteOffReasonColumn)
                  Next
                  _posted = False
                  _loadedCustomer = customer
                  _loadedCurrency = Currency()
                  Dim note As String = ""
                  If ds.Tables.Count > 2 AndAlso ds.Tables(2).Rows.Count > 0 Then
                      Dim h As DataRow = ds.Tables(2).Rows(0)
                      note = "Balance " & Fmt.Money(Fmt.Dec(ServiceBase.Col(h, "BalanceGBP", 0D))) &
                             " | unapplied cash " & Fmt.Money(Fmt.Dec(ServiceBase.Col(h, "UnappliedCashGBP", 0D))) &
                             " | small difference tolerance " &
                             Fmt.Money(Fmt.Dec(ServiceBase.Col(h, "SmallDifferenceToleranceGBP", 0D))) & " | "
                  End If
                  SetStatus(note & grdItems.Rows.Count & " open item(s) in " & Currency() &
                            ".  Key the amounts (and any small difference to write off, with its reason), press Preview, then F10 to post.", "")
              End Sub)
    End Sub

    ''' <summary>Sum of what the clerk has keyed - only to decide whether there is anything to
    ''' send; what it is worth is the engine's arithmetic.</summary>
    Private Function Allocated() As Decimal
        Dim total As Decimal = 0D
        If _items Is Nothing Then Return total
        For Each r As DataRow In _items.Rows
            total += Fmt.Dec(r(AllocColumn)) + Fmt.Dec(r(WriteOffColumn))
        Next
        Return total
    End Function

    ''' <summary>Fills a RCPTALOC COMMAREA from the header and the keyed instructions.</summary>
    Private Function BuildArea() As CommArea
        Dim amount As Decimal = Fmt.Dec(txtAmount.Text)
        Dim d As Date? = Fmt.ParseDate(txtReceiptDate.Text)
        Dim ca As CommArea = _svc.NewReceipt(txtCustomer.Text.Trim(), If(d.HasValue, d.Value, SterpSession.Current.BusinessDate),
                                             Currency(), txtBankAccount.Text.Trim(), Fmt.Str(cboMethod.SelectedItem),
                                             amount, txtBankReference.Text.Trim(), txtExternalReference.Text.Trim())
        Dim n As Integer = 0
        For Each r As DataRow In _items.Rows
            Dim apply As Decimal = Fmt.Dec(r(AllocColumn))
            Dim writeOff As Decimal = Fmt.Dec(r(WriteOffColumn))
            If apply = 0D AndAlso writeOff = 0D Then Continue For
            n += 1
            If n > DebtorsService.MaxAllocations Then Exit For
            _svc.AddAllocation(ca, n, Fmt.Int(r("OpenItemId")), Fmt.Str(r("DocumentNumber")),
                               Fmt.Str(r("CurrencyCode")), apply, writeOff, Fmt.Str(r(WriteOffReasonColumn)).Trim())
        Next
        Return ca
    End Function

    ''' <summary>Shows the totals RCPTALOC put back in the COMMAREA.</summary>
    Private Sub ShowTotals(ByVal ca As CommArea)
        For Each kv As KeyValuePair(Of String, String) In TotalFields
            Total(kv.Key).Text = DebtorsService.AreaMoney(ca, kv.Value)
        Next
    End Sub

    ''' <summary>PREVIEW has no business effect - it only reports what posting would do.</summary>
    Private Sub PreviewClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbPreview.Click
        If Not Ready() Then Return
        Guard(Sub()
                  Dim ca As CommArea = BuildArea()
                  Dim res As CobolResult = _svc.RunReceipt(ca, "PREVIEW")
                  ShowTotals(ca)
                  If res IsNot Nothing AndAlso res.ReturnCode = 6 Then
                      ' A preview requests nothing - say what posting would do instead of the hold text.
                      ShowWarning(res.Message & " - posting this receipt would be held. Preview only: nothing " &
                                  "has been posted and no approval has been requested.", res.ReasonCode)
                      Return
                  End If
                  If Not ShowResult(res) Then Return
                  SetStatus("Preview only - nothing has been posted.  Residual " &
                            DebtorsService.AreaMoney(ca, "RA-RESIDUAL") & ", unallocated cash " &
                            DebtorsService.AreaMoney(ca, "RA-UNALLOCATED") & ".", "")
              End Sub)
    End Sub

    ''' <summary>F10 - records the receipt, then applies the instructions.  Allocation has no
    ''' second cash effect, so the two links are deliberately separate (AR-010).</summary>
    Protected Overrides Sub OnConfirm()
        If Not Ready() Then Return
        If Not Confirm("Post this receipt of " & DebtorsService.Amount(Fmt.Dec(txtAmount.Text), Currency()) &
                       " for " & txtCustomer.Text.Trim() & "?") Then Return
        Guard(Sub()
                  Dim ca As CommArea = BuildArea()
                  Dim res As CobolResult = _svc.RunReceipt(ca, "RECEIPT")
                  ShowTotals(ca)
                  If Not ShowResult(res) Then
                      SetStatus("The receipt was not recorded: " & res.Display, "*** ERROR")
                      Return
                  End If
                  _posted = True
                  If ca.GetInt("RA-ALLOC-CNT") > 0 Then
                      res = _svc.RunReceipt(ca, "ALLOCATE")
                      ShowTotals(ca)
                      If Not ShowResult(res) Then
                          SetStatus("Receipt " & ca.GetString("RA-RECEIPT-NO") &
                                    " was recorded but not allocated: " & res.Display, "!  WARNING")
                          Return
                      End If
                  End If
                  _posted = True
                  SetStatus("Receipt " & ca.GetString("RA-RECEIPT-NO") & " posted; applied " &
                            DebtorsService.AreaMoney(ca, "RA-TOT-APPLIED") & ", unallocated cash " &
                            DebtorsService.AreaMoney(ca, "RA-UNALLOCATED") & ".", "OK")
              End Sub)
    End Sub

    ''' <summary>Reverses an allocation that was applied to the wrong item; the history of the
    ''' account keeps both the original and the correction (AR-011).</summary>
    Private Sub CorrectClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbCorrect.Click
        Dim id As String = InputBox("Allocation number to correct:", "Allocation correction", "")
        If id.Trim() = "" Then Return
        Dim reason As String = InputBox("Why is allocation " & id.Trim() & " being corrected?",
                                        "Allocation correction", "")
        If reason.Trim() = "" Then
            SetStatus("An allocation correction must carry a reason.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim res As CobolResult = _svc.CorrectAllocation(Fmt.Int(id.Trim()), reason.Trim())
                  If Not ShowResult(res) Then Return
                  SetStatus("Allocation " & id.Trim() & " reversed; " &
                            _svc.Area.GetInt("RA-REVERSED-CNT") & " entry(ies) backed out, journal " &
                            _svc.Area.GetString("RA-JOURNAL-NO") & ".", "OK")
                  LoadItems()
              End Sub)
    End Sub

    Private Function Ready() As Boolean
        grdItems.EndEdit()
        If _posted Then
            SetStatus("This receipt has already been recorded. Press F5 to start another receipt.", "!  WARNING")
            Return False
        End If
        If _items Is Nothing Then
            SetStatus("Press F5 for the open items of this account first.", "!  WARNING")
            Return False
        End If
        If txtCustomer.Text.Trim().ToUpperInvariant() <> _loadedCustomer OrElse Currency() <> _loadedCurrency Then
            For Each label As Label In Total.Values
                label.Text = ""
            Next
            SetStatus("The customer or currency has changed. Press F5 to reload the open items before previewing or posting.",
                      "!  WARNING")
            Return False
        End If
        If Fmt.Dec(txtAmount.Text) <= 0D Then
            SetStatus("Key the gross amount of the receipt in " & Currency() & ".", "!  WARNING")
            Return False
        End If
        Return True
    End Function

    Private Sub ItemsClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbItems.Click
        LoadItems()
    End Sub

    Private Sub PostClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbPost.Click
        OnConfirm()
    End Sub

    Private Sub CloseClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbClose.Click
        CloseForm()
    End Sub

End Class
