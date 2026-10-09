Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Despatch confirmation.  The pick list says what was found; this screen says what is actually
''' going out, and DESPATCH CONFIRM is the only thing that issues the stock, values the cost of
''' sales and raises the journal.  A confirmation that comes back 04, 06, 08, 12 or 16 leaves the
''' despatch reference, the COGS value and the journal id blank - a failed confirmation is never
''' displayed as shipped.
''' REQ: SO-012, SO-013, SO-022, COM-015, COM-024
''' </summary>
Public Class frmDespatchConfirm
    Inherits BaseForm

    Private Const DespatchColumn As String = "DespatchQty"

    Private ReadOnly _svc As New SalesOrderService()
    Private _lines As DataTable
    Private _confirmed As Boolean
    ''' <summary>The pick list and order on the screen; F10 acts on these, not on what is keyed since.</summary>
    Private _loadedPick As String = ""
    Private _loadedOrder As String = ""

    Public Sub New()
        InitializeComponent()
        GridHelper.Style(grdLines)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        txtDespatchDate.Text = Fmt.Date(SterpSession.Current.BusinessDate)
        txtPacks.Text = "1"
        txtWeight.Text = "0"
        If FormArgument <> "" Then txtPickRef.Text = FormArgument
        SetStatus("Key the pick list (F2 for the list) or the order number and press F5, then F10 to confirm the despatch.")
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadPick()
    End Sub

    Protected Overrides Function CanClose() As Boolean
        If _lines Is Nothing OrElse _confirmed Then Return True
        Return Confirm("Despatch quantities have been keyed but not confirmed.  Abandon them?")
    End Function

    Private Function DespatchDate() As Date
        Dim d As Date? = Fmt.ParseDate(txtDespatchDate.Text)
        Return If(d.HasValue, d.Value, SterpSession.Current.BusinessDate)
    End Function

    ''' <summary>Reads the pick list and offers the picked quantity as the despatch quantity -
    ''' the operator may reduce it, and the engine decides whether the result is allowed.  With no
    ''' pick list keyed, the order number is resolved to its pick list.</summary>
    Private Sub LoadPick()
        Dim pick As String = txtPickRef.Text.Trim().ToUpperInvariant()
        Dim keyedOrder As String = txtOrder.Text.Trim().ToUpperInvariant()
        ' a different order keyed over a loaded pick means "find that order's pick"
        If pick <> "" AndAlso pick = _loadedPick AndAlso keyedOrder <> "" AndAlso keyedOrder <> _loadedOrder Then pick = ""
        If pick = "" Then
            Dim order As String = keyedOrder
            If order = "" Then
                SetStatus("Key a pick list reference or an order number, or press F2 for the pick lists waiting to go out.",
                          "!  WARNING")
                Return
            End If
            pick = PickForOrder(order)
            If pick = "" Then Return
        End If
        txtPickRef.Text = pick
        ClearPick(False)
        Guard(Sub()
                  _lines = _svc.PickLines(pick)
                  If _lines.Rows.Count = 0 Then
                      _lines = Nothing
                      SetStatus("Pick list " & pick & " has no lines in " & SterpSession.Current.CompanyCode &
                                ".", "*** ERROR")
                      Return
                  End If
                  txtOrder.Text = Fmt.Str(_lines.Rows(0)("OrderNumber"))
                  lblCustomer.Text = Fmt.Str(_lines.Rows(0)("CustomerCode"))
                  _loadedPick = pick
                  _loadedOrder = txtOrder.Text.Trim().ToUpperInvariant()
                  _lines.Columns.Add(DespatchColumn, GetType(Decimal)).DefaultValue = 0D
                  For Each r As DataRow In _lines.Rows
                      r(DespatchColumn) = Fmt.Dec(r("PickedQty"))
                  Next
                  GridHelper.Bind(grdLines, _lines)
                  DecorateLines()
                  Outcome("", 0D, 0D, "NOT CONFIRMED - nothing has been despatched yet.")
                  _confirmed = False
                  SetStatus(_lines.Rows.Count & " pick line(s) for order " & txtOrder.Text &
                            ".  Adjust the despatch quantities, then press F10.", "")
              End Sub)
    End Sub

    ''' <summary>The pick list of an order: the only one is taken, several are offered, none is
    ''' explained - an order must be picked before it can be despatched.</summary>
    Private Function PickForOrder(ByVal order As String) As String
        Dim picks As DataTable = Nothing
        If Not Guard(Sub() picks = _svc.PicksForOrder(order)) Then Return ""
        If picks.Rows.Count = 1 Then Return Fmt.Str(picks.Rows(0)("PickListRef"))
        If picks.Rows.Count > 1 Then
            Return frmLookup.Pick(Me, SalesOrderService.PickListPickSql, order, "Pick lists for order " & order)
        End If
        ClearPick(True)
        SetStatus("Order " & order & " has no pick list waiting to go out at " & SterpSession.Current.SiteCode &
                  " - it must be picked before it can be despatched.", "*** ERROR")
        Return ""
    End Function

    ''' <summary>Removes the previous pick so nothing on view or actionable belongs to another one.</summary>
    Private Sub ClearPick(ByVal keepOrder As Boolean)
        _lines = Nothing
        _confirmed = False
        _loadedPick = ""
        _loadedOrder = ""
        If Not keepOrder Then txtOrder.Text = ""
        lblCustomer.Text = "-"
        GridHelper.Bind(grdLines, Nothing)
        Outcome("", 0D, 0D, "NOT CONFIRMED - nothing has been despatched yet.")
    End Sub

    Private Sub DecorateLines()
        GridHelper.Decorate(grdLines, "LineNum", "Line", "INT", 44)
        GridHelper.Decorate(grdLines, "OrderLineNum", "Order line", "INT", 70)
        GridHelper.Decorate(grdLines, "ItemCode", "Item", "CODE", 140)
        GridHelper.Decorate(grdLines, "RequiredQty", "Required qty", "QTY", 100)
        GridHelper.Decorate(grdLines, "PickedQty", "Picked qty", "QTY", 100)
        GridHelper.Decorate(grdLines, "ShortQty", "Short qty", "QTY", 94)
        GridHelper.Decorate(grdLines, DespatchColumn, "Despatch qty", "QTY", 104)
        GridHelper.Decorate(grdLines, "LotCode", "Lot", "CODE", 90)
        GridHelper.Decorate(grdLines, "SerialNo", "Serial", "CODE", 100)
        GridHelper.Decorate(grdLines, "WarehouseCode", "Warehouse", "CODE", 80)
        GridHelper.Decorate(grdLines, "LocationCode", "Location", "CODE", 80)
        GridHelper.Decorate(grdLines, "Status", "Pick status", "CODE", 80)
        For Each name As String In New String() {"ScheduleNo", "OrderNumber", "CustomerCode"}
            If grdLines.Columns.Contains(name) Then grdLines.Columns(name).Visible = False
        Next
        grdLines.ReadOnly = False
        For Each c As DataGridViewColumn In grdLines.Columns
            c.ReadOnly = (c.DataPropertyName <> DespatchColumn)
        Next
        GridHelper.FillWidth(grdLines)
    End Sub

    ''' <summary>The result panel.  Blank values plus a plain English outcome whenever the
    ''' engine did not confirm, so a failure can never read as a shipment.</summary>
    Private Sub Outcome(ByVal despatchRef As String, ByVal cogs As Decimal, ByVal journal As Decimal,
                        ByVal text As String)
        lblDespatchRef.Text = If(despatchRef = "", "-", despatchRef)
        lblCogs.Text = If(despatchRef = "", "-", Fmt.Money(cogs))
        lblJournal.Text = If(despatchRef = "", "-", journal.ToString("0"))
        lblOutcome.Text = text
    End Sub

    ''' <summary>F10 - DESPATCH CONFIRM.  Picking is not despatch; this is the issue.</summary>
    Protected Overrides Sub OnConfirm()
        grdLines.EndEdit()
        If _lines Is Nothing OrElse _lines.Rows.Count = 0 Then
            SetStatus("Load a pick list first (F5).", "!  WARNING")
            Return
        End If
        If txtPickRef.Text.Trim().ToUpperInvariant() <> _loadedPick OrElse
           txtOrder.Text.Trim().ToUpperInvariant() <> _loadedOrder Then
            SetStatus("The pick list or order has been changed since it was loaded - press F5 to load it first.", "!  WARNING")
            Return
        End If
        Dim total As Decimal = 0D
        For Each r As DataRow In _lines.Rows
            total += Fmt.Dec(r(DespatchColumn))
        Next
        If total <= 0D Then
            SetStatus("Key at least one despatch quantity.", "!  WARNING")
            Return
        End If
        If Not Confirm("Confirm the despatch of pick list " & txtPickRef.Text.Trim() & " for order " &
                       txtOrder.Text.Trim() & "?") Then Return
        Guard(Sub()
                  Dim ca As CommArea = BuildArea()
                  Dim res As CobolResult = _svc.Despatch(ca, "CONFIRM")
                  If Not ShowResult(res) Then
                      Outcome("", 0D, 0D, "NOT CONFIRMED (" & res.Severity & ") - " & res.Display &
                                          ".  Nothing has been despatched.")
                      SetStatus("Despatch refused: " & res.Display, If(res.IsHold, "## HOLD", "*** ERROR"))
                      Return
                  End If
                  _confirmed = True
                  Dim despatchRef As String = SalesOrderService.AreaText(ca, "DS-DESPATCH-REF")
                  Outcome(despatchRef, ca.GetDecimal("DS-COGS-GBP"), ca.GetDecimal("DS-JOURNAL-ID"),
                          "CONFIRMED - " & SalesOrderService.AreaText(ca, "DS-STATUS") & " on " &
                          SalesOrderService.AreaDate(ca, "DS-DESPATCH-DATE") & ".")
                  SetStatus("Despatch " & despatchRef & " confirmed; cost of sales " &
                            Fmt.Money(ca.GetDecimal("DS-COGS-GBP")) & ", journal " &
                            SalesOrderService.AreaText(ca, "DS-JOURNAL-NO") & ".", "OK")
              End Sub)
    End Sub

    Private Function BuildArea() As CommArea
        Dim ca As CommArea = _svc.NewDespatch(_loadedOrder, _loadedPick, lblCustomer.Text.Trim(),
                                              DespatchDate(), txtCarrier.Text.Trim(), txtCarrierRef.Text.Trim(),
                                              Fmt.Int(txtPacks.Text), Fmt.Dec(txtWeight.Text),
                                              txtIncoterms.Text.Trim(), txtIncoPlace.Text.Trim(),
                                              chkExport.Checked, txtDestination.Text.Trim(),
                                              txtEori.Text.Trim(), txtDeclRef.Text.Trim())
        Dim n As Integer = 0
        For Each r As DataRow In _lines.Rows
            Dim qty As Decimal = Fmt.Dec(r(DespatchColumn))
            If qty <= 0D Then Continue For
            n += 1
            If n > SalesOrderService.MaxLines Then Exit For
            ' The order line, schedule and bin of the pick - the pick line number is not the order line.
            _svc.AddDespatchLine(ca, n, Fmt.Int(r("OrderLineNum")), Fmt.Str(r("ItemCode")), Fmt.Str(r("ItemCode")),
                                 Fmt.Dec(r("PickedQty")), qty, "", Fmt.Str(r("LotCode")), Fmt.Str(r("SerialNo")),
                                 Fmt.Int(r("ScheduleNo")), Fmt.Str(r("WarehouseCode")), Fmt.Str(r("LocationCode")))
        Next
        Return ca
    End Function

    ''' <summary>F2 - the pick lists at this site with picked stock still to go out (GUI-004).</summary>
    Protected Overrides Sub OnLookup()
        Dim code As String = frmLookup.Pick(Me, SalesOrderService.PickListPickSql, "", "Pick lists waiting to be despatched")
        If code = "" Then Return
        txtPickRef.Text = code
        LoadPick()
    End Sub

    Private Sub LoadClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbLoad.Click
        LoadPick()
    End Sub

    Private Sub ConfirmClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbConfirm.Click
        OnConfirm()
    End Sub

    Private Sub CloseClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbClose.Click
        CloseForm()
    End Sub

End Class
