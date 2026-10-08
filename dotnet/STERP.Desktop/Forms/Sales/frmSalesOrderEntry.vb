Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Sales order entry.  The header and the lines are read from so.SalesOrder; every figure on
''' the screen is the engine's: PRICING chooses and explains the price, SODISC runs the discount,
''' allowance, charge and VAT sequence, SOVAT decides the liability, SOMARGN measures the margin,
''' SOAVAIL answers availability by labelled source and SOACCEPT validates, accepts and releases.
''' Release, fulfilment and billing state - and the reason for a hold - are shown as words, never
''' as colour alone, and an expected receipt is never presented as a guaranteed allocation.
''' REQ: SO-002, SO-003, SO-004, SO-005, SO-006, SO-009, SO-010, SO-011, COM-015, COM-024
''' </summary>
Public Class frmSalesOrderEntry
    Inherits BaseForm

    Private ReadOnly _svc As New SalesOrderService()
    Private _lines As DataTable

    Public Sub New()
        InitializeComponent()
        GridHelper.Style(grdLines)
        GridHelper.Style(grdWhy)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        txtOrderDate.Text = Fmt.Date(SterpSession.Current.BusinessDate)
        txtCurrency.Text = SterpSession.Current.BaseCurrency
        If FormArgument <> "" Then txtOrder.Text = FormArgument
        SetStatus("Key an order number (F2 for the list) and press F5.  Dates are dd/MM/yyyy.")
        If txtOrder.Text <> "" Then LoadOrder()
    End Sub

    Private Function Currency() As String
        Dim c As String = txtCurrency.Text.Trim().ToUpperInvariant()
        Return If(c = "", SterpSession.Current.BaseCurrency, c)
    End Function

    Private Function OrderDate() As Date
        Dim d As Date? = Fmt.ParseDate(txtOrderDate.Text)
        Return If(d.HasValue, d.Value, SterpSession.Current.BusinessDate)
    End Function

    ' ------------------------------------------------------------------ load and display

    Protected Overrides Sub OnRefreshKey()
        LoadOrder()
    End Sub

    Private Sub LoadOrder()
        Dim order As String = txtOrder.Text.Trim().ToUpperInvariant()
        If order = "" Then
            SetStatus("Key an order number, or press F2 for the list of orders.", "!  WARNING")
            Return
        End If
        txtOrder.Text = order
        Guard(Sub()
                  Dim h As DataRow = _svc.Header(order)
                  If h Is Nothing Then
                      SetStatus("Order " & order & " is not on file in " & SterpSession.Current.CompanyCode &
                                ".", "*** ERROR")
                      Return
                  End If
                  ShowHeader(h)
                  _lines = _svc.Lines(order)
                  GridHelper.Bind(grdLines, _lines)
                  DecorateLines()
                  Totals()
              End Sub)
    End Sub

    Private Sub ShowHeader(ByVal h As DataRow)
        txtCustomer.Text = Fmt.Str(ServiceBase.Col(h, "CustomerCode", ""))
        txtCustomerRef.Text = Fmt.Str(ServiceBase.Col(h, "CustomerOrderRef", ""))
        txtSalesperson.Text = Fmt.Str(ServiceBase.Col(h, "SalespersonCode", ""))
        txtOrderDate.Text = Fmt.Date(ServiceBase.Col(h, "OrderDate", SterpSession.Current.BusinessDate))
        txtCurrency.Text = Fmt.Str(ServiceBase.Col(h, "CurrencyCode", SterpSession.Current.BaseCurrency))
        txtTerms.Text = Fmt.Str(ServiceBase.Col(h, "TermsCode", ""))
        txtIncoterms.Text = Fmt.Str(ServiceBase.Col(h, "IncotermsRule", ""))
        txtIncoPlace.Text = Fmt.Str(ServiceBase.Col(h, "IncotermsNamedPlace", ""))
        txtDeliveryRef.Text = Fmt.Str(ServiceBase.Col(h, "DeliveryAddressRef", ""))
        lblOrderType.Text = Fmt.Str(ServiceBase.Col(h, "OrderType", "-"))
        lblCustomerName.Text = Fmt.Str(ServiceBase.Col(h, "CustomerName", "-"))
        ShowState(Fmt.Str(ServiceBase.Col(h, "ReleaseStatus", "")), Fmt.Str(ServiceBase.Col(h, "FulfilmentStatus", "")),
                  Fmt.Str(ServiceBase.Col(h, "BillingStatus", "")), Fmt.Str(ServiceBase.Col(h, "HoldReason", "")))
        lblDocNet.Text = SalesOrderService.Amount(Fmt.Dec(ServiceBase.Col(h, "NetAmount", 0D)), Currency())
    End Sub

    ''' <summary>COM-024 - the state is text, and the hold reason is spelled out in words.</summary>
    Private Sub ShowState(ByVal release As String, ByVal fulfil As String, ByVal billing As String,
                          ByVal holdReason As String)
        lblRelease.Text = SalesOrderService.StatusText(release, "")
        lblFulfil.Text = SalesOrderService.StatusText(fulfil, "")
        lblBilling.Text = SalesOrderService.StatusText(billing, "")
        lblHold.Text = If(holdReason.Trim() = "", "(none - the order is not on hold)", "## HOLD - " & holdReason.Trim())
    End Sub

    Private Sub DecorateLines()
        Dim cur As String = Currency()
        GridHelper.Decorate(grdLines, "LineNum", "Line", "INT", 44)
        GridHelper.Decorate(grdLines, "ItemCode", "Item", "CODE", 130)
        GridHelper.Decorate(grdLines, "Description", "Description", "TEXT", 170)
        GridHelper.Decorate(grdLines, "AcceptedQty", "Quantity", "QTY", 92)
        GridHelper.Decorate(grdLines, "UomCode", "Unit", "CODE", 44)
        GridHelper.Decorate(grdLines, "UnitPrice", "Unit price " & cur, "RATE", 94)
        GridHelper.Decorate(grdLines, "PriceSource", "Price source", "CODE", 86)
        GridHelper.Decorate(grdLines, "DiscountPct1", "Disc 1 %", "PCT", 62)
        GridHelper.Decorate(grdLines, "DiscountPct2", "Disc 2 %", "PCT", 62)
        GridHelper.Decorate(grdLines, "NetAmount", "Net " & cur, "MONEY", 92)
        GridHelper.Decorate(grdLines, "VatCode", "VAT code", "CODE", 62)
        GridHelper.Decorate(grdLines, "VatAmount", "VAT " & cur, "MONEY", 86)
        GridHelper.Decorate(grdLines, "MarginPct", "Margin %", "PCT", 68)
        HideColumns(grdLines, "ConversionFactor,PricePerQty,VatRatePct,CostSnapshotGBP,DespatchedQty,CancelledQty,Status")
        GridHelper.FillWidth(grdLines)
    End Sub

    Private Shared Sub HideColumns(ByVal g As DataGridView, ByVal columns As String)
        For Each name As String In columns.Split(","c)
            If g.Columns.Contains(name) Then g.Columns(name).Visible = False
        Next
    End Sub

    ''' <summary>SODISC DOCUMENT - the document net, VAT and gross as the engine reconciles them.</summary>
    Private Sub Totals()
        If _lines Is Nothing OrElse _lines.Rows.Count = 0 Then Return
        Dim res As CobolResult = _svc.Discount("DOCUMENT", txtOrder.Text.Trim(), txtCustomer.Text.Trim(),
                                               Currency(), OrderDate(), _lines)
        If res.IsNotFound Then
            SetStatus("SODISC is not in the engine yet (" & res.ReasonCode & ") - the stored totals are shown.", "!  WARNING")
            Return
        End If
        Dim ca As CommArea = _svc.Area
        lblDocNet.Text = SalesOrderService.Amount(ca.GetDecimal("SD-TOT-NET"), Currency())
        SetStatus(grdLines.Rows.Count & " line(s).  SODISC: net " &
                  SalesOrderService.Amount(ca.GetDecimal("SD-TOT-NET"), Currency()) & ", VAT " &
                  SalesOrderService.Amount(ca.GetDecimal("SD-TOT-VAT"), Currency()) & ", gross " &
                  SalesOrderService.Amount(ca.GetDecimal("SD-TOT-GROSS-TOTAL"), Currency()) & ".",
                  If(res.Succeeded, "OK", res.Severity))
    End Sub

    ' --------------------------------------------- price explanation, VAT and margin (SO-004/5/6)

    Private Sub ExplainClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbExplain.Click
        Dim r As DataRow = GridHelper.SelectedRow(grdLines)
        If r Is Nothing Then
            SetStatus("Select an order line first - the explanation is for one line.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim res As CobolResult = _svc.Price("EXPLAIN", Fmt.Str(r("ItemCode")), txtCustomer.Text.Trim(),
                                                      Fmt.Str(r("UomCode")), Currency(), Fmt.Dec(r("AcceptedQty")),
                                                      OrderDate(), txtOrder.Text.Trim())
                  Dim ca As CommArea = _svc.Area
                  GridHelper.Bind(grdWhy, _svc.Candidates())
                  DecorateWhy()
                  Dim chosen As String = SalesOrderService.AreaText(ca, "PR-PRICE-SOURCE") & " level " &
                                         ca.GetInt("PR-PRICE-LEVEL") & " list " &
                                         SalesOrderService.AreaText(ca, "PR-PRICE-LIST-CODE") & ", " &
                                         SalesOrderService.Amount(ca.GetDecimal("PR-UNIT-PRICE"), Currency()) &
                                         " per " & SalesOrderService.AreaText(ca, "PR-PRICE-UOM")
                  If Not res.Succeeded Then
                      ShowResult(res)
                  Else
                      SetStatus("PRICING: " & chosen & ".  " & SalesOrderService.AreaText(ca, "PR-EXPLANATION") &
                                "  " & Chain(r), If(res.IsWarning, "!  WARNING", "OK"))
                  End If
              End Sub)
    End Sub

    ''' <summary>SOVAT DECIDE then SOMARGN LINE for one line, reported in the engine's words.</summary>
    Private Function Chain(ByVal r As DataRow) As String
        Dim net As Decimal = Fmt.Dec(ServiceBase.Col(r, "NetAmount", 0D))
        Dim line As Integer = Fmt.Int(ServiceBase.Col(r, "LineNum", 0))
        Dim text As String = ""
        Dim v As CobolResult = _svc.Vat("DECIDE", txtOrder.Text.Trim(), line, Fmt.Str(r("ItemCode")),
                                        txtCustomer.Text.Trim(), Fmt.Str(ServiceBase.Col(r, "VatCode", "")),
                                        net, Currency(), OrderDate())
        If Not v.IsNotFound Then
            text &= "SOVAT: " & SalesOrderService.AreaText(_svc.Area, "SV-VAT-CODE") & " " &
                    Fmt.Pct(_svc.Area.GetDecimal("SV-VAT-RATE-PCT")) & " " &
                    SalesOrderService.AreaText(_svc.Area, "SV-LIABILITY") & ".  "
        End If
        Dim m As CobolResult = _svc.Margin("LINE", txtOrder.Text.Trim(), line, Fmt.Str(r("ItemCode")),
                                           txtCustomer.Text.Trim(), Fmt.Dec(ServiceBase.Col(r, "AcceptedQty", 0D)),
                                           net, Currency(), OrderDate())
        If Not m.IsNotFound Then
            Dim state As String = SalesOrderService.AreaText(_svc.Area, "SM-MARGIN-STATE")
            text &= "SOMARGN: margin " & If(state = "OK", Fmt.Pct(_svc.Area.GetDecimal("SM-MARGIN-PCT")), state) &
                    If(SalesOrderService.AreaText(_svc.Area, "SM-APPROVAL-REQD") = "Y", " - approval required.", ".")
        End If
        Return text
    End Function

    Private Sub DecorateWhy()
        GridHelper.Decorate(grdWhy, "PR-C-RANK", "Rank", "INT", 44)
        GridHelper.Decorate(grdWhy, "PR-C-LEVEL", "Level", "INT", 44)
        GridHelper.Decorate(grdWhy, "PR-C-SCOPE", "Scope", "CODE", 76)
        GridHelper.Decorate(grdWhy, "PR-C-LIST", "Price list", "CODE", 84)
        GridHelper.Decorate(grdWhy, "PR-C-PRICE", "Unit price", "RATE", 84)
        GridHelper.Decorate(grdWhy, "PR-C-CHOSEN", "Chosen", "TEXT", 50)
        GridHelper.Decorate(grdWhy, "PR-C-WHY", "Why this candidate was or was not chosen", "TEXT", 300)
        HideColumns(grdWhy, "PR-C-CURRENCY,PR-C-UOM,PR-C-QTYBRK,PR-C-PER-QTY,PR-C-DISC,PR-C-MINMGN,PR-C-FROM,PR-C-TO")
        GridHelper.FillWidth(grdWhy)
    End Sub

    ' ------------------------------------------------------ F2 availability and reservation (SO-010)

    Protected Overrides Sub OnLookup()
        If Me.ActiveControl Is txtOrder Then
            Dim code As String = frmLookup.Pick(Me, SalesOrderService.OrderPickSql, "", "Sales order")
            If code <> "" Then
                txtOrder.Text = code
                LoadOrder()
            End If
            Return
        End If
        ShowAvailability()
    End Sub

    ''' <summary>SO-010: free stock, reservations and expected receipts are separate labelled
    ''' lines.  An expected receipt is a supplier promise and is never an allocation.</summary>
    Private Sub ShowAvailability()
        Dim r As DataRow = GridHelper.SelectedRow(grdLines)
        If r Is Nothing Then
            SetStatus("Select an order line, then press F2 for its availability.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim uom As String = Fmt.Str(r("UomCode"))
                  Dim qty As Decimal = Fmt.Dec(r("AcceptedQty"))
                  Dim res As CobolResult = _svc.Availability(Fmt.Str(r("ItemCode")), "", uom, qty, OrderDate())
                  If res.IsNotFound Then
                      txtAvail.Text = "SOAVAIL is not in the engine yet (" & res.ReasonCode & ")."
                      SetStatus("Availability is not available: " & res.Display, "!  WARNING")
                      Return
                  End If
                  Dim ca As CommArea = _svc.Area
                  Dim sb As New Text.StringBuilder()
                  sb.AppendLine(Fmt.Str(r("ItemCode")) & " at " & SterpSession.Current.SiteCode &
                                ", required " & Fmt.Qty(qty, uom))
                  sb.AppendLine("")
                  sb.AppendLine("Physical free stock (guaranteed) : " & Fmt.Qty(ca.GetDecimal("SA-QTY-FREE"), uom))
                  sb.AppendLine("Reserved for other demand        : " & Fmt.Qty(ca.GetDecimal("SA-QTY-RESERVED"), uom))
                  sb.AppendLine("Expected receipts (NOT allocated): " & Fmt.Qty(ca.GetDecimal("SA-QTY-EXPECTED"), uom) &
                                "  first " & SalesOrderService.AreaDate(ca, "SA-EXPECTED-DATE"))
                  sb.AppendLine("Shortfall                        : " & Fmt.Qty(ca.GetDecimal("SA-QTY-SHORTFALL"), uom))
                  sb.AppendLine("")
                  sb.AppendLine("Labelled supply lines - each source stands on its own:")
                  For Each s As DataRow In _svc.SupplyLines().Rows
                      sb.AppendLine(Fmt.Str(s("SA-L-KIND")).PadRight(11) & Fmt.Str(s("SA-L-LABEL")).PadRight(26) &
                                    Fmt.Qty(s("SA-L-QTY"), uom).PadLeft(16) & "  " &
                                    If(Fmt.Str(s("SA-L-GUARANTEED")) = "Y", "GUARANTEED    ", "NOT GUARANTEED") &
                                    "  " & Fmt.Str(s("SA-L-TEXT")))
                  Next
                  txtAvail.Text = sb.ToString()
                  SetStatus("SOAVAIL: " & SalesOrderService.AreaText(ca, "SA-PROMISE-TEXT"),
                            If(res.Succeeded, "OK", res.Severity))
              End Sub)
    End Sub

    Private Sub ReserveClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbReserve.Click
        Dim r As DataRow = GridHelper.SelectedRow(grdLines)
        If r Is Nothing Then
            SetStatus("Select the order line to reserve.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim res As CobolResult = _svc.Reserve(txtOrder.Text.Trim(), Fmt.Int(r("LineNum")),
                                                        Fmt.Str(r("ItemCode")), "", Fmt.Dec(r("AcceptedQty")),
                                                        OrderDate(), txtCustomer.Text.Trim())
                  If Not ShowResult(res) Then Return
                  SetStatus("SORESV: reserved " & Fmt.Qty(_svc.Area.GetDecimal("SV-QTY-RESERVED"), Fmt.Str(r("UomCode"))) &
                            ", shortfall " & Fmt.Qty(_svc.Area.GetDecimal("SV-QTY-SHORTFALL"), Fmt.Str(r("UomCode"))) &
                            ", reservation " & _svc.Area.GetDecimal("SV-RESERVATION-ID") & ".", "OK")
              End Sub)
    End Sub

    ' ------------------------------------------------- F10 accept and release (SO-002, SO-003, SO-011)

    Protected Overrides Sub OnConfirm()
        If txtOrder.Text.Trim() = "" Then
            SetStatus("Key an order number first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim res As CobolResult = Acceptance("VALIDATE")
                  Dim findings As String = BlockerText()
                  If Not res.Succeeded Then
                      If findings <> "" Then Info(findings, "Order validation - SOACCEPT")
                      ShowResult(res)
                      SetStatus("Order " & txtOrder.Text.Trim() & " is NOT accepted: " & res.Display, "*** ERROR")
                      Return
                  End If
                  If findings <> "" Then Info(findings, "Order validation - SOACCEPT")
                  If Not Confirm("Accept and release order " & txtOrder.Text.Trim() & " for " &
                                 txtCustomer.Text.Trim() & "?") Then Return
                  res = Acceptance("ACCEPT")
                  Dim ca As CommArea = _svc.Area
                  ShowState(SalesOrderService.AreaText(ca, "SA-RELEASE-STATUS"),
                            SalesOrderService.AreaText(ca, "SA-FULFIL-STATUS"),
                            SalesOrderService.AreaText(ca, "SA-BILLING-STATUS"),
                            SalesOrderService.AreaText(ca, "SA-HOLD-REASON"))
                  If Not ShowResult(res) Then
                      SetStatus("Order " & txtOrder.Text.Trim() & " is " &
                                SalesOrderService.AreaText(ca, "SA-RELEASE-STATUS") & ": " & res.Display,
                                If(res.IsHold, "## HOLD", "*** ERROR"))
                      Return
                  End If
                  SetStatus("Order " & txtOrder.Text.Trim() & " accepted, release status " &
                            SalesOrderService.AreaText(ca, "SA-RELEASE-STATUS") & ".", "OK")
                  LoadOrder()
              End Sub)
    End Sub

    Private Function Acceptance(ByVal func As String) As CobolResult
        Return _svc.Accept(func, txtOrder.Text.Trim(), txtCustomer.Text.Trim(), OrderDate(), Currency(),
                           txtTerms.Text.Trim(), txtSalesperson.Text.Trim(), txtIncoterms.Text.Trim(),
                           txtIncoPlace.Text.Trim(), txtDeliveryRef.Text.Trim())
    End Function

    ''' <summary>Every finding at once, severity first, so nothing is discovered one at a time.</summary>
    Private Function BlockerText() As String
        Dim sb As New Text.StringBuilder()
        For Each b As DataRow In _svc.Blockers().Rows
            sb.AppendLine(Fmt.Str(b("SA-B-SEVERITY")).PadRight(9) & Fmt.Str(b("SA-B-REASON")) & "  line " &
                          Fmt.Int(b("SA-B-LINE-NUM")) & "  " & Fmt.Str(b("SA-B-FIELD")) & ": " &
                          Fmt.Str(b("SA-B-TEXT")))
        Next
        Return sb.ToString()
    End Function

    Private Sub LoadClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbLoad.Click
        LoadOrder()
    End Sub

    Private Sub AvailClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbAvail.Click
        ShowAvailability()
    End Sub

    Private Sub AcceptClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbAccept.Click
        OnConfirm()
    End Sub

    Private Sub CloseClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbClose.Click
        CloseForm()
    End Sub

End Class
