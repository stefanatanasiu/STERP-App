Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Customer enquiry for the sales ledger.  ar.usp_CustomerEnquiry supplies the balance, the
''' aged profile, the open items and the collection history; ARENQ supplies the trading metrics
''' so that turnover, DSO and credit utilisation are the engine's figures and not the screen's.
''' Enquiry only - nothing on this window posts.
''' REQ: AR-015, AR-017, AR-021, AR-022, COM-024
''' </summary>
Public Class frmCustomerEnquiry
    Inherits BaseForm

    Private ReadOnly _svc As New DebtorsService()

    Public Sub New()
        InitializeComponent()
        GridHelper.Style(grdAged)
        GridHelper.Style(grdItems)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        txtAsAt.Text = Fmt.Date(SterpSession.Current.BusinessDate)
        If FormArgument <> "" Then txtCustomer.Text = FormArgument
        SetStatus("Key an account and press F5, or press F2 for the account list.")
        If txtCustomer.Text <> "" Then LoadData()
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadData()
    End Sub

    Protected Overrides Sub OnLookup()
        Dim code As String = frmLookup.Pick(Me, DebtorsService.CustomerPickSql, "", "Customer account")
        If code = "" Then Return
        txtCustomer.Text = code
        LoadData()
    End Sub

    Private Function AsAt() As Date
        Dim d As Date? = Fmt.ParseDate(txtAsAt.Text)
        Return If(d.HasValue, d.Value, SterpSession.Current.BusinessDate)
    End Function

    Private Sub LoadData()
        Dim customer As String = txtCustomer.Text.Trim().ToUpperInvariant()
        If customer = "" Then
            SetStatus("Key an account first, or press F2 for the account list.", "!  WARNING")
            Return
        End If
        txtCustomer.Text = customer
        Guard(Sub()
                  Dim ds As DataSet = _svc.Enquiry(customer)
                  ShowBalance(ds.Tables(0))
                  GridHelper.Bind(grdAged, ds.Tables(1))
                  GridHelper.Bind(grdItems, ds.Tables(3))
                  GridHelper.FillWidth(grdItems)
                  ShowTurnover(ds.Tables(2))
                  ShowMetrics(customer)
                  SetStatus(grdItems.Rows.Count & " open item(s) at " & Fmt.Date(AsAt()) &
                            ".  F2 account list, F5 refresh, F3 close.", "")
              End Sub)
    End Sub

    ''' <summary>Result set 1 - ar.vw_CustomerBalance, the credit control position.</summary>
    Private Sub ShowBalance(ByVal t As DataTable)
        If t Is Nothing OrElse t.Rows.Count = 0 Then Return
        Dim r As DataRow = t.Rows(0)
        lblCustomerName.Text = Fmt.Str(ServiceBase.Col(r, "LegalName", "")) & "  (" &
                               Fmt.Str(ServiceBase.Col(r, "CurrencyCode", "GBP")) & ")"
        Metric("STATUS").Text = Fmt.Str(ServiceBase.Col(r, "CreditState", "")) &
                                If(Fmt.Bool(ServiceBase.Col(r, "HoldFlag", False)), " / ON HOLD", "")
        Metric("BALANCE").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "BalanceGBP", 0D)))
        Metric("OVERDUE").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "OverdueGBP", 0D)))
        Metric("DISPUTED").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "DisputedGBP", 0D)))
        Metric("LIMIT").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "CreditLimitGBP", 0D)))
        Metric("EXPOSURE").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "ExposureGBP", 0D)))
        Metric("AVAILABLE").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "CreditAvailableGBP", 0D)))
        Metric("UTILISED").Text = Fmt.Dec(ServiceBase.Col(r, "UtilisationPct", 0D)).ToString("0.00")
        Metric("UNAPPLIED").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "UnappliedCashGBP", 0D)))
        Metric("WORST").Text = Fmt.Int(ServiceBase.Col(r, "MaxDaysOverdue", 0)).ToString()
        Dim oldest As Object = ServiceBase.Col(r, "OldestDueDate", Nothing)
        Metric("OLDEST").Text = If(oldest Is Nothing, "-", Fmt.Date(oldest))
    End Sub

    ''' <summary>Result set 3 - turnover, DSO and receipt effectiveness.  An undefined DSO is
    ''' shown as "undefined", never as zero (AR-022).</summary>
    Private Sub ShowTurnover(ByVal t As DataTable)
        If t Is Nothing OrElse t.Rows.Count = 0 Then Return
        Dim r As DataRow = t.Rows(0)
        Metric("TURNOVER").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "NetTurnoverGBP", 0D)))
        Metric("RECEIPTS").Text = Fmt.Money(Fmt.Dec(ServiceBase.Col(r, "ReceivedGBP", 0D)))
        Dim dso As Object = ServiceBase.Col(r, "DsoDays", Nothing)
        Metric("DSO").Text = If(dso Is Nothing, "undefined", Fmt.Dec(dso).ToString("0.0"))
        Dim avg As Object = ServiceBase.Col(r, "AverageDaysToPay", Nothing)
        Metric("AVGDAYS").Text = If(avg Is Nothing, "-", Fmt.Dec(avg).ToString("0.0"))
        Metric("ITEMS").Text = grdItems.Rows.Count.ToString()
    End Sub

    ''' <summary>ARENQ refines the figures with the engine's own ageing and DSO basis.</summary>
    Private Sub ShowMetrics(ByVal customer As String)
        Dim res As CobolResult = _svc.Metrics(customer, AsAt())
        If Not res.Succeeded Then Return
        Dim ca As CommArea = _svc.Area
        Metric("WORST").Text = ca.GetInt("EQ-WORST-DAYS").ToString()
        Metric("ITEMS").Text = ca.GetInt("EQ-OPEN-ITEMS").ToString()
        Metric("OLDEST").Text = DebtorsService.AreaDate(ca, "EQ-OLDEST-DUE")
        Metric("TURNOVER").Text = DebtorsService.AreaMoney(ca, "EQ-TURNOVER-INV")
        Metric("RECEIPTS").Text = DebtorsService.AreaMoney(ca, "EQ-RECEIPTS-VALUE")
        Metric("AVGDAYS").Text = ca.GetInt("EQ-AVG-DAYS-PAID").ToString()
        Metric("DSO").Text = If(ca.GetString("EQ-DSO-DEFINED") = "Y",
                                ca.GetDecimal("EQ-DSO").ToString("0.0"), "undefined")
    End Sub

    ''' <summary>The engine's view of exposure and the release decision for this account.</summary>
    Private Sub CreditCheckClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbCreditCheck.Click
        If txtCustomer.Text.Trim() = "" Then Return
        Guard(Sub()
                  Dim res As CobolResult = _svc.CreditCheck(txtCustomer.Text.Trim(), 0D,
                                                            SterpSession.Current.BaseCurrency, "ORDER", "")
                  If Not ShowResult(res) Then Return
                  Dim ca As CommArea = _svc.Area
                  SetStatus("Decision " & ca.GetString("CK-DECISION") & " | exposure " &
                            DebtorsService.AreaMoney(ca, "CK-EXPOSURE") & " of limit " &
                            DebtorsService.AreaMoney(ca, "CK-LIMIT") & " | headroom " &
                            DebtorsService.AreaMoney(ca, "CK-HEADROOM") & " | " & ca.GetString("CK-DECISION-TEXT"),
                            If(ca.GetString("CK-DECISION") = "RELEASE", "OK", "## HOLD"))
              End Sub)
    End Sub

    ''' <summary>Replaces the open item list with the statement lines at the chosen date.</summary>
    Private Sub StatementClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbStatement.Click
        If txtCustomer.Text.Trim() = "" Then Return
        Guard(Sub()
                  Dim ds As DataSet = _svc.Statement(txtCustomer.Text.Trim(), AsAt(), "OPENITEM")
                  GridHelper.Bind(grdItems, ds.Tables(ds.Tables.Count - 1))
                  GridHelper.FillWidth(grdItems)
                  lblItemsCaption.Text = "Open item statement at " & Fmt.Date(AsAt()) & " - press F5 for the open items:"
                  SetStatus(grdItems.Rows.Count & " statement line(s) at " & Fmt.Date(AsAt()) & ".", "")
              End Sub)
    End Sub

    Private Sub FindClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbFind.Click
        lblItemsCaption.Text = "Open items, oldest due date first:"
        LoadData()
    End Sub

    Private Sub CloseClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbClose.Click
        CloseForm()
    End Sub

End Class
