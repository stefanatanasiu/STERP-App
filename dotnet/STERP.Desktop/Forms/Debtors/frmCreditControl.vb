Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Credit control worklist.  ar.usp_CreditControlWorklist orders the overdue items by the
''' dunning stage they have reached and names the owner of each; ARDISPUT records a dispute or
''' a promise to pay.  A dispute does not erase the debt, and the engine - not this screen -
''' decides whether either is accepted.  The dunning and aged debt runs go through the standard
''' report launcher so that every run is recorded in core.JobRun.
''' REQ: AR-015, AR-016, AR-017, AR-024, D-06, COM-024
''' </summary>
Public Class frmCreditControl
    Inherits BaseForm

    Private ReadOnly _svc As New DebtorsService()

    Public Sub New()
        InitializeComponent()
        GridHelper.Style(grdWork)
        GridHelper.Style(grdAccounts)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        txtOwner.Text = If(FormArgument <> "", FormArgument, "")
        LoadData()
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadData()
    End Sub

    Private Sub LoadData()
        Guard(Sub()
                  Dim ds As DataSet = _svc.Worklist(txtOwner.Text.Trim())
                  Dim work As DataTable = ds.Tables(0)
                  Dim stage As String = Fmt.Str(cboStage.SelectedItem)
                  If stage <> "" AndAlso stage <> "ALL STAGES" Then
                      Dim view As New DataView(work, "StageCode = '" & stage.Replace("'", "''") & "'", "", DataViewRowState.CurrentRows)
                      work = view.ToTable()
                  End If
                  GridHelper.Bind(grdWork, work)
                  GridHelper.FillWidth(grdWork)
                  GridHelper.Bind(grdAccounts, ds.Tables(1))
                  GridHelper.FillWidth(grdAccounts)
                  SetStatus(grdWork.Rows.Count & " item(s) to work at " & stage.ToLowerInvariant() & ", " &
                            grdAccounts.Rows.Count & " account(s) behind them.  F10 records a promise to pay.",
                            If(grdWork.Rows.Count = 0, "OK", "!  WARNING"))
              End Sub)
    End Sub

    Private Sub StageChanged(ByVal sender As Object, ByVal e As EventArgs) Handles cboStage.SelectedIndexChanged
        If Me.IsHandleCreated Then LoadData()
    End Sub

    ''' <summary>Raises a dispute against the selected item.  The debt stays on the account and
    ''' exposure is not reduced by the act of disputing it (AR-015).</summary>
    Private Sub DisputeClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbDispute.Click
        Dim row As DataRow = GridHelper.SelectedRow(grdWork)
        If row Is Nothing Then
            SetStatus("Select an overdue item first.", "!  WARNING")
            Return
        End If
        Dim reasonCode As String = InputBox("Dispute reason code for " & Fmt.Str(row("DocumentNumber")) &
                                            " (PRICE, QUALITY, DELIVERY, PAPERWORK):", "Raise dispute", "PRICE")
        If reasonCode.Trim() = "" Then Return
        Dim reason As String = InputBox("What is being disputed on " & Fmt.Str(row("DocumentNumber")) & "?",
                                        "Raise dispute", "")
        If reason.Trim() = "" Then
            SetStatus("A dispute must carry a reason.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim res As CobolResult = _svc.RaiseDispute(Fmt.Str(row("CustomerCode")), Fmt.Str(row("DocumentNumber")),
                                                             Fmt.Int(row("OpenItemId")), Fmt.Dec(row("ResidualAmountGBP")),
                                                             reasonCode.Trim().ToUpperInvariant(), reason.Trim())
                  If Not ShowResult(res) Then Return
                  SetStatus("Dispute " & _svc.Area.GetString("DS-CASE-REF") & " raised on " &
                            Fmt.Str(row("DocumentNumber")) & "; the debt stays on the account, undisputed " &
                            DebtorsService.AreaMoney(_svc.Area, "DS-UNDISPUTED-AMT") & ".", "OK")
                  LoadData()
              End Sub)
    End Sub

    ''' <summary>F10 - records a promise to pay against the selected item's collection case.</summary>
    Protected Overrides Sub OnConfirm()
        Dim row As DataRow = GridHelper.SelectedRow(grdWork)
        If row Is Nothing Then
            SetStatus("Select an overdue item first.", "!  WARNING")
            Return
        End If
        Dim amount As String = InputBox("Amount promised (£) by " & Fmt.Str(row("CustomerName")) & ":",
                                        "Promise to pay", Fmt.Dec(row("ResidualAmountGBP")).ToString("0.00"))
        If amount.Trim() = "" Then Return
        Dim promised As String = InputBox("Date promised (dd/MM/yyyy):", "Promise to pay",
                                          Fmt.Date(SterpSession.Current.BusinessDate.AddDays(7)))
        Dim due As Date? = Fmt.ParseDate(promised)
        If Not due.HasValue Then
            SetStatus("A promise needs a date in dd/MM/yyyy form.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim res As CobolResult = _svc.RecordPromise(Fmt.Str(row("CustomerCode")), Fmt.Str(row("CaseRef")),
                                                              Fmt.Dec(amount), due.Value,
                                                              "Promise taken on the credit control worklist.")
                  If Not ShowResult(res) Then Return
                  SetStatus("Promise of " & Fmt.Money(Fmt.Dec(amount)) & " by " & Fmt.Date(due.Value) &
                            " recorded, status " & _svc.Area.GetString("DS-PROMISE-STATUS") &
                            ".  A broken promise is picked up by the engine's review sweep.", "OK")
                  LoadData()
              End Sub)
    End Sub

    Private Sub AgedClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbAged.Click
        Launch("AGEDDEBT", "Aged debt")
    End Sub

    Private Sub DunningClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbDunning.Click
        Launch("DUNNING", "Dunning letters")
    End Sub

    ''' <summary>Opens the standard report launcher on one report definition.</summary>
    Private Sub Launch(ByVal reportCode As String, ByVal title As String)
        Guard(Sub()
                  Dim f As New frmReportLauncher()
                  f.FormArgument = reportCode
                  If Me.MdiParent IsNot Nothing Then f.MdiParent = Me.MdiParent
                  f.Show()
                  SetStatus(title & " - choose the parameters in the report launcher and press F10 to run.", "")
              End Sub)
    End Sub

    Private Sub RefreshClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbRefresh.Click
        LoadData()
    End Sub

    Private Sub CloseClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbClose.Click
        CloseForm()
    End Sub

End Class
