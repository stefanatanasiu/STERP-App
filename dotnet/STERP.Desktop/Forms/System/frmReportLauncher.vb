Imports STERP.Business
Imports STERP.Common

''' <summary>
''' Report launcher: pick a report from core.ReportDef, fill in the KEY=VALUE
''' parameters named by ParamSpec and run the COBOL batch program.  The spool file
''' named on the STERP-I-SPOOL line is opened in the viewer (CONVENTIONS 3.6).
''' </summary>
Partial Public Class frmReportLauncher
    Inherits BaseForm

    Private ReadOnly _svc As New ReportService()
    Private ReadOnly _params As New Dictionary(Of String, TextBox)
    Public ReadOnly InitialParameters As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)

    Public Sub New()
        InitializeComponent()
        GridHelper.Style(grdReports)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        UiStyle.Tool(tbRpt, "Run (F10)", "RUN", AddressOf RunClicked)
        UiStyle.Tool(tbRpt, "Refresh (F5)", "REFRESH", AddressOf RefreshClicked)
        tbRpt.Items.Add(New ToolStripSeparator())
        UiStyle.Tool(tbRpt, "Close (F3)", "CLOSE", Sub(s As Object, a As EventArgs) CloseForm())
        LoadData()
    End Sub


    Private Sub LoadData()
        Guard(Sub()
                  GridHelper.Bind(grdReports, _svc.Definitions(FormArgument))
                  SetStatus(grdReports.Rows.Count & " report(s) available.", "")
              End Sub)
    End Sub

    Private Sub grdReports_SelectionChanged(ByVal sender As Object, ByVal e As EventArgs) _
            Handles grdReports.SelectionChanged
        BuildParameters()
    End Sub

    ''' <summary>One labelled box per key named in ParamSpec, e.g. "CO,SITE,ASAT".</summary>
    Private Sub BuildParameters()
        pnlParams.Controls.Clear()
        _params.Clear()
        Dim spec As String = Fmt.Str(GridHelper.Selected(grdReports, "ParamSpec"))
        Dim y As Integer = 8
        For Each raw As String In spec.Split(","c)
            Dim key As String = raw.Trim().ToUpperInvariant()
            If key.Length = 0 Then Continue For
            UiStyle.Caption(pnlParams, key & ":", 8, y, 70)
            Dim t As TextBox = UiStyle.Edit(pnlParams, 82, y, 150)
            Select Case key
                Case "CO" : t.Text = SterpSession.Current.CompanyCode
                Case "SITE" : t.Text = SterpSession.Current.SiteCode
                Case "USER" : t.Text = SterpSession.Current.UserCode
                Case "ASAT", "TO" : t.Text = Fmt.Yyyymmdd(SterpSession.Current.BusinessDate)
                Case "FROM" : t.Text = Fmt.Yyyymmdd(New Date(SterpSession.Current.BusinessDate.Year, 1, 1))
                Case "PERIOD" : t.Text = SterpSession.Current.FiscalYear.ToString("0000") &
                                         SterpSession.Current.PeriodNo.ToString("00")
                Case "MODE" : t.Text = "PREVIEW"
            End Select
            If InitialParameters.ContainsKey(key) Then t.Text = InitialParameters(key)
            _params(key) = t
            y += 25
        Next
        If _params.Count = 0 Then UiStyle.Caption(pnlParams, "(this report takes no parameters)", 8, 8, 240)
    End Sub

    Protected Overrides Sub OnConfirm()
        RunClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadData()
    End Sub

    Private Sub RefreshClicked(ByVal sender As Object, ByVal e As EventArgs)
        LoadData()
    End Sub

    Private Sub RunClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = Fmt.Str(GridHelper.Selected(grdReports, "ReportCode"))
        Dim prog As String = Fmt.Str(GridHelper.Selected(grdReports, "ProgramName"))
        If prog = "" Then
            SetStatus("Select a report first.", "")
            Return
        End If
        Dim args As New Dictionary(Of String, String)
        For Each kv As KeyValuePair(Of String, TextBox) In _params
            If kv.Value.Text.Trim().Length > 0 Then args(kv.Key) = kv.Value.Text.Trim()
        Next
        ' Identity is always the signed-on user, even for reports whose ParamSpec
        ' omits USER (APBACS otherwise falls back to APMGR).
        args("USER") = SterpSession.Current.UserCode
        Guard(Sub()
                  Dim rr As ReportRun = _svc.Run(code, prog, args)
                  txtLog.Text = rr.Output.Replace(vbLf, vbCrLf).Replace(vbCr & vbCr, vbCr)
                  If rr.Succeeded Then
                      SetStatus("Return code " & rr.ReturnCode & ".  " &
                                If(rr.SpoolFile = "", "No spool file produced.", rr.SpoolFile), "OK")
                      If rr.SpoolFile <> "" Then
                          Dim main As frmMain = TryCast(Me.MdiParent, frmMain)
                          If main IsNot Nothing Then main.OpenForm("frmSpoolViewer", rr.SpoolFile)
                      End If
                  Else
                      ShowError("The report ended with return code " & rr.ReturnCode & ".", "RPT" & rr.ReturnCode.ToString("0000"))
                  End If
              End Sub)
    End Sub

End Class
