Imports STERP.Business
Imports STERP.Common

''' <summary>
''' Sales invoice run launcher.  The screen only collects the company, the site, the despatch
''' date range and the mode, and hands them to the framework batch launcher, which runs
''' SOINVRUN.EXE with KEY=VALUE arguments, records the run in core.JobRun and picks up the
''' STERP-I-SPOOL line.  PREVIEW has no business effect; only COMMIT bills anything.
''' REQ: SO-014, COM-019, COM-024
''' </summary>
Public Class frmSalesInvoiceRun
    Inherits BaseForm

    Private ReadOnly _svc As New SalesOrderService()
    Private _spoolFile As String = ""

    Public Sub New()
        InitializeComponent()
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        Dim s As SterpSession = SterpSession.Current
        txtCompany.Text = s.CompanyCode
        txtSite.Text = s.SiteCode
        txtFrom.Text = Fmt.Date(New Date(s.BusinessDate.Year, s.BusinessDate.Month, 1))
        txtTo.Text = Fmt.Date(s.BusinessDate)
        cboMode.SelectedIndex = 0
        If FormArgument = "COMMIT" Then cboMode.SelectedIndex = 1
        SetStatus("Check the range and the mode, then press F10 to run SOINVRUN.")
    End Sub

    Private Function Stamp(ByVal text As String) As Date
        Dim d As Date? = Fmt.ParseDate(text)
        Return If(d.HasValue, d.Value, SterpSession.Current.BusinessDate)
    End Function

    ''' <summary>F10 - run the batch program through the framework launcher.</summary>
    Protected Overrides Sub OnConfirm()
        Dim mode As String = Fmt.Str(cboMode.SelectedItem)
        If mode = "COMMIT" AndAlso Not Confirm("COMMIT raises the sales invoices for " &
                                               txtCompany.Text.Trim() & " / " & txtSite.Text.Trim() &
                                               ".  Run it now?") Then Return
        Guard(Sub()
                  _spoolFile = ""
                  Dim rr As ReportRun = _svc.InvoiceRun(txtCompany.Text.Trim().ToUpperInvariant(),
                                                        txtSite.Text.Trim().ToUpperInvariant(),
                                                        Stamp(txtFrom.Text), Stamp(txtTo.Text), mode)
                  txtLog.Text = rr.Output.Replace(vbLf, vbCrLf).Replace(vbCr & vbCr, vbCr)
                  _spoolFile = rr.SpoolFile
                  lblSpool.Text = "Spool file: " & If(rr.SpoolFile = "", "(none produced)", rr.SpoolFile)
                  lblJobStatus.Text = ReportService.JobStatus(rr.ReturnCode) & " - return code " &
                                      rr.ReturnCode & ", job run " & rr.JobRunId & ", mode " & mode
                  If Not rr.Succeeded Then
                      ShowError("SOINVRUN ended with return code " & rr.ReturnCode & ".  " &
                                "Nothing has been billed by this run.", "SOI" & rr.ReturnCode.ToString("0000"))
                      Return
                  End If
                  SetStatus("SOINVRUN " & mode & " finished, return code " & rr.ReturnCode & ".  " &
                            If(rr.SpoolFile = "", "No spool file was produced.", rr.SpoolFile),
                            If(rr.ReturnCode = 0, "OK", "!  WARNING"))
                  OpenSpool()
              End Sub)
    End Sub

    Private Sub OpenSpool()
        If _spoolFile = "" Then Return
        Dim main As frmMain = TryCast(Me.MdiParent, frmMain)
        If main IsNot Nothing Then main.OpenForm("frmSpoolViewer", _spoolFile)
    End Sub

    Private Sub RunClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbRun.Click
        OnConfirm()
    End Sub

    Private Sub SpoolClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbSpool.Click
        If _spoolFile = "" Then
            SetStatus("This run produced no spool file.", "!  WARNING")
            Return
        End If
        OpenSpool()
    End Sub

    Private Sub CloseClicked(ByVal sender As Object, ByVal e As EventArgs) Handles tsbClose.Click
        CloseForm()
    End Sub

End Class
