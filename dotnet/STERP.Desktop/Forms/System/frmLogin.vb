Imports STERP.Business
Imports STERP.Common

''' <summary>
''' Sign-on dialog: Windows authentication, then the company / site the operator will
''' work in and the business date.  SYSADM may continue as another user so that a
''' demonstration can be given through the eyes of a clerk.
''' </summary>
Partial Public Class frmLogin
    Inherits Form

    Private ReadOnly _login As New LoginService()

    Public Sub New()
        InitializeComponent()
        UiStyle.ApplyForm(Me)
    End Sub

    Private Sub frmLogin_Load(ByVal sender As Object, ByVal e As EventArgs) Handles MyBase.Load
        lblLoginValue.Text = _login.WindowsLogin
        dtpDate.Value = Date.Today
        Me.Cursor = Cursors.WaitCursor
        Dim msg As String = _login.SignOn()
        Me.Cursor = Cursors.Default
        If msg <> "" Then
            lblMessage.Text = "*** " & msg
            cboRunAs.Enabled = False
            Return
        End If
        ShowContext()
    End Sub

    Private Sub ShowContext()
        Dim s As SterpSession = SterpSession.Current
        lblUserValue.Text = s.UserCode & "   " & s.DisplayName
        cboRunAs.Enabled = (s.RealUserCode = "SYSADM")
        If cboRunAs.Enabled AndAlso cboRunAs.Items.Count = 0 Then
            Dim t As DataTable = _login.UserList()
            For Each r As DataRow In t.Rows
                cboRunAs.Items.Add(Convert.ToString(r("UserCode")).Trim())
            Next
            cboRunAs.SelectedItem = s.UserCode
        End If
        lstAccess.Items.Clear()
        For Each cs As CompanySite In s.Access
            lstAccess.Items.Add(cs)
        Next
        If lstAccess.Items.Count > 0 Then lstAccess.SelectedIndex = 0
        lblMessage.Text = If(lstAccess.Items.Count = 0,
            "*** No company or site has been granted to this user.",
            lstAccess.Items.Count & " company/site combination(s) available.")
        btnOk.Enabled = lstAccess.Items.Count > 0
    End Sub

    Private Sub cboRunAs_SelectedIndexChanged(ByVal sender As Object, ByVal e As EventArgs) _
            Handles cboRunAs.SelectedIndexChanged
        If Not cboRunAs.Enabled OrElse cboRunAs.SelectedItem Is Nothing Then Return
        Dim code As String = Convert.ToString(cboRunAs.SelectedItem)
        If code = SterpSession.Current.UserCode Then Return
        Dim msg As String = _login.RunAs(code)
        If msg <> "" Then lblMessage.Text = "*** " & msg Else ShowContext()
    End Sub

    Private Sub btnOk_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnOk.Click
        Dim cs As CompanySite = TryCast(lstAccess.SelectedItem, CompanySite)
        If cs Is Nothing Then
            lblMessage.Text = "*** Select the company and site to work in."
            Return
        End If
        SterpSession.Current.SelectCompanySite(cs)
        _login.LoadPeriod(dtpDate.Value.Date)
        AuditService.Note("SIGNON", SterpSession.Current.UserCode,
                          "Signed on to " & cs.CompanyCode & "/" & cs.SiteCode)
        Me.DialogResult = DialogResult.OK
        Me.Close()
    End Sub

    Private Sub btnCancel_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnCancel.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub lstAccess_DoubleClick(ByVal sender As Object, ByVal e As EventArgs) Handles lstAccess.DoubleClick
        If btnOk.Enabled Then btnOk_Click(sender, e)
    End Sub

End Class
