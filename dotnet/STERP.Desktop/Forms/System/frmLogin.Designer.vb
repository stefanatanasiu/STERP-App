Partial Public Class frmLogin
    Inherits Form

    Private components As System.ComponentModel.IContainer
    Friend WithEvents grpUser As GroupBox
    Friend WithEvents lblLogin As Label
    Friend WithEvents lblLoginValue As Label
    Friend WithEvents lblUser As Label
    Friend WithEvents lblUserValue As Label
    Friend WithEvents lblRunAs As Label
    Friend WithEvents cboRunAs As ComboBox
    Friend WithEvents grpContext As GroupBox
    Friend WithEvents lblAccess As Label
    Friend WithEvents lstAccess As ListBox
    Friend WithEvents lblDate As Label
    Friend WithEvents dtpDate As DateTimePicker
    Friend WithEvents lblMessage As Label
    Friend WithEvents btnOk As Button
    Friend WithEvents btnCancel As Button

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.grpUser = New GroupBox()
        Me.lblLogin = New Label()
        Me.lblLoginValue = New Label()
        Me.lblUser = New Label()
        Me.lblUserValue = New Label()
        Me.lblRunAs = New Label()
        Me.cboRunAs = New ComboBox()
        Me.grpContext = New GroupBox()
        Me.lblAccess = New Label()
        Me.lstAccess = New ListBox()
        Me.lblDate = New Label()
        Me.dtpDate = New DateTimePicker()
        Me.lblMessage = New Label()
        Me.btnOk = New Button()
        Me.btnCancel = New Button()
        Me.SuspendLayout()
        'grpUser  - who is signing on
        Me.grpUser.Text = "Operator"
        Me.grpUser.FlatStyle = FlatStyle.Standard
        Me.grpUser.SetBounds(8, 6, 420, 92)
        Me.lblLogin.Text = "&Windows login:"
        Me.lblLogin.SetBounds(12, 22, 100, 16)
        Me.lblLoginValue.BorderStyle = BorderStyle.Fixed3D
        Me.lblLoginValue.SetBounds(118, 19, 290, 18)
        Me.lblUser.Text = "STERP &user:"
        Me.lblUser.SetBounds(12, 46, 100, 16)
        Me.lblUserValue.BorderStyle = BorderStyle.Fixed3D
        Me.lblUserValue.SetBounds(118, 43, 290, 18)
        Me.lblRunAs.Text = "Continue &as:"
        Me.lblRunAs.SetBounds(12, 70, 100, 16)
        Me.cboRunAs.DropDownStyle = ComboBoxStyle.DropDownList
        Me.cboRunAs.FlatStyle = FlatStyle.Standard
        Me.cboRunAs.SetBounds(118, 66, 140, 21)
        Me.grpUser.Controls.AddRange(New Control() {Me.lblLogin, Me.lblLoginValue, Me.lblUser,
                                                    Me.lblUserValue, Me.lblRunAs, Me.cboRunAs})
        'grpContext - where the work will be posted
        Me.grpContext.Text = "Company, site and business date"
        Me.grpContext.FlatStyle = FlatStyle.Standard
        Me.grpContext.SetBounds(8, 102, 420, 168)
        Me.lblAccess.Text = "Company / &site:"
        Me.lblAccess.SetBounds(12, 20, 110, 16)
        Me.lstAccess.BorderStyle = BorderStyle.Fixed3D
        Me.lstAccess.Font = New Font("Microsoft Sans Serif", 8.25!)
        Me.lstAccess.IntegralHeight = False
        Me.lstAccess.SetBounds(12, 38, 396, 92)
        Me.lblDate.Text = "&Business date:"
        Me.lblDate.SetBounds(12, 142, 110, 16)
        Me.dtpDate.Format = DateTimePickerFormat.Custom
        Me.dtpDate.CustomFormat = "dd/MM/yyyy"
        Me.dtpDate.SetBounds(126, 138, 110, 20)
        Me.grpContext.Controls.AddRange(New Control() {Me.lblAccess, Me.lstAccess, Me.lblDate, Me.dtpDate})
        'message line and buttons
        Me.lblMessage.BorderStyle = BorderStyle.Fixed3D
        Me.lblMessage.SetBounds(8, 276, 420, 18)
        Me.lblMessage.TextAlign = ContentAlignment.MiddleLeft
        Me.btnOk.Text = "&OK"
        Me.btnOk.FlatStyle = FlatStyle.Standard
        Me.btnOk.SetBounds(266, 302, 76, 23)
        Me.btnCancel.Text = "&Cancel"
        Me.btnCancel.FlatStyle = FlatStyle.Standard
        Me.btnCancel.SetBounds(350, 302, 76, 23)
        'frmLogin
        Me.AcceptButton = Me.btnOk
        Me.CancelButton = Me.btnCancel
        Me.ClientSize = New Size(436, 334)
        Me.Controls.AddRange(New Control() {Me.grpUser, Me.grpContext, Me.lblMessage, Me.btnOk, Me.btnCancel})
        Me.FormBorderStyle = FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "frmLogin"
        Me.StartPosition = FormStartPosition.CenterScreen
        Me.Text = "STERP - Sign On"
        Me.ResumeLayout(False)
    End Sub

End Class
