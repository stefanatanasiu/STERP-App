Partial Public Class frmLookup
    Inherits Form

    Private components As System.ComponentModel.IContainer
    Friend WithEvents lblSearch As Label
    Friend WithEvents txtSearch As TextBox
    Friend WithEvents btnSearch As Button
    Friend WithEvents grdList As DataGridView
    Friend WithEvents lblCount As Label
    Friend WithEvents btnOk As Button
    Friend WithEvents btnCancel As Button

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.lblSearch = New Label()
        Me.txtSearch = New TextBox()
        Me.btnSearch = New Button()
        Me.grdList = New DataGridView()
        Me.lblCount = New Label()
        Me.btnOk = New Button()
        Me.btnCancel = New Button()
        CType(Me.grdList, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        Me.lblSearch.SetBounds(8, 11, 50, 16)
        Me.lblSearch.Text = "&Find:"
        Me.txtSearch.SetBounds(56, 8, 220, 20)
        Me.txtSearch.BorderStyle = BorderStyle.Fixed3D
        Me.btnSearch.SetBounds(282, 7, 70, 23)
        Me.btnSearch.Text = "&Search"
        Me.btnSearch.FlatStyle = FlatStyle.Standard
        '
        Me.grdList.SetBounds(8, 34, 540, 250)
        Me.grdList.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        '
        Me.lblCount.SetBounds(8, 290, 400, 16)
        '
        Me.btnOk.SetBounds(392, 288, 76, 23)
        Me.btnOk.Text = "&OK"
        Me.btnOk.FlatStyle = FlatStyle.Standard
        Me.btnOk.Anchor = AnchorStyles.Bottom Or AnchorStyles.Right
        Me.btnCancel.SetBounds(472, 288, 76, 23)
        Me.btnCancel.Text = "&Cancel"
        Me.btnCancel.FlatStyle = FlatStyle.Standard
        Me.btnCancel.DialogResult = DialogResult.Cancel
        Me.btnCancel.Anchor = AnchorStyles.Bottom Or AnchorStyles.Right
        '
        Me.AcceptButton = Me.btnOk
        Me.CancelButton = Me.btnCancel
        Me.ClientSize = New Size(556, 320)
        Me.Controls.AddRange(New Control() {Me.lblSearch, Me.txtSearch, Me.btnSearch, Me.grdList,
                                            Me.lblCount, Me.btnOk, Me.btnCancel})
        Me.FormBorderStyle = FormBorderStyle.SizableToolWindow
        Me.MinimumSize = New Size(420, 260)
        Me.Name = "frmLookup"
        Me.StartPosition = FormStartPosition.CenterParent
        Me.Text = "Lookup"
        CType(Me.grdList, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()
    End Sub

End Class
