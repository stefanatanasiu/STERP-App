Partial Public Class frmAbout
    Inherits Form

    Private components As System.ComponentModel.IContainer
    Friend WithEvents lblProduct As Label
    Friend WithEvents lblVersion As Label
    Friend WithEvents lblCopyright As Label
    Friend WithEvents txtDetail As TextBox
    Friend WithEvents btnOk As Button

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.lblProduct = New Label()
        Me.lblVersion = New Label()
        Me.lblCopyright = New Label()
        Me.txtDetail = New TextBox()
        Me.btnOk = New Button()
        Me.SuspendLayout()
        '
        Me.lblProduct.Font = New Font("Microsoft Sans Serif", 12.0!, FontStyle.Bold)
        Me.lblProduct.ForeColor = Color.FromArgb(0, 0, 128)
        Me.lblProduct.SetBounds(12, 12, 440, 20)
        Me.lblProduct.Text = STERP.Common.SterpConfig.ProductName & "  v4.2.17"
        '
        Me.lblVersion.SetBounds(14, 34, 440, 16)
        Me.lblVersion.Text = "Release 2026/3   Serial STC-0001-4417   Windows client (x86)"
        '
        Me.lblCopyright.SetBounds(14, 50, 440, 16)
        Me.lblCopyright.Text = "(c) 1994-2026 Stellar Business Systems Limited.  All rights reserved."
        '
        Me.txtDetail.SetBounds(12, 74, 440, 226)
        Me.txtDetail.Multiline = True
        Me.txtDetail.ReadOnly = True
        Me.txtDetail.ScrollBars = ScrollBars.Both
        Me.txtDetail.WordWrap = False
        Me.txtDetail.Font = New Font("Courier New", 8.25!)
        Me.txtDetail.BackColor = UiStyle.WindowFace
        Me.txtDetail.HideSelection = True
        Me.txtDetail.TabStop = False
        '
        Me.btnOk.SetBounds(376, 308, 76, 23)
        Me.btnOk.Text = "&OK"
        Me.btnOk.FlatStyle = FlatStyle.Standard
        '
        Me.AcceptButton = Me.btnOk
        Me.CancelButton = Me.btnOk
        Me.ClientSize = New Size(464, 340)
        Me.Controls.AddRange(New Control() {Me.lblProduct, Me.lblVersion, Me.lblCopyright,
                                            Me.txtDetail, Me.btnOk})
        Me.FormBorderStyle = FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "frmAbout"
        Me.StartPosition = FormStartPosition.CenterParent
        Me.Text = "About " & STERP.Common.SterpConfig.ProductName
        Me.ResumeLayout(False)
        Me.PerformLayout()
    End Sub

End Class
