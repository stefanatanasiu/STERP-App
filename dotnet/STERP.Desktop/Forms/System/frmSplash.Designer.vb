Partial Public Class frmSplash
    Inherits Form

    Private components As System.ComponentModel.IContainer
    Friend WithEvents pnlFrame As Panel
    Friend WithEvents lblProduct As Label
    Friend WithEvents lblSub As Label
    Friend WithEvents lblVersion As Label
    Friend WithEvents lblCopyright As Label
    Friend WithEvents lblStep As Label

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.pnlFrame = New Panel()
        Me.lblProduct = New Label()
        Me.lblSub = New Label()
        Me.lblVersion = New Label()
        Me.lblCopyright = New Label()
        Me.lblStep = New Label()
        Me.SuspendLayout()
        'pnlFrame
        Me.pnlFrame.BackColor = Color.FromArgb(0, 0, 128)
        Me.pnlFrame.Location = New Point(0, 0)
        Me.pnlFrame.Size = New Size(422, 6)
        'lblProduct
        Me.lblProduct.Font = New Font("Microsoft Sans Serif", 20.0!, FontStyle.Bold)
        Me.lblProduct.ForeColor = Color.FromArgb(0, 0, 128)
        Me.lblProduct.Location = New Point(16, 20)
        Me.lblProduct.Size = New Size(390, 32)
        Me.lblProduct.Text = STERP.Common.SterpConfig.ProductName
        'lblSub
        Me.lblSub.Font = New Font("Microsoft Sans Serif", 11.0!)
        Me.lblSub.Location = New Point(18, 52)
        Me.lblSub.Size = New Size(390, 20)
        Me.lblSub.Text = "Finance, sales, purchasing, stock and manufacturing"
        'lblVersion
        Me.lblVersion.Location = New Point(18, 84)
        Me.lblVersion.Size = New Size(390, 16)
        Me.lblVersion.Text = "Version 4.2.17   Release 2026/3   Serial STC-0001-4417"
        'lblCopyright
        Me.lblCopyright.Location = New Point(18, 102)
        Me.lblCopyright.Size = New Size(390, 16)
        Me.lblCopyright.Text = "(c) 1994-2026 Stellar Business Systems Limited.  All rights reserved."
        'lblStep
        Me.lblStep.BorderStyle = BorderStyle.Fixed3D
        Me.lblStep.Location = New Point(16, 132)
        Me.lblStep.Size = New Size(390, 18)
        Me.lblStep.TextAlign = ContentAlignment.MiddleLeft
        'frmSplash
        Me.BackColor = SystemColors.Control
        Me.ControlBox = False
        Me.Controls.AddRange(New Control() {Me.lblStep, Me.lblCopyright, Me.lblVersion,
                                            Me.lblSub, Me.lblProduct, Me.pnlFrame})
        Me.FormBorderStyle = FormBorderStyle.FixedDialog
        ' set after the border style: changing the border keeps the outer size and would
        ' otherwise eat the progress line at the foot of the banner
        Me.ClientSize = New Size(422, 162)
        Me.Name = "frmSplash"
        Me.ShowInTaskbar = False
        Me.StartPosition = FormStartPosition.CenterScreen
        Me.Text = STERP.Common.SterpConfig.ProductName
        Me.ResumeLayout(False)
    End Sub

End Class
