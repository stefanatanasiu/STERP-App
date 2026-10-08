Partial Public Class frmSpoolViewer
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer
    Friend WithEvents tbSpool As ToolStrip
    Friend WithEvents pnlTop As Panel
    Friend WithEvents lblPage As Label
    Friend WithEvents txtPage As TextBox

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbSpool = New ToolStrip()
        Me.pnlTop = New Panel()
        Me.lblPage = New Label()
        Me.txtPage = New TextBox()
        Me.SuspendLayout()
        '
        Me.tbSpool.Dock = DockStyle.Top
        Me.tbSpool.RenderMode = ToolStripRenderMode.System
        Me.tbSpool.GripStyle = ToolStripGripStyle.Hidden
        Me.tbSpool.ImageScalingSize = New Size(16, 16)
        '
        Me.pnlTop.Dock = DockStyle.Top
        Me.pnlTop.Height = 20
        Me.lblPage.SetBounds(8, 3, 220, 16)
        Me.lblPage.Text = "Page 0 of 0"
        Me.pnlTop.Controls.Add(Me.lblPage)
        '
        Me.txtPage.Dock = DockStyle.Fill
        Me.txtPage.Multiline = True
        Me.txtPage.ReadOnly = True
        Me.txtPage.WordWrap = False
        Me.txtPage.ScrollBars = ScrollBars.Both
        Me.txtPage.BackColor = SystemColors.Window
        Me.txtPage.BorderStyle = BorderStyle.Fixed3D
        '
        Me.ClientSize = New Size(860, 560)
        Me.MinimumSize = New Size(620, 380)
        Me.Controls.Add(Me.txtPage)
        Me.Controls.Add(Me.pnlTop)
        Me.Controls.Add(Me.tbSpool)
        Me.Name = "frmSpoolViewer"
        Me.Text = "Spool File Viewer"
        Me.ResumeLayout(False)
        Me.PerformLayout()
    End Sub

End Class
