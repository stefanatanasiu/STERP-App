Partial Public Class frmReportLauncher
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer
    Friend WithEvents tbRpt As ToolStrip
    Friend WithEvents grdReports As DataGridView
    Friend WithEvents pnlBottom As Panel
    Friend WithEvents grpParams As GroupBox
    Friend WithEvents pnlParams As Panel
    Friend WithEvents grpLog As GroupBox
    Friend WithEvents txtLog As TextBox

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbRpt = New ToolStrip()
        Me.grdReports = New DataGridView()
        Me.pnlBottom = New Panel()
        Me.grpParams = New GroupBox()
        Me.pnlParams = New Panel()
        Me.grpLog = New GroupBox()
        Me.txtLog = New TextBox()
        CType(Me.grdReports, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        Me.tbRpt.Dock = DockStyle.Top
        Me.tbRpt.RenderMode = ToolStripRenderMode.System
        Me.tbRpt.GripStyle = ToolStripGripStyle.Hidden
        Me.tbRpt.ImageScalingSize = New Size(16, 16)
        '
        Me.grdReports.Dock = DockStyle.Fill
        '
        Me.pnlBottom.Dock = DockStyle.Bottom
        Me.pnlBottom.Height = 230
        '
        Me.grpParams.Text = "Parameters"
        Me.grpParams.FlatStyle = FlatStyle.Standard
        Me.grpParams.Dock = DockStyle.Left
        Me.grpParams.Width = 280
        Me.pnlParams.Dock = DockStyle.Fill
        Me.pnlParams.AutoScroll = True
        Me.grpParams.Controls.Add(Me.pnlParams)
        '
        Me.grpLog.Text = "Run log"
        Me.grpLog.FlatStyle = FlatStyle.Standard
        Me.grpLog.Dock = DockStyle.Fill
        Me.txtLog.Dock = DockStyle.Fill
        Me.txtLog.Multiline = True
        Me.txtLog.ReadOnly = True
        Me.txtLog.WordWrap = False
        Me.txtLog.ScrollBars = ScrollBars.Both
        Me.txtLog.Font = New Font("Courier New", 8.25!)
        Me.txtLog.BackColor = SystemColors.Window
        Me.txtLog.BorderStyle = BorderStyle.Fixed3D
        Me.grpLog.Controls.Add(Me.txtLog)
        '
        Me.pnlBottom.Controls.Add(Me.grpLog)
        Me.pnlBottom.Controls.Add(Me.grpParams)
        '
        Me.ClientSize = New Size(880, 580)
        Me.MinimumSize = New Size(700, 480)
        Me.Controls.Add(Me.grdReports)
        Me.Controls.Add(Me.pnlBottom)
        Me.Controls.Add(Me.tbRpt)
        Me.Name = "frmReportLauncher"
        Me.Text = "Run Report"
        CType(Me.grdReports, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()
    End Sub

End Class
