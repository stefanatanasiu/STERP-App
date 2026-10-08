Partial Public Class frmMain
    Inherits Form

    Private components As System.ComponentModel.IContainer
    Friend WithEvents mnuBar As MenuStrip
    Friend WithEvents tbMain As ToolStrip
    Friend WithEvents sbMain As StatusStrip
    Friend WithEvents sbMessage As ToolStripStatusLabel
    Friend WithEvents sbUser As ToolStripStatusLabel
    Friend WithEvents sbCompany As ToolStripStatusLabel
    Friend WithEvents sbSite As ToolStripStatusLabel
    Friend WithEvents sbPeriod As ToolStripStatusLabel
    Friend WithEvents sbDate As ToolStripStatusLabel
    Friend WithEvents sbEngine As ToolStripStatusLabel
    Friend WithEvents sbCaps As ToolStripStatusLabel
    Friend WithEvents sbNum As ToolStripStatusLabel
    Friend WithEvents tmrStatus As Timer

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Function Panel(ByVal name As String, ByVal width As Integer) As ToolStripStatusLabel
        Dim l As New ToolStripStatusLabel(name)
        l.AutoSize = False
        l.Width = width
        l.TextAlign = ContentAlignment.MiddleLeft
        l.BorderSides = ToolStripStatusLabelBorderSides.All
        l.BorderStyle = Border3DStyle.SunkenOuter
        Return l
    End Function

    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.mnuBar = New MenuStrip()
        Me.tbMain = New ToolStrip()
        Me.sbMain = New StatusStrip()
        Me.tmrStatus = New Timer(Me.components)
        Me.SuspendLayout()
        'mnuBar
        Me.mnuBar.RenderMode = ToolStripRenderMode.System
        Me.mnuBar.Dock = DockStyle.Top
        'tbMain
        Me.tbMain.RenderMode = ToolStripRenderMode.System
        Me.tbMain.Dock = DockStyle.Top
        Me.tbMain.GripStyle = ToolStripGripStyle.Visible
        Me.tbMain.ImageScalingSize = New Size(16, 16)
        'sbMain
        Me.sbMain.RenderMode = ToolStripRenderMode.System
        Me.sbMain.SizingGrip = True
        Me.sbMessage = New ToolStripStatusLabel("") With {.Spring = True,
            .TextAlign = ContentAlignment.MiddleLeft,
            .BorderSides = ToolStripStatusLabelBorderSides.All,
            .BorderStyle = Border3DStyle.SunkenOuter}
        Me.sbUser = Panel("User:", 150)
        Me.sbCompany = Panel("Co:", 70)
        Me.sbSite = Panel("Site:", 70)
        Me.sbPeriod = Panel("Period:", 120)
        Me.sbDate = Panel("", 72)
        Me.sbEngine = Panel("Engine:", 110)
        Me.sbCaps = Panel("", 38)
        Me.sbNum = Panel("", 36)
        Me.sbMain.Items.AddRange(New ToolStripItem() {Me.sbMessage, Me.sbUser, Me.sbCompany, Me.sbSite,
            Me.sbPeriod, Me.sbDate, Me.sbEngine, Me.sbCaps, Me.sbNum})
        'tmrStatus
        Me.tmrStatus.Interval = 1000
        'frmMain
        Me.BackColor = SystemColors.AppWorkspace
        Me.ClientSize = New Size(1008, 680)
        Me.Controls.Add(Me.tbMain)
        Me.Controls.Add(Me.mnuBar)
        Me.Controls.Add(Me.sbMain)
        Me.IsMdiContainer = True
        Me.MainMenuStrip = Me.mnuBar
        Me.Name = "frmMain"
        Me.StartPosition = FormStartPosition.CenterScreen
        Me.Text = STERP.Common.SterpConfig.ProductName
        Me.WindowState = FormWindowState.Maximized
        Me.ResumeLayout(False)
        Me.PerformLayout()
    End Sub

End Class
