Partial Public Class frmEngineDiag
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer
    Friend WithEvents tbDiag As ToolStrip
    Friend WithEvents grpStatus As GroupBox
    Friend WithEvents lblEngine As Label
    Friend WithEvents lblDb As Label
    Friend WithEvents lblTime As Label
    Friend WithEvents lblDbVersion As Label
    Friend WithEvents lblRc As Label
    Friend WithEvents lblMessage As Label
    Friend WithEvents grpPrograms As GroupBox
    Friend WithEvents lstPrograms As ListBox
    Friend WithEvents lblCount As Label
    Friend WithEvents grpTest As GroupBox
    Friend WithEvents lblAmount As Label
    Friend WithEvents txtAmount As TextBox
    Friend WithEvents txtDump As TextBox

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Function Value(ByVal parent As Control, ByVal caption As String, ByVal y As Integer) As Label
        Dim l As New Label()
        l.Text = caption
        l.SetBounds(10, y + 2, 118, 16)
        parent.Controls.Add(l)
        Dim v As New Label()
        v.BorderStyle = BorderStyle.Fixed3D
        v.SetBounds(130, y, 430, 18)
        v.TextAlign = ContentAlignment.MiddleLeft
        parent.Controls.Add(v)
        Return v
    End Function

    Private Sub InitializeComponent()
        Me.tbDiag = New ToolStrip()
        Me.grpStatus = New GroupBox()
        Me.grpPrograms = New GroupBox()
        Me.lstPrograms = New ListBox()
        Me.lblCount = New Label()
        Me.grpTest = New GroupBox()
        Me.lblAmount = New Label()
        Me.txtAmount = New TextBox()
        Me.txtDump = New TextBox()
        Me.SuspendLayout()
        '
        Me.tbDiag.Dock = DockStyle.Top
        Me.tbDiag.RenderMode = ToolStripRenderMode.System
        Me.tbDiag.GripStyle = ToolStripGripStyle.Hidden
        Me.tbDiag.ImageScalingSize = New Size(16, 16)
        '
        Me.grpStatus.Text = "Engine and database"
        Me.grpStatus.FlatStyle = FlatStyle.Standard
        Me.grpStatus.SetBounds(8, 32, 580, 162)
        Me.lblEngine = Value(Me.grpStatus, "Engine version:", 20)
        Me.lblDb = Value(Me.grpStatus, "Database:", 42)
        Me.lblTime = Value(Me.grpStatus, "Server time:", 64)
        Me.lblDbVersion = Value(Me.grpStatus, "Database version:", 86)
        Me.lblRc = Value(Me.grpStatus, "Return code:", 108)
        Me.lblMessage = Value(Me.grpStatus, "Message:", 130)
        '
        Me.grpPrograms.Text = "Program directory"
        Me.grpPrograms.FlatStyle = FlatStyle.Standard
        Me.grpPrograms.SetBounds(596, 32, 230, 420)
        Me.lstPrograms.BorderStyle = BorderStyle.Fixed3D
        Me.lstPrograms.IntegralHeight = False
        Me.lstPrograms.SetBounds(10, 20, 210, 360)
        Me.lblCount.SetBounds(10, 386, 210, 16)
        Me.grpPrograms.Controls.AddRange(New Control() {Me.lstPrograms, Me.lblCount})
        '
        Me.grpTest.Text = "COMMAREA test (PINGCA)"
        Me.grpTest.FlatStyle = FlatStyle.Standard
        Me.grpTest.SetBounds(8, 200, 580, 252)
        Me.lblAmount.Text = "PG-ECHO-AMOUNT:"
        Me.lblAmount.SetBounds(10, 23, 118, 16)
        Me.txtAmount.SetBounds(130, 20, 110, 20)
        Me.txtAmount.BorderStyle = BorderStyle.Fixed3D
        Me.txtAmount.TextAlign = HorizontalAlignment.Right
        Me.txtDump.SetBounds(10, 46, 560, 196)
        Me.txtDump.Multiline = True
        Me.txtDump.ReadOnly = True
        Me.txtDump.WordWrap = False
        Me.txtDump.ScrollBars = ScrollBars.Both
        Me.txtDump.BackColor = SystemColors.Window
        Me.txtDump.BorderStyle = BorderStyle.Fixed3D
        Me.grpTest.Controls.AddRange(New Control() {Me.lblAmount, Me.txtAmount, Me.txtDump})
        '
        Me.ClientSize = New Size(836, 492)
        Me.MinimumSize = New Size(700, 430)
        Me.Controls.AddRange(New Control() {Me.grpStatus, Me.grpTest, Me.grpPrograms, Me.tbDiag})
        Me.Name = "frmEngineDiag"
        Me.Text = "Engine Diagnostics"
        Me.ResumeLayout(False)
        Me.PerformLayout()
    End Sub

End Class
