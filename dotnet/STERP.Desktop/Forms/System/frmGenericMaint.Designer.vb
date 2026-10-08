Partial Public Class frmGenericMaint
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer
    Friend WithEvents tbMaint As ToolStrip
    Friend WithEvents pnlFind As Panel
    Friend WithEvents lblFind As Label
    Friend WithEvents txtSearch As TextBox
    Friend WithEvents btnSearch As Button
    Friend WithEvents lblHelp As Label
    Friend WithEvents grdList As DataGridView
    Friend WithEvents splitter As Splitter
    Friend WithEvents pnlEdit As Panel

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbMaint = New ToolStrip()
        Me.pnlFind = New Panel()
        Me.lblFind = New Label()
        Me.txtSearch = New TextBox()
        Me.btnSearch = New Button()
        Me.lblHelp = New Label()
        Me.grdList = New DataGridView()
        Me.splitter = New Splitter()
        Me.pnlEdit = New Panel()
        CType(Me.grdList, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        Me.tbMaint.Dock = DockStyle.Top
        Me.tbMaint.RenderMode = ToolStripRenderMode.System
        Me.tbMaint.GripStyle = ToolStripGripStyle.Hidden
        Me.tbMaint.ImageScalingSize = New Size(16, 16)
        '
        Me.pnlFind.Dock = DockStyle.Top
        Me.pnlFind.Height = 28
        Me.lblFind.Text = "&Find:"
        Me.lblFind.SetBounds(8, 7, 36, 16)
        Me.txtSearch.SetBounds(44, 4, 180, 20)
        Me.txtSearch.BorderStyle = BorderStyle.Fixed3D
        Me.btnSearch.SetBounds(228, 3, 70, 22)
        Me.btnSearch.Text = "&Search"
        Me.btnSearch.FlatStyle = FlatStyle.Standard
        Me.lblHelp.SetBounds(310, 7, 520, 16)
        Me.lblHelp.ForeColor = SystemColors.GrayText
        Me.pnlFind.Controls.AddRange(New Control() {Me.lblFind, Me.txtSearch, Me.btnSearch, Me.lblHelp})
        '
        Me.pnlEdit.Dock = DockStyle.Bottom
        Me.pnlEdit.Height = 180
        Me.pnlEdit.BorderStyle = BorderStyle.Fixed3D
        Me.pnlEdit.AutoScroll = True
        '
        Me.splitter.Dock = DockStyle.Bottom
        Me.splitter.Height = 4
        '
        Me.grdList.Dock = DockStyle.Fill
        '
        Me.ClientSize = New Size(880, 540)
        Me.MinimumSize = New Size(680, 470)
        Me.Controls.Add(Me.grdList)
        Me.Controls.Add(Me.splitter)
        Me.Controls.Add(Me.pnlEdit)
        Me.Controls.Add(Me.pnlFind)
        Me.Controls.Add(Me.tbMaint)
        Me.Name = "frmGenericMaint"
        Me.Text = "File maintenance"
        CType(Me.grdList, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()
    End Sub

End Class
