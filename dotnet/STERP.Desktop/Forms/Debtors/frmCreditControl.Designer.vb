Partial Public Class frmCreditControl
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer

    Friend WithEvents tbWork As ToolStrip
    Friend WithEvents tsbRefresh As ToolStripButton
    Friend WithEvents tsbDispute As ToolStripButton
    Friend WithEvents tsbPromise As ToolStripButton
    Friend WithEvents tsbAged As ToolStripButton
    Friend WithEvents tsbDunning As ToolStripButton
    Friend WithEvents tsbClose As ToolStripButton
    Friend WithEvents txtOwner As TextBox
    Friend WithEvents cboStage As ComboBox
    Friend WithEvents lblWorkCaption As Label
    Friend WithEvents lblAccountCaption As Label
    Friend WithEvents grdWork As DataGridView
    Friend WithEvents grdAccounts As DataGridView

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbWork = New ToolStrip()
        Me.grdWork = New DataGridView()
        Me.grdAccounts = New DataGridView()
        Me.SuspendLayout()

        Me.tbWork.Dock = DockStyle.Top
        Me.tbWork.RenderMode = ToolStripRenderMode.System
        Me.tbWork.GripStyle = ToolStripGripStyle.Hidden
        Me.Controls.Add(Me.tbWork)
        Me.tsbRefresh = UiStyle.Tool(Me.tbWork, "Refresh (F5)", "REFRESH", Nothing)
        Me.tsbDispute = UiStyle.Tool(Me.tbWork, "Raise dispute", "REJECT", Nothing)
        Me.tsbPromise = UiStyle.Tool(Me.tbWork, "Promise to pay (F10)", "POST", Nothing)
        Me.tbWork.Items.Add(New ToolStripSeparator())
        Me.tsbAged = UiStyle.Tool(Me.tbWork, "Aged debt report", "PRINT", Nothing)
        Me.tsbDunning = UiStyle.Tool(Me.tbWork, "Dunning letters", "PRINT", Nothing)
        Me.tbWork.Items.Add(New ToolStripSeparator())
        Me.tsbClose = UiStyle.Tool(Me.tbWork, "Close (F3)", "CLOSE", Nothing)

        UiStyle.Caption(Me, "&Owner (credit controller):", 8, 32, 140)
        Me.txtOwner = UiStyle.Edit(Me, 150, 32, 100, 20)
        UiStyle.Caption(Me, "Collection &stage:", 266, 32, 100)
        Me.cboStage = UiStyle.Picker(Me, 368, 32, 130)
        Me.cboStage.Items.AddRange(New Object() {"ALL STAGES", "STATEMENT", "REMINDER1", "REMINDER2",
                                                 "ACCOUNTSTOP", "FINALDEMAND"})
        Me.cboStage.SelectedIndex = 0

        Me.lblWorkCaption = UiStyle.Caption(Me, "Overdue items to work - worst stage and largest value first:", 8, 58, 520)
        Me.grdWork.SetBounds(8, 78, 884, 236)
        Me.grdWork.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        Me.lblAccountCaption = UiStyle.Caption(Me, "Accounts behind the list, with the cash sitting unapplied against them:", 8, 318, 520)
        Me.grdAccounts.SetBounds(8, 338, 884, 158)
        Me.grdAccounts.Anchor = AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom

        Me.Controls.AddRange(New Control() {Me.grdWork, Me.grdAccounts})
        Me.ClientSize = New Size(900, 528)
        Me.Name = "frmCreditControl"
        Me.Text = "Credit Control Worklist - Sales Ledger"
        Me.ResumeLayout(False)
    End Sub

End Class
