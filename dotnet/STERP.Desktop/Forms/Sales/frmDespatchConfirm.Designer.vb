Partial Public Class frmDespatchConfirm
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer

    Friend WithEvents tbDs As ToolStrip
    Friend WithEvents tsbLoad As ToolStripButton
    Friend WithEvents tsbConfirm As ToolStripButton
    Friend WithEvents tsbClose As ToolStripButton
    Friend WithEvents grpHeader As GroupBox
    Friend WithEvents txtPickRef As TextBox
    Friend WithEvents txtOrder As TextBox
    Friend WithEvents lblCustomer As Label
    Friend WithEvents txtDespatchDate As TextBox
    Friend WithEvents txtCarrier As TextBox
    Friend WithEvents txtCarrierRef As TextBox
    Friend WithEvents txtPacks As TextBox
    Friend WithEvents txtWeight As TextBox
    Friend WithEvents txtIncoterms As TextBox
    Friend WithEvents chkExport As CheckBox
    Friend WithEvents txtDestination As TextBox
    Friend WithEvents txtIncoPlace As TextBox
    Friend WithEvents txtEori As TextBox
    Friend WithEvents txtDeclRef As TextBox
    Friend WithEvents lblLines As Label
    Friend WithEvents grdLines As DataGridView
    Friend WithEvents grpResult As GroupBox
    Friend WithEvents lblDespatchRef As Label
    Friend WithEvents lblCogs As Label
    Friend WithEvents lblJournal As Label
    Friend WithEvents lblOutcome As Label

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbDs = New ToolStrip()
        Me.lblLines = New Label()
        Me.grdLines = New DataGridView()
        Me.SuspendLayout()

        Me.tbDs.Dock = DockStyle.Top
        Me.tbDs.RenderMode = ToolStripRenderMode.System
        Me.tbDs.GripStyle = ToolStripGripStyle.Hidden
        Me.Controls.Add(Me.tbDs)
        Me.tsbLoad = UiStyle.Tool(Me.tbDs, "Load pick (F5)", "REFRESH", Nothing)
        Me.tsbConfirm = UiStyle.Tool(Me.tbDs, "Confirm despatch (F10)", "POST", Nothing)
        Me.tbDs.Items.Add(New ToolStripSeparator())
        Me.tsbClose = UiStyle.Tool(Me.tbDs, "Close (F3)", "CLOSE", Nothing)

        Me.grpHeader = UiStyle.Box(Me, "Despatch header", 8, 30, 904, 140)
        UiStyle.Caption(Me.grpHeader, "&Pick list reference:", 8, 16, 116)
        Me.txtPickRef = UiStyle.Edit(Me.grpHeader, 128, 14, 130, 30)
        UiStyle.Caption(Me.grpHeader, "&Order number:", 268, 16, 116)
        Me.txtOrder = UiStyle.Edit(Me.grpHeader, 388, 14, 130, 30)
        UiStyle.Caption(Me.grpHeader, "Customer:", 528, 16, 116)
        Me.lblCustomer = UiStyle.Caption(Me.grpHeader, "-", 648, 16, 240)

        UiStyle.Caption(Me.grpHeader, "&Despatch date:", 8, 40, 116)
        Me.txtDespatchDate = UiStyle.Edit(Me.grpHeader, 128, 38, 130, 10)
        UiStyle.Caption(Me.grpHeader, "C&arrier:", 268, 40, 116)
        Me.txtCarrier = UiStyle.Edit(Me.grpHeader, 388, 38, 130, 20)
        UiStyle.Caption(Me.grpHeader, "Carrier &reference:", 528, 40, 116)
        Me.txtCarrierRef = UiStyle.Edit(Me.grpHeader, 648, 38, 240, 60)

        UiStyle.Caption(Me.grpHeader, "Pac&ks:", 8, 64, 116)
        Me.txtPacks = UiStyle.Edit(Me.grpHeader, 128, 62, 130, 6)
        UiStyle.Caption(Me.grpHeader, "&Gross weight (kg):", 268, 64, 116)
        Me.txtWeight = UiStyle.Edit(Me.grpHeader, 388, 62, 130, 14)
        UiStyle.Caption(Me.grpHeader, "&Incoterms rule:", 528, 64, 116)
        Me.txtIncoterms = UiStyle.Edit(Me.grpHeader, 648, 62, 60, 3)

        Me.chkExport = UiStyle.Tick(Me.grpHeader, "E&xport consignment", 8, 88, 180)
        UiStyle.Caption(Me.grpHeader, "Destination countr&y:", 268, 88, 116)
        Me.txtDestination = UiStyle.Edit(Me.grpHeader, 388, 86, 130, 2)
        UiStyle.Caption(Me.grpHeader, "Named p&lace:", 528, 88, 116)
        Me.txtIncoPlace = UiStyle.Edit(Me.grpHeader, 648, 86, 240, 60)

        UiStyle.Caption(Me.grpHeader, "Customer &EORI:", 8, 112, 116)
        Me.txtEori = UiStyle.Edit(Me.grpHeader, 128, 110, 130, 17)
        UiStyle.Caption(Me.grpHeader, "Export declaration re&f:", 268, 112, 116)
        Me.txtDeclRef = UiStyle.Edit(Me.grpHeader, 388, 110, 130, 40)

        Me.lblLines.Text = "Despatch lines - key the quantity actually going out in the ""Despatch qty"" column:"
        Me.lblLines.SetBounds(8, 176, 700, 16)
        Me.grdLines.SetBounds(8, 194, 904, 180)
        Me.grdLines.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom

        Me.grpResult = UiStyle.Box(Me, "Confirmation result - shown only when DESPATCH CONFIRM succeeded", 8, 382, 904, 96)
        Me.grpResult.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left Or AnchorStyles.Right
        UiStyle.Caption(Me.grpResult, "Despatch reference:", 8, 18, 124)
        Me.lblDespatchRef = UiStyle.Caption(Me.grpResult, "-", 136, 18, 160)
        UiStyle.Caption(Me.grpResult, "COGS value (£):", 306, 18, 100)
        Me.lblCogs = UiStyle.Caption(Me.grpResult, "-", 410, 18, 120)
        UiStyle.Caption(Me.grpResult, "Journal id:", 540, 18, 70)
        Me.lblJournal = UiStyle.Caption(Me.grpResult, "-", 614, 18, 274)
        UiStyle.Caption(Me.grpResult, "Outcome:", 8, 42, 124)
        Me.lblOutcome = UiStyle.Caption(Me.grpResult, "NOT CONFIRMED - nothing has been despatched.", 136, 42, 752)
        UiStyle.Caption(Me.grpResult, "A confirmation that fails is never shown as shipped: the reference, the COGS value and the journal stay blank.",
                        8, 64, 880)

        Me.Controls.AddRange(New Control() {Me.lblLines, Me.grdLines})
        Me.ClientSize = New Size(920, 512)
        Me.Name = "frmDespatchConfirm"
        Me.Text = "Despatch Confirmation"
        Me.ResumeLayout(False)
    End Sub

End Class
