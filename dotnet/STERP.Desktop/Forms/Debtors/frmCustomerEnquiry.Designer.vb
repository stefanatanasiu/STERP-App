Partial Public Class frmCustomerEnquiry
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer

    Friend WithEvents tbEnq As ToolStrip
    Friend WithEvents tsbFind As ToolStripButton
    Friend WithEvents tsbStatement As ToolStripButton
    Friend WithEvents tsbCreditCheck As ToolStripButton
    Friend WithEvents tsbClose As ToolStripButton
    Friend WithEvents txtCustomer As TextBox
    Friend WithEvents txtAsAt As TextBox
    Friend WithEvents lblCustomerName As Label
    Friend WithEvents lblAgedCaption As Label
    Friend WithEvents lblItemsCaption As Label
    Friend WithEvents grpPosition As GroupBox
    Friend WithEvents grdAged As DataGridView
    Friend WithEvents grdItems As DataGridView

    ''' <summary>The labelled metric boxes, keyed by the name the code file sets them with.</summary>
    Protected ReadOnly Metric As New Dictionary(Of String, Label)

    '  every figure on the panel keeps a caption of its own, in reading order (COM-024)
    Private Shared ReadOnly _metricSpec As String()() = New String()() {
        New String() {"STATUS", "Account status:"}, New String() {"BALANCE", "Balance (£):"},
        New String() {"OVERDUE", "Overdue (£):"}, New String() {"DISPUTED", "Disputed (£):"},
        New String() {"LIMIT", "Credit limit (£):"}, New String() {"EXPOSURE", "Exposure (£):"},
        New String() {"AVAILABLE", "Credit available (£):"}, New String() {"UTILISED", "Credit utilised (%):"},
        New String() {"TURNOVER", "Turnover 12 months (£):"}, New String() {"DSO", "Days sales outstanding:"},
        New String() {"AVGDAYS", "Average days to pay:"}, New String() {"RECEIPTS", "Receipts 12 months (£):"},
        New String() {"ITEMS", "Open items (count):"}, New String() {"OLDEST", "Oldest due date:"},
        New String() {"WORST", "Worst days overdue:"}, New String() {"UNAPPLIED", "Unapplied cash (£):"}}

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbEnq = New ToolStrip()
        Me.grdAged = New DataGridView()
        Me.grdItems = New DataGridView()
        Me.SuspendLayout()

        Me.tbEnq.Dock = DockStyle.Top
        Me.tbEnq.RenderMode = ToolStripRenderMode.System
        Me.tbEnq.GripStyle = ToolStripGripStyle.Hidden
        Me.Controls.Add(Me.tbEnq)
        Me.tsbFind = UiStyle.Tool(Me.tbEnq, "Find (F5)", "REFRESH", Nothing)
        Me.tsbStatement = UiStyle.Tool(Me.tbEnq, "Statement", "PRINT", Nothing)
        Me.tsbCreditCheck = UiStyle.Tool(Me.tbEnq, "Credit check", "ENGINE", Nothing)
        Me.tbEnq.Items.Add(New ToolStripSeparator())
        Me.tsbClose = UiStyle.Tool(Me.tbEnq, "Close (F3)", "CLOSE", Nothing)

        UiStyle.Caption(Me, "&Customer account:", 8, 32, 100)
        Me.txtCustomer = UiStyle.Edit(Me, 110, 32, 110, 20)
        Me.lblCustomerName = UiStyle.Caption(Me, "(press F2 for the account list)", 228, 32, 380)
        UiStyle.Caption(Me, "&As at (dd/MM/yyyy):", 618, 32, 118)
        Me.txtAsAt = UiStyle.Edit(Me, 738, 32, 90, 10)

        Me.grpPosition = UiStyle.Box(Me, "Account position - all figures sterling", 8, 58, 864, 112)
        For i As Integer = 0 To _metricSpec.Length - 1
            Dim x As Integer = 10 + (i \ 4) * 214
            Dim y As Integer = 15 + (i Mod 4) * 22
            UiStyle.Caption(Me.grpPosition, _metricSpec(i)(1), x, y, 128)
            Dim v As Label = UiStyle.Caption(Me.grpPosition, "-", x + 128, y, 78)
            v.TextAlign = ContentAlignment.TopRight
            Me.Metric(_metricSpec(i)(0)) = v
        Next

        Me.lblAgedCaption = UiStyle.Caption(Me, "Aged profile (£ sterling) - every bucket is shown even when it is empty:", 8, 173, 500)
        Me.grdAged.SetBounds(8, 194, 864, 104)
        Me.grdAged.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
        Me.lblItemsCaption = UiStyle.Caption(Me, "Open items, oldest due date first:", 8, 301, 500)
        Me.grdItems.SetBounds(8, 322, 864, 206)
        Me.grdItems.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom

        Me.Controls.AddRange(New Control() {Me.grdAged, Me.grdItems})
        Me.ClientSize = New Size(880, 560)
        Me.Name = "frmCustomerEnquiry"
        Me.Text = "Customer Enquiry - Sales Ledger"
        Me.ResumeLayout(False)
    End Sub

End Class
