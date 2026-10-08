Partial Public Class frmReceiptEntry
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer

    Friend WithEvents tbRcp As ToolStrip
    Friend WithEvents tsbItems As ToolStripButton
    Friend WithEvents tsbPreview As ToolStripButton
    Friend WithEvents tsbPost As ToolStripButton
    Friend WithEvents tsbCorrect As ToolStripButton
    Friend WithEvents tsbClose As ToolStripButton
    Friend WithEvents grpHeader As GroupBox
    Friend WithEvents txtCustomer As TextBox
    Friend WithEvents txtReceiptDate As TextBox
    Friend WithEvents cboCurrency As ComboBox
    Friend WithEvents cboMethod As ComboBox
    Friend WithEvents txtBankAccount As TextBox
    Friend WithEvents txtAmount As TextBox
    Friend WithEvents txtBankReference As TextBox
    Friend WithEvents txtExternalReference As TextBox
    Friend WithEvents lblCustomerName As Label
    Friend WithEvents lblItemsCaption As Label
    Friend WithEvents grdItems As DataGridView
    Friend WithEvents grpTotals As GroupBox

    ''' <summary>The labelled preview boxes RCPTALOC fills in, keyed by preview name.</summary>
    Protected ReadOnly Total As New Dictionary(Of String, Label)
    ''' <summary>Preview name to the RCPTALOC COMMAREA field that holds the figure.</summary>
    Protected ReadOnly TotalFields As New Dictionary(Of String, String)

    Private Shared ReadOnly _totalSpec As String()() = New String()() {
        New String() {"APPLIED", "Applied (£):", "RA-TOT-APPLIED"},
        New String() {"DISCOUNT", "Settlement discount (£):", "RA-TOT-DISCOUNT"},
        New String() {"WRITEOFF", "Written off (£):", "RA-TOT-WRITEOFF"},
        New String() {"RESIDUAL", "Residual of receipt (£):", "RA-RESIDUAL"},
        New String() {"UNALLOCATED", "Unallocated cash (£):", "RA-UNALLOCATED"}}

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbRcp = New ToolStrip()
        Me.lblItemsCaption = New Label()
        Me.grdItems = New DataGridView()
        Me.SuspendLayout()

        Me.tbRcp.Dock = DockStyle.Top
        Me.tbRcp.RenderMode = ToolStripRenderMode.System
        Me.tbRcp.GripStyle = ToolStripGripStyle.Hidden
        Me.Controls.Add(Me.tbRcp)
        Me.tsbItems = UiStyle.Tool(Me.tbRcp, "Open items (F5)", "REFRESH", Nothing)
        Me.tsbPreview = UiStyle.Tool(Me.tbRcp, "Preview residual", "FIND", Nothing)
        Me.tsbPost = UiStyle.Tool(Me.tbRcp, "Post receipt (F10)", "POST", Nothing)
        Me.tsbCorrect = UiStyle.Tool(Me.tbRcp, "Correct allocation", "REJECT", Nothing)
        Me.tbRcp.Items.Add(New ToolStripSeparator())
        Me.tsbClose = UiStyle.Tool(Me.tbRcp, "Close (F3)", "CLOSE", Nothing)

        Me.grpHeader = UiStyle.Box(Me, "Receipt header", 8, 30, 864, 92)
        UiStyle.Caption(Me.grpHeader, "&Customer account:", 8, 18, 100)
        Me.txtCustomer = UiStyle.Edit(Me.grpHeader, 110, 16, 100, 20)
        UiStyle.Caption(Me.grpHeader, "Receipt &date:", 250, 18, 104)
        Me.txtReceiptDate = UiStyle.Edit(Me.grpHeader, 356, 16, 86, 10)
        UiStyle.Caption(Me.grpHeader, "Receipt c&urrency:", 470, 18, 118)
        Me.cboCurrency = UiStyle.Picker(Me.grpHeader, 592, 16, 70)
        Me.cboCurrency.Items.AddRange(New Object() {"GBP", "EUR", "USD"})
        UiStyle.Caption(Me.grpHeader, "&Method:", 700, 18, 66)
        Me.cboMethod = UiStyle.Picker(Me.grpHeader, 772, 16, 82)
        Me.cboMethod.Items.AddRange(New Object() {"BACS", "CHEQUE", "DD", "CARD", "CASH"})

        UiStyle.Caption(Me.grpHeader, "&Bank account:", 8, 42, 100)
        Me.txtBankAccount = UiStyle.Edit(Me.grpHeader, 110, 40, 100, 20)
        UiStyle.Caption(Me.grpHeader, "&Gross amount:", 250, 42, 104)
        Me.txtAmount = UiStyle.Edit(Me.grpHeader, 356, 40, 86, 15)
        Me.txtAmount.TextAlign = HorizontalAlignment.Right
        UiStyle.Caption(Me.grpHeader, "Bank &reference:", 470, 42, 118)
        Me.txtBankReference = UiStyle.Edit(Me.grpHeader, 592, 40, 262, 60)

        UiStyle.Caption(Me.grpHeader, "&External reference:", 8, 66, 100)
        Me.txtExternalReference = UiStyle.Edit(Me.grpHeader, 110, 64, 100, 60)
        UiStyle.Caption(Me.grpHeader, "Customer name:", 250, 66, 104)
        Me.lblCustomerName = UiStyle.Caption(Me.grpHeader, "(press F2 for the account list)", 356, 66, 498)

        Me.lblItemsCaption.Text = "Open items - key the amount to allocate, in the receipt currency, against each item:"
        Me.lblItemsCaption.SetBounds(8, 128, 600, 16)
        Me.grdItems.SetBounds(8, 146, 864, 270)
        Me.grdItems.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom

        Me.grpTotals = UiStyle.Box(Me, "Residual preview - RCPTALOC works the totals out, the screen only shows them", 8, 424, 864, 54)
        Me.grpTotals.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left Or AnchorStyles.Right
        For i As Integer = 0 To _totalSpec.Length - 1
            Dim x As Integer = 10 + i * 172
            UiStyle.Caption(Me.grpTotals, _totalSpec(i)(1), x, 22, 98)
            Dim v As New Label()
            v.Text = "-"
            v.SetBounds(x + 98, 25, 66, 15)
            v.TextAlign = ContentAlignment.TopRight
            Me.grpTotals.Controls.Add(v)
            Me.Total(_totalSpec(i)(0)) = v
            Me.TotalFields(_totalSpec(i)(0)) = _totalSpec(i)(2)
        Next

        Me.Controls.AddRange(New Control() {Me.lblItemsCaption, Me.grdItems})
        Me.ClientSize = New Size(880, 520)
        Me.Name = "frmReceiptEntry"
        Me.Text = "Receipt Entry and Allocation - Sales Ledger"
        Me.ResumeLayout(False)
    End Sub

End Class
