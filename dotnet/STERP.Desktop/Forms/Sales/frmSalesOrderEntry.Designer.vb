Partial Public Class frmSalesOrderEntry
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer

    Friend WithEvents tbSo As ToolStrip
    Friend WithEvents tsbLoad As ToolStripButton
    Friend WithEvents tsbExplain As ToolStripButton
    Friend WithEvents tsbAvail As ToolStripButton
    Friend WithEvents tsbReserve As ToolStripButton
    Friend WithEvents tsbAccept As ToolStripButton
    Friend WithEvents tsbClose As ToolStripButton
    Friend WithEvents grpHeader As GroupBox
    Friend WithEvents txtOrder As TextBox
    Friend WithEvents txtCustomer As TextBox
    Friend WithEvents txtCustomerRef As TextBox
    Friend WithEvents txtSalesperson As TextBox
    Friend WithEvents txtOrderDate As TextBox
    Friend WithEvents txtCurrency As TextBox
    Friend WithEvents txtTerms As TextBox
    Friend WithEvents txtIncoterms As TextBox
    Friend WithEvents txtIncoPlace As TextBox
    Friend WithEvents txtDeliveryRef As TextBox
    Friend WithEvents lblOrderType As Label
    Friend WithEvents lblCustomerName As Label
    Friend WithEvents lblRelease As Label
    Friend WithEvents lblFulfil As Label
    Friend WithEvents lblBilling As Label
    Friend WithEvents lblDocNet As Label
    Friend WithEvents lblHold As Label
    Friend WithEvents lblLines As Label
    Friend WithEvents grdLines As DataGridView
    Friend WithEvents grpWhy As GroupBox
    Friend WithEvents grdWhy As DataGridView
    Friend WithEvents grpAvail As GroupBox
    Friend WithEvents txtAvail As TextBox

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbSo = New ToolStrip()
        Me.lblLines = New Label()
        Me.grdLines = New DataGridView()
        Me.txtAvail = New TextBox()
        Me.grdWhy = New DataGridView()
        Me.SuspendLayout()

        Me.tbSo.Dock = DockStyle.Top
        Me.tbSo.RenderMode = ToolStripRenderMode.System
        Me.tbSo.GripStyle = ToolStripGripStyle.Hidden
        Me.Controls.Add(Me.tbSo)
        Me.tsbLoad = UiStyle.Tool(Me.tbSo, "Load order (F5)", "REFRESH", Nothing)
        Me.tsbExplain = UiStyle.Tool(Me.tbSo, "Explain price", "FIND", Nothing)
        Me.tsbAvail = UiStyle.Tool(Me.tbSo, "Availability (F2)", "ENGINE", Nothing)
        Me.tsbReserve = UiStyle.Tool(Me.tbSo, "Reserve line", "SAVE", Nothing)
        Me.tsbAccept = UiStyle.Tool(Me.tbSo, "Accept / release (F10)", "POST", Nothing)
        Me.tbSo.Items.Add(New ToolStripSeparator())
        Me.tsbClose = UiStyle.Tool(Me.tbSo, "Close (F3)", "CLOSE", Nothing)

        Me.grpHeader = UiStyle.Box(Me, "Order header", 8, 30, 964, 140)
        UiStyle.Caption(Me.grpHeader, "&Order number:", 8, 16, 104)
        Me.txtOrder = UiStyle.Edit(Me.grpHeader, 116, 14, 120, 30)
        UiStyle.Caption(Me.grpHeader, "C&ustomer:", 244, 16, 104)
        Me.txtCustomer = UiStyle.Edit(Me.grpHeader, 352, 14, 120, 20)
        UiStyle.Caption(Me.grpHeader, "Customer order &ref:", 480, 16, 108)
        Me.txtCustomerRef = UiStyle.Edit(Me.grpHeader, 592, 14, 130, 40)
        UiStyle.Caption(Me.grpHeader, "Sales&person:", 730, 16, 100)
        Me.txtSalesperson = UiStyle.Edit(Me.grpHeader, 834, 14, 120, 20)

        UiStyle.Caption(Me.grpHeader, "Order &date:", 8, 40, 104)
        Me.txtOrderDate = UiStyle.Edit(Me.grpHeader, 116, 38, 120, 10)
        UiStyle.Caption(Me.grpHeader, "Curre&ncy:", 244, 40, 104)
        Me.txtCurrency = UiStyle.Edit(Me.grpHeader, 352, 38, 120, 3)
        UiStyle.Caption(Me.grpHeader, "Payment &terms:", 480, 40, 108)
        Me.txtTerms = UiStyle.Edit(Me.grpHeader, 592, 38, 130, 12)
        UiStyle.Caption(Me.grpHeader, "Order type:", 730, 40, 100)
        Me.lblOrderType = UiStyle.Caption(Me.grpHeader, "-", 834, 40, 120)

        UiStyle.Caption(Me.grpHeader, "&Incoterms rule:", 8, 64, 104)
        Me.txtIncoterms = UiStyle.Edit(Me.grpHeader, 116, 62, 120, 3)
        UiStyle.Caption(Me.grpHeader, "Named p&lace:", 244, 64, 104)
        Me.txtIncoPlace = UiStyle.Edit(Me.grpHeader, 352, 62, 120, 60)
        UiStyle.Caption(Me.grpHeader, "Delivery &address:", 480, 64, 108)
        Me.txtDeliveryRef = UiStyle.Edit(Me.grpHeader, 592, 62, 130, 10)
        UiStyle.Caption(Me.grpHeader, "Customer name:", 730, 64, 100)
        Me.lblCustomerName = UiStyle.Caption(Me.grpHeader, "-", 834, 64, 120)

        UiStyle.Caption(Me.grpHeader, "Release status:", 8, 88, 104)
        Me.lblRelease = UiStyle.Caption(Me.grpHeader, "-", 116, 88, 120)
        UiStyle.Caption(Me.grpHeader, "Fulfilment status:", 244, 88, 104)
        Me.lblFulfil = UiStyle.Caption(Me.grpHeader, "-", 352, 88, 120)
        UiStyle.Caption(Me.grpHeader, "Billing status:", 480, 88, 108)
        Me.lblBilling = UiStyle.Caption(Me.grpHeader, "-", 592, 88, 130)
        UiStyle.Caption(Me.grpHeader, "Document net:", 730, 88, 100)
        Me.lblDocNet = UiStyle.Caption(Me.grpHeader, "-", 834, 88, 120)

        UiStyle.Caption(Me.grpHeader, "Hold reason:", 8, 112, 104)
        Me.lblHold = UiStyle.Caption(Me.grpHeader, "(none - the order is not on hold)", 116, 112, 838)

        Me.lblLines.Text = "Order lines - quantity, unit, unit price and price source are the engine's, not the screen's:"
        Me.lblLines.SetBounds(8, 176, 700, 16)
        Me.grdLines.SetBounds(8, 194, 964, 170)
        Me.grdLines.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom

        Me.grpWhy = UiStyle.Box(Me, "Price explanation - PRICING EXPLAIN, ranked candidates and why each was or was not chosen", 8, 372, 596, 196)
        Me.grpWhy.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        Me.grdWhy.SetBounds(8, 16, 580, 172)
        Me.grdWhy.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        Me.grpWhy.Controls.Add(Me.grdWhy)

        Me.grpAvail = UiStyle.Box(Me, "Availability (F2) - SOAVAIL, each source on its own line", 612, 372, 360, 196)
        Me.grpAvail.Anchor = AnchorStyles.Bottom Or AnchorStyles.Right
        Me.txtAvail.SetBounds(8, 16, 344, 172)
        Me.txtAvail.Multiline = True
        Me.txtAvail.ReadOnly = True
        Me.txtAvail.WordWrap = False
        Me.txtAvail.ScrollBars = ScrollBars.Both
        Me.txtAvail.Font = UiStyle.FixedFont
        Me.txtAvail.BorderStyle = BorderStyle.Fixed3D
        Me.txtAvail.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        Me.txtAvail.Text = "Press F2 on a line for free stock, reservations and expected receipts."
        Me.grpAvail.Controls.Add(Me.txtAvail)

        Me.Controls.AddRange(New Control() {Me.lblLines, Me.grdLines})
        Me.ClientSize = New Size(980, 604)
        Me.Name = "frmSalesOrderEntry"
        Me.Text = "Sales Order Entry"
        Me.ResumeLayout(False)
    End Sub

End Class
