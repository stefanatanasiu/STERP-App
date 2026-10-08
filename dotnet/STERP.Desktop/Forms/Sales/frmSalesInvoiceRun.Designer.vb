Partial Public Class frmSalesInvoiceRun
    Inherits BaseForm

    Private components As System.ComponentModel.IContainer

    Friend WithEvents tbRun As ToolStrip
    Friend WithEvents tsbRun As ToolStripButton
    Friend WithEvents tsbSpool As ToolStripButton
    Friend WithEvents tsbClose As ToolStripButton
    Friend WithEvents grpRun As GroupBox
    Friend WithEvents txtCompany As TextBox
    Friend WithEvents txtSite As TextBox
    Friend WithEvents cboMode As ComboBox
    Friend WithEvents txtFrom As TextBox
    Friend WithEvents txtTo As TextBox
    Friend WithEvents lblJobStatus As Label
    Friend WithEvents lblSpool As Label
    Friend WithEvents lblLogCaption As Label
    Friend WithEvents txtLog As TextBox

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.tbRun = New ToolStrip()
        Me.lblSpool = New Label()
        Me.lblLogCaption = New Label()
        Me.txtLog = New TextBox()
        Me.SuspendLayout()

        Me.tbRun.Dock = DockStyle.Top
        Me.tbRun.RenderMode = ToolStripRenderMode.System
        Me.tbRun.GripStyle = ToolStripGripStyle.Hidden
        Me.Controls.Add(Me.tbRun)
        Me.tsbRun = UiStyle.Tool(Me.tbRun, "Run SOINVRUN (F10)", "RUN", Nothing)
        Me.tsbSpool = UiStyle.Tool(Me.tbRun, "Open spool file", "PRINT", Nothing)
        Me.tbRun.Items.Add(New ToolStripSeparator())
        Me.tsbClose = UiStyle.Tool(Me.tbRun, "Close (F3)", "CLOSE", Nothing)

        Me.grpRun = UiStyle.Box(Me, "Sales invoice run - batch program SOINVRUN", 8, 30, 744, 116)
        UiStyle.Caption(Me.grpRun, "&Company:", 8, 16, 100)
        Me.txtCompany = UiStyle.Edit(Me.grpRun, 112, 14, 100, 10)
        UiStyle.Caption(Me.grpRun, "&Site:", 240, 16, 100)
        Me.txtSite = UiStyle.Edit(Me.grpRun, 344, 14, 100, 10)
        UiStyle.Caption(Me.grpRun, "&Mode:", 472, 16, 100)
        Me.cboMode = UiStyle.Picker(Me.grpRun, 576, 14, 140)
        Me.cboMode.Items.AddRange(New Object() {"PREVIEW", "COMMIT"})

        UiStyle.Caption(Me.grpRun, "Despatched &from:", 8, 40, 100)
        Me.txtFrom = UiStyle.Edit(Me.grpRun, 112, 38, 100, 10)
        UiStyle.Caption(Me.grpRun, "Despatched &to:", 240, 40, 100)
        Me.txtTo = UiStyle.Edit(Me.grpRun, 344, 38, 100, 10)
        UiStyle.Caption(Me.grpRun, "Dates are dd/MM/yyyy", 472, 40, 240)

        UiStyle.Caption(Me.grpRun, "PREVIEW lists what would be billed and has no business effect; only COMMIT raises invoices.",
                        8, 64, 700)
        UiStyle.Caption(Me.grpRun, "Job status:", 8, 88, 100)
        Me.lblJobStatus = UiStyle.Caption(Me.grpRun, "(not run)", 112, 88, 604)

        Me.lblSpool.Text = "Spool file: (none yet)"
        Me.lblSpool.SetBounds(8, 152, 744, 16)
        Me.lblSpool.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
        Me.lblLogCaption.Text = "Run log - the last STERP-I-SPOOL line names the spool file:"
        Me.lblLogCaption.SetBounds(8, 172, 500, 16)
        Me.txtLog.SetBounds(8, 190, 744, 230)
        Me.txtLog.Multiline = True
        Me.txtLog.ReadOnly = True
        Me.txtLog.WordWrap = False
        Me.txtLog.ScrollBars = ScrollBars.Both
        Me.txtLog.Font = UiStyle.FixedFont
        Me.txtLog.BorderStyle = BorderStyle.Fixed3D
        Me.txtLog.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom

        Me.Controls.AddRange(New Control() {Me.lblSpool, Me.lblLogCaption, Me.txtLog})
        Me.ClientSize = New Size(760, 460)
        Me.Name = "frmSalesInvoiceRun"
        Me.Text = "Sales Invoice Run"
        Me.ResumeLayout(False)
    End Sub

End Class
