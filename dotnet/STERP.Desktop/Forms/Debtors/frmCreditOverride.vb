Imports STERP.Business
Imports STERP.Common

''' <summary>Order-scoped requests and independent CREDCHK approval (AR-008, D-06).</summary>
Public Class frmCreditOverride
    Inherits BaseForm

    Private ReadOnly _svc As New DebtorsService()
    Private ReadOnly txtCustomer As New TextBox() With {.Name = "txtCustomer", .MaxLength = 20}
    Private ReadOnly txtDocument As New TextBox() With {.Name = "txtDocument", .MaxLength = 40}
    Private ReadOnly nudAmount As New NumericUpDown() With {.Name = "nudAmount", .DecimalPlaces = 2,
        .Minimum = 0.01D, .Maximum = 99999999999.99D, .ThousandsSeparator = True}
    Private ReadOnly txtReason As New TextBox() With {.Name = "txtReason", .MaxLength = 300}
    Private ReadOnly btnRequest As New Button() With {.Name = "btnRequest", .Text = "&Request override", .Width = 150}
    Private ReadOnly btnApprove As New Button() With {.Name = "btnApprove", .Text = "&Approve selected", .Width = 150}
    Private ReadOnly grdOverrides As New DataGridView() With {.Name = "grdOverrides", .Dock = DockStyle.Fill}

    Public Sub New()
        Me.Name = "frmCreditOverride"
        Me.Text = "Credit Overrides - scoped independent approval"
        Me.Size = New Size(1060, 540)
        Dim entry As New TableLayoutPanel() With {.Dock = DockStyle.Top, .Height = 160,
            .ColumnCount = 2, .RowCount = 5, .Padding = New Padding(8)}
        entry.ColumnStyles.Add(New ColumnStyle(SizeType.Absolute, 160))
        entry.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 100))
        AddField(entry, 0, "Customer account", txtCustomer)
        AddField(entry, 1, "Order reference (scope ORDER)", txtDocument)
        AddField(entry, 2, "Override amount GBP", nudAmount)
        AddField(entry, 3, "Reason", txtReason)
        Dim actions As New FlowLayoutPanel() With {.Dock = DockStyle.Fill}
        actions.Controls.AddRange(New Control() {btnRequest, btnApprove})
        entry.Controls.Add(actions, 1, 4)
        Me.Controls.Add(grdOverrides)
        Me.Controls.Add(entry)
        GridHelper.Style(grdOverrides)
        AddHandler btnRequest.Click, Sub(sender, e) RequestOverride()
        AddHandler btnApprove.Click, Sub(sender, e) ApproveOverride()
    End Sub

    Private Shared Sub AddField(ByVal panel As TableLayoutPanel, ByVal row As Integer,
                                ByVal caption As String, ByVal field As Control)
        panel.Controls.Add(New Label() With {.Text = caption, .AutoSize = True}, 0, row)
        field.Dock = DockStyle.Fill
        panel.Controls.Add(field, 1, row)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        Dim login As New LoginService()
        btnRequest.Enabled = login.HasPermission("SO.ORDER.ENTER") OrElse login.HasPermission("AR.CREDIT.OVERRIDE")
        btnApprove.Enabled = FormArgument <> "REQUEST" AndAlso login.HasPermission("AR.CREDIT.OVERRIDE")
        LoadData()
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadData()
    End Sub

    Private Sub LoadData()
        Guard(Sub()
                  GridHelper.Bind(grdOverrides, _svc.CreditOverrides(Not btnApprove.Enabled))
                  GridHelper.FillWidth(grdOverrides)
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        If FormArgument = "REQUEST" Then
            RequestOverride()
        Else
            ApproveOverride()
        End If
    End Sub

    Private Sub RequestOverride()
        If Not btnRequest.Enabled Then Return
        If txtCustomer.Text.Trim() = "" OrElse txtDocument.Text.Trim() = "" OrElse txtReason.Text.Trim() = "" Then
            SetStatus("Enter customer, order reference, GBP amount and reason.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim result = _svc.RequestCreditOverride(txtCustomer.Text.Trim().ToUpperInvariant(),
                      txtDocument.Text.Trim().ToUpperInvariant(), nudAmount.Value, txtReason.Text.Trim())
                  If Not ShowResult(result) Then Return
                  LoadData()
                  SetStatus("Override " & _svc.Area.GetInt("CK-OVERRIDE-ID") & " PENDING; " &
                            result.Message & ". Switch user for independent approval.", "OK")
              End Sub)
    End Sub

    Private Sub ApproveOverride()
        If Not btnApprove.Enabled Then Return
        Dim row As DataRow = GridHelper.SelectedRow(grdOverrides)
        If row Is Nothing Then
            SetStatus("Select a pending override first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim result = _svc.ApproveCreditOverride(Fmt.Str(row("CustomerCode")), Fmt.Int(row("OverrideId")))
                  If Not ShowResult(result) Then Return
                  LoadData()
                  SetStatus(result.Message & ". Return to Sales Order Entry for normal accept/release.", "OK")
              End Sub)
    End Sub
End Class
