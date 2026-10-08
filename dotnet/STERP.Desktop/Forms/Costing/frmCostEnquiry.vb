Imports STERP.Business.Costing
Imports STERP.Common

''' <summary>
''' Item cost enquiry.  Every basis we hold for the item is shown side by side - average,
''' the oldest FIFO layer, the standard from the active set, the allowance and the net
''' realisable value - and the screen states which basis it is showing rather than letting
''' the reader assume one.  Nothing here changes a cost.
''' REQ: CST-001, CST-021, CST-026
''' </summary>
Public Class frmCostEnquiry
    Inherits ListForm

    Private ReadOnly _svc As New CostingService()
    Private _txtItem As TextBox
    Private _txtRev As TextBox
    Private _txtSite As TextBox

    Public Sub New()
        Me.Text = "Item Cost Enquiry"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtItem = FilterEdit("&Item:", 200)
        _txtRev = FilterEdit("&Rev:", 50)
        _txtSite = FilterEdit("&Site:", 70)
        If FormArgument <> "" Then _txtItem.Text = FormArgument
    End Sub

    Protected Overrides Sub LoadData()
        If _txtItem Is Nothing Then Return
        If _txtItem.Text.Trim() = "" Then
            SetStatus("Enter an item code and press Refresh (F5).", "")
            Return
        End If
        Guard(Sub()
                  Dim t = _svc.CostEnquiry(_txtItem.Text.Trim().ToUpperInvariant(),
                                           _txtRev.Text.Trim(), _txtSite.Text.Trim())
                  Populate(t)
                  SetStatus(t.Rows.Count & " basis row(s).  A basis is stated, never substituted.",
                            If(t.Rows.Count = 0, "!  WARNING", "OK"))
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        LoadData()
    End Sub

End Class
