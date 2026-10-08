Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Stock enquiry with the availability buckets: physical, eligible, reserved, free, expected
''' and planned.  Third party owned, in transit and quarantined quantities are reported
''' separately and can never be read as ordinary available stock.
''' REQ: ITM-012, ITM-013, ITM-023
''' </summary>
Public Class frmStockEnquiry
    Inherits ListForm

    Private ReadOnly _svc As New StockService()
    Private _txtItem As TextBox
    Private _txtSite As TextBox
    Private _cboCondition As ComboBox

    Public Sub New()
        Me.Text = "Stock Enquiry"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtItem = FilterEdit("&Item:", 180)
        _txtSite = FilterEdit("S&ite:", 60)
        _cboCondition = FilterList("&Condition:", 110, "ALL", "ACCEPTED", "QUARANTINE", "INSPECTION", "REJECTED", "EXPIRED")
        _txtSite.Text = SterpSession.Current.SiteCode
        If FormArgument <> "" Then _txtItem.Text = FormArgument
        AddHandler _cboCondition.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Availability (F10)", "ENGINE", AddressOf AvailabilityClicked)
        AddTool("Movements", "FIND", AddressOf MovementsClicked)
        AddTool("Lot trace", "RUN", AddressOf TraceClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub()
                  Populate(_svc.Enquiry(Item(), _txtSite.Text.Trim().ToUpperInvariant(), Condition(), False))
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        AvailabilityClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>AVAILQ owns the bucket arithmetic; the screen only reports it.</summary>
    Private Sub AvailabilityClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Item() = "" Then
            SetStatus("Enter an item code first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Availability(Item(), _txtSite.Text.Trim().ToUpperInvariant())
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If Not ShowResult(r) Then Return
                  SetStatus("Physical " & Fmt.Qty(_svc.Physical, "") & " | eligible " & Fmt.Qty(_svc.Eligible, "") &
                            " | reserved " & Fmt.Qty(_svc.ReservedQty, "") & " | free " & Fmt.Qty(_svc.FreeQty, "") &
                            " | expected " & Fmt.Qty(_svc.ExpectedQty, "") & " | planned " & Fmt.Qty(_svc.PlannedQty, ""),
                            If(_svc.FreeQty > 0D, "OK", "!  WARNING"))
              End Sub)
    End Sub

    Private Sub MovementsClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub() Populate(_svc.Movements(Item(), _txtSite.Text.Trim().ToUpperInvariant(), 180)))
    End Sub

    Private Sub TraceClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim lot As String = Fmt.Str(GridHelper.Selected(Grid, "LotCode"))
        Dim serial As String = Fmt.Str(GridHelper.Selected(Grid, "SerialNo"))
        If Item() = "" OrElse (lot = "" AndAlso serial = "") Then
            SetStatus("Select a lot or serial row first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Trace(Item(), lot, serial)
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If ShowResult(r) Then SetStatus("Traceability for " & If(lot <> "", lot, serial) & ".", "OK")
              End Sub)
    End Sub

    Private Function Item() As String
        Return _txtItem.Text.Trim().ToUpperInvariant()
    End Function

    Private Function Condition() As String
        Dim c As String = Convert.ToString(_cboCondition.SelectedItem)
        Return If(c = "ALL", "", c)
    End Function

End Class
