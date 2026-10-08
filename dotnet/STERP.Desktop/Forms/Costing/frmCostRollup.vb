Imports STERP.Business.Costing
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Cost roll-up and what-if enquiry.  COSTROLL prices the item for a stated good output -
''' calling BOMCOST for the explosion, because costing never explodes a bill itself - and the
''' costed tree is shown with the element breakdown underneath it.  An incomplete roll-up is
''' labelled with the count of missing costs and rates; it is never presented as a price.
''' WHATIF prices variants beside the base case and writes nothing at all (CST-023).
''' REQ: CST-002, CST-010, CST-023, CST-024
''' </summary>
Public Class frmCostRollup
    Inherits ListForm

    Private ReadOnly _svc As New CostingService()
    Private _txtItem As TextBox
    Private _txtRev As TextBox
    Private _txtSet As TextBox
    Private _txtLot As TextBox
    Private _txtPrice As TextBox
    Private _cboBoundary As ComboBox

    Public Sub New()
        Me.Text = "Cost Roll-up and What-if"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtItem = FilterEdit("&Item:", 180)
        _txtRev = FilterEdit("&Rev:", 50)
        _txtSet = FilterEdit("Cost &set:", 130)
        _txtLot = FilterEdit("&Lot size:", 70)
        _txtPrice = FilterEdit("Target &price:", 80)
        _cboBoundary = FilterList("&Boundary:", 170, "STOP AT SUBASSEMBLY", "EXPAND SUBASSEMBLY")
        _txtLot.Text = "1"
        If FormArgument <> "" Then _txtItem.Text = FormArgument
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Roll-up (F10)", "ENGINE", AddressOf RollupClicked)
        AddTool("What-if", "RUN", AddressOf WhatIfClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If _txtItem Is Nothing Then Return
        SetStatus("Enter an item and press Roll-up (F10).  Nothing is written by this screen.", "")
    End Sub

    Protected Overrides Sub OnConfirm()
        RollupClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>One valuation boundary per branch, stated on every line (CST-010).</summary>
    Private Sub RollupClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Asked() Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Rollup(Item(), _txtRev.Text.Trim(), CostSet(), Lot(),
                                                     _cboBoundary.SelectedIndex = 1)
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If Not ShowResult(r) Then Return
                  Dim missing As Integer = CInt(_svc.Num("CR-MISSING-COUNT"))
                  SetStatus("Material " & Fmt.Money(_svc.Num("CR-MATERIAL-COST")) &
                            " | conversion " & Fmt.Money(_svc.Num("CR-LABOUR-COST") + _svc.Num("CR-MACHINE-COST") +
                                                         _svc.Num("CR-SETUP-COST") + _svc.Num("CR-SUBCON-COST")) &
                            " | overhead " & Fmt.Money(_svc.Num("CR-OVERHEAD-COST")) &
                            " | recovery " & Fmt.Money(_svc.Num("CR-RECOVERY-CREDIT")) &
                            " | total " & Fmt.Money(_svc.Num("CR-TOTAL-COST")) &
                            " | unit " & Fmt.Money(_svc.Num("CR-UNIT-COST")) &
                            " | " & _svc.Text("CR-COMPLETENESS") &
                            If(missing > 0, ", " & missing & " missing cost(s) or rate(s)", "") &
                            If(_svc.Text("CR-WARNING") <> "", " | " & _svc.Text("CR-WARNING"), ""),
                            If(_svc.Text("CR-COMPLETENESS") = "COMPLETE" AndAlso missing = 0, "OK", "!  WARNING"))
              End Sub)
    End Sub

    ''' <summary>Base case and variants priced side by side.  Nothing is saved (CST-023).</summary>
    Private Sub WhatIfClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Asked() Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Scenario(Item(), _txtRev.Text.Trim(), CostSet(), Lot(),
                                                       Price(), 1D, 1D)
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If Not ShowResult(r) Then Return
                  Dim best As Integer = CInt(_svc.Num("WI-BEST-VARIANT"))
                  SetStatus("Base " & Fmt.Money(_svc.Num("WI-BASE-TOTAL")) & ", unit " &
                            Fmt.Money(_svc.Num("WI-BASE-UNIT")) & " (" & _svc.Text("WI-BASE-COMPLETE") & ")" &
                            If(best > 0, " | best variant " & best, "") &
                            If(Price() > 0D, " | priced against " & Fmt.Money(Price()), "") &
                            " | simulation only, nothing written" &
                            If(_svc.Text("WI-WARNING") <> "", " | " & _svc.Text("WI-WARNING"), ""),
                            If(_svc.Text("WI-BASE-COMPLETE") = "COMPLETE", "OK", "!  WARNING"))
              End Sub)
    End Sub

    Private Function Asked() As Boolean
        If Item() <> "" Then Return True
        SetStatus("Enter an item code first.", "!  WARNING")
        Return False
    End Function

    Private Function Item() As String
        Return _txtItem.Text.Trim().ToUpperInvariant()
    End Function

    Private Function CostSet() As String
        Return _txtSet.Text.Trim().ToUpperInvariant()
    End Function

    Private Function Lot() As Decimal
        Dim q As Decimal
        If Not Decimal.TryParse(_txtLot.Text.Trim(), q) OrElse q <= 0D Then q = 1D
        Return q
    End Function

    Private Function Price() As Decimal
        Dim p As Decimal
        If Not Decimal.TryParse(_txtPrice.Text.Trim(), p) Then p = 0D
        Return p
    End Function

End Class
