Imports System.Data
Imports STERP.Business.Kits
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Kit execution: the requirement lines of one kit, a check pass before anything is posted,
''' component issue (with reason and approval for a shortage or an overissue), backflush of the
''' flagged components and the running cost position.
''' REQ: KIT-003, KIT-010, KIT-011, KIT-012, KIT-013, KIT-020, KIT-022
''' </summary>
Public Class frmKitExecution
    Inherits ListForm

    Private ReadOnly _kit As New KitService()
    Private ReadOnly _svc As New KitExecutionService()
    Private _txtKit As TextBox
    Private _txtQty As TextBox
    Private _txtLot As TextBox
    Private _txtReason As TextBox

    Public Sub New()
        Me.Text = "Kit Execution"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtKit = FilterEdit("&Kit:", 150)
        _txtQty = FilterEdit("&Qty:", 70)
        _txtLot = FilterEdit("&Lot/serial:", 110)
        _txtReason = FilterEdit("&Reason:", 150)
        If FormArgument <> "" Then _txtKit.Text = FormArgument
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Check", "FIND", AddressOf CheckClicked)
        AddTool("Issue (F10)", "RUN", AddressOf IssueClicked)
        AddTool("Backflush", "ENGINE", AddressOf BackflushClicked)
        AddTool("Cost", "ENGINE", AddressOf CostClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If _txtKit Is Nothing OrElse Code(_txtKit) = "" Then
            SetStatus("Enter a kit reference and press Refresh (F5).", "")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _kit.Enquire(Code(_txtKit))
                  If _kit.Lines IsNot Nothing Then Populate(_kit.Lines)
                  If ShowResult(r) Then
                      SetStatus("Kit " & Code(_txtKit) & " - " & _kit.Text("KC-STATUS") & ", " & _kit.Text("KC-COMPLETENESS") &
                                ", " & _kit.Num("KC-SHORTAGE-CNT") & " shortage(s), WIP " & Fmt.Money(_kit.Num("KC-WIP-VALUE")),
                                If(_kit.Num("KC-SHORTAGE-CNT") = 0D, "OK", "!  WARNING"))
                  End If
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        IssueClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>A check pass posts nothing - it only reports what an issue would do (KIT-010).</summary>
    Private Sub CheckClicked(ByVal sender As Object, ByVal e As EventArgs)
        Post(True, False)
    End Sub

    ''' <summary>
    ''' Issue of one component.  An overissue is offered only with a reason, and the engine
    ''' still refuses to create physical stock that is not there (KIT-011, KIT-012).
    ''' </summary>
    Private Sub IssueClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Selected() Then Return
        If Not Confirm("Issue " & _txtQty.Text & " of " & Item() & " to " & Code(_txtKit) & "?") Then Return
        Post(False, _txtReason.Text.Trim() <> "")
    End Sub

    Private Sub Post(ByVal checkOnly As Boolean, ByVal allowOver As Boolean)
        If Not Selected() Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Issue(Code(_txtKit), LineNum(), Item(), Code(_txtLot), "",
                                                    Qty(), Fmt.Str(GridHelper.Selected(Grid, "Location")),
                                                    If(allowOver, "OVERISSUE", ""), _txtReason.Text.Trim(),
                                                    allowOver, checkOnly)
                  If Not ShowResult(r) Then Return
                  SetStatus(If(checkOnly, "Check: ", "Issued: ") & Item() &
                            " | line " & _svc.Text("KS-LINE-STATUS") &
                            " | outstanding " & Fmt.Qty(_svc.Num("KS-LINE-REMAINING"), "") &
                            " | excess " & Fmt.Qty(_svc.Num("KS-LINE-EXCESS"), "") &
                            " | shortage " & Fmt.Qty(_svc.Num("KS-SHORTAGE-QTY"), "") &
                            " | WIP " & Fmt.Money(_svc.Num("KS-WIP-VALUE")),
                            If(_svc.Num("KS-SHORTAGE-QTY") = 0D, "OK", "!  WARNING"))
                  If Not checkOnly Then LoadData()
              End Sub)
    End Sub

    ''' <summary>Backflush runs a full check pass first, so it cannot half succeed (KIT-010).</summary>
    Private Sub BackflushClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Code(_txtKit) = "" Then
            SetStatus("Enter a kit reference first.", "!  WARNING")
            Return
        End If
        If Not Confirm("Backflush " & _txtQty.Text & " good output on " & Code(_txtKit) & "?") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Backflush(Code(_txtKit), Qty())
                  If Not ShowResult(r) Then Return
                  SetStatus("Backflush: " & _svc.Num("KS-LINES-DONE") & " line(s) done, " &
                            _svc.Num("KS-LINES-FAILED") & " failed" &
                            If(_svc.Text("KS-FAILED-ITEM") <> "", " (" & _svc.Text("KS-FAILED-ITEM") & ")", ""),
                            If(_svc.Num("KS-LINES-FAILED") = 0D, "OK", "*** ERROR"))
                  LoadData()
              End Sub)
    End Sub

    ''' <summary>Planned against actual, element by element (KIT-020).</summary>
    Private Sub CostClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Code(_txtKit) = "" Then
            SetStatus("Enter a kit reference first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Cost(Code(_txtKit), "STANDARD", False)
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If Not ShowResult(r) Then Return
                  SetStatus("Planned " & Fmt.Money(_svc.Num("KK-PLANNED-GBP")) &
                            " | issued " & Fmt.Money(_svc.Num("KK-ISSUED-GBP")) &
                            " | completed " & Fmt.Money(_svc.Num("KK-COMPLETED-GBP")) &
                            " | residual " & Fmt.Money(_svc.Num("KK-RESIDUAL-GBP")) &
                            " | variance " & Fmt.Money(_svc.Num("KK-VARIANCE-GBP")),
                            If(_svc.Text("KK-RECONCILED") = "Y", "OK", "!  WARNING"))
              End Sub)
    End Sub

    Private Function Selected() As Boolean
        If Code(_txtKit) <> "" AndAlso Item() <> "" Then Return True
        SetStatus("Enter a kit reference and select a component line.", "!  WARNING")
        Return False
    End Function

    Private Function Item() As String
        Return Fmt.Str(GridHelper.Selected(Grid, "Item"))
    End Function

    Private Function LineNum() As Integer
        Return Fmt.Int(GridHelper.Selected(Grid, "LineNum"))
    End Function

    Private Function Qty() As Decimal
        Dim q As Decimal
        If Not Decimal.TryParse(_txtQty.Text.Trim(), q) Then q = 0D
        Return q
    End Function

    Private Shared Function Code(ByVal t As TextBox) As String
        Return t.Text.Trim().ToUpperInvariant()
    End Function

End Class
