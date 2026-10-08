Imports System.Data
Imports STERP.Business.Kits
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Genealogy enquiry.  GENEALOG walks a lot or serial forwards into the parents it built,
''' backwards into the components a parent actually consumed, or answers the where-used recall
''' question.  Substitutions, returns and scrap stay visible, and the planned component is
''' always distinguishable from the one actually issued.
''' REQ: KIT-019, KIT-022
''' </summary>
Public Class frmKitGenealogy
    Inherits ListForm

    Private ReadOnly _svc As New KitExecutionService()
    Private _txtItem As TextBox
    Private _txtLot As TextBox
    Private _txtSerial As TextBox
    Private _txtKit As TextBox
    Private _cboDirection As ComboBox

    Public Sub New()
        Me.Text = "Genealogy Enquiry"
    End Sub

    Protected Overrides Sub BuildFilters()
        _cboDirection = FilterList("&Direction:", 110, "FORWARD", "BACKWARD", "WHEREUSD")
        _txtItem = FilterEdit("&Item:", 150)
        _txtLot = FilterEdit("&Lot:", 110)
        _txtSerial = FilterEdit("&Serial:", 110)
        _txtKit = FilterEdit("&Kit:", 130)
        If FormArgument <> "" Then _txtItem.Text = FormArgument
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Trace (F10)", "ENGINE", AddressOf TraceClicked)
        AddTool("Recall list", "FIND", AddressOf RecallClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If _txtItem Is Nothing Then Return
        If Code(_txtItem) = "" AndAlso Code(_txtKit) = "" Then
            SetStatus("Enter an item with its lot or serial, or a kit reference, then Trace (F10).", "")
            Return
        End If
        TraceClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnConfirm()
        TraceClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub TraceClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Code(_txtItem) = "" AndAlso Code(_txtKit) = "" Then
            SetStatus("Enter an item or a kit reference first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Genealogy(Direction(), Code(_txtKit), Code(_txtItem),
                                                        Code(_txtLot), Code(_txtSerial), 10)
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If Not ShowResult(r) Then Return
                  SetStatus(Direction() & ": " & _svc.Num("GN-KIT-CNT") & " kit(s), depth " &
                            _svc.Num("GN-DEPTH-REACHED") & ", " & _svc.Num("GN-SUBSTITUTE-CNT") &
                            " substitution(s), " & _svc.Num("GN-RETURN-CNT") & " return(s), " &
                            _svc.Num("GN-SCRAP-CNT") & " scrap" &
                            If(_svc.Text("GN-TRUNCATED") = "Y", "  *** TRUNCATED", ""),
                            If(_svc.Text("GN-TRUNCATED") = "Y", "!  WARNING", "OK"))
              End Sub)
    End Sub

    ''' <summary>The whole recall list from the database, for an export (KIT-019).</summary>
    Private Sub RecallClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Code(_txtItem) = "" Then
            SetStatus("A recall list needs an item code.", "!  WARNING")
            Return
        End If
        Guard(Sub() Populate(_svc.GenealogyList(Code(_txtItem), Code(_txtLot), Code(_txtSerial), 5)))
    End Sub

    Private Function Direction() As String
        Return Convert.ToString(_cboDirection.SelectedItem)
    End Function

    Private Shared Function Code(ByVal t As TextBox) As String
        Return t.Text.Trim().ToUpperInvariant()
    End Function

End Class
