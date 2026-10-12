Imports STERP.Business.Costing
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Cost set maintenance.  A set is prepared, reviewed, approved and only then activated, and
''' only activation revalues stock.  The approver may not be the preparer: STDCOST refuses
''' that with CST0405 and this screen shows the refusal as an error rather than silently
''' doing nothing, which is the whole point of the control.
''' REQ: CST-007, CST-009, CST-012
''' </summary>
Public Class frmCostSet
    Inherits ListForm

    Private ReadOnly _svc As New CostingService()
    Private _cboStatus As ComboBox
    Private _txtCompare As TextBox
    Private _comparisonSet As String = ""

    Public Sub New()
        Me.Text = "Cost Sets"
    End Sub

    Protected Overrides Sub BuildFilters()
        _cboStatus = FilterList("&Status:", 120, "ALL", "DRAFT", "REVIEW", "APPROVED", "ACTIVE")
        _txtCompare = FilterEdit("Compare &with:", 150)
        AddHandler _cboStatus.SelectedIndexChanged, Sub(s, e) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Review", "CHECK", AddressOf ReviewClicked)
        AddTool("Approve (F10)", "POST", AddressOf ApproveClicked)
        AddTool("Activate", "RUN", AddressOf ActivateClicked)
        AddSeparator()
        AddTool("Impact", "ENGINE", AddressOf ImpactClicked)
        AddTool("Compare", "LIST", AddressOf CompareClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If _cboStatus Is Nothing Then Return
        Guard(Sub()
                  Dim t = _svc.Sets(Status())
                  _comparisonSet = ""
                  Populate(t)
                  Dim self As Integer = 0
                  For Each r As DataRow In t.Rows
                      If Fmt.Str(r("SelfApproved")) = "YES" Then self += 1
                  Next
                  SetStatus(t.Rows.Count & " cost set(s)." &
                            If(self > 0, "  " & self & " approved by their own preparer - activation will be refused.", ""),
                            If(self > 0, "!  WARNING", "OK"))
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        ApproveClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub ReviewClicked(ByVal sender As Object, ByVal e As EventArgs)
        Act("Review", Function(code) _svc.Review(code))
    End Sub

    ''' <summary>Approval of a reviewed set - never self approval (CST-007).</summary>
    Private Sub ApproveClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Chosen() = "" Then Return
        If Not Confirm("Approve cost set " & Chosen() & " as " & SterpSession.Current.UserCode & "?") Then Return
        Act("Approve", Function(code) _svc.Approve(code))
    End Sub

    ''' <summary>Activation revalues the stock it owns and raises the variance (CST-007).</summary>
    Private Sub ActivateClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Chosen() = "" Then Return
        If Not Confirm("Activate " & Chosen() & "?  This revalues stock and posts the revaluation variance.") Then Return
        Act("Activate", Function(code) _svc.Activate(code))
    End Sub

    ''' <summary>What activation would do.  Changes nothing (CST-012).</summary>
    Private Sub ImpactClicked(ByVal sender As Object, ByVal e As EventArgs)
        Act("Impact", Function(code) _svc.Impact(code))
    End Sub

    ''' <summary>Line by line against another set; an item in only one of them is kept (CST-012).</summary>
    Private Sub CompareClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = Chosen()
        If code = "" Then Return
        If _txtCompare.Text.Trim() = "" Then
            SetStatus("Enter the set to compare with first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim other As String = _txtCompare.Text.Trim().ToUpperInvariant()
                  Dim r As CobolResult = _svc.Compare(code, other)
                  If ShowResult(r) Then
                      If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                      _comparisonSet = code
                      SetStatus("Compared " & code & " with " & other &
                                ": " & _svc.Num("SC-ITEM-COUNT") & " item(s), revaluation " &
                                Fmt.Money(_svc.Num("SC-REVAL-GBP")) & ".  Press F5 for the set list.", "OK")
                  End If
              End Sub)
    End Sub

    Private Sub Act(ByVal what As String, ByVal call_ As Func(Of String, CobolResult))
        Dim code As String = Chosen()
        If code = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = call_(code)
                  If Not ShowResult(r) Then Return
                  LoadData()
                  For Each row As DataGridViewRow In Grid.Rows
                      If Fmt.Str(row.Cells("CostSet").Value) = code Then
                          Grid.CurrentCell = row.Cells("CostSet")
                          Exit For
                      End If
                  Next
                  SetStatus(what & " " & code & ": status " & _svc.Text("SC-STATUS") &
                            ", prepared by " & _svc.Text("SC-PREPARED-BY") &
                            ", approved by " & If(_svc.Text("SC-APPROVED-BY") = "", "-", _svc.Text("SC-APPROVED-BY")) &
                            ", " & _svc.Num("SC-ITEM-COUNT") & " item(s), revaluation " &
                            Fmt.Money(_svc.Num("SC-REVAL-GBP")) &
                            If(_svc.Text("SC-WARNING") <> "", "  | " & _svc.Text("SC-WARNING"), ""),
                            If(_svc.Text("SC-WARNING") <> "", "!  WARNING", "OK"))
              End Sub)
    End Sub

    Private Function Chosen() As String
        Dim c As String = Fmt.Str(GridHelper.Selected(Grid, "CostSet"))
        If c = "" Then c = _comparisonSet
        If c = "" Then SetStatus("Select a cost set first.", "!  WARNING")
        Return c
    End Function

    Private Function Status() As String
        Dim s As String = Fmt.Str(_cboStatus.SelectedItem)
        Return If(s = "ALL", "", s)
    End Function

End Class
