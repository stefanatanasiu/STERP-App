Imports STERP.Business
Imports STERP.Common

''' <summary>Approvals inbox - approve or reject with a comment (COM-003).</summary>
Public Class frmApprovals
    Inherits ListForm

    Private ReadOnly _svc As New ApprovalService()
    Private cboStatus As ComboBox

    Public Sub New()
        Me.Text = "Approvals Inbox"
    End Sub

    Protected Overrides Sub BuildFilters()
        cboStatus = FilterList("&Status:", 110, "PENDING", "APPROVED", "REJECTED", "INVALIDATED", "ALL")
        AddHandler cboStatus.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Approve (F10)", "POST", AddressOf ApproveClicked)
        AddTool("Reject", "REJECT", AddressOf RejectClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub() Populate(_svc.Inbox(Convert.ToString(cboStatus.SelectedItem))))
    End Sub

    Protected Overrides Sub OnConfirm()
        ApproveClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub ApproveClicked(ByVal sender As Object, ByVal e As EventArgs)
        Decide("APPROVED", "Approve")
    End Sub

    Private Sub RejectClicked(ByVal sender As Object, ByVal e As EventArgs)
        Decide("REJECTED", "Reject")
    End Sub

    Private Sub Decide(ByVal decision As String, ByVal verb As String)
        Dim id As Object = GridHelper.Selected(Grid, "ApprovalId")
        If id Is Nothing Then
            SetStatus("Select an approval request first.", "")
            Return
        End If
        Dim docRef As String = Fmt.Str(GridHelper.Selected(Grid, "DocumentRef"))
        Dim comment As String = InputBox(verb & " " & docRef & ".  Enter a comment for the audit trail:",
                                         "STERP - " & verb, "")
        If comment Is Nothing Then Return
        Guard(Sub()
                  Dim msg As String = _svc.Decide(Fmt.Int(id), decision, comment)
                  If msg <> "" Then
                      ShowError(msg, "")
                  Else
                      SetStatus(docRef & " " & LCase(decision) & ".", "OK")
                      LoadData()
                  End If
              End Sub)
    End Sub

End Class
