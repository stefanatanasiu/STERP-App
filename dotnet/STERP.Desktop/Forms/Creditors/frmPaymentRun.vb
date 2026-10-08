Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Payment run: the proposals, what each one selected and excluded, the independent approval
''' and the instructions the run produced.  A proposal may never be approved by the person who
''' prepared it; the engine refuses and the refusal is shown as an error.
''' REQ: AP-011, AP-012, AP-013, AP-016, AP-020
''' </summary>
Public Class frmPaymentRun
    Inherits ListForm

    Private ReadOnly _svc As New PaymentService()
    Private _txtProposal As TextBox
    Private _showingLines As Boolean

    Public Sub New()
        Me.Text = "Payment Run"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtProposal = FilterEdit("&Proposal:", 150)
        If FormArgument <> "" Then _txtProposal.Text = FormArgument
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Totals (F10)", "ENGINE", AddressOf TotalsClicked)
        AddTool("Lines", "FIND", AddressOf LinesClicked)
        AddSeparator()
        AddTool("Request approval", "RUN", AddressOf RequestClicked)
        AddTool("Approve", "POST", AddressOf ApproveClicked)
        AddTool("Reject", "REJECT", AddressOf RejectClicked)
        AddSeparator()
        AddTool("Create instructions", "RUN", AddressOf CreateClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        _showingLines = False
        Guard(Sub()
                  Populate(_svc.Proposals())
                  SetStatus(Grid.Rows.Count & " payment proposal(s).", "")
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        TotalsClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub LinesClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim pref As String = Proposal()
        If pref = "" Then Return
        Guard(Sub()
                  _showingLines = True
                  Populate(_svc.Lines(pref))
                  SetStatus("Lines of " & pref & ". An excluded line names its reason; press F5 for " &
                            "the proposal list.", "")
              End Sub)
    End Sub

    Private Sub TotalsClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim pref As String = Proposal()
        If pref = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Totals(pref)
                  If Not ShowResult(r) Then Return
                  SetStatus(pref & ": " & _svc.SelectedCount & " line(s) " & Fmt.Money(_svc.CashPrincipal) &
                            " less credits " & Fmt.Money(_svc.CreditsApplied) & " = " &
                            Fmt.Money(_svc.TotalOutflow) & " | " & _svc.ExcludedCount & " excluded | status " &
                            _svc.ProposalStatus, "OK")
              End Sub)
    End Sub

    Private Sub RequestClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim pref As String = Proposal()
        If pref = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.RequestApproval(pref, Amount())
                  If Not ShowResult(r) Then Return
                  SetStatus("Approval requested for " & pref & ". " & _svc.Note, "OK")
              End Sub)
    End Sub

    ''' <summary>Approving a run you prepared yourself is refused by the engine; the message is
    ''' shown as a proper error, never swallowed (AP-012, D-07).</summary>
    Private Sub ApproveClicked(ByVal sender As Object, ByVal e As EventArgs)
        Decide("APPROVE", "Approved from the payment run screen")
    End Sub

    Private Sub RejectClicked(ByVal sender As Object, ByVal e As EventArgs)
        Decide("REJECT", "Rejected from the payment run screen")
    End Sub

    Private Sub Decide(ByVal decision As String, ByVal comment As String)
        Dim pref As String = Proposal()
        If pref = "" Then Return
        If Not Confirm(decision & " payment proposal " & pref & "?") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Decide(pref, decision, comment, Amount())
                  If Not ShowResult(r) Then
                      SetStatus(decision & " refused: " & r.Display, "*** ERROR")
                      Return
                  End If
                  LoadData()
                  SetStatus(pref & " is now " & _svc.ProposalStatus & ". " & _svc.Note, "OK")
              End Sub)
    End Sub

    Private Sub CreateClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim pref As String = Proposal()
        If pref = "" Then Return
        If Not Confirm("Create the bank instructions for " & pref & "?") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.CreateInstructions(pref)
                  If Not ShowResult(r) Then
                      SetStatus("Payment run refused: " & r.Display, "*** ERROR")
                      Return
                  End If
                  SetStatus(_svc.InstructionCount & " instruction(s) created for " & pref &
                            ". Run the BACS report to produce the Standard 18 file.", "OK")
              End Sub)
    End Sub

    Private Function Amount() As Decimal
        If _showingLines Then Return 0D
        Return Fmt.Dec(GridHelper.Selected(Grid, "TotalOutflowGBP"))
    End Function

    Private Function Proposal() As String
        Dim pref As String = _txtProposal.Text.Trim().ToUpperInvariant()
        If pref = "" AndAlso Not _showingLines Then pref = Fmt.Str(GridHelper.Selected(Grid, "ProposalRef"))
        If pref = "" Then SetStatus("Select a proposal, or type its reference.", "!  WARNING")
        Return pref
    End Function

End Class
