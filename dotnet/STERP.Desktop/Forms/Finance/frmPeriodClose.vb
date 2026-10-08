Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Period close checklist: the standard steps for a company, fiscal year and period, with
''' soft close, hard close and reopen.  PERCLOSE decides readiness, holds the materiality
''' limit and refuses a hard close while a blocking step is open; the screen only reports it.
''' REQ: FIN-008, FIN-010, FIN-011, FIN-012
''' </summary>
Public Class frmPeriodClose
    Inherits ListForm

    Private ReadOnly _svc As New PeriodService()
    Private _txtYear As TextBox
    Private _txtPeriod As TextBox

    Public Sub New()
        Me.Text = "Period Close Checklist"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtYear = FilterEdit("Fiscal &year:", 70)
        _txtPeriod = FilterEdit("&Period:", 50)
        _txtYear.Text = Convert.ToString(SterpSession.Current.FiscalYear)
        _txtPeriod.Text = Convert.ToString(SterpSession.Current.PeriodNo)
        If FormArgument <> "" Then _txtPeriod.Text = FormArgument
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Complete step (F10)", "SAVE", AddressOf StepClicked)
        AddTool("Soft close", "RETIRE", AddressOf SoftClicked)
        AddTool("Hard close", "POST", AddressOf HardClicked)
        AddTool("Reopen", "RUN", AddressOf ReopenClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub()
                  Dim r As CobolResult = _svc.Status(Fy(), Pn())
                  Dim steps As DataTable = _svc.Steps
                  If steps Is Nothing Then steps = _svc.Checklist(Fy(), Pn())
                  Populate(steps)
                  If r.IsNotFound AndAlso steps Is Nothing Then
                      SetStatus("No checklist is held for " & Fmt.Period(Fy(), Pn()) & ".", "!  WARNING")
                      Return
                  End If
                  Report()
              End Sub)
    End Sub

    Private Sub Report()
        SetStatus(Fmt.Period(Fy(), Pn()) & " " & _svc.PeriodName & " is " & _svc.PeriodStatus &
                  ".  " & _svc.DoneCount & " of " & _svc.StepCount & " step(s) done, " & _svc.BlockingOpen &
                  " blocking, unresolved difference " & Fmt.Money(_svc.UnresolvedDifference) &
                  " against a materiality limit of " & Fmt.Money(_svc.Materiality) & ".  Readiness " &
                  _svc.Readiness & ".", If(_svc.Readiness = "READY", "OK", "!  WARNING"))
    End Sub

    Protected Overrides Sub OnConfirm()
        StepClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>Each completed step must reference its evidence (FIN-011).</summary>
    Private Sub StepClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = Fmt.Str(GridHelper.Selected(Grid, "StepCode"))
        If code = "" Then code = Fmt.Str(GridHelper.Selected(Grid, "CL-S-CODE"))
        If code = "" Then
            SetStatus("Select a checklist step first.", "!  WARNING")
            Return
        End If
        Dim evidence As String = InputBox("Evidence reference for step " & code & " (a reconciliation, " &
                                          "a report or a sign-off):", "STERP - Close Step", "")
        If evidence Is Nothing OrElse evidence.Trim() = "" Then Return
        Run(Function() _svc.CompleteStep(Fy(), Pn(), code, "DONE", evidence.Trim(), 0D))
    End Sub

    Private Sub SoftClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Confirm("Soft close " & Fmt.Period(Fy(), Pn()) &
                       "?  Only authorised close adjustments may be posted into it afterwards.") Then Return
        Run(Function() _svc.Close("SOFT", Fy(), Pn(), "", "", False))
    End Sub

    Private Sub HardClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Confirm("Hard close " & Fmt.Period(Fy(), Pn()) & "?  Nothing may be posted into it afterwards.") Then Return
        Run(Function() _svc.Close("HARD", Fy(), Pn(), "", "", False))
    End Sub

    ''' <summary>Reopening a closed period needs a recorded reason and a named approver (FIN-012).</summary>
    Private Sub ReopenClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim reason As String = InputBox("Why is " & Fmt.Period(Fy(), Pn()) & " being reopened?",
                                        "STERP - Reopen Period", "")
        If reason Is Nothing OrElse reason.Trim() = "" Then Return
        Dim approver As String = InputBox("Which financial controller approved the reopening?",
                                          "STERP - Reopen Period", "")
        If approver Is Nothing OrElse approver.Trim() = "" Then Return
        Run(Function() _svc.Close("REOPEN", Fy(), Pn(), reason.Trim(), approver.Trim().ToUpperInvariant(), False))
    End Sub

    Private Sub Run(ByVal work As Func(Of CobolResult))
        Guard(Sub()
                  Dim r As CobolResult = work()
                  If _svc.Steps IsNot Nothing Then Populate(_svc.Steps)
                  If Not ShowResult(r) Then Return
                  Report()
              End Sub)
    End Sub

    Private Function Fy() As Integer
        Return Fmt.Int(_txtYear.Text)
    End Function

    Private Function Pn() As Integer
        Return Fmt.Int(_txtPeriod.Text)
    End Function

End Class
