Imports STERP.Business.Costing
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Job cost enquiry and work in progress.  The worklist shows budget, actual, committed and
''' variance with sub-jobs under their parent and counted once (D-13); JOBCOST answers the
''' element breakdown and the estimate at completion, and WIPCALC proves the roll-forward
''' opening + additions - deductions = closing without committing anything.
''' Opened from the Work in Progress menu entry (FormArgument "WIP") the roll-forward is the
''' default action.
''' REQ: CST-014, CST-016, CST-017, CST-018, CST-020
''' </summary>
Public Class frmJobCost
    Inherits ListForm

    Private ReadOnly _svc As New CostingService()
    Private _txtJob As TextBox
    Private _cboStatus As ComboBox
    Private _cboSubs As ComboBox

    Public Sub New()
        Me.Text = "Job Cost Enquiry"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtJob = FilterEdit("&Job:", 140)
        _cboStatus = FilterList("&Status:", 130, "ALL", "OPEN", "SUSPENDED", "COMPLETE", "CLOSED")
        _cboSubs = FilterList("S&ub-jobs:", 130, "INCLUDED", "THIS JOB ONLY")
        AddHandler _cboStatus.SelectedIndexChanged, Sub(s, e) LoadData()
        If Wip() Then Me.Text = "Work in Progress"
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Elements", "LIST", AddressOf ElementsClicked)
        AddTool("Forecast", "ENGINE", AddressOf ForecastClicked)
        AddTool("Work in progress", "RUN", AddressOf WipClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If _txtJob Is Nothing Then Return
        Guard(Sub()
                  Dim t = _svc.Jobs(_txtJob.Text.Trim().ToUpperInvariant(), Status())
                  Populate(t)
                  SetStatus(t.Rows.Count & " job(s).  Sub-jobs are shown under their parent and counted once." &
                            If(Wip(), "  Select a job and press F10 for the roll-forward.", ""), "OK")
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        If Wip() Then WipClicked(Nothing, EventArgs.Empty) Else ElementsClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>Budget against actual by element, with the estimate at completion (CST-016).</summary>
    Private Sub ElementsClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Chosen() = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.JobEnquire(Chosen(), Subs())
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If Not ShowResult(r) Then Return
                  SetStatus(Chosen() & " " & _svc.Text("JC-OPER-STATUS") & "/" & _svc.Text("JC-FIN-STATUS") &
                            " | budget " & Fmt.Money(_svc.Num("JC-APPROVED-BUDGET")) &
                            " | actual " & Fmt.Money(_svc.Num("JC-ACTUAL-GBP")) &
                            " | committed " & Fmt.Money(_svc.Num("JC-COMMITTED-GBP")) &
                            " | WIP " & Fmt.Money(_svc.Num("JC-WIP-GBP")) &
                            " | " & _svc.Num("JC-SUBJOB-COUNT") & " sub-job(s), " &
                            _svc.Num("JC-TXN-COUNT") & " transaction(s)" &
                            Note(), Cue())
              End Sub)
    End Sub

    ''' <summary>Estimate at completion - budget, actual and commitment, not a straight line (CST-017).</summary>
    Private Sub ForecastClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Chosen() = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Forecast(Chosen(), Subs())
                  If Not ShowResult(r) Then Return
                  SetStatus(Chosen() & " forecast: budget " & Fmt.Money(_svc.Num("JC-APPROVED-BUDGET")) &
                            " | actual " & Fmt.Money(_svc.Num("JC-ACTUAL-GBP")) &
                            " | committed " & Fmt.Money(_svc.Num("JC-COMMITTED-GBP")) &
                            " | still to come " & Fmt.Money(_svc.Num("JC-ETC-GBP")) &
                            " | estimate at completion " & Fmt.Money(_svc.Num("JC-EAC-GBP")) &
                            " | variance " & Fmt.Money(_svc.Num("JC-FCST-VAR-GBP")) &
                            If(_svc.Text("JC-FCST-ADVERSE") = "Y", " ADVERSE", " favourable") & Note(),
                            If(_svc.Text("JC-FCST-ADVERSE") = "Y", "!  WARNING", "OK"))
              End Sub)
    End Sub

    ''' <summary>Opening + additions - deductions = closing, proved before anything posts (CST-018).</summary>
    Private Sub WipClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Chosen() = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Wip(Chosen(), Subs())
                  If Not ShowResult(r) Then Return
                  SetStatus(Chosen() & " WIP " & Fmt.Period(SterpSession.Current.FiscalYear, SterpSession.Current.PeriodNo) &
                            ": opening " & Fmt.Money(_svc.Num("WC-OPENING-GBP")) &
                            " + additions " & Fmt.Money(_svc.Num("WC-ADDITIONS-GBP")) &
                            " - deductions " & Fmt.Money(_svc.Num("WC-DEDUCTIONS-GBP")) &
                            " = closing " & Fmt.Money(_svc.Num("WC-CLOSING-GBP")) &
                            If(_svc.Text("WC-BALANCED") = "Y", " (balanced)", " (DOES NOT BALANCE)") &
                            If(_svc.Text("WC-HAS-RESIDUAL") = "Y",
                               " | residual " & Fmt.Money(_svc.Num("WC-RESIDUAL-GBP")) & " must be cleared before closure", "") &
                            If(_svc.Text("WC-WARNING") <> "", " | " & _svc.Text("WC-WARNING"), ""),
                            If(_svc.Text("WC-BALANCED") = "Y" AndAlso _svc.Text("WC-HAS-RESIDUAL") <> "Y",
                               "OK", "!  WARNING"))
              End Sub)
    End Sub

    Private Function Note() As String
        Dim w As String = _svc.Text("JC-WARNING")
        Dim d As String = _svc.Text("JC-DISPOSITION")
        Return If(d <> "", " | " & d, "") & If(w <> "", " | " & w, "")
    End Function

    Private Function Cue() As String
        Return If(_svc.Text("JC-WARNING") = "", "OK", "!  WARNING")
    End Function

    Private Function Chosen() As String
        Dim j As String = Fmt.Str(GridHelper.Selected(Grid, "Job"))
        If j = "" Then j = _txtJob.Text.Trim().ToUpperInvariant()
        If j = "" Then SetStatus("Select a job first.", "!  WARNING")
        Return j
    End Function

    Private Function Subs() As Boolean
        Return _cboSubs.SelectedIndex <> 1
    End Function

    Private Function Status() As String
        Dim s As String = Fmt.Str(_cboStatus.SelectedItem)
        Return If(s = "ALL", "", s)
    End Function

    Private Function Wip() As Boolean
        Return String.Equals(FormArgument, "WIP", StringComparison.OrdinalIgnoreCase)
    End Function

End Class
