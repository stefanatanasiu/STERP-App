Imports System.Data
Imports STERP.CobolInterop

Namespace Costing

    ''' <summary>
    ''' COMMAREA plumbing shared by the costing screens.  A "spec" names the display column,
    ''' its type and the COBOL field in one string - "Item=CR-L-ITEM,Cost:D=CR-L-EXT-COST" -
    ''' so the grid and the copy out of the OCCURS window are driven from the same text.
    ''' REQ: CST-010, CST-012, CST-016
    ''' </summary>
    Public Module CostArea

        ''' <summary>Builds the display table. ":I" integer, ":D" decimal, ":T" date, else text.</summary>
        Public Function NewTable(ByVal spec As String) As DataTable
            Dim t As New DataTable()
            For Each entry As String In spec.Split(","c)
                Dim parts() As String = entry.Split("="c)(0).Split(":"c)
                Dim ty As Type = GetType(String)
                If parts.Length > 1 Then
                    Select Case parts(1)
                        Case "I" : ty = GetType(Integer)
                        Case "D" : ty = GetType(Decimal)
                        Case "T" : ty = GetType(Date)
                    End Select
                End If
                t.Columns.Add(parts(0), ty)
            Next
            Return t
        End Function

        ''' <summary>Copies occurrences <paramref name="first"/>..<paramref name="last"/> into the table.</summary>
        Public Sub Copy(ByVal ca As CommArea, ByVal rows As DataTable, ByVal spec As String,
                        ByVal first As Integer, ByVal last As Integer)
            Dim entries() As String = spec.Split(","c)
            For i As Integer = first To last
                Dim d As DataRow = rows.NewRow()
                For Each entry As String In entries
                    Dim nv() As String = entry.Split("="c)
                    Dim col As String = nv(0).Split(":"c)(0)
                    If ca.Has(nv(1)) Then
                        Dim f As String = nv(1) & "(" & i & ")"
                        Dim ty As Type = rows.Columns(col).DataType
                        If ty Is GetType(Integer) Then
                            d(col) = ca.GetInt(f)
                        ElseIf ty Is GetType(Decimal) Then
                            d(col) = ca.GetDecimal(f)
                        ElseIf ty Is GetType(Date) Then
                            Dim dt As Date? = ca.GetDate(f)
                            If dt.HasValue Then d(col) = dt.Value
                        Else
                            d(col) = ca.GetString(f)
                        End If
                    End If
                Next
                rows.Rows.Add(d)
            Next
        End Sub

        ''' <summary>A blank filter means "all", which the stored procedures express as NULL.</summary>
        Public Function Opt(ByVal value As String) As Object
            If value Is Nothing OrElse value.Trim().Length = 0 Then Return DBNull.Value
            Return value.Trim()
        End Function

    End Module

    ''' <summary>
    ''' Facade over the costing engines.  STDCOST owns the cost set lifecycle, COSTROLL the
    ''' roll-up, WHATIF the priced scenarios and JOBCOST / WIPCALC the job and its work in
    ''' progress.  This class fills the COMMAREA, calls the gateway and shapes the answer for a
    ''' screen: the separation of preparer and approver, the valuation boundary, the
    ''' completeness scoring and the estimate at completion all stay in COBOL.
    ''' REQ: CST-007, CST-010, CST-012, CST-014, CST-016, CST-017, CST-018, CST-023, CST-024
    ''' </summary>
    Public Class CostingService
        Inherits ServiceBase

        Private _ca As CommArea
        Private _lines As DataTable

        ''' <summary>Cost set comparison window of STDCOSTCA (CST-012).</summary>
        Public Const CompareSpec As String =
            "Item=SC-L-ITEM,Revision=SC-L-REV,OldCost:D=SC-L-OLD-COST,NewCost:D=SC-L-NEW-COST," &
            "Difference:D=SC-L-DIFF,DiffPct:D=SC-L-PCT,QtyHeld:D=SC-L-QTY-HELD," &
            "Revaluation:D=SC-L-REVAL,Note=SC-L-NOTE"

        ''' <summary>Costed tree window of COSTROLLCA (CST-010, CST-024).</summary>
        Public Const TreeSpec As String =
            "Seq:I=CR-L-SEQ,Depth:I=CR-L-DEPTH,Item=CR-L-ITEM,Revision=CR-L-REV," &
            "Description=CR-L-DESC,Element=CR-L-ELEMENT,Source=CR-L-SOURCE,Boundary=CR-L-BOUNDARY," &
            "Required:D=CR-L-REQ-QTY,UnitCost:D=CR-L-UNIT-COST,Extended:D=CR-L-EXT-COST," &
            "Contributes=CR-L-CONTRIB,Warning=CR-L-WARNING"

        ''' <summary>Scenario window of WHATIFCA - nothing here is ever written (CST-023).</summary>
        Public Const ScenarioSpec As String =
            "Scenario=WI-V-LABEL,CostSet=WI-V-COST-SET,LotSize:D=WI-V-LOT-SIZE," &
            "Material:D=WI-V-MATERIAL,Conversion:D=WI-V-CONVERSION,Overhead:D=WI-V-OVERHEAD," &
            "Total:D=WI-V-TOTAL,UnitCost:D=WI-V-UNIT,Difference:D=WI-V-DIFF,DiffPct:D=WI-V-DIFF-PCT," &
            "MarginPct:D=WI-V-MARGIN-PCT,Completeness=WI-V-COMPLETE,Warning=WI-V-WARNING"

        ''' <summary>Job element window of JOBCOSTCA - budget against actual (CST-016).</summary>
        Public Const JobSpec As String =
            "Key=JC-L-KEY,Description=JC-L-DESC,Element=JC-L-ELEMENT,Budget:D=JC-L-BUDGET," &
            "Actual:D=JC-L-ACTUAL,Committed:D=JC-L-COMMITTED,Etc:D=JC-L-ETC,Eac:D=JC-L-EAC," &
            "Variance:D=JC-L-VARIANCE,Adverse=JC-L-ADVERSE"

        ''' <summary>The window the last engine call filled, already shaped for a grid.</summary>
        Public ReadOnly Property Lines() As DataTable
            Get
                Return _lines
            End Get
        End Property

        ''' <summary>A field of the last COMMAREA as text ("" when the program is not installed).</summary>
        Public Function Text(ByVal field As String) As String
            If _ca Is Nothing OrElse Not _ca.Has(field) Then Return ""
            Return _ca.GetString(field)
        End Function

        ''' <summary>A field of the last COMMAREA as a number.</summary>
        Public Function Num(ByVal field As String) As Decimal
            If _ca Is Nothing OrElse Not _ca.Has(field) Then Return 0D
            Return _ca.GetDecimal(field)
        End Function

        ' ------------------------------------------------------------- enquiries
        ''' <summary>Every basis we hold for one item, side by side (CST-001, CST-026).</summary>
        Public Function CostEnquiry(ByVal item As String, ByVal revision As String,
                                    ByVal site As String) As DataTable
            If Not Deployed("cst.usp_CostEnquiry") Then Return New DataTable()
            Return Query("cst.usp_CostEnquiry", "@CompanyCode", Company, "@ItemCode", item,
                         "@Revision", revision, "@SiteCode", Opt(site))
        End Function

        ''' <summary>The cost set worklist with preparer and approver side by side (CST-007).</summary>
        Public Function Sets(ByVal status As String) As DataTable
            If Not Deployed("cst.usp_CostSetList") Then Return New DataTable()
            Return Query("cst.usp_CostSetList", "@CompanyCode", Company, "@Status", Opt(status))
        End Function

        ''' <summary>The job worklist, sub-jobs under their parent and counted once (D-13).</summary>
        Public Function Jobs(ByVal jobRef As String, ByVal status As String) As DataTable
            If Not Deployed("cst.usp_JobList") Then Return New DataTable()
            Return Query("cst.usp_JobList", "@CompanyCode", Company, "@JobRef", Opt(jobRef),
                         "@Status", Opt(status))
        End Function

        ''' <summary>Budget, actual, WIP and realised margin per job (CST-016, RPT-014).</summary>
        Public Function Profitability(ByVal jobRef As String) As DataTable
            If Not Deployed("cst.usp_JobProfitability") Then Return New DataTable()
            Return Query("cst.usp_JobProfitability", "@CompanyCode", Company,
                         "@JobRef", Opt(jobRef), "@IncludeSubs", 1)
        End Function

        ' --------------------------------------------------------------- STDCOST
        Private Function SetArea(ByVal costSet As String, ByVal compareTo As String) As CommArea
            Dim ca As CommArea = NewArea("STDCOSTCA")
            ca.SetString("SC-COST-SET", costSet)
            ca.SetString("SC-COMPARE-SET", compareTo)
            ca.SetDate("SC-EFF-DATE", Session.BusinessDate)
            ca.SetString("SC-COMMIT", "N")
            _ca = ca
            Return ca
        End Function

        ''' <summary>Moves a draft set to review and scores it against the approved set (CST-012).</summary>
        Public Function Review(ByVal costSet As String) As CobolResult
            Dim ca As CommArea = SetArea(costSet, "")
            ca.SetString("SC-COMMIT", "Y")
            Return Link("STDCOST", ca, "REVIEW", Session.NewCorrelationId())
        End Function

        ''' <summary>
        ''' Approves a reviewed set.  The engine refuses CST0405 when the approver prepared the
        ''' set; the screen shows that refusal as an error rather than quietly doing nothing.
        ''' </summary>
        Public Function Approve(ByVal costSet As String) As CobolResult
            Dim ca As CommArea = SetArea(costSet, "")
            ca.SetString("SC-COMMIT", "Y")
            Return Link("STDCOST", ca, "APPROVE", Session.NewCorrelationId())
        End Function

        ''' <summary>Activation revalues owned stock and raises the revaluation variance (CST-007).</summary>
        Public Function Activate(ByVal costSet As String) As CobolResult
            Dim ca As CommArea = SetArea(costSet, "")
            ca.SetString("SC-COMMIT", "Y")
            Return Link("STDCOST", ca, "ACTIVATE", Session.NewCorrelationId())
        End Function

        ''' <summary>What activation would do.  Changes nothing (CST-012).</summary>
        Public Function Impact(ByVal costSet As String) As CobolResult
            Return Link("STDCOST", SetArea(costSet, ""), "IMPACT")
        End Function

        ''' <summary>Line by line comparison of two sets, items in only one of them kept (CST-012).</summary>
        Public Function Compare(ByVal costSet As String, ByVal compareTo As String) As CobolResult
            Dim ca As CommArea = SetArea(costSet, compareTo)
            ca.SetInt("SC-FROM-LINE", 0)
            _lines = NewTable(CompareSpec)
            Dim r As CobolResult = Link("STDCOST", ca, "COMPARE")
            If r.Succeeded Then Copy(ca, _lines, CompareSpec, 1, ca.GetInt("SC-LINE-CNT"))
            Return r
        End Function

        ' -------------------------------------------------------------- COSTROLL
        ''' <summary>
        ''' Rolls the item up for the stated good output.  COSTROLL calls BOMCOST for the
        ''' explosion - costing never explodes a bill itself - and the costed tree is paged
        ''' back through GETLINE (CST-010, CST-024).
        ''' </summary>
        Public Function Rollup(ByVal item As String, ByVal revision As String, ByVal costSet As String,
                               ByVal lotSize As Decimal, ByVal expand As Boolean) As CobolResult
            Dim ca As CommArea = NewArea("COSTROLLCA")
            ca.SetString("CR-ITEM-CODE", item)
            ca.SetString("CR-REVISION", revision)
            ca.SetString("CR-COST-SET", costSet)
            ca.SetString("CR-TARGET-SET", "")
            ca.SetDate("CR-EFF-DATE", Session.BusinessDate)
            ca.SetDate("CR-COST-DATE", Session.BusinessDate)
            ca.SetDecimal("CR-LOT-SIZE", lotSize)
            ca.SetString("CR-CURRENCY", Session.BaseCurrency)
            ca.SetString("CR-BOUNDARY", If(expand, "EXPAND", "STOP"))
            ca.SetInt("CR-MAX-DEPTH", 10)
            ca.SetInt("CR-FROM-LINE", 0)
            ca.SetInt("CR-WIN-MAX", 20)
            _ca = ca
            _lines = NewTable(TreeSpec)
            Dim r As CobolResult = Link("COSTROLL", ca, "ROLLUP")
            If Not r.Succeeded Then Return r

            ' CR-FROM-LINE is the 1-based line to read next (WHATIF and the unit tests pass 1)
            Dim from As Integer = 1
            For page As Integer = 1 To 25
                Dim n As Integer = ca.GetInt("CR-LINE-CNT")
                Copy(ca, _lines, TreeSpec, 1, n)
                from += n
                If n = 0 OrElse ca.GetString("CR-MORE-LINES") <> "Y" Then Exit For
                ca.SetInt("CR-FROM-LINE", from)
                If Not Link("COSTROLL", ca, "GETLINE").Succeeded Then Exit For
            Next
            Return r
        End Function

        ' ---------------------------------------------------------------- WHATIF
        ''' <summary>
        ''' A base case and three variants priced through COSTROLL with the target set cleared.
        ''' SAVESET is never issued, so nothing is written, reserved or activated (CST-023).
        ''' </summary>
        Public Function Scenario(ByVal item As String, ByVal revision As String, ByVal costSet As String,
                                 ByVal lotSize As Decimal, ByVal targetPrice As Decimal,
                                 ByVal material As Decimal, ByVal rate As Decimal) As CobolResult
            Dim ca As CommArea = NewArea("WHATIFCA")
            ca.SetString("WI-ITEM-CODE", item)
            ca.SetString("WI-REVISION", revision)
            ca.SetString("WI-SCENARIO-REF", "SCR-" & Session.BusinessDate.ToString("yyyyMMdd"))
            ca.SetDate("WI-EFF-DATE", Session.BusinessDate)
            ca.SetString("WI-BASE-COST-SET", costSet)
            ca.SetDecimal("WI-BASE-LOT-SIZE", lotSize)
            ca.SetDecimal("WI-TARGET-PRICE", targetPrice)
            ca.SetString("WI-SIDE-EFFECTS", "N")
            ca.SetInt("WI-VARIANT-CNT", 3)
            SetVariant(ca, 1, "AS ENTERED", costSet, lotSize, material, rate, "STOP")
            SetVariant(ca, 2, "MATERIAL -5%", costSet, lotSize, material * 0.95D, rate, "STOP")
            SetVariant(ca, 3, "LOT X2 RATE +10%", costSet, lotSize * 2D, material, rate * 1.1D, "EXPAND")
            _ca = ca
            _lines = NewTable(ScenarioSpec)

            Dim func As String = If(targetPrice > 0D, "MARGINWI", "SCENARIO")
            Dim r As CobolResult = Link("WHATIF", ca, func)
            If r.Succeeded Then Copy(ca, _lines, ScenarioSpec, 1, ca.GetInt("WI-VARIANT-CNT"))
            Return r
        End Function

        Private Shared Sub SetVariant(ByVal ca As CommArea, ByVal i As Integer, ByVal label As String,
                                      ByVal costSet As String, ByVal lotSize As Decimal,
                                      ByVal material As Decimal, ByVal rate As Decimal, ByVal boundary As String)
            ca.SetString("WI-V-LABEL(" & i & ")", label)
            ca.SetString("WI-V-COST-SET(" & i & ")", costSet)
            ca.SetDecimal("WI-V-LOT-SIZE(" & i & ")", lotSize)
            ca.SetDecimal("WI-V-MAT-FACTOR(" & i & ")", material)
            ca.SetDecimal("WI-V-RATE-FACTR(" & i & ")", rate)
            ca.SetString("WI-V-BOUNDARY(" & i & ")", boundary)
        End Sub

        ' --------------------------------------------------------- JOBCOST/WIPCALC
        ''' <summary>Job header and its elements, sub-jobs counted once (CST-014, CST-016).</summary>
        Public Function JobEnquire(ByVal jobRef As String, ByVal includeSubs As Boolean) As CobolResult
            Dim ca As CommArea = NewArea("JOBCOSTCA")
            ca.SetString("JC-JOB-REF", jobRef)
            ca.SetString("JC-INCLUDE-SUBJOBS", If(includeSubs, "Y", "N"))
            ca.SetString("JC-COMMIT", "N")
            ca.SetInt("JC-FROM-LINE", 0)
            _ca = ca
            _lines = NewTable(JobSpec)
            Dim r As CobolResult = Link("JOBCOST", ca, "ENQUIRE")
            If r.Succeeded Then Copy(ca, _lines, JobSpec, 1, ca.GetInt("JC-LINE-CNT"))
            Return r
        End Function

        ''' <summary>Estimate at completion from budget, actual and commitment (CST-017).</summary>
        Public Function Forecast(ByVal jobRef As String, ByVal includeSubs As Boolean) As CobolResult
            Dim ca As CommArea = NewArea("JOBCOSTCA")
            ca.SetString("JC-JOB-REF", jobRef)
            ca.SetString("JC-INCLUDE-SUBJOBS", If(includeSubs, "Y", "N"))
            ca.SetString("JC-COMMIT", "N")
            _ca = ca
            Return Link("JOBCOST", ca, "FORECAST")
        End Function

        ''' <summary>
        ''' The work in progress roll-forward for the period.  CALC never commits, so the screen
        ''' can show opening, additions, deductions and closing before anybody posts (CST-018).
        ''' </summary>
        Public Function Wip(ByVal jobRef As String, ByVal includeSubs As Boolean) As CobolResult
            Dim ca As CommArea = NewArea("WIPCALCCA")
            ca.SetString("WC-JOB-REF", jobRef)
            ca.SetInt("WC-FISCAL-YEAR", Session.FiscalYear)
            ca.SetInt("WC-PERIOD-NO", Session.PeriodNo)
            ca.SetString("WC-INCLUDE-SUBJOBS", If(includeSubs, "Y", "N"))
            ca.SetString("WC-COMMIT", "N")
            _ca = ca
            Return Link("WIPCALC", ca, "CALC")
        End Function

    End Class

End Namespace
