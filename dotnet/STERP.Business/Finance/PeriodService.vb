Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Period control and the close checklist: PERCLOSE owns the readiness test, the
''' materiality limit and the sign-off evidence; fin.usp_CloseStatus reports them and
''' fin.usp_ValidatePeriod answers whether a date may be posted into at all.
''' REQ: FIN-008, FIN-010, FIN-011, FIN-012
''' </summary>
Public Class PeriodService
    Inherits ServiceBase

    Public Property PeriodName As String = ""
    Public Property PeriodStatus As String = ""
    Public Property Readiness As String = ""
    Public Property StepCount As Integer
    Public Property DoneCount As Integer
    Public Property BlockingOpen As Integer
    Public Property UnresolvedDifference As Decimal
    Public Property Materiality As Decimal
    Public Property Steps As DataTable

    ''' <summary>The period state and its checklist, read from PERCLOSE (FIN-010, FIN-011).</summary>
    Public Function Status(ByVal fiscalYear As Integer, ByVal periodNo As Integer) As CobolResult
        Dim ca As CommArea = Area(fiscalYear, periodNo)
        Return Finish(ca, Link("PERCLOSE", ca, "STATUS"))
    End Function

    ''' <summary>Completes or accepts one checklist step with its evidence (FIN-011).</summary>
    Public Function CompleteStep(ByVal fiscalYear As Integer, ByVal periodNo As Integer, ByVal stepCode As String,
                                 ByVal action As String, ByVal evidence As String, ByVal difference As Decimal) As CobolResult
        Dim ca As CommArea = Area(fiscalYear, periodNo)
        ca.SetString("CL-STEP-CODE", stepCode)
        ca.SetString("CL-STEP-ACTION", action)
        ca.SetString("CL-EVIDENCE-REF", evidence)
        ca.SetDecimal("CL-DIFFERENCE", difference)
        Return Finish(ca, Link("PERCLOSE", ca, "STEP", Session.NewCorrelationId()))
    End Function

    ''' <summary>SOFT, HARD or REOPEN - PERCLOSE refuses a hard close that is not ready (FIN-011, FIN-012).</summary>
    Public Function Close(ByVal func As String, ByVal fiscalYear As Integer, ByVal periodNo As Integer,
                          ByVal reason As String, ByVal reviewer As String, ByVal force As Boolean) As CobolResult
        Dim ca As CommArea = Area(fiscalYear, periodNo)
        ca.SetString("CL-REASON", reason)
        ca.SetString("CL-REVIEWER", reviewer)
        ca.SetString("CL-FORCE", If(force, "Y", "N"))
        Return Finish(ca, Link("PERCLOSE", ca, func, Session.NewCorrelationId()))
    End Function

    Private Function Area(ByVal fiscalYear As Integer, ByVal periodNo As Integer) As CommArea
        Dim ca As CommArea = NewArea("PERCLOSECA")
        ca.SetInt("CL-FISCAL-YEAR", fiscalYear)
        ca.SetInt("CL-PERIOD-NO", periodNo)
        Return ca
    End Function

    Private Function Finish(ByVal ca As CommArea, ByVal r As CobolResult) As CobolResult
        PeriodName = ca.GetString("CL-PERIOD-NAME")
        PeriodStatus = ca.GetString("CL-PERIOD-STATUS")
        Readiness = ca.GetString("CL-READINESS")
        StepCount = ca.GetInt("CL-STEP-CNT")
        DoneCount = ca.GetInt("CL-DONE-CNT")
        BlockingOpen = ca.GetInt("CL-BLOCKING-OPEN")
        UnresolvedDifference = ca.GetDecimal("CL-UNRESOLVED-DIFF")
        Materiality = ca.GetDecimal("CL-MATERIALITY")
        Dim listed As Integer = ca.GetInt("CL-STEPS-LISTED")
        Steps = If(listed > 0, ca.GetTable("CL-STEPS", listed), Nothing)
        Return r
    End Function

    ''' <summary>The same checklist from the database, used when the engine holds no PERCLOSE.</summary>
    Public Function Checklist(ByVal fiscalYear As Integer, ByVal periodNo As Integer) As DataTable
        Dim ds As DataSet = QuerySet("fin.usp_CloseStatus", "@CompanyCode", Company,
                                     "@FiscalYear", fiscalYear, "@PeriodNo", periodNo)
        If ds Is Nothing OrElse ds.Tables.Count < 2 Then Return Nothing
        Return ds.Tables(1)
    End Function

    ''' <summary>The company calendar, newest period first (FIN-008).</summary>
    Public Function Periods(ByVal fiscalYear As Integer) As DataTable
        Return Query("SELECT FiscalYear, PeriodNo, Name, StartDate, EndDate, Status, SoftCloseAt, HardCloseAt, " &
                     "ClosedBy, ReopenedBy, ReopenReason FROM fin.Period WHERE CompanyCode = @c " &
                     "AND (@y = 0 OR FiscalYear = @y) ORDER BY FiscalYear DESC, PeriodNo DESC",
                     "@c", Company, "@y", fiscalYear)
    End Function

    ''' <summary>May this date be posted into?  OPEN posts, SOFT needs authority, HARD blocks (FIN-010).</summary>
    Public Function MayPost(ByVal postingDate As Date) As String
        Try
            QueryRow("fin.usp_ValidatePeriod", "@CompanyCode", Company, "@PostingDate", postingDate,
                     "@UserCode", User, "@Action", "POST")
            Return ""
        Catch ex As STERP.Data.BusinessRuleException
            Return ex.Message & "  (" & ex.Code & ")"
        End Try
    End Function

End Class
