Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' VAT facade: rates and determination (VATDET), the nine box return with its box
''' drill-down (VATRET9, fin.usp_VatBoxes, fin.usp_VatBoxDrill) and error correction
''' (VATERRC).  Nothing is typed into a box - every figure is an aggregation - and the
''' local filing hand-off is written to a file because STERP has no HMRC connection.
''' REQ: FIN-016, FIN-017, FIN-018, FIN-025, FIN-026, FIN-027, FIN-029
''' </summary>
Public Class VatService
    Inherits ServiceBase

    Private ReadOnly _box(8) As Decimal

    Public Property VatRegNo As String = ""
    Public Property ReturnStatus As String = ""
    Public Property StartDate As String = ""
    Public Property EndDate As String = ""
    Public Property FilingDue As String = ""
    Public Property RecordCount As Integer
    Public Property LateCount As Integer
    Public Property IsRepayment As Boolean
    Public Property Drilled As DataTable
    Public Property Answer As String = ""

    ''' <summary>Box 1 to box 9 as VATRET9 computed them.</summary>
    Public Function BoxValue(ByVal boxNo As Integer) As Decimal
        If boxNo < 1 OrElse boxNo > 9 Then Return 0D
        Return _box(boxNo - 1)
    End Function

    ''' <summary>The nine boxes of a VAT period (FIN-018, FIN-025, FIN-026).</summary>
    Public Function Prepare(ByVal periodKey As String, ByVal basis As String) As CobolResult
        Dim ca As CommArea = Area(periodKey, basis)
        Dim r As CobolResult = Link("VATRET9", ca, "PREPARE")
        ClearReturn()
        If r.Succeeded Then ReadArea(ca)
        Return r
    End Function

    Private Sub ReadArea(ByVal ca As CommArea, Optional ByVal readBoxes As Boolean = True)
        If readBoxes Then
            For i As Integer = 1 To 9
                _box(i - 1) = ca.GetDecimal("VR-BOX" & i)
            Next
        End If
        VatRegNo = ca.GetString("VR-VAT-REG-NO")
        ReturnStatus = ca.GetString("VR-RETURN-STATUS")
        StartDate = Fmt.Date(ca.GetDate("VR-START-DATE"))
        EndDate = Fmt.Date(ca.GetDate("VR-END-DATE"))
        FilingDue = Fmt.Date(ca.GetDate("VR-FILING-DUE"))
        RecordCount = ca.GetInt("VR-RECORD-CNT")
        LateCount = ca.GetInt("VR-LATE-CNT")
        IsRepayment = (ca.GetString("VR-IS-REPAYMENT") = "Y")
    End Sub

    Private Sub ClearReturn()
        Array.Clear(_box, 0, _box.Length)
        VatRegNo = "" : ReturnStatus = "" : StartDate = "" : EndDate = "" : FilingDue = ""
        RecordCount = 0 : LateCount = 0 : IsRepayment = False
    End Sub

    ''' <summary>Fresh, fail-closed authority; never use the shell's permission cache for an export.</summary>
    Public Function CanAct(ByVal permissionCode As String) As Boolean
        Dim row As DataRow = QueryRow("core.usp_CheckPermission", "@UserCode", User,
            "@PermissionCode", permissionCode, "@CompanyCode", Company, "@SiteCode", Site)
        Return row IsNot Nothing AndAlso Fmt.Bool(Col(row, "Allowed", False))
    End Function

    ''' <summary>Review the latest persisted return without originating or recomputing it.</summary>
    Public Sub LoadReturn(ByVal periodKey As String)
        ClearReturn()
        If Not (CanAct("FIN.VAT.PREPARE") OrElse CanAct("FIN.VAT.APPROVE") OrElse CanAct("FIN.VAT.SUBMIT")) Then
            Throw New UnauthorizedAccessException("VAT return review is not permitted for this company/site.")
        End If
        Dim row As DataRow = QueryRow(
            "SELECT TOP (1) r.*, p.StartDate, p.EndDate, p.FilingDueDate, " &
            "(SELECT COUNT(DISTINCT d.VatTxnId) FROM fin.VatReturnDetail d WHERE d.VatReturnId=r.VatReturnId) AS Records, " &
            "(SELECT COUNT(DISTINCT d.VatTxnId) FROM fin.VatReturnDetail d WHERE d.VatReturnId=r.VatReturnId AND d.IsLateEntry=1) AS Late " &
            "FROM fin.VatReturn r JOIN fin.VatPeriod p ON p.CompanyCode=r.CompanyCode AND p.VatPeriodKey=r.VatPeriodKey " &
            "WHERE r.CompanyCode=@c AND r.VatPeriodKey=@p ORDER BY r.Version DESC",
            "@c", Company, "@p", periodKey)
        If row Is Nothing Then Throw New InvalidOperationException("No prepared VAT return exists for " & Company & "/" & periodKey & ".")
        For i As Integer = 1 To 9
            _box(i - 1) = Fmt.Dec(row("Box" & i))
        Next
        VatRegNo = Fmt.Str(row("VatRegNo"))
        ReturnStatus = Fmt.Str(row("Status"))
        StartDate = Fmt.Date(Convert.ToDateTime(row("StartDate")))
        EndDate = Fmt.Date(Convert.ToDateTime(row("EndDate")))
        FilingDue = Fmt.Date(Convert.ToDateTime(row("FilingDueDate")))
        RecordCount = Convert.ToInt32(row("Records"))
        LateCount = Convert.ToInt32(row("Late"))
        IsRepayment = BoxValue(5) < 0D
    End Sub

    ''' <summary>The nine boxes straight from the aggregation, for the box arithmetic (FIN-025, FIN-026).</summary>
    Public Function Boxes(ByVal periodKey As String, ByVal basis As String) As DataRow
        Return QueryRow("fin.usp_VatBoxes", "@CompanyCode", Company, "@VatPeriodKey", periodKey, "@Basis", basis)
    End Function

    ''' <summary>The source records behind one box (FIN-018, FIN-026).</summary>
    Public Function Drill(ByVal periodKey As String, ByVal boxNo As Integer) As CobolResult
        Dim ca As CommArea = Area(periodKey, "")
        ca.SetInt("VR-BOX-NO", boxNo)
        Dim r As CobolResult = Link("VATRET9", ca, "DRILL")
        Dim n As Integer = ca.GetInt("VR-DRILL-CNT")
        Drilled = If(n > 0, ca.GetTable("VR-DRILL", n), Nothing)
        If Drilled Is Nothing AndAlso Deployed("fin.usp_VatBoxDrill") Then
            Drilled = Query("fin.usp_VatBoxDrill", "@CompanyCode", Company, "@VatPeriodKey", periodKey, "@BoxNo", boxNo)
        End If
        Return r
    End Function

    ''' <summary>FINALISE freezes the figures and the detail; SUBMIT records the receipt (FIN-018).</summary>
    Public Function Record(ByVal func As String, ByVal periodKey As String, ByVal submissionRef As String,
                           ByVal response As String) As CobolResult
        Dim ca As CommArea = Area(periodKey, "")
        ca.SetString("VR-SUBMISSION-REF", submissionRef)
        ca.SetString("VR-HMRC-RESPONSE", response)
        Dim result As CobolResult = Link("VATRET9", ca, func, Session.NewCorrelationId())
        If result.Succeeded Then
            ReadArea(ca, func <> "SUBMIT")
            If func = "FINALISE" Then LoadReturn(periodKey)
        End If
        Return result
    End Function

    ''' <summary>
    ''' Writes only a local hand-off file, then records that fact through VATRET9.
    ''' No HMRC request or receipt is involved. Permission and frozen state are checked
    ''' before touching the filesystem; a rejected native record removes the file.
    ''' </summary>
    Public Function WriteSubmissionFile(ByVal periodKey As String) As String
        If Not CanAct("FIN.VAT.SUBMIT") Then
            Throw New UnauthorizedAccessException("FIN.VAT.SUBMIT is required to write a local VAT file.")
        End If
        LoadReturn(periodKey)
        If ReturnStatus <> "FINALISED" Then
            Throw New InvalidOperationException("Only a finalised VAT return may be written to a local file.")
        End If
        If String.IsNullOrWhiteSpace(VatRegNo) Then Throw New InvalidOperationException("The frozen return has no VAT registration number.")
        If Company.IndexOfAny(System.IO.Path.GetInvalidFileNameChars()) >= 0 OrElse
           periodKey.IndexOfAny(System.IO.Path.GetInvalidFileNameChars()) >= 0 Then
            Throw New ArgumentException("Invalid company or period key for a VAT filename.")
        End If
        Dim dir As String = System.IO.Path.Combine(SterpConfig.RunDir, "work\vat")
        Dim path As String = System.IO.Path.Combine(dir, "VAT-" & Company & "-" & periodKey & "-" &
                                                    Guid.NewGuid().ToString("N").Substring(0, 12) & ".MTD")
        Dim sb As New Text.StringBuilder()
        sb.AppendLine(SterpConfig.ProductName & " - LOCAL VAT HAND-OFF (NOT TRANSMITTED)")
        sb.AppendLine("COMPANY   " & Company)
        sb.AppendLine("VRN       " & VatRegNo)
        sb.AppendLine("PERIODKEY " & periodKey)
        sb.AppendLine("PERIOD    " & StartDate & " TO " & EndDate)
        sb.AppendLine("EXPORTED  " & User & " " & Fmt.Stamp(Date.Now))
        For i As Integer = 1 To 9
            sb.AppendLine("BOX" & i & "      " & _box(i - 1).ToString("0.00", Fmt.UK).PadLeft(18))
        Next
        sb.AppendLine("RECORDS   " & RecordCount & "  LATE " & LateCount)
        System.IO.Directory.CreateDirectory(dir)
        Try
            System.IO.File.WriteAllText(path, sb.ToString())
            Dim result As CobolResult = Record("SUBMIT", periodKey, System.IO.Path.GetFileName(path),
                                              "LOCAL FILE ONLY; NOT TRANSMITTED")
            If Not result.Succeeded Then Throw New InvalidOperationException(result.Display)
        Catch
            If System.IO.File.Exists(path) Then System.IO.File.Delete(path)
            Throw
        End Try
        LoadReturn(periodKey)
        SterpLog.Info("Local VAT hand-off file written (not transmitted): " & path)
        Return path
    End Function

    ''' <summary>The effective rate and treatment of a VAT code at a tax point (FIN-016, FIN-017).</summary>
    Public Function Rate(ByVal vatCode As String, ByVal taxPoint As Date) As CobolResult
        Dim ca As CommArea = NewArea("VATDETCA")
        ca.SetString("VD-VAT-CODE", vatCode)
        ca.SetDate("VD-TAX-POINT", taxPoint)
        ca.SetString("VD-DIRECTION", "OUTPUT")
        Dim r As CobolResult = Link("VATDET", ca, "RATE")
        Answer = Fmt.Pct(ca.GetDecimal("VD-RATE-PCT")) & " " & ca.GetString("VD-LIABILITY") &
                 " " & ca.GetString("VD-MECHANISM")
        Return r
    End Function

    ''' <summary>Decides whether a net error may be corrected on the next return (FIN-027).</summary>
    Public Function AssessError(ByVal originalKey As String, ByVal correctionKey As String,
                                ByVal netError As Decimal, ByVal deliberate As Boolean) As CobolResult
        Dim ca As CommArea = NewArea("VATERRCCA")
        ca.SetString("VE-ORIG-PERIOD-KEY", originalKey)
        ca.SetString("VE-CORR-PERIOD-KEY", correctionKey)
        ca.SetDate("VE-DISCOVERED-DATE", Session.BusinessDate)
        ca.SetDecimal("VE-NET-ERROR", netError)
        ca.SetDecimal("VE-BOX6-TURNOVER", BoxValue(6))
        ca.SetString("VE-IS-DELIBERATE", If(deliberate, "Y", "N"))
        Dim r As CobolResult = Link("VATERRC", ca, "ASSESS")
        Answer = ca.GetString("VE-METHOD") & " - " & ca.GetString("VE-EXPLANATION")
        Return r
    End Function

    ''' <summary>The VAT return calendar for the period picker.</summary>
    Public Function VatPeriods() As DataTable
        Return Query("SELECT VatPeriodKey, StartDate, EndDate, FilingDueDate, PaymentDueDate, Frequency, Status, " &
                     "SubmissionRef FROM fin.VatPeriod WHERE CompanyCode = @c ORDER BY StartDate DESC", "@c", Company)
    End Function

    Private Function Area(ByVal periodKey As String, ByVal basis As String) As CommArea
        Dim ca As CommArea = NewArea("VATRET9CA")
        ca.SetString("VR-VAT-PERIOD-KEY", periodKey)
        ca.SetString("VR-BASIS", basis)
        Return ca
    End Function

End Class
