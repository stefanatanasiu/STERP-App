Imports System.Collections.Generic
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Manual journal facade: account and dimension validation (GLVALID), preparation and
''' staging (JNLENTRY), posting and reversal (JNLPOST) and the posted journal enquiry.
''' The balance rule, the period decision and the approval rule belong to the COBOL
''' programs and to fin.usp_PostJournal - nothing is decided here.
''' REQ: FIN-002, FIN-003, FIN-004, FIN-005, FIN-006, FIN-023
''' </summary>
Public Class JournalService
    Inherits ServiceBase

    ''' <summary>Ties VALIDAT, SAVE and POST together so a repeat is not a second posting (COM-015).</summary>
    Public Property CorrelationId As String = ""
    Public Property TotalDebit As Decimal
    Public Property TotalCredit As Decimal
    Public Property OutOfBalance As Decimal
    Public Property PeriodStatus As String = ""
    Public Property ErrorCount As Integer
    Public Property LineResults As DataTable
    Public Property JournalNumber As String = ""
    Public Property WasDuplicate As Boolean
    Public Property AccountName As String = ""

    Public Sub StartNew()
        CorrelationId = Session.NewCorrelationId()
    End Sub

    ''' <summary>One line checked against the chart of accounts and the dimension rules (FIN-002, FIN-003).</summary>
    Public Function ValidateLine(ByVal account As String, ByVal costCentre As String, ByVal project As String,
                                 ByVal vatCode As String, ByVal drcr As String, ByVal amount As Decimal) As CobolResult
        Dim ca As CommArea = NewArea("GLVALIDCA")
        ca.SetString("GV-ACCOUNT", account)
        ca.SetDate("GV-POSTING-DATE", Session.BusinessDate)
        ca.SetString("GV-DIM-CC", costCentre)
        ca.SetString("GV-DIM-PROJ", project)
        ca.SetString("GV-VAT-CODE", vatCode)
        ca.SetString("GV-DR-CR", drcr)
        ca.SetString("GV-SOURCE-MODULE", "FIN")
        ca.SetString("GV-MANUAL-ENTRY", "Y")
        ca.SetDecimal("GV-AMOUNT", amount)
        Dim r As CobolResult = Link("GLVALID", ca, "LINE")
        AccountName = ca.GetString("GV-ACCOUNT-NAME")
        Return r
    End Function

    ''' <summary>VALIDAT, SAVE, SUBMIT, APPROVE, REJECT or VOID through JNLENTRY (FIN-004).</summary>
    Public Function Prepare(ByVal func As String, ByVal head As Dictionary(Of String, String),
                            ByVal lines As DataTable) As CobolResult
        If CorrelationId = "" Then StartNew()
        Dim ca As CommArea = NewArea("JNLENTRYCA")
        ca.SetString("JE-JOURNAL-REF", S(head, "REF"))
        ca.SetString("JE-JOURNAL-TYPE", Dflt(head, "TYPE", "GL"))
        ca.SetString("JE-DESCRIPTION", S(head, "DESC"))
        ca.SetString("JE-SOURCE-MODULE", "FIN")
        ca.SetString("JE-CURRENCY", Dflt(head, "CCY", Session.BaseCurrency))
        ca.SetString("JE-EVIDENCE-REF", S(head, "EVIDENCE"))
        ca.SetString("JE-PREPARED-BY", User)
        ca.SetDate("JE-DOCUMENT-DATE", AsDate(S(head, "DOCDATE")))
        ca.SetDate("JE-POSTING-DATE", AsDate(S(head, "POSTDATE")))
        Dim n As Integer = Math.Min(If(lines Is Nothing, 0, lines.Rows.Count), 50)
        ca.SetInt("JE-LINE-CNT", n)
        For i As Integer = 0 To n - 1
            Dim row As DataRow = lines.Rows(i)
            ca.SetRow("JE-LINES", i + 1, "JE-L-NUM", i + 1,
                      "JE-L-ACCOUNT", Fmt.Str(row("AccountCode")), "JE-L-DEBIT", Fmt.Dec(row("Debit")),
                      "JE-L-CREDIT", Fmt.Dec(row("Credit")), "JE-L-DIM-CC", Fmt.Str(row("CostCentre")),
                      "JE-L-DIM-PROJ", Fmt.Str(row("Project")), "JE-L-VAT-CODE", Fmt.Str(row("VatCode")),
                      "JE-L-NARRATIVE", Fmt.Str(row("Narrative")))
        Next
        Dim r As CobolResult = Link("JNLENTRY", ca, func, CorrelationId)
        TotalDebit = ca.GetDecimal("JE-TOTAL-DEBIT")
        TotalCredit = ca.GetDecimal("JE-TOTAL-CREDIT")
        OutOfBalance = ca.GetDecimal("JE-OUT-OF-BALANCE")
        PeriodStatus = ca.GetString("JE-PERIOD-STATUS")
        ErrorCount = ca.GetInt("JE-ERROR-CNT")
        If n > 0 Then LineResults = ca.GetTable("JE-LINES", n)
        Return r
    End Function

    ''' <summary>Posts the staged lines of the correlation id (FIN-005).</summary>
    Public Function Post(ByVal head As Dictionary(Of String, String)) As CobolResult
        If CorrelationId = "" Then StartNew()
        Dim ca As CommArea = NewArea("JNLPOSTCA")
        ca.SetString("JP-DESCRIPTION", S(head, "DESC"))
        ca.SetString("JP-SOURCE-REF", S(head, "REF"))
        ca.SetString("JP-JOURNAL-TYPE", Dflt(head, "TYPE", "GL"))
        ca.SetString("JP-CURRENCY", Dflt(head, "CCY", Session.BaseCurrency))
        ca.SetString("JP-EVIDENCE-REF", S(head, "EVIDENCE"))
        ca.SetDate("JP-POSTING-DATE", AsDate(S(head, "POSTDATE")))
        Return Answer(ca, Link("JNLPOST", ca, "POST", CorrelationId))
    End Function

    ''' <summary>A posted journal is never altered; a correction is a linked reversal (FIN-006).</summary>
    Public Function Reverse(ByVal journalNumber As String, ByVal pct As Decimal, ByVal reason As String) As CobolResult
        Dim ca As CommArea = NewArea("JNLPOSTCA")
        ca.SetString("JP-JOURNAL-NUMBER", journalNumber)
        ca.SetDecimal("JP-REVERSE-PCT", pct)
        ca.SetString("JP-REASON", reason)
        Return Answer(ca, Link("JNLPOST", ca, "REVERSE", Session.NewCorrelationId()))
    End Function

    Private Function Answer(ByVal ca As CommArea, ByVal r As CobolResult) As CobolResult
        JournalNumber = ca.GetString("JP-NEW-JOURNAL-NO")
        TotalDebit = ca.GetDecimal("JP-TOTAL-DR-GBP")
        TotalCredit = ca.GetDecimal("JP-TOTAL-CR-GBP")
        WasDuplicate = (ca.GetString("JP-WAS-DUPLICATE") = "Y")
        Return r
    End Function

    ''' <summary>Posted headers with their approval, evidence and reversal links (FIN-023).</summary>
    Public Function Enquiry(ByVal journalNumber As String, ByVal status As String, ByVal account As String) As DataTable
        Return Query("fin.usp_JournalEnquiry", "@CompanyCode", Company, "@JournalNumber", journalNumber,
                     "@Status", status, "@AccountCode", account)
    End Function

    ''' <summary>The lines behind one posted journal - the drill-down of the enquiry (FIN-023).</summary>
    Public Function Lines(ByVal journalId As Object) As DataTable
        Return Query("SELECT LineNum, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DebitAmountGBP, " &
                     "CreditAmountGBP, DimCostCentre, DimProject, VatCode, SubledgerRef, Narrative " &
                     "FROM fin.JournalLine WHERE JournalId = @j ORDER BY LineNum", "@j", journalId)
    End Function

    ''' <summary>An empty line table in the shape the entry screen and Prepare() exchange.</summary>
    Public Shared Function NewLineTable() As DataTable
        Dim t As New DataTable("Lines")
        For Each name As String In New String() {"AccountCode", "CostCentre", "Project", "VatCode", "Narrative"}
            t.Columns.Add(name, GetType(String))
        Next
        t.Columns.Add("Debit", GetType(Decimal))
        t.Columns.Add("Credit", GetType(Decimal))
        Return t
    End Function

    Private Shared Function S(ByVal v As Dictionary(Of String, String), ByVal key As String) As String
        If v Is Nothing OrElse Not v.ContainsKey(key) OrElse v(key) Is Nothing Then Return ""
        Return v(key).Trim()
    End Function

    Private Shared Function Dflt(ByVal v As Dictionary(Of String, String), ByVal key As String, ByVal spare As String) As String
        Return If(S(v, key) = "", spare, S(v, key))
    End Function

    Private Shared Function AsDate(ByVal text As String) As Date
        Dim d As Date? = Fmt.ParseDate(text)
        Return If(d.HasValue, d.Value, Session.BusinessDate)
    End Function

End Class
