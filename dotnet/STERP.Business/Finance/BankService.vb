Imports System.Collections.Generic
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Bank reconciliation facade: the statement-to-book bridge, grouped matching and the
''' reversal of a match group.  BANKREC owns the rule that unequal totals are only
''' reconciled with a written explanation; fin.usp_BankOutstanding reports the position.
''' REQ: FIN-013, FIN-014, FIN-015
''' </summary>
Public Class BankService
    Inherits ServiceBase

    Public Property BookBalance As Decimal
    Public Property StatementBalance As Decimal
    Public Property OutstandingReceipts As Decimal
    Public Property OutstandingPayments As Decimal
    Public Property BridgedBalance As Decimal
    Public Property Unexplained As Decimal
    Public Property UnmatchedCount As Integer
    Public Property StatementTotal As Decimal
    Public Property BookTotal As Decimal
    Public Property Difference As Decimal
    Public Property MatchType As String = ""
    Public Property MatchedCount As Integer

    ''' <summary>Book balance, outstanding items and the statement position (FIN-013).</summary>
    Public Function Bridge(ByVal bankAccount As String, ByVal asAt As Date) As CobolResult
        Dim ca As CommArea = Area(bankAccount, asAt)
        Dim r As CobolResult = Link("BANKREC", ca, "BRIDGE")
        BookBalance = ca.GetDecimal("BR-BOOK-BALANCE")
        StatementBalance = ca.GetDecimal("BR-STMT-BALANCE")
        OutstandingReceipts = ca.GetDecimal("BR-OUT-RECEIPTS")
        OutstandingPayments = ca.GetDecimal("BR-OUT-PAYMENTS")
        BridgedBalance = ca.GetDecimal("BR-BRIDGED-BALANCE")
        Unexplained = ca.GetDecimal("BR-UNEXPLAINED")
        UnmatchedCount = ca.GetInt("BR-UNMATCHED-CNT")
        Return r
    End Function

    ''' <summary>The same bridge straight from the database, for the enquiry and the tests (FIN-013).</summary>
    Public Function Outstanding(ByVal bankAccount As String, ByVal asAt As Date) As DataRow
        Return QueryRow("fin.usp_BankOutstanding", "@CompanyCode", Company,
                        "@BankAccountCode", bankAccount, "@AsOfDate", asAt)
    End Function

    ''' <summary>
    ''' Ties n statement entries to m book entries.  Each entry is {line id, amount};
    ''' an out-of-balance group needs the written explanation (FIN-014).
    ''' </summary>
    Public Function Match(ByVal bankAccount As String, ByVal statement As List(Of Decimal()),
                          ByVal book As List(Of Decimal()), ByVal narrative As String) As CobolResult
        Dim ca As CommArea = Area(bankAccount, Session.BusinessDate)
        ca.SetString("BR-NARRATIVE", narrative)
        ca.SetInt("BR-STMT-CNT", Math.Min(statement.Count, 20))
        For i As Integer = 0 To Math.Min(statement.Count, 20) - 1
            ca.SetRow("BR-STMT", i + 1, "BR-ST-LINE-ID", statement(i)(0), "BR-ST-AMOUNT", statement(i)(1))
        Next
        ca.SetInt("BR-BOOK-CNT", Math.Min(book.Count, 20))
        For i As Integer = 0 To Math.Min(book.Count, 20) - 1
            ca.SetRow("BR-BOOK", i + 1, "BR-BK-LINE-ID", book(i)(0), "BR-BK-AMOUNT", book(i)(1))
        Next
        Dim r As CobolResult = Link("BANKREC", ca, "MATCH", Session.NewCorrelationId())
        StatementTotal = ca.GetDecimal("BR-STMT-TOTAL")
        BookTotal = ca.GetDecimal("BR-BOOK-TOTAL")
        Difference = ca.GetDecimal("BR-DIFFERENCE")
        MatchType = ca.GetString("BR-MATCH-TYPE")
        MatchedCount = ca.GetInt("BR-MATCHED-CNT")
        Return r
    End Function

    ''' <summary>Reverses a match group; the statement evidence stays on file (FIN-014).</summary>
    Public Function Unmatch(ByVal bankAccount As String, ByVal matchId As Object, ByVal reason As String) As CobolResult
        Dim ca As CommArea = Area(bankAccount, Session.BusinessDate)
        ca.SetDecimal("BR-MATCH-ID", Fmt.Dec(matchId))
        ca.SetString("BR-REASON", reason)
        Return Link("BANKREC", ca, "UNMATCH", Session.NewCorrelationId())
    End Function

    ''' <summary>Statement entries loaded for the account, unmatched ones first (FIN-013).</summary>
    Public Function StatementLines(ByVal bankAccount As String, ByVal status As String) As DataTable
        Return Query("SELECT TOP (300) StatementLineId, EntryDate, ValueDate, StatementRef, LineNum, Description, " &
                     "BankReference, EntryType, Amount, CurrencyCode, MatchStatus, MatchedAmount, MatchedBy " &
                     "FROM fin.BankStatementLine WHERE CompanyCode = @c AND BankAccountCode = @b " &
                     "AND (@s = 'ALL' OR MatchStatus = @s) ORDER BY EntryDate, LineNum",
                     "@c", Company, "@b", bankAccount, "@s", status)
    End Function

    ''' <summary>The cash book side: posted journal lines that touch the bank account (FIN-015).</summary>
    Public Function BookLines(ByVal bankAccount As String, ByVal asAt As Date, ByVal outstandingOnly As Boolean) As DataTable
        Return Query("SELECT TOP (300) JournalLineId, PostingDate, JournalNumber, SourceModule, SourceRef, Narrative, " &
                     "MovementType, SignedGBP, CurrencyCode, ReconMatchId, ReconState FROM fin.vw_CashBook " &
                     "WHERE CompanyCode = @c AND BankAccountCode = @b AND PostingDate <= @d " &
                     "AND (@o = 0 OR ReconMatchId IS NULL) ORDER BY PostingDate, JournalLineId",
                     "@c", Company, "@b", bankAccount, "@d", asAt, "@o", If(outstandingOnly, 1, 0))
    End Function

    ''' <summary>Bank accounts of the company for the picker.</summary>
    Public Function Accounts() As DataTable
        Return Query("SELECT BankAccountCode, Name, CurrencyCode, AccountCode, StatementBalance, StatementDate, " &
                     "LastReconciledDate, Status FROM fin.BankAccount WHERE CompanyCode = @c ORDER BY BankAccountCode",
                     "@c", Company)
    End Function

    Private Function Area(ByVal bankAccount As String, ByVal asAt As Date) As CommArea
        Dim ca As CommArea = NewArea("BANKRECCA")
        ca.SetString("BR-BANK-ACCOUNT", bankAccount)
        ca.SetDate("BR-AS-AT-DATE", asAt)
        ca.SetString("BR-CURRENCY", Session.BaseCurrency)
        Return ca
    End Function

End Class
