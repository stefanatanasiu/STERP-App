Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Payment proposal, approval and payment run facade (module ap).  APPAYPRP selects and
''' reserves, APPAYAPR holds the two pairs of eyes and APPAYRUN turns an approved proposal
''' into bank instructions - none of those decisions are taken here.
''' REQ: AP-011, AP-012, AP-013, AP-016, AP-020
''' </summary>
Public Class PaymentService
    Inherits ServiceBase

    Public Property TotalOutflow As Decimal
    Public Property CashPrincipal As Decimal
    Public Property CreditsApplied As Decimal
    Public Property SelectedCount As Integer
    Public Property ExcludedCount As Integer
    Public Property ProposalStatus As String = ""
    Public Property ContentHash As String = ""
    Public Property InstructionCount As Integer
    Public Property Note As String = ""
    Public Property ProposalReference As String = ""

    ''' <summary>Every proposal for the company, newest first.</summary>
    Public Function Proposals() As DataTable
        Return Query("ap.usp_ListProposals", "@CompanyCode", Company, "@ProposalRef", "")
    End Function

    ''' <summary>The lines of one proposal with the instruction each produced.</summary>
    Public Function Lines(ByVal proposalRef As String) As DataTable
        Return Query("ap.usp_ListProposals", "@CompanyCode", Company, "@ProposalRef", proposalRef)
    End Function

    ''' <summary>APPAYPRP selects the due items, applies credits and reports what it excluded
    ''' and why; a blocked or disputed item can never be selected (AP-011).</summary>
    Public Function Select_(ByVal proposalRef As String, ByVal cutoff As Date, ByVal payDate As Date,
                            ByVal method As String, ByVal bankAccount As String) As CobolResult
        Dim ca As CommArea = NewArea("APPAYPRPCA")
        ca.SetString("PP-PROPOSAL-REF", proposalRef)
        ca.SetString("PP-CUTOFF-DATE", Fmt.Yyyymmdd(cutoff))
        ca.SetString("PP-PAY-DATE", Fmt.Yyyymmdd(payDate))
        ca.SetString("PP-METHOD", method)
        ca.SetString("PP-BANK-ACCOUNT", bankAccount)
        ca.SetString("PP-MODE", "DRAFT")
        Dim r As CobolResult = Link("APPAYPRP", ca, "SELECT", Session.NewCorrelationId())
        ReadProposal(ca)
        Return r
    End Function

    ''' <summary>Re-totals a proposal without changing it (AP-011).</summary>
    Public Function Totals(ByVal proposalRef As String) As CobolResult
        Dim ca As CommArea = NewArea("APPAYPRPCA")
        ca.SetString("PP-PROPOSAL-REF", proposalRef)
        Dim row As DataRow = QueryRow(
            "SELECT p.ProposalId, p.Status, p.CashPrincipalGBP, p.CreditsAppliedGBP, p.TotalOutflowGBP, " &
            "p.ExcludedCount, p.ContentHash, SelectedCount = (SELECT COUNT(*) FROM ap.PaymentProposalLine l " &
            "WHERE l.ProposalId = p.ProposalId AND l.IsExcluded = 0) FROM ap.PaymentProposal p " &
            "WHERE p.CompanyCode = @c AND p.ProposalRef = @r",
            "@c", Company, "@r", proposalRef)
        If row Is Nothing Then
            TotalOutflow = 0D
            CashPrincipal = 0D
            CreditsApplied = 0D
            SelectedCount = 0
            ExcludedCount = 0
            ProposalStatus = ""
            ContentHash = ""
            Note = "The payment proposal is not held for this company."
            Return New CobolResult With {
                .Program = "APPAYPRP", .ReturnCode = 8, .ReasonCode = "APP0000", .Message = Note}
        End If
        ca.SetInt("PP-PROPOSAL-ID", Fmt.Int(row("ProposalId")))
        ca.SetString("PP-STATUS", Fmt.Str(row("Status")))
        ca.SetString("PP-CONTENT-HASH", Fmt.Str(row("ContentHash")))
        ca.SetDecimal("PP-CASH-PRINCIPAL", Fmt.Dec(row("CashPrincipalGBP")))
        ca.SetDecimal("PP-CREDITS-APPLIED", Fmt.Dec(row("CreditsAppliedGBP")))
        ca.SetDecimal("PP-TOTAL-OUTFLOW", Fmt.Dec(row("TotalOutflowGBP")))
        ca.SetInt("PP-SELECTED-CNT", Fmt.Int(row("SelectedCount")))
        ca.SetInt("PP-EXCLUDED-CNT", Fmt.Int(row("ExcludedCount")))
        Dim r As CobolResult = Link("APPAYPRP", ca, "TOTALS")
        ReadProposal(ca)
        Return r
    End Function

    ''' <summary>Asks for the approval of a proposal, which fixes the content hash: any later
    ''' change to the selection invalidates the approval (AP-012, D-07).</summary>
    Public Function RequestApproval(ByVal proposalRef As String, ByVal amount As Decimal) As CobolResult
        Dim ca As CommArea = NewArea("APPAYAPRCA")
        ca.SetString("PA-PROPOSAL-REF", proposalRef)
        ca.SetString("PA-DOCUMENT-TYPE", "PAYPROPOSAL")
        ca.SetString("PA-DOCUMENT-REF", proposalRef)
        ca.SetDecimal("PA-AMOUNT-GBP", amount)
        ca.SetString("PA-REQUESTED-BY", User)
        Dim r As CobolResult = Link("APPAYAPR", ca, "REQUEST", Session.NewCorrelationId())
        ReadApproval(ca)
        Return r
    End Function

    ''' <summary>Records an approval decision.  The approver is always the signed on user, so
    ''' the preparer of the run cannot approve it: the engine refuses and the screen shows the
    ''' refusal as an error (AP-012, D-07).</summary>
    Public Function Decide(ByVal proposalRef As String, ByVal decision As String, ByVal comment As String,
                           ByVal amount As Decimal) As CobolResult
        Dim ca As CommArea = NewArea("APPAYAPRCA")
        Dim row As DataRow = QueryRow(
            "SELECT ProposalId, ApprovalId FROM ap.PaymentProposal WHERE CompanyCode = @c AND ProposalRef = @r",
            "@c", Company, "@r", proposalRef)
        If row IsNot Nothing Then
            ca.SetInt("PA-PROPOSAL-ID", Fmt.Int(row("ProposalId")))
            ca.SetInt("PA-APPROVAL-ID", Fmt.Int(Col(row, "ApprovalId", 0)))
        End If
        ca.SetString("PA-PROPOSAL-REF", proposalRef)
        ca.SetString("PA-DOCUMENT-TYPE", "PAYPROPOSAL")
        ca.SetString("PA-DOCUMENT-REF", proposalRef)
        ca.SetDecimal("PA-AMOUNT-GBP", amount)
        ca.SetString("PA-APPROVER", User)
        ca.SetString("PA-DECISION", If(decision = "APPROVE", "APPROVED",
                                     If(decision = "REJECT", "REJECTED", decision)))
        ca.SetString("PA-COMMENT", comment)
        Dim r As CobolResult = Link("APPAYAPR", ca, "DECIDE", Session.NewCorrelationId())
        ReadApproval(ca)
        Return r
    End Function

    ''' <summary>Reserve the existing draft selection through APPAYPRP.</summary>
    Public Function Reserve(ByVal proposalRef As String) As CobolResult
        Return Reservation(proposalRef, "RESERVE")
    End Function

    ''' <summary>Release reservations, not payment authority (APPAYPRP RELEASE).</summary>
    Public Function Release(ByVal proposalRef As String) As CobolResult
        Return Reservation(proposalRef, "RELEASE")
    End Function

    Private Function Reservation(ByVal proposalRef As String, ByVal action As String) As CobolResult
        Dim row As DataRow = QueryRow(
            "SELECT ProposalId, SelectionCutoffDate, PayDate, PaymentMethod, BankAccountCode, CurrencyCode " &
            "FROM ap.PaymentProposal WHERE CompanyCode = @c AND ProposalRef = @r",
            "@c", Company, "@r", proposalRef)
        If row Is Nothing Then
            Return New CobolResult With {.Program = "APPAYPRP", .ReturnCode = 8,
                .ReasonCode = "APP0000", .Message = "The payment proposal is not held for this company."}
        End If
        ' APPAYPRP creates a header when the ID is zero; carry the stored selection
        ' settings so reserving/releasing cannot create a second proposal or change dates.
        Dim ca As CommArea = NewArea("APPAYPRPCA")
        ca.SetString("PP-PROPOSAL-REF", proposalRef)
        ca.SetInt("PP-PROPOSAL-ID", Fmt.Int(row("ProposalId")))
        ca.SetString("PP-CUTOFF-DATE", Fmt.Yyyymmdd(CDate(row("SelectionCutoffDate"))))
        ca.SetString("PP-PAY-DATE", Fmt.Yyyymmdd(CDate(row("PayDate"))))
        ca.SetString("PP-METHOD", Fmt.Str(row("PaymentMethod")))
        ca.SetString("PP-BANK-ACCOUNT", Fmt.Str(row("BankAccountCode")))
        ca.SetString("PP-CURRENCY", Fmt.Str(row("CurrencyCode")))
        Dim r As CobolResult = Link("APPAYPRP", ca, action, Session.NewCorrelationId())
        ReadProposal(ca)
        Return r
    End Function

    ''' <summary>APPAYRUN turns the released proposal into payment instructions ready for the
    ''' BACS Standard 18 file (AP-013, AP-016).</summary>
    Public Function CreateInstructions(ByVal proposalRef As String) As CobolResult
        Dim ca As CommArea = NewArea("APPAYRUNCA")
        ca.SetString("PR-PROPOSAL-REF", proposalRef)
        ca.SetInt("PR-PROPOSAL-ID", Fmt.Int(Scalar(
            "SELECT ProposalId FROM ap.PaymentProposal WHERE CompanyCode = @c AND ProposalRef = @r",
            "@c", Company, "@r", proposalRef)))
        Dim r As CobolResult = Link("APPAYRUN", ca, "CREATE", Session.NewCorrelationId())
        InstructionCount = ca.GetInt("PR-CREATED-CNT")
        Note = ca.GetString("PR-NOTE")
        Return r
    End Function

    Private Sub ReadProposal(ByVal ca As CommArea)
        ProposalReference = ca.GetString("PP-PROPOSAL-REF")
        TotalOutflow = ca.GetDecimal("PP-TOTAL-OUTFLOW")
        CashPrincipal = ca.GetDecimal("PP-CASH-PRINCIPAL")
        CreditsApplied = ca.GetDecimal("PP-CREDITS-APPLIED")
        SelectedCount = ca.GetInt("PP-SELECTED-CNT")
        ExcludedCount = ca.GetInt("PP-EXCLUDED-CNT")
        ProposalStatus = ca.GetString("PP-STATUS")
        ContentHash = ca.GetString("PP-CONTENT-HASH")
        Note = ca.GetString("PP-NOTE")
    End Sub

    Private Sub ReadApproval(ByVal ca As CommArea)
        ProposalStatus = ca.GetString("PA-STATUS")
        ContentHash = ca.GetString("PA-CONTENT-HASH")
        Note = ca.GetString("PA-NOTE")
    End Sub

End Class
