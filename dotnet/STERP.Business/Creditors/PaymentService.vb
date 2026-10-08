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
        ca.SetString("PA-PROPOSAL-REF", proposalRef)
        ca.SetString("PA-DOCUMENT-TYPE", "PAYPROPOSAL")
        ca.SetString("PA-DOCUMENT-REF", proposalRef)
        ca.SetDecimal("PA-AMOUNT-GBP", amount)
        ca.SetString("PA-APPROVER", User)
        ca.SetString("PA-DECISION", decision)
        ca.SetString("PA-COMMENT", comment)
        Dim r As CobolResult = Link("APPAYAPR", ca, "DECIDE", Session.NewCorrelationId())
        ReadApproval(ca)
        Return r
    End Function

    ''' <summary>Releases an approved proposal for payment (AP-013).</summary>
    Public Function Release(ByVal proposalRef As String) As CobolResult
        Dim ca As CommArea = NewArea("APPAYPRPCA")
        ca.SetString("PP-PROPOSAL-REF", proposalRef)
        Dim r As CobolResult = Link("APPAYPRP", ca, "RELEASE", Session.NewCorrelationId())
        ReadProposal(ca)
        Return r
    End Function

    ''' <summary>APPAYRUN turns the released proposal into payment instructions ready for the
    ''' BACS Standard 18 file (AP-013, AP-016).</summary>
    Public Function CreateInstructions(ByVal proposalRef As String) As CobolResult
        Dim ca As CommArea = NewArea("APPAYRUNCA")
        ca.SetString("PR-PROPOSAL-REF", proposalRef)
        Dim r As CobolResult = Link("APPAYRUN", ca, "CREATE", Session.NewCorrelationId())
        InstructionCount = ca.GetInt("PR-CREATED-CNT")
        Note = ca.GetString("PR-NOTE")
        Return r
    End Function

    Private Sub ReadProposal(ByVal ca As CommArea)
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
