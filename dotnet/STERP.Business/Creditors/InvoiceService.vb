Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Invoice register and three way matching facade (module ap).  APINVREG owns registration,
''' the duplicate test and the state machine; APMATCH owns the tolerance arithmetic.
''' REQ: AP-003, AP-004, AP-006, AP-007, AP-008, AP-023
''' </summary>
Public Class InvoiceService
    Inherits ServiceBase

    Public Property RegisterNumber As String = ""
    Public Property DuplicateFound As Boolean
    Public Property DuplicateRegister As String = ""
    Public Property SimilarCount As Integer
    Public Property LinesMatched As Integer
    Public Property LinesVariance As Integer
    Public Property LinesUnmatched As Integer
    Public Property MatchState As String = ""
    Public Property Note As String = ""

    ''' <summary>The register with its four states and the days overdue.</summary>
    Public Function Register(ByVal filter As String, ByVal mode As String) As DataTable
        Return Query("ap.usp_ListInvoiceRegister", "@CompanyCode", Company, "@SupplierCode", filter, "@Mode", mode)
    End Function

    ''' <summary>Open items for one supplier, already aged.</summary>
    Public Function OpenItems(ByVal supplier As String, ByVal mode As String) As DataTable
        Return Query("ap.usp_ListApOpenItems", "@CompanyCode", Company, "@SupplierCode", supplier, "@Mode", mode)
    End Function

    ''' <summary>APINVREG decides whether this document has been seen before.  An exact repeat is
    ''' refused; a near miss is reported for the clerk to judge (AP-004).</summary>
    Public Function DuplicateCheck(ByVal supplier As String, ByVal docRef As String, ByVal gross As Decimal,
                                   ByVal docDate As Date, Optional ByVal documentType As String = "INVOICE") As CobolResult
        Dim ca As CommArea = NewArea("APINVREGCA")
        ca.SetString("IR-SUPPLIER", supplier)
        ca.SetString("IR-DOC-TYPE", documentType)
        ca.SetString("IR-SUPPLIER-REF", docRef)
        ca.SetDecimal("IR-STATED-GROSS", gross)
        ca.SetString("IR-DOCUMENT-DATE", Fmt.Yyyymmdd(docDate))
        Dim r As CobolResult = Link("APINVREG", ca, "DUPCHK")
        DuplicateFound = (ca.GetString("IR-DUP-FOUND") = "Y")
        DuplicateRegister = ca.GetString("IR-DUP-REGISTER")
        SimilarCount = ca.GetInt("IR-SIMILAR-CNT")
        Note = r.Message
        Return r
    End Function

    ''' <summary>Reads one registered document back out of the engine (AP-023).</summary>
    Public Function Get_(ByVal registerNumber As String) As CobolResult
        Dim ca As CommArea = NewArea("APINVREGCA")
        ca.SetString("IR-REGISTER-NUMBER", registerNumber)
        Dim r As CobolResult = Link("APINVREG", ca, "GET")
        RegisterNumber = ca.GetString("IR-REGISTER-NUMBER")
        MatchState = ca.GetString("IR-MATCH-STATE")
        Note = ca.GetString("IR-BLOCK-REASON")
        Return r
    End Function

    ''' <summary>APMATCH matches every line of the invoice against its order and receipt and
    ''' reports how many are clean, within tolerance or still unmatched (AP-006, AP-007).</summary>
    Public Function MatchAll(ByVal registerNumber As String) As CobolResult
        Dim ca As CommArea = NewArea("APMATCHCA")
        ca.SetString("MT-REGISTER-NUMBER", registerNumber)
        Dim r As CobolResult = Link("APMATCH", ca, "MATCHALL", Session.NewCorrelationId())
        LinesMatched = ca.GetInt("MT-LINES-MATCHED")
        LinesVariance = ca.GetInt("MT-LINES-VARIANCE")
        LinesUnmatched = ca.GetInt("MT-LINES-UNMATCHED")
        MatchState = ca.GetString("MT-INVOICE-STATE")
        Note = ca.GetString("MT-NOTE")
        Return r
    End Function

    ''' <summary>The tolerance test on its own, with no database work: the screen can show the
    ''' clerk why a line would be held before anything is written (AP-006).</summary>
    Public Function ToleranceCheck(ByVal invoiceQty As Decimal, ByVal invoicePrice As Decimal,
                                   ByVal orderQty As Decimal, ByVal orderPrice As Decimal,
                                   ByVal receivedQty As Decimal) As CobolResult
        Dim ca As CommArea = NewArea("APMATCHCA")
        ca.SetDecimal("MT-INV-QTY", invoiceQty)
        ca.SetDecimal("MT-INV-PRICE", invoicePrice)
        ca.SetDecimal("MT-ORD-QTY", orderQty)
        ca.SetDecimal("MT-ORD-PRICE", orderPrice)
        ca.SetDecimal("MT-RCV-ACCEPTED", receivedQty)
        Dim r As CobolResult = Link("APMATCH", ca, "TOLCHK")
        MatchState = ca.GetString("MT-MATCH-STATUS")
        Note = ca.GetString("MT-TOLERANCE-NOTE")
        Return r
    End Function

End Class
