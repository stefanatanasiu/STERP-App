Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Supplier verification and bank detail facade (module ap).  APSUPVAL owns the supplier
''' rules and APBANKVR owns Confirmation of Payee, the call back and the independent
''' approval of a bank change - this class only carries the COMMAREA across.
''' REQ: AP-001, AP-002, AP-024, AP-027, AP-030
''' </summary>
Public Class SupplierService
    Inherits ServiceBase

    Public Property Balance As Decimal
    Public Property OpenItems As Integer
    Public Property TermDays As Integer
    Public Property CanPurchase As Boolean
    Public Property CanPay As Boolean
    Public Property Note As String = ""
    Public Property CopResult As String = ""
    Public Property BankStatus As String = ""

    ''' <summary>Supplier list with ledger balance and verification counts.</summary>
    Public Function Suppliers(ByVal filter As String, ByVal mode As String) As DataTable
        Return Query("ap.usp_SupplierEnquiry", "@CompanyCode", Company, "@SupplierCode", filter, "@Mode", mode)
    End Function

    ''' <summary>Every bank record held for a supplier, newest first.</summary>
    Public Function Banks(ByVal supplier As String) As DataTable
        Return Query("ap.usp_ListSupplierBanks", "@CompanyCode", Company, "@SupplierCode", supplier)
    End Function

    ''' <summary>APSUPVAL decides whether the supplier may be ordered from or paid (AP-001).</summary>
    Public Function Validate(ByVal supplier As String) As CobolResult
        Dim ca As CommArea = NewArea("APSUPVALCA")
        ca.SetString("SV-SUPPLIER", supplier)
        Dim r As CobolResult = Link("APSUPVAL", ca, "VALIDATE")
        ReadSupplier(ca)
        Return r
    End Function

    ''' <summary>APSUPVAL answers the payment eligibility question on its own (AP-024).</summary>
    Public Function PaymentCheck(ByVal supplier As String) As CobolResult
        Dim ca As CommArea = NewArea("APSUPVALCA")
        ca.SetString("SV-SUPPLIER", supplier)
        Dim r As CobolResult = Link("APSUPVAL", ca, "CANPAY")
        ReadSupplier(ca)
        Return r
    End Function

    ''' <summary>Put a supplier on, or take it off, purchase or payment hold (AP-024).</summary>
    Public Function Hold(ByVal supplier As String, ByVal holdType As String, ByVal reason As String,
                         ByVal release As Boolean) As CobolResult
        Dim ca As CommArea = NewArea("APSUPVALCA")
        ca.SetString("SV-SUPPLIER", supplier)
        ca.SetString("SV-HOLD-TYPE", holdType)
        ca.SetString("SV-REASON", reason)
        Dim r As CobolResult = Link("APSUPVAL", ca, If(release, "RELEASE", "HOLD"), Session.NewCorrelationId())
        ReadSupplier(ca)
        Return r
    End Function

    ''' <summary>Confirmation of Payee on a bank record; the answer is never a payment decision
    ''' on its own, it is evidence the approver has to weigh (AP-002, D-19).</summary>
    Public Function ConfirmationOfPayee(ByVal supplier As String, ByVal bankId As Integer) As CobolResult
        Dim ca As CommArea = NewArea("APBANKVRCA")
        ca.SetString("BV-SUPPLIER", supplier)
        ca.SetInt("BV-BANK-ID", bankId)
        Dim r As CobolResult = Link("APBANKVR", ca, "COP", Session.NewCorrelationId())
        ReadBank(ca)
        Return r
    End Function

    ''' <summary>Independent approval of a bank change.  The approver is always the signed on
    ''' user, so a preparer who tries to approve their own change is refused by the engine
    ''' and the caller shows that refusal (AP-027, D-19).</summary>
    Public Function ApproveBank(ByVal supplier As String, ByVal bankId As Integer, ByVal decision As String) As CobolResult
        Dim ca As CommArea = NewArea("APBANKVRCA")
        ca.SetString("BV-SUPPLIER", supplier)
        ca.SetInt("BV-BANK-ID", bankId)
        ca.SetString("BV-APPROVER", User)
        ca.SetString("BV-DECISION", decision)
        Dim r As CobolResult = Link("APBANKVR", ca, "APPROVE", Session.NewCorrelationId())
        ReadBank(ca)
        Return r
    End Function

    Private Sub ReadSupplier(ByVal ca As CommArea)
        Balance = ca.GetDecimal("SV-OPEN-BAL-GBP")
        OpenItems = ca.GetInt("SV-OPEN-ITEM-CNT")
        TermDays = ca.GetInt("SV-TERM-DAYS")
        CanPurchase = (ca.GetString("SV-CAN-PURCHASE") = "Y")
        CanPay = (ca.GetString("SV-CAN-PAY") = "Y")
        Note = ca.GetString("SV-NOTE")
    End Sub

    Private Sub ReadBank(ByVal ca As CommArea)
        CopResult = ca.GetString("BV-COP-RESULT")
        BankStatus = ca.GetString("BV-STATUS")
        Note = ca.GetString("BV-NOTE")
    End Sub

End Class
