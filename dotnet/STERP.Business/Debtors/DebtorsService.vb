Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Debtors (sales ledger) facade.  The enquiries are stored procedure and view reads; every
''' decision - exposure, the release of an order, what a receipt may be applied to, whether a
''' dispute is accepted - belongs to CREDCHK, RCPTALOC, ARDISPUT and ARENQ.  Nothing in this
''' class decides anything; it fills a COMMAREA, links, and shapes the answer for the screen.
''' REQ: AR-007, AR-008, AR-009, AR-010, AR-011, AR-012, AR-015, AR-016, AR-017, AR-019, AR-022
''' </summary>
Public Class DebtorsService
    Inherits ServiceBase

    ''' <summary>Largest number of allocation instructions RCPTALOCCA can carry in one link.</summary>
    Public Const MaxAllocations As Integer = 20

    ''' <summary>F2 customer picker: a read of ar.vw_CustomerBalance, so the operator sees the
    ''' credit state of an account before choosing it.</summary>
    Public Const CustomerPickSql As String =
        "SELECT CustomerCode, LegalName AS CustomerName, CurrencyCode, CreditState, BalanceGBP " &
        "FROM ar.vw_CustomerBalance WHERE CompanyCode = @CompanyCode " &
        "AND (@Search IS NULL OR CustomerCode LIKE @Search + '%' OR LegalName LIKE '%' + @Search + '%') " &
        "ORDER BY CustomerCode"

    ''' <summary>COMMAREA of the last COBOL link, so the screen can label every figure in it.</summary>
    Public Property Area As CommArea

    ' ------------------------------------------------------------------ enquiries (SQL)

    ''' <summary>The five panels of the enquiry screen for one account (AR-017, AR-022).</summary>
    Public Function Enquiry(ByVal customer As String) As DataSet
        Return QuerySet("ar.usp_CustomerEnquiry", "@CompanyCode", Company,
                        "@CustomerCode", customer, "@UserCode", User)
    End Function

    ''' <summary>Aged debt at a date: bucket summary, then the same figures per account (AR-017).</summary>
    Public Function AgedDebt(ByVal asOf As Date) As DataSet
        Return QuerySet("ar.usp_AgedDebtSummary", "@CompanyCode", Company, "@AsOf", asOf)
    End Function

    ''' <summary>The credit controller's daily list and the accounts behind it (AR-016, AR-024).</summary>
    Public Function Worklist(ByVal ownerUser As String) As DataSet
        Return QuerySet("ar.usp_CreditControlWorklist", "@CompanyCode", Company,
                        "@UserCode", User, "@OwnerUser", ownerUser)
    End Function

    ''' <summary>What a receipt may be applied to, the credits available, and the tolerances
    ''' the allocation screen previews a residual with (AR-010, AR-019).</summary>
    Public Function Allocatable(ByVal customer As String, ByVal currencyCode As String) As DataSet
        Return QuerySet("ar.usp_OpenItemsForAllocation", "@CompanyCode", Company,
                        "@CustomerCode", customer, "@CurrencyCode", currencyCode)
    End Function

    ''' <summary>Statement lines for one account, open item or brought forward style (AR-021).</summary>
    Public Function Statement(ByVal customer As String, ByVal asOf As Date, ByVal style As String) As DataSet
        Return QuerySet("ar.usp_CustomerStatementLines", "@CompanyCode", Company,
                        "@CustomerCode", customer, "@AsOf", asOf, "@Style", style)
    End Function

    ' ------------------------------------------------------------------ engine (COBOL)

    ''' <summary>ARENQ: the trading metrics for the twelve months to the stated date.  DSO on
    ''' no credit sales comes back undefined, never zero (AR-017, AR-022).</summary>
    Public Function Metrics(ByVal customer As String, ByVal asOf As Date) As CobolResult
        Dim ca As CommArea = NewArea("ARENQCA")
        ca.SetString("EQ-CUSTOMER", customer)
        ca.SetString("EQ-AS-AT", Fmt.Yyyymmdd(asOf))
        ca.SetString("EQ-FROM-DATE", Fmt.Yyyymmdd(asOf.AddMonths(-12)))
        ca.SetString("EQ-TO-DATE", Fmt.Yyyymmdd(asOf))
        Area = ca
        Return Link("ARENQ", ca, "METRICS")
    End Function

    ''' <summary>CREDCHK: exposure and the release decision.  RELEASE, HOLD or BLOCK is the
    ''' engine's word; the screen only reports it (AR-007, AR-008, D-06).</summary>
    Public Function CreditCheck(ByVal customer As String, ByVal orderValue As Decimal,
                                ByVal currencyCode As String, ByVal checkPoint As String,
                                ByVal documentRef As String) As CobolResult
        Dim ca As CommArea = NewArea("CREDCHKCA")
        ca.SetString("CK-CUSTOMER", customer)
        ca.SetString("CK-CHECK-POINT", If(checkPoint = "", "ORDER", checkPoint))
        ca.SetString("CK-DOCUMENT-REF", documentRef)
        ca.SetString("CK-EVAL-DATE", Fmt.Yyyymmdd(Session.BusinessDate))
        ca.SetString("CK-CURRENCY", If(currencyCode = "", Session.BaseCurrency, currencyCode))
        ca.SetDecimal("CK-ORDER-VALUE", orderValue)
        Area = ca
        Return Link("CREDCHK", ca, "CHECK", Session.NewCorrelationId())
    End Function

    Public Function CreditOverrides(Optional ByVal requestsOnly As Boolean = False) As DataTable
        Return STERP.Data.SqlHelper.ExecuteDataTable(
            "SELECT OverrideId, CustomerCode, Scope, DocumentRef, OverrideAmountGBP, Reason, " &
            "RequestedBy, ApprovedBy, ValidFrom, ExpiresOn, Status FROM ar.CreditOverride " &
            "WHERE CompanyCode = @CompanyCode AND (@RequestsOnly = 0 OR RequestedBy = @UserCode) " &
            "ORDER BY OverrideId DESC", "@CompanyCode", Company, "@RequestsOnly", requestsOnly, "@UserCode", User)
    End Function

    Public Function RequestCreditOverride(ByVal customer As String, ByVal documentRef As String,
                                          ByVal amountGBP As Decimal, ByVal reason As String) As CobolResult
        Dim ca As CommArea = NewArea("CREDCHKCA")
        ca.SetString("CK-CUSTOMER", customer)
        ca.SetString("CK-CHECK-POINT", "ORDER")
        ca.SetString("CK-DOCUMENT-REF", documentRef)
        ca.SetString("CK-CURRENCY", "GBP")
        ca.SetString("CK-EVAL-DATE", Fmt.Yyyymmdd(Session.BusinessDate))
        ca.SetString("CK-OVERRIDE-SCOPE", "ORDER")
        ca.SetDecimal("CK-OVERRIDE-AMT", amountGBP)
        ca.SetString("CK-OVERRIDE-REASON", reason)
        Area = ca
        Return Link("CREDCHK", ca, "OVERRIDE", Session.NewCorrelationId())
    End Function

    Public Function ApproveCreditOverride(ByVal customer As String, ByVal overrideId As Integer) As CobolResult
        Dim ca As CommArea = NewArea("CREDCHKCA")
        ca.SetString("CK-CUSTOMER", customer)
        ca.SetString("CK-EVAL-DATE", Fmt.Yyyymmdd(Session.BusinessDate))
        ca.SetInt("CK-OVERRIDE-ID", overrideId)
        Area = ca
        Return Link("CREDCHK", ca, "APPROVE", Session.NewCorrelationId())
    End Function

    ''' <summary>Builds the RCPTALOC COMMAREA for one receipt header; nothing is committed
    ''' until the caller links with RECEIPT (AR-009).</summary>
    Public Function NewReceipt(ByVal customer As String, ByVal receiptDate As Date, ByVal currencyCode As String,
                               ByVal bankAccount As String, ByVal method As String, ByVal gross As Decimal,
                               ByVal bankReference As String, ByVal externalReference As String) As CommArea
        Dim ca As CommArea = NewArea("RCPTALOCCA")
        ca.SetString("RA-CUSTOMER", customer)
        ca.SetString("RA-PAYER", customer)
        ca.SetString("RA-METHOD", method)
        ca.SetString("RA-BANK-ACCOUNT", bankAccount)
        ca.SetString("RA-RECEIPT-DATE", Fmt.Yyyymmdd(receiptDate))
        ca.SetString("RA-VALUE-DATE", Fmt.Yyyymmdd(receiptDate))
        ca.SetString("RA-POSTING-DATE", Fmt.Yyyymmdd(Session.BusinessDate))
        ca.SetString("RA-EFFECTIVE-DATE", Fmt.Yyyymmdd(receiptDate))
        ca.SetString("RA-CURRENCY", currencyCode)
        ca.SetDecimal("RA-GROSS", gross)
        ca.SetString("RA-BANK-REF", bankReference)
        ca.SetString("RA-EXTERNAL-REF", externalReference)
        ca.SetString("RA-SOURCE-TYPE", "RECEIPT")
        ca.SetString("RA-CONFIRM-STATE", "CONFIRMED")
        ca.SetInt("RA-ALLOC-CNT", 0)
        Area = ca
        Return ca
    End Function

    ''' <summary>Writes one allocation instruction (1 based, COBOL style) and keeps the
    ''' occurrence count in step.  The engine decides whether it is accepted.</summary>
    Public Sub AddAllocation(ByVal ca As CommArea, ByVal index As Integer, ByVal openItemId As Integer,
                             ByVal documentNumber As String, ByVal currencyCode As String, ByVal apply As Decimal)
        ca.SetInt("RA-A-ITEM-ID(" & index & ")", openItemId)
        ca.SetString("RA-A-DOC-NO(" & index & ")", documentNumber)
        ca.SetString("RA-A-CURRENCY(" & index & ")", currencyCode)
        ca.SetDecimal("RA-A-APPLY(" & index & ")", apply)
        ca.SetInt("RA-ALLOC-CNT", index)
    End Sub

    ''' <summary>As above, plus a small-difference write off and its reason (AR-012, D-22).  The
    ''' engine applies the write off limit and refuses a write off without a reason.</summary>
    Public Sub AddAllocation(ByVal ca As CommArea, ByVal index As Integer, ByVal openItemId As Integer,
                             ByVal documentNumber As String, ByVal currencyCode As String, ByVal apply As Decimal,
                             ByVal writeOff As Decimal, ByVal writeOffReason As String)
        AddAllocation(ca, index, openItemId, documentNumber, currencyCode, apply)
        ca.SetDecimal("RA-A-WRITEOFF(" & index & ")", writeOff)
        ca.SetString("RA-A-WO-REASON(" & index & ")", If(writeOffReason, ""))
    End Sub

    ''' <summary>Links RCPTALOC with PREVIEW (no business effect), RECEIPT (the cash effect) or
    ''' ALLOCATE (applies the instructions, no second cash effect) - AR-009, AR-010.</summary>
    Public Function RunReceipt(ByVal ca As CommArea, ByVal func As String) As CobolResult
        Area = ca
        If func = "PREVIEW" Then Return Link("RCPTALOC", ca, func)
        Return Link("RCPTALOC", ca, func, Session.NewCorrelationId())
    End Function

    ''' <summary>Authorised correction: the allocation is reversed, never erased (AR-011).</summary>
    Public Function CorrectAllocation(ByVal allocationId As Integer, ByVal reason As String) As CobolResult
        Dim ca As CommArea = NewArea("RCPTALOCCA")
        ca.SetInt("RA-ALLOC-ID", allocationId)
        ca.SetString("RA-CORRECT-REASON", reason)
        ca.SetString("RA-CORRECT-DATE", Fmt.Yyyymmdd(Session.BusinessDate))
        Area = ca
        Return Link("RCPTALOC", ca, "UNALLOC", Session.NewCorrelationId())
    End Function

    ''' <summary>ARDISPUT: raises or amends a disputed amount.  A dispute neither erases the
    ''' debt nor reduces credit exposure of itself (AR-015).</summary>
    Public Function RaiseDispute(ByVal customer As String, ByVal documentNumber As String, ByVal openItemId As Integer,
                                 ByVal amount As Decimal, ByVal reasonCode As String, ByVal reason As String) As CobolResult
        Dim ca As CommArea = NewArea("ARDISPUTCA")
        ca.SetString("DS-CUSTOMER", customer)
        ca.SetString("DS-CASE-TYPE", "DISPUTE")
        ca.SetString("DS-DOCUMENT-NO", documentNumber)
        ca.SetInt("DS-OPEN-ITEM-ID", openItemId)
        ca.SetString("DS-REASON-CODE", reasonCode)
        ca.SetString("DS-REASON", reason)
        ca.SetDecimal("DS-DISPUTED-AMT", amount)
        ca.SetString("DS-OWNER-USER", User)
        Area = ca
        Return Link("ARDISPUT", ca, "DISPUTE", Session.NewCorrelationId())
    End Function

    ''' <summary>ARDISPUT: records a promise to pay.  The sweep that marks a promise broken
    ''' belongs to the engine, not to the screen (AR-016).</summary>
    Public Function RecordPromise(ByVal customer As String, ByVal caseRef As String, ByVal amount As Decimal,
                                  ByVal promiseDate As Date, ByVal narrative As String) As CobolResult
        Dim ca As CommArea = NewArea("ARDISPUTCA")
        ca.SetString("DS-CUSTOMER", customer)
        ca.SetString("DS-CASE-REF", caseRef)
        ca.SetString("DS-CASE-TYPE", "COLLECTION")
        ca.SetString("DS-ACTION-TYPE", "PROMISE")
        ca.SetString("DS-ACTION-DATE", Fmt.Yyyymmdd(Session.BusinessDate))
        ca.SetDecimal("DS-PROMISE-AMT", amount)
        ca.SetString("DS-PROMISE-DATE", Fmt.Yyyymmdd(promiseDate))
        ca.SetString("DS-NARRATIVE", narrative)
        ca.SetString("DS-OWNER-USER", User)
        Area = ca
        Return Link("ARDISPUT", ca, "PROMISE", Session.NewCorrelationId())
    End Function

    ' ------------------------------------------------------------------ display shaping

    ''' <summary>Money in the currency it was stated in; a foreign amount never gets a pound
    ''' sign and sterling always does (COM-024).</summary>
    Public Shared Function Amount(ByVal value As Decimal, ByVal currencyCode As String) As String
        If currencyCode = "" OrElse currencyCode = "GBP" Then Return Fmt.Money(value)
        Return Fmt.Money(value, currencyCode)
    End Function

    ''' <summary>A COMMAREA money field as sterling, or "-" when the copybook has no such field.</summary>
    Public Shared Function AreaMoney(ByVal ca As CommArea, ByVal field As String) As String
        If ca Is Nothing OrElse Not ca.Has(field) Then Return "-"
        Return Fmt.Money(ca.GetDecimal(field))
    End Function

    ''' <summary>A COMMAREA PIC 9(8) date shown as dd/MM/yyyy, or "-" when it is unset.</summary>
    Public Shared Function AreaDate(ByVal ca As CommArea, ByVal field As String) As String
        If ca Is Nothing OrElse Not ca.Has(field) Then Return "-"
        Dim raw As String = ca.GetString(field)
        If raw = "" OrElse raw = "00000000" Then Return "-"
        Dim d As Date? = Fmt.ParseDate(raw)
        Return If(d.HasValue, Fmt.Date(d.Value), raw)
    End Function

End Class
