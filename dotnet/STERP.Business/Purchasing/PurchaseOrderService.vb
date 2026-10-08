Imports System.Collections.Generic
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Purchase order facade - capture, pricing explanation, budget check, approval, issue,
''' acknowledgement, amendment, blanket call-off and the buyer's open/commitment/overdue
''' enquiries.  POORDER, POPRICE, BUDGCHK, POAPPRV, POSUPCHK and PAYTERMC keep every rule;
''' this class only moves fields in and out of their COMMAREAs.
''' REQ: PO-003, PO-004, PO-005, PO-006, PO-007, PO-008, PO-017, PO-021, PO-022, PO-024,
''' REQ: PO-026, PO-027, COM-015
''' </summary>
Public Class PurchaseOrderService
    Inherits ServiceBase

    Public Property OrderNumber As String = ""
    Public Property Status As String = ""
    Public Property Outcome As String = ""
    Public Property NetAmount As Decimal
    Public Property VatAmount As Decimal
    Public Property GrossAmount As Decimal
    Public Property CommitmentGBP As Decimal
    Public Property LineNum As Integer
    Public Property LineNetGBP As Decimal
    Public Property PriceSource As String = ""
    ''' <summary>The sentence behind <see cref="PriceSource"/>: agreement, dates and break (PO-004).</summary>
    Public Property PriceSourceText As String = ""
    Public Property NeedsReapproval As Boolean
    Public Property MaterialChange As Boolean
    Public Property ApprovalStatus As String = ""
    Public Property WasInvalidated As Boolean
    Public Property Budget As CommArea
    Public Property Price As CommArea
    ''' <summary>Lines captured in this sitting, with the price explanation beside each one.</summary>
    Public ReadOnly Property Lines As New DataTable("OrderLines")

    Public Sub New()
        PoMap.Columns(Lines, "LineNum:INT", "ItemCode", "Description", "OrderedQty:DEC", "UomCode",
                      "UnitPrice:DEC", "PricePer:DEC", "DiscountPct:DEC", "NetGBP:DEC",
                      "PriceSource", "BudgetOutcome", "DestSite")
    End Sub

    ' ---------------------------------------------------------------- enquiries (PO-024)

    Public Function OpenOrders(ByVal supplier As String, ByVal orderNo As String,
                               ByVal includeClosed As Boolean) As DataTable
        Return Query("po.usp_OpenOrders", "@CompanyCode", Company, "@SiteCode", Site,
                     "@SupplierCode", supplier, "@OrderNumber", orderNo,
                     "@IncludeClosed", If(includeClosed, 1, 0))
    End Function

    Public Function Commitments(ByVal budgetCode As String, ByVal groupBy As String) As DataTable
        Return Query("po.usp_CommitmentEnquiry", "@CompanyCode", Company, "@BudgetCode", budgetCode,
                     "@GroupBy", If(groupBy = "", "ORDER", groupBy))
    End Function

    Public Function Overdue(ByVal supplier As String) As DataTable
        Return Query("po.usp_OverdueSchedules", "@CompanyCode", Company, "@SupplierCode", supplier,
                     "@AsOfDate", Session.BusinessDate, "@SiteCode", Site)
    End Function

    ' ---------------------------------------------------------------- order capture

    ''' <summary>Order header: supplier, currency, terms, incoterms and delivery (PO-003).</summary>
    Public Function Create(ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = NewArea("POORDERCA")
        ca.SetString("OR-ORDER-TYPE", If(PoMap.S(v, "TYPE") = "", "STANDARD", PoMap.S(v, "TYPE")))
        ca.SetString("OR-SUPPLIER", PoMap.S(v, "SUPPLIER"))
        ca.SetString("OR-BUYER", User)
        ca.SetDate("OR-ORDER-DATE", Session.BusinessDate)
        ca.SetString("OR-CURRENCY", If(PoMap.S(v, "CCY") = "", Session.BaseCurrency, PoMap.S(v, "CCY")))
        ca.SetString("OR-TERMS-CODE", PoMap.S(v, "TERMS"))
        ca.SetString("OR-INCOTERMS", PoMap.S(v, "INCO"))
        ca.SetString("OR-NAMED-PLACE", PoMap.T(v, "PLACE"))
        ca.SetString("OR-DELIVERY-ADDR", PoMap.T(v, "ADDRESS"))
        ca.SetString("OR-SUPPLIER-REF", PoMap.T(v, "SUPPREF"))
        ca.SetString("OR-BUDGET-CODE", PoMap.S(v, "BUDGET"))
        ca.SetString("OR-BLANKET-NUMBER", PoMap.S(v, "BLANKET"))
        ca.SetString("OR-LINKED-SO", PoMap.S(v, "SALESORDER"))
        Dim r As CobolResult = Link("POORDER", ca, "CREATE", Session.NewCorrelationId())
        Read(ca)
        Lines.Rows.Clear()
        Return r
    End Function

    ''' <summary>One order line; AUTO-PRICE lets POPRICE fill the price in (PO-003, PO-004).</summary>
    Public Function AddLine(ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = NewArea("POORDERCA")
        ca.SetString("OR-ORDER-NUMBER", OrderNumber)
        ca.SetString("OR-LINE-TYPE", If(PoMap.S(v, "LINETYPE") = "", "STOCK", PoMap.S(v, "LINETYPE")))
        ca.SetString("OR-ITEM-CODE", PoMap.S(v, "ITEM"))
        ca.SetString("OR-REVISION", PoMap.S(v, "REV"))
        ca.SetString("OR-DESCRIPTION", PoMap.T(v, "DESC"))
        ca.SetDecimal("OR-ORDERED-QTY", PoMap.D(v, "QTY"))
        ca.SetString("OR-UOM-CODE", PoMap.S(v, "UOM"))
        ca.SetDecimal("OR-UNIT-PRICE", PoMap.D(v, "PRICE"))
        ca.SetDecimal("OR-PRICE-PER", PoMap.D(v, "PRICEPER"))
        ca.SetDecimal("OR-DISCOUNT-PCT", PoMap.D(v, "DISCPCT"))
        ca.SetDecimal("OR-CHARGE-AMT", PoMap.D(v, "CHARGE"))
        ca.SetString("OR-VAT-CODE", PoMap.S(v, "VATCODE"))
        ca.SetString("OR-ACCOUNT-CODE", PoMap.S(v, "ACCOUNT"))
        ca.SetString("OR-COST-CENTRE", PoMap.S(v, "CC"))
        ca.SetString("OR-PROJECT", PoMap.S(v, "PROJECT"))
        ca.SetString("OR-DEST-SITE", If(PoMap.S(v, "DESTSITE") = "", Site, PoMap.S(v, "DESTSITE")))
        ca.SetString("OR-DEST-LOCATION", PoMap.S(v, "DESTLOC"))
        ca.SetString("OR-REQ-NUMBER", PoMap.S(v, "REQ"))
        ca.SetString("OR-AUTO-PRICE", If(PoMap.D(v, "PRICE") = 0D, "Y", "N"))
        ca.SetDate("OR-SCH-REQ-DATE", PoMap.Dt(v, "DUE", Session.BusinessDate))
        Dim r As CobolResult = Link("POORDER", ca, "ADDLINE")
        Read(ca)
        If r.Succeeded Then
            Lines.Rows.Add(LineNum, PoMap.S(v, "ITEM"), PoMap.T(v, "DESC"), PoMap.D(v, "QTY"),
                           ca.GetString("OR-UOM-CODE"), ca.GetDecimal("OR-UNIT-PRICE"),
                           ca.GetDecimal("OR-PRICE-PER"), ca.GetDecimal("OR-DISCOUNT-PCT"),
                           LineNetGBP, PriceSource, "", ca.GetString("OR-DEST-SITE"))
        End If
        Return r
    End Function

    ''' <summary>A further delivery schedule on an existing line (PO-008).</summary>
    Public Function Schedule(ByVal line As Integer, ByVal qty As Decimal, ByVal due As Date,
                             ByVal destSite As String) As CobolResult
        Dim ca As CommArea = NewArea("POORDERCA")
        ca.SetString("OR-ORDER-NUMBER", OrderNumber)
        ca.SetInt("OR-LINE-NUM", line)
        ca.SetDecimal("OR-SCH-QTY", qty)
        ca.SetDate("OR-SCH-REQ-DATE", due)
        ca.SetString("OR-SCH-DEST-SITE", If(destSite = "", Site, destSite))
        Dim r As CobolResult = Link("POORDER", ca, "SCHEDULE")
        Read(ca)
        Return r
    End Function

    ''' <summary>ISSUE / ENQUIRE / CALLOFF / COPY against an order number (PO-006, PO-021).</summary>
    Public Function Act(ByVal func As String, ByVal orderNo As String,
                        ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = NewArea("POORDERCA")
        ca.SetString("OR-ORDER-NUMBER", If(orderNo = "", OrderNumber, orderNo))
        ca.SetString("OR-COPY-FROM", PoMap.S(v, "COPYFROM"))
        ca.SetString("OR-BLANKET-NUMBER", PoMap.S(v, "BLANKET"))
        ca.SetString("OR-SUPPLIER", PoMap.S(v, "SUPPLIER"))
        ca.SetDecimal("OR-SCH-QTY", PoMap.D(v, "QTY"))
        ca.SetString("OR-ITEM-CODE", PoMap.S(v, "ITEM"))
        If PoMap.T(v, "DUE") <> "" Then ca.SetDate("OR-SCH-REQ-DATE", PoMap.Dt(v, "DUE", Session.BusinessDate))
        Dim r As CobolResult = Link("POORDER", ca, func, Session.NewCorrelationId())
        Read(ca)
        Return r
    End Function

    ''' <summary>Supplier acknowledgement of quantity, price and date (PO-007).</summary>
    Public Function Acknowledge(ByVal orderNo As String, ByVal line As Integer, ByVal qty As Decimal,
                                ByVal price As Decimal, ByVal confirmed As Date, ByVal note As String) As CobolResult
        Dim ca As CommArea = NewArea("POORDERCA")
        ca.SetString("OR-ORDER-NUMBER", orderNo)
        ca.SetInt("OR-LINE-NUM", line)
        ca.SetDecimal("OR-ACK-QTY", qty)
        ca.SetDecimal("OR-ACK-PRICE", price)
        ca.SetDate("OR-ACK-DATE", confirmed)
        ca.SetString("OR-ACK-NOTE", note)
        Dim r As CobolResult = Link("POORDER", ca, "ACKNOWL")
        Read(ca)
        Return r
    End Function

    ''' <summary>An amendment; a material change re-opens the approval (PO-007, PO-006).</summary>
    Public Function Amend(ByVal orderNo As String, ByVal line As Integer, ByVal field As String,
                          ByVal newValue As String, ByVal reason As String) As CobolResult
        Dim ca As CommArea = NewArea("POORDERCA")
        ca.SetString("OR-ORDER-NUMBER", orderNo)
        ca.SetInt("OR-LINE-NUM", line)
        ca.SetString("OR-AMEND-FIELD", field)
        ca.SetString("OR-AMEND-NEW", newValue)
        ca.SetString("OR-AMEND-REASON", reason)
        Dim r As CobolResult = Link("POORDER", ca, "AMEND", Session.NewCorrelationId())
        Read(ca)
        Return r
    End Function

    ' ---------------------------------------------------------------- price, budget, approval

    ''' <summary>Where the price came from: agreement, break, quote or last paid (PO-004).</summary>
    Public Function Explain(ByVal supplier As String, ByVal item As String, ByVal qty As Decimal,
                            ByVal uom As String, ByVal currency As String) As CobolResult
        Dim ca As CommArea = NewArea("POPRICECA")
        ca.SetString("PR-SUPPLIER", supplier)
        ca.SetString("PR-ITEM-CODE", item)
        ca.SetString("PR-UOM-CODE", uom)
        ca.SetString("PR-CURRENCY", If(currency = "", Session.BaseCurrency, currency))
        ca.SetDecimal("PR-QUANTITY", qty)
        ca.SetDate("PR-PRICING-DATE", Session.BusinessDate)
        Dim r As CobolResult = Link("POPRICE", ca, "PRICE")
        Price = ca
        ReadPrice(ca)
        Return r
    End Function

    ''' <summary>Where a price last actually paid came from - PO-004's HISTORY basis.</summary>
    Public Function LastPaid(ByVal supplier As String, ByVal item As String) As CobolResult
        Dim ca As CommArea = NewArea("POPRICECA")
        ca.SetString("PR-SUPPLIER", supplier)
        ca.SetString("PR-ITEM-CODE", item)
        ca.SetDate("PR-PRICING-DATE", Session.BusinessDate)
        Dim r As CobolResult = Link("POPRICE", ca, "HISTORY")
        Price = ca
        ReadPrice(ca)
        Return r
    End Function

    ''' <summary>POPRICE always names its basis, so the buyer is never shown a price with no origin.</summary>
    Private Sub ReadPrice(ByVal ca As CommArea)
        PriceSource = ca.GetString("PR-PRICE-BASIS").Trim()
        PriceSourceText = ca.GetString("PR-SOURCE-TEXT").Trim()
        If PriceSource.Length = 0 Then PriceSource = "UNKNOWN"
        If PriceSourceText.Length = 0 Then PriceSourceText = PriceSource
    End Sub

    ''' <summary>Quantity break ladder behind the price (PO-004).</summary>
    Public Function Breaks(ByVal supplier As String, ByVal item As String) As DataTable
        Dim ca As CommArea = NewArea("POPRICECA")
        ca.SetString("PR-SUPPLIER", supplier)
        ca.SetString("PR-ITEM-CODE", item)
        ca.SetDate("PR-PRICING-DATE", Session.BusinessDate)
        Link("POPRICE", ca, "BREAKS")
        Price = ca
        ReadPrice(ca)
        Return ca.GetTable("PR-BREAK", ca.GetInt("PR-BREAK-CNT"))
    End Function

    ''' <summary>Budget answer measured after the proposed purchase: OK, WARN or BLOCK (PO-005).</summary>
    Public Function CheckBudget(ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = NewArea("BUDGCHKCA")
        ca.SetString("BC-BUDGET-CODE", PoMap.S(v, "BUDGET"))
        ca.SetString("BC-ACCOUNT-CODE", PoMap.S(v, "ACCOUNT"))
        ca.SetString("BC-COST-CENTRE", PoMap.S(v, "CC"))
        ca.SetString("BC-PROJECT", PoMap.S(v, "PROJECT"))
        ca.SetString("BC-DOC-TYPE", If(PoMap.S(v, "DOCTYPE") = "", "ORDER", PoMap.S(v, "DOCTYPE")))
        ca.SetString("BC-DOC-REF", If(PoMap.S(v, "DOCREF") = "", OrderNumber, PoMap.S(v, "DOCREF")))
        ca.SetString("BC-REQUESTER", User)
        ca.SetDate("BC-MOVEMENT-DATE", Session.BusinessDate)
        ca.SetDecimal("BC-NET-GBP", PoMap.D(v, "NETGBP"))
        ca.SetDecimal("BC-NONREC-VAT-GBP", PoMap.D(v, "NONRECVAT"))
        ca.SetDecimal("BC-CHARGE-GBP", PoMap.D(v, "CHARGE"))
        ca.SetString("BC-OVERRIDE-BY", PoMap.S(v, "OVERRIDEBY"))
        ca.SetString("BC-OVERRIDE-REASON", PoMap.T(v, "OVERRIDEREASON"))
        Dim r As CobolResult = Link("BUDGCHK", ca, If(PoMap.S(v, "FUNC") = "", "CHECK", PoMap.S(v, "FUNC")))
        Budget = ca
        Return r
    End Function

    ''' <summary>Approval request, decision or state check for the exact order version (PO-006).</summary>
    Public Function Approval(ByVal func As String, ByVal orderNo As String, ByVal decision As String,
                             ByVal comment As String) As CobolResult
        Dim ca As CommArea = NewArea("POAPPRVCA")
        ca.SetString("AV-ORDER-NUMBER", orderNo)
        ca.SetString("AV-APPROVAL-TYPE", "PURCHASEORDER")
        ca.SetString("AV-APPROVER", User)
        ca.SetString("AV-DECISION", decision)
        ca.SetString("AV-COMMENT", comment)
        Dim r As CobolResult = Link("POAPPRV", ca, func, Session.NewCorrelationId())
        ApprovalStatus = ca.GetString("AV-APPROVAL-STATUS")
        WasInvalidated = (ca.GetString("AV-WAS-INVALIDATED") = "Y")
        Approver = ca.GetString("AV-REQUESTED-BY")
        RequiredPermission = ca.GetString("AV-REQUIRED-PERM")
        AmountGBP = ca.GetDecimal("AV-AMOUNT-GBP")
        Return r
    End Function

    Public Property Approver As String = ""
    Public Property RequiredPermission As String = ""
    Public Property AmountGBP As Decimal

    ''' <summary>Supplier due diligence before an order may be issued (PO-026).</summary>
    Public Function Diligence(ByVal supplier As String, ByVal valueGbp As Decimal) As CommArea
        Dim ca As CommArea = NewArea("POSUPCHKCA")
        ca.SetString("DD-SUPPLIER", supplier)
        ca.SetDate("DD-CHECK-DATE", Session.BusinessDate)
        ca.SetDecimal("DD-ORDER-VALUE-GBP", valueGbp)
        ca.SetString("DD-CHECKED-BY", User)
        LastResult = Link("POSUPCHK", ca, "CHECK")
        Return ca
    End Function

    ''' <summary>Payment terms against the company's published policy (PO-027, D-25).</summary>
    Public Function Terms(ByVal supplier As String, ByVal termsCode As String, ByVal valueGbp As Decimal) As CommArea
        Dim ca As CommArea = NewArea("PAYTERMCCA")
        ca.SetString("PT-SUPPLIER", supplier)
        ca.SetString("PT-ORDER-NUMBER", OrderNumber)
        ca.SetString("PT-TERMS-CODE", termsCode)
        ca.SetDate("PT-ORDER-DATE", Session.BusinessDate)
        ca.SetDecimal("PT-VALUE-GBP", valueGbp)
        LastResult = Link("PAYTERMC", ca, "CHECK")
        Return ca
    End Function

    Public Property LastResult As CobolResult

    Private Sub Read(ByVal ca As CommArea)
        If ca.GetString("OR-ORDER-NUMBER") <> "" Then OrderNumber = ca.GetString("OR-ORDER-NUMBER")
        Status = ca.GetString("OR-STATUS")
        Outcome = ca.GetString("OR-OUTCOME")
        NetAmount = ca.GetDecimal("OR-NET-AMOUNT")
        VatAmount = ca.GetDecimal("OR-VAT-AMOUNT")
        GrossAmount = ca.GetDecimal("OR-GROSS-AMOUNT")
        CommitmentGBP = ca.GetDecimal("OR-COMMITMENT-GBP")
        LineNum = ca.GetInt("OR-LINE-NUM")
        LineNetGBP = ca.GetDecimal("OR-LINE-NET-GBP")
        PriceSource = ca.GetString("OR-PRICE-SOURCE")
        NeedsReapproval = (ca.GetString("OR-NEEDS-REAPPR") = "Y")
        MaterialChange = (ca.GetString("OR-MATERIAL-CHANGE") = "Y")
    End Sub
End Class
