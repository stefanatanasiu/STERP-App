Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Sales order facade.  Every commercial decision belongs to the engine: PRICING chooses and
''' explains the selling price, SODISC runs the fixed discount/allowance/charge/VAT sequence,
''' SOVAT decides the liability, SOMARGN measures the margin against a named comparable cost,
''' SOACCEPT validates, accepts and releases, SOAVAIL reports availability by labelled source,
''' SORESV reserves, PICKGEN picks and DESPATCH confirms.  Nothing here decides anything - it
''' fills a COMMAREA, links, and shapes the answer for the screen.  The return code, the reason
''' code and the message always travel back untouched, so 00 OK, 04 warning, 06 hold, 08 error,
''' 12 severe and 16 not found stay distinguishable to the caller.
''' REQ: SO-002, SO-003, SO-004, SO-005, SO-006, SO-007, SO-009, SO-010, SO-011, SO-012,
''' REQ: SO-013, SO-014, SO-022, SO-025, COM-015, COM-024, RPT-008
''' </summary>
Public Class SalesOrderService
    Inherits ServiceBase

    ''' <summary>Lines SODISC, DESPATCH and PICKGEN carry in one link (the smallest OCCURS).</summary>
    Public Const MaxLines As Integer = 30

    ''' <summary>F2 order picker - a parameterised read, never a SQL string built from input.</summary>
    Public Const OrderPickSql As String =
        "SELECT OrderNumber, CustomerCode, OrderDate, CurrencyCode, ReleaseStatus, FulfilmentStatus " &
        "FROM so.SalesOrder WHERE CompanyCode = @CompanyCode " &
        "AND (@Search IS NULL OR @Search = '' OR OrderNumber LIKE @Search + '%' OR CustomerCode LIKE @Search + '%') " &
        "ORDER BY OrderNumber"

    ''' <summary>
    ''' F2 pick list picker for Despatch Confirmation: this company and site, not cancelled, with
    ''' picked stock still waiting to go out (picked on the schedule but not yet despatched against
    ''' it).  @Search matches the pick list, the order or the
    ''' customer; @Argument, when given, restricts the list to one order.
    ''' </summary>
    Public Const PickListPickSql As String =
        "SELECT p.PickListRef, p.OrderNumber, p.CustomerCode, p.Status, p.RequiredDate, p.WarehouseCode " &
        "FROM so.PickList p WHERE p.CompanyCode = @CompanyCode AND p.SiteCode = @SiteCode " &
        "AND p.Status <> 'CANCELLED' " &
        "AND (@Argument IS NULL OR @Argument = '' OR p.OrderNumber = @Argument) " &
        "AND (@Search IS NULL OR @Search = '' OR p.PickListRef LIKE @Search + '%' " &
        "OR p.OrderNumber LIKE @Search + '%' OR p.CustomerCode LIKE @Search + '%') " &
        "AND EXISTS (SELECT 1 FROM so.PickListLine l JOIN so.DeliverySchedule s " &
        "ON s.SalesOrderLineId = l.SalesOrderLineId AND s.ScheduleNo = l.ScheduleNo " &
        "WHERE l.PickListId = p.PickListId AND l.PickedQty > 0 AND s.Quantity > s.DespatchedQty " &
        "AND s.DespatchedQty < (SELECT SUM(x.PickedQty) FROM so.PickListLine x JOIN so.PickList xp " &
        "ON xp.PickListId = x.PickListId WHERE xp.Status <> 'CANCELLED' " &
        "AND x.SalesOrderLineId = l.SalesOrderLineId AND x.ScheduleNo = l.ScheduleNo)) " &
        "ORDER BY p.PickListRef"

    Private Const HeaderSql As String =
        "SELECT o.OrderNumber, o.OrderType, o.SiteCode, o.CustomerCode, c.LegalName AS CustomerName, " &
        "o.CustomerOrderRef, o.OrderDate, o.CurrencyCode, o.TermsCode, o.SalespersonCode, " &
        "o.IncotermsRule, o.IncotermsNamedPlace, o.DeliveryAddressRef, o.CommercialStatus, " &
        "o.ReleaseStatus, o.FulfilmentStatus, o.BillingStatus, o.HoldReason, o.NetAmount, " &
        "o.VatAmount, o.GrossAmount FROM so.SalesOrder o LEFT JOIN ar.Customer c " &
        "ON c.CompanyCode = o.CompanyCode AND c.CustomerCode = o.CustomerCode " &
        "WHERE o.CompanyCode = @CompanyCode AND o.OrderNumber = @OrderNumber"

    Private Const LineSql As String =
        "SELECT LineNum, ItemCode, Description, AcceptedQty, UomCode, ConversionFactor, UnitPrice, " &
        "PricePerQty, PriceSource, DiscountPct1, DiscountPct2, NetAmount, VatCode, VatRatePct, " &
        "VatAmount, MarginPct, CostSnapshotGBP, DespatchedQty, CancelledQty, Status " &
        "FROM so.SalesOrderLine WHERE CompanyCode = @CompanyCode AND OrderNumber = @OrderNumber " &
        "ORDER BY LineNum"

    Private Const PickLineSql As String =
        "SELECT l.LineNum, l.ItemCode, l.RequiredQty, l.PickedQty, l.ShortQty, l.WarehouseCode, " &
        "l.LocationCode, l.LotCode, l.SerialNo, l.ScheduleNo, l.Status, h.OrderNumber, h.CustomerCode, " &
        "o.LineNum AS OrderLineNum " &
        "FROM so.PickListLine l JOIN so.PickList h ON h.PickListId = l.PickListId " &
        "LEFT JOIN so.SalesOrderLine o ON o.SalesOrderLineId = l.SalesOrderLineId " &
        "WHERE h.CompanyCode = @CompanyCode AND h.PickListRef = @PickListRef ORDER BY l.LineNum"

    ''' <summary>COMMAREA of the last link, so the screen can label every figure it shows.</summary>
    Public Property Area As CommArea

    ' ------------------------------------------------------------------ enquiries (SQL)

    ''' <summary>Order header, or Nothing when the order is not on file in this company.</summary>
    Public Function Header(ByVal orderNumber As String) As DataRow
        If Not Deployed("so.SalesOrder") Then Return Nothing
        Return QueryRow(HeaderSql, "@CompanyCode", Company, "@OrderNumber", orderNumber)
    End Function

    Public Function Lines(ByVal orderNumber As String) As DataTable
        If Not Deployed("so.SalesOrderLine") Then Return New DataTable()
        Return Query(LineSql, "@CompanyCode", Company, "@OrderNumber", orderNumber)
    End Function

    Public Function PickLines(ByVal pickListRef As String) As DataTable
        If Not Deployed("so.PickListLine") Then Return New DataTable()
        Return Query(PickLineSql, "@CompanyCode", Company, "@PickListRef", pickListRef)
    End Function

    ''' <summary>Despatchable pick lists of one order at the signed-on site (same rules as the F2 list).</summary>
    Public Function PicksForOrder(ByVal orderNumber As String) As DataTable
        If Not Deployed("so.PickList") Then Return New DataTable()
        Return Query(PickListPickSql, "@CompanyCode", Company, "@SiteCode", Site,
                     "@Search", "", "@Argument", orderNumber)
    End Function

    ' ------------------------------------------------------------------ price (SO-004)

    ''' <summary>PRICING SELECT or EXPLAIN.  EXPLAIN also fills the ranked candidate table with
    ''' the reason each row was or was not chosen; the screen must be able to show it.</summary>
    Public Function Price(ByVal func As String, ByVal item As String, ByVal customer As String,
                          ByVal uom As String, ByVal currencyCode As String, ByVal qty As Decimal,
                          ByVal orderDate As Date, ByVal docRef As String) As CobolResult
        Dim ca As CommArea = NewArea("PRICINGCA")
        ca.SetString("PR-ITEM-CODE", item)
        ca.SetString("PR-CUSTOMER", customer)
        ca.SetString("PR-UOM-CODE", uom)
        ca.SetString("PR-CURRENCY", If(currencyCode = "", Session.BaseCurrency, currencyCode))
        ca.SetDecimal("PR-QUANTITY", qty)
        ca.SetString("PR-ORDER-DATE", Fmt.Yyyymmdd(orderDate))
        ca.SetString("PR-PRICE-DATE", Fmt.Yyyymmdd(orderDate))
        ca.SetString("PR-DOC-TYPE", "SALESORDER")
        ca.SetString("PR-DOC-REF", docRef)
        Area = ca
        Return Link("PRICING", ca, If(func = "", "SELECT", func))
    End Function

    ''' <summary>The ranked candidates PRICING EXPLAIN put back.  Empty when the last link was
    ''' not an EXPLAIN or the engine carries a shorter copybook.</summary>
    Public Function Candidates() As DataTable
        If Area Is Nothing OrElse Not Area.Has("PR-CAND") Then Return New DataTable()
        Return Area.GetTable("PR-CAND", Area.GetInt("PR-CAND-CNT"))
    End Function

    ' ------------------------------------- discount, VAT and margin chain (SO-005, SO-006, SO-025)

    ''' <summary>SODISC LINE, DOCUMENT or RECALC over a grid of order lines.  The sequence and
    ''' the arithmetic are the engine's - the screen never multiplies a discount itself.</summary>
    Public Function Discount(ByVal func As String, ByVal docRef As String, ByVal customer As String,
                             ByVal currencyCode As String, ByVal priceDate As Date,
                             ByVal lines As DataTable) As CobolResult
        Dim ca As CommArea = NewArea("SODISCCA")
        ca.SetString("SD-DOC-TYPE", "SALESORDER")
        ca.SetString("SD-DOC-REF", docRef)
        ca.SetString("SD-CUSTOMER", customer)
        ca.SetString("SD-CURRENCY", If(currencyCode = "", Session.BaseCurrency, currencyCode))
        ca.SetString("SD-PRICE-DATE", Fmt.Yyyymmdd(priceDate))
        ca.SetInt("SD-ROUND-PLACES", 2)
        Dim n As Integer = 0
        If lines IsNot Nothing Then
            For Each r As DataRow In lines.Rows
                If n >= MaxLines Then Exit For
                n += 1
                AddDiscountLine(ca, n, r)
            Next
        End If
        ca.SetInt("SD-LINE-CNT", n)
        Area = ca
        Return Link("SODISC", ca, If(func = "", "DOCUMENT", func))
    End Function

    ''' <summary>Writes one SODISC occurrence (1 based, COBOL style) from an order line row.
    ''' A missing unit conversion is left at the row's own value - never assumed to be one.</summary>
    Public Sub AddDiscountLine(ByVal ca As CommArea, ByVal index As Integer, ByVal r As DataRow)
        Dim per As Decimal = Fmt.Dec(Col(r, "PricePerQty", 1D))
        If per = 0D Then per = 1D
        ca.SetInt("SD-L-LINE-NUM(" & index & ")", Fmt.Int(Col(r, "LineNum", index)))
        ca.SetString("SD-L-ITEM-CODE(" & index & ")", Fmt.Str(Col(r, "ItemCode", "")))
        ca.SetDecimal("SD-L-ENT-QTY(" & index & ")", Fmt.Dec(Col(r, "AcceptedQty", 0D)))
        ca.SetString("SD-L-ENT-UOM(" & index & ")", Fmt.Str(Col(r, "UomCode", "")))
        ca.SetString("SD-L-PRICE-UOM(" & index & ")", Fmt.Str(Col(r, "PriceUomCode", Fmt.Str(Col(r, "UomCode", "")))))
        ca.SetDecimal("SD-L-FACTOR(" & index & ")", Fmt.Dec(Col(r, "ConversionFactor", 0D)))
        ca.SetDecimal("SD-L-UNIT-PRICE(" & index & ")", Fmt.Dec(Col(r, "UnitPrice", 0D)))
        ca.SetDecimal("SD-L-PER-QTY(" & index & ")", per)
        ca.SetDecimal("SD-L-DISC-PCT1(" & index & ")", Fmt.Dec(Col(r, "DiscountPct1", 0D)))
        ca.SetDecimal("SD-L-DISC-PCT2(" & index & ")", Fmt.Dec(Col(r, "DiscountPct2", 0D)))
        ca.SetString("SD-L-VAT-CODE(" & index & ")", Fmt.Str(Col(r, "VatCode", "")))
        ca.SetDecimal("SD-L-VAT-RATE(" & index & ")", Fmt.Dec(Col(r, "VatRatePct", 0D)))
        ca.SetDecimal("SD-L-COST-GBP(" & index & ")", Fmt.Dec(Col(r, "CostSnapshotGBP", 0D)))
    End Sub

    ''' <summary>SOVAT DECIDE, EXPLAIN, EVIDENCE or OVERRIDE: the liability, the mechanism, the
    ''' evidence state and the wording the invoice must carry.</summary>
    Public Function Vat(ByVal func As String, ByVal docRef As String, ByVal lineNum As Integer,
                        ByVal item As String, ByVal customer As String, ByVal vatCode As String,
                        ByVal net As Decimal, ByVal currencyCode As String, ByVal supplyDate As Date) As CobolResult
        Dim ca As CommArea = NewArea("SOVATCA")
        ca.SetString("SV-DOC-TYPE", "SALESORDER")
        ca.SetString("SV-DOC-REF", docRef)
        ca.SetInt("SV-LINE-NUM", lineNum)
        ca.SetString("SV-ITEM-CODE", item)
        ca.SetString("SV-LINE-TYPE", "GOODS")
        ca.SetString("SV-CUSTOMER", customer)
        ca.SetString("SV-ITEM-VAT-CODE", vatCode)
        ca.SetDecimal("SV-NET-AMOUNT", net)
        ca.SetString("SV-CURRENCY", If(currencyCode = "", Session.BaseCurrency, currencyCode))
        ca.SetString("SV-SUPPLY-DATE", Fmt.Yyyymmdd(supplyDate))
        ca.SetString("SV-READ-MASTER", "Y")
        Area = ca
        Return Link("SOVAT", ca, If(func = "", "DECIDE", func))
    End Function

    ''' <summary>SOMARGN LINE, CHECK, PROPOSE or EXPLAIN.  Margin is measured on the ex-VAT net
    ''' against an explicitly named comparable cost; a zero denominator comes back UNDEFINED.</summary>
    Public Function Margin(ByVal func As String, ByVal docRef As String, ByVal lineNum As Integer,
                           ByVal item As String, ByVal customer As String, ByVal qty As Decimal,
                           ByVal net As Decimal, ByVal currencyCode As String, ByVal effective As Date) As CobolResult
        Dim ca As CommArea = NewArea("SOMARGNCA")
        ca.SetString("SM-DOC-TYPE", "SALESORDER")
        ca.SetString("SM-DOC-REF", docRef)
        ca.SetInt("SM-LINE-NUM", lineNum)
        ca.SetString("SM-ITEM-CODE", item)
        ca.SetString("SM-CUSTOMER", customer)
        ca.SetString("SM-SITE-CODE", Site)
        ca.SetString("SM-EFF-DATE", Fmt.Yyyymmdd(effective))
        ca.SetDecimal("SM-QUANTITY", qty)
        ca.SetDecimal("SM-NET-AMOUNT", net)
        ca.SetString("SM-CURRENCY", If(currencyCode = "", Session.BaseCurrency, currencyCode))
        ca.SetString("SM-COST-BASIS", "STANDARD")
        Area = ca
        Return Link("SOMARGN", ca, If(func = "", "LINE", func))
    End Function

    ' ---------------------------------------- acceptance and release (SO-002, SO-003, SO-007, SO-011)

    ''' <summary>SOACCEPT VALIDATE (no business effect), ACCEPT or RELEASE.  VALIDATE returns the
    ''' whole blocker list at once, so an incomplete draft can never look accepted.</summary>
    Public Function Accept(ByVal func As String, ByVal orderNumber As String, ByVal customer As String,
                           ByVal orderDate As Date, ByVal currencyCode As String, ByVal terms As String,
                           ByVal salesperson As String, ByVal incotermsRule As String,
                           ByVal incotermsPlace As String, ByVal deliveryRef As String) As CobolResult
        Dim ca As CommArea = NewArea("SOACCEPTCA")
        ca.SetString("SA-ORDER-NUMBER", orderNumber)
        ca.SetString("SA-CUSTOMER", customer)
        ca.SetString("SA-ORDER-TYPE", "STANDARD")
        ca.SetString("SA-ORDER-DATE", Fmt.Yyyymmdd(orderDate))
        ca.SetString("SA-CURRENCY", If(currencyCode = "", Session.BaseCurrency, currencyCode))
        ca.SetString("SA-TERMS-CODE", terms)
        ca.SetString("SA-SALESPERSON", salesperson)
        ca.SetString("SA-INCOTERMS-RULE", incotermsRule)
        ca.SetString("SA-INCOTERMS-PLACE", incotermsPlace)
        ca.SetString("SA-DELIVERY-REF", deliveryRef)
        ca.SetString("SA-PRINT-ONLY", "N")
        Area = ca
        If func = "VALIDATE" Then Return Link("SOACCEPT", ca, func)
        Return Link("SOACCEPT", ca, If(func = "", "ACCEPT", func), Session.NewCorrelationId())
    End Function

    ''' <summary>The findings SOACCEPT raised, each with its severity (BLOCKER or WARNING),
    ''' reason code, line number, field and text.</summary>
    Public Function Blockers() As DataTable
        If Area Is Nothing OrElse Not Area.Has("SA-BLOCKERS") Then Return New DataTable()
        Return Area.GetTable("SA-BLOCKERS", Area.GetInt("SA-BLOCKER-CNT"))
    End Function

    ' ------------------------------------------- availability and reservation (SO-009, SO-010)

    ''' <summary>SOAVAIL ENQUIRE: free stock, reservations, expected receipts and planned supply,
    ''' each on its own labelled line.  Only physical free stock is ever flagged guaranteed.</summary>
    Public Function Availability(ByVal item As String, ByVal siteCode As String, ByVal uom As String,
                                 ByVal qty As Decimal, ByVal requiredDate As Date) As CobolResult
        Dim ca As CommArea = NewArea("SOAVAILCA")
        ca.SetString("SA-ITEM-CODE", item)
        ca.SetString("SA-SITE", If(siteCode = "", Site, siteCode))
        ca.SetString("SA-UOM-CODE", uom)
        ca.SetDecimal("SA-QTY-REQUIRED", qty)
        ca.SetString("SA-REQUIRED-DATE", Fmt.Yyyymmdd(requiredDate))
        ca.SetString("SA-AS-AT", Fmt.Yyyymmdd(Session.BusinessDate))
        ca.SetInt("SA-HORIZON-DAYS", 90)
        ca.SetString("SA-WITH-RECEIPTS", "Y")
        ca.SetString("SA-WITH-KIT", "N")
        Area = ca
        Return Link("SOAVAIL", ca, "ENQUIRE")
    End Function

    ''' <summary>The labelled supply lines SOAVAIL returned (FREESTOCK, RESERVED, EXPECTED,
    ''' PLANNED, KITBUILD, SHORTFALL).  They are kept apart on the screen, never added up.</summary>
    Public Function SupplyLines() As DataTable
        If Area Is Nothing OrElse Not Area.Has("SA-LINES") Then Return New DataTable()
        Return Area.GetTable("SA-LINES", Area.GetInt("SA-LINE-CNT"))
    End Function

    ''' <summary>SORESV RESERVE - the only instruction that stops another order taking the units.</summary>
    Public Function Reserve(ByVal orderNumber As String, ByVal lineNum As Integer, ByVal item As String,
                            ByVal siteCode As String, ByVal qty As Decimal, ByVal requiredDate As Date,
                            ByVal customer As String) As CobolResult
        Dim ca As CommArea = NewArea("SORESVCA")
        ca.SetString("SV-ORDER-NUMBER", orderNumber)
        ca.SetInt("SV-LINE-NUM", lineNum)
        ca.SetString("SV-ITEM-CODE", item)
        ca.SetString("SV-SITE", If(siteCode = "", Site, siteCode))
        ca.SetString("SV-CUSTOMER", customer)
        ca.SetString("SV-DEMAND-TYPE", "SALESORDER")
        ca.SetString("SV-DEMAND-REF", orderNumber)
        ca.SetInt("SV-DEMAND-LINE", lineNum)
        ca.SetDecimal("SV-QTY-REQUESTED", qty)
        ca.SetString("SV-REQUIRED-DATE", Fmt.Yyyymmdd(requiredDate))
        ca.SetString("SV-ALLOW-PARTIAL", "Y")
        Area = ca
        Return Link("SORESV", ca, "RESERVE", Session.NewCorrelationId())
    End Function

    ' ---------------------------------------------- picking and despatch (SO-012, SO-013, SO-022)

    ''' <summary>PICKGEN GENERATE or CONFIRM.  Picking is not despatch: confirming a pick records
    ''' what was found, it does not issue the stock.</summary>
    Public Function Pick(ByVal func As String, ByVal orderNumber As String, ByVal pickListRef As String,
                         ByVal siteCode As String, ByVal requiredDate As Date) As CobolResult
        Dim ca As CommArea = NewArea("PICKGENCA")
        ca.SetString("PK-ORDER-NUMBER", orderNumber)
        ca.SetString("PK-PICKLIST-REF", pickListRef)
        ca.SetString("PK-SITE", If(siteCode = "", Site, siteCode))
        ca.SetString("PK-REQUIRED-DATE", Fmt.Yyyymmdd(requiredDate))
        ca.SetString("PK-PICKER-USER", User)
        Area = ca
        Return Link("PICKGEN", ca, If(func = "", "GENERATE", func), Session.NewCorrelationId())
    End Function

    ''' <summary>Builds the DESPATCH COMMAREA.  Nothing is confirmed until the caller links with
    ''' CONFIRM, and a CONFIRM that fails leaves no despatch reference to show.</summary>
    Public Function NewDespatch(ByVal orderNumber As String, ByVal pickListRef As String, ByVal customer As String,
                                ByVal despatchDate As Date, ByVal carrier As String, ByVal carrierRef As String,
                                ByVal packs As Integer, ByVal weightKg As Decimal, ByVal incotermsRule As String,
                                ByVal incotermsPlace As String, ByVal isExport As Boolean, ByVal destination As String,
                                ByVal eori As String, ByVal declarationRef As String) As CommArea
        Dim ca As CommArea = NewArea("DESPATCHCA")
        ca.SetString("DS-ORDER-NUMBER", orderNumber)
        ca.SetString("DS-PICKLIST-REF", pickListRef)
        ca.SetString("DS-CUSTOMER", customer)
        ca.SetString("DS-SITE", Site)
        ca.SetString("DS-DESPATCH-DATE", Fmt.Yyyymmdd(despatchDate))
        ca.SetString("DS-POSTING-DATE", Fmt.Yyyymmdd(Session.BusinessDate))
        ca.SetString("DS-CARRIER-CODE", carrier)
        ca.SetString("DS-CARRIER-REF", carrierRef)
        ca.SetInt("DS-PACK-COUNT", packs)
        ca.SetDecimal("DS-GROSS-WEIGHT", weightKg)
        ca.SetString("DS-INCOTERMS", incotermsRule)
        ca.SetString("DS-INCOTERMS-PLACE", incotermsPlace)
        ca.SetString("DS-IS-EXPORT", If(isExport, "Y", "N"))
        ca.SetString("DS-DEST-COUNTRY", destination)
        ca.SetString("DS-CUSTOMER-EORI", eori)
        ca.SetString("DS-EXPORT-DECL-REF", declarationRef)
        ca.SetInt("DS-LINE-CNT", 0)
        Area = ca
        Return ca
    End Function

    ''' <summary>Writes one despatch line: what was picked and what is actually going out.</summary>
    Public Sub AddDespatchLine(ByVal ca As CommArea, ByVal index As Integer, ByVal orderLine As Integer,
                               ByVal item As String, ByVal description As String, ByVal picked As Decimal,
                               ByVal despatched As Decimal, ByVal uom As String, ByVal lot As String,
                               ByVal serial As String, Optional ByVal schedule As Integer = 0,
                               Optional ByVal warehouse As String = "", Optional ByVal location As String = "")
        ca.SetInt("DS-L-LINE-NUM(" & index & ")", index)
        ca.SetInt("DS-L-SCHEDULE(" & index & ")", schedule)
        ca.SetString("DS-L-WHSE(" & index & ")", warehouse)
        ca.SetString("DS-L-LOCATION(" & index & ")", location)
        ca.SetInt("DS-L-ORDER-LINE(" & index & ")", orderLine)
        ca.SetString("DS-L-ITEM(" & index & ")", item)
        ca.SetString("DS-L-DESC(" & index & ")", description)
        ca.SetDecimal("DS-L-PICKED(" & index & ")", picked)
        ca.SetDecimal("DS-L-QTY(" & index & ")", despatched)
        ca.SetString("DS-L-UOM(" & index & ")", uom)
        ca.SetString("DS-L-LOT(" & index & ")", lot)
        ca.SetString("DS-L-SERIAL(" & index & ")", serial)
        ca.SetInt("DS-LINE-CNT", index)
    End Sub

    ''' <summary>DESPATCH CONFIRM, REVERSE or ENQUIRE.  A confirmation is repeatable work, so it
    ''' travels under a correlation id (COM-015); an enquiry has no effect and needs none.</summary>
    Public Function Despatch(ByVal ca As CommArea, ByVal func As String) As CobolResult
        Area = ca
        If func = "ENQUIRE" Then Return Link("DESPATCH", ca, func)
        Return Link("DESPATCH", ca, func, Session.NewCorrelationId())
    End Function

    ' --------------------------------------------------------- batch runs (SO-014, RPT-008, RPT-016)

    ''' <summary>Runs SOINVRUN through the framework batch launcher.  PREVIEW has no business
    ''' effect; only COMMIT bills anything (SO-014).</summary>
    Public Function InvoiceRun(ByVal companyCode As String, ByVal siteCode As String, ByVal fromDate As Date,
                               ByVal toDate As Date, ByVal mode As String) As ReportRun
        Dim a As New Dictionary(Of String, String)
        a("CO") = companyCode
        a("SITE") = siteCode
        a("FROM") = Fmt.Yyyymmdd(fromDate)
        a("TO") = Fmt.Yyyymmdd(toDate)
        a("USER") = User
        a("MODE") = If(mode = "", "PREVIEW", mode)
        Return New ReportService().Run("SOINVRUN", "SOINVRUN", a)
    End Function

    ''' <summary>Runs the sales backlog and fulfilment report SOBACKLG (RPT-008).</summary>
    Public Function BacklogReport(ByVal companyCode As String, ByVal siteCode As String,
                                  ByVal fromDate As Date, ByVal toDate As Date) As ReportRun
        Dim a As New Dictionary(Of String, String)
        a("CO") = companyCode
        a("SITE") = siteCode
        a("FROM") = Fmt.Yyyymmdd(fromDate)
        a("TO") = Fmt.Yyyymmdd(toDate)
        Return New ReportService().Run("RPT-008", "SOBACKLG", a)
    End Function

    ' ------------------------------------------------------------------ display shaping

    ''' <summary>Money in the currency it was stated in - a foreign amount never gets a pound
    ''' sign and sterling always does (COM-024).</summary>
    Public Shared Function Amount(ByVal value As Decimal, ByVal currencyCode As String) As String
        If currencyCode = "" OrElse currencyCode = "GBP" Then Return Fmt.Money(value)
        Return Fmt.Money(value, currencyCode)
    End Function

    ''' <summary>A COMMAREA money field in sterling, or "-" when the field is not in the layout.</summary>
    Public Shared Function AreaMoney(ByVal ca As CommArea, ByVal field As String) As String
        If ca Is Nothing OrElse Not ca.Has(field) Then Return "-"
        Return Fmt.Money(ca.GetDecimal(field))
    End Function

    ''' <summary>A COMMAREA PIC 9(8) date as dd/MM/yyyy, or "-" when it is unset.</summary>
    Public Shared Function AreaDate(ByVal ca As CommArea, ByVal field As String) As String
        If ca Is Nothing OrElse Not ca.Has(field) Then Return "-"
        Dim raw As String = ca.GetString(field)
        If raw = "" OrElse raw = "00000000" Then Return "-"
        Dim d As Date? = Fmt.ParseDate(raw)
        Return If(d.HasValue, Fmt.Date(d.Value), raw)
    End Function

    ''' <summary>A COMMAREA text field, or "" when the engine carries a shorter copybook.</summary>
    Public Shared Function AreaText(ByVal ca As CommArea, ByVal field As String) As String
        If ca Is Nothing OrElse Not ca.Has(field) Then Return ""
        Return ca.GetString(field).Trim()
    End Function

    ''' <summary>A state shown in words, never by colour alone: "CREDITHOLD - over limit by
    ''' £1,250.00" rather than a red box (COM-024).</summary>
    Public Shared Function StatusText(ByVal status As String, ByVal reason As String) As String
        Dim s As String = If(status, "").Trim()
        Dim r As String = If(reason, "").Trim()
        If s = "" Then s = "(not set)"
        If r = "" Then Return s
        Return s & " - " & r
    End Function

End Class
