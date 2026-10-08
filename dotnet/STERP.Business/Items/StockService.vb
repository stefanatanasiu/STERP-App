Imports System.Collections.Generic
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Stock facade: enquiry, availability buckets, movement posting, reservations, lot tracing
''' and counting.  A displayed balance is never edited - every change is an authorised
''' movement posted by STKMOVE / inv.usp_PostStockMovement.
''' REQ: ITM-012, ITM-013, ITM-014, ITM-015, ITM-016, ITM-017, ITM-018
''' </summary>
Public Class StockService
    Inherits ServiceBase

    Private ReadOnly _maint As New MaintService()

    Public Property Lines As DataTable
    Public Property Physical As Decimal
    Public Property Eligible As Decimal
    Public Property ReservedQty As Decimal
    Public Property FreeQty As Decimal
    Public Property ExpectedQty As Decimal
    Public Property PlannedQty As Decimal
    Public Property MovementId As Decimal
    Public Property QtyAfter As Decimal

    ''' <summary>Stock by site, location, lot, owner and quality condition (ITM-012).</summary>
    Public Function Enquiry(ByVal item As String, ByVal site As String, ByVal condition As String,
                            ByVal includeZero As Boolean) As DataTable
        Return Query("inv.usp_StockEnquiry", "@CompanyCode", Company, "@ItemCode", item, "@SiteCode", site,
                     "@Condition", condition, "@IncludeZero", If(includeZero, 1, 0))
    End Function

    ''' <summary>Availability buckets from AVAILQ, the only place the definitions live (ITM-013).</summary>
    Public Function Availability(ByVal item As String, ByVal site As String) As CobolResult
        Dim ca As CommArea = NewArea("AVAILQCA")
        ca.SetString("AV-ITEM-CODE", item)
        ca.SetString("AV-SITE", site)
        ca.SetDate("AV-AS-AT", Session.BusinessDate)
        Dim r As CobolResult = Link("AVAILQ", ca, "AVAIL")
        Physical = ca.GetDecimal("AV-QTY-PHYSICAL")
        Eligible = ca.GetDecimal("AV-QTY-ELIGIBLE")
        ReservedQty = ca.GetDecimal("AV-QTY-RESERVED")
        FreeQty = ca.GetDecimal("AV-QTY-FREE")
        ExpectedQty = ca.GetDecimal("AV-QTY-EXPECTED")
        PlannedQty = ca.GetDecimal("AV-QTY-PLANNED")
        Lines = ItemService.Rows(ca, "AV-LINES", ca.GetInt("AV-LINE-CNT"))
        Return r
    End Function

    ''' <summary>Posts one movement; STKMOVE owns the rules, the transit leg and the messages.</summary>
    Public Function Post(ByVal func As String, ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = NewArea("STKMOVECA")
        ca.SetString("SM-ITEM-CODE", S(v, "ITEM"))
        ca.SetString("SM-REVISION", S(v, "REV"))
        ca.SetString("SM-SITE", If(S(v, "SITE") = "", Site, S(v, "SITE")))
        ca.SetString("SM-WAREHOUSE", S(v, "WHS"))
        ca.SetString("SM-LOCATION", S(v, "LOC"))
        ca.SetString("SM-TO-SITE", S(v, "TOSITE"))
        ca.SetString("SM-TO-WAREHOUSE", S(v, "TOWHS"))
        ca.SetString("SM-TO-LOCATION", S(v, "TOLOC"))
        ca.SetString("SM-LOT-CODE", S(v, "LOT"))
        ca.SetString("SM-SERIAL-NO", S(v, "SERIAL"))
        ca.SetString("SM-CONDITION", If(S(v, "COND") = "", "ACCEPTED", S(v, "COND")))
        ca.SetString("SM-TO-CONDITION", S(v, "TOCOND"))
        ca.SetString("SM-UOM-CODE", S(v, "UOM"))
        ca.SetString("SM-REASON-CODE", S(v, "REASONCODE"))
        ca.SetString("SM-REASON", S(v, "REASON"))
        ca.SetString("SM-SOURCE-MODULE", "ITM")
        ca.SetString("SM-SOURCE-DOC-TYPE", S(v, "DOCTYPE"))
        ca.SetString("SM-SOURCE-DOC-REF", S(v, "DOCREF"))
        ca.SetString("SM-VIA-TRANSIT", If(S(v, "TRANSIT") = "Y", "Y", "N"))
        ca.SetString("SM-HOLD-TYPE", S(v, "HOLDTYPE"))
        ca.SetString("SM-HOLD-OUTCOME", S(v, "OUTCOME"))
        ca.SetDecimal("SM-QUANTITY", D(v, "QTY"))
        ca.SetDecimal("SM-UNIT-COST", D(v, "COST"))
        ca.SetDate("SM-MOVEMENT-DATE", AsDate(S(v, "DATE")))
        Dim r As CobolResult = Link("STKMOVE", ca, func, Session.NewCorrelationId())
        MovementId = ca.GetDecimal("SM-MOVEMENT-ID")
        QtyAfter = ca.GetDecimal("SM-QTY-AFTER")
        Return r
    End Function

    ''' <summary>Reserve, release, consume or revalidate against a named demand (ITM-014).</summary>
    Public Function Reserve(ByVal func As String, ByVal item As String, ByVal demandType As String,
                            ByVal demandRef As String, ByVal qty As Decimal, ByVal allowPartial As Boolean) As CobolResult
        Dim ca As CommArea = NewArea("STKRESVCA")
        ca.SetString("SR-ITEM-CODE", item)
        ca.SetString("SR-SITE", Site)
        ca.SetString("SR-DEMAND-TYPE", demandType)
        ca.SetString("SR-DEMAND-REF", demandRef)
        ca.SetString("SR-FIRMNESS", "FIRM")
        ca.SetDecimal("SR-QTY-REQUESTED", qty)
        ca.SetString("SR-ALLOW-PARTIAL", If(allowPartial, "Y", "N"))
        Dim r As CobolResult = Link("STKRESV", ca, func, Session.NewCorrelationId())
        ReservedQty = ca.GetDecimal("SR-QTY-RESERVED")
        FreeQty = ca.GetDecimal("SR-QTY-FREE-AFTER")
        Lines = ItemService.Rows(ca, "SR-LINES", ca.GetInt("SR-LINE-CNT"))
        Return r
    End Function

    ''' <summary>Forward and backward traceability for a lot or a serial number (ITM-016).</summary>
    Public Function Trace(ByVal item As String, ByVal lot As String, ByVal serial As String) As CobolResult
        Dim ca As CommArea = NewArea("LOTSERCA")
        ca.SetString("LS-ITEM-CODE", item)
        ca.SetString("LS-LOT-CODE", lot)
        ca.SetString("LS-SERIAL-NO", serial)
        ca.SetDate("LS-AS-AT", Session.BusinessDate)
        Dim r As CobolResult = Link("LOTSER", ca, "TRACE")
        Lines = ItemService.Rows(ca, "LS-TRACE", ca.GetInt("LS-TRACE-CNT"))
        Return r
    End Function

    ''' <summary>Posted movement history behind the enquiry and the stock status report.</summary>
    Public Function Movements(ByVal item As String, ByVal site As String, ByVal days As Integer) As DataTable
        Return Query("SELECT TOP (500) MovementId, MovementDate, MovementType, SiteCode, WarehouseCode, LocationCode, " &
                     "ItemCode, LotCode, SerialNo, QualityStatus, Quantity, UomCode, UnitCost, ValueGBP, SourceDocType, " &
                     "SourceDocRef, ReasonCode, Reason, CreatedBy FROM inv.StockMovement WHERE CompanyCode = @c " &
                     "AND (@i IS NULL OR ItemCode = @i) AND (@s IS NULL OR SiteCode = @s) " &
                     "AND MovementDate >= DATEADD(day, -@d, @t) ORDER BY MovementId DESC",
                     "@c", Company, "@i", item, "@s", site, "@d", days, "@t", Session.BusinessDate)
    End Function

    ''' <summary>Count headers in scope (ITM-018).</summary>
    Public Function Counts(ByVal status As String) As DataTable
        Return Query("SELECT CountId, CountRef, CountType, SiteCode, WarehouseCode, ScopeDescription, CutoffAt, " &
                     "IsFrozen, Status, CountedBy, ApprovedBy, ApprovedAt FROM inv.StockCount WHERE CompanyCode = @c " &
                     "AND (@s IS NULL OR @s = 'ALL' OR Status = @s) ORDER BY CountRef", "@c", Company, "@s", status)
    End Function

    ''' <summary>Count lines with the book quantity, the counted quantity and the variance.</summary>
    Public Function CountLines(ByVal countRef As String) As DataTable
        Return Query("SELECT l.CountId, l.LineNum, l.ItemCode, l.Revision, l.WarehouseCode, l.LocationCode, l.LotCode, " &
                     "l.SerialNo, l.ExpectedQty, l.CountedQty, l.RecountQty, l.InterveningMovementQty, " &
                     "ISNULL(l.CountedQty, 0) - l.ExpectedQty AS VarianceQty, l.AdjustmentQty, l.AdjustmentValueGBP, " &
                     "l.ReasonCode, l.MovementId, l.Status FROM inv.StockCountLine l JOIN inv.StockCount h " &
                     "ON h.CountId = l.CountId WHERE h.CompanyCode = @c AND h.CountRef = @r ORDER BY l.LineNum",
                     "@c", Company, "@r", countRef)
    End Function

    ''' <summary>Records a counted quantity through the audited maintenance contract.</summary>
    Public Function SaveCount(ByVal countId As Object, ByVal lineNum As Object, ByVal qty As String) As String
        Dim v As New Dictionary(Of String, String)
        v("CountId") = Fmt.Str(countId)
        v("LineNum") = Fmt.Str(lineNum)
        v("CountedQty") = qty
        v("Status") = "COUNTED"
        Return _maint.Save("ITM.COUNTLINE", "CHANGE", v)
    End Function

    Private Shared Function S(ByVal v As Dictionary(Of String, String), ByVal key As String) As String
        If v Is Nothing OrElse Not v.ContainsKey(key) OrElse v(key) Is Nothing Then Return ""
        Return v(key).Trim()
    End Function

    Private Shared Function D(ByVal v As Dictionary(Of String, String), ByVal key As String) As Decimal
        Dim parsed As Decimal
        If Decimal.TryParse(S(v, key), parsed) Then Return parsed
        Return 0D
    End Function

    Private Shared Function AsDate(ByVal text As String) As Date
        Dim d As Date? = Fmt.ParseDate(text)
        If d.HasValue Then Return d.Value
        Return Session.BusinessDate
    End Function

End Class
