Imports System.Collections.Generic
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Goods receipt and inspection facade.  VALIDATE reads the outstanding order lines into the
''' COMMAREA, the screen writes the counted quantities back into the same buffer and CONFIRM
''' posts it - the old pseudo-conversational habit.  RECEIPT owns ownership, tolerance and the
''' GRNI posting; INSPECT owns quarantine, acceptance, rejection and concession.
''' REQ: PO-009, PO-010, PO-011, PO-014, PO-025
''' </summary>
Public Class GoodsReceiptService
    Inherits ServiceBase

    Private _area As CommArea

    ''' <summary>
    ''' The outstanding lines of an order, as the receipt screen needs them.  RECEIPT reads
    ''' nothing itself - the screen hands it the order and RECEIPT judges it - so this is the
    ''' only read in the goods inward path.
    ''' </summary>
    Private Const OutstandingLines As String =
        "SELECT TOP 20 l.LineNum, l.ItemCode, ISNULL(l.Revision,'') AS Revision, " &
        "ISNULL(l.Description,'') AS Description, l.OrderedQty, l.CancelledQty, l.ReceivedQty, " &
        "l.UomCode, l.ConversionFactor, l.UnitPrice, " &
        "ISNULL(l.OverDeliveryTolerancePct,0) AS OverDeliveryTolerancePct, " &
        "ScheduleNo = ISNULL((SELECT TOP 1 s.ScheduleNo FROM po.PurchaseSchedule s " &
        "WHERE s.PurchaseOrderLineId = l.PurchaseOrderLineId AND s.Status IN ('OPEN','PARTIAL') " &
        "ORDER BY ISNULL(s.CurrentConfirmedDate, s.RequestedDate), s.ScheduleNo), 1), " &
        "Promised = (SELECT TOP 1 ISNULL(s.CurrentConfirmedDate, s.RequestedDate) FROM po.PurchaseSchedule s " &
        "WHERE s.PurchaseOrderLineId = l.PurchaseOrderLineId AND s.Status IN ('OPEN','PARTIAL') " &
        "ORDER BY ISNULL(s.CurrentConfirmedDate, s.RequestedDate), s.ScheduleNo), " &
        "Traceability = ISNULL(i.TraceabilityType,'NONE'), " &
        "NeedsInspection = ISNULL(i.RequiresInspection,0), " &
        "IsStocked = CASE WHEN l.LineType IN ('SERVICE','CHARGE','TEXT') THEN 0 ELSE ISNULL(i.IsStocked,1) END, " &
        "DefaultLocation = COALESCE(NULLIF(l.DestinationLocationCode, ''), " &
        "(SELECT TOP 1 loc.LocationCode FROM inv.Location loc WHERE loc.CompanyCode = l.CompanyCode " &
        "AND loc.SiteCode = @s AND loc.IsActive = 1 AND loc.LocationType IN ('GOODSIN','STOCK') " &
        "ORDER BY CASE WHEN loc.LocationType = 'GOODSIN' THEN 0 ELSE 1 END, loc.LocationCode), '') " &
        "FROM po.PurchaseOrderLine l " &
        "LEFT JOIN inv.Item i ON i.CompanyCode = l.CompanyCode AND i.ItemCode = l.ItemCode " &
        "WHERE l.CompanyCode = @c AND l.OrderNumber = @o AND l.Status NOT IN ('CANCELLED','SHORTCLOSED') " &
        "AND l.OrderedQty - l.CancelledQty - l.ReceivedQty > 0 ORDER BY l.LineNum"

    Public Property ReceiptRef As String = ""
    Public Property Status As String = ""
    Public Property TotalGBP As Decimal
    Public Property GrniGBP As Decimal
    Public Property PpvGBP As Decimal
    Public Property JournalNumber As String = ""
    Public Property LinesAccepted As Integer
    Public Property LinesHeld As Integer
    Public Property LinesQuarantined As Integer
    Public Property WasDuplicate As Boolean
    Public Property Inspection As CommArea

    ''' <summary>Pulls the outstanding lines of an order into the COMMAREA (PO-009, PO-014).</summary>
    Public Function Validate(ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = NewArea("RECEIPTCA")
        ca.SetString("RC-ORDER-NUMBER", PoMap.S(v, "ORDER"))
        ca.SetString("RC-SUPPLIER", PoMap.S(v, "SUPPLIER"))
        ca.SetString("RC-DELIVERY-NOTE", PoMap.T(v, "DELNOTE"))
        ca.SetDate("RC-PHYSICAL-DATE", PoMap.Dt(v, "PHYSDATE", Session.BusinessDate))
        ca.SetDate("RC-POSTING-DATE", PoMap.Dt(v, "POSTDATE", Session.BusinessDate))
        ca.SetString("RC-WAREHOUSE", PoMap.S(v, "WHS"))
        ca.SetString("RC-OWNERSHIP", PoMap.S(v, "OWNERSHIP"))
        ca.SetString("RC-INCOTERM", PoMap.S(v, "INCO"))
        ca.SetString("RC-SHIP-EVIDENCE", PoMap.T(v, "EVIDENCE"))
        ca.SetString("RC-IMPORT-ENTRY", PoMap.S(v, "IMPORTENTRY"))
        ca.SetString("RC-SUBCONTRACT-JOB", PoMap.S(v, "JOB"))
        ca.SetString("RC-DUP-POLICY", If(PoMap.S(v, "DUPPOLICY") = "", "WARN", PoMap.S(v, "DUPPOLICY")))
        ca.SetString("RC-RECEIVED-BY", User)
        LoadLines(ca, v)
        Dim r As CobolResult = Link("RECEIPT", ca, "VALIDATE")
        _area = ca
        Read(ca)
        Return r
    End Function

    ''' <summary>
    ''' Fills the COMMAREA with what is still outstanding on the order, defaulting this
    ''' delivery to the whole of it - the lorry brought what was ordered until the receiver
    ''' says otherwise.  RECEIPT then decides ownership, tolerance and traceability
    ''' (PO-009, PO-014, PO-025).
    ''' </summary>
    Private Sub LoadLines(ByVal ca As CommArea, ByVal v As Dictionary(Of String, String))
        Dim t As DataTable = Query(OutstandingLines, "@c", Company, "@o", PoMap.S(v, "ORDER"), "@s", Site)
        Dim n As Integer = Math.Min(t.Rows.Count, 20)
        ca.SetInt("RC-LINE-CNT", n)
        For i As Integer = 0 To n - 1
            Dim row As DataRow = t.Rows(i)
            Dim j As String = "(" & (i + 1) & ")"
            Dim outstanding As Decimal = Fmt.Dec(row("OrderedQty")) - Fmt.Dec(row("CancelledQty")) -
                                         Fmt.Dec(row("ReceivedQty"))
            ca.SetInt("RC-L-NUM" & j, i + 1)
            ca.SetInt("RC-L-PO-LINE" & j, Fmt.Int(row("LineNum")))
            ca.SetInt("RC-L-SCHEDULE" & j, Fmt.Int(row("ScheduleNo")))
            ca.SetString("RC-L-ITEM" & j, Fmt.Str(row("ItemCode")))
            ca.SetString("RC-L-REVISION" & j, Fmt.Str(row("Revision")))
            ca.SetString("RC-L-DESC" & j, Fmt.Str(row("Description")))
            ca.SetDecimal("RC-L-ORDERED" & j, Fmt.Dec(row("OrderedQty")))
            ca.SetDecimal("RC-L-CANCELLED" & j, Fmt.Dec(row("CancelledQty")))
            ' RC-L-COUNTED is what the line has already had; RC-L-RECEIVED is this delivery
            ca.SetDecimal("RC-L-COUNTED" & j, Fmt.Dec(row("ReceivedQty")))
            ca.SetDecimal("RC-L-RECEIVED" & j, outstanding)
            ca.SetString("RC-L-UOM" & j, Fmt.Str(row("UomCode")))
            ca.SetDecimal("RC-L-FACTOR" & j, If(Fmt.Dec(row("ConversionFactor")) = 0D, 1D,
                                                Fmt.Dec(row("ConversionFactor"))))
            ca.SetDecimal("RC-L-PRICE" & j, Fmt.Dec(row("UnitPrice")))
            ca.SetDecimal("RC-L-OVER-PCT" & j, Fmt.Dec(row("OverDeliveryTolerancePct")))
            ca.SetString("RC-L-TRACE" & j, Fmt.Str(row("Traceability")))
            ca.SetString("RC-L-INSPECT" & j, If(Fmt.Bool(row("NeedsInspection")), "Y", "N"))
            ca.SetString("RC-L-STOCKED" & j, If(Fmt.Bool(row("IsStocked")), "Y", "N"))
            ca.SetString("RC-L-LOCATION" & j, If(PoMap.S(v, "LOC") = "", Fmt.Str(row("DefaultLocation")),
                                                 PoMap.S(v, "LOC")))
            ' A lot or serial traceable line cannot be validated without its traceability, so a
            ' caller that already knows it (a hand-held terminal, a batch) may offer it up front.
            If PoMap.S(v, "LOT") <> "" Then ca.SetString("RC-L-LOT" & j, PoMap.S(v, "LOT"))
            If PoMap.S(v, "SERIAL") <> "" Then ca.SetString("RC-L-SERIAL" & j, PoMap.S(v, "SERIAL"))
            If Not IsDBNull(row("Promised")) Then ca.SetDate("RC-L-PROMISED" & j, Convert.ToDateTime(row("Promised")))
        Next
    End Sub

    ''' <summary>The validated lines, one row per outstanding order line.</summary>
    Public Function Lines() As DataTable
        If _area Is Nothing Then Return New DataTable("ReceiptLines")
        Return _area.GetTable("RC-LINE", Math.Max(_area.GetInt("RC-LINE-CNT"), 0))
    End Function

    ''' <summary>
    ''' Writes one counted quantity back into the validated COMMAREA.  RECEIPT judges
    ''' RC-L-RECEIVED - the quantity on this delivery - against RC-L-COUNTED, which is what the
    ''' order line has already had, so the count goes into RC-L-RECEIVED.
    ''' </summary>
    Public Sub Count(ByVal index As Integer, ByVal counted As Decimal, ByVal location As String,
                     ByVal lot As String, ByVal serial As String)
        If _area Is Nothing Then Return
        Dim i As String = "(" & index & ")"
        _area.SetDecimal("RC-L-RECEIVED" & i, counted)
        If location <> "" Then _area.SetString("RC-L-LOCATION" & i, location)
        If lot <> "" Then _area.SetString("RC-L-LOT" & i, lot)
        If serial <> "" Then _area.SetString("RC-L-SERIAL" & i, serial)
    End Sub

    ''' <summary>CONFIRM posts the receipt, TRANSIT books it in transit (PO-009, PO-011, COM-015).</summary>
    Public Function Confirm(ByVal func As String) As CobolResult
        If _area Is Nothing Then
            Dim bad As New CobolResult()
            bad.Program = "RECEIPT"
            bad.ReturnCode = 8
            bad.ReasonCode = "RCP0000"
            bad.Message = "The delivery has not been validated against an order yet."
            Return bad
        End If
        Dim r As CobolResult = Link("RECEIPT", _area, func, Session.NewCorrelationId())
        Read(_area)
        Return r
    End Function

    ''' <summary>Reversal of a posted receipt with its reason (PO-009).</summary>
    Public Function Reverse(ByVal receiptRef As String, ByVal reason As String) As CobolResult
        Dim ca As CommArea = NewArea("RECEIPTCA")
        ca.SetString("RC-REVERSE-REF", receiptRef)
        ca.SetString("RC-REVERSE-REASON", reason)
        ca.SetString("RC-RECEIVED-BY", User)
        ca.SetDate("RC-POSTING-DATE", Session.BusinessDate)
        Dim r As CobolResult = Link("RECEIPT", ca, "REVERSE", Session.NewCorrelationId())
        _area = ca
        Read(ca)
        Return r
    End Function

    ''' <summary>Inspection disposition of a quarantined receipt line (PO-010).</summary>
    Public Function Inspect(ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = NewArea("INSPECTCA")
        ca.SetString("IN-RECEIPT-REF", If(PoMap.S(v, "RECEIPT") = "", ReceiptRef, PoMap.S(v, "RECEIPT")))
        ca.SetInt("IN-RECEIPT-LINE", PoMap.I(v, "LINE"))
        ca.SetString("IN-ITEM-CODE", PoMap.S(v, "ITEM"))
        ca.SetString("IN-LOT-CODE", PoMap.S(v, "LOT"))
        ca.SetString("IN-SERIAL-NO", PoMap.S(v, "SERIAL"))
        ca.SetString("IN-WAREHOUSE", PoMap.S(v, "WHS"))
        ca.SetString("IN-LOCATION", PoMap.S(v, "LOC"))
        ca.SetString("IN-GOOD-LOCATION", PoMap.S(v, "GOODLOC"))
        ca.SetString("IN-UOM-CODE", PoMap.S(v, "UOM"))
        ca.SetString("IN-INSPECTOR", User)
        ca.SetDate("IN-INSPECT-DATE", Session.BusinessDate)
        ca.SetDecimal("IN-INSPECTED-QTY", PoMap.D(v, "INSPECTED"))
        ca.SetDecimal("IN-ACCEPTED-QTY", PoMap.D(v, "ACCEPTED"))
        ca.SetDecimal("IN-REJECTED-QTY", PoMap.D(v, "REJECTED"))
        ca.SetDecimal("IN-REWORK-QTY", PoMap.D(v, "REWORK"))
        ca.SetString("IN-DISPOSITION", PoMap.S(v, "DISPOSITION"))
        ca.SetString("IN-REJECT-REASON", PoMap.S(v, "REASONCODE"))
        ca.SetString("IN-REJECT-NARR", PoMap.T(v, "REASON"))
        ca.SetString("IN-CONCESSION-REF", PoMap.T(v, "CONCESSION"))
        ca.SetString("IN-CONCESSION-BY", PoMap.S(v, "CONCESSIONBY"))
        ca.SetString("IN-CERT-REF", PoMap.T(v, "CERT"))
        Dim r As CobolResult = Link("INSPECT", ca, If(PoMap.S(v, "FUNC") = "", "RECORD", PoMap.S(v, "FUNC")),
                                    Session.NewCorrelationId())
        Inspection = ca
        Return r
    End Function

    Private Sub Read(ByVal ca As CommArea)
        If ca.GetString("RC-RECEIPT-REF") <> "" Then ReceiptRef = ca.GetString("RC-RECEIPT-REF")
        Status = ca.GetString("RC-STATUS")
        TotalGBP = ca.GetDecimal("RC-TOTAL-GBP")
        GrniGBP = ca.GetDecimal("RC-GRNI-GBP")
        PpvGBP = ca.GetDecimal("RC-PPV-GBP")
        JournalNumber = ca.GetString("RC-JOURNAL-NUMBER")
        LinesAccepted = ca.GetInt("RC-LINES-ACCEPTED")
        LinesHeld = ca.GetInt("RC-LINES-HELD")
        LinesQuarantined = ca.GetInt("RC-LINES-QUARANT")
        WasDuplicate = (ca.GetString("RC-WAS-DUPLICATE") = "Y")
    End Sub
End Class
