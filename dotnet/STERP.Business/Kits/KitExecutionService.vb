Imports System.Data
Imports STERP.CobolInterop
Imports STERP.Common

Namespace Kits

    ''' <summary>
    ''' Facade over the kit execution engines: KITISSUE (material onto the job), GENEALOG
    ''' (component-to-parent traceability), KITCOST (planned against actual), KITCOMM
    ''' (commercial kit sets, allocation and returns) and DISASSY (controlled disassembly).
    ''' The screens read the few values they show through <see cref="Text"/> and
    ''' <see cref="Num"/> so no rule is ever restated in VB.
    ''' REQ: KIT-001, KIT-010, KIT-011, KIT-012, KIT-013, KIT-019, KIT-020, KIT-021, KIT-023
    ''' </summary>
    Public Class KitExecutionService
        Inherits KitFacade

        ''' <summary>Genealogy line window of GENEALOGCA (KIT-019).</summary>
        Public Const GeneaSpec As String =
            "Seq:I=GN-L-SEQ,Depth:I=GN-L-DEPTH,KitRef=GN-L-KIT-REF,Parent=GN-L-PARENT," &
            "ParentLot=GN-L-PAR-LOT,ParentSerial=GN-L-PAR-SERIAL,Item=GN-L-ITEM,Lot=GN-L-LOT," &
            "Serial=GN-L-SERIAL,Quantity:D=GN-L-QTY,Unit=GN-L-UOM,PlannedItem=GN-L-PLANNED," &
            "Substitute=GN-L-SUBSTITUTE,Event=GN-L-EVENT,Happened:T=GN-L-WHEN," &
            "Operator=GN-L-OPERATOR,Note=GN-L-NOTE"

        ''' <summary>Cost element window of KITCOSTCA (KIT-020).</summary>
        Public Const CostSpec As String =
            "Element=KK-E-ELEMENT,Description=KK-E-DESC,Planned:D=KK-E-PLANNED," &
            "Actual:D=KK-E-ACTUAL,Variance:D=KK-E-VARIANCE,Quantity:D=KK-E-QTY,Unit=KK-E-UOM"

        ''' <summary>Commercial kit component window of KITCOMMCA (KIT-023).</summary>
        Public Const CommSpec As String =
            "LineNum:I=KV-L-LINE-NUM,Item=KV-L-ITEM,Description=KV-L-DESC,QtyPer:D=KV-L-QTY-PER," &
            "Unit=KV-L-UOM,Required:D=KV-L-REQUIRED,Free:D=KV-L-FREE,Delivered:D=KV-L-DELIVERED," &
            "Sets:D=KV-L-SETS,StandalonePrice:D=KV-L-SSP,AllocationPct:D=KV-L-ALLOC-PCT," &
            "AllocatedNet:D=KV-L-ALLOC-NET,UnitCost:D=KV-L-UNIT-COST,CostGBP:D=KV-L-COST-GBP," &
            "VatCode=KV-L-VAT-CODE,CustomerSpecified=KV-L-CUST-SPEC,SubstituteOk=KV-L-SUB-OK," &
            "Limiting=KV-L-LIMITING"

        ''' <summary>Recovered component window of DISASSYCA (KIT-021).</summary>
        Public Const DisassySpec As String =
            "LineNum:I=DS-L-LINE-NUM,Item=DS-L-ITEM,Revision=DS-L-REV,Lot=DS-L-LOT," &
            "Serial=DS-L-SERIAL,Planned:D=DS-L-PLANNED,Recovered:D=DS-L-RECOVERED," &
            "Damaged:D=DS-L-DAMAGED,Unit=DS-L-UOM,Quality=DS-L-QUALITY,ValueGBP:D=DS-L-VALUE," &
            "Location=DS-L-LOCATION"

        ' ----------------------------------------------------------- KITISSUE
        ''' <summary>
        ''' Issues one component to a kit, or only tests it when <paramref name="checkOnly"/> is
        ''' set.  Shortage, overissue and substitution all need a reason and the engine decides
        ''' whether the approval is enough (KIT-010, KIT-011, KIT-012).
        ''' </summary>
        Public Function Issue(ByVal kitRef As String, ByVal lineNum As Integer, ByVal item As String,
                              ByVal lot As String, ByVal serial As String, ByVal qty As Decimal,
                              ByVal location As String, ByVal reasonCode As String, ByVal reasonText As String,
                              ByVal allowOver As Boolean, ByVal checkOnly As Boolean) As CobolResult

            Dim ca As CommArea = NewArea("KITISSUECA")
            ca.SetString("KS-KIT-REF", kitRef)
            ca.SetInt("KS-LINE-NUM", lineNum)
            ca.SetString("KS-ITEM-CODE", item)
            ca.SetString("KS-SITE", Site)
            ca.SetString("KS-LOCATION", location)
            ca.SetString("KS-LOT-CODE", lot)
            ca.SetString("KS-SERIAL-NO", serial)
            ca.SetDecimal("KS-QUANTITY", qty)
            ca.SetDate("KS-ACTUAL-DATE", Session.BusinessDate)
            ca.SetDate("KS-POSTING-DATE", Session.BusinessDate)
            ca.SetString("KS-REASON-CODE", reasonCode)
            ca.SetString("KS-REASON-TEXT", reasonText)
            ca.SetString("KS-ALLOW-OVER", If(allowOver, "Y", "N"))
            ca.SetString("KS-CHECK-ONLY", If(checkOnly, "Y", "N"))
            _ca = ca
            _lines = Nothing
            If checkOnly Then Return Link("KITISSUE", ca, "CHECK")
            Return Link("KITISSUE", ca, "ISSUE", Session.NewCorrelationId())
        End Function

        ''' <summary>
        ''' Backflush of the non-lot, non-serial components of an explicitly flagged kit.  The
        ''' engine runs a full check pass first so it can never half succeed (KIT-010, KIT-013).
        ''' </summary>
        Public Function Backflush(ByVal kitRef As String, ByVal goodQty As Decimal) As CobolResult
            Dim ca As CommArea = NewArea("KITISSUECA")
            ca.SetString("KS-KIT-REF", kitRef)
            ca.SetString("KS-SITE", Site)
            ca.SetDecimal("KS-GOOD-QTY", goodQty)
            ca.SetDate("KS-ACTUAL-DATE", Session.BusinessDate)
            ca.SetDate("KS-POSTING-DATE", Session.BusinessDate)
            _ca = ca
            _lines = Nothing
            Return Link("KITISSUE", ca, "BACKFLSH", Session.NewCorrelationId())
        End Function

        ' ----------------------------------------------------------- GENEALOG
        ''' <summary>
        ''' Traceability in either direction.  FORWARD follows a lot into the parents it built,
        ''' BACKWARD opens a parent up, WHEREUSD answers the recall question.  Substitutions,
        ''' returns and scrap stay visible (KIT-019).
        ''' </summary>
        Public Function Genealogy(ByVal direction As String, ByVal kitRef As String, ByVal item As String,
                                  ByVal lot As String, ByVal serial As String, ByVal maxDepth As Integer) As CobolResult

            Dim ca As CommArea = NewArea("GENEALOGCA")
            ca.SetString("GN-DIRECTION", direction)
            ca.SetString("GN-KIT-REF", kitRef)
            ca.SetString("GN-ITEM-CODE", item)
            ca.SetString("GN-LOT-CODE", lot)
            ca.SetString("GN-SERIAL-NO", serial)
            ca.SetInt("GN-MAX-DEPTH", maxDepth)
            ca.SetString("GN-INCLUDE-RET", "Y")
            ca.SetString("GN-INCLUDE-SCRAP", "Y")
            ca.SetInt("GN-FROM-LINE", 0)
            ca.SetInt("GN-WIN-MAX", 20)
            _ca = ca
            _lines = NewTable(GeneaSpec)
            Dim fn As String = direction.Trim().ToUpperInvariant()
            If fn <> "BACKWARD" AndAlso fn <> "WHEREUSD" Then fn = "FORWARD"
            Dim r As CobolResult = Link("GENEALOG", ca, fn)
            Return Page(ca, "GN", _lines, GeneaSpec, r, Function() Link("GENEALOG", ca, fn))
        End Function

        ''' <summary>The recall list straight out of the database, for an export (KIT-019).</summary>
        Public Function GenealogyList(ByVal item As String, ByVal lot As String, ByVal serial As String,
                                      ByVal maxDepth As Integer) As DataTable
            If Not Deployed("kit.usp_KitGenealogy") Then Return New DataTable()
            Return Query("kit.usp_KitGenealogy", "@CompanyCode", Company, "@ItemCode", item,
                         "@LotCode", Opt(lot), "@SerialNo", Opt(serial), "@MaxDepth", maxDepth)
        End Function

        ' ------------------------------------------------------------ KITCOST
        ''' <summary>
        ''' Planned, issued, returned, scrapped, absorbed, completed and residual value of a
        ''' kit, with the element breakdown.  A made subassembly is charged once (KIT-020).
        ''' </summary>
        Public Function Cost(ByVal kitRef As String, ByVal costSet As String, ByVal varianceOnly As Boolean) As CobolResult
            Dim ca As CommArea = NewArea("KITCOSTCA")
            ca.SetString("KK-KIT-REF", kitRef)
            ca.SetString("KK-COST-SET", costSet)
            ca.SetDate("KK-AS-AT", Session.BusinessDate)
            ca.SetString("KK-CURRENCY", "GBP")
            ca.SetString("KK-DOUBLE-CHECK", "Y")
            _ca = ca
            _lines = NewTable(CostSpec)
            Dim r As CobolResult = Link("KITCOST", ca, If(varianceOnly, "VARIANCE", "COST"))
            If r.Succeeded Then r = Link("KITCOST", ca, "GETELEM")
            If r.Succeeded Then Copy(ca, _lines, CostSpec, ca.GetInt("KK-ELEM-CNT"))
            Return r
        End Function

        ' ------------------------------------------------------------ KITCOMM
        ''' <summary>
        ''' Commercial kit fulfilment.  SETS proves how many whole sets were delivered,
        ''' ALLOCATE spreads the parent net value over the components, FULFIL tests whether the
        ''' parent may be despatched and RETURNCK prices a component return (KIT-001, KIT-023).
        ''' </summary>
        Public Function Commercial(ByVal func As String, ByVal kitItem As String, ByVal version As String,
                                   ByVal orderRef As String, ByVal orderLine As Integer, ByVal parentQty As Decimal,
                                   ByVal parentNet As Decimal, ByVal returnLine As Integer,
                                   ByVal returnQty As Decimal) As CobolResult

            Dim ca As CommArea = NewArea("KITCOMMCA")
            ca.SetString("KV-KIT-ITEM", kitItem)
            ca.SetString("KV-KIT-VERSION", version)
            ca.SetString("KV-SITE", Site)
            ca.SetString("KV-ORDER-REF", orderRef)
            ca.SetInt("KV-ORDER-LINE", orderLine)
            ca.SetDecimal("KV-PARENT-QTY", parentQty)
            ca.SetDecimal("KV-PARENT-NET", parentNet)
            ca.SetString("KV-CURRENCY", "GBP")
            ca.SetInt("KV-RETURN-LINE", returnLine)
            ca.SetDecimal("KV-RETURN-QTY", returnQty)
            ca.SetDate("KV-AS-AT", Session.BusinessDate)
            _ca = ca
            _lines = NewTable(CommSpec)
            Dim r As CobolResult = Link("KITCOMM", ca, func, Session.NewCorrelationId())
            If r.Succeeded Then Copy(ca, _lines, CommSpec, ca.GetInt("KV-LINE-CNT"))
            Return r
        End Function

        ' ------------------------------------------------------------ DISASSY
        ''' <summary>
        ''' One step of a controlled disassembly: CREATE the note, ADDLINE a recovered
        ''' component, VALUE the recovery, APPROVE it, POST the movements or ENQUIRE.  The
        ''' engine refuses to recover more value than the parent plus approved costs (KIT-021).
        ''' </summary>
        Public Function Disassembly(ByVal func As String, ByVal disassyRef As String, ByVal parentItem As String,
                                    ByVal revision As String, ByVal lot As String, ByVal serial As String,
                                    ByVal consumedQty As Decimal, ByVal location As String,
                                    ByVal reason As String, ByVal line As DataRow) As CobolResult

            Dim ca As CommArea = NewArea("DISASSYCA")
            ca.SetString("DS-DISASSY-REF", disassyRef)
            ca.SetString("DS-SITE", Site)
            ca.SetString("DS-PARENT-ITEM", parentItem)
            ca.SetString("DS-REVISION", revision)
            ca.SetString("DS-LOT-CODE", lot)
            ca.SetString("DS-SERIAL-NO", serial)
            ca.SetDecimal("DS-CONSUMED-QTY", consumedQty)
            ca.SetString("DS-LOCATION", location)
            ca.SetDate("DS-DISASSY-DATE", Session.BusinessDate)
            ca.SetDate("DS-POSTING-DATE", Session.BusinessDate)
            ca.SetString("DS-ALLOC-BASIS", "STANDARD")
            ca.SetString("DS-REASON-TEXT", reason)
            ca.SetString("DS-APPROVED-BY", User)
            If line IsNot Nothing Then
                ca.SetInt("DS-A-LINE-NUM", Fmt.Int(Col(line, "LineNum", 0)))
                ca.SetString("DS-A-ITEM", Fmt.Str(Col(line, "Item", "")))
                ca.SetString("DS-A-REV", Fmt.Str(Col(line, "Revision", "")))
                ca.SetString("DS-A-LOT", Fmt.Str(Col(line, "Lot", "")))
                ca.SetString("DS-A-SERIAL", Fmt.Str(Col(line, "Serial", "")))
                ca.SetDecimal("DS-A-PLANNED", Fmt.Dec(Col(line, "Planned", 0D)))
                ca.SetDecimal("DS-A-RECOVERED", Fmt.Dec(Col(line, "Recovered", 0D)))
                ca.SetDecimal("DS-A-DAMAGED", Fmt.Dec(Col(line, "Damaged", 0D)))
                ca.SetString("DS-A-UOM", Fmt.Str(Col(line, "Unit", "")))
                ca.SetString("DS-A-QUALITY", Fmt.Str(Col(line, "Quality", "INSPECTION")))
                ca.SetString("DS-A-LOCATION", Fmt.Str(Col(line, "Location", "")))
            End If
            ca.SetInt("DS-FROM-LINE", 0)
            _ca = ca
            _lines = NewTable(DisassySpec)
            Dim r As CobolResult = Link("DISASSY", ca, func, Session.NewCorrelationId())
            Return Page(ca, "DS", _lines, DisassySpec, r, Function() Link("DISASSY", ca, "ENQUIRE"))
        End Function

    End Class

End Namespace
