Imports System.Data
Imports STERP.CobolInterop

Namespace Kits

    ''' <summary>
    ''' Facade over the kit definition and feasibility engines.  KITCREAT owns the execution kit
    ''' (create, review, release, enquire, kind check) and KITTRIAL owns the non-committing trial.
    ''' This class fills the COMMAREA, calls the gateway and shapes the answer for a screen; the
    ''' revision freeze, completeness and buildable quantity all stay in COBOL.
    ''' REQ: KIT-001, KIT-002, KIT-003, KIT-004, KIT-006, KIT-008, KIT-022, KIT-024
    ''' </summary>
    Public Class KitService
        Inherits KitFacade

        ''' <summary>Kit line window of KITCREATCA - requirements against actuals (KIT-003).</summary>
        Public Const LineSpec As String =
            "LineNum:I=KC-L-LINE-NUM,Path=KC-L-PATH,Item=KC-L-ITEM,Revision=KC-L-REV," &
            "Description=KC-L-DESC,Basis=KC-L-BASIS,Unit=KC-L-UOM,Original:D=KC-L-ORIG-REQ," &
            "Required:D=KC-L-CURR-REQ,ScrapPct:D=KC-L-SCRAP-PCT,Reserved:D=KC-L-RESERVED," &
            "Picked:D=KC-L-PICKED,Issued:D=KC-L-NET-ISS,Returned:D=KC-L-RETURNED," &
            "Scrapped:D=KC-L-SCRAPPED,Outstanding:D=KC-L-REMAINING,Excess:D=KC-L-EXCESS," &
            "Substitute=KC-L-SUBST,NeedDate:T=KC-L-NEED-DATE,Location=KC-L-LOCATION," &
            "UnitCost:D=KC-L-UNIT-COST,Status=KC-L-STATUS"

        ''' <summary>Trial line window of KITTRIALCA (KIT-006).</summary>
        Public Const TrialSpec As String =
            "LineNum:I=KT-L-LINE-NUM,Depth:I=KT-L-DEPTH,Path=KT-L-PATH,Item=KT-L-ITEM," &
            "Revision=KT-L-REV,Unit=KT-L-UOM,Gross:D=KT-L-GROSS,Required:D=KT-L-NET-REQ," &
            "Free:D=KT-L-FREE,OwnReserved:D=KT-L-OWN-RESV,OtherReserved:D=KT-L-OTHER-RESV," &
            "Inbound:D=KT-L-INBOUND,InboundDate:T=KT-L-INB-DATE,Allocated:D=KT-L-ALLOCATED," &
            "Shortage:D=KT-L-SHORTAGE,ShortDate:T=KT-L-SHORT-DATE,Limiting=KT-L-LIMITING," &
            "Action=KT-L-ACTION,Warning=KT-L-WARNING"

        ''' <summary>Kit worklist with its readiness to pick (KIT-008, KIT-022).</summary>
        Public Function Worklist(ByVal site As String, ByVal status As String, ByVal kitRef As String) As DataTable
            If Not Deployed("kit.usp_KitReadiness") Then Return New DataTable()
            Return Query("kit.usp_KitReadiness", "@CompanyCode", Company, "@SiteCode", Opt(site),
                         "@Status", Opt(status), "@KitRef", Opt(kitRef))
        End Function

        ' ----------------------------------------------------------- KITCREAT
        ''' <summary>Creates an execution kit from an approved bill or job requirement (KIT-002).</summary>
        Public Function Create(ByVal kitRef As String, ByVal kind As String, ByVal parentItem As String,
                               ByVal revision As String, ByVal site As String, ByVal plannedQty As Decimal,
                               ByVal needDate As Date, ByVal sourceType As String, ByVal sourceRef As String,
                               ByVal backflush As Boolean) As CobolResult

            Dim ca As CommArea = Header(kitRef, kind, site)
            ca.SetString("KC-PARENT-ITEM", parentItem)
            ca.SetString("KC-PARENT-REV", revision)
            ca.SetDecimal("KC-PLANNED-QTY", plannedQty)
            ca.SetDate("KC-NEED-DATE", needDate)
            ca.SetDate("KC-SELECTION-DATE", Session.BusinessDate)
            ca.SetString("KC-SOURCE-TYPE", sourceType)
            ca.SetString("KC-SOURCE-REF", sourceRef)
            ca.SetString("KC-BACKFLUSH", If(backflush, "Y", "N"))
            ca.SetString("KC-OWNER-USER", User)
            ca.SetString("KC-EXPAND-PHANTOM", "Y")
            Return Link("KITCREAT", ca, "CREATE", Session.NewCorrelationId())
        End Function

        ''' <summary>Pre-release review of requirements and shortages (KIT-002, KIT-022).</summary>
        Public Function Review(ByVal kitRef As String) As CobolResult
            Return Link("KITCREAT", Header(kitRef, "", Site), "REVIEW")
        End Function

        ''' <summary>Release freezes revisions, units and calculation assumptions (KIT-004).</summary>
        Public Function Release(ByVal kitRef As String) As CobolResult
            Dim ca As CommArea = Header(kitRef, "", Site)
            ca.SetString("KC-OWNER-USER", User)
            Return Link("KITCREAT", ca, "RELEASE", Session.NewCorrelationId())
        End Function

        ''' <summary>Header plus every kit line, paged through GETLINE (KIT-003, KIT-022).</summary>
        Public Function Enquire(ByVal kitRef As String) As CobolResult
            Dim ca As CommArea = Header(kitRef, "", Site)
            ca.SetInt("KC-FROM-LINE", 0)
            ca.SetInt("KC-WIN-MAX", 20)
            _lines = NewTable(LineSpec)
            Dim r As CobolResult = Link("KITCREAT", ca, "ENQUIRE")
            Return Page(ca, "KC", _lines, LineSpec, r, Function() Link("KITCREAT", ca, "GETLINE"))
        End Function

        ''' <summary>
        ''' Answers whether this kind of parent may be received as finished stock.  A commercial
        ''' kit parent always answers "N" - its sets are proved by KITCOMM (KIT-001, KIT-024).
        ''' </summary>
        Public Function KindCheck(ByVal kitRef As String, ByVal kind As String, ByVal parentItem As String) As CobolResult
            Dim ca As CommArea = Header(kitRef, kind, Site)
            ca.SetString("KC-PARENT-ITEM", parentItem)
            Return Link("KITCREAT", ca, "KINDCHK")
        End Function

        ' ----------------------------------------------------------- KITTRIAL
        ''' <summary>
        ''' Trial kitting: feasibility, shortages and the limiting requirement for a stated
        ''' quantity and date.  Nothing is reserved, issued or posted (KIT-006).
        ''' </summary>
        Public Function Trial(ByVal kitRef As String, ByVal parentItem As String, ByVal revision As String,
                              ByVal site As String, ByVal qty As Decimal, ByVal requiredBy As Date,
                              ByVal useInbound As Boolean, ByVal useSubassembly As Boolean) As CobolResult

            Dim ca As CommArea = TrialArea(kitRef, parentItem, revision, site, qty, requiredBy,
                                           useInbound, useSubassembly)
            _lines = NewTable(TrialSpec)
            Dim r As CobolResult = Link("KITTRIAL", ca, "TRIAL")
            Return Page(ca, "KT", _lines, TrialSpec, r, Function() Link("KITTRIAL", ca, "GETLINE"))
        End Function

        ''' <summary>How many whole parents the eligible stock supports, and what limits it (KIT-006).</summary>
        Public Function MaxBuild(ByVal kitRef As String, ByVal parentItem As String, ByVal revision As String,
                                 ByVal site As String, ByVal requiredBy As Date, ByVal useInbound As Boolean) As CobolResult
            Dim ca As CommArea = TrialArea(kitRef, parentItem, revision, site, 0D, requiredBy, useInbound, False)
            Return Link("KITTRIAL", ca, "MAXBUILD")
        End Function

        ' ------------------------------------------------------------ private
        Private Function Header(ByVal kitRef As String, ByVal kind As String, ByVal site As String) As CommArea
            Dim ca As CommArea = NewArea("KITCREATCA")
            ca.SetString("KC-KIT-REF", kitRef)
            ca.SetString("KC-KIT-KIND", kind)
            ca.SetString("KC-SITE", site)
            _ca = ca
            _lines = Nothing
            Return ca
        End Function

        Private Function TrialArea(ByVal kitRef As String, ByVal parentItem As String, ByVal revision As String,
                                   ByVal site As String, ByVal qty As Decimal, ByVal requiredBy As Date,
                                   ByVal useInbound As Boolean, ByVal useSubassembly As Boolean) As CommArea
            Dim ca As CommArea = NewArea("KITTRIALCA")
            ca.SetString("KT-KIT-REF", kitRef)
            ca.SetString("KT-PARENT-ITEM", parentItem)
            ca.SetString("KT-PARENT-REV", revision)
            ca.SetString("KT-SITE", site)
            ca.SetDecimal("KT-REQUESTED-QTY", qty)
            ca.SetDate("KT-REQ-DATE", requiredBy)
            ca.SetDate("KT-SELECTION-DATE", Session.BusinessDate)
            ca.SetString("KT-USE-INBOUND", If(useInbound, "Y", "N"))
            ca.SetString("KT-USE-SUBASSY", If(useSubassembly, "Y", "N"))
            ca.SetString("KT-COMMITTED", "N")
            ca.SetInt("KT-FROM-LINE", 0)
            ca.SetInt("KT-WIN-MAX", 20)
            _ca = ca
            _lines = Nothing
            Return ca
        End Function

    End Class

End Namespace
