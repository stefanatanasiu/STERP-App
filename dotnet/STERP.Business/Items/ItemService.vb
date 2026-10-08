Imports System.Collections.Generic
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Item and part maintenance facade.  Searching, validation, copying, lifecycle impact and
''' compliance decisions all belong to the COBOL services and to the generic maintenance
''' contract; nothing here decides anything.  Items are retired, never deleted.
''' REQ: ITM-001, ITM-002, ITM-009, ITM-010, ITM-020, ITM-021, ITM-025, ITM-026
''' </summary>
Public Class ItemService
    Inherits ServiceBase

    Private ReadOnly _maint As New MaintService()

    Public Property Faults As DataTable
    Public Property Impact As DataTable
    Public Property Preview As DataTable
    Public Property Blocking As Boolean

    ''' <summary>Code, description and partner alias search (ITEMVAL SEARCH, ITM-001/ITM-009).</summary>
    Public Function Search(ByVal text As String) As DataTable
        Dim ca As CommArea = NewArea("ITEMVALCA")
        ca.SetString("IV-SEARCH-TEXT", text)
        Dim r As CobolResult = Link("ITEMVAL", ca, "SEARCH")
        If r.Succeeded Then
            Dim t As DataTable = Rows(ca, "IV-MATCHES", ca.GetInt("IV-MATCH-CNT"))
            If t IsNot Nothing Then Return t
        End If
        Return Query("SELECT TOP (200) ItemCode, Description, LifecycleStatus, MatchSource, Alias, PartnerCode " &
                     "FROM inv.vw_ItemAlias WHERE CompanyCode = @c AND (ItemCode LIKE @t OR Alias LIKE @t " &
                     "OR Description LIKE @t) ORDER BY ItemCode", "@c", Company, "@t", "%" & text & "%")
    End Function

    ''' <summary>The governed catalogue identity of one item.</summary>
    Public Function Header(ByVal item As String) As DataRow
        Return QueryRow("SELECT * FROM inv.Item WHERE CompanyCode = @c AND ItemCode = @i",
                        "@c", Company, "@i", item)
    End Function

    ''' <summary>One tab of the item master.  Balances and costs are never editable here.</summary>
    Public Function Part(ByVal kind As String, ByVal item As String) As DataTable
        Dim sql As String
        Select Case kind
            Case "REVISION"
                sql = "SELECT Revision, Description, Status, EffectiveFrom, EffectiveTo, IsInterchangeable, " &
                      "DrawingRef, ChangeReason, ApprovedBy, ApprovedAt FROM inv.ItemRevision"
            Case "UOM"
                sql = "SELECT UomCode, UsageType, StockUnitsPerUom, PackDescription, EffectiveFrom, EffectiveTo FROM inv.ItemUom"
            Case "SUPPLY"
                sql = "SELECT SiteCode, DefaultWarehouseCode, DefaultLocationCode, MakeBuy, LeadTimeDays, MinOrderQty, " &
                      "PurchaseMultipleQty, ReorderPointQty, SafetyStockQty, MaxStockQty, AbcClass FROM inv.ItemSite"
            Case "PARTNER"
                sql = "SELECT PartnerType, PartnerCode, PartnerItemCode, PartnerDescription, UomCode, QtyBreak, " &
                      "CurrencyCode, UnitPrice, LeadTimeDays, PreferenceRank, IsApprovedSource, EffectiveFrom, " &
                      "EffectiveTo FROM inv.ItemPartner"
            Case "TRACE"
                sql = "SELECT LotCode, Revision, SupplierLotRef, SupplierCode, ManufacturedDate, ReceivedDate, " &
                      "ExpiryDate, QualityStatus, CertificateRef, OriginCountry FROM inv.Lot"
            Case "VAT"
                sql = "SELECT Direction, VatCode, EffectiveFrom, EffectiveTo, EvidenceRef, ApprovedBy FROM inv.ItemVatClass"
            Case "CUSTOMS"
                sql = "SELECT EffectiveFrom, CommodityCode, CountryOfOrigin, PreferentialOrigin, PreferenceEvidenceRef, " &
                      "DutyRatePct, NetWeightKgPerStockUnit FROM inv.ItemCustoms"
            Case "PACKAGING"
                sql = "SELECT PackagingMaterial, PackagingType, WeightGrams, RecycledContentPct, NationOfSupply, " &
                      "IsPlasticPackagingTax FROM inv.ItemPackaging"
            Case Else
                sql = "SELECT SubstituteItemCode, ConversionRatio, ContextPurchase, ContextSales, ContextKit, " &
                      "ContextBom, RequiresCustomerAgreement, Restrictions, ValidFrom, ValidTo FROM inv.ItemSubstitute"
        End Select
        Return Query(sql & " WHERE CompanyCode = @c AND ItemCode = @i", "@c", Company, "@i", item)
    End Function

    ''' <summary>Activation review or release (ITEMVAL VALIDATE / ACTIVATE, ITM-010).</summary>
    Public Function Review(ByVal item As String, ByVal func As String) As CobolResult
        Dim ca As CommArea = NewArea("ITEMVALCA")
        ca.SetString("IV-ITEM-CODE", item)
        ca.SetString("IV-ACTION", func)
        Dim r As CobolResult = Link("ITEMVAL", ca, func, Session.NewCorrelationId())
        Faults = Rows(ca, "IV-FAULTS", ca.GetInt("IV-FAULT-CNT"))
        Return r
    End Function

    ''' <summary>What still depends on the item before it is blocked, superseded or retired (ITM-020).</summary>
    Public Function Lifecycle(ByVal item As String, ByVal func As String, ByVal successor As String,
                              ByVal reason As String) As CobolResult
        Dim ca As CommArea = NewArea("ITMIMPCTCA")
        ca.SetString("IM-ITEM-CODE", item)
        ca.SetString("IM-NEW-STATUS", func)
        ca.SetString("IM-SUCCESSOR", successor)
        ca.SetString("IM-REASON", reason)
        ca.SetString("IM-DISPOSITION", reason)
        Dim r As CobolResult = Link("ITMIMPCT", ca, func, Session.NewCorrelationId())
        Blocking = (ca.GetString("IM-BLOCKING") = "Y")
        Impact = Rows(ca, "IM-LINES", ca.GetInt("IM-LINE-CNT"))
        Return r
    End Function

    ''' <summary>Copies an item and the related data the user asked for (ITEMCOPY, ITM-021).</summary>
    Public Function Copy(ByVal source As String, ByVal target As String, ByVal description As String,
                         ByVal commit As Boolean) As CobolResult
        Dim ca As CommArea = NewArea("ITEMCOPYCA")
        ca.SetString("IC-SOURCE-ITEM", source)
        ca.SetString("IC-TARGET-ITEM", target)
        ca.SetString("IC-TARGET-DESC", description)
        For Each f As String In New String() {"IC-INCLUDE-UOM", "IC-INCLUDE-SITE", "IC-INCLUDE-VAT",
                                              "IC-INCLUDE-CUSTOMS", "IC-INCLUDE-PACKAGE", "IC-INCLUDE-PARTNER",
                                              "IC-INCLUDE-SUBST", "IC-INCLUDE-REVISION"}
            ca.SetString(f, "Y")
        Next
        Dim r As CobolResult = Link("ITEMCOPY", ca, If(commit, "COPY", "PREVIEW"), Session.NewCorrelationId())
        Preview = Rows(ca, "IC-PREVIEWS", ca.GetInt("IC-PREVIEW-CNT"))
        Return r
    End Function

    ''' <summary>VAT, customs and packaging obligations for a tax point (ITMCOMPL, ITM-025..ITM-027).</summary>
    Public Function Compliance(ByVal item As String, ByVal direction As String) As CobolResult
        Dim ca As CommArea = NewArea("ITMCOMPLCA")
        ca.SetString("CO-ITEM-CODE", item)
        ca.SetString("CO-DIRECTION", direction)
        ca.SetDate("CO-TAX-POINT", Session.BusinessDate)
        Return Link("ITMCOMPL", ca, "COMPLY")
    End Function

    ''' <summary>Audited header save; RETIRE clears the in-use flag, there is no hard delete (ITM-022).</summary>
    Public Function Save(ByVal mode As String, ByVal values As Dictionary(Of String, String)) As String
        Return _maint.Save("ITM.ITEM", mode, values)
    End Function

    Friend Shared Function Rows(ByVal ca As CommArea, ByVal name As String, ByVal n As Integer) As DataTable
        If n <= 0 Then Return Nothing
        Return ca.GetTable(name, n)
    End Function

End Class
