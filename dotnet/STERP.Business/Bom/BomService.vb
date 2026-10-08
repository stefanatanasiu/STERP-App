Imports System.Data
Imports STERP.Data
Imports STERP.CobolInterop

Namespace Bom

    ''' <summary>
    ''' Facade over the bill of material engine.  Every calculation is done by the COBOL programs -
    ''' this class only fills the COMMAREA, calls the gateway and turns the answer into a DataTable
    ''' for the forms.  Enquiries that are pure set operations (where-used, revision comparison,
    ''' release) go to the stored procedures instead.
    ''' REQ: BOM-014, BOM-020, BOM-021, BOM-023, BOM-025, BOM-032
    ''' </summary>
    Public Class BomService
        Inherits ServiceBase

        Private _completeness As String = ""
        Private _stopSummary As String = ""
        Private _occurrences As Integer = 0
        Private _depthReached As Integer = 0
        Private _maxBuildable As Decimal = 0D
        Private _shortages As Integer = 0
        Private _totalCost As Decimal = 0D
        Private _unitCost As Decimal = 0D
        Private _materialCost As Decimal = 0D
        Private _conversionCost As Decimal = 0D
        Private _recoveryCredit As Decimal = 0D
        Private _missingRates As Integer = 0
        Private _validationOutcome As String = ""
        Private _validationErrors As Integer = 0
        Private _validationWarnings As Integer = 0
        Private _releaseOk As Boolean = False
        Private _cyclePath As String = ""

        Public ReadOnly Property TotalCost() As Decimal
            Get
                Return _totalCost
            End Get
        End Property

        Public ReadOnly Property UnitCost() As Decimal
            Get
                Return _unitCost
            End Get
        End Property

        Public ReadOnly Property MaterialCost() As Decimal
            Get
                Return _materialCost
            End Get
        End Property

        Public ReadOnly Property ConversionCost() As Decimal
            Get
                Return _conversionCost
            End Get
        End Property

        Public ReadOnly Property RecoveryCredit() As Decimal
            Get
                Return _recoveryCredit
            End Get
        End Property

        Public ReadOnly Property MissingRates() As Integer
            Get
                Return _missingRates
            End Get
        End Property

        Public ReadOnly Property ValidationOutcome() As String
            Get
                Return _validationOutcome
            End Get
        End Property

        Public ReadOnly Property ValidationErrors() As Integer
            Get
                Return _validationErrors
            End Get
        End Property

        Public ReadOnly Property ValidationWarnings() As Integer
            Get
                Return _validationWarnings
            End Get
        End Property

        Public ReadOnly Property ReleaseAllowed() As Boolean
            Get
                Return _releaseOk
            End Get
        End Property

        Public ReadOnly Property CyclePath() As String
            Get
                Return _cyclePath
            End Get
        End Property

        Public ReadOnly Property Completeness() As String
            Get
                Return _completeness
            End Get
        End Property

        Public ReadOnly Property StopSummary() As String
            Get
                Return _stopSummary
            End Get
        End Property

        Public ReadOnly Property Occurrences() As Integer
            Get
                Return _occurrences
            End Get
        End Property

        Public ReadOnly Property DepthReached() As Integer
            Get
                Return _depthReached
            End Get
        End Property

        Public ReadOnly Property MaxBuildable() As Decimal
            Get
                Return _maxBuildable
            End Get
        End Property

        Public ReadOnly Property Shortages() As Integer
            Get
                Return _shortages
            End Get
        End Property

        ''' <summary>Single level, indented or leaf view of one explosion (BOM-014).</summary>
        Public Function Explode(ByVal itemCode As String, ByVal revision As String, ByVal qty As Decimal,
                                ByVal effectiveDate As Date, ByVal view As String, ByVal maxDepth As Integer,
                                ByRef rows As DataTable) As CobolResult

            Dim ca As CommArea = NewArea("BOMLEAFCA")
            ca.SetString("BL-PARENT-ITEM", itemCode)
            ca.SetString("BL-PARENT-REV", revision)
            ca.SetString("BL-PURPOSE", "STANDARD")
            ca.SetDecimal("BL-REQ-QTY", qty)
            ca.SetInt("BL-EFF-DATE", CInt(effectiveDate.ToString("yyyyMMdd")))
            ca.SetInt("BL-MAX-DEPTH", maxDepth)
            ca.SetString("BL-REEXPLODE", "Y")
            ca.SetInt("BL-FROM-LINE", 0)
            ca.SetInt("BL-WIN-MAX", 25)

            rows = NewExplosionTable()
            Dim fn As String = ViewToFunction(view)
            Dim r As CobolResult = Link("BOMLEAF", ca, fn)
            If Not r.Succeeded Then Return r

            _completeness = ca.GetString("BL-COMPLETENESS")
            _stopSummary = ca.GetString("BL-STOP-SUMMARY")
            _occurrences = ca.GetInt("BL-OCC-COUNT")
            _depthReached = ca.GetInt("BL-DEPTH-REACHED")

            ' BOMLEAF leaves BL-FROM-LINE on the next unread occurrence, so it is passed back as is
            Dim more As Boolean = True
            Dim guard As Integer = 0
            While more AndAlso guard < 100
                guard += 1
                Dim count As Integer = ca.GetInt("BL-WIN-CNT")
                For i As Integer = 1 To count
                    Dim d As DataRow = rows.NewRow()
                    d("Level") = ca.GetInt("BL-L-DEPTH(" & i & ")")
                    d("Item") = ca.GetString("BL-L-ITEM(" & i & ")")
                    d("Revision") = ca.GetString("BL-L-REV(" & i & ")")
                    d("Description") = ca.GetString("BL-L-DESC(" & i & ")")
                    d("Kind") = ca.GetString("BL-L-NODE-TYPE(" & i & ")")
                    d("Supply") = ca.GetString("BL-L-SUPPLY(" & i & ")")
                    d("Basis") = ca.GetString("BL-L-BASIS(" & i & ")")
                    d("Usage") = ca.GetDecimal("BL-L-USAGE(" & i & ")")
                    d("Unit") = ca.GetString("BL-L-UOM(" & i & ")")
                    d("Required") = ca.GetDecimal("BL-L-REQ-QTY(" & i & ")")
                    d("Unrounded") = ca.GetDecimal("BL-L-UNROUNDED(" & i & ")")
                    d("StopReason") = ca.GetString("BL-L-STOP(" & i & ")")
                    d("Path") = ca.GetString("BL-L-PATH(" & i & ")")
                    d("Warning") = ca.GetString("BL-L-WARNING(" & i & ")")
                    rows.Rows.Add(d)
                Next
                more = (ca.GetString("BL-MORE-LINES") = "Y")
                If more Then
                    ca.SetString("BL-REEXPLODE", "N")
                    r = Link("BOMLEAF", ca, fn)
                    If Not r.Succeeded Then Exit While
                End If
            End While
            Return r
        End Function

        ''' <summary>Netted plan: eligible stock and inbound supply are consumed once (BOM-020).</summary>
        Public Function NettedPlan(ByVal itemCode As String, ByVal qty As Decimal, ByVal effectiveDate As Date,
                                   ByRef rows As DataTable) As CobolResult

            Dim ca As CommArea = NewArea("BOMNETCA")
            ca.SetString("BN-PARENT-ITEM", itemCode)
            ca.SetString("BN-PURPOSE", "STANDARD")
            ca.SetDecimal("BN-REQ-QTY", qty)
            ca.SetInt("BN-EFF-DATE", CInt(effectiveDate.ToString("yyyyMMdd")))
            ca.SetInt("BN-MAX-DEPTH", 20)
            ca.SetString("BN-STOCK-SOURCE", "LIVE")
            ca.SetInt("BN-FROM-LINE", 0)
            ca.SetInt("BN-WIN-MAX", 25)

            rows = NewNettedTable()
            Dim r As CobolResult = Link("BOMNET", ca, "NET")
            If Not r.Succeeded Then Return r
            _maxBuildable = ca.GetDecimal("BN-MAX-BUILDABLE")
            _shortages = ca.GetInt("BN-SHORTAGE-CNT")
            _completeness = ca.GetString("BN-COMPLETENESS")

            ' BOMNET leaves BN-FROM-LINE on the next unread occurrence
            Dim more As Boolean = True
            Dim guard As Integer = 0
            While more AndAlso guard < 100
                guard += 1
                r = Link("BOMNET", ca, "GETLINE")
                If Not r.Succeeded Then Exit While
                Dim count As Integer = ca.GetInt("BN-WIN-CNT")
                For i As Integer = 1 To count
                    Dim d As DataRow = rows.NewRow()
                    d("Level") = ca.GetInt("BN-L-DEPTH(" & i & ")")
                    d("Item") = ca.GetString("BN-L-ITEM(" & i & ")")
                    d("Description") = ca.GetString("BN-L-DESC(" & i & ")")
                    d("Unit") = ca.GetString("BN-L-UOM(" & i & ")")
                    d("Required") = ca.GetDecimal("BN-L-REQ-QTY(" & i & ")")
                    d("StockUsed") = ca.GetDecimal("BN-L-STOCK-USED(" & i & ")")
                    d("Inbound") = ca.GetDecimal("BN-L-INBOUND(" & i & ")")
                    d("ToMake") = ca.GetDecimal("BN-L-TO-MAKE(" & i & ")")
                    d("Shortage") = ca.GetDecimal("BN-L-SHORTAGE(" & i & ")")
                    d("Excess") = ca.GetDecimal("BN-L-EXCESS(" & i & ")")
                    d("StopReason") = ca.GetString("BN-L-STOP(" & i & ")")
                    rows.Rows.Add(d)
                Next
                more = (ca.GetString("BN-MORE-LINES") = "Y")
            End While
            Return r
        End Function

        ''' <summary>Costed explosion: one value boundary per branch, never added twice (BOM-021).</summary>
        Public Function CostedExplosion(ByVal itemCode As String, ByVal revision As String, ByVal qty As Decimal,
                                        ByVal effectiveDate As Date, ByVal costSet As String,
                                        ByRef rows As DataTable) As CobolResult

            Dim ca As CommArea = NewArea("BOMCOSTCA")
            ca.SetString("BC-PARENT-ITEM", itemCode)
            ca.SetString("BC-PARENT-REV", revision)
            ca.SetString("BC-PURPOSE", "STANDARD")
            ca.SetDecimal("BC-REQ-QTY", qty)
            ca.SetInt("BC-EFF-DATE", CInt(effectiveDate.ToString("yyyyMMdd")))
            ca.SetInt("BC-COST-DATE", CInt(effectiveDate.ToString("yyyyMMdd")))
            ca.SetInt("BC-MAX-DEPTH", 0)
            ca.SetString("BC-COST-SET", costSet)
            ca.SetString("BC-CURRENCY", "GBP")
            ca.SetString("BC-NETTED", "N")
            ca.SetInt("BC-FROM-LINE", 0)
            ca.SetInt("BC-WIN-MAX", 25)

            rows = NewCostedTable()
            Dim r As CobolResult = Link("BOMCOST", ca, "COST")
            If Not r.Succeeded Then Return r

            _completeness = ca.GetString("BC-COMPLETENESS")
            _occurrences = ca.GetInt("BC-OCC-COUNT")
            _missingRates = ca.GetInt("BC-MISSING-RATES")
            _totalCost = ca.GetDecimal("BC-TOTAL-COST")
            _unitCost = ca.GetDecimal("BC-UNIT-COST")
            _materialCost = ca.GetDecimal("BC-MATERIAL-COST")
            _conversionCost = ca.GetDecimal("BC-CONVERSION-COST") + ca.GetDecimal("BC-SETUP-COST")
            _recoveryCredit = ca.GetDecimal("BC-RECOVERY-CREDIT")
            _stopSummary = ca.GetString("BC-ERROR-PATH")

            ' BOMCOST leaves BC-FROM-LINE on the next unread occurrence
            Dim more As Boolean = True
            Dim guard As Integer = 0
            While more AndAlso guard < 100
                guard += 1
                r = Link("BOMCOST", ca, "GETLINE")
                If Not r.Succeeded Then Exit While
                Dim count As Integer = ca.GetInt("BC-WIN-CNT")
                For i As Integer = 1 To count
                    Dim d As DataRow = rows.NewRow()
                    d("Level") = ca.GetInt("BC-L-DEPTH(" & i & ")")
                    d("Item") = ca.GetString("BC-L-ITEM(" & i & ")")
                    d("Revision") = ca.GetString("BC-L-REV(" & i & ")")
                    d("Description") = ca.GetString("BC-L-DESC(" & i & ")")
                    d("Kind") = ca.GetString("BC-L-NODE-TYPE(" & i & ")")
                    d("Element") = ca.GetString("BC-L-ELEMENT(" & i & ")")
                    d("Source") = ca.GetString("BC-L-SOURCE(" & i & ")")
                    d("Required") = ca.GetDecimal("BC-L-REQ-QTY(" & i & ")")
                    d("Unit") = ca.GetString("BC-L-UOM(" & i & ")")
                    d("UnitCost") = ca.GetDecimal("BC-L-UNIT-COST(" & i & ")")
                    d("ExtendedCost") = ca.GetDecimal("BC-L-EXT-COST(" & i & ")")
                    d("BranchCost") = ca.GetDecimal("BC-L-BRANCH(" & i & ")")
                    d("Path") = ca.GetString("BC-L-PATH(" & i & ")")
                    d("Warning") = ca.GetString("BC-L-WARNING(" & i & ")")
                    rows.Rows.Add(d)
                Next
                more = (ca.GetString("BC-MORE-LINES") = "Y")
            End While
            Return r
        End Function

        ''' <summary>Editor feedback: validate without recording a validation run (BOM-004).</summary>
        Public Function Validate(ByVal bomRef As String, ByVal revision As String, ByVal effectiveDate As Date,
                                 ByVal record As Boolean, ByRef findings As DataTable) As CobolResult

            Dim ca As CommArea = NewArea("BOMVALIDCA")
            ca.SetString("BV-BOM-REF", bomRef)
            ca.SetString("BV-BOM-REV", revision)
            ca.SetInt("BV-EFF-DATE", CInt(effectiveDate.ToString("yyyyMMdd")))
            ca.SetInt("BV-FIND-MAX", 50)

            findings = NewFindingTable()
            Dim r As CobolResult = Link("BOMVALID", ca, If(record, "VALIDATE", "CHECK"))
            _validationOutcome = ca.GetString("BV-OUTCOME")
            _validationErrors = ca.GetInt("BV-ERROR-CNT")
            _validationWarnings = ca.GetInt("BV-WARNING-CNT")
            _releaseOk = (ca.GetString("BV-RELEASE-OK") = "Y")
            _cyclePath = ca.GetString("BV-CYCLE-PATH")
            If Not r.Succeeded Then Return r

            For i As Integer = 1 To ca.GetInt("BV-FIND-CNT")
                Dim d As DataRow = findings.NewRow()
                d("Severity") = ca.GetString("BV-F-SEVERITY(" & i & ")")
                d("Reason") = ca.GetString("BV-F-REASON(" & i & ")")
                d("LineNum") = ca.GetInt("BV-F-LINE-NUM(" & i & ")")
                d("Item") = ca.GetString("BV-F-ITEM(" & i & ")")
                d("Path") = ca.GetString("BV-F-PATH(" & i & ")")
                d("Message") = ca.GetString("BV-F-MESSAGE(" & i & ")")
                findings.Rows.Add(d)
            Next
            Return r
        End Function

        ''' <summary>Every revision of a bill or of a parent item, newest first (BOM-002).</summary>
        Public Function Revisions(ByVal bomRef As String, ByVal parentItem As String, ByVal asOf As Date) As DataTable
            Return ReadStructure("REVISIONS", bomRef, "", parentItem, asOf)
        End Function

        ''' <summary>One revision: the declared boundary and the release evidence (BOM-001).</summary>
        Public Function BillHeader(ByVal bomRef As String, ByVal revision As String, ByVal asOf As Date) As DataRow
            Dim t As DataTable = ReadStructure("HEADER", bomRef, revision, "", asOf)
            If t Is Nothing OrElse t.Rows.Count = 0 Then Return Nothing
            Return t.Rows(0)
        End Function

        ''' <summary>The component occurrences of one revision (BOM-006..BOM-011, BOM-027).</summary>
        Public Function BillLines(ByVal bomRef As String, ByVal revision As String, ByVal asOf As Date) As DataTable
            Return ReadStructure("LINES", bomRef, revision, "", asOf)
        End Function

        ''' <summary>The by-product recoveries of one revision (BOM-026).</summary>
        Public Function ByProducts(ByVal bomRef As String, ByVal revision As String, ByVal asOf As Date) As DataTable
            Return ReadStructure("BYPRODUCT", bomRef, revision, "", asOf)
        End Function

        Private Function ReadStructure(ByVal mode As String, ByVal bomRef As String, ByVal revision As String,
                                   ByVal parentItem As String, ByVal asOf As Date) As DataTable
            Return Query("bom.usp_BomStructure",
                "@CompanyCode", Company, "@BomRef", bomRef, "@BomRevision", revision,
                "@ParentItemCode", parentItem, "@Mode", mode, "@AsOf", asOf)
        End Function

        ''' <summary>Where used - direct and indirect parents of a component (BOM-023).</summary>
        Public Function WhereUsed(ByVal company As String, ByVal itemCode As String, ByVal asOf As Date,
                                  ByVal maxLevels As Integer, ByVal includeDraft As Boolean) As DataTable
            Return SqlHelper.ExecuteDataTable("bom.usp_BomWhereUsed",
                "@CompanyCode", company, "@ComponentItemCode", itemCode, "@AsOf", asOf,
                "@MaxLevels", maxLevels, "@IncludeDraft", If(includeDraft, 1, 0))
        End Function

        ''' <summary>Added, removed and changed components between two revisions (BOM-025).</summary>
        Public Function CompareRevisions(ByVal company As String, ByVal bomRef As String,
                                         ByVal fromRevision As String, ByVal toRevision As String,
                                         ByVal costSet As String) As DataTable
            Return SqlHelper.ExecuteDataTable("bom.usp_BomCompareRevisions",
                "@CompanyCode", company, "@BomRef", bomRef, "@FromRevision", fromRevision,
                "@ToRevision", toRevision, "@CostSetCode", costSet)
        End Function

        ''' <summary>Release a validated revision; the procedure refuses anything else (BOM-004).</summary>
        Public Function Release(ByVal company As String, ByVal bomRef As String, ByVal revision As String,
                                ByVal user As String, ByVal reviewer As String, ByVal effectiveFrom As Date,
                                ByVal supersede As String, ByVal reason As String) As DataTable
            Return SqlHelper.ExecuteDataTable("bom.usp_ReleaseBom",
                "@CompanyCode", company, "@BomRef", bomRef, "@BomRevision", revision,
                "@UserCode", user, "@ReviewerUser", reviewer, "@EffectiveFrom", effectiveFrom,
                "@SupersedeRevision", supersede, "@Reason", reason,
                "@CorrelationId", Session.NewCorrelationId())
        End Function

        Private Function ViewToFunction(ByVal view As String) As String
            Select Case UCase(Trim(view))
                Case "SINGLE"
                    Return "SINGLE"
                Case "LEAF"
                    Return "FLATTEN"
                Case Else
                    Return "INDENT"
            End Select
        End Function

        Private Function NewExplosionTable() As DataTable
            Dim t As New DataTable("Explosion")
            t.Columns.Add("Level", GetType(Integer))
            t.Columns.Add("Item", GetType(String))
            t.Columns.Add("Revision", GetType(String))
            t.Columns.Add("Description", GetType(String))
            t.Columns.Add("Kind", GetType(String))
            t.Columns.Add("Supply", GetType(String))
            t.Columns.Add("Basis", GetType(String))
            t.Columns.Add("Usage", GetType(Decimal))
            t.Columns.Add("Unit", GetType(String))
            t.Columns.Add("Required", GetType(Decimal))
            t.Columns.Add("Unrounded", GetType(Decimal))
            t.Columns.Add("StopReason", GetType(String))
            t.Columns.Add("Path", GetType(String))
            t.Columns.Add("Warning", GetType(String))
            Return t
        End Function

        Private Function NewCostedTable() As DataTable
            Dim t As New DataTable("Costed")
            t.Columns.Add("Level", GetType(Integer))
            t.Columns.Add("Item", GetType(String))
            t.Columns.Add("Revision", GetType(String))
            t.Columns.Add("Description", GetType(String))
            t.Columns.Add("Kind", GetType(String))
            t.Columns.Add("Element", GetType(String))
            t.Columns.Add("Source", GetType(String))
            t.Columns.Add("Required", GetType(Decimal))
            t.Columns.Add("Unit", GetType(String))
            t.Columns.Add("UnitCost", GetType(Decimal))
            t.Columns.Add("ExtendedCost", GetType(Decimal))
            t.Columns.Add("BranchCost", GetType(Decimal))
            t.Columns.Add("Path", GetType(String))
            t.Columns.Add("Warning", GetType(String))
            Return t
        End Function

        Private Function NewFindingTable() As DataTable
            Dim t As New DataTable("Findings")
            t.Columns.Add("Severity", GetType(String))
            t.Columns.Add("Reason", GetType(String))
            t.Columns.Add("LineNum", GetType(Integer))
            t.Columns.Add("Item", GetType(String))
            t.Columns.Add("Path", GetType(String))
            t.Columns.Add("Message", GetType(String))
            Return t
        End Function

        Private Function NewNettedTable() As DataTable
            Dim t As New DataTable("Netted")
            t.Columns.Add("Level", GetType(Integer))
            t.Columns.Add("Item", GetType(String))
            t.Columns.Add("Description", GetType(String))
            t.Columns.Add("Unit", GetType(String))
            t.Columns.Add("Required", GetType(Decimal))
            t.Columns.Add("StockUsed", GetType(Decimal))
            t.Columns.Add("Inbound", GetType(Decimal))
            t.Columns.Add("ToMake", GetType(Decimal))
            t.Columns.Add("Shortage", GetType(Decimal))
            t.Columns.Add("Excess", GetType(Decimal))
            t.Columns.Add("StopReason", GetType(String))
            Return t
        End Function
    End Class
End Namespace
