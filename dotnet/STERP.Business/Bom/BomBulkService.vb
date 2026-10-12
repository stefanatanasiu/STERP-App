Imports System.Data
Imports STERP.CobolInterop

Namespace Bom
    ''' <summary>Preview only: all proposals and refusals come from BOMBULK.</summary>
    Public Class BomBulkService
        Inherits ServiceBase

        Public Property TargetCount As Integer
        Public Property FailedCount As Integer
        Public Property AppliedCount As Integer
        Public Property MayApply As Boolean
        Public Property Summary As String = ""
        Public Property Refusal As String = ""

        Public Function Preview(ByVal changeRef As String, ByVal oldItem As String,
                                ByVal newItem As String, ByVal effectiveDate As Date,
                                ByVal scope As String, ByVal scopeItem As String,
                                ByVal includeDraft As Boolean, ByRef rows As DataTable) As CobolResult
            TargetCount = 0 : FailedCount = 0 : AppliedCount = 0 : MayApply = False
            Summary = "" : Refusal = ""
            rows = New DataTable()
            For Each name As String In {"Bill", "Revision", "ProposedRevision", "Parent", "Treatment",
                                        "Outcome", "Reason", "Detail"}
                rows.Columns.Add(name, GetType(String))
            Next
            rows.Columns.Add("Line", GetType(Integer))
            rows.Columns.Add("BeforeQty", GetType(Decimal))
            rows.Columns.Add("AfterQty", GetType(Decimal))
            Dim ca As CommArea = NewArea("BOMBULKCA")
            ca.SetString("BB-CHANGE-REF", changeRef)
            ca.SetString("BB-ACTION", "REPLACE")
            ca.SetString("BB-SCOPE", scope)
            ca.SetString("BB-SCOPE-ITEM", scopeItem)
            ca.SetString("BB-OLD-ITEM", oldItem)
            ca.SetString("BB-NEW-ITEM", newItem)
            ca.SetInt("BB-EFF-DATE", CInt(effectiveDate.ToString("yyyyMMdd")))
            ca.SetString("BB-INCLUDE-DRAFT", If(includeDraft, "Y", "N"))
            ca.SetInt("BB-FROM-LINE", 1)
            ca.SetInt("BB-WIN-MAX", 25)
            Dim result As CobolResult = Link("BOMBULK", ca, "PREVIEW")
            TargetCount = ca.GetInt("BB-TARGET-CNT")
            FailedCount = ca.GetInt("BB-FAILED-CNT")
            AppliedCount = ca.GetInt("BB-APPLIED-CNT")
            MayApply = ca.GetString("BB-MAY-APPLY") = "Y"
            Summary = ca.GetString("BB-SUMMARY")
            Refusal = ca.GetString("BB-REFUSAL")
            ' A refused proposal still carries all per-bill outcomes.
            Do
                For i As Integer = 1 To ca.GetInt("BB-WIN-CNT")
                    rows.Rows.Add(ca.GetString("BB-L-BOM-REF(" & i & ")"),
                        ca.GetString("BB-L-BOM-REV(" & i & ")"), ca.GetString("BB-L-NEW-REV(" & i & ")"),
                        ca.GetString("BB-L-PARENT(" & i & ")"), ca.GetString("BB-L-TREATMENT(" & i & ")"),
                        ca.GetString("BB-L-OUTCOME(" & i & ")"), ca.GetString("BB-L-REASON(" & i & ")"),
                        ca.GetString("BB-L-DETAIL(" & i & ")"), ca.GetInt("BB-L-LINE-NUM(" & i & ")"),
                        ca.GetDecimal("BB-L-OLD-QTY(" & i & ")"), ca.GetDecimal("BB-L-NEW-QTY(" & i & ")"))
                Next
                If ca.GetString("BB-MORE-LINES") <> "Y" Then Exit Do
                Dim page As CobolResult = Link("BOMBULK", ca, "GETLINE")
                If Not page.Succeeded Then Return page
            Loop
            Return result
        End Function
    End Class
End Namespace
