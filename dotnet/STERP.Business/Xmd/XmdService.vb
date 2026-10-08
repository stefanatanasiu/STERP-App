Imports System.Data
Imports STERP.CobolInterop
Imports STERP.Common

Namespace Xmd

    ''' <summary>
    ''' Facade over the cross module controls and reporting family.  The only interactive
    ''' program in XMD is CHAINTRC, which takes one document reference and returns the ordered
    ''' steps of its order to cash, procure to pay or assembly chain, with the independent
    ''' return, credit and reversal consequences marked separately.  Everything else in the
    ''' family is a catalogued batch report, so the facade also exposes those rows.
    ''' REQ: XMD-001, XMD-002, XMD-003, XMD-004, XMD-013
    ''' </summary>
    Public Class XmdService
        Inherits ServiceBase

        ''' <summary>Chain step window of CHAINTRCCA - 24 steps, the COMMAREA maximum.</summary>
        Public Const StepSpec As String =
            "Seq:I=CT-SEQ,Stage=CT-STAGE,DocType=CT-DOC-T,Reference=CT-REF,Dated:T=CT-DATE," &
            "Status=CT-STATUS,Quantity:D=CT-QTY,ValueGBP:D=CT-VALUE,Kind=CT-KIND,Note=CT-NOTE"

        ''' <summary>CHAINTRC fills at most this many steps and then reports truncation.</summary>
        Public Const MaxSteps As Integer = 24

        Private _ca As CommArea
        Private _steps As DataTable

        ''' <summary>The chain the last trace returned, already shaped for a grid.</summary>
        Public ReadOnly Property Steps() As DataTable
            Get
                Return _steps
            End Get
        End Property

        ''' <summary>A field of the last COMMAREA as text ("" when the program is not installed).</summary>
        Public Function Text(ByVal field As String) As String
            If _ca Is Nothing OrElse Not _ca.Has(field) Then Return ""
            Return _ca.GetString(field)
        End Function

        ''' <summary>A field of the last COMMAREA as a number.</summary>
        Public Function Num(ByVal field As String) As Decimal
            If _ca Is Nothing OrElse Not _ca.Has(field) Then Return 0D
            Return _ca.GetDecimal(field)
        End Function

        ''' <summary>
        ''' Traces one document through its chain.  <paramref name="docType"/> is the kind of
        ''' document the reference belongs to (ORDER, PO, KIT, INVOICE ...); CHAINTRC works the
        ''' chain out for itself when it is left blank.
        ''' REQ: XMD-001, XMD-002, XMD-003
        ''' </summary>
        Public Function ChainTrace(ByVal docType As String, ByVal docRef As String) As CobolResult
            _steps = NewTable(StepSpec)
            Dim ca As CommArea = NewArea("CHAINTRCCA")
            ca.SetString("CT-DOC-TYPE", docType)
            ca.SetString("CT-DOC-REF", docRef)
            ca.SetInt("CT-MAX-STEPS", MaxSteps)
            _ca = ca
            Dim r As CobolResult = Link("CHAINTRC", ca, "TRACE")
            If r IsNot Nothing AndAlso r.Succeeded Then
                Dim n As Integer = ca.GetInt("CT-STEP-CNT")
                If n > MaxSteps Then n = MaxSteps
                Copy(ca, _steps, StepSpec, n)
            End If
            Return r
        End Function

        ''' <summary>
        ''' The same chain as a set operation, for an export and for the whole chain when it is
        ''' longer than the COMMAREA window.  rpt.usp_ChainTrace is the twin of CHAINTRC.
        ''' REQ: XMD-004
        ''' </summary>
        Public Function ChainTraceList(ByVal docType As String, ByVal docRef As String) As DataTable
            If Not Deployed("rpt.usp_ChainTrace") Then Return Nothing
            Return Query("rpt.usp_ChainTrace", "@CompanyCode", Company,
                         "@DocType", docType, "@Reference", docRef)
        End Function

        ''' <summary>The catalogued reports of this family, for the launcher. REQ: XMD-013</summary>
        Public Function Reports() As DataTable
            Dim catalogue As New ReportService()
            Return catalogue.Definitions("XMD")
        End Function

        ' ------------------------------------------------------------- plumbing

        ''' <summary>Builds the display table. ":I" integer, ":D" decimal, ":T" date, else text.</summary>
        Private Shared Function NewTable(ByVal spec As String) As DataTable
            Dim t As New DataTable()
            For Each entry As String In spec.Split(","c)
                Dim parts() As String = entry.Split("="c)(0).Split(":"c)
                Dim ty As Type = GetType(String)
                If parts.Length > 1 Then
                    Select Case parts(1)
                        Case "I" : ty = GetType(Integer)
                        Case "D" : ty = GetType(Decimal)
                        Case "T" : ty = GetType(Date)
                    End Select
                End If
                t.Columns.Add(parts(0), ty)
            Next
            Return t
        End Function

        ''' <summary>Copies the first <paramref name="count"/> occurrences into the table.</summary>
        Private Shared Sub Copy(ByVal ca As CommArea, ByVal rows As DataTable, ByVal spec As String,
                                ByVal count As Integer)
            Dim entries() As String = spec.Split(","c)
            For i As Integer = 1 To count
                Dim d As DataRow = rows.NewRow()
                For Each entry As String In entries
                    Dim nv() As String = entry.Split("="c)
                    Dim col As String = nv(0).Split(":"c)(0)
                    If ca.Has(nv(1)) Then
                        Dim f As String = nv(1) & "(" & i & ")"
                        Dim ty As Type = rows.Columns(col).DataType
                        If ty Is GetType(Integer) Then
                            d(col) = ca.GetInt(f)
                        ElseIf ty Is GetType(Decimal) Then
                            d(col) = ca.GetDecimal(f)
                        ElseIf ty Is GetType(Date) Then
                            Dim dt As Date? = ca.GetDate(f)
                            If dt.HasValue Then d(col) = dt.Value
                        Else
                            d(col) = ca.GetString(f)
                        End If
                    End If
                Next
                rows.Rows.Add(d)
            Next
        End Sub

        ''' <summary>A blank filter means "all", which a stored procedure expresses as NULL.</summary>
        Private Shared Function Opt(ByVal value As String) As Object
            If String.IsNullOrEmpty(value) Then Return DBNull.Value
            Return value
        End Function

        ''' <summary>rpt.usp_ChainTrace only walks the three chains it can start from.</summary>
        Public Shared Function IsSetTraceable(ByVal docType As String) As Boolean
            Select Case docType
                Case "ORDER", "PO", "KIT"
                    Return True
            End Select
            Return False
        End Function

    End Class

End Namespace
