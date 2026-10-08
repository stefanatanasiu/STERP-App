Imports System.Data
Imports STERP.CobolInterop

Namespace Kits

    ''' <summary>
    ''' COMMAREA plumbing shared by the kit facades.  A "spec" names the display column, its
    ''' type and the COBOL field in one string - "Item=KC-L-ITEM,Required:D=KC-L-CURR-REQ" -
    ''' so a table and the copy out of an OCCURS window are driven from the same text.
    ''' Paging follows the GETLINE convention every kit enquiry program uses.
    ''' REQ: KIT-003, KIT-022
    ''' </summary>
    Public Module KitArea

        ''' <summary>Builds the display table. ":I" integer, ":D" decimal, ":T" date, else text.</summary>
        Public Function NewTable(ByVal spec As String) As DataTable
            Dim t As New DataTable()
            For Each entry As String In spec.Split(","c)
                Dim name As String = entry.Split("="c)(0)
                Dim parts() As String = name.Split(":"c)
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
        Public Sub Copy(ByVal ca As CommArea, ByVal rows As DataTable, ByVal spec As String,
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

        ''' <summary>
        ''' Drains an OCCURS window: copy, then re-call <paramref name="more"/> (GETLINE) while
        ''' the program says there are further lines.  The guard stops a looping engine.
        ''' </summary>
        Public Function Page(ByVal ca As CommArea, ByVal prefix As String, ByVal rows As DataTable,
                             ByVal spec As String, ByVal r As CobolResult,
                             ByVal more As Func(Of CobolResult)) As CobolResult
            If r Is Nothing OrElse Not r.Succeeded Then Return r
            Dim from As Integer = 0
            For guard As Integer = 1 To 100
                Dim n As Integer = ca.GetInt(prefix & "-WIN-CNT")
                Copy(ca, rows, spec, n)
                from += n
                If n = 0 OrElse ca.GetString(prefix & "-MORE-LINES") <> "Y" Then Exit For
                ca.SetInt(prefix & "-FROM-LINE", from)
                r = more()
                If Not r.Succeeded Then Exit For
            Next
            Return r
        End Function

        ''' <summary>A blank filter means "all", which the stored procedures express as NULL.</summary>
        Public Function Opt(ByVal value As String) As Object
            If String.IsNullOrEmpty(value) Then Return DBNull.Value
            Return value
        End Function

    End Module

    ''' <summary>
    ''' Common state of a kit facade: the COMMAREA the last call left behind and the window it
    ''' filled.  A screen reads the handful of values it displays by field name, so no value is
    ''' restated as a property and no rule is restated in VB.
    ''' </summary>
    Public MustInherit Class KitFacade
        Inherits ServiceBase

        Protected _ca As CommArea
        Protected _lines As DataTable

        ''' <summary>The window the last call filled, already shaped for a grid.</summary>
        Public ReadOnly Property Lines() As DataTable
            Get
                Return _lines
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

    End Class

End Namespace
