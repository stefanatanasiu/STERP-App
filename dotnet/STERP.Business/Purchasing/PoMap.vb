Imports System.Collections.Generic
Imports STERP.Common

''' <summary>
''' Small conversion helpers shared by the purchasing facades.  Screens hand their fields over
''' as a plain name/value dictionary - exactly the way the old CICS maps did - and these
''' routines turn that into the typed COMMAREA values.
''' REQ: PO-003
''' </summary>
Public Module PoMap

    ''' <summary>A code field: trimmed and upper cased, never Nothing.</summary>
    Public Function S(ByVal v As Dictionary(Of String, String), ByVal key As String) As String
        Return T(v, key).ToUpperInvariant()
    End Function

    ''' <summary>A free text field: trimmed, case preserved.</summary>
    Public Function T(ByVal v As Dictionary(Of String, String), ByVal key As String) As String
        If v Is Nothing OrElse Not v.ContainsKey(key) OrElse v(key) Is Nothing Then Return ""
        Return v(key).Trim()
    End Function

    Public Function D(ByVal v As Dictionary(Of String, String), ByVal key As String) As Decimal
        Dim s As String = T(v, key)
        Dim parsed As Decimal
        If s = "" OrElse Not Decimal.TryParse(s.Replace("£", "").Replace(",", ""), parsed) Then Return 0D
        Return parsed
    End Function

    Public Function I(ByVal v As Dictionary(Of String, String), ByVal key As String) As Integer
        Return CInt(Decimal.Truncate(D(v, key)))
    End Function

    ''' <summary>A date box, UK format; the business date when the user left it empty.</summary>
    Public Function Dt(ByVal v As Dictionary(Of String, String), ByVal key As String,
                       ByVal dflt As Date) As Date
        Dim d As Date? = Fmt.ParseDate(T(v, key))
        If d.HasValue Then Return d.Value
        Return dflt
    End Function

    ''' <summary>Builds the columns of a working grid from "Name" / "Name:INT" / "Name:DEC" specs.</summary>
    Public Sub Columns(ByVal t As DataTable, ByVal ParamArray specs() As String)
        For Each spec As String In specs
            Dim p() As String = spec.Split(":"c)
            Dim ty As Type = GetType(String)
            If p.Length > 1 AndAlso p(1) = "INT" Then ty = GetType(Integer)
            If p.Length > 1 AndAlso p(1) = "DEC" Then ty = GetType(Decimal)
            If p.Length > 1 AndAlso p(1) = "DATE" Then ty = GetType(Date)
            t.Columns.Add(p(0), ty)
        Next
    End Sub
End Module
