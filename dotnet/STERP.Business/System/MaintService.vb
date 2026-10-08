Imports System.Text
Imports STERP.Common

''' <summary>
''' Generic maintenance driven by core.MaintDef / core.MaintColumn.  The client never
''' builds SQL: it posts column name / value pairs and the stored procedure validates
''' them against the registered column list (QUOTENAME whitelist) and writes the audit.
''' </summary>
Public Class MaintService
    Inherits ServiceBase

    Public Definition As DataRow
    Public Columns As DataTable

    ''' <summary>Loads the definition (result set 1) and its columns (result set 2).</summary>
    Public Function LoadDefinition(ByVal maintCode As String) As Boolean
        Dim ds As DataSet = QuerySet("core.usp_UiMaintDef", "@MaintCode", maintCode, "@UserCode", User,
                                     "@CompanyCode", Company)
        If ds.Tables.Count = 0 OrElse ds.Tables(0).Rows.Count = 0 Then Return False
        Definition = ds.Tables(0).Rows(0)
        Columns = If(ds.Tables.Count > 1, ds.Tables(1), New DataTable())
        Return True
    End Function

    Public Function List(ByVal maintCode As String, ByVal search As String) As DataTable
        Return Query("core.usp_UiMaintList", "@MaintCode", maintCode, "@UserCode", User,
                     "@CompanyCode", Company, "@Search", search)
    End Function

    ''' <summary>mode is ADD, CHANGE or RETIRE.  Returns "" when saved, otherwise the message.</summary>
    Public Function Save(ByVal maintCode As String, ByVal mode As String,
                         ByVal values As Dictionary(Of String, String)) As String
        Try
            Exec("core.usp_UiMaintSave", "@MaintCode", maintCode, "@UserCode", User,
                 "@CompanyCode", Company, "@Mode", mode.ToUpperInvariant(), "@Data", ToXml(values))
            Return ""
        Catch ex As STERP.Data.BusinessRuleException
            Return ex.Message & "  (" & ex.Code & ")"
        Catch ex As Exception
            SterpLog.Error("MaintSave " & maintCode, ex)
            Return ex.Message
        End Try
    End Function

    ''' <summary>Every maintenance definition the signed-on user may open.</summary>
    Public Function Definitions() As DataTable
        Return Query("core.usp_UiMaintDefList", "@UserCode", User, "@CompanyCode", Company)
    End Function

    ''' <summary>&lt;r&gt;&lt;c n="ColumnName" v="value" /&gt;...&lt;/r&gt; - shredded and whitelisted by the procedure.</summary>
    Public Shared Function ToXml(ByVal values As Dictionary(Of String, String)) As String
        Dim sb As New StringBuilder("<r>")
        For Each k As String In values.Keys
            sb.Append("<c n=""").Append(Esc(k)).Append("""")
            If values(k) Is Nothing Then
                sb.Append(" null=""1"" />")
            Else
                sb.Append(" v=""").Append(Esc(values(k))).Append(""" />")
            End If
        Next
        sb.Append("</r>")
        Return sb.ToString()
    End Function

    Private Shared Function Esc(ByVal s As String) As String
        If s Is Nothing Then Return ""
        Return s.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;").Replace("""", "&quot;")
    End Function

End Class

''' <summary>
''' Backs the F2 lookup window.  A lookup is just a stored procedure that takes
''' @CompanyCode, @SiteCode and @Search and returns a result set whose first column
''' is the code to return to the field.
''' </summary>
Public Class LookupService
    Inherits ServiceBase

    Public Function Find(ByVal procedure As String, ByVal text As String,
                         ByVal argument As String) As DataTable
        Return Query(procedure, "@CompanyCode", Company, "@SiteCode", Site,
                     "@Search", text, "@Argument", argument)
    End Function

End Class
