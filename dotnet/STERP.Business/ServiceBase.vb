Imports System.Data.SqlClient
Imports STERP.Common
Imports STERP.CobolInterop
Imports STERP.Data

''' <summary>
''' Common plumbing for every STERP service facade: short names for the SqlHelper
''' calls and for a COBOL LINK, plus the sign-on context.  Services orchestrate -
''' business rules live in COBOL and in stored procedures.
''' </summary>
Public MustInherit Class ServiceBase

    Public Shared ReadOnly Property Session() As SterpSession
        Get
            Return SterpSession.Current
        End Get
    End Property

    Protected Shared ReadOnly Property Company() As String
        Get
            Return Session.CompanyCode
        End Get
    End Property

    Protected Shared ReadOnly Property Site() As String
        Get
            Return Session.SiteCode
        End Get
    End Property

    Protected Shared ReadOnly Property User() As String
        Get
            Return Session.UserCode
        End Get
    End Property

    ' ---------------------------------------------------------------- SQL
    Protected Function Query(ByVal proc As String, ByVal ParamArray nv() As Object) As DataTable
        Return SqlHelper.ExecuteDataTable(proc, nv)
    End Function

    Protected Function QuerySet(ByVal proc As String, ByVal ParamArray nv() As Object) As DataSet
        Return SqlHelper.ExecuteDataSet(proc, nv)
    End Function

    Protected Function QueryRow(ByVal proc As String, ByVal ParamArray nv() As Object) As DataRow
        Return SqlHelper.ExecuteRow(proc, nv)
    End Function

    Protected Function Exec(ByVal proc As String, ByVal ParamArray nv() As Object) As Integer
        Return SqlHelper.ExecuteNonQuery(proc, nv)
    End Function

    Protected Function Scalar(ByVal proc As String, ByVal ParamArray nv() As Object) As Object
        Return SqlHelper.ExecuteScalar(proc, nv)
    End Function

    ''' <summary>True when the database object is deployed (modules come online one by one).</summary>
    Protected Shared Function Deployed(ByVal objectName As String) As Boolean
        Return SqlHelper.ObjectExists(objectName)
    End Function

    ''' <summary>Reads a column if the result set happens to have it, otherwise the default.</summary>
    Public Shared Function Col(ByVal r As DataRow, ByVal name As String, Optional ByVal dflt As Object = Nothing) As Object
        If r Is Nothing OrElse Not r.Table.Columns.Contains(name) OrElse IsDBNull(r(name)) Then Return dflt
        Return r(name)
    End Function

    ' -------------------------------------------------------------- COBOL
    Protected Function NewArea(ByVal copybook As String) As CommArea
        Return CommArea.For(copybook)
    End Function

    Protected Function Link(ByVal program As String, ByVal area As CommArea, ByVal func As String) As CobolResult
        Return CobolGateway.Link(program, area, func)
    End Function

    Protected Function Link(ByVal program As String, ByVal area As CommArea, ByVal func As String,
                            ByVal correlationId As String) As CobolResult
        Return CobolGateway.Link(program, area, func, correlationId)
    End Function

End Class
