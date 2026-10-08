Imports STERP.Common

''' <summary>Menu tree from core.MenuItem, already filtered by permission (COM-002).</summary>
Public Class MenuService
    Inherits ServiceBase

    ''' <summary>
    ''' Columns: MenuCode, ParentMenuCode, Caption, FormClass, FormArgument,
    ''' PermissionCode, SortOrder, Shortcut.
    ''' </summary>
    Public Function Load() As DataTable
        Try
            Return Query("core.usp_UiMenuLoad", "@UserCode", User, "@CompanyCode", Company, "@SiteCode", Site)
        Catch ex As Exception
            SterpLog.Error("Menu load failed", ex)
            Return New DataTable()
        End Try
    End Function

End Class

''' <summary>Per-user window and grid preferences (core.UserPreference).</summary>
Public Class PreferenceService
    Inherits ServiceBase

    Public Function GetValue(ByVal key As String, ByVal dflt As String) As String
        Try
            Dim o As Object = Scalar("core.usp_UiUserPrefGet", "@UserCode", User, "@PrefKey", key)
            If o Is Nothing OrElse IsDBNull(o) Then Return dflt
            Return Convert.ToString(o)
        Catch ex As Exception
            Return dflt
        End Try
    End Function

    Public Sub SetValue(ByVal key As String, ByVal value As String)
        Try
            Exec("core.usp_UiUserPrefSet", "@UserCode", User, "@PrefKey", key, "@PrefValue", value)
        Catch ex As Exception
            SterpLog.Warn("Preference " & key & ": " & ex.Message)
        End Try
    End Sub

End Class
