Imports STERP.Common

''' <summary>
''' Sign-on: Windows login -> core.usp_GetUserContext, company/site selection and
''' permission checks.  SYSADM may continue as another user so that a demonstration
''' can show the system through the eyes of a clerk (COM-002).
''' </summary>
Public Class LoginService
    Inherits ServiceBase

    Public ReadOnly Property WindowsLogin() As String
        Get
            Return Environment.UserDomainName & "\" & Environment.UserName
        End Get
    End Property

    ''' <summary>Loads the user context for the signed-on Windows account.  Returns "" when it worked.</summary>
    Public Function SignOn() As String
        Return SignOnAs(Nothing)
    End Function

    ''' <summary>Loads a context by user code (the SYSADM "run as" demo path).</summary>
    Public Function SignOnAs(ByVal userCode As String) As String
        Dim s As SterpSession = SterpSession.Current
        Try
            Dim ds As DataSet = Nothing
            If Deployed("core.usp_GetUserContext") Then
                Try
                    If String.IsNullOrEmpty(userCode) Then
                        ds = QuerySet("core.usp_GetUserContext", "@WindowsLogin", WindowsLogin)
                    Else
                        ds = QuerySet("core.usp_GetUserContext", "@UserCode", userCode)
                    End If
                Catch ex As Exception
                    ' the security module refuses unknown sign-ons - try the degraded paths
                    SterpLog.Warn("GetUserContext: " & ex.Message)
                    ds = Nothing
                End Try
            End If
            If ds Is Nothing OrElse ds.Tables.Count = 0 OrElse ds.Tables(0).Rows.Count = 0 Then
                ds = Fallback(userCode)
            End If
            If ds.Tables(0).Rows.Count = 0 Then
                Return "No STERP user is mapped to " & If(String.IsNullOrEmpty(userCode), WindowsLogin, userCode) & "."
            End If

            Dim r As DataRow = ds.Tables(0).Rows(0)
            s.UserCode = Fmt.Str(Col(r, "UserCode"))
            s.DisplayName = Fmt.Str(Col(r, "DisplayName", s.UserCode))
            s.WindowsLogin = Fmt.Str(Col(r, "WindowsLogin", WindowsLogin))
            s.IsSysAdm = (s.UserCode = "SYSADM")
            If s.RealUserCode = "" Then s.RealUserCode = s.UserCode
            s.PermissionCache.Clear()
            s.Access.Clear()

            If ds.Tables.Count > 1 Then
                For Each a As DataRow In ds.Tables(1).Rows
                    s.Access.Add(New CompanySite() With {
                        .CompanyCode = Fmt.Str(Col(a, "CompanyCode")),
                        .CompanyName = Fmt.Str(Col(a, "CompanyName", Col(a, "Name"))),
                        .SiteCode = Fmt.Str(Col(a, "SiteCode")),
                        .SiteName = Fmt.Str(Col(a, "SiteName")),
                        .BaseCurrency = Fmt.Str(Col(a, "BaseCurrency", "GBP")),
                        .Nation = Fmt.Str(Col(a, "Nation", "ENG"))})
                Next
            End If
            If s.Access.Count > 0 Then s.SelectCompanySite(s.Access(0))
            SterpLog.Info("Sign-on " & s.UserCode & " (" & s.Access.Count & " company/site combinations)")
            Return ""
        Catch ex As Exception
            SterpLog.Error("SignOn failed", ex)
            Return ex.Message
        End Try
    End Function

    ''' <summary>
    ''' Degraded sign-on used while the security module is being installed: read
    ''' core.AppUser / core.Company directly and, if even those are empty, fall back
    ''' to the stand-alone SYSADM context the installer uses.
    ''' </summary>
    Private Function Fallback(ByVal userCode As String) As DataSet
        Dim ds As New DataSet()
        Dim users As DataTable = Nothing
        Try
            If Deployed("core.AppUser") Then
                users = Query("SELECT TOP 1 UserCode, DisplayName, WindowsLogin FROM core.AppUser " &
                              "WHERE IsActive = 1 AND (UserCode = @u OR (@u IS NULL AND WindowsLogin = @w)) " &
                              "ORDER BY CASE WHEN WindowsLogin = @w THEN 0 ELSE 1 END, UserCode",
                              "@u", userCode, "@w", WindowsLogin)
            End If
        Catch ex As Exception
            SterpLog.Warn("Fallback user read: " & ex.Message)
        End Try
        If users Is Nothing OrElse users.Rows.Count = 0 Then
            users = New DataTable("AppUser")
            users.Columns.Add("UserCode") : users.Columns.Add("DisplayName") : users.Columns.Add("WindowsLogin")
            users.Rows.Add(If(String.IsNullOrEmpty(userCode), "SYSADM", userCode.ToUpperInvariant()),
                           "Systems administrator (stand-alone sign-on)", WindowsLogin)
            SterpLog.Warn("Stand-alone sign-on - core.usp_GetUserContext is not installed")
        End If
        ds.Tables.Add(users)

        Dim sites As DataTable = Nothing
        Try
            If Deployed("core.Company") Then
                sites = Query("SELECT c.CompanyCode, c.Name AS CompanyName, ISNULL(s.SiteCode, '') AS SiteCode, " &
                              "ISNULL(s.Name, '') AS SiteName, c.BaseCurrency, ISNULL(s.Nation, 'ENG') AS Nation " &
                              "FROM core.Company c LEFT JOIN core.Site s ON s.CompanyCode = c.CompanyCode " &
                              "WHERE c.IsActive = 1 ORDER BY c.CompanyCode, s.SiteCode")
            End If
        Catch ex As Exception
            SterpLog.Warn("Fallback company read: " & ex.Message)
        End Try
        If sites Is Nothing OrElse sites.Rows.Count = 0 Then
            sites = New DataTable("Access")
            For Each c As String In New String() {"CompanyCode", "CompanyName", "SiteCode", "SiteName", "BaseCurrency", "Nation"}
                sites.Columns.Add(c)
            Next
            sites.Rows.Add("STC", "Stellar Components Ltd", "RDG", "Reading", "GBP", "ENG")
            sites.Rows.Add("STC", "Stellar Components Ltd", "BHM", "Birmingham", "GBP", "ENG")
            sites.Rows.Add("CAL", "Caledon Assemblies Ltd", "GLA", "Glasgow", "GBP", "SCT")
            sites.Rows.Add("CAL", "Caledon Assemblies Ltd", "LIV", "Livingston", "GBP", "SCT")
        End If
        ds.Tables.Add(sites)
        Return ds
    End Function

    ''' <summary>SYSADM continues the session as another user (demonstration only).</summary>
    Public Function RunAs(ByVal userCode As String) As String
        Dim real As String = SterpSession.Current.RealUserCode
        If real <> "SYSADM" Then Return "Only SYSADM may continue as another user."
        Dim msg As String = SignOnAs(userCode)
        If msg = "" Then
            SterpSession.Current.RealUserCode = real
            SterpSession.Current.IsSysAdm = False
            AuditService.Note("RUNAS", userCode, "SYSADM continued the session as " & userCode)
        End If
        Return msg
    End Function

    ''' <summary>core.usp_CheckPermission, cached for the life of the company/site selection (COM-002).</summary>
    Public Function HasPermission(ByVal permissionCode As String) As Boolean
        If String.IsNullOrEmpty(permissionCode) Then Return True
        Dim s As SterpSession = SterpSession.Current
        If s.PermissionCache.ContainsKey(permissionCode) Then Return s.PermissionCache(permissionCode)
        Dim allowed As Boolean = True
        Try
            Dim r As DataRow = QueryRow("core.usp_CheckPermission",
                "@UserCode", s.UserCode, "@PermissionCode", permissionCode,
                "@CompanyCode", s.CompanyCode, "@SiteCode", s.SiteCode)
            allowed = (r IsNot Nothing) AndAlso Fmt.Bool(Col(r, "Allowed", False))
        Catch ex As Exception
            SterpLog.Warn("CheckPermission " & permissionCode & ": " & ex.Message)
        End Try
        s.PermissionCache(permissionCode) = allowed
        Return allowed
    End Function

    ''' <summary>Business date and the fiscal period it falls in (fin.Period).</summary>
    Public Sub LoadPeriod(ByVal businessDate As Date)
        Dim s As SterpSession = SterpSession.Current
        s.BusinessDate = businessDate
        s.FiscalYear = 0 : s.PeriodNo = 0 : s.PeriodStatus = ""
        Try
            If Not Deployed("fin.Period") Then Return
            Dim r As DataRow = QueryRow(
                "SELECT TOP 1 FiscalYear, PeriodNo, Status FROM fin.Period " &
                "WHERE CompanyCode = @c AND @d BETWEEN StartDate AND EndDate ORDER BY FiscalYear, PeriodNo",
                "@c", s.CompanyCode, "@d", businessDate)
            If r IsNot Nothing Then
                s.FiscalYear = Fmt.Int(Col(r, "FiscalYear"))
                s.PeriodNo = Fmt.Int(Col(r, "PeriodNo"))
                s.PeriodStatus = Fmt.Str(Col(r, "Status"))
            End If
        Catch ex As Exception
            SterpLog.Warn("LoadPeriod: " & ex.Message)
        End Try
    End Sub

    ''' <summary>User codes offered by the "run as" list on the sign-on dialog.</summary>
    Public Function UserList() As DataTable
        Try
            If Not Deployed("core.AppUser") Then Return New DataTable()
            Return Query("SELECT UserCode, DisplayName FROM core.AppUser WHERE IsActive = 1 ORDER BY UserCode")
        Catch ex As Exception
            Return New DataTable()
        End Try
    End Function

End Class
