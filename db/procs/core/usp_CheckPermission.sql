/*  core.usp_CheckPermission / core.usp_GetUserContext - authority for an action in a company and
    site, and the sign-on context of a Windows login.  REQ: COM-002, COM-019 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_CheckPermission @UserCode varchar(20), @PermissionCode varchar(40), @CompanyCode varchar(10), @SiteCode varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    DECLARE @allowed bit = core.fn_HasPermission(@UserCode, @PermissionCode, @CompanyCode, @SiteCode), @reason varchar(200);
    IF @allowed = 1 SET @reason = 'Permitted';
    ELSE IF NOT EXISTS (SELECT 1 FROM core.AppUser WHERE UserCode = @UserCode) SET @reason = 'STERP-E-0001: User ' + ISNULL(@UserCode, '?') + ' is not registered.';
    ELSE IF NOT EXISTS (SELECT 1 FROM core.AppUser WHERE UserCode = @UserCode AND IsActive = 1) SET @reason = 'STERP-E-0001: User ' + @UserCode + ' is not active.';
    ELSE IF NOT EXISTS (SELECT 1 FROM core.UserRole WHERE UserCode = @UserCode AND (CompanyCode = @CompanyCode OR CompanyCode = '*'))
        SET @reason = 'STERP-E-0001: User ' + @UserCode + ' has no role in company ' + ISNULL(@CompanyCode, '?') + '.';
    ELSE SET @reason = 'STERP-E-0001: User ' + @UserCode + ' does not hold permission ' + @PermissionCode + ' for company ' + ISNULL(@CompanyCode, '?') + ISNULL(' site ' + @SiteCode, '') + '.';
    SELECT @UserCode AS UserCode, @PermissionCode AS PermissionCode, @CompanyCode AS CompanyCode, @SiteCode AS SiteCode, @allowed AS Allowed, @reason AS Reason;
END
GO
CREATE OR ALTER PROCEDURE core.usp_GetUserContext @WindowsLogin varchar(128) = NULL, @UserCode varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @UserCode IS NULL AND @WindowsLogin IS NULL SET @WindowsLogin = SUSER_SNAME();
    IF @UserCode IS NULL SELECT @UserCode = UserCode FROM core.AppUser WHERE WindowsLogin = @WindowsLogin;
    IF @UserCode IS NULL OR NOT EXISTS (SELECT 1 FROM core.AppUser WHERE UserCode = @UserCode AND IsActive = 1)
        THROW 50002, 'STERP-E-0002: No active STERP user is mapped to this sign-on. Ask the business administrator to register it.', 1;
    SELECT u.UserCode, u.DisplayName, u.WindowsLogin, u.EmailAddress, u.JobTitle, u.DefaultCompanyCode, u.DefaultSiteCode, u.IsReadOnly, u.IsActive,
           CAST(SYSDATETIME() AS date) AS BusinessDate FROM core.AppUser u WHERE u.UserCode = @UserCode;
    SELECT DISTINCT c.CompanyCode, c.Name AS CompanyName, s.SiteCode, s.Name AS SiteName, s.Nation, s.CalendarCode, c.BaseCurrency
      FROM core.UserRole ur
      JOIN core.Company c ON (c.CompanyCode = ur.CompanyCode OR ur.CompanyCode = '*') AND c.IsActive = 1
      JOIN core.Site s ON s.CompanyCode = c.CompanyCode AND (ur.SiteCode = '*' OR ur.SiteCode = s.SiteCode) AND s.IsActive = 1
     WHERE ur.UserCode = @UserCode AND ur.ValidFrom <= CAST(SYSDATETIME() AS date) AND (ur.ValidTo IS NULL OR ur.ValidTo >= CAST(SYSDATETIME() AS date))
     ORDER BY c.CompanyCode, s.SiteCode;
END
GO
