/*  core.fn_HasPermission - does a user hold a permission for this company (and site)?
    Site '*' on the role assignment means every site.  REQ: COM-002, COM-019 */
SET NOCOUNT ON;
GO
CREATE OR ALTER FUNCTION core.fn_HasPermission (@UserCode varchar(20), @PermissionCode varchar(40), @CompanyCode varchar(10), @SiteCode varchar(10))
RETURNS bit
AS
BEGIN
    DECLARE @today date = CAST(SYSDATETIME() AS date);
    IF NOT EXISTS (SELECT 1 FROM core.AppUser WHERE UserCode = @UserCode AND IsActive = 1) RETURN 0;
    IF EXISTS (SELECT 1 FROM core.UserRole ur
                 JOIN core.Role r ON r.RoleCode = ur.RoleCode AND r.IsActive = 1
                 JOIN core.RolePermission rp ON rp.RoleCode = ur.RoleCode AND rp.PermissionCode = @PermissionCode
                WHERE ur.UserCode = @UserCode AND (ur.CompanyCode = @CompanyCode OR ur.CompanyCode = '*')
                  AND (ur.SiteCode = '*' OR @SiteCode IS NULL OR ur.SiteCode = @SiteCode)
                  AND ur.ValidFrom <= @today AND (ur.ValidTo IS NULL OR ur.ValidTo >= @today)) RETURN 1;
    RETURN 0;
END
GO
