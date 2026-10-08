/* core.usp_UiCheckAccess - single point where the client framework asks the security
   module whether a permission is granted.  Returns @Allowed = 1 when no permission is
   required or when the security module is not deployed yet; otherwise it answers from
   core.fn_HasPermission (falling back to core.usp_CheckPermission) and denies when the
   security module cannot answer at all.                                        COM-002 */
CREATE OR ALTER PROCEDURE core.usp_UiCheckAccess
    @UserCode       varchar(20),
    @PermissionCode varchar(60) = NULL,
    @CompanyCode    varchar(10) = NULL,
    @SiteCode       varchar(10) = NULL,
    @Allowed        bit OUTPUT
AS
BEGIN
    SET NOCOUNT ON;
    SET @Allowed = 1;
    IF @PermissionCode IS NULL OR LTRIM(RTRIM(@PermissionCode)) = '' RETURN;

    IF OBJECT_ID(N'core.fn_HasPermission') IS NOT NULL
    BEGIN
        /* indirect so this procedure still compiles before the security module is deployed,
           and an OUTPUT parameter so callers may wrap us in their own INSERT ... EXEC */
        DECLARE @a bit;
        EXEC sp_executesql
            N'SELECT @r = core.fn_HasPermission(@u, @p, @c, @s);',
            N'@u varchar(20), @p varchar(60), @c varchar(10), @s varchar(10), @r bit OUTPUT',
            @u = @UserCode, @p = @PermissionCode, @c = @CompanyCode, @s = @SiteCode, @r = @a OUTPUT;
        SET @Allowed = ISNULL(@a, 0);
        RETURN;
    END

    IF OBJECT_ID(N'core.usp_CheckPermission') IS NULL RETURN;

    DECLARE @r table (UserCode varchar(20) NULL, PermissionCode varchar(60) NULL,
                      CompanyCode varchar(10) NULL, SiteCode varchar(10) NULL,
                      Allowed bit NULL, Reason varchar(400) NULL);
    BEGIN TRY
        INSERT @r (UserCode, PermissionCode, CompanyCode, SiteCode, Allowed, Reason)
            EXEC core.usp_CheckPermission @UserCode = @UserCode, @PermissionCode = @PermissionCode,
                                          @CompanyCode = @CompanyCode, @SiteCode = @SiteCode;
        SELECT @Allowed = CONVERT(bit, ISNULL(MIN(CONVERT(tinyint, Allowed)), 0)) FROM @r;
    END TRY
    BEGIN CATCH
        /* a security check that cannot be answered denies access (COM-002) */
        SET @Allowed = 0;
    END CATCH
END
GO
