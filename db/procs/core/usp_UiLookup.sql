/* core.usp_UiLookup - the generic F2 lookup.  @Argument names a core.MaintDef, so any
   registered master file can be looked up without writing a new procedure.  The
   lookup is a read-only pick list, so it does not need the maintain permission of
   the file it reads (a buyer picks items without being allowed to amend them).   */
CREATE OR ALTER PROCEDURE core.usp_UiLookup
    @CompanyCode varchar(10)   = NULL,
    @SiteCode    varchar(10)   = NULL,
    @Search      nvarchar(100) = NULL,
    @Argument    varchar(30)   = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF @Argument IS NULL OR LTRIM(RTRIM(@Argument)) = ''
        THROW 50099, 'STERP-E-0099: No lookup was configured for this field.', 1;
    EXEC core.usp_UiMaintList @MaintCode = @Argument, @UserCode = NULL,
                              @CompanyCode = @CompanyCode, @Search = @Search, @Top = 200,
                              @ForLookup = 1;
END
GO
