/* core.usp_UiUserPrefGet - one client preference value (window size, last company). */
CREATE OR ALTER PROCEDURE core.usp_UiUserPrefGet
    @UserCode varchar(20),
    @PrefKey  varchar(60)
AS
BEGIN
    SET NOCOUNT ON;
    SELECT PrefValue FROM core.UserPreference WHERE UserCode = @UserCode AND PrefKey = @PrefKey;
END
GO
