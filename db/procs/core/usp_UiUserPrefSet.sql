/* core.usp_UiUserPrefSet - store a client preference value (re-runnable MERGE). */
CREATE OR ALTER PROCEDURE core.usp_UiUserPrefSet
    @UserCode  varchar(20),
    @PrefKey   varchar(60),
    @PrefValue nvarchar(400) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    UPDATE core.UserPreference SET PrefValue = @PrefValue, ModifiedAt = SYSDATETIME()
    WHERE UserCode = @UserCode AND PrefKey = @PrefKey;
    IF @@ROWCOUNT = 0
        INSERT core.UserPreference (UserCode, PrefKey, PrefValue) VALUES (@UserCode, @PrefKey, @PrefValue);
END
GO
