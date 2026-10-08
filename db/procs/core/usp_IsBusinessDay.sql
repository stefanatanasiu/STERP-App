/*  core.usp_IsBusinessDay - England & Wales, Scotland, Northern Ireland bank holidays and the
    Bacs processing calendar.  REQ: COM-026, AP-026, AR-025 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_IsBusinessDay @CalendarCode varchar(10), @Date date
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF NOT EXISTS (SELECT 1 FROM core.Calendar WHERE CalendarCode = @CalendarCode AND IsActive = 1)
        THROW 50020, 'STERP-E-0020: That business-day calendar is not maintained. Use ENG, SCT, NIR or BACS.', 1;
    DECLARE @bd bit, @type varchar(10), @hol varchar(60);
    SELECT @bd = IsBusinessDay, @type = DayType, @hol = HolidayName FROM core.CalendarDay WHERE CalendarCode = @CalendarCode AND CalDate = @Date;
    IF @bd IS NULL
    BEGIN
        SET @bd = CASE WHEN ((DATEPART(dw, @Date) + @@DATEFIRST) % 7) IN (0, 1) THEN 0 ELSE 1 END;
        SET @type = CASE WHEN @bd = 1 THEN 'WORKING' ELSE 'WEEKEND' END;
    END
    SELECT @CalendarCode AS CalendarCode, @Date AS CheckDate, @bd AS IsBusinessDay, @type AS DayType, @hol AS HolidayName,
           core.fn_AddBusinessDays(@CalendarCode, @Date, 0) AS NextBusinessDay,
           core.fn_AddBusinessDays(@CalendarCode, DATEADD(day, 1, @Date), -1) AS PreviousBusinessDay;
END
GO
