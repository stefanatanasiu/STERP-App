/*  core.fn_AddBusinessDays - add (or subtract) business days on a named calendar.  With @Days = 0
    the date rolls forward to the next business day.  Dates outside the seeded calendar fall back
    to a weekend rule.  REQ: COM-026, AP-026, AR-025 */
SET NOCOUNT ON;
GO
CREATE OR ALTER FUNCTION core.fn_AddBusinessDays (@CalendarCode varchar(10), @FromDate date, @Days int)
RETURNS date
AS
BEGIN
    DECLARE @d date = @FromDate, @left int = ABS(ISNULL(@Days, 0)), @step int = CASE WHEN ISNULL(@Days, 0) < 0 THEN -1 ELSE 1 END, @bd bit, @guard int = 0;
    WHILE @guard < 4000
    BEGIN
        SELECT @bd = IsBusinessDay FROM core.CalendarDay WHERE CalendarCode = @CalendarCode AND CalDate = @d;
        IF @bd IS NULL SET @bd = CASE WHEN ((DATEPART(dw, @d) + @@DATEFIRST) % 7) IN (0, 1) THEN 0 ELSE 1 END;
        IF @left = 0 AND @bd = 1 BREAK;
        SET @d = DATEADD(day, CASE WHEN @left = 0 THEN 1 ELSE @step END, @d);
        IF @left > 0
        BEGIN
            SELECT @bd = IsBusinessDay FROM core.CalendarDay WHERE CalendarCode = @CalendarCode AND CalDate = @d;
            IF @bd IS NULL SET @bd = CASE WHEN ((DATEPART(dw, @d) + @@DATEFIRST) % 7) IN (0, 1) THEN 0 ELSE 1 END;
            IF @bd = 1 SET @left = @left - 1;
        END
        SET @guard = @guard + 1;
    END
    RETURN @d;
END
GO
