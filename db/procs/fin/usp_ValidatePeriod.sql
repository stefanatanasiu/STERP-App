/*  fin.usp_ValidatePeriod - a business event date is not a posting authority.  OPEN posts
    normally, SOFT needs explicit close-adjustment authority, HARD and FUTURE block.
    REQ: COM-009, FIN-010, D-15 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE fin.usp_ValidatePeriod @CompanyCode varchar(10), @PostingDate date, @UserCode varchar(20) = NULL, @Action varchar(12) = 'POST'
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    DECLARE @fy smallint, @pn tinyint, @st varchar(8);
    SELECT @fy = FiscalYear, @pn = PeriodNo, @st = Status FROM fin.Period WHERE CompanyCode = @CompanyCode AND @PostingDate BETWEEN StartDate AND EndDate;
    IF @st IS NULL THROW 50009, 'STERP-E-0009: No financial period covers that posting date for this company.', 1;
    IF @st = 'HARD' THROW 50009, 'STERP-E-0009: That accounting period is hard closed. Post the correction in the current open period or ask the financial controller to reopen it.', 1;
    IF @st = 'FUTURE' THROW 50009, 'STERP-E-0009: That accounting period is not yet open for posting.', 1;
    IF @st = 'SOFT' AND (@UserCode IS NULL OR core.fn_HasPermission(@UserCode, 'FIN.PERIOD.SOFTPOST', @CompanyCode, NULL) = 0)
        THROW 50009, 'STERP-E-0009: That accounting period is soft closed. Only an authorised close adjustment may be posted into it.', 1;
    SELECT @CompanyCode AS CompanyCode, @fy AS FiscalYear, @pn AS PeriodNo, @st AS Status, CAST(1 AS bit) AS Allowed, @Action AS Action;
END
GO
