/*  ap.usp_PaymentPracticesData - store the half yearly payment
    practices and performance figures (RPT-018) that APPPR has just
    calculated, as a new version of the period's report.  The detail
    rows are kept so the headline figures can be defended invoice by
    invoice if the published return is ever challenged.
    REQ: AP-028                                                        */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE ap.usp_PaymentPracticesData
    @CompanyCode    varchar(10),
    @PeriodRef      varchar(30),
    @AverageDays    decimal(9,2),
    @PctWithin30    decimal(9,2),
    @Pct31To60      decimal(9,2),
    @PctOver60      decimal(9,2),
    @PctNotInTerms  decimal(9,2),
    @InvoiceCount   int,
    @TotalValue     decimal(18,2),
    @CreatedBy      varchar(20)
AS
BEGIN
    SET NOCOUNT ON;
    SET XACT_ABORT ON;

    /*  PPR-yyyymmdd-yyyymmdd is the reference APPPR builds */
    DECLARE @PeriodKey  varchar(12),
            @Start      date,
            @End        date,
            @Version    smallint;

    IF (@PeriodRef IS NULL OR LEN(@PeriodRef) < 21)
    BEGIN
        RAISERROR('STERP-E-AP028 PeriodRef must be PPR-from-to', 16, 1);
        RETURN 12;
    END

    SET @Start = TRY_CONVERT(date, SUBSTRING(@PeriodRef, 5, 8));
    SET @End   = TRY_CONVERT(date, SUBSTRING(@PeriodRef, 14, 8));

    IF (@Start IS NULL OR @End IS NULL OR @End < @Start)
    BEGIN
        RAISERROR('STERP-E-AP028 reporting period is not valid', 16, 1);
        RETURN 12;
    END

    SET @PeriodKey = CONVERT(varchar(6), @Start, 112)
                   + '-' + CONVERT(varchar(2), DATEPART(day, @End));

    SELECT  @Version = ISNULL(MAX(Version), 0) + 1
    FROM    ap.PaymentPracticesReport
    WHERE   CompanyCode = @CompanyCode
      AND   ReportingPeriodKey = @PeriodKey;

    BEGIN TRAN;

    INSERT ap.PaymentPracticesReport
        (CompanyCode, ReportingPeriodKey, PeriodStart, PeriodEnd,
         DueDate, AverageDaysToPay, PctPaidWithin30, PctPaid31To60,
         PctPaid61Plus, PctNotPaidWithinTerms, InvoiceCount,
         Status, Version, CreatedAt)
    VALUES
        (@CompanyCode, @PeriodKey, @Start, @End,
         DATEADD(day, 30, @End), @AverageDays, @PctWithin30,
         @Pct31To60, @PctOver60, @PctNotInTerms, @InvoiceCount,
         'DRAFT', @Version, SYSDATETIME());

    DECLARE @ReportId int = SCOPE_IDENTITY();

    /*  keep the supporting detail - one row per counted invoice */
    INSERT ap.PaymentPracticesDetail
        (ReportId, CompanyCode, SupplierCode, RegisterNumber,
         DateReceived, MeasurementStartDate, DueDate, PaymentDate,
         DaysToPay, GrossAmountGBP, PaidAmountGBP)
    SELECT  @ReportId, b.CompanyCode, b.SupplierCode,
            b.DocumentNumber, b.StartDate, b.StartDate,
            b.DueDate, b.PaidDate,
            DATEDIFF(day, b.StartDate, b.PaidDate),
            b.AmountBase, b.AmountBase
    FROM    ap.vw_PaymentPracticesBase b
    WHERE   b.CompanyCode = @CompanyCode
      AND   b.PaidDate >= @Start
      AND   b.PaidDate <= @End;

    COMMIT TRAN;

    SELECT @ReportId AS ReportId, @Version AS Version,
           @PeriodKey AS ReportingPeriodKey;
    RETURN 0;
END
GO
