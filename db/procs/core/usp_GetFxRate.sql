/*  core.usp_GetFxRate / core.usp_CheckFxStaleness - exchange rates are quoted as foreign units
    per one pound sterling (D-14).  A missing or zero rate blocks monetary confirmation; no parity
    conversion is ever assumed.  REQ: COM-010, FIN-019 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_GetFxRate @CurrencyCode char(3), @RateDate date, @RateType varchar(6) = 'SPOT', @CompanyCode varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    DECLARE @base char(3) = 'GBP';
    IF @CompanyCode IS NOT NULL SELECT @base = BaseCurrency FROM core.Company WHERE CompanyCode = @CompanyCode;
    IF @CurrencyCode IS NULL OR @RateDate IS NULL THROW 50010, 'STERP-E-0010: A currency and a rate date are required before a monetary document can be confirmed.', 1;
    IF @CurrencyCode = @base
    BEGIN
        SELECT @CurrencyCode AS CurrencyCode, @RateDate AS RateDate, @RateType AS RateType, CAST(1 AS decimal(18,8)) AS UnitsPerGBP, 'Base currency' AS Source, 0 AS AgeDays;
        RETURN;
    END
    DECLARE @r decimal(18,8), @d date, @src varchar(40);
    SELECT TOP (1) @r = UnitsPerGBP, @d = RateDate, @src = Source FROM core.FxRate
     WHERE CurrencyCode = @CurrencyCode AND RateType = @RateType AND RateDate <= @RateDate ORDER BY RateDate DESC;
    IF @r IS NULL OR @r = 0
        THROW 50010, 'STERP-E-0010: No positive exchange rate is held for that currency, rate type and date. Load the rate before confirming the document.', 1;
    SELECT @CurrencyCode AS CurrencyCode, @d AS RateDate, @RateType AS RateType, @r AS UnitsPerGBP, @src AS Source, DATEDIFF(day, @d, @RateDate) AS AgeDays;
END
GO
/*  Daily control: report currencies whose latest spot rate is older than the approved tolerance. */
CREATE OR ALTER PROCEDURE core.usp_CheckFxStaleness @AsOf date = NULL, @MaxAgeDays int = NULL, @RateType varchar(6) = 'SPOT'
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @AsOf = ISNULL(@AsOf, CAST(SYSDATETIME() AS date));
    SET @MaxAgeDays = ISNULL(@MaxAgeDays, TRY_CONVERT(int, core.fn_Param('FX.MAXAGE.DAYS', '*', @AsOf)));
    SET @MaxAgeDays = ISNULL(@MaxAgeDays, 4);
    DECLARE @stale TABLE (CurrencyCode char(3), LatestRateDate date, AgeDays int);
    INSERT @stale (CurrencyCode, LatestRateDate, AgeDays)
    SELECT c.CurrencyCode, x.LatestRateDate, DATEDIFF(day, ISNULL(x.LatestRateDate, '2000-01-01'), @AsOf)
      FROM core.Currency c
      OUTER APPLY (SELECT MAX(f.RateDate) AS LatestRateDate FROM core.FxRate f WHERE f.CurrencyCode = c.CurrencyCode AND f.RateType = @RateType AND f.RateDate <= @AsOf) x
     WHERE c.IsActive = 1 AND c.CurrencyCode <> 'GBP' AND (x.LatestRateDate IS NULL OR DATEDIFF(day, x.LatestRateDate, @AsOf) > @MaxAgeDays);
    INSERT core.ExceptionItem (CompanyCode, Module, Severity, Category, DocumentType, DocumentRef, Message, OwnerRole, Priority, RaisedBy)
    SELECT '*', 'FIN', 'WARNING', 'FX-RATE-STALE', 'CURRENCY', s.CurrencyCode,
           'Exchange rate for ' + s.CurrencyCode + ' is ' + CAST(s.AgeDays AS varchar(10)) + ' days old (limit ' + CAST(@MaxAgeDays AS varchar(10)) + '). Load the latest Bank of England spot rate.',
           'FINCTRL', 3, 'SYSTEM'
      FROM @stale s
     WHERE NOT EXISTS (SELECT 1 FROM core.ExceptionItem e WHERE e.Category = 'FX-RATE-STALE' AND e.DocumentRef = s.CurrencyCode AND e.Status <> 'RESOLVED');
    SELECT CurrencyCode, LatestRateDate, AgeDays, @MaxAgeDays AS MaxAgeDays FROM @stale ORDER BY CurrencyCode;
END
GO
