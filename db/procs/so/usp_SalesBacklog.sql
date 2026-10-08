/*  so.usp_SalesBacklog - the aggregation behind RPT-008 and the backlog panel on the sales desk.
    The backlog is a set operation over the open order book, so it lives in SQL; what counts as
    outstanding (accepted less cancelled less despatched) is defined once in so.vw_OpenOrderBook
    and never recomputed here.  A held order is still backlog: the customer is owed the goods
    whether or not the warehouse may act (SO-003).
    REQ: SO-015, SO-019, SO-022, SO-023, RPT-008 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE so.usp_SalesBacklog
    @CompanyCode  varchar(10),
    @GroupBy      varchar(12) = 'ORDER',
    @SiteCode     varchar(10) = NULL,
    @CustomerCode varchar(20) = NULL,
    @ItemCode     varchar(40) = NULL,
    @FromDate     date        = NULL,
    @ToDate       date        = NULL,
    @LateOnly     bit         = 0,
    @UserCode     varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;

    IF @CompanyCode IS NULL OR @CompanyCode = ''
        THROW 51901, 'STERP-E-1901: A company must be given for a sales backlog enquiry.', 1;

    SET @GroupBy = UPPER(ISNULL(NULLIF(@GroupBy, ''), 'ORDER'));
    IF @GroupBy NOT IN ('ORDER', 'CUSTOMER', 'ITEM', 'SITE', 'PERIOD')
        THROW 51904, 'STERP-E-1904: The sales backlog groups by ORDER, CUSTOMER, ITEM, SITE or PERIOD.', 1;

    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'SO.ENQUIRY.VIEW', @CompanyCode, @SiteCode) = 0
        THROW 51903, 'STERP-E-1903: You are not permitted to enquire on sales orders in this company.', 1;

    /*  The promise date window is the one a planner thinks in - when the goods are due, not when
        the order was taken. */
    DECLARE @book table (SiteCode varchar(10), OrderNumber varchar(30), CustomerCode varchar(20),
                         ItemCode varchar(40), Description varchar(200), CurrencyCode char(3),
                         CurrentPromiseDate date, OriginalPromiseDate date, OutstandingQty decimal(19,6),
                         BacklogValue decimal(18,2), BacklogValueGBP decimal(18,2),
                         IsLate bit, IsRescheduled bit, LineCount int);
    INSERT @book
    SELECT b.SiteCode, b.OrderNumber, b.CustomerCode, b.ItemCode, b.Description, b.CurrencyCode,
           b.CurrentPromiseDate, b.OriginalPromiseDate, b.OutstandingQty, b.BacklogValue,
           b.BacklogValueGBP, b.IsLate, b.IsRescheduled, 1
      FROM so.vw_OpenOrderBook b
     WHERE b.CompanyCode = @CompanyCode
       AND (@SiteCode     IS NULL OR b.SiteCode     = @SiteCode)
       AND (@CustomerCode IS NULL OR b.CustomerCode = @CustomerCode)
       AND (@ItemCode     IS NULL OR b.ItemCode     = @ItemCode)
       AND (@FromDate     IS NULL OR b.CurrentPromiseDate >= @FromDate OR b.CurrentPromiseDate IS NULL)
       AND (@ToDate       IS NULL OR b.CurrentPromiseDate <= @ToDate   OR b.CurrentPromiseDate IS NULL)
       AND (@LateOnly = 0 OR b.IsLate = 1);

    IF @GroupBy = 'ORDER'
        SELECT GroupBy = 'ORDER', GroupKey = b.OrderNumber,
               GroupName = ISNULL(c.TradingName, ISNULL(c.LegalName, b.CustomerCode)),
               b.CustomerCode, b.SiteCode, Lines = COUNT(*), OutstandingQty = SUM(b.OutstandingQty),
               BacklogValue = SUM(b.BacklogValue), BacklogValueGBP = SUM(b.BacklogValueGBP),
               EarliestPromise = MIN(b.CurrentPromiseDate), LatestPromise = MAX(b.CurrentPromiseDate),
               LateLines = SUM(CONVERT(int, b.IsLate)), RescheduledLines = SUM(CONVERT(int, b.IsRescheduled))
          FROM @book b
          LEFT JOIN ar.Customer c ON c.CompanyCode = @CompanyCode AND c.CustomerCode = b.CustomerCode
         GROUP BY b.OrderNumber, b.CustomerCode, b.SiteCode, c.TradingName, c.LegalName
         ORDER BY SUM(b.BacklogValueGBP) DESC;
    ELSE IF @GroupBy = 'CUSTOMER'
        SELECT GroupBy = 'CUSTOMER', GroupKey = b.CustomerCode,
               GroupName = ISNULL(c.TradingName, ISNULL(c.LegalName, b.CustomerCode)),
               b.CustomerCode, SiteCode = CONVERT(varchar(10), NULL), Lines = COUNT(*),
               OutstandingQty = SUM(b.OutstandingQty), BacklogValue = SUM(b.BacklogValue),
               BacklogValueGBP = SUM(b.BacklogValueGBP), EarliestPromise = MIN(b.CurrentPromiseDate),
               LatestPromise = MAX(b.CurrentPromiseDate), LateLines = SUM(CONVERT(int, b.IsLate)),
               RescheduledLines = SUM(CONVERT(int, b.IsRescheduled))
          FROM @book b
          LEFT JOIN ar.Customer c ON c.CompanyCode = @CompanyCode AND c.CustomerCode = b.CustomerCode
         GROUP BY b.CustomerCode, c.TradingName, c.LegalName
         ORDER BY SUM(b.BacklogValueGBP) DESC;
    ELSE IF @GroupBy = 'ITEM'
        SELECT GroupBy = 'ITEM', GroupKey = b.ItemCode, GroupName = MAX(b.Description),
               CustomerCode = CONVERT(varchar(20), NULL), SiteCode = CONVERT(varchar(10), NULL),
               Lines = COUNT(*), OutstandingQty = SUM(b.OutstandingQty), BacklogValue = SUM(b.BacklogValue),
               BacklogValueGBP = SUM(b.BacklogValueGBP), EarliestPromise = MIN(b.CurrentPromiseDate),
               LatestPromise = MAX(b.CurrentPromiseDate), LateLines = SUM(CONVERT(int, b.IsLate)),
               RescheduledLines = SUM(CONVERT(int, b.IsRescheduled))
          FROM @book b GROUP BY b.ItemCode
         ORDER BY SUM(b.BacklogValueGBP) DESC;
    ELSE IF @GroupBy = 'SITE'
        SELECT GroupBy = 'SITE', GroupKey = b.SiteCode, GroupName = ISNULL(s.Name, b.SiteCode),
               CustomerCode = CONVERT(varchar(20), NULL), b.SiteCode, Lines = COUNT(*),
               OutstandingQty = SUM(b.OutstandingQty), BacklogValue = SUM(b.BacklogValue),
               BacklogValueGBP = SUM(b.BacklogValueGBP), EarliestPromise = MIN(b.CurrentPromiseDate),
               LatestPromise = MAX(b.CurrentPromiseDate), LateLines = SUM(CONVERT(int, b.IsLate)),
               RescheduledLines = SUM(CONVERT(int, b.IsRescheduled))
          FROM @book b
          LEFT JOIN core.Site s ON s.CompanyCode = @CompanyCode AND s.SiteCode = b.SiteCode
         GROUP BY b.SiteCode, s.Name
         ORDER BY SUM(b.BacklogValueGBP) DESC;
    ELSE
        /*  PERIOD buckets on the current promise, which is what a fulfilment review works through. */
        SELECT GroupBy = 'PERIOD',
               GroupKey = CONVERT(varchar(7), ISNULL(b.CurrentPromiseDate, '1900-01-01'), 126),
               GroupName = CASE WHEN b.CurrentPromiseDate IS NULL THEN 'No promise date'
                                ELSE DATENAME(month, b.CurrentPromiseDate) + ' ' +
                                     CONVERT(varchar(4), YEAR(b.CurrentPromiseDate)) END,
               CustomerCode = CONVERT(varchar(20), NULL), SiteCode = CONVERT(varchar(10), NULL),
               Lines = COUNT(*), OutstandingQty = SUM(b.OutstandingQty), BacklogValue = SUM(b.BacklogValue),
               BacklogValueGBP = SUM(b.BacklogValueGBP), EarliestPromise = MIN(b.CurrentPromiseDate),
               LatestPromise = MAX(b.CurrentPromiseDate), LateLines = SUM(CONVERT(int, b.IsLate)),
               RescheduledLines = SUM(CONVERT(int, b.IsRescheduled))
          FROM @book b
         GROUP BY CONVERT(varchar(7), ISNULL(b.CurrentPromiseDate, '1900-01-01'), 126),
                  CASE WHEN b.CurrentPromiseDate IS NULL THEN 'No promise date'
                       ELSE DATENAME(month, b.CurrentPromiseDate) + ' ' +
                            CONVERT(varchar(4), YEAR(b.CurrentPromiseDate)) END
         ORDER BY 2;
END
GO
PRINT 'Deployed so.usp_SalesBacklog';
GO
