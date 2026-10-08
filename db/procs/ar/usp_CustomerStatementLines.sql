/*  ar.usp_CustomerStatementLines - the lines of a customer statement in either of the two
    presentations the specification requires.  OPENITEM lists what is still outstanding at the
    statement date; TRANSACTION lists the opening balance, every movement in the statement period
    and the closing balance, so that opening plus movement equals closing by construction.  Both
    are rebuilt from ar.fn_OpenItemAsOf, so reprinting an old statement gives the old figures.
    A statement is produced for the statement debtor of the account and never implies a legal
    consolidation of separate debtors (AR-018).
    REQ: AR-017, AR-018, AR-021, AR-027 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE ar.usp_CustomerStatementLines @CompanyCode varchar(10), @CustomerCode varchar(20),
    @AsOf date = NULL, @Style varchar(12) = 'OPENITEM', @UserCode varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'AR.ENQUIRY', @CompanyCode, NULL) = 0
        THROW 50301, 'STERP-E-0301: You are not authorised to enquire on debtor accounts in this company.', 1;
    IF NOT EXISTS (SELECT 1 FROM ar.Customer WHERE CompanyCode = @CompanyCode AND CustomerCode = @CustomerCode)
        THROW 50302, 'STERP-E-0302: That customer account does not exist in this company.', 1;
    SET @Style = UPPER(ISNULL(@Style, 'OPENITEM'));
    IF @Style NOT IN ('OPENITEM', 'TRANSACTION')
        THROW 50306, 'STERP-E-0306: Unknown statement style. Use OPENITEM or TRANSACTION.', 1;
    SET @AsOf = ISNULL(@AsOf, CAST(GETDATE() AS date));
    DECLARE @From date = DATEFROMPARTS(YEAR(@AsOf), MONTH(@AsOf), 1), @Prior date;
    SET @Prior = DATEADD(day, -1, @From);
    /*  1 - statement header with the opening, movement and closing balances. */
    SELECT c.CompanyCode, c.CustomerCode, c.LegalName AS CustomerName, c.StatementCustomerCode, c.CurrencyCode, c.TermsCode,
           c.StatementFrequency, @Style AS Style, @From AS PeriodFrom, @AsOf AS AsOf, ad.Name AS StatementName, ad.Line1,
           ad.Town, ad.County, ad.Postcode, ad.CountryCode, op.OpeningGBP, cl.ClosingGBP, cl.ClosingGBP - op.OpeningGBP AS MovementGBP,
           cl.OverdueGBP, cl.CreditsGBP, cl.ItemCount
      FROM ar.Customer c
     OUTER APPLY (SELECT TOP (1) x.Name, x.Line1, x.Town, x.County, x.Postcode, x.CountryCode FROM ar.CustomerAddress x
                   WHERE x.CompanyCode = c.CompanyCode AND x.CustomerCode = c.CustomerCode
                   ORDER BY CASE x.AddressRole WHEN 'STATEMENT' THEN 0 WHEN 'BILLING' THEN 1 ELSE 2 END, x.AddressId) AS ad
     CROSS APPLY (SELECT ISNULL(SUM(a.ResidualAsOfGBP), 0) FROM ar.fn_OpenItemAsOf(@CompanyCode, @Prior) a
                   WHERE a.CustomerCode = c.CustomerCode) AS op (OpeningGBP)
     CROSS APPLY (SELECT ISNULL(SUM(a.ResidualAsOfGBP), 0), ISNULL(SUM(CASE WHEN a.BucketNo >= 2 THEN a.DebitGBP ELSE 0 END), 0),
                         ISNULL(SUM(a.CreditGBP), 0), ISNULL(COUNT_BIG(*), 0)
                    FROM ar.fn_OpenItemAsOf(@CompanyCode, @AsOf) a WHERE a.CustomerCode = c.CustomerCode)
          AS cl (ClosingGBP, OverdueGBP, CreditsGBP, ItemCount)
     WHERE c.CompanyCode = @CompanyCode AND c.CustomerCode = @CustomerCode;
    /*  2 - the lines themselves.  The transaction style opens with a brought forward line so that
        the running balance on the printed statement starts from the opening figure. */
    IF @Style = 'OPENITEM'
        SELECT 1 AS LineKind, a.OpenItemId, a.ItemType, a.DocumentNumber, a.DocumentDate, a.DueDate, a.CurrencyCode,
               a.OriginalAmount, a.ResidualAsOfGBP AS AmountGBP, a.DebitGBP, a.CreditGBP, a.DaysOverdue, a.BucketNo,
               a.BucketName, a.IsDisputed, a.DisputedAmountGBP,
               SUM(a.ResidualAsOfGBP) OVER (ORDER BY a.DocumentDate, a.DocumentNumber ROWS UNBOUNDED PRECEDING) AS RunningBalanceGBP
          FROM ar.fn_OpenItemAsOf(@CompanyCode, @AsOf) a
         WHERE a.CustomerCode = @CustomerCode ORDER BY a.DocumentDate, a.DocumentNumber;
    ELSE
        SELECT m.LineKind, m.OpenItemId, m.ItemType, m.DocumentNumber, m.DocumentDate, m.DueDate, m.CurrencyCode,
               m.OriginalAmount, m.AmountGBP, m.DebitGBP, m.CreditGBP, m.DaysOverdue, m.BucketNo, m.BucketName, m.IsDisputed,
               m.DisputedAmountGBP, SUM(m.AmountGBP) OVER (ORDER BY m.LineKind, m.DocumentDate, m.DocumentNumber ROWS UNBOUNDED PRECEDING) AS RunningBalanceGBP
          FROM (SELECT 0 AS LineKind, CAST(NULL AS bigint) AS OpenItemId, 'BFWD' AS ItemType, 'BROUGHT FORWARD' AS DocumentNumber,
                       @Prior AS DocumentDate, CAST(NULL AS date) AS DueDate, 'GBP' AS CurrencyCode,
                       CAST(NULL AS decimal(18,2)) AS OriginalAmount, ISNULL(SUM(o.ResidualAsOfGBP), 0) AS AmountGBP,
                       CAST(NULL AS decimal(18,2)) AS DebitGBP, CAST(NULL AS decimal(18,2)) AS CreditGBP, 0 AS DaysOverdue,
                       CAST(NULL AS int) AS BucketNo, CAST(NULL AS varchar(30)) AS BucketName, CAST(0 AS bit) AS IsDisputed,
                       CAST(0 AS decimal(18,2)) AS DisputedAmountGBP
                  FROM ar.fn_OpenItemAsOf(@CompanyCode, @Prior) o WHERE o.CustomerCode = @CustomerCode
                UNION ALL
                SELECT 1, a.OpenItemId, a.ItemType, a.DocumentNumber, a.DocumentDate, a.DueDate, a.CurrencyCode, a.OriginalAmount,
                       a.OriginalAmountGBP, CASE WHEN a.OriginalAmountGBP > 0 THEN a.OriginalAmountGBP ELSE 0 END,
                       CASE WHEN a.OriginalAmountGBP < 0 THEN -a.OriginalAmountGBP ELSE 0 END,
                       CASE WHEN @AsOf > a.DueDate THEN DATEDIFF(day, a.DueDate, @AsOf) ELSE 0 END, NULL, NULL,
                       a.IsDisputed, a.DisputedAmountGBP
                  FROM ar.OpenItem a WHERE a.CompanyCode = @CompanyCode AND a.CustomerCode = @CustomerCode
                   AND a.SettlementStatus <> 'REVERSED' AND a.PostingDate BETWEEN @From AND @AsOf) AS m
         ORDER BY m.LineKind, m.DocumentDate, m.DocumentNumber;
END
GO
