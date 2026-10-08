/*  ar.usp_OpenItemsForAllocation - the items a receipt, credit note or deposit may be applied to,
    and the cash that is available to apply.  Only open items of the same legal debtor are offered,
    because unapplied credit belonging to another debtor may not be used to clear this one (AR-010,
    section 9).  Settlement discount is offered only while the invoice is inside its discount
    period plus the two working day receipt-timing grace, and the short-payment tolerance a clerk
    may write off without approval is the D-22 figure of GBP 5.00 read from the parameter file.
    Cross-currency pairs are flagged so that the caller must give an explicit conversion
    instruction rather than matching equal numbers (AR-019).  Enquiry only - nothing here posts.
    REQ: AR-010, AR-012, AR-019, D-22 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE ar.usp_OpenItemsForAllocation @CompanyCode varchar(10), @CustomerCode varchar(20),
    @CurrencyCode char(3) = NULL, @UserCode varchar(20) = NULL, @AsOf date = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'AR.ENQUIRY', @CompanyCode, NULL) = 0
        THROW 50301, 'STERP-E-0301: You are not authorised to enquire on debtor accounts in this company.', 1;
    IF NOT EXISTS (SELECT 1 FROM ar.Customer WHERE CompanyCode = @CompanyCode AND CustomerCode = @CustomerCode)
        THROW 50302, 'STERP-E-0302: That customer account does not exist in this company.', 1;
    IF @CurrencyCode IS NOT NULL AND NOT EXISTS (SELECT 1 FROM core.Currency WHERE CurrencyCode = @CurrencyCode AND IsActive = 1)
        THROW 50305, 'STERP-E-0305: That currency is not an active currency in this system.', 1;
    SET @AsOf = ISNULL(@AsOf, CAST(GETDATE() AS date));
    DECLARE @tol decimal(18,2) = ISNULL(TRY_CONVERT(decimal(18,2), core.fn_Param('AR.WRITEOFF.AUTO.GBP', @CompanyCode, @AsOf)), 5.00),
            @grace int = ISNULL(TRY_CONVERT(int, core.fn_Param('AR.DISCOUNT.GRACE.DAYS', @CompanyCode, @AsOf)), 2);
    /*  1 - debit items this customer still owes, oldest due date first. */
    SELECT a.OpenItemId, a.ItemType, a.DocumentNumber, a.DocumentDate, a.DueDate, a.CurrencyCode, a.FxRate, a.OriginalAmount,
           a.ResidualAmount, a.ResidualAmountGBP AS OutstandingGBP, a.SettlementStatus, a.TermsCode, a.DaysOverdue,
           a.BucketNo, a.BucketName, a.IsDisputed, a.DisputedAmountGBP,
           CASE WHEN @CurrencyCode IS NOT NULL AND a.CurrencyCode <> @CurrencyCode THEN 1 ELSE 0 END AS IsCrossCurrency,
           CASE WHEN a.DiscountDueDate IS NOT NULL AND @AsOf <= DATEADD(day, @grace, a.DiscountDueDate) THEN 1 ELSE 0 END AS DiscountAvailable,
           CASE WHEN a.DiscountDueDate IS NOT NULL AND @AsOf <= DATEADD(day, @grace, a.DiscountDueDate) THEN a.DiscountAmount END AS DiscountAmount,
           @tol AS SmallDifferenceToleranceGBP
      FROM ar.vw_OpenItemAged a
     WHERE a.CompanyCode = @CompanyCode AND a.CustomerCode = @CustomerCode AND a.BalanceSide = 'DEBIT'
       AND a.WrittenOff = 0 AND a.ItemType IN ('INVOICE', 'ADJUSTMENT', 'INTEREST')
       AND (@CurrencyCode IS NULL OR a.CurrencyCode = @CurrencyCode)
     ORDER BY a.DueDate, a.DocumentNumber;
    /*  2 - the credits, unapplied receipts and deposits available to apply against them. */
    SELECT a.OpenItemId, a.ItemType, a.DocumentNumber, a.DocumentDate, a.CurrencyCode, a.FxRate, a.OriginalAmount,
           a.UnappliedGBP AS AvailableGBP, -a.ResidualAmount AS AvailableAmount, a.SettlementStatus, a.SourceReceiptId,
           CASE WHEN @CurrencyCode IS NOT NULL AND a.CurrencyCode <> @CurrencyCode THEN 1 ELSE 0 END AS IsCrossCurrency,
           r.ReceiptNumber, r.PaymentMethod, r.ConfirmationState, r.BankReference
      FROM ar.vw_OpenItemAged a
      LEFT JOIN ar.Receipt r ON r.ReceiptId = a.SourceReceiptId
     WHERE a.CompanyCode = @CompanyCode AND a.CustomerCode = @CustomerCode AND a.BalanceSide = 'CREDIT'
       AND ISNULL(r.ConfirmationState, 'CONFIRMED') IN ('CONFIRMED', 'EXPECTED')
       AND (@CurrencyCode IS NULL OR a.CurrencyCode = @CurrencyCode)
     ORDER BY a.DocumentDate, a.DocumentNumber;
    /*  3 - the totals and tolerances the allocation screen needs to preview a residual. */
    SELECT @CompanyCode AS CompanyCode, @CustomerCode AS CustomerCode, @CurrencyCode AS CurrencyCode, @AsOf AS AsOf,
           @tol AS SmallDifferenceToleranceGBP, @grace AS DiscountGraceDays, b.BalanceGBP, b.GrossDebtGBP, b.UnappliedCashGBP,
           b.CreditNoteGBP, b.DepositGBP, b.OverdueGBP, b.HoldFlag, b.CreditState
      FROM ar.vw_CustomerBalance b WHERE b.CompanyCode = @CompanyCode AND b.CustomerCode = @CustomerCode;
END
GO
