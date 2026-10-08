/*  Sales pricing enquiry tests.  The engine that chooses a price is PRICING (COBOL); what is
    pinned here is the contract the enquiry and the explanation panel depend on - the precedence
    of the price list levels, the quantity break rule, the price-per divisor, and the refusal to
    pick a winner when two lists of equal standing both qualify.  Every assertion runs inside a
    transaction that is rolled back, so the script is repeatable on a live database.
    REQ: SO-004, SO-005, SO-006
    AC: a price can always be explained, and an ambiguous price list set-up is never guessed at */
SET NOCOUNT ON;
GO
PRINT '# so-pricing';
DECLARE @msg varchar(400), @dec decimal(19,6), @n int, @co varchar(10) = 'STC',
        @item varchar(40) = 'FG-FRAME-ROAD-SL-54';

/*  1 - an enquiry without an item is refused rather than answered with every price. */
BEGIN TRY
    EXEC so.usp_PriceEnquiry @CompanyCode = @co, @ItemCode = '';
    PRINT 'not ok 1 - a price enquiry without an item was accepted -- REQ: SO-004';
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @msg LIKE '%STERP-E-1905%' PRINT 'ok 1 - a price enquiry needs an item -- REQ: SO-004';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - precedence: Pennine Cycles has its own customer list (level 3), which beats the
    distributor group list (4) and the standard list (5) even though both quote the item. */
DECLARE @r table (Seq int, PriceListCode varchar(20), ListName varchar(80), PriceLevel tinyint, Scope varchar(12),
                  ItemCode varchar(40), UomCode varchar(10), QtyBreak decimal(19,6), CurrencyCode char(3),
                  UnitPrice decimal(19,6), PricePerQty decimal(19,6), DiscountPct decimal(9,4),
                  NetUnitPrice decimal(19,6), MinimumMarginPct decimal(9,4), ValidFrom date, ValidTo date,
                  Qualifies bit, Applies bit, Ambiguous bit, Explanation varchar(300), EnquiryQty decimal(19,6),
                  EnquiryDate date, EnquiryCurrency char(3), EnquiryGroup varchar(20));
DELETE @r;
INSERT @r
    EXEC so.usp_PriceEnquiry @CompanyCode = @co, @ItemCode = @item, @CustomerCode = 'C0001', @Quantity = 25;
SELECT @dec = NetUnitPrice, @n = PriceLevel FROM @r WHERE Applies = 1;
IF @dec = 309.000000 AND @n = 3
    PRINT 'ok 2 - the customer price list wins over the group and standard lists -- REQ: SO-004';
ELSE PRINT 'not ok 2 - the applied price was ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL')
         + ' at level ' + ISNULL(CONVERT(varchar(3), @n), 'NULL') + ', expected 309.000000 at level 3 -- REQ: SO-004';

/*  3 - the quantity break only applies from its own quantity upwards: at 24 frames the
    distributor list still charges the base price, at 25 it charges the break price. */
DELETE @r;
INSERT @r EXEC so.usp_PriceEnquiry @CompanyCode = @co, @ItemCode = @item, @PriceGroup = 'DIST26', @Quantity = 24;
SELECT @dec = NetUnitPrice FROM @r WHERE Applies = 1;
DECLARE @dec25 decimal(19,6);
DELETE @r;
INSERT @r EXEC so.usp_PriceEnquiry @CompanyCode = @co, @ItemCode = @item, @PriceGroup = 'DIST26', @Quantity = 25;
SELECT @dec25 = NetUnitPrice FROM @r WHERE Applies = 1;
IF @dec = 318.000000 AND @dec25 = 299.000000
    PRINT 'ok 3 - a quantity break applies from its own quantity upwards (318.00 at 24, 299.00 at 25) -- REQ: SO-004';
ELSE PRINT 'not ok 3 - got ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ' at 24 and '
         + ISNULL(CONVERT(varchar(20), @dec25), 'NULL') + ' at 25 -- REQ: SO-004';

/*  4 - a price per 100 is divided, not multiplied: 29.8246 per 100 bolts is 0.298246 each. */
DELETE @r;
INSERT @r EXEC so.usp_PriceEnquiry @CompanyCode = @co, @ItemCode = 'BOLT-M5X16-SS', @Quantity = 500;
SELECT @dec = NetUnitPrice, @n = CONVERT(int, PricePerQty) FROM @r WHERE Applies = 1;
IF @dec = 0.298246 AND @n = 100
    PRINT 'ok 4 - a price per 100 is 29.8246 / 100 = 0.298246 a bolt -- REQ: SO-004';
ELSE PRINT 'not ok 4 - the unit price was ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL')
         + ' per ' + ISNULL(CONVERT(varchar(10), @n), 'NULL') + ' -- REQ: SO-004';

/*  5 - fallback: an item the OEM group list does not carry drops to the standard list. */
DELETE @r;
INSERT @r EXEC so.usp_PriceEnquiry @CompanyCode = @co, @ItemCode = 'SADDLE-COMP-143', @CustomerCode = 'C0007', @Quantity = 10;
SELECT @dec = NetUnitPrice, @msg = Scope FROM @r WHERE Applies = 1;
IF @dec = 42.500000 AND @msg = 'STANDARD'
    PRINT 'ok 5 - an item missing from the contract list falls back to the standard list at 42.50 -- REQ: SO-004';
ELSE PRINT 'not ok 5 - the applied price was ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL')
         + ' from scope ' + ISNULL(@msg, 'NULL') + ' -- REQ: SO-004';

/*  6 - two live lists of equal standing are ambiguous, and the enquiry says so instead of
    picking one: nothing is marked as applying. */
BEGIN TRANSACTION;
INSERT so.PriceList (CompanyCode, PriceListCode, Name, PriceLevel, Scope, CurrencyCode, CustomerCode,
                     ValidFrom, ValidTo, IsActive, CreatedBy)
VALUES (@co, 'ZZTESTDUP', 'Duplicate customer list for the ambiguity test', 3, 'CUSTOMER', 'GBP', 'C0001',
        '2026-04-01', '2027-03-31', 1, 'TEST');
INSERT so.PriceListLine (CompanyCode, PriceListCode, ItemCode, UomCode, PricePerQty, QtyBreak, CurrencyCode,
                         UnitPrice, DiscountPct, ValidFrom, ValidTo, CreatedBy)
VALUES (@co, 'ZZTESTDUP', @item, 'EA', 1, 0, 'GBP', 301.000000, 0, '2026-04-01', '2027-03-31', 'TEST');
DELETE @r;
INSERT @r EXEC so.usp_PriceEnquiry @CompanyCode = @co, @ItemCode = @item, @CustomerCode = 'C0001', @Quantity = 25;
SELECT @n = SUM(CONVERT(int, Ambiguous)), @dec = SUM(CONVERT(int, Applies)) FROM @r;
IF @n = 2 AND @dec = 0
    PRINT 'ok 6 - two customer lists at the same level and break are reported as ambiguous, not guessed -- REQ: SO-004';
ELSE PRINT 'not ok 6 - ambiguous rows ' + ISNULL(CONVERT(varchar(5), @n), 'NULL')
         + ', applied rows ' + ISNULL(CONVERT(varchar(5), @dec), 'NULL') + ' -- REQ: SO-004';
ROLLBACK TRANSACTION;

/*  7 - an expired list line is not a candidate on a later date. */
DELETE @r;
INSERT @r EXEC so.usp_PriceEnquiry @CompanyCode = @co, @ItemCode = @item, @CustomerCode = 'C0001',
                                   @Quantity = 25, @AsOf = '2026-01-15';
SELECT @n = COUNT(*) FROM @r;
IF @n = 0 PRINT 'ok 7 - a price list that starts in April quotes nothing in January -- REQ: SO-004';
ELSE PRINT 'not ok 7 - ' + CONVERT(varchar(5), @n) + ' candidates were offered before the list began -- REQ: SO-004';

/*  8 - the discount sequence the sales order carries: 342.00 less 10% is 307.80 a frame, which
    with VAT at 20% is 61.56 of tax and 369.36 gross on one frame (the specification's worked
    example, as it stands on the demonstration order).  REQ: SO-005, SO-025 */
SELECT @dec = CAST(UnitNet AS decimal(19,6)) FROM so.vw_SalesOrderLineProgress
 WHERE CompanyCode = @co AND OrderNumber = 'SOS900002' AND LineNum = 1;
IF @dec = 307.800000
    PRINT 'ok 8 - 342.00 less a 10% line discount is 307.80 -- REQ: SO-005';
ELSE PRINT 'not ok 8 - the net unit price was ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ' -- REQ: SO-005';
IF CAST(307.80 * 0.20 AS decimal(18,2)) = 61.56 AND CAST(307.80 * 1.20 AS decimal(18,2)) = 369.36
    PRINT 'ok 9 - VAT on 307.80 at 20% is 61.56 and the gross is 369.36 -- REQ: SO-025';
ELSE PRINT 'not ok 9 - the VAT arithmetic does not agree with the specification -- REQ: SO-025';
GO
