/*  so.usp_PriceEnquiry - the candidate list behind the F2 price enquiry and the price
    explanation panel.  It returns every price list line that could apply to an item for a
    customer on a date, in the precedence PRICING walks (level 2 contract, 3 customer, 4 group,
    5 standard; within a level, the largest qualifying quantity break wins).  It deliberately
    does NOT choose: the chosen price, the discount sequence and the margin test belong to
    PRICING, SODISC and SOMARGN, and a screen that disagreed with the engine would be a bug.
    The Applies column marks the row PRICING would take, so a salesperson can see the
    precedence; Ambiguous marks two live candidates of equal standing, which the engine blocks.
    REQ: SO-004, SO-005, SO-006 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE so.usp_PriceEnquiry
    @CompanyCode  varchar(10),
    @ItemCode     varchar(40),
    @CustomerCode varchar(20) = NULL,
    @PriceGroup   varchar(20) = NULL,
    @Quantity     decimal(19,6) = 1,
    @AsOf         date        = NULL,
    @CurrencyCode char(3)     = NULL,
    @UserCode     varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;

    IF @CompanyCode IS NULL OR @CompanyCode = ''
        THROW 51901, 'STERP-E-1901: A company must be given for a price enquiry.', 1;
    IF @ItemCode IS NULL OR @ItemCode = ''
        THROW 51905, 'STERP-E-1905: An item must be given for a price enquiry.', 1;

    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'SO.ENQUIRY.VIEW', @CompanyCode, NULL) = 0
        THROW 51903, 'STERP-E-1903: You are not permitted to enquire on sales prices in this company.', 1;

    SET @AsOf = ISNULL(@AsOf, CAST(SYSDATETIME() AS date));
    SET @Quantity = CASE WHEN ISNULL(@Quantity, 0) <= 0 THEN 1 ELSE @Quantity END;
    /*  The customer's own list code doubles as a price group in this installation - the sales
        ledger carries it on ar.Customer.PriceListCode - so either may be supplied. */
    IF @PriceGroup IS NULL AND @CustomerCode IS NOT NULL
        SELECT @PriceGroup = ISNULL(c.PriceGroup, c.PriceListCode)
          FROM ar.Customer c WHERE c.CompanyCode = @CompanyCode AND c.CustomerCode = @CustomerCode;
    IF @CurrencyCode IS NULL AND @CustomerCode IS NOT NULL
        SELECT @CurrencyCode = c.CurrencyCode
          FROM ar.Customer c WHERE c.CompanyCode = @CompanyCode AND c.CustomerCode = @CustomerCode;
    SET @CurrencyCode = ISNULL(@CurrencyCode, 'GBP');

    DECLARE @cand table (Seq int IDENTITY(1,1) PRIMARY KEY, PriceListCode varchar(20), ListName varchar(80),
                         PriceLevel tinyint, Scope varchar(12), ItemCode varchar(40), UomCode varchar(10) NULL,
                         QtyBreak decimal(19,6), CurrencyCode char(3), UnitPrice decimal(19,6),
                         PricePerQty decimal(19,6), DiscountPct decimal(9,4), NetUnitPrice decimal(19,6),
                         MinimumMarginPct decimal(9,4) NULL, ValidFrom date, ValidTo date NULL,
                         Qualifies bit);
    INSERT @cand (PriceListCode, ListName, PriceLevel, Scope, ItemCode, UomCode, QtyBreak, CurrencyCode,
                  UnitPrice, PricePerQty, DiscountPct, NetUnitPrice, MinimumMarginPct, ValidFrom, ValidTo, Qualifies)
    SELECT e.PriceListCode, e.Name, e.PriceLevel, e.Scope, e.ItemCode, e.UomCode, e.QtyBreak, e.CurrencyCode,
           e.UnitPrice, e.PricePerQty, e.DiscountPct, e.NetUnitPrice, e.MinimumMarginPct, e.ValidFrom, e.ValidTo,
           Qualifies = CASE WHEN e.QtyBreak <= @Quantity THEN 1 ELSE 0 END
      FROM so.vw_PriceListEffective e
     WHERE e.CompanyCode = @CompanyCode
       AND e.ItemCode = @ItemCode
       AND e.IsActive = 1
       AND e.CurrencyCode = @CurrencyCode
       AND e.ListValidFrom <= @AsOf AND (e.ListValidTo IS NULL OR e.ListValidTo >= @AsOf)
       AND e.ValidFrom     <= @AsOf AND (e.ValidTo     IS NULL OR e.ValidTo     >= @AsOf)
       AND (e.Scope = 'STANDARD'
         OR (e.Scope IN ('CONTRACT','CUSTOMER') AND e.CustomerCode = @CustomerCode)
         OR (e.Scope = 'GROUP' AND e.PriceGroup = @PriceGroup))
     ORDER BY e.PriceLevel, e.QtyBreak DESC, e.PriceListCode;

    /*  The winner: lowest level, then the largest break the quantity reaches.  Two qualifying
        rows at the same level and the same break are an ambiguous price list set-up, which
        PRICING refuses rather than silently picking one (SO-004). */
    DECLARE @bestLevel tinyint, @bestBreak decimal(19,6), @bestCount int = 0;
    SELECT TOP (1) @bestLevel = PriceLevel, @bestBreak = QtyBreak
      FROM @cand WHERE Qualifies = 1 ORDER BY PriceLevel, QtyBreak DESC;
    SELECT @bestCount = COUNT(*) FROM @cand
     WHERE Qualifies = 1 AND PriceLevel = @bestLevel AND QtyBreak = @bestBreak;

    SELECT Seq, PriceListCode, ListName, PriceLevel, Scope, ItemCode, UomCode, QtyBreak, CurrencyCode,
           UnitPrice, PricePerQty, DiscountPct, NetUnitPrice, MinimumMarginPct, ValidFrom, ValidTo,
           Qualifies,
           Applies = CASE WHEN Qualifies = 1 AND PriceLevel = @bestLevel AND QtyBreak = @bestBreak
                            AND @bestCount = 1 THEN 1 ELSE 0 END,
           Ambiguous = CASE WHEN Qualifies = 1 AND PriceLevel = @bestLevel AND QtyBreak = @bestBreak
                              AND @bestCount > 1 THEN 1 ELSE 0 END,
           Explanation = CASE WHEN Qualifies = 0
                              THEN 'Quantity break ' + CONVERT(varchar(20), CONVERT(decimal(19,3), QtyBreak))
                                   + ' not reached'
                              ELSE LOWER(Scope) + ' price list ' + PriceListCode
                                   + ' level ' + CONVERT(varchar(3), PriceLevel)
                                   + ', break ' + CONVERT(varchar(20), CONVERT(decimal(19,3), QtyBreak))
                                   + CASE WHEN DiscountPct <> 0
                                          THEN ', list discount ' + CONVERT(varchar(10), CONVERT(decimal(9,2), DiscountPct)) + '%'
                                          ELSE '' END END,
           EnquiryQty = @Quantity, EnquiryDate = @AsOf, EnquiryCurrency = @CurrencyCode,
           EnquiryGroup = @PriceGroup
      FROM @cand
     ORDER BY Seq;
END
GO
PRINT 'Deployed so.usp_PriceEnquiry';
GO
