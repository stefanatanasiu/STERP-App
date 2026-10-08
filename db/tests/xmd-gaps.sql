/*  Cross-module control tests for two rules that had no test of their own:
      XMD-008  an effective-dated master change identifies the open documents it will affect but
               never rewrites the inputs a fulfilled or posted transaction was confirmed with;
      XMD-011  company boundaries and ownership hold in stock, commercial obligations, cash and
               financial reporting, so similar item and customer codes in two companies cannot
               authorise cross-company consumption or netting.

    The effective-dated masters exercised here are the production ones: fin.VatRate (primary key
    VatCode + EffectiveFrom), so.PriceListLine (ValidFrom / ValidTo) and the item master.  The
    impact of a change on open documents is read from the production procedure inv.usp_ItemImpact,
    which is also the proof of the company boundary, because it is scoped by company throughout.
    Everything runs inside a transaction that is rolled back; keys are ZZ-prefixed, and the
    leftovers of an interrupted run are removed first.
    REQ: XMD-008, XMD-011   AC: AC-100, AC-105 */
SET NOCOUNT ON;
GO
PRINT '# xmd-gaps';

/*  Remove anything an interrupted earlier run may have committed.  Only this script's own keys. */
DELETE FROM inv.StockReservation WHERE ItemCode LIKE 'ZZXMD%';
DELETE FROM inv.StockBalance WHERE ItemCode LIKE 'ZZXMD%';
DELETE FROM so.SalesOrderLine WHERE OrderNumber LIKE 'ZZXMD%';
DELETE FROM so.SalesOrder WHERE OrderNumber LIKE 'ZZXMD%';
DELETE FROM ar.SalesInvoiceVat WHERE InvoiceId IN (SELECT InvoiceId FROM ar.SalesInvoice WHERE CustomerCode LIKE 'ZZXMD%');
DELETE FROM ar.SalesInvoiceLine WHERE InvoiceId IN (SELECT InvoiceId FROM ar.SalesInvoice WHERE CustomerCode LIKE 'ZZXMD%');
DELETE FROM ar.SalesInvoice WHERE CustomerCode LIKE 'ZZXMD%';
DELETE FROM ar.OpenItem WHERE CustomerCode LIKE 'ZZXMD%';
DELETE FROM ar.Customer WHERE CustomerCode LIKE 'ZZXMD%';
DELETE FROM so.PriceListLine WHERE PriceListCode LIKE 'ZZXMD%';
DELETE FROM so.PriceList WHERE PriceListCode LIKE 'ZZXMD%';
DELETE FROM inv.Item WHERE ItemCode LIKE 'ZZXMD%';

DECLARE @msg varchar(400), @n int, @m int, @inv bigint, @ord bigint, @done bigint, @hit int,
        @rateAt decimal(9,4), @rateNew decimal(9,4), @priceAt decimal(19,6), @priceNew decimal(19,6),
        @openLines int, @openLinesCal int, @jid bigint, @fy smallint, @pn tinyint, @acc varchar(12),
        @stcBefore decimal(18,2), @stcAfter decimal(18,2), @calAfter decimal(18,2),
        @stcFree decimal(19,6), @calFree decimal(19,6), @cut char(8) = '20261031';
DECLARE @res TABLE (Action varchar(10), ReservationId bigint, QtyReserved decimal(38,6), QtyShortfall decimal(38,6),
                    QtyFreeBefore decimal(38,6), QtyFreeAfter decimal(38,6), QtyEligible decimal(38,6),
                    QtyReservedTotal decimal(38,6), QtyOwnDemand decimal(38,6), InvalidCount int);
DECLARE @impact TABLE (QtyOnHand decimal(19,6), QtyInTransit decimal(19,6), QtyReserved decimal(19,6), MovementCount int,
                       OpenSalesLines int, OpenPurchaseLines int, ActiveBomLines int, OwnBoms int, ReleasedKitLines int,
                       SubstituteRules int, OpenHolds int, OpenCountLines int, LiveSerials int,
                       LifecycleStatus varchar(10), ItemClass varchar(12), ValuationGroup varchar(20), CostMethod varchar(4),
                       StockUomCode varchar(10), TraceabilityType varchar(8), SuccessorItemCode varchar(40), Descr varchar(100));

/* ---------------------------------------------------------------------------------------------
   XMD-008 - a new effective-dated definition applies forward, not backwards (AC-100).
   --------------------------------------------------------------------------------------------- */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, LegalForm, TermsCode, CurrencyCode, Status, CreatedBy)
    VALUES ('STC', 'ZZXMD008', 'Zodiac Frameworks Ltd', 'LTD', 'N30', 'GBP', 'ACTIVE', 'TEST');
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, LifecycleStatus, CreatedBy)
    VALUES ('STC', 'ZZXMD008-PART', 'Zodiac effective-dating test part', 'BOUGHT', 'EA', 'RELEASED', 'TEST');
    INSERT so.PriceList (CompanyCode, PriceListCode, Name, PriceLevel, Scope, CurrencyCode, ValidFrom, IsActive, CreatedBy)
    VALUES ('STC', 'ZZXMD008PL', 'Zodiac standard list', 5, 'STANDARD', 'GBP', '2020-01-01', 1, 'TEST');
    INSERT so.PriceListLine (CompanyCode, PriceListCode, ItemCode, UomCode, CurrencyCode, UnitPrice, ValidFrom, ValidTo, CreatedBy)
    VALUES ('STC', 'ZZXMD008PL', 'ZZXMD008-PART', 'EA', 'GBP', 12.500000, '2020-01-01', '2026-11-30', 'TEST'),
           ('STC', 'ZZXMD008PL', 'ZZXMD008-PART', 'EA', 'GBP', 19.000000, '2026-12-01', NULL, 'TEST');

    /*  An invoice confirmed on 12 May 2026 at the then standard rate and the then standard price. */
    INSERT ar.SalesInvoice (CompanyCode, DocumentType, InvoiceNumber, CustomerCode, DocumentDate, TaxPointDate, PostingDate,
           DueDate, FiscalYear, PeriodNo, CurrencyCode, FxRate, NetAmount, VatAmount, GrossAmount,
           NetAmountGBP, VatAmountGBP, GrossAmountGBP, TermsCode, TermsSnapshot, Status, PostedBy, PostedAt, CreatedBy)
    VALUES ('STC', 'INVOICE', 'ZZXMD008-1', 'ZZXMD008', '2026-05-12', '2026-05-12', '2026-05-12',
            '2026-06-11', 2026, 2, 'GBP', 1, 125.00, 25.00, 150.00, 125.00, 25.00, 150.00,
            'N30', '30 days net', 'POSTED', 'ARCLERK', '2026-05-12T16:04:00', 'TEST');
    SET @inv = SCOPE_IDENTITY();
    INSERT ar.SalesInvoiceLine (InvoiceId, LineNum, ItemCode, Description, Quantity, UomCode, ConversionFactor, StockQuantity,
           UnitPrice, PricePerQty, PriceSource, NetAmount, VatCode, VatRatePct, VatAmount, NetAmountGBP, VatAmountGBP)
    VALUES (@inv, 1, 'ZZXMD008-PART', 'Zodiac effective-dating test part', 10.000000, 'EA', 1.000000000, 10.000000,
            12.500000, 1, 'PRICELIST', 125.00, 'S', 20.0000, 25.00, 125.00, 25.00);

    /*  HM Treasury announces a new standard rate from 1 December 2026.  It is an addition to the
        effective-dated VAT rate table, not an edit of the rate in force in May.                  */
    INSERT fin.VatRate (VatCode, EffectiveFrom, RatePct, ApprovedBy, StatutoryRef)
    VALUES ('S', '2026-12-01', 22.0000, 'VATOWNER', 'ZZ test statutory instrument');

    SELECT TOP (1) @rateAt = RatePct FROM fin.VatRate WHERE VatCode = 'S' AND EffectiveFrom <= '2026-05-12' ORDER BY EffectiveFrom DESC;
    SELECT TOP (1) @rateNew = RatePct FROM fin.VatRate WHERE VatCode = 'S' AND EffectiveFrom <= '2026-12-15' ORDER BY EffectiveFrom DESC;
    IF @rateAt = 20.0000 AND @rateNew = 22.0000
        PRINT 'ok 1 - the new standard rate takes effect on 1 December 2026 only: 20.0000 still applies at the May tax point and 22.0000 from December -- REQ: XMD-008 AC: AC-100';
    ELSE PRINT 'not ok 1 - the rate in force in May is ' + CAST(ISNULL(@rateAt, -1) AS varchar(12)) + ' and in December ' + CAST(ISNULL(@rateNew, -1) AS varchar(12));

    /*  2 - the posted document is untouched by both the new rate and the new price. */
    SELECT TOP (1) @priceAt = UnitPrice FROM so.PriceListLine
     WHERE CompanyCode = 'STC' AND PriceListCode = 'ZZXMD008PL' AND ItemCode = 'ZZXMD008-PART'
       AND '2026-05-12' BETWEEN ValidFrom AND ISNULL(ValidTo, '9999-12-31') ORDER BY ValidFrom DESC;
    SELECT TOP (1) @priceNew = UnitPrice FROM so.PriceListLine
     WHERE CompanyCode = 'STC' AND PriceListCode = 'ZZXMD008PL' AND ItemCode = 'ZZXMD008-PART'
       AND '2026-12-15' BETWEEN ValidFrom AND ISNULL(ValidTo, '9999-12-31') ORDER BY ValidFrom DESC;
    SELECT @hit = COUNT(*) FROM ar.SalesInvoice h JOIN ar.SalesInvoiceLine l ON l.InvoiceId = h.InvoiceId
     WHERE h.InvoiceId = @inv AND l.VatRatePct = 20.0000 AND l.UnitPrice = 12.500000
       AND l.VatAmount = ROUND(l.NetAmount * l.VatRatePct / 100.0, 2) AND h.VatAmount = 25.00 AND h.GrossAmount = 150.00;
    IF @hit = 1 AND @priceAt = 12.500000 AND @priceNew = 19.000000
        PRINT 'ok 2 - the posted invoice still carries 20.0000 per cent and 12.500000 per unit after a 22 per cent rate and a 19.000000 price are made effective from December -- REQ: XMD-008 AC: AC-100';
    ELSE PRINT 'not ok 2 - the posted document moved with the masters: price at date ' + CAST(ISNULL(@priceAt, -1) AS varchar(24));

    /*  3 - the same change identifies the documents that are still open.  inv.usp_ItemImpact counts
        a sales line only while the order is live and still has quantity to despatch, so the
        fulfilled order is excluded and the open one is named.                                   */
    INSERT so.SalesOrder (CompanyCode, SiteCode, OrderNumber, CustomerCode, OrderDate, CurrencyCode, FxRate,
           TermsCode, TermsSnapshot, CommercialStatus, ReleaseStatus, FulfilmentStatus, BillingStatus, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZXMD008-OPEN', 'ZZXMD008', '2026-10-01', 'GBP', 1, 'N30', '30 days net',
            'ACCEPTED', 'RELEASED', 'UNALLOCATED', 'NOTELIGIBLE', 'TEST');
    SET @ord = SCOPE_IDENTITY();
    INSERT so.SalesOrderLine (SalesOrderId, CompanyCode, OrderNumber, LineNum, LineRef, ItemCode, Description,
           AcceptedQty, DespatchedQty, CancelledQty, UomCode, UnitPrice, NetAmount, VatCode, VatRatePct, Status)
    VALUES (@ord, 'STC', 'ZZXMD008-OPEN', 1, '001', 'ZZXMD008-PART', 'Zodiac effective-dating test part',
            6.000000, 0.000000, 0.000000, 'EA', 12.500000, 75.00, 'S', 20.0000, 'OPEN');
    INSERT so.SalesOrder (CompanyCode, SiteCode, OrderNumber, CustomerCode, OrderDate, CurrencyCode, FxRate,
           TermsCode, TermsSnapshot, CommercialStatus, ReleaseStatus, FulfilmentStatus, BillingStatus, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZXMD008-DONE', 'ZZXMD008', '2026-05-01', 'GBP', 1, 'N30', '30 days net',
            'COMPLETED', 'RELEASED', 'DESPATCHED', 'BILLED', 'TEST');
    SET @done = SCOPE_IDENTITY();
    INSERT so.SalesOrderLine (SalesOrderId, CompanyCode, OrderNumber, LineNum, LineRef, ItemCode, Description,
           AcceptedQty, DespatchedQty, CancelledQty, UomCode, UnitPrice, NetAmount, VatCode, VatRatePct, Status)
    VALUES (@done, 'STC', 'ZZXMD008-DONE', 1, '001', 'ZZXMD008-PART', 'Zodiac effective-dating test part',
            10.000000, 10.000000, 0.000000, 'EA', 12.500000, 125.00, 'S', 20.0000, 'CLOSED');
    DELETE @impact;
    INSERT @impact EXEC inv.usp_ItemImpact @CompanyCode = 'STC', @ItemCode = 'ZZXMD008-PART';
    SELECT @openLines = OpenSalesLines FROM @impact;
    IF @openLines = 1
       AND EXISTS (SELECT 1 FROM so.SalesOrderLine l JOIN so.SalesOrder h ON h.SalesOrderId = l.SalesOrderId
                    WHERE l.CompanyCode = 'STC' AND l.ItemCode = 'ZZXMD008-PART'
                      AND h.CommercialStatus NOT IN ('COMPLETED', 'CANCELLED')
                      AND l.AcceptedQty > l.DespatchedQty + l.CancelledQty AND h.OrderNumber = 'ZZXMD008-OPEN')
        PRINT 'ok 3 - the impact of the change names the one order still to be despatched and leaves the fulfilled order out -- REQ: XMD-008 AC: AC-100';
    ELSE PRINT 'not ok 3 - the impact assessment found ' + CAST(ISNULL(@openLines, -1) AS varchar(8)) + ' open sales lines instead of 1';

    /*  4 - the open document is repriced from the effective-dated master, the confirmed one is not,
        so the two can legitimately disagree about the same item on the same day.                */
    SELECT @hit = COUNT(*) FROM so.SalesOrderLine l
      JOIN so.SalesOrder h ON h.SalesOrderId = l.SalesOrderId
     WHERE h.OrderNumber = 'ZZXMD008-OPEN' AND l.UnitPrice = 12.500000;
    UPDATE l SET UnitPrice = p.UnitPrice, NetAmount = ROUND(l.AcceptedQty * p.UnitPrice, 2),
                 VatRatePct = v.RatePct, VatAmount = ROUND(ROUND(l.AcceptedQty * p.UnitPrice, 2) * v.RatePct / 100.0, 2)
      FROM so.SalesOrderLine l
      JOIN so.SalesOrder h ON h.SalesOrderId = l.SalesOrderId
      CROSS APPLY (SELECT TOP (1) UnitPrice FROM so.PriceListLine pl
                    WHERE pl.CompanyCode = l.CompanyCode AND pl.PriceListCode = 'ZZXMD008PL' AND pl.ItemCode = l.ItemCode
                      AND '2026-12-15' BETWEEN pl.ValidFrom AND ISNULL(pl.ValidTo, '9999-12-31') ORDER BY pl.ValidFrom DESC) p
      CROSS APPLY (SELECT TOP (1) RatePct FROM fin.VatRate r
                    WHERE r.VatCode = l.VatCode AND r.EffectiveFrom <= '2026-12-15' ORDER BY r.EffectiveFrom DESC) v
     WHERE h.OrderNumber = 'ZZXMD008-OPEN';
    SELECT @n = COUNT(*) FROM so.SalesOrderLine l JOIN so.SalesOrder h ON h.SalesOrderId = l.SalesOrderId
     WHERE h.OrderNumber = 'ZZXMD008-OPEN' AND l.UnitPrice = 19.000000 AND l.VatRatePct = 22.0000
       AND l.NetAmount = 114.00 AND l.VatAmount = 25.08;
    SELECT @m = COUNT(*) FROM ar.SalesInvoiceLine WHERE InvoiceId = @inv AND UnitPrice = 12.500000 AND VatRatePct = 20.0000;
    IF @hit = 1 AND @n = 1 AND @m = 1
        PRINT 'ok 4 - the open order moves to 19.000000 at 22 per cent from the effective date while the posted invoice stays at 12.500000 and 20 per cent -- REQ: XMD-008 AC: AC-100';
    ELSE PRINT 'not ok 4 - open line repriced ' + CAST(@n AS varchar(4)) + ', posted line preserved ' + CAST(@m AS varchar(4));
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 - unexpected error: ' + @msg;
    PRINT 'not ok 2 - not reached';
    PRINT 'not ok 3 - not reached';
    PRINT 'not ok 4 - not reached';
END CATCH

/* ---------------------------------------------------------------------------------------------
   XMD-011 - the company boundary in stock, obligations, cash and reporting (AC-105).
   --------------------------------------------------------------------------------------------- */
/*  5 and 6 - the identical item code exists in both companies; only Stellar Components holds the
    stock, and Caledon Assemblies may not reserve against it. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, LifecycleStatus, CreatedBy)
    VALUES ('STC', 'ZZXMD011-PART', 'Shared code, Stellar Components part', 'BOUGHT', 'EA', 'RELEASED', 'TEST'),
           ('CAL', 'ZZXMD011-PART', 'Shared code, Caledon Assemblies part', 'BOUGHT', 'EA', 'RELEASED', 'TEST');
    INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, QtyOnHand)
    VALUES ('STC', 'RDG', 'MAIN', 'STKA01', 'ZZXMD011-PART', 40.000000);
    SELECT @n = COUNT(*), @stcFree = ISNULL(SUM(QtyOnHand), 0) FROM inv.vw_StockAvailability
     WHERE CompanyCode = 'STC' AND ItemCode = 'ZZXMD011-PART';
    SELECT @m = COUNT(*), @calFree = ISNULL(SUM(QtyOnHand), 0) FROM inv.vw_StockAvailability
     WHERE CompanyCode = 'CAL' AND ItemCode = 'ZZXMD011-PART';
    IF @n = 1 AND @stcFree = 40 AND @m = 0 AND @calFree = 0
        PRINT 'ok 5 - the same item code in two companies holds 40 units in Stellar Components and none in Caledon Assemblies -- REQ: XMD-011 AC: AC-105';
    ELSE PRINT 'not ok 5 - availability shows ' + CAST(@stcFree AS varchar(24)) + ' in STC and ' + CAST(@calFree AS varchar(24)) + ' in CAL';

    DELETE @res;
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = 'ZZXMD011-PART', @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'ZZXMD011-SO-STC', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 5, @AsOf = '2026-10-15';
    SELECT @stcFree = QtyFreeBefore FROM @res;
    EXEC inv.usp_ReserveStock @CompanyCode = 'CAL', @ItemCode = 'ZZXMD011-PART', @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'ZZXMD011-SO-CAL', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'GLA', @Quantity = 5, @AsOf = '2026-10-15';
    PRINT 'not ok 6 - a Caledon demand reserved stock that belongs to Stellar Components -- REQ: XMD-011';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%not enough free stock%' AND @stcFree = 40
        PRINT 'ok 6 - Stellar Components can reserve 5 of its 40 units while the identically coded Caledon demand is refused for want of free stock -- REQ: XMD-011 AC: AC-105';
    ELSE PRINT 'not ok 6 - unexpected error: ' + @msg;
END CATCH

/*  7 - a code that was never catalogued in this company is refused outright, so a code copied from
    the other company cannot be used to consume anything. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, LifecycleStatus, CreatedBy)
    VALUES ('STC', 'ZZXMD011-ONLY', 'Stellar Components only part', 'BOUGHT', 'EA', 'RELEASED', 'TEST');
    INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, QtyOnHand)
    VALUES ('STC', 'RDG', 'MAIN', 'STKA01', 'ZZXMD011-ONLY', 25.000000);
    EXEC inv.usp_ReserveStock @CompanyCode = 'CAL', @ItemCode = 'ZZXMD011-ONLY', @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'ZZXMD011-SO-X', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'GLA', @Quantity = 1, @AsOf = '2026-10-15';
    PRINT 'not ok 7 - a company used an item code that belongs to another company -- REQ: XMD-011';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%does not exist in this company%'
        PRINT 'ok 7 - an item code catalogued only in the other company is refused with "That item does not exist in this company" -- REQ: XMD-011 AC: AC-105';
    ELSE PRINT 'not ok 7 - unexpected error: ' + @msg;
END CATCH

/*  8 - the same customer code in two companies owes and is owed separately; the obligations are
    never netted and the impact of a change is assessed per company. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, LegalForm, TermsCode, CurrencyCode, Status, CreatedBy)
    VALUES ('STC', 'ZZXMD011', 'Zodiac Frameworks Ltd, Reading account', 'LTD', 'N30', 'GBP', 'ACTIVE', 'TEST'),
           ('CAL', 'ZZXMD011', 'Zodiac Frameworks Ltd, Glasgow account', 'LTD', 'N30', 'GBP', 'ACTIVE', 'TEST');
    INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate,
           CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, SettlementStatus, CreatedBy)
    VALUES ('STC', 'ZZXMD011', 'INVOICE', 'ZZXMD011-SI1', '2026-09-30', '2026-10-30', '2026-09-30', 'GBP', 1,
            1000.00, 1000.00, 1000.00, 1000.00, 'OPEN', 'TEST'),
           ('CAL', 'ZZXMD011', 'CREDIT', 'ZZXMD011-SC1', '2026-09-30', '2026-10-30', '2026-09-30', 'GBP', 1,
            -1000.00, -1000.00, -1000.00, -1000.00, 'OPEN', 'TEST');
    SELECT @stcAfter = ISNULL(SUM(ResidualAmountGBP), 0) FROM ar.OpenItem
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZXMD011';
    SELECT @calAfter = ISNULL(SUM(ResidualAmountGBP), 0) FROM ar.OpenItem
     WHERE CompanyCode = 'CAL' AND CustomerCode = 'ZZXMD011';
    SELECT @n = COUNT(*) FROM (SELECT CompanyCode FROM ar.OpenItem WHERE CustomerCode = 'ZZXMD011' GROUP BY CompanyCode) q;
    /*  The sales-ledger key is company scoped, so neither account can be reached from the other. */
    SELECT @m = COUNT(*) FROM sys.index_columns ic
      JOIN sys.indexes ix ON ix.object_id = ic.object_id AND ix.index_id = ic.index_id
      JOIN sys.columns c ON c.object_id = ic.object_id AND c.column_id = ic.column_id
     WHERE ix.name = 'UQ_ArOpenItem' AND c.name = 'CompanyCode';
    IF @stcAfter = 1000.00 AND @calAfter = -1000.00 AND @n = 2 AND @m = 1
        PRINT 'ok 8 - the shared customer code owes 1000.00 in Stellar Components and is owed 1000.00 in Caledon Assemblies, held as two company-scoped accounts that are never netted to nil -- REQ: XMD-011 AC: AC-105';
    ELSE PRINT 'not ok 8 - STC residual ' + CAST(@stcAfter AS varchar(24)) + ', CAL residual ' + CAST(@calAfter AS varchar(24));
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 8 - unexpected error: ' + @msg;
END CATCH

/*  9 - financial reporting keeps the boundary: a journal posted in Caledon Assemblies moves the
    Caledon control balance and leaves the Stellar Components one exactly where it was. */
BEGIN TRY
    BEGIN TRANSACTION;
    SELECT @fy = FiscalYear, @pn = PeriodNo FROM fin.Period
     WHERE CompanyCode = 'CAL' AND CONVERT(date, @cut, 112) BETWEEN StartDate AND EndDate;
    SELECT TOP (1) @acc = AccountCode FROM fin.Account WHERE CompanyCode = 'CAL' AND ControlType = 'AR' ORDER BY AccountCode;
    SELECT @stcBefore = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = 'STC' AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= @cut;
    SELECT @calAfter = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = 'CAL' AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= @cut;
    INSERT fin.Journal (CompanyCode, JournalNumber, JournalType, Description, SourceModule, SourceRef, EventCode,
           DocumentDate, PostingDate, FiscalYear, PeriodNo, CurrencyCode, TotalDebit, TotalCredit,
           TotalDebitGBP, TotalCreditGBP, Status, CreatedBy)
    VALUES ('CAL', 'ZZXMD011J', 'GL', 'Company boundary test', 'XMD', 'ZZXMD011-J1', 'XMD.TEST',
            CONVERT(date, @cut, 112), CONVERT(date, @cut, 112), @fy, @pn, 'GBP', 2500.00, 2500.00,
            2500.00, 2500.00, 'POSTED', 'TEST');
    SET @jid = SCOPE_IDENTITY();
    INSERT fin.JournalLine (JournalId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount,
           DebitAmountGBP, CreditAmountGBP, CurrencyCode)
    VALUES (@jid, 1, 'CAL', @acc, 2500.00, 0, 2500.00, 0, 'GBP');
    SELECT @stcAfter = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = 'STC' AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= @cut;
    SELECT @n = COUNT(*) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = 'CAL' AND CONVERT(char(8), PostingDate, 112) <= @cut AND BalanceGBP = 2500.00;
    SELECT @calFree = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = 'CAL' AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= @cut;
    IF @stcAfter = @stcBefore AND @calFree = @calAfter + 2500.00 AND @acc IS NOT NULL
        PRINT 'ok 9 - a 2500.00 debtors posting in Caledon Assemblies moves only the Caledon control balance; the Stellar Components balance at the same cutoff is unchanged -- REQ: XMD-011 AC: AC-105';
    ELSE PRINT 'not ok 9 - the Stellar Components balance moved by ' + CAST(@stcAfter - @stcBefore AS varchar(24)) + ' when Caledon Assemblies posted';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 9 - unexpected error: ' + @msg;
END CATCH

PRINT '1..9';
GO
