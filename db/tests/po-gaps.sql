/*  Purchasing coverage gaps - the parts of approved sourcing (PO-002), order capture (PO-003),
    copies / imports / blanket call-offs (PO-021) and supplier due diligence (PO-026) that are
    owned by the database rather than by a COBOL program.  Every assertion either runs inside a
    transaction that is rolled back or deletes its own ZZ- keyed fixtures, so the script is
    repeatable against a live database.  The COBOL side of the same four requirements is in
    cobol\tests\po\TPOGAP.cob.
    REQ: PO-002, PO-003, PO-021, PO-026
    AC: AC-050, AC-058, AC-114 */
SET NOCOUNT ON;
GO
PRINT '# po-gaps';
DECLARE @msg varchar(400), @n int, @txt varchar(200), @dec decimal(18,2),
        @co varchar(10) = 'STC', @site varchar(10) = 'RDG', @supp varchar(20) = 'S0001',
        @item varchar(40) = 'ZZ-SQL-GAP-ITEM', @src varchar(30) = 'ZZ-SQL-GAP-SRC',
        @ord varchar(30) = 'ZZ-SQL-GAP-PO1', @blk varchar(30) = 'ZZ-SQL-GAP-BLK',
        @mrn varchar(40) = 'ZZ-SQL-GAP-MRN-1', @eid int, @poid bigint, @bid int, @d date = '2026-10-07';

/*  leftovers from an interrupted run go first - the script owns every ZZ-SQL-GAP key */
DELETE q FROM po.SourcingQuote q JOIN po.SourcingEvent e ON e.SourcingEventId = q.SourcingEventId
 WHERE e.SourcingRef LIKE 'ZZ-SQL-GAP%';
DELETE FROM po.SourcingEvent WHERE SourcingRef LIKE 'ZZ-SQL-GAP%';
DELETE FROM po.SupplierSource WHERE ItemCode LIKE 'ZZ-SQL-GAP%';
DELETE FROM po.PurchaseOrderLine WHERE OrderNumber LIKE 'ZZ-SQL-GAP%';
DELETE FROM po.PurchaseOrder WHERE OrderNumber LIKE 'ZZ-SQL-GAP%';
DELETE FROM po.BlanketBalance WHERE BlanketOrderNumber LIKE 'ZZ-SQL-GAP%';
DELETE l FROM po.ImportEntryLine l JOIN po.ImportEntry e ON e.ImportEntryId = l.ImportEntryId
 WHERE e.ImportEntryRef LIKE 'ZZ-SQL-GAP%';
DELETE FROM po.ImportEntry WHERE ImportEntryRef LIKE 'ZZ-SQL-GAP%';
DELETE FROM po.SupplierDiligence WHERE SupplierCode LIKE 'ZZSQLGAP%';

/*  1 - the approved source register models approval as a state, so "unapproved" is a decision
    somebody has to authorise rather than a note nobody reads. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT po.SupplierSource (CompanyCode, ItemCode, SupplierCode, ApprovalStatus, EffectiveFrom, CreatedBy)
    VALUES (@co, @item, 'ZZSQLGAPA', 'MAYBE', '2026-01-01', 'TEST');
    PRINT 'not ok 1 - an invented approval status was accepted -- REQ: PO-002';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_SupSrc_As%' PRINT 'ok 1 - a source is APPROVED, CONDITION, UNAPPROVED or SUSPENDED and nothing else -- REQ: PO-002';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg + ' -- REQ: PO-002';
END CATCH

/*  2 - the buyer compares on the supplier item reference, the approved flag, the quotation, the
    lead time, the minimum order and the preference rank; a missing column is a broken contract. */
SELECT @n = COUNT(*) FROM sys.columns
 WHERE object_id = OBJECT_ID(N'inv.ItemPartner')
   AND name IN ('PartnerItemCode', 'IsApprovedSource', 'QuoteRef', 'LeadTimeDays',
                'MinOrderQty', 'OrderMultipleQty', 'PreferenceRank', 'EffectiveFrom', 'EffectiveTo');
IF @n = 9 PRINT 'ok 2 - the item/supplier reference carries approval, quote, lead time and order rules -- REQ: PO-002';
ELSE PRINT 'not ok 2 - only ' + CONVERT(varchar(10), @n) + ' of the 9 sourcing columns were found -- REQ: PO-002';

/*  3 - three quotations: 900.00, 1,000.00 and 1,200.00 for the same 100 units.  The cheapest is
    ranked first, but the chosen source is the dearer one with the 14 day lead time, and the
    event keeps the reason and records that price did not decide it (AC-050). */
BEGIN TRANSACTION;
INSERT po.SourcingEvent (CompanyCode, SourcingRef, ItemCode, Description, Quantity, UomCode,
                         BuyerUser, EventDate, Status, CreatedBy)
VALUES (@co, @src, @item, 'ZZ sql gap sourcing event', 100, 'EA', 'BUYER', @d, 'OPEN', 'TEST');
SET @eid = CAST(SCOPE_IDENTITY() AS int);
INSERT po.SourcingQuote (SourcingEventId, LineNum, SupplierCode, ApprovalStatus, QuoteRef, CurrencyCode,
                         UnitPrice, PricePerQty, LineValueGBP, LeadTimeDays, OnTimePct, QualityPct)
VALUES (@eid, 1, 'ZZSQLGAPC', 'CONDITION', 'ZZ-Q-C', 'GBP',  9.000000, 1,  900.00, 35, 70.0000, 88.0000),
       (@eid, 2, 'ZZSQLGAPA', 'APPROVED',  'ZZ-Q-A', 'GBP', 10.000000, 1, 1000.00, 14, 98.0000, 99.5000),
       (@eid, 3, 'ZZSQLGAPU', 'UNAPPROVED','ZZ-Q-U', 'GBP', 12.000000, 1, 1200.00, 10, 60.0000, 80.0000);
UPDATE q SET RankByPrice = r.rk
  FROM po.SourcingQuote q
  JOIN (SELECT SourcingQuoteId, RANK() OVER (ORDER BY LineValueGBP) AS rk
          FROM po.SourcingQuote WHERE SourcingEventId = @eid) r ON r.SourcingQuoteId = q.SourcingQuoteId;
UPDATE po.SourcingEvent
   SET SelectedSupplierCode = 'ZZSQLGAPA', SelectionReason = 'Lead time 14 days against 35 and 98% on time',
       SelectedOnPrice = 0, Status = 'SELECTED'
 WHERE SourcingEventId = @eid;
UPDATE po.SourcingQuote SET IsSelected = CASE WHEN SupplierCode = 'ZZSQLGAPA' THEN 1 ELSE 0 END
 WHERE SourcingEventId = @eid;
SELECT @txt = q.SupplierCode + '/' + CONVERT(varchar(10), q.RankByPrice)
  FROM po.SourcingQuote q WHERE q.SourcingEventId = @eid AND q.RankByPrice = 1;
SELECT @n = CASE WHEN e.SelectedSupplierCode = 'ZZSQLGAPA' AND e.SelectedOnPrice = 0
                  AND e.SelectionReason IS NOT NULL THEN 1 ELSE 0 END
  FROM po.SourcingEvent e WHERE e.SourcingEventId = @eid;
IF @txt = 'ZZSQLGAPC/1' AND @n = 1
    PRINT 'ok 3 - the cheapest quotation is ranked first but is not the automatic winner -- REQ: PO-002, AC: AC-050';
ELSE PRINT 'not ok 3 - cheapest was ' + ISNULL(@txt, 'NULL') + ' and the selection audit was ' + CONVERT(varchar(2), ISNULL(@n, -1)) + ' -- REQ: PO-002';

/*  4 - the premium of 100.00 over the cheapest is visible beside the chosen quotation, so the
    decision can be questioned afterwards. */
SELECT @dec = (SELECT LineValueGBP FROM po.SourcingQuote WHERE SourcingEventId = @eid AND IsSelected = 1)
             - MIN(LineValueGBP)
  FROM po.SourcingQuote WHERE SourcingEventId = @eid;
IF @dec = 100.00 PRINT 'ok 4 - the chosen source costs 100.00 more than the cheapest quotation -- REQ: PO-002, AC: AC-050';
ELSE PRINT 'not ok 4 - the premium was ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ', expected 100.00 -- REQ: PO-002';

/*  5 - using an unapproved source is not forbidden, it is evidenced: the event names who
    authorised it and says plainly that an unapproved source was used. */
UPDATE po.SourcingEvent
   SET SelectedSupplierCode = 'ZZSQLGAPU', UnapprovedSourceUsed = 1, AuthorisedBy = 'PURCHMGR',
       SelectionReason = 'Sole source for a breakdown repair', Status = 'SELECTED'
 WHERE SourcingEventId = @eid;
SELECT @n = COUNT(*) FROM po.SourcingEvent
 WHERE SourcingEventId = @eid AND UnapprovedSourceUsed = 1 AND AuthorisedBy IS NOT NULL AND SelectionReason IS NOT NULL;
IF @n = 1 PRINT 'ok 5 - an unapproved source records its authoriser and reason -- REQ: PO-002, AC: AC-050';
ELSE PRINT 'not ok 5 - the unapproved source authorisation was not retained -- REQ: PO-002';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  6 - an order line is one of the six governed kinds; a purchase cannot invent a seventh. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT po.PurchaseOrder (CompanyCode, SiteCode, OrderNumber, SupplierCode, OrderDate, CreatedBy)
    VALUES (@co, @site, @ord, @supp, @d, 'TEST');
    SET @poid = CAST(SCOPE_IDENTITY() AS bigint);
    INSERT po.PurchaseOrderLine (PurchaseOrderId, CompanyCode, OrderNumber, LineNum, Description,
                                 LineType, OrderedQty, UnitPrice)
    VALUES (@poid, @co, @ord, 1, 'ZZ invented kind of line', 'WHATEVER', 1, 1.000000);
    PRINT 'not ok 6 - an invented line type was accepted -- REQ: PO-003';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_POrdL_Lt%' PRINT 'ok 6 - a line is STOCK, NONSTOCK, SERVICE, SUBCON, CHARGE or TEXT and nothing else -- REQ: PO-003';
    ELSE PRINT 'not ok 6 - unexpected error: ' + @msg + ' -- REQ: PO-003';
END CATCH

/*  7 - the header keeps the whole commercial agreement: currency, agreed terms wording, the
    Incoterms 2020 rule and named place, the point ownership passes and the conditions version. */
BEGIN TRANSACTION;
INSERT po.PurchaseOrder (CompanyCode, SiteCode, OrderNumber, SupplierCode, BuyerUser, OrderDate,
                         CurrencyCode, FxRate, TermsCode, TermsSnapshot, IncotermsRule, IncotermsNamedPlace,
                         OwnershipPassesAt, DeliveryAddressSnapshot, TermsAndConditionsVersion, CreatedBy)
VALUES (@co, @site, @ord, @supp, 'BUYER', @d, 'EUR', 1.17000000, 'N30',
        '30 days from receipt of a valid invoice', 'FCA', 'Rotterdam', 'SHIPMENT',
        'ZZ Works, Reading, RG1 1AA', 'PO-TC-2024', 'TEST');
SET @poid = CAST(SCOPE_IDENTITY() AS bigint);
SELECT @txt = CurrencyCode + '|' + IncotermsRule + '|' + IncotermsNamedPlace + '|' + OwnershipPassesAt
            + '|' + TermsAndConditionsVersion + '|' + Status
  FROM po.PurchaseOrder WHERE PurchaseOrderId = @poid;
IF @txt = 'EUR|FCA|Rotterdam|SHIPMENT|PO-TC-2024|DRAFT'
    PRINT 'ok 7 - currency, delivery terms, ownership point and conditions version are captured -- REQ: PO-003';
ELSE PRINT 'not ok 7 - the header read back as ' + ISNULL(@txt, 'NULL') + ' -- REQ: PO-003';

/*  8 - a free-text service line carries its nominal coding, no item code and no catalogue entry:
    the 2,000.00 service obligation of specification 06 section 9 is bought, not stocked. */
INSERT po.PurchaseOrderLine (PurchaseOrderId, CompanyCode, OrderNumber, LineNum, Description, LineType,
                             OrderedQty, UomCode, UnitPrice, NetAmount, AccountCode, DimCostCentre, ServiceValue)
VALUES (@poid, @co, @ord, 1, 'ZZ annual boiler inspection service', 'SERVICE', 1, 'EA',
        2000.000000, 2000.00, '7100', 'CC100', 2000.00);
SELECT @n = COUNT(*) FROM po.PurchaseOrderLine
 WHERE PurchaseOrderId = @poid AND LineType = 'SERVICE' AND ItemCode IS NULL
   AND AccountCode IS NOT NULL AND NetAmount = 2000.00;
SELECT @dec = COUNT(*) FROM inv.Item
 WHERE CompanyCode = @co AND (ItemCode LIKE 'ZZ-SQL-GAP%' OR Description LIKE 'ZZ annual boiler%');
IF @n = 1 AND @dec = 0
    PRINT 'ok 8 - a coded free-text service line creates no stocked catalogue item -- REQ: PO-003, AC: AC-050';
ELSE PRINT 'not ok 8 - service line count ' + CONVERT(varchar(10), @n) + ', catalogue rows created ' + CONVERT(varchar(10), @dec) + ' -- REQ: PO-003';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  9 - the blanket draw down is conditional on the remaining balance, so repeating an accepted
    call-off of 60 against the 40 that are left updates no row at all (AC-058). */
BEGIN TRANSACTION;
INSERT po.BlanketBalance (CompanyCode, BlanketOrderNumber, LineNum, ItemCode, Description,
                          AgreedQty, CalledOffQty, AgreedValue, CalledOffValue, CurrencyCode,
                          ValidFrom, ValidTo, Status, ModifiedBy)
VALUES (@co, @blk, 1, @item, 'ZZ blanket agreement', 100, 60, 10000.00, 6000.00, 'GBP',
        '2026-01-01', '2026-12-31', 'OPEN', 'TEST');
SET @bid = CAST(SCOPE_IDENTITY() AS int);
UPDATE po.BlanketBalance
   SET CalledOffQty = CalledOffQty + 60, CalledOffValue = CalledOffValue + 6000.00,
       Status = CASE WHEN AgreedQty > 0 AND CalledOffQty + 60 >= AgreedQty THEN 'EXHAUSTED' ELSE Status END
 WHERE BlanketBalanceId = @bid AND (AgreedQty = 0 OR CalledOffQty + 60 <= AgreedQty);
SET @n = @@ROWCOUNT;
SELECT @dec = CalledOffQty FROM po.BlanketBalance WHERE BlanketBalanceId = @bid;
IF @n = 0 AND @dec = 60
    PRINT 'ok 9 - a repeated call-off above the blanket balance issues no second obligation -- REQ: PO-021, AC: AC-058';
ELSE PRINT 'not ok 9 - ' + CONVERT(varchar(10), @n) + ' row(s) were drawn down, called off is now ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ' -- REQ: PO-021';

/*  10 - a call-off that fits is still allowed, and it exhausts the agreement exactly. */
UPDATE po.BlanketBalance
   SET CalledOffQty = CalledOffQty + 40, CalledOffValue = CalledOffValue + 4000.00,
       Status = CASE WHEN AgreedQty > 0 AND CalledOffQty + 40 >= AgreedQty THEN 'EXHAUSTED' ELSE Status END
 WHERE BlanketBalanceId = @bid AND (AgreedQty = 0 OR CalledOffQty + 40 <= AgreedQty);
SELECT @txt = CONVERT(varchar(20), CAST(CalledOffQty AS decimal(19,2))) + '|'
            + CONVERT(varchar(20), CalledOffValue) + '|' + Status
  FROM po.BlanketBalance WHERE BlanketBalanceId = @bid;
IF @txt = '100.00|10000.00|EXHAUSTED'
    PRINT 'ok 10 - the blanket is drawn down to its agreed ceiling and then closed -- REQ: PO-021, AC: AC-058';
ELSE PRINT 'not ok 10 - the blanket read back as ' + ISNULL(@txt, 'NULL') + ' -- REQ: PO-021';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  11 - a copy is a new draft that still says where it came from; POAMEND writes the source
    order into this column while clearing the receipt, approval and payment state. */
IF COL_LENGTH(N'po.PurchaseOrder', N'CopiedFromOrderNumber') IS NOT NULL
    PRINT 'ok 11 - a copied order retains a reference to its source order -- REQ: PO-021';
ELSE PRINT 'not ok 11 - po.PurchaseOrder has no CopiedFromOrderNumber column -- REQ: PO-021';

/*  12 - the same customs declaration cannot be loaded twice: one movement reference number is
    one import obligation, however many times the agent sends the file (AC-058). */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT po.ImportEntry (CompanyCode, ImportEntryRef, MovementReferenceNumber, EntryDate,
                           SupplierCode, CustomsValueGBP, DutyAmountGBP, ImportVatGBP, ImportVatMethod, CreatedBy)
    VALUES (@co, 'ZZ-SQL-GAP-IMP1', @mrn, @d, @supp, 10000.00, 450.00, 2090.00, 'PVA', 'TEST');
    INSERT po.ImportEntry (CompanyCode, ImportEntryRef, MovementReferenceNumber, EntryDate,
                           SupplierCode, CustomsValueGBP, DutyAmountGBP, ImportVatGBP, ImportVatMethod, CreatedBy)
    VALUES (@co, 'ZZ-SQL-GAP-IMP2', @mrn, @d, @supp, 10000.00, 450.00, 2090.00, 'PVA', 'TEST');
    PRINT 'not ok 12 - the same customs declaration was accepted twice -- REQ: PO-021';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%UQ_ImportEntry_Mrn%'
        PRINT 'ok 12 - repeating an accepted import raises no duplicate duty or VAT obligation -- REQ: PO-021, AC: AC-058';
    ELSE PRINT 'not ok 12 - unexpected error: ' + @msg + ' -- REQ: PO-021';
END CATCH

/*  13 - a supplier whose name matches the UK sanctions list is a potential match, the decision
    is left pending and trading is blocked until somebody authorised decides (AC-114). */
BEGIN TRANSACTION;
CREATE TABLE #s (ScreeningId bigint, CompanyCode varchar(10), PartyType varchar(12), PartyCode varchar(20),
                 PartyName varchar(120), MatchStatus varchar(12), MatchedEntryId int, MatchedName varchar(120),
                 MatchScore decimal(9,4), Decision varchar(12), ScreenedAt datetime2(3), Note varchar(400));
BEGIN TRY
    INSERT #s EXEC core.usp_ScreenParty @CompanyCode = @co, @PartyType = 'SUPPLIER', @PartyCode = 'ZZSQLGAPB',
        @PartyName = 'Vostok Precision Metals OOO', @Country = 'RU', @UserCode = 'BUYER', @TriggerEvent = 'ONBOARD';
    SELECT @txt = MatchStatus + '|' + Decision FROM #s;
    IF @txt IN ('MATCH|PENDING', 'POTENTIAL|PENDING')
        PRINT 'ok 13 - a sanctions name match leaves the supplier undecided and blocked -- REQ: PO-026, AC: AC-114';
    ELSE PRINT 'not ok 13 - the screening returned ' + ISNULL(@txt, 'NULL') + ' -- REQ: PO-026';
    SELECT @n = COUNT(*) FROM core.ExceptionItem
     WHERE CompanyCode = @co AND Category = 'SANCTIONS-MATCH' AND DocumentRef = 'ZZSQLGAPB'
       AND Severity = 'HOLD' AND Status = 'OPEN';
    IF @n >= 1 PRINT 'ok 14 - the match raises an owned HOLD exception rather than a silent flag -- REQ: PO-026, AC: AC-114';
    ELSE PRINT 'not ok 14 - no HOLD exception was raised for the sanctions match -- REQ: PO-026';
END TRY
BEGIN CATCH
    PRINT 'not ok 13 - sanctions screening failed: ' + ERROR_MESSAGE() + ' -- REQ: PO-026';
    PRINT 'not ok 14 - sanctions screening failed -- REQ: PO-026';
END CATCH
DROP TABLE #s;
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  15 - a clean supplier screens clear and is decided there and then, so the block in 13 is a
    real discrimination and not a blanket refusal. */
BEGIN TRANSACTION;
CREATE TABLE #c (ScreeningId bigint, CompanyCode varchar(10), PartyType varchar(12), PartyCode varchar(20),
                 PartyName varchar(120), MatchStatus varchar(12), MatchedEntryId int, MatchedName varchar(120),
                 MatchScore decimal(9,4), Decision varchar(12), ScreenedAt datetime2(3), Note varchar(400));
BEGIN TRY
    INSERT #c EXEC core.usp_ScreenParty @CompanyCode = @co, @PartyType = 'SUPPLIER', @PartyCode = 'ZZSQLGAPA',
        @PartyName = 'ZZ Test Approved Source Ltd', @UserCode = 'BUYER', @TriggerEvent = 'ONBOARD';
    SELECT @txt = MatchStatus + '|' + Decision FROM #c;
    IF @txt = 'CLEAR|CLEARED' PRINT 'ok 15 - a supplier with no match screens clear and may trade -- REQ: PO-026, AC: AC-114';
    ELSE PRINT 'not ok 15 - the clean screening returned ' + ISNULL(@txt, 'NULL') + ' -- REQ: PO-026';
END TRY
BEGIN CATCH
    PRINT 'not ok 15 - sanctions screening failed: ' + ERROR_MESSAGE() + ' -- REQ: PO-026';
END CATCH
DROP TABLE #c;
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  16 - the due diligence register holds the whole D-29 evidence set, one row per check per
    occasion, and refuses evidence of a kind the policy does not name. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT po.SupplierDiligence (CompanyCode, SupplierCode, EvidenceType, Outcome, IsBlocking, IsMandatory, CheckedBy)
    VALUES (@co, 'ZZSQLGAPA', 'SANCTIONS', 'PASS', 1, 1, 'TEST'),
           (@co, 'ZZSQLGAPA', 'COMPANIESHSE', 'PASS', 0, 1, 'TEST'),
           (@co, 'ZZSQLGAPA', 'VATNUMBER', 'PASS', 0, 1, 'TEST'),
           (@co, 'ZZSQLGAPA', 'BUSINESSSIZE', 'PASS', 0, 1, 'TEST'),
           (@co, 'ZZSQLGAPA', 'MODERNSLAVERY', 'PASS', 0, 0, 'TEST'),
           (@co, 'ZZSQLGAPA', 'ANTIBRIBERY', 'PASS', 0, 0, 'TEST'),
           (@co, 'ZZSQLGAPA', 'INSURANCE', 'EXPIRED', 0, 1, 'TEST'),
           (@co, 'ZZSQLGAPA', 'QUALITY', 'REVIEW', 0, 0, 'TEST');
    SELECT @n = COUNT(DISTINCT EvidenceType) FROM po.SupplierDiligence WHERE SupplierCode = 'ZZSQLGAPA';
    IF @n = 8 PRINT 'ok 16 - sanctions, Companies House, VAT, size, slavery, bribery, insurance and quality are all held -- REQ: PO-026';
    ELSE PRINT 'not ok 16 - only ' + CONVERT(varchar(10), @n) + ' of the 8 evidence types were accepted -- REQ: PO-026';
    INSERT po.SupplierDiligence (CompanyCode, SupplierCode, EvidenceType, Outcome, CheckedBy)
    VALUES (@co, 'ZZSQLGAPA', 'GUTFEELING', 'PASS', 'TEST');
    PRINT 'not ok 17 - an invented evidence type was accepted -- REQ: PO-026';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_PoDil_Et%' PRINT 'ok 17 - due diligence evidence outside the policy set is refused -- REQ: PO-026, D-29';
    ELSE PRINT 'not ok 17 - unexpected error: ' + @msg + ' -- REQ: PO-026';
END CATCH

/*  18 - onboarding evidence lives on the supplier too: the Companies House number and status,
    the HMRC VAT validation and its date, and the business size the payment terms depend on. */
SELECT @n = COUNT(*) FROM sys.columns
 WHERE object_id = OBJECT_ID(N'ap.Supplier')
   AND name IN ('CompaniesHouseNo', 'CompaniesHouseStatus', 'CompaniesHouseCheckedAt',
                'VatRegNo', 'VatValidatedAt', 'VatValidationRef', 'BusinessSize',
                'SanctionsStatus', 'SanctionsScreenedAt', 'PurchaseHold');
IF @n = 10 PRINT 'ok 18 - supplier onboarding records Companies House, HMRC VAT and business size evidence -- REQ: PO-026, D-29';
ELSE PRINT 'not ok 18 - only ' + CONVERT(varchar(10), @n) + ' of the 10 due diligence columns were found -- REQ: PO-026';

/*  19 - an evidence row is a point in time, so this year's failure cannot overwrite last year's
    pass: the register is keyed by company, supplier, evidence type AND the moment of the check. */
SELECT @n = COUNT(*) FROM sys.indexes i
  JOIN sys.index_columns ic ON ic.object_id = i.object_id AND ic.index_id = i.index_id
  JOIN sys.columns c ON c.object_id = ic.object_id AND c.column_id = ic.column_id
 WHERE i.object_id = OBJECT_ID(N'po.SupplierDiligence') AND i.name = N'UQ_SupplierDiligence'
   AND c.name IN ('CompanyCode', 'SupplierCode', 'EvidenceType', 'CheckedAt');
IF @n = 4 PRINT 'ok 19 - due diligence keeps the history of what was known when -- REQ: PO-026, D-29';
ELSE PRINT 'not ok 19 - the due diligence key covers ' + CONVERT(varchar(10), @n) + ' of the 4 expected columns -- REQ: PO-026';
GO
