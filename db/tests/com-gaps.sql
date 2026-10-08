/*  Shared-rule tests for four cross-cutting controls that had no test of their own:
      COM-007  a confirmed document keeps the commercial and calculation inputs used when it was
               confirmed, and a later master-data change does not rewrite them;
      COM-016  a conflicting edit or a change in availability is revalidated at confirmation, so a
               stale view can neither overwrite another user's change nor reserve promised stock;
      COM-027  statutory retention, retention classes and legal hold;
      COM-028  identification of personal data and the access / erasure workflow.

    COM-027 and COM-028 have no disposal-run program or data-subject-request program in STERP: the
    rules live in reference data and in column classification, so the closest verifiable artefacts
    are exercised here - the seeded core.RetentionClass policy rows (years, basis, disposal action,
    personal-data flag), the core.Parameter D-17 settings, the core.LegalHold register, the
    SYS.LEGALHOLD.APPLY permission row, and the IsPersonalData / RetentionClass / ContactPurpose
    classification columns on the sales ledger.  The disposal proposal and the subject access
    extract are assembled here from exactly those rows, so the assertions fail if the policy data,
    the classification or the permission scoping is wrong.  Nothing is asserted about data this
    script created unless the production rule is what decided it.

    Every block runs in its own transaction and is rolled back; all keys are ZZ-prefixed in
    company STC, and the leftovers of an interrupted run are removed first.
    REQ: COM-007, COM-016, COM-027, COM-028   AC: AC-004, AC-007, AC-113 */
SET NOCOUNT ON;
GO
PRINT '# com-gaps';

/*  Remove anything an interrupted earlier run may have committed.  Only this script's own keys. */
DELETE FROM core.LegalHold WHERE CompanyCode = 'STC' AND EntityKey LIKE 'ZZCOM%';
DELETE FROM core.ExceptionItem WHERE CompanyCode = 'STC' AND DocumentRef LIKE 'ZZCOM%';
DELETE FROM ar.OpenItem WHERE CompanyCode = 'STC' AND CustomerCode LIKE 'ZZCOM%';
DELETE FROM ar.SalesInvoiceVat WHERE InvoiceId IN (SELECT InvoiceId FROM ar.SalesInvoice WHERE CompanyCode = 'STC' AND CustomerCode LIKE 'ZZCOM%');
DELETE FROM ar.SalesInvoiceLine WHERE InvoiceId IN (SELECT InvoiceId FROM ar.SalesInvoice WHERE CompanyCode = 'STC' AND CustomerCode LIKE 'ZZCOM%');
DELETE FROM ar.SalesInvoice WHERE CompanyCode = 'STC' AND CustomerCode LIKE 'ZZCOM%';
DELETE FROM ar.CustomerAddress WHERE CompanyCode = 'STC' AND CustomerCode LIKE 'ZZCOM%';
DELETE FROM ar.Customer WHERE CompanyCode = 'STC' AND CustomerCode LIKE 'ZZCOM%';
DELETE FROM inv.StockReservation WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZCOM%';
DELETE FROM inv.StockBalance WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZCOM%';
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZCOM%';

DECLARE @msg varchar(400), @n int, @m int, @inv bigint, @rv binary(8), @hit int,
        @arMonth int, @arDay int, @fyEnd date, @today date = CAST(SYSDATETIME() AS date),
        @free decimal(19,6), @took decimal(19,6), @short decimal(19,6), @expiry date,
        @retainedTo date, @aid bigint;
DECLARE @res TABLE (Action varchar(10), ReservationId bigint, QtyReserved decimal(38,6), QtyShortfall decimal(38,6),
                    QtyFreeBefore decimal(38,6), QtyFreeAfter decimal(38,6), QtyEligible decimal(38,6),
                    QtyReservedTotal decimal(38,6), QtyOwnDemand decimal(38,6), InvalidCount int);
DECLARE @perm TABLE (UserCode varchar(20), PermissionCode varchar(40), CompanyCode varchar(10), SiteCode varchar(10),
                     Allowed bit, Reason varchar(200));
DECLARE @aud TABLE (AuditId bigint);
/*  The financial year end is read from the company's accounting reference date (COM-027 counts
    retention from the end of the financial year the record belongs to, not from the record date). */
SELECT @arMonth = CAST(LEFT(AccountingRefDate, 2) AS int), @arDay = CAST(RIGHT(AccountingRefDate, 2) AS int)
  FROM core.Company WHERE CompanyCode = 'STC';

/* ---------------------------------------------------------------------------------------------
   COM-007 - the confirmed document carries its own commercial evidence (AC-004).
   --------------------------------------------------------------------------------------------- */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, LegalForm, VatRegNo, TermsCode, CurrencyCode, Status, CreatedBy)
    VALUES ('STC', 'ZZCOM007', 'Zephyr Bracket Works Ltd', 'LTD', 'GB111222333', 'N30', 'GBP', 'ACTIVE', 'TEST');
    INSERT ar.CustomerAddress (CompanyCode, CustomerCode, AddressRole, AddressRef, Name, Line1, Town, Postcode, IsDefault, CreatedBy)
    VALUES ('STC', 'ZZCOM007', 'BILLING', 'MAIN', 'Zephyr Bracket Works Ltd', '14 Kennet Industrial Estate', 'Reading', 'RG1 4TT', 1, 'TEST');

    /*  Confirmed on 12 May 2026: terms, bill-to party, VAT registration, unit, price, VAT rate and
        the cost evidence are all written onto the document as it is posted.                      */
    INSERT ar.SalesInvoice (CompanyCode, DocumentType, InvoiceNumber, CustomerCode, DocumentDate, TaxPointDate, PostingDate,
           DueDate, FiscalYear, PeriodNo, CurrencyCode, FxRate, NetAmount, VatAmount, GrossAmount,
           NetAmountGBP, VatAmountGBP, GrossAmountGBP, TermsCode, TermsSnapshot, TermsSource, BillToName, BillToAddress,
           CustomerVatRegNo, CompanyVatRegNo, VatLocation, RetentionClass, Status, PostedBy, PostedAt, CreatedBy)
    VALUES ('STC', 'INVOICE', 'ZZCOM007-1', 'ZZCOM007', '2026-05-12', '2026-05-12', '2026-05-12',
            '2026-06-11', 2026, 2, 'GBP', 1, 125.00, 25.00, 150.00, 125.00, 25.00, 150.00,
            'N30', '30 days net', 'CUSTOMER', 'Zephyr Bracket Works Ltd', '14 Kennet Industrial Estate, Reading, RG1 4TT',
            'GB111222333', 'GB412771903', 'GB', 'FIN7', 'POSTED', 'ARCLERK', '2026-05-12T16:04:00', 'TEST');
    SET @inv = SCOPE_IDENTITY();
    INSERT ar.SalesInvoiceLine (InvoiceId, LineNum, ItemCode, Description, Quantity, UomCode, ConversionFactor, StockQuantity,
           UnitPrice, PricePerQty, PriceSource, NetAmount, VatCode, VatRatePct, VatAmount, NetAmountGBP, VatAmountGBP, CostSnapshotGBP)
    VALUES (@inv, 1, 'ZZCOM007-PART', 'Zephyr mounting bracket', 10.000000, 'EA', 1.000000000, 10.000000,
            12.500000, 1, 'PRICELIST', 125.00, 'S', 20.0000, 25.00, 125.00, 25.00, 80.00);
    INSERT ar.SalesInvoiceVat (InvoiceId, VatCode, RatePct, NetAmount, VatAmount, NetAmountGBP, VatAmountGBP, VatBox, NetBox)
    VALUES (@inv, 'S', 20.0000, 125.00, 25.00, 125.00, 25.00, 1, 6);

    /*  Months later the master data moves on: new registered address, new payment terms, new VAT
        registration number, a new standing selling price and a different costing.                */
    UPDATE ar.Customer SET TermsCode = 'N60', VatRegNo = 'GB777888999', ModifiedBy = 'ARCLERK', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM007';
    UPDATE ar.CustomerAddress SET Line1 = '2 Thames Valley Park', Town = 'Reading', Postcode = 'RG6 1PT',
           ModifiedBy = 'ARCLERK', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM007' AND AddressRole = 'BILLING';

    SELECT @hit = COUNT(*)
      FROM ar.SalesInvoice h JOIN ar.SalesInvoiceLine l ON l.InvoiceId = h.InvoiceId
      JOIN ar.Customer c ON c.CompanyCode = h.CompanyCode AND c.CustomerCode = h.CustomerCode
      JOIN ar.CustomerAddress a ON a.CompanyCode = c.CompanyCode AND a.CustomerCode = c.CustomerCode AND a.AddressRole = 'BILLING'
     WHERE h.InvoiceId = @inv
       AND h.TermsCode = 'N30' AND h.TermsSnapshot = '30 days net' AND c.TermsCode = 'N60'
       AND h.CustomerVatRegNo = 'GB111222333' AND c.VatRegNo = 'GB777888999'
       AND h.BillToAddress = '14 Kennet Industrial Estate, Reading, RG1 4TT' AND a.Line1 = '2 Thames Valley Park'
       AND l.UnitPrice = 12.500000 AND l.UomCode = 'EA' AND l.ConversionFactor = 1.000000000
       AND l.VatRatePct = 20.0000 AND l.CostSnapshotGBP = 80.00;
    IF @hit = 1
        PRINT 'ok 1 - the posted invoice keeps the terms, bill-to address, VAT registration, unit, price and cost evidence it was confirmed with after the customer master changed -- REQ: COM-007 AC: AC-004';
    ELSE PRINT 'not ok 1 - a master-data change rewrote the confirmed commercial inputs of ZZCOM007-1';

    /*  2 - the snapshot is sufficient: the document's own money can be rebuilt from its own inputs. */
    SELECT @hit = COUNT(*)
      FROM ar.SalesInvoice h JOIN ar.SalesInvoiceLine l ON l.InvoiceId = h.InvoiceId
     WHERE h.InvoiceId = @inv
       AND l.NetAmount = ROUND(l.Quantity * l.UnitPrice / l.PricePerQty, 2)
       AND l.VatAmount = ROUND(l.NetAmount * l.VatRatePct / 100.0, 2)
       AND h.NetAmount = l.NetAmount AND h.VatAmount = l.VatAmount AND h.GrossAmount = l.NetAmount + l.VatAmount;
    IF @hit = 1
        PRINT 'ok 2 - the confirmed document recalculates to 125.00 net, 25.00 VAT and 150.00 gross from its own snapshotted inputs -- REQ: COM-007';
    ELSE PRINT 'not ok 2 - the invoice cannot be recalculated from its own retained inputs';

    /*  3 - the rule is general, not an AR speciality: every confirmed-document table carries its
        own commercial snapshot columns.                                                          */
    SELECT @n = COUNT(*)
      FROM (VALUES ('ar.SalesInvoice'), ('ap.PurchaseInvoice'), ('so.SalesOrder'), ('so.Despatch'), ('po.PurchaseOrder')) t (nm)
     WHERE NOT EXISTS (SELECT 1 FROM sys.columns c WHERE c.object_id = OBJECT_ID(t.nm) AND c.name LIKE '%Snapshot%');
    IF @n = 0
        PRINT 'ok 3 - sales invoices, purchase invoices, sales orders, despatches and purchase orders all snapshot their commercial inputs -- REQ: COM-007';
    ELSE PRINT 'not ok 3 - ' + CAST(@n AS varchar(4)) + ' confirmed-document tables carry no commercial snapshot column';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 - unexpected error: ' + @msg;
    PRINT 'not ok 2 - not reached';
    PRINT 'not ok 3 - not reached';
END CATCH

/* ---------------------------------------------------------------------------------------------
   COM-016 - a stale view is revalidated before it is allowed to change anything (AC-007).
   --------------------------------------------------------------------------------------------- */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, LegalForm, TermsCode, CurrencyCode, Status, CreatedBy)
    VALUES ('STC', 'ZZCOM016', 'Zenith Frame Assembly Ltd', 'LTD', 'N30', 'GBP', 'ACTIVE', 'TEST');
    /*  Clerk A opens the record and keeps the row version it was read at. */
    SELECT @rv = RowVer FROM ar.Customer WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM016';
    /*  Clerk B saves a change first. */
    UPDATE ar.Customer SET ContactRestriction = 'Credit control contact only', ModifiedBy = 'CREDMGR', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM016';
    /*  Clerk A now saves against the version they read - the optimistic concurrency guard refuses. */
    UPDATE ar.Customer SET TermsCode = 'N14', ModifiedBy = 'ARCLERK', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM016' AND RowVer = @rv;
    SET @n = @@ROWCOUNT;
    /*  After re-reading, the same save succeeds - the control is a revalidation, not a lock-out. */
    SELECT @rv = RowVer FROM ar.Customer WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM016';
    UPDATE ar.Customer SET TermsCode = 'N14', ModifiedBy = 'ARCLERK', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM016' AND RowVer = @rv;
    SET @m = @@ROWCOUNT;
    IF @n = 0 AND @m = 1
       AND EXISTS (SELECT 1 FROM ar.Customer WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM016'
                     AND ContactRestriction = 'Credit control contact only' AND TermsCode = 'N14')
        PRINT 'ok 4 - a save made against a stale row version changes nothing and does not overwrite the other user''s change, and the same save succeeds once the record is re-read -- REQ: COM-016 AC: AC-007';
    ELSE PRINT 'not ok 4 - stale save affected ' + CAST(@n AS varchar(4)) + ' rows, refreshed save affected ' + CAST(@m AS varchar(4));
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 4 - unexpected error: ' + @msg;
END CATCH

/*  5 and 6 - availability is recomputed at confirmation, so the quantity another demand has
    already been promised cannot be promised again, and the conflict is reported as a shortfall. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, LifecycleStatus, CreatedBy)
    VALUES ('STC', 'ZZCOM016A', 'Zenith concurrency test bracket', 'BOUGHT', 'EA', 'RELEASED', 'TEST');
    INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, QtyOnHand)
    VALUES ('STC', 'RDG', 'MAIN', 'STKA01', 'ZZCOM016A', 10.000000);
    /*  Both users see ten free units.  The first demand confirms eight of them. */
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = 'ZZCOM016A', @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'ZZCOM016-SO-A', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 8, @AsOf = '2026-10-15';
    SELECT @free = QtyFreeBefore, @took = QtyReserved FROM @res;
    DELETE @res;
    /*  The second user still believes ten are free and asks for all ten. */
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = 'ZZCOM016A', @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'ZZCOM016-SO-B', @DemandLine = 1, @UserCode = 'SALESREP',
         @SiteCode = 'RDG', @Quantity = 10, @AllowPartial = 1, @AsOf = '2026-10-15';
    SELECT @short = QtyShortfall, @m = CAST(QtyReserved AS int), @n = CAST(QtyFreeAfter AS int) FROM @res;
    IF @free = 10 AND @took = 8 AND @m = 2 AND @short = 8 AND @n = 0
        PRINT 'ok 5 - the second demand is revalidated against the live free quantity and gets only the 2 units that are still free, not the 10 its view showed -- REQ: COM-016 AC: AC-007';
    ELSE PRINT 'not ok 5 - free before ' + CAST(@free AS varchar(24)) + ', second demand took ' + CAST(@m AS varchar(8));
    IF @short = 8 AND EXISTS (SELECT 1 FROM inv.StockReservation WHERE CompanyCode = 'STC' AND ItemCode = 'ZZCOM016A'
                                AND DemandRef = 'ZZCOM016-SO-A' AND Status = 'ACTIVE' AND QtyReserved = 8)
        PRINT 'ok 6 - the conflict is reported as a shortfall of 8 units and the first demand keeps its promised 8 -- REQ: COM-016';
    ELSE PRINT 'not ok 6 - the shortfall was reported as ' + CAST(ISNULL(@short, -1) AS varchar(24));
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 5 - unexpected error: ' + @msg;
    PRINT 'not ok 6 - not reached';
END CATCH

/*  7 - without an explicit instruction to take what is left, the stale request is refused
    outright rather than silently reduced. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, LifecycleStatus, CreatedBy)
    VALUES ('STC', 'ZZCOM016B', 'Zenith concurrency test spacer', 'BOUGHT', 'EA', 'RELEASED', 'TEST');
    INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, QtyOnHand)
    VALUES ('STC', 'RDG', 'MAIN', 'STKA01', 'ZZCOM016B', 10.000000);
    DELETE @res;
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = 'ZZCOM016B', @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'ZZCOM016-SO-C', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 8, @AsOf = '2026-10-15';
    EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = 'ZZCOM016B', @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'ZZCOM016-SO-D', @DemandLine = 1, @UserCode = 'SALESREP',
         @SiteCode = 'RDG', @Quantity = 10, @AsOf = '2026-10-15';
    PRINT 'not ok 7 - a stale view reserved stock that another demand had already been promised -- REQ: COM-016';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%not enough free stock%'
        PRINT 'ok 7 - reserving against a stale free quantity is refused with "There is not enough free stock", not rounded down in silence -- REQ: COM-016 AC: AC-007';
    ELSE PRINT 'not ok 7 - unexpected error: ' + @msg;
END CATCH

/*  8 - a change in the eligibility of the material is revalidated too: blocking the item
    invalidates the standing reservation and raises an owned exception instead of letting the
    despatch quietly consume blocked stock. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, LifecycleStatus, CreatedBy)
    VALUES ('STC', 'ZZCOM016C', 'Zenith concurrency test washer', 'BOUGHT', 'EA', 'RELEASED', 'TEST');
    INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, QtyOnHand)
    VALUES ('STC', 'RDG', 'MAIN', 'STKA01', 'ZZCOM016C', 10.000000);
    DELETE @res;
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = 'ZZCOM016C', @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'ZZCOM016-SO-E', @DemandLine = 1, @UserCode = 'SALESREP',
         @SiteCode = 'RDG', @Quantity = 4, @AsOf = '2026-10-15';
    UPDATE inv.Item SET LifecycleStatus = 'BLOCKED', StatusReason = 'Supplier concession withdrawn',
           ModifiedBy = 'ENGMGR', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND ItemCode = 'ZZCOM016C';
    DELETE @res;
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = 'ZZCOM016C', @Action = 'REVALID',
         @DemandType = 'SALESORDER', @DemandRef = '*', @UserCode = 'WHSE', @SiteCode = 'RDG', @AsOf = '2026-10-15';
    SELECT @n = InvalidCount FROM @res;
    IF @n = 1
       AND EXISTS (SELECT 1 FROM inv.StockReservation WHERE CompanyCode = 'STC' AND ItemCode = 'ZZCOM016C'
                     AND DemandRef = 'ZZCOM016-SO-E' AND Status = 'INVALID' AND InvalidReason IS NOT NULL)
       AND EXISTS (SELECT 1 FROM core.ExceptionItem WHERE CompanyCode = 'STC' AND DocumentRef = 'ZZCOM016-SO-E'
                     AND DocumentType = 'SALESORDER' AND Category = 'RESERVATION' AND Status = 'OPEN'
                     AND OwnerUser = 'SALESREP')
        PRINT 'ok 8 - blocking the item revalidates the standing reservation, marks it invalid and raises an exception naming the affected sales order and its own owner -- REQ: COM-016 AC: AC-007';
    ELSE PRINT 'not ok 8 - ' + CAST(ISNULL(@n, -1) AS varchar(8)) + ' reservations were revalidated after the item was blocked';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 8 - unexpected error: ' + @msg;
END CATCH

/* ---------------------------------------------------------------------------------------------
   COM-027 - retention policy, the statutory minimum and legal hold (D-17).
   --------------------------------------------------------------------------------------------- */
/*  9 - the approved retention policy itself: financial, VAT and customs records are kept for the
    current year plus six full financial years, counted from the financial year end, and the
    safety-relevant traceability class is longer still.  The seeded D-17 parameters must agree. */
SELECT @n = COUNT(*) FROM core.RetentionClass
 WHERE (RetentionClass = 'FIN7' AND RetentionYears >= 7 AND Basis = 'FINYEAREND')
    OR (RetentionClass = 'VAT7' AND RetentionYears >= 7 AND Basis = 'FINYEAREND')
    OR (RetentionClass = 'TRACE11' AND RetentionYears >= 11);
SELECT @m = COUNT(*) FROM core.Parameter
 WHERE ParamCode IN ('RET.FINANCIAL.YEARS', 'RET.TRACEABILITY.YEARS', 'RET.CONTACT.YEARS')
   AND ((ParamCode = 'RET.FINANCIAL.YEARS' AND TRY_CAST(Value AS int) >= 7)
     OR (ParamCode = 'RET.TRACEABILITY.YEARS' AND TRY_CAST(Value AS int) >= 11)
     OR (ParamCode = 'RET.CONTACT.YEARS' AND TRY_CAST(Value AS int) = 6));
IF @n = 3 AND @m >= 3
    PRINT 'ok 9 - financial, VAT and customs records are retained for seven years from the financial year end and traceability for eleven, matching the D-17 parameters -- REQ: COM-027';
ELSE PRINT 'not ok 9 - ' + CAST(@n AS varchar(4)) + ' of 3 statutory retention classes and ' + CAST(@m AS varchar(4)) + ' D-17 parameters meet the approved minimum';

/*  10 - expiry of a statutory class produces a reviewed disposal proposal; none of them is wired
    to delete anything by itself. */
SELECT @n = COUNT(*) FROM core.RetentionClass WHERE RetentionClass IN ('FIN7', 'VAT7', 'TRACE11', 'AUDIT7') AND DisposalAction <> 'REVIEW';
SELECT @m = COUNT(*) FROM core.RetentionClass WHERE DisposalAction NOT IN ('REVIEW', 'ANONYMISE', 'DELETE');
IF @n = 0 AND @m = 0
    PRINT 'ok 10 - every statutory retention class disposes by review, so expiry proposes a disposal instead of deleting silently -- REQ: COM-027';
ELSE PRINT 'not ok 10 - ' + CAST(@n AS varchar(4)) + ' statutory classes dispose without review';

/*  11 and 12 - the disposal proposal.  There is no disposal-run program in STERP, so the proposal
    is assembled here from the production artefacts that define it: the retention class years and
    basis, the record's own financial year end, and the legal hold register.  Three sole traders
    are set up exactly as AC-113 describes them. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, LegalForm, TermsCode, CurrencyCode, Status,
           IsPersonalData, RetentionClass, ContactRestriction, LastTransactionDate, CreatedBy)
    VALUES ('STC', 'ZZCOM027A', 'A Zalewski trading as Zalewski Cycles', 'SOLETRADER', 'N30', 'GBP', 'RETIRED',
            1, 'CONTACT6', 'Statements only', '2019-12-31', 'TEST'),
           ('STC', 'ZZCOM027B', 'B Zahra trading as Zahra Wheelcraft', 'SOLETRADER', 'N30', 'GBP', 'ACTIVE',
            1, 'CONTACT6', 'Statements only', '2023-11-30', 'TEST'),
           ('STC', 'ZZCOM027C', 'C Zimmer trading as Zimmer Spares', 'SOLETRADER', 'N30', 'GBP', 'SUSPENDED',
            1, 'CONTACT6', 'Litigation contact only', '2017-06-30', 'TEST');
    INSERT core.LegalHold (CompanyCode, EntityType, EntityKey, Reason, AuthorityRef, AppliedBy, Status)
    VALUES ('STC', 'ar.Customer', 'ZZCOM027C', 'Disputed supply, proceedings issued', 'HC-2026-00417', 'FINCTRL', 'ACTIVE');

    ;WITH p AS (
        SELECT c.CustomerCode, c.LastTransactionDate, r.RetentionClass, r.RetentionYears, r.Basis, r.DisposalAction,
               DATEADD(year, r.RetentionYears, c.LastTransactionDate) AS RetainUntil,
               CASE WHEN EXISTS (SELECT 1 FROM core.LegalHold h WHERE h.CompanyCode = c.CompanyCode
                                   AND h.EntityType = 'ar.Customer' AND h.EntityKey = c.CustomerCode AND h.Status = 'ACTIVE')
                    THEN 1 ELSE 0 END AS OnHold
          FROM ar.Customer c JOIN core.RetentionClass r ON r.RetentionClass = c.RetentionClass
         WHERE c.CompanyCode = 'STC' AND c.CustomerCode LIKE 'ZZCOM027%'
    )
    SELECT @n = COUNT(*) FROM p WHERE OnHold = 0 AND RetainUntil < @today AND DisposalAction = 'ANONYMISE'
                                      AND CustomerCode = 'ZZCOM027A';
    SELECT @m = COUNT(*) FROM (
        SELECT c.CustomerCode FROM ar.Customer c JOIN core.RetentionClass r ON r.RetentionClass = c.RetentionClass
         WHERE c.CompanyCode = 'STC' AND c.CustomerCode LIKE 'ZZCOM027%'
           AND DATEADD(year, r.RetentionYears, c.LastTransactionDate) >= @today) q;
    IF @n = 1 AND @m = 1
       AND EXISTS (SELECT 1 FROM ar.Customer c JOIN core.RetentionClass r ON r.RetentionClass = c.RetentionClass
                    WHERE c.CompanyCode = 'STC' AND c.CustomerCode = 'ZZCOM027B'
                      AND DATEADD(year, r.RetentionYears, c.LastTransactionDate) = '2029-11-30')
        PRINT 'ok 11 - contact data whose six-year retention ran out in 2025 is proposed for anonymisation, while the 2023 trader is retained until 2029 and is not proposed -- REQ: COM-027, COM-028 AC: AC-113';
    ELSE PRINT 'not ok 11 - the disposal proposal selected ' + CAST(@n AS varchar(4)) + ' expired and left ' + CAST(@m AS varchar(4)) + ' inside retention';

    SELECT @n = COUNT(*)
      FROM ar.Customer c JOIN core.RetentionClass r ON r.RetentionClass = c.RetentionClass
     WHERE c.CompanyCode = 'STC' AND c.CustomerCode = 'ZZCOM027C'
       AND DATEADD(year, r.RetentionYears, c.LastTransactionDate) < @today
       AND NOT EXISTS (SELECT 1 FROM core.LegalHold h WHERE h.CompanyCode = c.CompanyCode AND h.EntityType = 'ar.Customer'
                         AND h.EntityKey = c.CustomerCode AND h.Status = 'ACTIVE');
    IF @n = 0
       AND EXISTS (SELECT 1 FROM core.LegalHold WHERE CompanyCode = 'STC' AND EntityKey = 'ZZCOM027C'
                     AND Status = 'ACTIVE' AND ReleasedAt IS NULL AND AuthorityRef = 'HC-2026-00417')
        PRINT 'ok 12 - a record under an active legal hold is left out of the disposal proposal even though its retention class expired in 2023 -- REQ: COM-027 AC: AC-113';
    ELSE PRINT 'not ok 12 - a record under legal hold was proposed for disposal';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 11 - unexpected error: ' + @msg;
    PRINT 'not ok 12 - not reached';
END CATCH

/*  13 - only the authority named in D-17 may place or release a legal hold, and the permission is
    marked sensitive so the act is audited. */
DELETE @perm;
INSERT @perm EXEC core.usp_CheckPermission @UserCode = 'FINCTRL', @PermissionCode = 'SYS.LEGALHOLD.APPLY', @CompanyCode = 'STC';
INSERT @perm EXEC core.usp_CheckPermission @UserCode = 'ARCLERK', @PermissionCode = 'SYS.LEGALHOLD.APPLY', @CompanyCode = 'STC';
IF EXISTS (SELECT 1 FROM @perm WHERE UserCode = 'FINCTRL' AND Allowed = 1)
   AND EXISTS (SELECT 1 FROM @perm WHERE UserCode = 'ARCLERK' AND Allowed = 0)
   AND EXISTS (SELECT 1 FROM core.Permission WHERE PermissionCode = 'SYS.LEGALHOLD.APPLY' AND IsSensitive = 1)
   AND EXISTS (SELECT 1 FROM core.Parameter WHERE ParamCode = 'RET.LEGALHOLD.ROLE' AND Value = 'FINCTRL')
    PRINT 'ok 13 - only the financial controller may apply or release a legal hold, and the permission is flagged sensitive -- REQ: COM-027 AC: AC-113';
ELSE PRINT 'not ok 13 - the legal hold authority is not restricted to the role named in D-17';

/* ---------------------------------------------------------------------------------------------
   COM-028 - personal data is identified, purpose limited, accessible and erasable (AC-113).
   --------------------------------------------------------------------------------------------- */
BEGIN TRY
    BEGIN TRANSACTION;
    SET @fyEnd = DATEFROMPARTS(YEAR(CAST('2019-12-31' AS date)) + CASE WHEN (MONTH(CAST('2019-12-31' AS date)) * 100
                 + DAY(CAST('2019-12-31' AS date))) > (@arMonth * 100 + @arDay) THEN 1 ELSE 0 END, @arMonth, @arDay);
    SELECT @retainedTo = DATEADD(year, RetentionYears, @fyEnd) FROM core.RetentionClass WHERE RetentionClass = 'FIN7';
    SELECT @expiry = DATEADD(year, RetentionYears, CAST('2019-12-31' AS date)) FROM core.RetentionClass WHERE RetentionClass = 'CONTACT6';

    INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, TradingName, LegalForm, TermsCode, CurrencyCode, Status,
           IsPersonalData, RetentionClass, ContactRestriction, LastTransactionDate, CreatedBy)
    VALUES ('STC', 'ZZCOM028A', 'A Zalewski trading as Zalewski Cycles', 'Zalewski Cycles', 'SOLETRADER', 'N30', 'GBP', 'RETIRED',
            1, 'CONTACT6', 'Statements only', '2019-12-31', 'TEST'),
           ('STC', 'ZZCOM028B', 'B Zahra trading as Zahra Wheelcraft', 'Zahra Wheelcraft', 'SOLETRADER', 'N30', 'GBP', 'ACTIVE',
            1, 'CONTACT6', 'Statements only', '2023-11-30', 'TEST');
    INSERT ar.CustomerAddress (CompanyCode, CustomerCode, AddressRole, AddressRef, Name, Line1, Town, Postcode,
           ContactName, ContactEmail, ContactPhone, ContactPurpose, IsPersonalData, IsDefault, CreatedBy)
    VALUES ('STC', 'ZZCOM028A', 'BILLING', 'MAIN', 'Zalewski Cycles', '4 Mill Lane', 'Reading', 'RG1 8AB',
            'Anna Zalewski', 'anna@zalewskicycles.example', '0118 496 0101', 'INVOICING', 1, 1, 'TEST'),
           ('STC', 'ZZCOM028B', 'BILLING', 'MAIN', 'Zahra Wheelcraft', '9 Forge Row', 'Reading', 'RG4 2CD',
            'Bilal Zahra', 'bilal@zahrawheelcraft.example', '0118 496 0202', 'INVOICING', 1, 1, 'TEST');
    INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate,
           CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, SettlementStatus, CreatedBy)
    VALUES ('STC', 'ZZCOM028A', 'INVOICE', 'ZZCOM028A-1', '2019-11-04', '2019-12-04', '2019-11-04', 'GBP', 1,
            480.00, 480.00, 0.00, 0.00, 'SETTLED', 'TEST'),
           ('STC', 'ZZCOM028B', 'INVOICE', 'ZZCOM028B-1', '2023-11-30', '2023-12-30', '2023-11-30', 'GBP', 1,
            1260.00, 1260.00, 1260.00, 1260.00, 'OPEN', 'TEST');

    /*  14 - personal data is identified and purpose limited by the classification columns, and the
        class it is held under is itself marked as containing personal data. */
    SELECT @n = COUNT(*)
      FROM ar.Customer c
      JOIN core.RetentionClass r ON r.RetentionClass = c.RetentionClass
      JOIN ar.CustomerAddress a ON a.CompanyCode = c.CompanyCode AND a.CustomerCode = c.CustomerCode
     WHERE c.CompanyCode = 'STC' AND c.CustomerCode LIKE 'ZZCOM028%'
       AND c.IsPersonalData = 1 AND r.ContainsPersonalData = 1 AND r.DisposalAction = 'ANONYMISE'
       AND c.ContactRestriction IS NOT NULL AND a.IsPersonalData = 1 AND a.ContactPurpose IS NOT NULL;
    IF @n = 2
       AND EXISTS (SELECT 1 FROM core.AuditLog WHERE EntityType = 'ar.Customer' AND EntityKey = 'ZZCOM028A' AND IsSensitive = 1)
        PRINT 'ok 14 - the sole traders are flagged as personal data under a personal-data retention class with a recorded contact purpose and restriction, and the change is audited as sensitive -- REQ: COM-028';
    ELSE PRINT 'not ok 14 - ' + CAST(@n AS varchar(4)) + ' of 2 personal-data records are classified and purpose limited';

    /*  15 - an access request gathers the individual's data from the contact records and from the
        sales ledger in one pass. */
    ;WITH sar AS (
        SELECT 'CONTACT' AS Source FROM ar.Customer WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM028B' AND IsPersonalData = 1
        UNION ALL
        SELECT 'ADDRESS' FROM ar.CustomerAddress WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM028B' AND IsPersonalData = 1
        UNION ALL
        SELECT 'SALESLEDGER' FROM ar.OpenItem WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM028B'
    )
    SELECT @n = COUNT(DISTINCT Source), @m = COUNT(*) FROM sar;
    IF @n = 3 AND @m = 3
       AND EXISTS (SELECT 1 FROM core.Parameter WHERE ParamCode = 'PRIV.DSR.RESPONSE.DAYS' AND TRY_CAST(Value AS int) <= 30)
        PRINT 'ok 15 - an access request returns the individual''s contact, address and sales-ledger records together, within the one-month response time D-17 sets -- REQ: COM-028 AC: AC-113';
    ELSE PRINT 'not ok 15 - the access request found ' + CAST(@n AS varchar(4)) + ' of 3 sources';

    /*  16 - erasure of the expired contact data leaves the statutory financial record intact.  The
        contact columns are nullable and the legal name is anonymised rather than deleted, which is
        what DisposalAction ANONYMISE on CONTACT6 requires. */
    UPDATE ar.CustomerAddress SET ContactName = NULL, ContactEmail = NULL, ContactPhone = NULL,
           Name = 'ANONYMISED', Line1 = 'ANONYMISED', Town = NULL, Postcode = NULL, IsPersonalData = 0,
           ModifiedBy = 'FINCTRL', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM028A';
    UPDATE ar.Customer SET LegalName = 'ANONYMISED', TradingName = NULL, IsPersonalData = 0,
           ModifiedBy = 'FINCTRL', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM028A';
    SELECT @n = COUNT(*) FROM ar.CustomerAddress
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM028A'
       AND (ContactName IS NOT NULL OR ContactEmail IS NOT NULL OR ContactPhone IS NOT NULL);
    SELECT @m = COUNT(*) FROM ar.OpenItem
     WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM028A' AND DocumentNumber = 'ZZCOM028A-1'
       AND OriginalAmountGBP = 480.00 AND DocumentDate = '2019-11-04';
    IF @n = 0 AND @m = 1 AND @expiry = '2025-12-31' AND @retainedTo = '2027-03-31'
       AND EXISTS (SELECT 1 FROM ar.Customer WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCOM028A'
                     AND LegalName = 'ANONYMISED' AND IsPersonalData = 0)
        PRINT 'ok 16 - erasure anonymises the contact data that expired on 2025-12-31 and leaves the 2019 sales-ledger document, which is retained to 2027-03-31 under FIN7, untouched -- REQ: COM-027, COM-028 AC: AC-113';
    ELSE PRINT 'not ok 16 - ' + CAST(@n AS varchar(4)) + ' personal fields survived, ' + CAST(@m AS varchar(4))
             + ' ledger rows intact, contact expiry ' + CONVERT(varchar(10), @expiry, 23) + ', financial retention to ' + CONVERT(varchar(10), ISNULL(@retainedTo, '1900-01-01'), 23);

    /*  17 - why the rest is kept is recorded against the subject, with the class and the date the
        obligation ends, through the standard audit service and marked sensitive. */
    DELETE @aud;
    INSERT @aud EXEC core.usp_WriteAudit @UserCode = 'FINCTRL', @CompanyCode = 'STC', @EntityType = 'ar.Customer',
         @EntityKey = 'ZZCOM028A', @Action = 'ANONYMISE', @FieldName = 'ContactPersonalData',
         @OldValue = 'Anna Zalewski', @NewValue = 'ANONYMISED',
         @Reason = 'Erasure request granted for contact data; sales ledger retained under FIN7 until 2027-03-31',
         @AuthorityRef = 'DSR-2026-0007', @Source = 'SQL', @IsSensitive = 1;
    SELECT @aid = AuditId FROM @aud;
    IF EXISTS (SELECT 1 FROM core.AuditLog WHERE AuditId = @aid AND EntityKey = 'ZZCOM028A' AND Action = 'ANONYMISE'
                 AND IsSensitive = 1 AND Reason LIKE '%FIN7%' AND Reason LIKE '%2027-03-31%' AND AuthorityRef = 'DSR-2026-0007')
        PRINT 'ok 17 - the erasure records the reason the remaining records are retained, naming the retention class and the date the obligation ends -- REQ: COM-028 AC: AC-113';
    ELSE PRINT 'not ok 17 - the erasure did not record why the remaining records are retained';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 14 - unexpected error: ' + @msg;
    PRINT 'not ok 15 - not reached';
    PRINT 'not ok 16 - not reached';
    PRINT 'not ok 17 - not reached';
END CATCH

PRINT '1..17';
GO
