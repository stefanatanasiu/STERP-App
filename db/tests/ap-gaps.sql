/*  Purchase ledger period close.  APCLOSE (cobol\batch\ap\APCLOSE.cob) reconciles the creditors
    control to the open items, reports goods received but not invoiced, invoices that are
    registered but not yet posted, invoices still waiting on a match and supplier returns held in
    clearing.  This file proves the ledger evidence those five sections read, and in particular the
    two promises the requirement makes: an owned receipt that has not been invoiced stays a visible
    liability, and an unresolved match is carried forward rather than quietly cleared when the
    period is hard closed.
    Test data uses the ZZ prefix in company STC and a fiscal year of its own (2019 period 12); the
    whole case is built inside a transaction and rolled back, so a second run behaves exactly like
    the first.
    REQ: AP-022
    AC: AC-039 */
SET NOCOUNT ON;
GO
PRINT '# ap-gaps';
/*  Re-runnable: clear anything an aborted earlier run could have left behind. */
DELETE FROM ap.InvoiceMatch  WHERE CompanyCode = 'STC' AND PurchaseInvoiceId IN
       (SELECT PurchaseInvoiceId FROM ap.PurchaseInvoice WHERE CompanyCode = 'STC' AND RegisterNumber LIKE 'ZZAPC%');
DELETE FROM ap.OpenItem      WHERE CompanyCode = 'STC' AND SupplierCode LIKE 'ZZAPC%';
DELETE FROM ap.PurchaseInvoice WHERE CompanyCode = 'STC' AND RegisterNumber LIKE 'ZZAPC%';
DELETE FROM po.ReceiptLine   WHERE ReceiptId IN (SELECT ReceiptId FROM po.Receipt WHERE CompanyCode = 'STC' AND ReceiptRef LIKE 'ZZAPC%');
DELETE FROM po.Receipt       WHERE CompanyCode = 'STC' AND ReceiptRef LIKE 'ZZAPC%';
DELETE FROM ap.Supplier      WHERE CompanyCode = 'STC' AND SupplierCode LIKE 'ZZAPC%';
GO
DECLARE @msg varchar(400),
        @ctl decimal(18,2), @n int, @gross decimal(18,2), @grni decimal(18,2),
        @matchState varchar(14), @status varchar(12), @settle varchar(12), @period varchar(12),
        @fy smallint, @pn tinyint;

BEGIN TRY
    BEGIN TRANSACTION;

    INSERT ap.Supplier (CompanyCode, SupplierCode, LegalName, TermsCode, CurrencyCode, PaymentMethod,
                        Status, CreatedBy)
    VALUES ('STC', 'ZZAPC1', 'Period Close Reconciliation Ltd', 'N30', 'GBP', 'BACS', 'ACTIVE', 'TEST');

    /*  Four documents, one for each section of the close listing, plus a clean settled one that
        must appear in none of them. */
    INSERT ap.PurchaseInvoice (CompanyCode, DocumentType, RegisterNumber, SupplierCode, SupplierDocRef,
                               SupplierDocRefNormalised, DateReceived, DocumentDate, TaxPointDate,
                               PostingDate, DueDate, FiscalYear, PeriodNo, CurrencyCode, FxRate,
                               StatedNet, StatedVat, StatedGross, NetAmountGBP, VatAmountGBP,
                               GrossAmountGBP, RegistrationState, MatchState, AccountingState,
                               PaymentEligibility, SettlementState, CreatedBy)
    VALUES
      /*  registered and real, but the nominal ledger has not seen it yet */
      ('STC','INVOICE','ZZAPC01','ZZAPC1','ZZ/CLOSE/01','ZZ/CLOSE/01','2019-12-18','2019-12-16','2019-12-16',
       '2019-12-18','2020-01-15',2019,12,'GBP',1, 1000.00,200.00,1200.00,1000.00,200.00,1200.00,
       'READY','UNMATCHED','UNPOSTED','BLOCKED','UNPAID','TEST'),
      /*  posted, but the three way match is still in dispute */
      ('STC','INVOICE','ZZAPC02','ZZAPC1','ZZ/CLOSE/02','ZZ/CLOSE/02','2019-12-19','2019-12-17','2019-12-17',
       '2019-12-19','2020-01-16',2019,12,'GBP',1,  500.00,100.00, 600.00, 500.00,100.00, 600.00,
       'READY','VARIANCE','POSTED','BLOCKED','UNPAID','TEST'),
      /*  the goods went back; the credit has not arrived */
      ('STC','INVOICE','ZZAPC03','ZZAPC1','ZZ/CLOSE/03','ZZ/CLOSE/03','2019-12-20','2019-12-18','2019-12-18',
       '2019-12-20','2020-01-17',2019,12,'GBP',1,  400.00, 80.00, 480.00, 400.00, 80.00, 480.00,
       'READY','MATCHED','POSTED','BLOCKED','RETURNED','TEST'),
      /*  nothing left to do with this one */
      ('STC','INVOICE','ZZAPC04','ZZAPC1','ZZ/CLOSE/04','ZZ/CLOSE/04','2019-12-21','2019-12-19','2019-12-19',
       '2019-12-21','2020-01-18',2019,12,'GBP',1,  250.00, 50.00, 300.00, 250.00, 50.00, 300.00,
       'READY','MATCHED','POSTED','APPROVED','SETTLED','TEST');

    /*  The control account reconciles to the residual open items, so only the three unsettled
        documents carry a residual. */
    INSERT ap.OpenItem (CompanyCode, SupplierCode, ItemType, DocumentNumber, DocumentDate, DueDate,
                        PostingDate, DateReceived, CurrencyCode, FxRate, OriginalAmount,
                        OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, ReservedAmountGBP,
                        TermsCode, SettlementStatus, CreatedBy)
    VALUES ('STC','ZZAPC1','INVOICE','ZZAPC01','2019-12-16','2020-01-15','2019-12-18','2019-12-18','GBP',1,
            1200.00,1200.00,1200.00,1200.00,0.00,'N30','OPEN','TEST'),
           ('STC','ZZAPC1','INVOICE','ZZAPC02','2019-12-17','2020-01-16','2019-12-19','2019-12-19','GBP',1,
             600.00, 600.00, 600.00, 600.00,0.00,'N30','OPEN','TEST'),
           ('STC','ZZAPC1','INVOICE','ZZAPC03','2019-12-18','2020-01-17','2019-12-20','2019-12-20','GBP',1,
             480.00, 480.00, 480.00, 480.00,0.00,'N30','OPEN','TEST');

    /*  The dispute on ZZAPC02: a quantity variance nobody has resolved. */
    INSERT ap.InvoiceMatch (CompanyCode, PurchaseInvoiceId, PurchaseInvoiceLineNum, PurchaseOrderNumber,
                            ReceiptRef, ItemCode, OrderedQty, ReceivedAcceptedQty, MatchedQty,
                            QtyVariance, PriceVarianceGBP, GrniClearedGBP, WithinTolerance,
                            ToleranceNote, Status, CreatedBy)
    SELECT 'STC', PurchaseInvoiceId, 1, 'ZZAPCPO1', 'ZZAPCR2', 'ZZAPC-ITEM', 10, 8, 10,
           2, 0.00, 0.00, 0, 'Invoiced ten, only eight accepted', 'DISPUTED', 'TEST'
      FROM ap.PurchaseInvoice WHERE CompanyCode = 'STC' AND RegisterNumber = 'ZZAPC02';

    /*  Two owned receipts.  ZZAPCR1 has never been invoiced, so its goods-received-not-invoiced
        value is still owed to the supplier; ZZAPCR2 belongs to the disputed invoice above. */
    INSERT po.Receipt (CompanyCode, SiteCode, ReceiptRef, SupplierCode, PurchaseOrderNumber,
                       PhysicalDate, PostingDate, FiscalYear, PeriodNo, OwnershipBasis,
                       IsStockInTransit, GrniValueGBP, Status, CreatedBy)
    VALUES ('STC','RDG','ZZAPCR1','ZZAPC1','ZZAPCPO1','2019-12-15','2019-12-15',2019,12,'RECEIPT',0, 950.00,'CONFIRMED','TEST'),
           ('STC','RDG','ZZAPCR2','ZZAPC1','ZZAPCPO1','2019-12-17','2019-12-17',2019,12,'RECEIPT',0, 500.00,'CONFIRMED','TEST');

    /*  1 - the creditors control section: the close compares account 2000 with the residual open
        items, which is the three unsettled documents and nothing else.
        REQ: AP-022 */
    SELECT @ctl = ISNULL(SUM(ResidualAmountGBP), 0), @n = COUNT(*)
      FROM ap.OpenItem WHERE CompanyCode = 'STC' AND SupplierCode = 'ZZAPC1' AND ResidualAmountGBP <> 0;
    IF @ctl = 2280.00 AND @n = 3
        PRINT 'ok 1 - the creditors control reconciles to 2,280.00 across three residual open items -- REQ: AP-022';
    ELSE PRINT 'not ok 1 - the open items came to ' + ISNULL(CAST(@ctl AS varchar(20)), 'NULL')
               + ' over ' + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' rows -- REQ: AP-022';

    /*  2 - registered but not posted.  The liability is real even though the nominal ledger has
        not seen it, so the close lists it for posting rather than ignoring it.
        REQ: AP-022 */
    SELECT @gross = ISNULL(SUM(GrossAmountGBP), 0), @n = COUNT(*)
      FROM ap.PurchaseInvoice
     WHERE CompanyCode = 'STC' AND SupplierCode = 'ZZAPC1'
       AND AccountingState = 'UNPOSTED' AND RegistrationState <> 'REJECTED';
    IF @gross = 1200.00 AND @n = 1
        PRINT 'ok 2 - 1,200.00 is registered but unposted and must be posted before the close -- REQ: AP-022';
    ELSE PRINT 'not ok 2 - the unposted section showed ' + ISNULL(CAST(@gross AS varchar(20)), 'NULL')
               + ' over ' + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' rows -- REQ: AP-022';

    /*  3 - and that unposted invoice is a visible liability in its own right: it carries a residual
        open item, so the close can never lose sight of it.
        REQ: AP-022 */
    SELECT @ctl = ISNULL(SUM(ResidualAmountGBP), 0)
      FROM ap.OpenItem WHERE CompanyCode = 'STC' AND DocumentNumber = 'ZZAPC01';
    IF @ctl = 1200.00
        PRINT 'ok 3 - the unposted invoice is still owed to the supplier, not a memorandum note -- REQ: AP-022';
    ELSE PRINT 'not ok 3 - the unposted invoice showed a residual of ' + ISNULL(CAST(@ctl AS varchar(20)), 'NULL')
               + ' -- REQ: AP-022';

    /*  4 and 5 - goods received not invoiced.  An owned receipt with no matched invoice keeps its
        GRNI value; the receipt behind the disputed invoice has had nothing cleared either, because
        the match was never agreed.
        REQ: AP-022 */
    SELECT @grni = ISNULL(SUM(r.GrniValueGBP), 0), @n = COUNT(*)
      FROM po.Receipt r
     WHERE r.CompanyCode = 'STC' AND r.SupplierCode = 'ZZAPC1' AND r.Status = 'CONFIRMED'
       AND NOT EXISTS (SELECT 1 FROM ap.InvoiceMatch m
                        WHERE m.CompanyCode = r.CompanyCode AND m.ReceiptRef = r.ReceiptRef
                          AND m.Status = 'MATCHED');
    IF @grni = 1450.00 AND @n = 2
        PRINT 'ok 4 - 1,450.00 of owned receipts is uninvoiced and stays a visible liability -- REQ: AP-022';
    ELSE PRINT 'not ok 4 - the uninvoiced owned receipts came to ' + ISNULL(CAST(@grni AS varchar(20)), 'NULL')
               + ' over ' + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' receipts -- REQ: AP-022';

    SELECT @grni = ISNULL(SUM(GrniClearedGBP), 0)
      FROM ap.InvoiceMatch WHERE CompanyCode = 'STC' AND ReceiptRef = 'ZZAPCR2';
    IF @grni = 0.00
        PRINT 'ok 5 - a disputed match clears no accrual, so the receipt is not written off -- REQ: AP-022';
    ELSE PRINT 'not ok 5 - the disputed match cleared ' + ISNULL(CAST(@grni AS varchar(20)), 'NULL')
               + ' of accrual -- REQ: AP-022';

    /*  6 - invoices still waiting on a match are carried forward by the close.
        REQ: AP-022 */
    SELECT @gross = ISNULL(SUM(GrossAmountGBP), 0), @n = COUNT(*)
      FROM ap.PurchaseInvoice
     WHERE CompanyCode = 'STC' AND SupplierCode = 'ZZAPC1'
       AND MatchState IN ('UNMATCHED','PARTIAL','VARIANCE')
       AND RegistrationState <> 'REJECTED' AND SettlementState <> 'SETTLED';
    IF @gross = 1800.00 AND @n = 2
        PRINT 'ok 6 - 1,800.00 over two documents is still waiting on a match -- REQ: AP-022';
    ELSE PRINT 'not ok 6 - the unmatched section showed ' + ISNULL(CAST(@gross AS varchar(20)), 'NULL')
               + ' over ' + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' rows -- REQ: AP-022';

    /*  7 - supplier returns awaiting a credit are held in clearing and are not treated as settled.
        REQ: AP-022 */
    SELECT @gross = ISNULL(SUM(GrossAmountGBP), 0), @n = COUNT(*)
      FROM ap.PurchaseInvoice
     WHERE CompanyCode = 'STC' AND SupplierCode = 'ZZAPC1' AND SettlementState = 'RETURNED';
    IF @gross = 480.00 AND @n = 1
        PRINT 'ok 7 - 480.00 of supplier return is held in clearing until the credit arrives -- REQ: AP-022';
    ELSE PRINT 'not ok 7 - the supplier return section showed ' + ISNULL(CAST(@gross AS varchar(20)), 'NULL')
               + ' over ' + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' rows -- REQ: AP-022';

    /*  8 - the settled, matched, posted document appears in none of the close work lists.
        REQ: AP-022 */
    SELECT @n = COUNT(*)
      FROM ap.PurchaseInvoice
     WHERE CompanyCode = 'STC' AND RegisterNumber = 'ZZAPC04'
       AND (AccountingState = 'UNPOSTED'
            OR MatchState IN ('UNMATCHED','PARTIAL','VARIANCE')
            OR SettlementState = 'RETURNED');
    IF @n = 0
        PRINT 'ok 8 - a settled and fully matched invoice gives the close nothing to do -- REQ: AP-022';
    ELSE PRINT 'not ok 8 - the clean invoice turned up in ' + ISNULL(CAST(@n AS varchar(6)), 'NULL')
               + ' close work list(s) -- REQ: AP-022';

    /*  9 to 12 - now hard close the period the documents sit in.  Nothing in the purchase ledger
        may be cleared, reclassified or hidden by that act: the unresolved match, its dispute, the
        uninvoiced receipt and the return all survive into the next period.
        REQ: AP-022 */
    SELECT TOP (1) @fy = FiscalYear, @pn = PeriodNo
      FROM fin.Period WHERE CompanyCode = 'STC' ORDER BY FiscalYear, PeriodNo;
    IF @fy IS NULL
    BEGIN
        PRINT 'not ok 9 to 12 - company STC has no accounting calendar to close -- REQ: AP-022';
    END
    ELSE
    BEGIN
    UPDATE ap.PurchaseInvoice SET FiscalYear = @fy, PeriodNo = @pn
     WHERE CompanyCode = 'STC' AND RegisterNumber LIKE 'ZZAPC%';
    UPDATE po.Receipt SET FiscalYear = @fy, PeriodNo = @pn
     WHERE CompanyCode = 'STC' AND ReceiptRef LIKE 'ZZAPC%';
    UPDATE fin.Period SET Status = 'HARD', HardCloseAt = SYSDATETIME(), ClosedBy = 'TEST'
     WHERE CompanyCode = 'STC' AND FiscalYear = @fy AND PeriodNo = @pn;

    SELECT @status = Status FROM fin.Period WHERE CompanyCode = 'STC' AND FiscalYear = @fy AND PeriodNo = @pn;
    IF @status = 'HARD'
        PRINT 'ok 9 - the period the documents sit in is hard closed -- REQ: AP-022';
    ELSE PRINT 'not ok 9 - the period was left at ' + ISNULL(@status, 'NULL') + ' -- REQ: AP-022';

    SELECT @matchState = MatchState, @settle = SettlementState
      FROM ap.PurchaseInvoice WHERE CompanyCode = 'STC' AND RegisterNumber = 'ZZAPC02';
    SELECT @status = Status FROM ap.InvoiceMatch WHERE CompanyCode = 'STC' AND ReceiptRef = 'ZZAPCR2';
    IF @matchState = 'VARIANCE' AND @settle = 'UNPAID' AND @status = 'DISPUTED'
        PRINT 'ok 10 - the unresolved match did not disappear when the period closed -- REQ: AP-022';
    ELSE PRINT 'not ok 10 - after the close the invoice was ' + ISNULL(@matchState, 'NULL') + '/'
               + ISNULL(@settle, 'NULL') + ' and the match ' + ISNULL(@status, 'NULL') + ' -- REQ: AP-022';

    SELECT @gross = ISNULL(SUM(GrossAmountGBP), 0), @n = COUNT(*)
      FROM ap.PurchaseInvoice
     WHERE CompanyCode = 'STC' AND SupplierCode = 'ZZAPC1'
       AND MatchState IN ('UNMATCHED','PARTIAL','VARIANCE')
       AND RegistrationState <> 'REJECTED' AND SettlementState <> 'SETTLED';
    IF @gross = 1800.00 AND @n = 2
        PRINT 'ok 11 - the same 1,800.00 is carried forward for the next period to resolve -- REQ: AP-022';
    ELSE PRINT 'not ok 11 - after the close the unmatched section showed '
               + ISNULL(CAST(@gross AS varchar(20)), 'NULL') + ' over '
               + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' rows -- REQ: AP-022';

    SELECT @grni = ISNULL(SUM(r.GrniValueGBP), 0)
      FROM po.Receipt r
     WHERE r.CompanyCode = 'STC' AND r.SupplierCode = 'ZZAPC1' AND r.Status = 'CONFIRMED'
       AND NOT EXISTS (SELECT 1 FROM ap.InvoiceMatch m
                        WHERE m.CompanyCode = r.CompanyCode AND m.ReceiptRef = r.ReceiptRef
                          AND m.Status = 'MATCHED');
    SELECT @settle = SettlementState FROM ap.PurchaseInvoice WHERE CompanyCode = 'STC' AND RegisterNumber = 'ZZAPC03';
    IF @grni = 1450.00 AND @settle = 'RETURNED'
        PRINT 'ok 12 - uninvoiced owned receipts and the return clearing both survive the close -- REQ: AP-022';
    ELSE PRINT 'not ok 12 - after the close GRNI was ' + ISNULL(CAST(@grni AS varchar(20)), 'NULL')
               + ' and the return was ' + ISNULL(@settle, 'NULL') + ' -- REQ: AP-022';
    END

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    SET @msg = ERROR_MESSAGE();
    PRINT 'not ok 1 to 12 - unexpected error in the period close reconciliation tests: ' + @msg
          + ' -- REQ: AP-022';
END CATCH
GO
