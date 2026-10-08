/*  Posting control tests: journal balance, analysis dimensions, period status, business event
    mapping and the no-negative-stock rule.  Each assertion rolls back.
    REQ: FIN-006, FIN-010, FIN-013, FIN-014, COM-006, ITM-013, D-03, D-09, D-15 */
SET NOCOUNT ON;
GO
PRINT '# core-posting';
DECLARE @msg varchar(400), @cid varchar(64), @num varchar(30), @before decimal(19,6), @after decimal(19,6);

/*  1 - a journal that does not balance is refused. */
BEGIN TRY
    BEGIN TRANSACTION;
    SET @cid = 'TST-JNL-UNBAL';
    INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, Narrative)
    VALUES (@cid, 1, 'STC', '6800', 100.00, 0, 'GBP', 'ADMIN', 'Test debit'),
           (@cid, 2, 'STC', '1340', 0, 90.00, 'GBP', NULL, 'Test credit');
    EXEC fin.usp_PostJournal @CompanyCode = 'STC', @CorrelationId = @cid, @UserCode = 'GLCLERK',
        @Description = 'Unbalanced test journal', @PostingDate = '2026-10-15', @JournalType = 'MANUAL',
        @CurrencyCode = 'GBP', @SeriesCode = 'JNL', @FxRate = 1.0;
    PRINT 'not ok 1 - an unbalanced journal was posted -- REQ: FIN-006';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0011%' PRINT 'ok 1 - an unbalanced journal is refused -- REQ: FIN-006, COM-006';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - a mandatory analysis dimension must be present. */
BEGIN TRY
    BEGIN TRANSACTION;
    SET @cid = 'TST-JNL-NODIM';
    INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, Narrative)
    VALUES (@cid, 1, 'STC', '6800', 100.00, 0, 'GBP', NULL, 'Missing cost centre'),
           (@cid, 2, 'STC', '1340', 0, 100.00, 'GBP', NULL, 'Petty cash');
    EXEC fin.usp_PostJournal @CompanyCode = 'STC', @CorrelationId = @cid, @UserCode = 'GLCLERK',
        @Description = 'Journal without a cost centre', @PostingDate = '2026-10-15', @JournalType = 'MANUAL',
        @CurrencyCode = 'GBP', @SeriesCode = 'JNL', @FxRate = 1.0;
    PRINT 'not ok 2 - a journal without a mandatory dimension was posted -- REQ: FIN-013';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0013%' PRINT 'ok 2 - a missing mandatory dimension is refused -- REQ: FIN-013, FIN-005';
    ELSE PRINT 'not ok 2 - unexpected error: ' + @msg;
END CATCH

/*  3 - a hard closed period cannot receive a posting. */
BEGIN TRY
    BEGIN TRANSACTION;
    SET @cid = 'TST-JNL-CLOSED';
    INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, Narrative)
    VALUES (@cid, 1, 'STC', '6800', 100.00, 0, 'GBP', 'ADMIN', 'Closed period debit'),
           (@cid, 2, 'STC', '1340', 0, 100.00, 'GBP', NULL, 'Closed period credit');
    EXEC fin.usp_PostJournal @CompanyCode = 'STC', @CorrelationId = @cid, @UserCode = 'GLCLERK',
        @Description = 'Journal into a closed period', @PostingDate = '2026-05-15', @JournalType = 'MANUAL',
        @CurrencyCode = 'GBP', @SeriesCode = 'JNL', @FxRate = 1.0;
    PRINT 'not ok 3 - a posting entered a hard closed period -- REQ: FIN-010';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0009%' PRINT 'ok 3 - a closed period refuses the posting -- REQ: FIN-010, D-15';
    ELSE PRINT 'not ok 3 - unexpected error: ' + @msg;
END CATCH

/*  4 - a business event with no approved account mapping is blocked, not suspensed. */
BEGIN TRY
    BEGIN TRANSACTION;
    SET @cid = 'TST-JNL-NOMAP';
    INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, Narrative)
    VALUES (@cid, 1, 'STC', '6800', 100.00, 0, 'GBP', 'ADMIN', 'Unmapped event debit'),
           (@cid, 2, 'STC', '1340', 0, 100.00, 'GBP', NULL, 'Unmapped event credit');
    EXEC fin.usp_PostJournal @CompanyCode = 'STC', @CorrelationId = @cid, @UserCode = 'GLCLERK',
        @Description = 'Journal for an unmapped event', @PostingDate = '2026-10-15', @JournalType = 'AUTO',
        @CurrencyCode = 'GBP', @SeriesCode = 'JNL', @FxRate = 1.0, @EventCode = 'XX.NOT.MAPPED';
    PRINT 'not ok 4 - an unmapped business event was posted -- REQ: FIN-014';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0014%' PRINT 'ok 4 - an unmapped business event is blocked -- REQ: FIN-014, XMD-005';
    ELSE PRINT 'not ok 4 - unexpected error: ' + @msg;
END CATCH

/*  5 - a correct journal posts, numbers itself and records balanced totals. */
BEGIN TRY
    BEGIN TRANSACTION;
    SET @cid = 'TST-JNL-GOOD';
    INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, Narrative)
    VALUES (@cid, 1, 'STC', '6800', 240.00, 0, 'GBP', 'ADMIN', 'Sundry expense'),
           (@cid, 2, 'STC', '1340', 0, 240.00, 'GBP', NULL, 'Paid from petty cash');
    EXEC fin.usp_PostJournal @CompanyCode = 'STC', @CorrelationId = @cid, @UserCode = 'GLCLERK',
        @Description = 'Petty cash expense', @PostingDate = '2026-10-15', @JournalType = 'MANUAL',
        @CurrencyCode = 'GBP', @SeriesCode = 'JNL', @FxRate = 1.0, @EventCode = 'FIN.ACCRUAL';
    SELECT TOP (1) @num = JournalNumber FROM fin.Journal
     WHERE CompanyCode = 'STC' AND Description = 'Petty cash expense' ORDER BY JournalId DESC;
    IF @num IS NOT NULL
       AND EXISTS (SELECT 1 FROM fin.JournalLine WHERE JournalId = (SELECT TOP (1) JournalId FROM fin.Journal
                    WHERE CompanyCode = 'STC' AND JournalNumber = @num)
                    GROUP BY JournalId HAVING SUM(DebitAmountGBP) = 240.00 AND SUM(CreditAmountGBP) = 240.00)
       AND NOT EXISTS (SELECT 1 FROM fin.JournalStage WHERE CorrelationId = @cid)
        PRINT 'ok 5 - a balanced journal posts and is numbered ' + @num + ' -- REQ: FIN-006, FIN-011';
    ELSE PRINT 'not ok 5 - the journal did not post as expected';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 5 - unexpected error: ' + @msg;
END CATCH

/*  6 - a stock movement may never create a negative balance. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = 'MAIN',
        @LocationCode = 'STKB01', @ItemCode = 'CHAIN-116L-11S', @MovementType = 'ISSUE', @Quantity = 999999,
        @MovementDate = '2026-10-15', @UserCode = 'WHSE', @SourceModule = 'ITM', @SourceDocType = 'TEST';
    PRINT 'not ok 6 - an issue drove the balance negative -- REQ: ITM-013';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0015%' PRINT 'ok 6 - negative stock is refused -- REQ: ITM-013, D-09';
    ELSE PRINT 'not ok 6 - unexpected error: ' + @msg;
END CATCH

/*  7 - a valid receipt updates the stock balance and writes a movement. */
BEGIN TRY
    BEGIN TRANSACTION;
    SELECT @before = ISNULL(SUM(QtyOnHand), 0) FROM inv.StockBalance
     WHERE CompanyCode = 'STC' AND SiteCode = 'RDG' AND LocationCode = 'STKB01' AND ItemCode = 'CHAIN-116L-11S';
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = 'MAIN',
        @LocationCode = 'STKB01', @ItemCode = 'CHAIN-116L-11S', @MovementType = 'RECEIPT', @Quantity = 25,
        @MovementDate = '2026-10-15', @UserCode = 'WHSE', @UnitCost = 12.500000, @SourceModule = 'ITM', @SourceDocType = 'TEST';
    SELECT @after = ISNULL(SUM(QtyOnHand), 0) FROM inv.StockBalance
     WHERE CompanyCode = 'STC' AND SiteCode = 'RDG' AND LocationCode = 'STKB01' AND ItemCode = 'CHAIN-116L-11S';
    IF @after = @before + 25 PRINT 'ok 7 - a receipt increases the stock balance -- REQ: ITM-013, ITM-014';
    ELSE PRINT 'not ok 7 - the balance moved from ' + CAST(@before AS varchar(20)) + ' to ' + CAST(@after AS varchar(20));
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 7 - unexpected error: ' + @msg;
END CATCH

/*  8 - a lot controlled item cannot move without a lot reference. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = 'MAIN',
        @LocationCode = 'RAW01', @ItemCode = 'TUBE-AL6061-28X1.5', @MovementType = 'RECEIPT', @Quantity = 10,
        @MovementDate = '2026-10-15', @UserCode = 'WHSE', @LotCode = '', @SourceModule = 'ITM', @SourceDocType = 'TEST';
    PRINT 'not ok 8 - a lot controlled item moved without a lot -- REQ: ITM-010';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0027%' PRINT 'ok 8 - a lot reference is required on every movement -- REQ: ITM-010, D-09';
    ELSE PRINT 'not ok 8 - unexpected error: ' + @msg;
END CATCH
GO
