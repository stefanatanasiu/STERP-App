/*  Sales ledger document numbering: the series the collection, dispute, insolvency and direct
    debit programs ask for must exist and must issue a number for every company.  ARDISPUT and
    ARINSOLV request ARCASE, DDMAND requests DDMAN and DDCOL.  A missing series made
    DebtorsService.RaiseDispute fail with STERP-E-0004 / DSP0061 and no dispute could be raised.
    Every assertion runs inside a transaction that is rolled back.
    REQ: AR-016, AR-022, COM-005   AC: AC-030 */
SET NOCOUNT ON;
GO
PRINT '# ar-numbering';
DECLARE @n int, @missing int, @doc varchar(30), @co varchar(10) = 'STC';

/*  1 - every company carries the three series the sales ledger engine asks for. */
SELECT @missing = COUNT(*)
  FROM core.Company c
 CROSS JOIN (VALUES ('ARCASE'), ('DDMAN'), ('DDCOL')) AS s (SeriesCode)
 WHERE NOT EXISTS (SELECT 1 FROM core.NumberSeries n
                    WHERE n.CompanyCode = c.CompanyCode AND n.SeriesCode = s.SeriesCode AND n.IsActive = 1);
IF @missing = 0 PRINT 'ok 1 - every company has an active ARCASE, DDMAN and DDCOL series -- REQ: COM-005, AR-016';
ELSE PRINT 'not ok 1 - ' + CAST(@missing AS varchar(10)) + ' company and series combinations are missing or inactive';

/*  2 - the collection case series issues a number, which is what ARDISPUT asks NUMGEN for. */
BEGIN TRANSACTION;
BEGIN TRY
    EXEC core.usp_NextDocumentNumber @CompanyCode = @co, @SeriesCode = 'ARCASE', @UserCode = 'CREDMGR',
         @CorrelationId = 'ar-numbering-test';
    SELECT TOP (1) @doc = DocumentNumber FROM core.DocumentNumber
     WHERE CompanyCode = @co AND SeriesCode = 'ARCASE' AND CorrelationId = 'ar-numbering-test'
     ORDER BY SequenceNo DESC;
END TRY
BEGIN CATCH
    SET @doc = NULL;
    PRINT '# ARCASE refused: ' + ERROR_MESSAGE();
END CATCH
IF @doc IS NOT NULL AND LEN(@doc) > 0
    PRINT 'ok 2 - a collection case number is issued (' + @doc + ') so a dispute can be raised -- REQ: AR-016, AC-030';
ELSE PRINT 'not ok 2 - no collection case number could be issued from the ARCASE series';

/*  3 - the direct debit series issue as well, so a mandate and a collection can be numbered. */
SET @n = 0;
BEGIN TRY
    EXEC core.usp_NextDocumentNumber @CompanyCode = @co, @SeriesCode = 'DDMAN', @UserCode = 'ARCLERK',
         @CorrelationId = 'ar-numbering-test';
    EXEC core.usp_NextDocumentNumber @CompanyCode = @co, @SeriesCode = 'DDCOL', @UserCode = 'ARCLERK',
         @CorrelationId = 'ar-numbering-test';
    SELECT @n = COUNT(*) FROM core.DocumentNumber
     WHERE CompanyCode = @co AND SeriesCode IN ('DDMAN', 'DDCOL') AND CorrelationId = 'ar-numbering-test';
END TRY
BEGIN CATCH
    PRINT '# direct debit series refused: ' + ERROR_MESSAGE();
END CATCH
IF @n = 2 PRINT 'ok 3 - a mandate and a collection instruction are both numbered -- REQ: AR-022, COM-005';
ELSE PRINT 'not ok 3 - ' + CAST(@n AS varchar(10)) + ' of 2 direct debit numbers were issued';
ROLLBACK TRANSACTION;
GO
