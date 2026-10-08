/*  Cross-cutting control tests: gap-free document numbering with traceable voids, idempotent
    repetition, scoped access rights, exchange rate availability and the business day calendar.
    REQ: COM-002, COM-005, COM-010, COM-013, COM-015, COM-019, D-14, D-15 */
SET NOCOUNT ON;
GO
PRINT '# core-controls';
DECLARE @n TABLE (CompanyCode varchar(10), SeriesCode varchar(20), DocumentNumber varchar(30), SequenceNo bigint);
DECLARE @p TABLE (UserCode varchar(20), PermissionCode varchar(40), CompanyCode varchar(10), SiteCode varchar(10),
                  Allowed bit, Reason varchar(200));
DECLARE @k TABLE (CorrelationId varchar(64), Operation varchar(60), Outcome varchar(12), ResultRef varchar(60), ResultMessage varchar(400));
DECLARE @f TABLE (CurrencyCode char(3), RateDate date, RateType varchar(6), UnitsPerGBP decimal(18,8), Source varchar(40), AgeDays int);
DECLARE @b TABLE (CalendarCode varchar(10), CheckDate date, IsBusinessDay bit, DayType varchar(12), HolidayName varchar(80),
                  NextBusinessDay date, PreviousBusinessDay date);
DECLARE @msg varchar(400), @doc varchar(30), @cnt int, @rate decimal(18,8), @bd bit, @next date;

/*  1 - every issued number is unique and the sequence is continuous. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @n;
    INSERT @n EXEC core.usp_NextDocumentNumber @CompanyCode = 'STC', @SeriesCode = 'SINV', @UserCode = 'ARCLERK';
    INSERT @n EXEC core.usp_NextDocumentNumber @CompanyCode = 'STC', @SeriesCode = 'SINV', @UserCode = 'ARCLERK';
    INSERT @n EXEC core.usp_NextDocumentNumber @CompanyCode = 'STC', @SeriesCode = 'SINV', @UserCode = 'ARCLERK';
    SELECT @cnt = COUNT(DISTINCT DocumentNumber) FROM @n;
    IF @cnt = 3 AND (SELECT MAX(SequenceNo) - MIN(SequenceNo) FROM @n) = 2
        PRINT 'ok 1 - document numbers are unique and continuous -- REQ: COM-005, D-15';
    ELSE PRINT 'not ok 1 - the series issued ' + CAST(@cnt AS varchar(4)) + ' distinct numbers';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - a voided number stays traceable with its reason and is never reissued. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @n;
    INSERT @n EXEC core.usp_NextDocumentNumber @CompanyCode = 'STC', @SeriesCode = 'SINV', @UserCode = 'ARCLERK';
    SELECT TOP (1) @doc = DocumentNumber FROM @n;
    EXEC core.usp_VoidDocumentNumber @CompanyCode = 'STC', @SeriesCode = 'SINV', @DocumentNumber = @doc,
        @UserCode = 'ARCLERK', @Reason = 'Printer failure, invoice destroyed';
    IF EXISTS (SELECT 1 FROM core.DocumentNumber WHERE CompanyCode = 'STC' AND DocumentNumber = @doc
                 AND Status = 'VOID' AND VoidReason = 'Printer failure, invoice destroyed')
       AND EXISTS (SELECT 1 FROM core.AuditLog WHERE EntityType = 'core.DocumentNumber' AND Action = 'VOID'
                     AND EntityKey = 'SINV/' + @doc)
       AND NOT EXISTS (SELECT 1 FROM core.NumberSeries WHERE CompanyCode = 'STC' AND SeriesCode = 'SINV'
                         AND NextNumber <= (SELECT SequenceNo FROM core.DocumentNumber WHERE CompanyCode = 'STC' AND DocumentNumber = @doc))
        PRINT 'ok 2 - a voided number is traceable and not reissued -- REQ: COM-005, COM-013, D-15';
    ELSE PRINT 'not ok 2 - the void was not recorded as expected for ' + ISNULL(@doc, 'NULL');
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 2 - unexpected error: ' + @msg;
END CATCH

/*  3 - repeating a completed instruction returns the original outcome, not a second effect. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @k;
    INSERT @k EXEC core.usp_IdempotentBegin @CorrelationId = 'TST-IDEM-1', @Operation = 'AR.RECEIPT.POST',
        @CompanyCode = 'STC', @UserCode = 'ARCLERK';
    EXEC core.usp_IdempotentEnd @CorrelationId = 'TST-IDEM-1', @Status = 'COMPLETED', @ResultRef = 'RCS00000042',
        @ResultMessage = 'Receipt posted';
    DELETE @k;
    INSERT @k EXEC core.usp_IdempotentBegin @CorrelationId = 'TST-IDEM-1', @Operation = 'AR.RECEIPT.POST',
        @CompanyCode = 'STC', @UserCode = 'ARCLERK';
    IF EXISTS (SELECT 1 FROM @k WHERE Outcome = 'DUPLICATE' AND ResultRef = 'RCS00000042')
        PRINT 'ok 3 - a repeated instruction returns the original result -- REQ: COM-015';
    ELSE PRINT 'not ok 3 - the repeat did not report the original result';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 3 - unexpected error: ' + @msg;
END CATCH

/*  4 - a restricted user is allowed in their own company and site only. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @p;
    INSERT @p EXEC core.usp_CheckPermission @UserCode = 'RESTRICT', @PermissionCode = 'ITM.ITEM.VIEW',
        @CompanyCode = 'STC', @SiteCode = 'RDG';
    INSERT @p EXEC core.usp_CheckPermission @UserCode = 'RESTRICT', @PermissionCode = 'ITM.ITEM.VIEW',
        @CompanyCode = 'CAL', @SiteCode = 'GLA';
    INSERT @p EXEC core.usp_CheckPermission @UserCode = 'RESTRICT', @PermissionCode = 'ITM.ITEM.VIEW',
        @CompanyCode = 'STC', @SiteCode = 'BHM';
    IF EXISTS (SELECT 1 FROM @p WHERE CompanyCode = 'STC' AND SiteCode = 'RDG' AND Allowed = 1)
       AND EXISTS (SELECT 1 FROM @p WHERE CompanyCode = 'CAL' AND Allowed = 0)
       AND EXISTS (SELECT 1 FROM @p WHERE SiteCode = 'BHM' AND Allowed = 0)
        PRINT 'ok 4 - the restricted user is denied company CAL and site BHM -- REQ: COM-002, COM-019';
    ELSE PRINT 'not ok 4 - the restricted user access scope is wrong';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 4 - unexpected error: ' + @msg;
END CATCH

/*  5 - the restricted user may not see unmasked bank details. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @p;
    INSERT @p EXEC core.usp_CheckPermission @UserCode = 'RESTRICT', @PermissionCode = 'SYS.BANKDETAIL.VIEW',
        @CompanyCode = 'STC', @SiteCode = 'RDG';
    INSERT @p EXEC core.usp_CheckPermission @UserCode = 'TREASURY', @PermissionCode = 'SYS.BANKDETAIL.VIEW',
        @CompanyCode = 'STC', @SiteCode = 'RDG';
    IF EXISTS (SELECT 1 FROM @p WHERE UserCode = 'RESTRICT' AND Allowed = 0)
       AND EXISTS (SELECT 1 FROM @p WHERE UserCode = 'TREASURY' AND Allowed = 1)
        PRINT 'ok 5 - bank details are limited to banking authority -- REQ: COM-021, D-17';
    ELSE PRINT 'not ok 5 - sensitive permission scope is wrong';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 5 - unexpected error: ' + @msg;
END CATCH

/*  6 - a missing exchange rate is refused rather than defaulted. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @f;
    INSERT @f EXEC core.usp_GetFxRate @CurrencyCode = 'JPY', @RateDate = '2026-10-15', @RateType = 'SPOT';
    PRINT 'not ok 6 - a missing exchange rate was accepted -- REQ: COM-010';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0010%' PRINT 'ok 6 - a missing exchange rate is refused -- REQ: COM-010, D-14';
    ELSE PRINT 'not ok 6 - unexpected error: ' + @msg;
END CATCH

/*  7 - a seeded rate is returned as foreign units per pound. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @f;
    INSERT @f EXEC core.usp_GetFxRate @CurrencyCode = 'EUR', @RateDate = '2026-10-15', @RateType = 'SPOT';
    SELECT TOP (1) @rate = UnitsPerGBP FROM @f;
    IF @rate > 0.5 AND @rate < 3.0 PRINT 'ok 7 - the euro spot rate is quoted per pound -- REQ: COM-010, D-14';
    ELSE PRINT 'not ok 7 - the euro rate returned was ' + ISNULL(CAST(@rate AS varchar(20)), 'NULL');
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 7 - unexpected error: ' + @msg;
END CATCH

/*  8 - bank holidays are honoured by the calendar and by business day arithmetic. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @b;
    INSERT @b EXEC core.usp_IsBusinessDay @CalendarCode = 'ENG', @Date = '2026-12-25';
    SELECT TOP (1) @bd = IsBusinessDay FROM @b;
    SET @next = core.fn_AddBusinessDays('ENG', '2026-12-24', 1);
    IF @bd = 0 AND @next > '2026-12-25'
        PRINT 'ok 8 - Christmas Day is not a business day and is skipped -- REQ: COM-009, D-15';
    ELSE PRINT 'not ok 8 - the calendar returned ' + ISNULL(CAST(@bd AS varchar(2)), 'NULL') + ' and next day ' + ISNULL(CAST(@next AS varchar(12)), 'NULL');
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 8 - unexpected error: ' + @msg;
END CATCH

/*  9 - changing protected master data writes an audit trail entry. */
BEGIN TRY
    BEGIN TRANSACTION;
    UPDATE core.Parameter SET Value = '61', ModifiedBy = 'FINCTRL', ModifiedAt = SYSDATETIME()
     WHERE ParamCode = 'TERMS.MAX.DAYS' AND CompanyCode = '*' AND EffectiveFrom = '2024-04-01';
    IF EXISTS (SELECT 1 FROM core.AuditLog WHERE EntityType LIKE '%Parameter%' AND NewValue = '61')
        PRINT 'ok 9 - a parameter change is audited -- REQ: COM-013, COM-018';
    ELSE PRINT 'not ok 9 - no audit entry was written for the parameter change';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 9 - unexpected error: ' + @msg;
END CATCH

/*  10 - a sanctions screening match is reported, not ignored. */
BEGIN TRY
    BEGIN TRANSACTION;
    DECLARE @scr TABLE (ScreeningId bigint, CompanyCode varchar(10), PartyType varchar(12), PartyCode varchar(20),
                        PartyName varchar(120), MatchStatus varchar(12), MatchedEntryId int, MatchedName varchar(120),
                        MatchScore decimal(9,4), Decision varchar(20), ScreenedAt datetime2(3), Note varchar(400));
    INSERT @scr EXEC core.usp_ScreenParty @CompanyCode = 'STC', @PartyType = 'SUPPLIER', @PartyCode = 'S0026',
        @PartyName = 'Ardenne Tooling SARL', @Country = 'Belgium', @UserCode = 'APMGR', @TriggerEvent = 'ONBOARD';
    IF EXISTS (SELECT 1 FROM @scr WHERE MatchStatus IN ('MATCH', 'POTENTIAL'))
        PRINT 'ok 10 - a sanctions list match is reported -- REQ: COM-029, D-29';
    ELSE PRINT 'not ok 10 - the screening did not report a match';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 10 - unexpected error: ' + @msg;
END CATCH
GO
