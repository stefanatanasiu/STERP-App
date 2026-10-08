/*  fin.usp_PostJournal - the single posting contract for every module.  Header values are
    parameters; lines are staged in fin.JournalStage under the caller's CorrelationId because
    COBOL cannot pass table-valued parameters.  The posting is all-or-nothing, balanced in the
    document currency and in sterling, uses active postable accounts, complete mandatory
    dimensions, an approved business-event mapping (never a silent suspense account) and an open
    period.  Repeating the same correlation id returns the original journal.
    REQ: FIN-003, FIN-004, FIN-005, COM-009, COM-010, COM-011, COM-014, COM-015 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE fin.usp_PostJournal @CompanyCode varchar(10), @CorrelationId varchar(64), @UserCode varchar(20), @Description varchar(200), @PostingDate date,
    @JournalType varchar(12) = 'GL', @DocumentDate date = NULL, @CurrencyCode char(3) = 'GBP', @SourceModule varchar(6) = 'FIN', @SourceRef varchar(40) = NULL,
    @EventCode varchar(30) = NULL, @SeriesCode varchar(20) = 'JNL', @FxRate decimal(18,8) = NULL, @EvidenceRef varchar(120) = NULL,
    @SuppressResult bit = 0
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @CorrelationId IS NULL OR LEN(LTRIM(@CorrelationId)) = 0 THROW 50016, 'STERP-E-0016: A correlation id is required so that a repeated posting instruction has no second effect.', 1;
    SET @DocumentDate = ISNULL(@DocumentDate, @PostingDate);
    DECLARE @prior varchar(60);
    SELECT @prior = ResultRef FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId AND Status = 'COMPLETED';
    IF @prior IS NOT NULL
    BEGIN
        IF @SuppressResult = 0
            SELECT j.JournalId, j.JournalNumber, j.CompanyCode, j.FiscalYear, j.PeriodNo, j.TotalDebitGBP, j.TotalCreditGBP, j.Status, CAST(1 AS bit) AS WasDuplicate
              FROM fin.Journal j WHERE j.CompanyCode = @CompanyCode AND j.JournalNumber = @prior;
        RETURN;
    END
    IF NOT EXISTS (SELECT 1 FROM fin.JournalStage WHERE CorrelationId = @CorrelationId)
        THROW 50017, 'STERP-E-0017: No journal lines are staged against that correlation id.', 1;
    /*  Period, accounts, dimensions, mapping and currency are all checked before anything is written. */
    DECLARE @fy smallint, @pn tinyint, @st varchar(8);
    SELECT @fy = FiscalYear, @pn = PeriodNo, @st = Status FROM fin.Period WHERE CompanyCode = @CompanyCode AND @PostingDate BETWEEN StartDate AND EndDate;
    IF @st IS NULL THROW 50009, 'STERP-E-0009: No financial period covers that posting date for this company.', 1;
    IF @st IN ('HARD', 'FUTURE') THROW 50009, 'STERP-E-0009: That accounting period is closed for posting. Post the entry in the current open period.', 1;
    IF @st = 'SOFT' AND core.fn_HasPermission(@UserCode, 'FIN.PERIOD.SOFTPOST', @CompanyCode, NULL) = 0
        THROW 50009, 'STERP-E-0009: That accounting period is soft closed. Only an authorised close adjustment may be posted into it.', 1;
    IF EXISTS (SELECT 1 FROM fin.JournalStage s LEFT JOIN fin.Account a ON a.CompanyCode = @CompanyCode AND a.AccountCode = s.AccountCode
                WHERE s.CorrelationId = @CorrelationId
                  AND (a.AccountCode IS NULL OR a.Status <> 'ACTIVE' OR a.AccountKind <> 'POSTING' OR a.ValidFrom > @PostingDate OR (a.ValidTo IS NOT NULL AND a.ValidTo < @PostingDate)))
        THROW 50012, 'STERP-E-0012: A journal line uses an account that is unknown, retired, not a posting account or not valid on the posting date.', 1;
    IF @JournalType = 'GL' AND @SourceModule = 'FIN'
       AND EXISTS (SELECT 1 FROM fin.JournalStage s JOIN fin.Account a ON a.CompanyCode = @CompanyCode AND a.AccountCode = s.AccountCode
                    WHERE s.CorrelationId = @CorrelationId AND a.AllowManualJournal = 0)
        THROW 50012, 'STERP-E-0012: A control account on this journal may only be posted by its own subledger, not by a manual entry.', 1;
    IF EXISTS (SELECT 1 FROM fin.JournalStage s JOIN fin.AccountDimRule r ON r.CompanyCode = @CompanyCode AND r.AccountCode = s.AccountCode
                WHERE s.CorrelationId = @CorrelationId AND r.Requirement = 'MANDATORY'
                  AND ((r.DimensionCode = 'CC' AND (s.DimCostCentre IS NULL OR s.DimCostCentre = '')) OR (r.DimensionCode = 'PROJ' AND (s.DimProject IS NULL OR s.DimProject = ''))))
        THROW 50013, 'STERP-E-0013: A mandatory analysis dimension is missing on a journal line for an account that requires it.', 1;
    IF EXISTS (SELECT 1 FROM fin.JournalStage s JOIN fin.AccountDimRule r ON r.CompanyCode = @CompanyCode AND r.AccountCode = s.AccountCode
                WHERE s.CorrelationId = @CorrelationId AND r.Requirement = 'FORBIDDEN'
                  AND ((r.DimensionCode = 'CC' AND s.DimCostCentre IS NOT NULL AND s.DimCostCentre <> '') OR (r.DimensionCode = 'PROJ' AND s.DimProject IS NOT NULL AND s.DimProject <> '')))
        THROW 50013, 'STERP-E-0013: A journal line carries an analysis dimension that is not permitted for that account.', 1;
    IF EXISTS (SELECT 1 FROM fin.JournalStage s WHERE s.CorrelationId = @CorrelationId AND s.DimCostCentre IS NOT NULL AND s.DimCostCentre <> ''
                 AND NOT EXISTS (SELECT 1 FROM fin.DimensionValue d WHERE d.CompanyCode = @CompanyCode AND d.DimensionCode = 'CC' AND d.DimValue = s.DimCostCentre AND d.IsActive = 1))
        THROW 50013, 'STERP-E-0013: A journal line uses a cost centre that is unknown or no longer active.', 1;
    IF EXISTS (SELECT 1 FROM fin.JournalStage s WHERE s.CorrelationId = @CorrelationId AND s.DimProject IS NOT NULL AND s.DimProject <> ''
                 AND NOT EXISTS (SELECT 1 FROM fin.DimensionValue d WHERE d.CompanyCode = @CompanyCode AND d.DimensionCode = 'PROJ' AND d.DimValue = s.DimProject AND d.IsActive = 1))
        THROW 50013, 'STERP-E-0013: A journal line uses a project code that is unknown or no longer active.', 1;
    IF @EventCode IS NOT NULL AND NOT EXISTS (SELECT 1 FROM fin.EventAccountMap WHERE CompanyCode = @CompanyCode AND EventCode = @EventCode AND IsActive = 1)
        THROW 50014, 'STERP-E-0014: No approved account mapping exists for that business event. The posting is blocked rather than sent to a suspense account.', 1;
    DECLARE @base char(3) = 'GBP';
    SELECT @base = BaseCurrency FROM core.Company WHERE CompanyCode = @CompanyCode;
    IF @CurrencyCode <> @base AND @FxRate IS NULL
        SELECT TOP (1) @FxRate = UnitsPerGBP FROM core.FxRate WHERE CurrencyCode = @CurrencyCode AND RateType = 'SPOT' AND RateDate <= @DocumentDate ORDER BY RateDate DESC;
    IF @CurrencyCode = @base SET @FxRate = 1;
    IF @FxRate IS NULL OR @FxRate = 0
        THROW 50010, 'STERP-E-0010: No positive exchange rate is held for that currency and date, so the sterling amounts cannot be established.', 1;
    UPDATE fin.JournalStage SET DebitAmountGBP = ROUND(DebitAmount / @FxRate, 2), CreditAmountGBP = ROUND(CreditAmount / @FxRate, 2) WHERE CorrelationId = @CorrelationId;
    DECLARE @td decimal(18,2), @tc decimal(18,2), @tdg decimal(18,2), @tcg decimal(18,2);
    SELECT @td = SUM(DebitAmount), @tc = SUM(CreditAmount), @tdg = SUM(DebitAmountGBP), @tcg = SUM(CreditAmountGBP) FROM fin.JournalStage WHERE CorrelationId = @CorrelationId;
    IF @td <> @tc OR @tdg <> @tcg
        THROW 50011, 'STERP-E-0011: The journal does not balance. Debits and credits must agree in the document currency and in sterling before it can be posted.', 1;
    IF @td = 0 THROW 50011, 'STERP-E-0011: A journal with no value cannot be posted.', 1;
    DECLARE @num TABLE (CompanyCode varchar(10), SeriesCode varchar(20), DocumentNumber varchar(30), SequenceNo bigint);
    DECLARE @jnum varchar(30), @jid bigint, @own bit = 0;
    IF @@TRANCOUNT = 0 BEGIN BEGIN TRANSACTION; SET @own = 1; END
    BEGIN TRY
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId)
            INSERT core.IdempotencyKey (CorrelationId, Operation, CompanyCode, UserCode, Status) VALUES (@CorrelationId, 'fin.usp_PostJournal', @CompanyCode, @UserCode, 'INPROGRESS');
        INSERT @num EXEC core.usp_NextDocumentNumber @CompanyCode, @SeriesCode, @UserCode, @CorrelationId;
        SELECT @jnum = DocumentNumber FROM @num;
        INSERT fin.Journal (CompanyCode, JournalNumber, JournalType, Description, SourceModule, SourceRef, EventCode, DocumentDate, PostingDate, FiscalYear, PeriodNo,
                            CurrencyCode, FxRate, FxRateDate, FxRateType, TotalDebit, TotalCredit, TotalDebitGBP, TotalCreditGBP, Status, PreparedBy, PostedBy, PostedAt,
                            CorrelationId, EvidenceRef, CreatedBy)
        VALUES (@CompanyCode, @jnum, @JournalType, @Description, @SourceModule, @SourceRef, @EventCode, @DocumentDate, @PostingDate, @fy, @pn,
                @CurrencyCode, @FxRate, @DocumentDate, CASE WHEN @CurrencyCode = @base THEN NULL ELSE 'SPOT' END, @td, @tc, @tdg, @tcg, 'POSTED', @UserCode, @UserCode, SYSDATETIME(),
                @CorrelationId, @EvidenceRef, @UserCode);
        SET @jid = CAST(SCOPE_IDENTITY() AS bigint);
        INSERT fin.JournalLine (JournalId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, DebitAmountGBP, CreditAmountGBP, CurrencyCode,
                                DimCostCentre, DimProject, Narrative, VatCode, JobRef, SubledgerType, SubledgerRef, SourceRef, BankAccountCode)
        SELECT @jid, s.LineNum, @CompanyCode, s.AccountCode, s.DebitAmount, s.CreditAmount, s.DebitAmountGBP, s.CreditAmountGBP, s.CurrencyCode,
               NULLIF(s.DimCostCentre, ''), NULLIF(s.DimProject, ''), s.Narrative, s.VatCode, s.JobRef, s.SubledgerType, s.SubledgerRef, ISNULL(s.SourceRef, @SourceRef), s.BankAccountCode
          FROM fin.JournalStage s WHERE s.CorrelationId = @CorrelationId ORDER BY s.LineNum;
        DELETE fin.JournalStage WHERE CorrelationId = @CorrelationId;
        UPDATE core.IdempotencyKey SET Status = 'COMPLETED', ResultRef = @jnum, ResultMessage = 'Journal posted', EndedAt = SYSDATETIME() WHERE CorrelationId = @CorrelationId;
        INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, NewValue, Reason, Source, CorrelationId)
        VALUES (@UserCode, @CompanyCode, 'fin.Journal', @jnum, 'POST', CAST(@tdg AS varchar(30)), @Description, 'SQL', @CorrelationId);
        IF @own = 1 COMMIT TRANSACTION;
    END TRY
    BEGIN CATCH
        IF @own = 1 AND XACT_STATE() <> 0 ROLLBACK TRANSACTION;
        THROW;
    END CATCH
    IF @SuppressResult = 0
        SELECT @jid AS JournalId, @jnum AS JournalNumber, @CompanyCode AS CompanyCode, @fy AS FiscalYear, @pn AS PeriodNo, @tdg AS TotalDebitGBP, @tcg AS TotalCreditGBP,
               'POSTED' AS Status, CAST(0 AS bit) AS WasDuplicate;
END
GO
