/*  fin.usp_ReverseJournal - corrections use a linked full or specifically authorised partial
    reversal.  The original entry is never altered and the reversed amount can never exceed the
    unreversed original.  REQ: FIN-006, COM-006 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE fin.usp_ReverseJournal @CompanyCode varchar(10), @JournalNumber varchar(30), @UserCode varchar(20), @Reason varchar(200),
    @PostingDate date = NULL, @ReversePct decimal(9,4) = 100, @CorrelationId varchar(64) = NULL, @SeriesCode varchar(20) = 'JNLR'
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @Reason IS NULL OR LEN(LTRIM(@Reason)) = 0 THROW 50018, 'STERP-E-0018: A reason is required before a posted journal can be reversed.', 1;
    IF @ReversePct IS NULL OR @ReversePct <= 0 OR @ReversePct > 100 THROW 50018, 'STERP-E-0018: The reversal percentage must be greater than zero and not more than 100.', 1;
    DECLARE @jid bigint, @td decimal(18,2), @tdg decimal(18,2), @done decimal(18,2), @cur char(3), @fx decimal(18,8), @docdate date, @src varchar(6), @ev varchar(30);
    SELECT @jid = JournalId, @td = TotalDebit, @tdg = TotalDebitGBP, @done = ReversedAmountGBP, @cur = CurrencyCode, @fx = FxRate, @docdate = DocumentDate, @src = SourceModule, @ev = EventCode
      FROM fin.Journal WHERE CompanyCode = @CompanyCode AND JournalNumber = @JournalNumber AND Status = 'POSTED';
    IF @jid IS NULL THROW 50018, 'STERP-E-0018: That posted journal does not exist in this company.', 1;
    DECLARE @want decimal(18,2) = ROUND(@tdg * @ReversePct / 100.0, 2);
    IF @want > @tdg - @done + 0.005
        THROW 50018, 'STERP-E-0018: The requested reversal exceeds the unreversed value of the original journal.', 1;
    SET @PostingDate = ISNULL(@PostingDate, CAST(SYSDATETIME() AS date));
    SET @CorrelationId = ISNULL(@CorrelationId, 'REV-' + @CompanyCode + '-' + @JournalNumber + '-' + CONVERT(varchar(8), @PostingDate, 112) + '-' + CAST(CAST(@ReversePct AS int) AS varchar(5)));
    DECLARE @f decimal(19,9) = @ReversePct / 100.0;
    DECLARE @newid bigint, @newnum varchar(30), @dup bit = 0, @nfy smallint, @npn tinyint, @ndr decimal(18,2), @ncr decimal(18,2);
    /*  COM-015: a repeated instruction returns the original result.  The idempotency key is read
        here rather than relying on the posting proc's own duplicate result set, because
        fin.usp_PostJournal already uses INSERT ... EXEC internally for document numbering and
        SQL Server cannot nest INSERT ... EXEC (msg 8164).  The posting is therefore called with
        @SuppressResult = 1 and its outcome is read back from the ledger. */
    SELECT @newnum = ResultRef FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId AND Status = 'COMPLETED';
    IF @newnum IS NOT NULL SET @dup = 1;
    IF @dup = 0
    BEGIN
        DELETE fin.JournalStage WHERE CorrelationId = @CorrelationId;
        INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, DimProject, Narrative, VatCode, JobRef,
                                 SubledgerType, SubledgerRef, SourceRef, BankAccountCode, CreatedBy)
        SELECT @CorrelationId, l.LineNum, l.CompanyCode, l.AccountCode, ROUND(l.CreditAmount * @f, 2), ROUND(l.DebitAmount * @f, 2), l.CurrencyCode, l.DimCostCentre, l.DimProject,
               LEFT('Reversal of ' + @JournalNumber + ': ' + ISNULL(l.Narrative, ''), 200), l.VatCode, l.JobRef, l.SubledgerType, l.SubledgerRef, l.SourceRef, l.BankAccountCode, @UserCode
          FROM fin.JournalLine l WHERE l.JournalId = @jid;
        EXEC fin.usp_PostJournal @CompanyCode = @CompanyCode, @CorrelationId = @CorrelationId, @UserCode = @UserCode,
            @Description = @Reason, @PostingDate = @PostingDate, @JournalType = 'REVERSAL', @DocumentDate = @docdate, @CurrencyCode = @cur,
            @SourceModule = @src, @SourceRef = @JournalNumber, @EventCode = @ev, @SeriesCode = @SeriesCode, @FxRate = @fx, @SuppressResult = 1;
        SELECT @newnum = ResultRef FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId AND Status = 'COMPLETED';
    END
    SELECT @newid = JournalId, @nfy = FiscalYear, @npn = PeriodNo, @ndr = TotalDebitGBP, @ncr = TotalCreditGBP
      FROM fin.Journal WHERE CompanyCode = @CompanyCode AND JournalNumber = @newnum;
    IF @dup = 0
    BEGIN
        UPDATE fin.Journal SET IsReversal = 1, ReversalOfJournalId = @jid, ReversedPct = @ReversePct WHERE JournalId = @newid;
        UPDATE fin.Journal SET ReversedAmountGBP = ReversedAmountGBP + @want, ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME() WHERE JournalId = @jid;
        INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Reason, Source, CorrelationId)
        VALUES (@UserCode, @CompanyCode, 'fin.Journal', @JournalNumber, 'REVERSE', CAST(@done AS varchar(30)), CAST(@done + @want AS varchar(30)), @Reason, 'SQL', @CorrelationId);
    END
    /*  The first nine columns are the posting contract shape that every caller already binds. */
    SELECT @newid AS JournalId, @newnum AS JournalNumber, @CompanyCode AS CompanyCode, @nfy AS FiscalYear, @npn AS PeriodNo,
           @ndr AS TotalDebitGBP, @ncr AS TotalCreditGBP, 'POSTED' AS Status, @dup AS WasDuplicate,
           @JournalNumber AS ReversalOfJournalNumber, @ndr AS ReversedAmountGBP,
           @tdg - @done - CASE WHEN @dup = 1 THEN 0 ELSE @want END AS RemainingReversibleGBP, @ReversePct AS ReversePct;
END
GO
