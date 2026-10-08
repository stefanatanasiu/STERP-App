/*  fin.usp_JournalEnquiry - journal headers with their status, approval and reversal links, for
    the journal enquiry screen and the approval queue.  Posted money is never updated, so a
    correction always appears as a second, linked entry (FIN-006, COM-006).
    REQ: FIN-004, FIN-006, FIN-023 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE fin.usp_JournalEnquiry @CompanyCode varchar(10), @JournalNumber varchar(30) = NULL, @Status varchar(12) = NULL,
    @FromDate date = NULL, @ToDate date = NULL, @AccountCode varchar(12) = NULL, @SourceModule varchar(6) = NULL, @MaxRows int = 300
AS
BEGIN
    SET NOCOUNT ON;
    SELECT TOP (@MaxRows) j.JournalId, j.JournalNumber, j.JournalType, j.Description, j.SourceModule, j.SourceRef, j.EventCode,
           j.DocumentDate, j.PostingDate, j.FiscalYear, j.PeriodNo, j.CurrencyCode, j.FxRate, j.TotalDebitGBP, j.TotalCreditGBP,
           j.Status, j.IsReversal, j.ReversedAmountGBP, j.TotalDebitGBP - j.ReversedAmountGBP AS ReversibleGBP,
           o.JournalNumber AS ReversalOfNumber, j.RecurringCode, j.OccurrenceNo, j.PreparedBy, j.ApprovedBy, j.ApprovedAt,
           j.PostedBy, j.PostedAt, j.EvidenceRef, j.CorrelationId
      FROM fin.Journal j
      LEFT JOIN fin.Journal o ON o.JournalId = j.ReversalOfJournalId
     WHERE j.CompanyCode = @CompanyCode
       AND (@JournalNumber IS NULL OR j.JournalNumber = @JournalNumber)
       AND (@Status IS NULL OR j.Status = @Status)
       AND (@FromDate IS NULL OR j.PostingDate >= @FromDate)
       AND (@ToDate IS NULL OR j.PostingDate <= @ToDate)
       AND (@SourceModule IS NULL OR j.SourceModule = @SourceModule)
       AND (@AccountCode IS NULL OR EXISTS (SELECT 1 FROM fin.JournalLine l WHERE l.JournalId = j.JournalId AND l.AccountCode = @AccountCode))
     ORDER BY j.PostingDate DESC, j.JournalId DESC;
END
GO
/*  fin.usp_CloseStatus - the period close checklist with the blocking items identified.  An
    unresolved material difference blocks an unqualified hard close; the controller may accept
    it explicitly, which is recorded as evidence rather than silently cleared (FIN-011, AC-013).
    REQ: FIN-010, FIN-011 */
CREATE OR ALTER PROCEDURE fin.usp_CloseStatus @CompanyCode varchar(10), @FiscalYear smallint, @PeriodNo tinyint
AS
BEGIN
    SET NOCOUNT ON;
    SELECT p.CompanyCode, p.FiscalYear, p.PeriodNo, p.Name AS PeriodName, p.StartDate, p.EndDate, p.Status AS PeriodStatus,
           p.SoftCloseAt, p.HardCloseAt, p.ClosedBy, p.ReopenedBy, p.ReopenReason,
           ISNULL(s.StepCount, 0) AS StepCount, ISNULL(s.DoneCount, 0) AS DoneCount, ISNULL(s.BlockingOpen, 0) AS BlockingOpen,
           ISNULL(s.MaterialDiff, 0) AS UnresolvedDifferenceGBP,
           CASE WHEN ISNULL(s.BlockingOpen, 0) = 0 THEN 'READY' ELSE 'BLOCKED' END AS CloseReadiness
      FROM fin.Period p
      OUTER APPLY (SELECT COUNT(*) AS StepCount,
                          SUM(CASE WHEN c.Status IN ('DONE', 'ACCEPTED') THEN 1 ELSE 0 END) AS DoneCount,
                          SUM(CASE WHEN c.IsBlocking = 1 AND c.Status NOT IN ('DONE', 'ACCEPTED') THEN 1 ELSE 0 END) AS BlockingOpen,
                          SUM(CASE WHEN c.Status NOT IN ('DONE', 'ACCEPTED') THEN ISNULL(c.DifferenceGBP, 0) ELSE 0 END) AS MaterialDiff
                     FROM fin.CloseChecklist c
                    WHERE c.CompanyCode = p.CompanyCode AND c.FiscalYear = p.FiscalYear AND c.PeriodNo = p.PeriodNo) s
     WHERE p.CompanyCode = @CompanyCode AND p.FiscalYear = @FiscalYear AND p.PeriodNo = @PeriodNo;

    SELECT c.ChecklistId, c.StepCode, c.StepName, c.OwnerRole, c.OwnerUser, c.Status, c.IsBlocking, c.DifferenceGBP,
           c.EvidenceRef, c.CompletedBy, c.CompletedAt, c.ReviewedBy, c.ReviewedAt, c.Note
      FROM fin.CloseChecklist c
     WHERE c.CompanyCode = @CompanyCode AND c.FiscalYear = @FiscalYear AND c.PeriodNo = @PeriodNo
     ORDER BY c.IsBlocking DESC, c.StepCode;
END
GO
/*  fin.usp_BankOutstanding - the statement-to-book bridge for a bank account: book balance,
    outstanding receipts and payments and the matched position.  REQ: FIN-013, FIN-014 */
CREATE OR ALTER PROCEDURE fin.usp_BankOutstanding @CompanyCode varchar(10), @BankAccountCode varchar(20), @AsOfDate date
AS
BEGIN
    SET NOCOUNT ON;
    SELECT b.CompanyCode, b.BankAccountCode, b.Name, b.CurrencyCode, b.AccountCode, b.StatementBalance, b.StatementDate, b.LastReconciledDate,
           ISNULL(bk.BookGBP, 0) AS BookBalanceGBP, ISNULL(bk.OutReceipts, 0) AS OutstandingReceipts, ISNULL(bk.OutPayments, 0) AS OutstandingPayments,
           ISNULL(st.UnmatchedStatement, 0) AS UnmatchedStatementValue, ISNULL(st.UnmatchedCount, 0) AS UnmatchedStatementCount,
           ISNULL(b.StatementBalance, 0) + ISNULL(bk.OutReceipts, 0) - ISNULL(bk.OutPayments, 0) AS BridgedBookBalance
      FROM fin.BankAccount b
      OUTER APPLY (SELECT SUM(v.SignedGBP) AS BookGBP,
                          SUM(CASE WHEN v.ReconMatchId IS NULL AND v.SignedGBP > 0 THEN v.SignedGBP ELSE 0 END) AS OutReceipts,
                          SUM(CASE WHEN v.ReconMatchId IS NULL AND v.SignedGBP < 0 THEN -v.SignedGBP ELSE 0 END) AS OutPayments
                     FROM fin.vw_CashBook v
                    WHERE v.CompanyCode = b.CompanyCode AND v.BankAccountCode = b.BankAccountCode AND v.PostingDate <= @AsOfDate) bk
      OUTER APPLY (SELECT SUM(s.Amount - s.MatchedAmount) AS UnmatchedStatement, COUNT(*) AS UnmatchedCount
                     FROM fin.BankStatementLine s
                    WHERE s.CompanyCode = b.CompanyCode AND s.BankAccountCode = b.BankAccountCode
                      AND s.EntryDate <= @AsOfDate AND s.MatchStatus IN ('UNMATCHED', 'PARTIAL')) st
     WHERE b.CompanyCode = @CompanyCode AND (@BankAccountCode IS NULL OR b.BankAccountCode = @BankAccountCode);
END
GO
