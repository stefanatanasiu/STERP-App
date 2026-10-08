/*  Finance demonstration data for both companies.  Posted journals for FY2026 periods one to
    seven so that the trial balance, the profit and loss account, the balance sheet, the budget
    variance report, the VAT return and the journal enquiry all show meaningful figures; the VAT
    transactions behind the nine box return; an October bank statement with matched and
    outstanding items for the reconciliation screen; an approved budget version to compare the
    actuals against; and the close checklist for the September and October periods.

    Everything is driven through fin.JournalStage and fin.usp_PostJournal, the single posting
    contract, rather than by writing to fin.Journal directly, so the demonstration data obeys
    exactly the same period, account, dimension, balance and currency rules as live postings.

    Re-runnable.  Each journal carries a fixed correlation id, so a repeat run is turned away by
    core.IdempotencyKey and has no second effect; every other insert is guarded by NOT EXISTS.

    Periods one to five of FY2026 are hard closed and period six is soft closed in the demo
    baseline (09-fin-period-vat.sql).  The loader opens the periods it needs, posts, and then puts
    the original status back from a saved copy, so the closed-period rules are still proved by the
    tests and the screens still show a closed year to date.
    REQ: FIN-002, FIN-003, FIN-004, FIN-005, FIN-010, FIN-013, FIN-016, FIN-017, FIN-018,
    FIN-019, FIN-020, FIN-021, FIN-022, FIN-023, FIN-029, COM-009, COM-014 */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'fin.usp_PostJournal') IS NULL
BEGIN
    PRINT 'fin.usp_PostJournal is not deployed yet - finance demonstration data skipped.';
    RETURN;
END
GO
/*  ------------------------------------------------------------------------------------------
    1.  Open the periods the loader posts into.  The original status is not snapshotted, because
        a run that fails half way through would take the snapshot with it and the next run would
        then save the opened status as though it were the baseline.  Section three puts the
        status back from the documented baseline rule instead, which makes the loader self
        healing: whatever state a failed run left behind, the next run restores it.
    ------------------------------------------------------------------------------------------ */
UPDATE fin.Period SET Status = 'OPEN'
 WHERE FiscalYear = 2026 AND PeriodNo BETWEEN 1 AND 7 AND Status <> 'OPEN';
GO
/*  ------------------------------------------------------------------------------------------
    2.  The journal templates.  Eight recurring business events a period, each one posted through
        fin.usp_PostJournal under its own correlation id.  Anything that touches a control
        account is posted from the subledger that owns it (AR, AP or the cash book CB); only the
        payroll and depreciation journals are manual finance entries, and those use accounts that
        allow a manual journal.  Amounts grow period by period and are scaled down for the
        smaller company, so the comparatives and the budget variance are not flat.
    ------------------------------------------------------------------------------------------ */
DECLARE @co varchar(10), @p tinyint, @f decimal(9,4), @pd date, @corr varchar(64), @desc varchar(200);
DECLARE @net decimal(18,2), @vat decimal(18,2), @gross decimal(18,2), @a decimal(18,2), @b decimal(18,2), @c decimal(18,2);
DECLARE @posted int = 0, @skipped int = 0, @sref varchar(40);

DECLARE co CURSOR LOCAL FAST_FORWARD FOR
    SELECT c.CompanyCode, CASE WHEN c.CompanyCode = 'STC' THEN 1.0000 ELSE 0.6000 END
      FROM core.Company c WHERE EXISTS (SELECT 1 FROM fin.Account a WHERE a.CompanyCode = c.CompanyCode);
OPEN co;
FETCH NEXT FROM co INTO @co, @f;
WHILE @@FETCH_STATUS = 0
BEGIN
    SET @p = 1;
    WHILE @p <= 7
    BEGIN
        SELECT @pd = EndDate FROM fin.Period WHERE CompanyCode = @co AND FiscalYear = 2026 AND PeriodNo = @p;

        /*  Sales invoiced in the period.  Posted from the sales ledger because the debtors
            control account and the sales accounts refuse a manual journal.  REQ: FIN-013 */
        SET @net = ROUND((120000 + 7500 * @p) * @f, 2); SET @vat = ROUND(@net * 0.20, 2); SET @gross = @net + @vat;
        SET @corr = 'DEMO-FIN-' + @co + '-2026-' + RIGHT('0' + CAST(@p AS varchar(2)), 2) + '-SALES';
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, DimProject, Narrative, VatCode, CreatedBy)
            VALUES (@corr, 1, @co, '1200', @gross, 0, 'GBP', NULL,    NULL,  'Sales invoiced in the period', NULL, 'SEED'),
                   (@corr, 2, @co, '4000', 0, @net,   'GBP', 'SALES', 'GEN', 'United Kingdom component sales', 'S', 'SEED'),
                   (@corr, 3, @co, '2040', 0, @vat,   'GBP', NULL,    NULL,  'Output tax at the standard rate', 'S', 'SEED');
            SET @sref = 'SALES-P' + RIGHT('0' + CAST(@p AS varchar(2)), 2);
            EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'FINCTRL',
                 @Description = 'Sales invoiced, period summary', @PostingDate = @pd, @JournalType = 'GL',
                 @SourceModule = 'AR', @SourceRef = @sref, @SuppressResult = 1;
            SET @posted = @posted + 1;
        END ELSE SET @skipped = @skipped + 1;

        /*  Goods purchased for resale.  REQ: FIN-013 */
        SET @net = ROUND((54000 + 3100 * @p) * @f, 2); SET @vat = ROUND(@net * 0.20, 2); SET @gross = @net + @vat;
        SET @corr = 'DEMO-FIN-' + @co + '-2026-' + RIGHT('0' + CAST(@p AS varchar(2)), 2) + '-PURCH';
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, DimProject, Narrative, VatCode, CreatedBy)
            VALUES (@corr, 1, @co, '5010', @net, 0, 'GBP', 'PROD1', 'GEN', 'Components consumed', 'S', 'SEED'),
                   (@corr, 2, @co, '1250', @vat, 0, 'GBP', NULL,    NULL,  'Input tax recoverable', 'S', 'SEED'),
                   (@corr, 3, @co, '2000', 0, @gross, 'GBP', NULL,  NULL,  'Supplier invoices for the period', NULL, 'SEED');
            SET @sref = 'PURCH-P' + RIGHT('0' + CAST(@p AS varchar(2)), 2);
            EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'FINCTRL',
                 @Description = 'Purchases invoiced, period summary', @PostingDate = @pd, @JournalType = 'GL',
                 @SourceModule = 'AP', @SourceRef = @sref, @SuppressResult = 1;
            SET @posted = @posted + 1;
        END ELSE SET @skipped = @skipped + 1;

        /*  Premises overheads, two expense lines on one supplier batch.  REQ: FIN-019 */
        SET @a = ROUND(8200 * @f, 2); SET @b = ROUND((1450 + 60 * @p) * @f, 2);
        SET @net = @a + @b; SET @vat = ROUND(@net * 0.20, 2); SET @gross = @net + @vat;
        SET @corr = 'DEMO-FIN-' + @co + '-2026-' + RIGHT('0' + CAST(@p AS varchar(2)), 2) + '-OVHD';
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, DimProject, Narrative, VatCode, CreatedBy)
            VALUES (@corr, 1, @co, '6100', @a, 0, 'GBP', 'ADMIN', 'GEN', 'Rent and business rates', 'S', 'SEED'),
                   (@corr, 2, @co, '6110', @b, 0, 'GBP', 'ADMIN', 'GEN', 'Light, heat and power', 'S', 'SEED'),
                   (@corr, 3, @co, '1250', @vat, 0, 'GBP', NULL,  NULL,  'Input tax recoverable', 'S', 'SEED'),
                   (@corr, 4, @co, '2000', 0, @gross, 'GBP', NULL, NULL, 'Overhead supplier invoices', NULL, 'SEED');
            SET @sref = 'OVHD-P' + RIGHT('0' + CAST(@p AS varchar(2)), 2);
            EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'FINCTRL',
                 @Description = 'Premises overheads, period summary', @PostingDate = @pd, @JournalType = 'GL',
                 @SourceModule = 'AP', @SourceRef = @sref, @SuppressResult = 1;
            SET @posted = @posted + 1;
        END ELSE SET @skipped = @skipped + 1;

        /*  Payroll.  A genuine manual finance journal: every account on it allows one.  REQ: FIN-002 */
        SET @a = ROUND((31500 + 220 * @p) * @f, 2); SET @b = ROUND(@a * 0.138, 2); SET @c = @a + @b;
        SET @corr = 'DEMO-FIN-' + @co + '-2026-' + RIGHT('0' + CAST(@p AS varchar(2)), 2) + '-PAYR';
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, DimProject, Narrative, VatCode, CreatedBy)
            VALUES (@corr, 1, @co, '6000', @a, 0, 'GBP', 'ADMIN', 'GEN', 'Wages and salaries', NULL, 'SEED'),
                   (@corr, 2, @co, '6010', @b, 0, 'GBP', 'ADMIN', 'GEN', 'Employer national insurance', NULL, 'SEED'),
                   (@corr, 3, @co, '2110', 0, @c, 'GBP', NULL,    NULL,  'Payroll control, net pay and deductions', NULL, 'SEED');
            SET @sref = 'PAYR-P' + RIGHT('0' + CAST(@p AS varchar(2)), 2);
            EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'FINCTRL',
                 @Description = 'Payroll journal for the period', @PostingDate = @pd, @JournalType = 'GL',
                 @SourceModule = 'FIN', @SourceRef = @sref, @SuppressResult = 1;
            SET @posted = @posted + 1;
        END ELSE SET @skipped = @skipped + 1;

        /*  Cash received from customers.  REQ: FIN-017 */
        SET @a = ROUND((108000 + 6200 * @p) * @f, 2);
        SET @corr = 'DEMO-FIN-' + @co + '-2026-' + RIGHT('0' + CAST(@p AS varchar(2)), 2) + '-RECT';
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, Narrative, BankAccountCode, CreatedBy)
            VALUES (@corr, 1, @co, '1300', @a, 0, 'GBP', 'Receipts banked in the period', 'GBP1', 'SEED'),
                   (@corr, 2, @co, '1200', 0, @a, 'GBP', 'Cash applied to customer accounts', NULL, 'SEED');
            SET @sref = 'RECT-P' + RIGHT('0' + CAST(@p AS varchar(2)), 2);
            EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'FINCTRL',
                 @Description = 'Customer receipts banked', @PostingDate = @pd, @JournalType = 'GL',
                 @SourceModule = 'AR', @SourceRef = @sref, @SuppressResult = 1;
            SET @posted = @posted + 1;
        END ELSE SET @skipped = @skipped + 1;

        /*  Payments made to suppliers.  REQ: FIN-017 */
        SET @a = ROUND((61000 + 2900 * @p) * @f, 2);
        SET @corr = 'DEMO-FIN-' + @co + '-2026-' + RIGHT('0' + CAST(@p AS varchar(2)), 2) + '-PAYM';
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, Narrative, BankAccountCode, CreatedBy)
            VALUES (@corr, 1, @co, '2000', @a, 0, 'GBP', 'Supplier payment run', NULL, 'SEED'),
                   (@corr, 2, @co, '1300', 0, @a, 'GBP', 'Payments out of the current account', 'GBP1', 'SEED');
            SET @sref = 'PAYM-P' + RIGHT('0' + CAST(@p AS varchar(2)), 2);
            EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'FINCTRL',
                 @Description = 'Supplier payment run', @PostingDate = @pd, @JournalType = 'GL',
                 @SourceModule = 'AP', @SourceRef = @sref, @SuppressResult = 1;
            SET @posted = @posted + 1;
        END ELSE SET @skipped = @skipped + 1;

        /*  Bank charges, raised by the cash book rather than by hand.  REQ: FIN-018 */
        SET @a = ROUND((145 + 5 * @p) * @f, 2);
        SET @corr = 'DEMO-FIN-' + @co + '-2026-' + RIGHT('0' + CAST(@p AS varchar(2)), 2) + '-CHRG';
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, DimProject, Narrative, BankAccountCode, CreatedBy)
            VALUES (@corr, 1, @co, '6420', @a, 0, 'GBP', 'ADMIN', 'GEN', 'Account maintenance and transaction charges', NULL, 'SEED'),
                   (@corr, 2, @co, '1300', 0, @a, 'GBP', NULL,    NULL,  'Charges taken by the bank', 'GBP1', 'SEED');
            SET @sref = 'CHRG-P' + RIGHT('0' + CAST(@p AS varchar(2)), 2);
            EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'FINCTRL',
                 @Description = 'Bank charges for the period', @PostingDate = @pd, @JournalType = 'GL',
                 @SourceModule = 'CB', @SourceRef = @sref, @SuppressResult = 1;
            SET @posted = @posted + 1;
        END ELSE SET @skipped = @skipped + 1;

        /*  Depreciation charge.  REQ: FIN-002 */
        SET @a = ROUND(4750 * @f, 2);
        SET @corr = 'DEMO-FIN-' + @co + '-2026-' + RIGHT('0' + CAST(@p AS varchar(2)), 2) + '-DEPN';
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount, CurrencyCode, DimCostCentre, DimProject, Narrative, VatCode, CreatedBy)
            VALUES (@corr, 1, @co, '6600', @a, 0, 'GBP', 'PROD1', 'GEN', 'Depreciation, plant and machinery', NULL, 'SEED'),
                   (@corr, 2, @co, '1060', 0, @a, 'GBP', NULL,    NULL,  'Accumulated depreciation, plant', NULL, 'SEED');
            SET @sref = 'DEPN-P' + RIGHT('0' + CAST(@p AS varchar(2)), 2);
            EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'FINCTRL',
                 @Description = 'Monthly depreciation charge', @PostingDate = @pd, @JournalType = 'GL',
                 @SourceModule = 'FIN', @SourceRef = @sref, @SuppressResult = 1;
            SET @posted = @posted + 1;
        END ELSE SET @skipped = @skipped + 1;

        SET @p = @p + 1;
    END
    FETCH NEXT FROM co INTO @co, @f;
END
CLOSE co; DEALLOCATE co;
PRINT 'Finance demonstration journals: ' + CAST(@posted AS varchar(10)) + ' posted, ' + CAST(@skipped AS varchar(10)) + ' already present';
GO
/*  ------------------------------------------------------------------------------------------
    3.  Put the period statuses back to the demo baseline of 09-fin-period-vat.sql: FY2026 hard
        closed to August 2026, September soft closed, October open.  The rule is repeated here
        rather than snapshotted so that the loader restores the baseline even after a failed run
        (noted in docs\tech-debt\fin.md).
    ------------------------------------------------------------------------------------------ */
UPDATE p
   SET p.Status      = CASE WHEN p.PeriodNo <= 5 THEN 'HARD' WHEN p.PeriodNo = 6 THEN 'SOFT' ELSE 'OPEN' END,
       p.SoftCloseAt = CASE WHEN p.PeriodNo <= 6 THEN DATEADD(day, 3, p.EndDate) END,
       p.HardCloseAt = CASE WHEN p.PeriodNo <= 5 THEN DATEADD(day, 10, p.EndDate) END,
       p.ClosedBy    = CASE WHEN p.PeriodNo <= 6 THEN 'FINCTRL' END
  FROM fin.Period p
 WHERE p.FiscalYear = 2026 AND p.PeriodNo BETWEEN 1 AND 7;
PRINT 'Restored the FY2026 period close baseline';
GO
/*  ------------------------------------------------------------------------------------------
    4.  The VAT transactions behind the nine box return, derived from the journals just posted so
        that the return and the ledger can never disagree.  Output tax from the sales journals,
        input tax from the purchase and overhead journals; the VAT period comes from the posting
        date rather than from the accounting period, because the tax point drives the return.
        REQ: FIN-013, FIN-016, FIN-023
    ------------------------------------------------------------------------------------------ */
INSERT fin.VatTransaction (CompanyCode, SourceModule, DocumentType, DocumentRef, TaxPointDate, PostingDate, VatPeriodKey,
        JournalId, VatCode, RatePct, Mechanism, Direction, CurrencyCode, FxRate, NetAmount, VatAmount, NetAmountGBP,
        VatAmountGBP, RecoverableVatGBP, VatBox, NetBox, PlaceOfSupply, EvidenceRef)
SELECT j.CompanyCode, j.SourceModule,
       CASE WHEN j.SourceModule = 'AR' THEN 'SALESINVOICE' ELSE 'PURCHASEINVOICE' END,
       j.JournalNumber, j.DocumentDate, j.PostingDate, vp.VatPeriodKey, j.JournalId, 'S', 20.0000, 'NORMAL',
       CASE WHEN j.SourceModule = 'AR' THEN 'OUTPUT' ELSE 'INPUT' END,
       'GBP', 1, v.Net, v.Vat, v.Net, v.Vat,
       CASE WHEN j.SourceModule = 'AR' THEN 0 ELSE v.Vat END,
       CASE WHEN j.SourceModule = 'AR' THEN 1 ELSE 4 END,
       CASE WHEN j.SourceModule = 'AR' THEN 6 ELSE 7 END,
       'UK', j.JournalNumber
  FROM fin.Journal j
 CROSS APPLY (SELECT SUM(CASE WHEN l.AccountCode IN ('4000', '5010', '6100', '6110') THEN l.DebitAmountGBP + l.CreditAmountGBP ELSE 0 END) AS Net,
                     SUM(CASE WHEN l.AccountCode IN ('2040', '1250') THEN l.DebitAmountGBP + l.CreditAmountGBP ELSE 0 END) AS Vat
                FROM fin.JournalLine l WHERE l.JournalId = j.JournalId) v
  JOIN fin.VatPeriod vp ON vp.CompanyCode = j.CompanyCode AND j.DocumentDate BETWEEN vp.StartDate AND vp.EndDate
 WHERE j.CorrelationId LIKE 'DEMO-FIN-%'
   AND j.Status = 'POSTED'
   AND j.SourceModule IN ('AR', 'AP')
   AND v.Net > 0 AND v.Vat > 0
   AND NOT EXISTS (SELECT 1 FROM fin.VatTransaction x WHERE x.CompanyCode = j.CompanyCode AND x.DocumentRef = j.JournalNumber);
PRINT 'Seeded fin.VatTransaction from the demonstration journals';
GO
/*  ------------------------------------------------------------------------------------------
    5.  The October 2026 bank statement for the sterling current account.  The receipts, payments
        and charges already in the cash book appear on it and are left unmatched for the
        reconciliation screen to match; two further lines - interest credited and a direct debit -
        are on the statement but not yet in the ledger, which is what an outstanding item looks
        like.  REQ: FIN-017, FIN-018
    ------------------------------------------------------------------------------------------ */
INSERT fin.BankStatementLine (CompanyCode, BankAccountCode, StatementRef, LineNum, EntryDate, ValueDate, Description,
        BankReference, Amount, CurrencyCode, EntryType, MatchStatus)
SELECT s.CompanyCode, 'GBP1', 'BS-2026-10', s.LineNum, s.EntryDate, s.EntryDate, s.Description,
       'BR' + s.CompanyCode + RIGHT('00' + CAST(s.LineNum AS varchar(3)), 3), s.Amount, 'GBP', s.EntryType, 'UNMATCHED'
  FROM (
    SELECT j.CompanyCode,
           ROW_NUMBER() OVER (PARTITION BY j.CompanyCode ORDER BY j.JournalNumber) AS LineNum,
           j.PostingDate AS EntryDate,
           CASE WHEN j.SourceModule = 'AR' THEN 'Credits, customer receipts'
                WHEN j.SourceModule = 'AP' THEN 'Supplier payment run'
                ELSE 'Account charges' END AS Description,
           CASE WHEN j.SourceModule = 'AR' THEN l.DebitAmountGBP ELSE -l.CreditAmountGBP END AS Amount,
           CASE WHEN j.SourceModule = 'AR' THEN 'RECEIPT' WHEN j.SourceModule = 'AP' THEN 'PAYMENT' ELSE 'CHARGE' END AS EntryType
      FROM fin.Journal j
      JOIN fin.JournalLine l ON l.JournalId = j.JournalId AND l.AccountCode = '1300'
     WHERE j.CorrelationId LIKE 'DEMO-FIN-%-2026-07-%' AND j.Status = 'POSTED'
  ) s
 WHERE NOT EXISTS (SELECT 1 FROM fin.BankStatementLine x WHERE x.CompanyCode = s.CompanyCode
                     AND x.BankAccountCode = 'GBP1' AND x.StatementRef = 'BS-2026-10' AND x.LineNum = s.LineNum);

INSERT fin.BankStatementLine (CompanyCode, BankAccountCode, StatementRef, LineNum, EntryDate, ValueDate, Description,
        BankReference, Amount, CurrencyCode, EntryType, MatchStatus)
SELECT c.CompanyCode, 'GBP1', 'BS-2026-10', o.LineNum, o.EntryDate, o.EntryDate, o.Description,
       'BR' + c.CompanyCode + RIGHT('00' + CAST(o.LineNum AS varchar(3)), 3), o.Amount, 'GBP', o.EntryType, 'UNMATCHED'
  FROM core.Company c
 CROSS JOIN (VALUES
    (90, CAST('2026-10-30' AS date), 'Credit interest for the quarter', CAST(318.44 AS decimal(18,2)), 'INTEREST'),
    (91, CAST('2026-10-31' AS date), 'Direct debit, business insurance', CAST(-1265.00 AS decimal(18,2)), 'PAYMENT')
 ) AS o (LineNum, EntryDate, Description, Amount, EntryType)
 WHERE EXISTS (SELECT 1 FROM fin.BankAccount b WHERE b.CompanyCode = c.CompanyCode AND b.BankAccountCode = 'GBP1')
   AND NOT EXISTS (SELECT 1 FROM fin.BankStatementLine x WHERE x.CompanyCode = c.CompanyCode
                     AND x.BankAccountCode = 'GBP1' AND x.StatementRef = 'BS-2026-10' AND x.LineNum = o.LineNum);

UPDATE b SET b.StatementBalance = x.Bal, b.StatementDate = '2026-10-31'
  FROM fin.BankAccount b
  JOIN (SELECT CompanyCode, SUM(Amount) AS Bal FROM fin.BankStatementLine
         WHERE BankAccountCode = 'GBP1' AND StatementRef = 'BS-2026-10' GROUP BY CompanyCode) x
    ON x.CompanyCode = b.CompanyCode
 WHERE b.BankAccountCode = 'GBP1' AND (b.StatementBalance IS NULL OR b.StatementDate <> '2026-10-31');
PRINT 'Seeded fin.BankStatementLine (October 2026 sterling statement)';
GO
/*  ------------------------------------------------------------------------------------------
    6.  The approved FY2026 budget.  Phased period by period rather than a year total divided by
        twelve, and set a few per cent away from the actuals so the variance report has something
        to report.  REQ: FIN-019, FIN-020
    ------------------------------------------------------------------------------------------ */
INSERT fin.Budget (CompanyCode, BudgetCode, FiscalYear, Name, BudgetType, Version, CurrencyCode, Status, IsBaseline,
        OwnerUser, ApprovedBy, ApprovedAt, CreatedBy)
SELECT c.CompanyCode, 'BUD26', 2026, 'Approved budget, financial year 2026 to 2027', 'BUDGET', 1, 'GBP', 'APPROVED', 1,
       'FINCTRL', 'DIRECTOR', CAST('2026-03-20T10:00:00' AS datetime2(3)), 'SEED'
  FROM core.Company c
 WHERE EXISTS (SELECT 1 FROM fin.Account a WHERE a.CompanyCode = c.CompanyCode)
   AND NOT EXISTS (SELECT 1 FROM fin.Budget b WHERE b.CompanyCode = c.CompanyCode AND b.BudgetCode = 'BUD26' AND b.FiscalYear = 2026 AND b.Version = 1);

INSERT fin.BudgetLine (CompanyCode, BudgetCode, FiscalYear, Version, PeriodNo, AccountCode, DimCostCentre, DimProject, Amount, Narrative)
SELECT b.CompanyCode, 'BUD26', 2026, 1, n.PeriodNo, a.AccountCode, a.CostCentre, 'GEN',
       ROUND(a.BaseAmount * CASE WHEN b.CompanyCode = 'STC' THEN 1.0000 ELSE 0.6000 END
             * (1 + a.Growth * n.PeriodNo), 2),
       'Approved budget phasing'
  FROM fin.Budget b
 CROSS JOIN (VALUES
    ('4000','SALES',CAST(-123000 AS decimal(18,2)),CAST(0.0600 AS decimal(9,4))),
    ('5010','PROD1',CAST(55500 AS decimal(18,2)),CAST(0.0550 AS decimal(9,4))),
    ('6000','ADMIN',CAST(31800 AS decimal(18,2)),CAST(0.0070 AS decimal(9,4))),
    ('6010','ADMIN',CAST(4390 AS decimal(18,2)),CAST(0.0070 AS decimal(9,4))),
    ('6100','ADMIN',CAST(8200 AS decimal(18,2)),CAST(0.0000 AS decimal(9,4))),
    ('6110','ADMIN',CAST(1500 AS decimal(18,2)),CAST(0.0400 AS decimal(9,4))),
    ('6420','ADMIN',CAST(150 AS decimal(18,2)),CAST(0.0300 AS decimal(9,4))),
    ('6600','PROD1',CAST(4750 AS decimal(18,2)),CAST(0.0000 AS decimal(9,4)))
 ) AS a (AccountCode, CostCentre, BaseAmount, Growth)
 CROSS JOIN (SELECT TOP (12) ROW_NUMBER() OVER (ORDER BY (SELECT 1)) AS PeriodNo FROM sys.all_objects) n
 WHERE b.BudgetCode = 'BUD26' AND b.FiscalYear = 2026 AND b.Version = 1
   AND EXISTS (SELECT 1 FROM fin.Account x WHERE x.CompanyCode = b.CompanyCode AND x.AccountCode = a.AccountCode)
   AND NOT EXISTS (SELECT 1 FROM fin.BudgetLine l WHERE l.CompanyCode = b.CompanyCode AND l.BudgetCode = 'BUD26'
                     AND l.FiscalYear = 2026 AND l.Version = 1 AND l.PeriodNo = n.PeriodNo AND l.AccountCode = a.AccountCode);
PRINT 'Seeded fin.Budget / fin.BudgetLine (BUD26)';
GO
/*  ------------------------------------------------------------------------------------------
    7.  The close checklist for the soft closed September period and the open October period, so
        that the period close screen has a worklist and the blocking steps can be demonstrated.
        REQ: FIN-010, FIN-011
    ------------------------------------------------------------------------------------------ */
INSERT fin.CloseChecklist (CompanyCode, FiscalYear, PeriodNo, StepCode, StepName, OwnerRole, IsBlocking, Status, CompletedBy, CompletedAt, Note)
SELECT c.CompanyCode, 2026, p.PeriodNo, s.StepCode, s.StepName, s.OwnerRole, s.IsBlocking,
       CASE WHEN p.PeriodNo = 6 THEN 'DONE' WHEN s.StepCode IN ('SUBLEDGER', 'BANKREC') THEN 'DONE' ELSE 'OPEN' END,
       CASE WHEN p.PeriodNo = 6 OR s.StepCode IN ('SUBLEDGER', 'BANKREC') THEN 'FINCTRL' END,
       CASE WHEN p.PeriodNo = 6 THEN CAST('2026-10-03T16:40:00' AS datetime2(3))
            WHEN s.StepCode IN ('SUBLEDGER', 'BANKREC') THEN CAST('2026-11-03T11:15:00' AS datetime2(3)) END,
       s.Note
  FROM core.Company c
 CROSS JOIN (VALUES (CAST(6 AS tinyint)), (CAST(7 AS tinyint))) AS p (PeriodNo)
 CROSS JOIN (VALUES
    ('SUBLEDGER','All subledgers posted and closed to the general ledger','FINCTRL',CAST(1 AS bit),'Debtors, creditors and stock must have no unposted batches left.'),
    ('BANKREC','Every bank account reconciled to the statement','TREASURY',CAST(1 AS bit),'Differences are investigated, never written off to suspense.'),
    ('CONTROL','Control accounts agreed to their subledger','FINCTRL',CAST(1 AS bit),'Debtors, creditors, stock, goods received not invoiced and VAT.'),
    ('ACCRUAL','Accruals and prepayments raised and reversed','FINCTRL',CAST(1 AS bit),'Last period reversals confirmed before the new ones are raised.'),
    ('FXREVAL','Foreign currency balances revalued at the period end rate','FINCTRL',CAST(0 AS bit),'Unrealised differences only; realised differences arise on settlement.'),
    ('REVIEW','Management accounts reviewed and variances explained','DIRECTOR',CAST(0 AS bit),'Variance over five per cent of budget needs a written explanation.')
 ) AS s (StepCode, StepName, OwnerRole, IsBlocking, Note)
 WHERE EXISTS (SELECT 1 FROM fin.Period x WHERE x.CompanyCode = c.CompanyCode AND x.FiscalYear = 2026 AND x.PeriodNo = p.PeriodNo)
   AND NOT EXISTS (SELECT 1 FROM fin.CloseChecklist y WHERE y.CompanyCode = c.CompanyCode AND y.FiscalYear = 2026
                     AND y.PeriodNo = p.PeriodNo AND y.StepCode = s.StepCode);
PRINT 'Seeded fin.CloseChecklist (FY2026 periods 6 and 7)';
GO
