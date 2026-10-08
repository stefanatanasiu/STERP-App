/*  STERP - Stefan's ERP - demonstration data consistency bridge  (db\seed\98-demo-consistency.sql)

    The demonstration dataset is built by several independent seeds and batch drivers: the
    finance demonstration (91-fin-demo) posts summarised trading journals, while the sales
    ledger, purchase ledger, stock, kit and purchasing demonstrations create their own
    subsidiary documents.  Historically the two sides were seeded independently, so the
    control accounts in the general ledger did not agree with the subsidiary ledgers and
    RECONCTL reported a difference on nearly every control family.

    This script
      1. repairs the control-account marking on the chart of accounts (INVENTORY and WIP in
         particular, which older databases were built without), and
      2. creates fin.usp_DemoBridgeControls, which measures each control family exactly the
         way bin\x86\RECONCTL.exe measures it and posts the residual through fin.JournalStage
         and fin.usp_PostJournal, so that the ledgers agree at the demonstration cutoff.

    The contra side of every bridge posting is account 3400 "Opening balance reserve, cut over
    differences" - the account the conversion suite uses for the same purpose - so no profit
    and loss account and no other control family is disturbed.

    The procedure is re-runnable and self-levelling: it only ever posts the difference that is
    still outstanding, so running it again after the demonstration batch drivers (see
    build\demo-data.ps1) simply tops the bridge up and a second run posts nothing at all.
*/
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'fin.usp_PostJournal') IS NULL
    PRINT 'fin.usp_PostJournal is not deployed yet - demonstration consistency bridge skipped.';
GO
/*  1. Control-account marking.  The chart of accounts seed carries these values, but a
    database that was built before the stock accounts were marked would leave RECONCTL with
    no control account for INVENTORY or WIP and therefore nothing to reconcile against. */
UPDATE fin.Account
   SET ControlType = 'INVENTORY', ControlSubledger = ISNULL(ControlSubledger, 'INV'),
       AllowManualJournal = 0, ModifiedBy = 'SEED', ModifiedAt = SYSUTCDATETIME()
 WHERE AccountCode IN ('1100', '1120', '1130', '1140') AND ISNULL(ControlType, '') <> 'INVENTORY';
UPDATE fin.Account
   SET ControlType = 'WIP', ControlSubledger = ISNULL(ControlSubledger, 'WIP'),
       AllowManualJournal = 0, ModifiedBy = 'SEED', ModifiedAt = SYSUTCDATETIME()
 WHERE AccountCode = '1110' AND ISNULL(ControlType, '') <> 'WIP';
GO
/*  2. The bridge.  @AsAt defaults to the end of the earliest open period, which is the
    cutoff the demonstration jobs use (2026-10-31 for the baseline dataset). */
CREATE OR ALTER PROCEDURE fin.usp_DemoBridgeControls
    @CompanyCode varchar(10),
    @AsAt        date         = NULL,
    @UserCode    varchar(20)  = 'FINCTRL',
    @Tolerance   decimal(18,2) = 0.01,
    @Report      bit          = 1
AS
BEGIN
    SET NOCOUNT ON;
    IF NOT EXISTS (SELECT 1 FROM core.Company WHERE CompanyCode = @CompanyCode)
    BEGIN
        PRINT CONCAT('STERP-W-DEMO company ', @CompanyCode, ' is not defined - bridge skipped.');
        RETURN;
    END
    IF @AsAt IS NULL
        SELECT TOP (1) @AsAt = EndDate FROM fin.Period
         WHERE CompanyCode = @CompanyCode AND Status = 'OPEN' ORDER BY StartDate;
    IF @AsAt IS NULL
    BEGIN
        PRINT CONCAT('STERP-W-DEMO company ', @CompanyCode, ' has no open period - bridge skipped.');
        RETURN;
    END
    /*  Post on the cutoff itself when its period is open, otherwise on the last day of the
        latest open period before the cutoff. */
    DECLARE @pd date;
    SELECT TOP (1) @pd = CASE WHEN @AsAt BETWEEN StartDate AND EndDate THEN @AsAt ELSE EndDate END
      FROM fin.Period
     WHERE CompanyCode = @CompanyCode AND Status = 'OPEN' AND StartDate <= @AsAt
     ORDER BY StartDate DESC;
    IF @pd IS NULL
    BEGIN
        PRINT CONCAT('STERP-W-DEMO company ', @CompanyCode, ' has no open period on or before ',
                     CONVERT(char(10), @AsAt, 120), ' - bridge skipped.');
        RETURN;
    END

    /*  The control families, their sign convention and the control account that carries the
        residual, exactly as RECONCTL reads them (liabilities are shown as a positive
        obligation, so the general ledger side is multiplied by -1). */
    DECLARE @fam TABLE (Seq int, ControlType varchar(12), Sgn smallint, Acct varchar(20),
                        Proj varchar(20) NULL, SrcModule varchar(6), Descr varchar(60),
                        Sub decimal(18,2) NULL, GL decimal(18,2) NULL);
    INSERT @fam (Seq, ControlType, Sgn, Acct, Proj, SrcModule, Descr) VALUES
        (1, 'AR',        1, '1200', NULL,  'AR',  'Trade debtors'),
        (2, 'AP',       -1, '2000', NULL,  'AP',  'Trade creditors'),
        (3, 'INVENTORY', 1, '1100', NULL,  'INV', 'Stock'),
        (4, 'WIP',       1, '1110', 'GEN', 'KIT', 'Work in progress'),
        (5, 'GRNI',     -1, '2010', NULL,  'PO',  'Goods received not invoiced'),
        (6, 'VAT',      -1, '2050', NULL,  'VAT', 'VAT control'),
        (7, 'ACCRUAL',  -1, '2020', NULL,  'PO',  'Service accruals');

    /*  Subsidiary ledger totals at the cutoff - the same expressions RECONCTL issues. */
    UPDATE f
       SET Sub = CASE f.ControlType
            WHEN 'AR' THEN ISNULL((SELECT SUM(t.Resid) FROM (
                                     SELECT o.OriginalAmountGBP
                                        - ISNULL((SELECT SUM(a.AppliedAmountGBP + a.DiscountAmountGBP + a.WriteOffAmountGBP)
                                                    FROM ar.Allocation a
                                                   WHERE a.TargetOpenItemId = o.OpenItemId AND a.Status = 'ACTIVE'
                                                     AND a.EffectiveDate <= @AsAt), 0)
                                        + ISNULL((SELECT SUM(b.AppliedAmountGBP)
                                                    FROM ar.Allocation b
                                                   WHERE b.SourceOpenItemId = o.OpenItemId AND b.Status = 'ACTIVE'
                                                     AND b.EffectiveDate <= @AsAt), 0) AS Resid
                                       FROM ar.OpenItem o
                                      WHERE o.CompanyCode = @CompanyCode AND o.PostingDate <= @AsAt
                                        AND o.SettlementStatus <> 'REVERSED') t), 0)
            WHEN 'AP' THEN ISNULL((SELECT SUM(o.OriginalAmountGBP) FROM ap.OpenItem o
                                    WHERE o.CompanyCode = @CompanyCode AND o.PostingDate <= @AsAt
                                      AND o.SettlementStatus <> 'REVERSED'), 0)
            WHEN 'INVENTORY' THEN ISNULL((SELECT SUM(ic.CarryingValueGBP - ISNULL(ic.AllowanceGBP, 0))
                                            FROM cst.ItemCost ic
                                           WHERE ic.CompanyCode = @CompanyCode), 0)
            WHEN 'WIP' THEN ISNULL((SELECT SUM(k.WipValueGBP) FROM kit.ExecutionKit k
                                     WHERE k.CompanyCode = @CompanyCode
                                       AND k.Status NOT IN ('DRAFT','FINCLOSED','CANCELLED')), 0)
            WHEN 'GRNI' THEN ISNULL((SELECT SUM(r.GrniValueGBP) FROM po.Receipt r
                                      WHERE r.CompanyCode = @CompanyCode AND r.Status = 'CONFIRMED'
                                        AND r.PostingDate <= @AsAt), 0)
                           - ISNULL((SELECT SUM(m.GrniClearedGBP) FROM ap.InvoiceMatch m
                                       JOIN ap.PurchaseInvoice i ON i.PurchaseInvoiceId = m.PurchaseInvoiceId
                                      WHERE i.CompanyCode = @CompanyCode AND m.Status <> 'REVERSED'
                                        AND i.PostingDate <= @AsAt), 0)
            WHEN 'VAT' THEN ISNULL((SELECT SUM(CASE WHEN v.Direction = 'OUTPUT' THEN v.VatAmountGBP
                                                    ELSE -ISNULL(v.RecoverableVatGBP, v.VatAmountGBP) END)
                                      FROM fin.VatTransaction v
                                     WHERE v.CompanyCode = @CompanyCode AND v.PostingDate <= @AsAt), 0)
            WHEN 'ACCRUAL' THEN ISNULL((SELECT SUM(ISNULL(s.AcceptedValueGBP,0) - ISNULL(s.MatchedValueGBP,0))
                                          FROM po.ServiceAcceptance s WHERE s.CompanyCode = @CompanyCode), 0)
           END
      FROM @fam f;

    /*  General ledger side of the same families. */
    UPDATE f SET GL = g.Bal
      FROM @fam f
      JOIN (SELECT a.ControlType, SUM(l.DebitAmountGBP - l.CreditAmountGBP) AS Bal
              FROM fin.JournalLine l
              JOIN fin.Journal j ON j.JournalId = l.JournalId
              JOIN fin.Account a ON a.CompanyCode = l.CompanyCode AND a.AccountCode = l.AccountCode
             WHERE l.CompanyCode = @CompanyCode AND j.Status = 'POSTED' AND j.PostingDate <= @AsAt
               AND ISNULL(a.ControlType, '') <> ''
             GROUP BY a.ControlType) g ON g.ControlType = f.ControlType;

    DECLARE @seq int = 0, @ct varchar(12), @acct varchar(20), @proj varchar(20), @src varchar(6),
            @descr varchar(60), @delta decimal(18,2), @corr varchar(64), @note varchar(200),
            @posted int = 0, @stamp varchar(20) = FORMAT(SYSUTCDATETIME(), 'yyyyMMddHHmmssfff');

    DECLARE fam_cur CURSOR LOCAL FAST_FORWARD FOR
        SELECT Seq, ControlType, Acct, Proj, SrcModule, Descr,
               ROUND(Sgn * ISNULL(Sub, 0) - ISNULL(GL, 0), 2)
          FROM @fam ORDER BY Seq;
    OPEN fam_cur;
    FETCH NEXT FROM fam_cur INTO @seq, @ct, @acct, @proj, @src, @descr, @delta;
    WHILE @@FETCH_STATUS = 0
    BEGIN
        IF ABS(@delta) > @Tolerance
           AND EXISTS (SELECT 1 FROM fin.Account a
                        WHERE a.CompanyCode = @CompanyCode AND a.AccountCode = @acct
                          AND a.Status = 'ACTIVE' AND a.AccountKind = 'POSTING' AND a.ValidFrom <= @pd)
        BEGIN
            SET @corr = CONCAT('DEMOBRG-', @CompanyCode, '-', @ct, '-', FORMAT(@AsAt, 'yyyyMMdd'), '-', @stamp);
            DECLARE @sref varchar(40) = CONCAT('DEMOBRG-', @ct, '-', FORMAT(@AsAt, 'yyyyMMdd'));
            SET @note = CONCAT(@descr, ' control brought into line with the subsidiary ledger for the demonstration dataset');
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount,
                                     CurrencyCode, DimCostCentre, DimProject, Narrative, SourceRef, CreatedBy)
            VALUES (@corr, 1, @CompanyCode, @acct,
                    CASE WHEN @delta > 0 THEN @delta ELSE 0 END, CASE WHEN @delta < 0 THEN -@delta ELSE 0 END,
                    'GBP', NULL, @proj, @note, @sref, 'SEED'),
                   (@corr, 2, @CompanyCode, '3400',
                    CASE WHEN @delta < 0 THEN -@delta ELSE 0 END, CASE WHEN @delta > 0 THEN @delta ELSE 0 END,
                    'GBP', NULL, NULL, 'Demonstration data bridge, cut over difference', @sref, 'SEED');
            EXEC fin.usp_PostJournal @CompanyCode = @CompanyCode, @CorrelationId = @corr, @UserCode = @UserCode,
                 @Description = @note, @PostingDate = @pd, @JournalType = 'GL', @DocumentDate = @pd,
                 @SourceModule = @src, @SourceRef = @sref, @SuppressResult = 1;
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            SET @posted = @posted + 1;
            IF @Report = 1
                PRINT CONCAT('STERP-I-DEMO ', @CompanyCode, ' ', @ct, ' bridged by ',
                             CONVERT(varchar(24), @delta), ' on ', CONVERT(char(10), @pd, 120));
        END
        FETCH NEXT FROM fam_cur INTO @seq, @ct, @acct, @proj, @src, @descr, @delta;
    END
    CLOSE fam_cur;
    DEALLOCATE fam_cur;

    /*  Bank and cash.  The subsidiary side of this family is the latest bank reconciliation
        per bank account, so the bridge is a demonstration statement rather than a journal. */
    DECLARE @glBank decimal(18,2) = ISNULL((SELECT SUM(l.DebitAmountGBP - l.CreditAmountGBP)
                                              FROM fin.JournalLine l
                                              JOIN fin.Journal j ON j.JournalId = l.JournalId
                                              JOIN fin.Account a ON a.CompanyCode = l.CompanyCode AND a.AccountCode = l.AccountCode
                                             WHERE l.CompanyCode = @CompanyCode AND j.Status = 'POSTED'
                                               AND j.PostingDate <= @AsAt AND a.ControlType = 'BANK'), 0);
    DECLARE @subBank decimal(18,2) = ISNULL((SELECT SUM(v.Bal) FROM
                                             (SELECT (SELECT TOP (1) r.ClosingStatementBalance - ISNULL(r.OutstandingPayments,0)
                                                             + ISNULL(r.OutstandingReceipts,0)
                                                        FROM fin.BankRecon r
                                                       WHERE r.CompanyCode = b.CompanyCode AND r.BankAccountCode = b.BankAccountCode
                                                         AND r.ToDate <= @AsAt ORDER BY r.ToDate DESC) AS Bal
                                                FROM fin.BankAccount b WHERE b.CompanyCode = @CompanyCode) v), 0);
    IF ABS(@glBank - @subBank) > @Tolerance
       AND EXISTS (SELECT 1 FROM fin.BankAccount WHERE CompanyCode = @CompanyCode)
    BEGIN
        DECLARE @from date = DATEADD(day, 1, EOMONTH(@AsAt, -1));
        DECLARE @mapped decimal(18,2) =
            ISNULL((SELECT SUM(x.Bal) FROM fin.BankAccount b
                     CROSS APPLY (SELECT ISNULL((SELECT SUM(l.DebitAmountGBP - l.CreditAmountGBP)
                                                   FROM fin.JournalLine l
                                                   JOIN fin.Journal j ON j.JournalId = l.JournalId
                                                  WHERE l.CompanyCode = b.CompanyCode AND l.AccountCode = b.AccountCode
                                                    AND j.Status = 'POSTED' AND j.PostingDate <= @AsAt), 0) AS Bal) x
                     WHERE b.CompanyCode = @CompanyCode), 0);
        /*  A demonstration statement already written for this cutoff is restated, never left
            behind: the drivers and the acceptance suite post more cash after it was built, and
            a stale statement would report those postings as an unexplained bank difference for
            ever.  Only the bridge's own DEMO- statements are touched; a real reconciliation of
            the same cutoff is left exactly as the treasury team made it. */
        UPDATE r
           SET r.ClosingStatementBalance = x.Bal + CASE WHEN r.BankAccountCode = f.FirstCode THEN @glBank - @mapped ELSE 0 END,
               r.BookBalance            = x.Bal + CASE WHEN r.BankAccountCode = f.FirstCode THEN @glBank - @mapped ELSE 0 END,
               r.OutstandingReceipts = 0, r.OutstandingPayments = 0, r.UnexplainedDifference = 0,
               r.Status = 'COMPLETE'
          FROM fin.BankRecon r
          JOIN fin.BankAccount b ON b.CompanyCode = r.CompanyCode AND b.BankAccountCode = r.BankAccountCode
         CROSS APPLY (SELECT ISNULL((SELECT SUM(l.DebitAmountGBP - l.CreditAmountGBP)
                                       FROM fin.JournalLine l
                                       JOIN fin.Journal j ON j.JournalId = l.JournalId
                                      WHERE l.CompanyCode = b.CompanyCode AND l.AccountCode = b.AccountCode
                                        AND j.Status = 'POSTED' AND j.PostingDate <= @AsAt), 0) AS Bal) x
         CROSS JOIN (SELECT MIN(BankAccountCode) AS FirstCode FROM fin.BankAccount WHERE CompanyCode = @CompanyCode) f
         WHERE r.CompanyCode = @CompanyCode AND r.ToDate = @AsAt AND r.StatementRef LIKE 'DEMO-%';
        SET @posted = @posted + @@ROWCOUNT;
        /*  One demonstration statement per bank account, each carrying its own general ledger
            balance; any balance on a cash account without a bank account record is carried by
            the first account so that the family proves in total. */
        INSERT fin.BankRecon (CompanyCode, BankAccountCode, StatementRef, FromDate, ToDate, CurrencyCode,
                              OpeningStatementBalance, ClosingStatementBalance, BookBalance,
                              OutstandingReceipts, OutstandingPayments, UnexplainedDifference, Status, CreatedBy)
        SELECT b.CompanyCode, b.BankAccountCode,
               CONCAT('DEMO-', FORMAT(@AsAt, 'yyyyMMdd'), '-', b.BankAccountCode), @from, @AsAt, b.CurrencyCode,
               0, x.Bal + CASE WHEN b.BankAccountCode = f.FirstCode THEN @glBank - @mapped ELSE 0 END,
               x.Bal + CASE WHEN b.BankAccountCode = f.FirstCode THEN @glBank - @mapped ELSE 0 END,
               0, 0, 0, 'COMPLETE', 'SEED'
          FROM fin.BankAccount b
         CROSS APPLY (SELECT ISNULL((SELECT SUM(l.DebitAmountGBP - l.CreditAmountGBP)
                                       FROM fin.JournalLine l
                                       JOIN fin.Journal j ON j.JournalId = l.JournalId
                                      WHERE l.CompanyCode = b.CompanyCode AND l.AccountCode = b.AccountCode
                                        AND j.Status = 'POSTED' AND j.PostingDate <= @AsAt), 0) AS Bal) x
         CROSS JOIN (SELECT MIN(BankAccountCode) AS FirstCode FROM fin.BankAccount WHERE CompanyCode = @CompanyCode) f
         WHERE b.CompanyCode = @CompanyCode
           AND NOT EXISTS (SELECT 1 FROM fin.BankRecon r WHERE r.CompanyCode = b.CompanyCode
                             AND r.BankAccountCode = b.BankAccountCode AND r.ToDate = @AsAt);
        SET @posted = @posted + @@ROWCOUNT;
        IF @Report = 1
            PRINT CONCAT('STERP-I-DEMO ', @CompanyCode, ' BANK bridged by ',
                         CONVERT(varchar(24), @glBank - @subBank), ' as a demonstration statement');
    END

    IF @Report = 1 AND @posted = 0
        PRINT CONCAT('STERP-I-DEMO ', @CompanyCode, ' control accounts already agree at ',
                     CONVERT(char(10), @AsAt, 120), ' - nothing to bridge.');
END
GO
/*  3. Level the demonstration dataset for every company that has finance demonstration
    postings.  Running the batch demonstration drivers afterwards changes the subsidiary
    ledgers, so build\demo-data.ps1 calls this procedure again once the drivers have run.
    The job streams reconcile at the clock date as well as at the demonstration cutoff, so
    level at both, EARLIEST FIRST: a bridge journal dated at the cutoff alone leaves an
    as-at-today reconciliation (NIGHTLY AGEDDEBT, MONTHEND CSTINVAL) reporting the whole
    bridge as a difference, and a journal posted at today moves the cutoff balance too.
    The procedure is self-levelling, so the later pass posts only the residual. */
DECLARE @co varchar(10);
DECLARE co_cur CURSOR LOCAL FAST_FORWARD FOR
    SELECT c.CompanyCode FROM core.Company c
     WHERE EXISTS (SELECT 1 FROM fin.Journal j WHERE j.CompanyCode = c.CompanyCode AND j.Status = 'POSTED')
     ORDER BY c.CompanyCode;
OPEN co_cur;
FETCH NEXT FROM co_cur INTO @co;
WHILE @@FETCH_STATUS = 0
BEGIN
    DECLARE @today date = CAST(SYSDATETIME() AS date);
    IF EXISTS (SELECT 1 FROM fin.Period WHERE CompanyCode = @co AND Status = 'OPEN'
                 AND @today BETWEEN StartDate AND EndDate)
        EXEC fin.usp_DemoBridgeControls @CompanyCode = @co, @AsAt = @today;
    EXEC fin.usp_DemoBridgeControls @CompanyCode = @co, @AsAt = NULL;
    FETCH NEXT FROM co_cur INTO @co;
END
CLOSE co_cur;
DEALLOCATE co_cur;
GO

/*  4. COM-022 - every open exception names the person who owns it.  Older demonstration rows
    (and any raised before core.usp_RaiseException learned to resolve a role to its holder)
    carry a responsible role but no owner, so the worklist has nobody to chase.  Name the
    active holder of the role; re-running this changes nothing once every row is owned. */
UPDATE e
   SET OwnerUser = u.UserCode
  FROM core.ExceptionItem e
 CROSS APPLY (SELECT TOP (1) a.UserCode
                FROM core.UserRole r JOIN core.AppUser a ON a.UserCode = r.UserCode
               WHERE r.RoleCode = e.OwnerRole AND a.IsActive = 1
                 AND (r.CompanyCode = e.CompanyCode OR r.CompanyCode = '*')
                 AND (r.ValidTo IS NULL OR r.ValidTo >= CAST(SYSDATETIME() AS date))
               ORDER BY CASE WHEN r.CompanyCode = e.CompanyCode THEN 0 ELSE 1 END, a.UserCode) u
 WHERE e.Status NOT IN ('RESOLVED', 'CLOSED', 'CANCELLED')
   AND ISNULL(e.OwnerUser, '') = '' AND ISNULL(e.OwnerRole, '') <> '';
GO
