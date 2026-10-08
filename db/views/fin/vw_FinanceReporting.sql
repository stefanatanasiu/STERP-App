/*  fin.vw_AccountBalance - posted movement per company, account, year and period with the
    class and statutory line attached.  Trial balance, profit and loss, balance sheet and the
    drill-down all derive from here, so they agree by construction.
    REQ: FIN-021, FIN-023 */
CREATE OR ALTER VIEW fin.vw_AccountBalance
AS
SELECT l.CompanyCode, l.AccountCode, a.Name AS AccountName, a.ClassCode, c.ClassType, c.NormalBalance, c.Statement,
       a.StatutoryLineCode, a.ManagementGroup, a.ControlType, a.TaxAnalysisFlag, j.FiscalYear, j.PeriodNo,
       SUM(l.DebitAmountGBP) AS DebitGBP, SUM(l.CreditAmountGBP) AS CreditGBP,
       SUM(l.DebitAmountGBP) - SUM(l.CreditAmountGBP) AS MovementGBP, COUNT_BIG(*) AS LineCount
  FROM fin.JournalLine l
  JOIN fin.Journal j ON j.JournalId = l.JournalId AND j.Status = 'POSTED'
  JOIN fin.Account a ON a.CompanyCode = l.CompanyCode AND a.AccountCode = l.AccountCode
  JOIN fin.AccountClass c ON c.ClassCode = a.ClassCode
 GROUP BY l.CompanyCode, l.AccountCode, a.Name, a.ClassCode, c.ClassType, c.NormalBalance, c.Statement,
          a.StatutoryLineCode, a.ManagementGroup, a.ControlType, a.TaxAnalysisFlag, j.FiscalYear, j.PeriodNo;
GO
/*  fin.vw_CashBook - every posted movement on a bank or cash account with its reconciliation
    state.  A subledger receipt appears once, as the bank leg of its own journal - never as a
    separately entered bank movement (FIN-013, AC-014).  REQ: FIN-013, FIN-014 */
CREATE OR ALTER VIEW fin.vw_CashBook
AS
SELECT j.CompanyCode, b.BankAccountCode, b.Name AS BankAccountName, b.CurrencyCode AS BankCurrency, l.AccountCode,
       j.JournalId, l.JournalLineId, j.JournalNumber, j.JournalType, j.SourceModule, j.SourceRef, j.PostingDate,
       j.DocumentDate, j.FiscalYear, j.PeriodNo, l.Narrative, l.CurrencyCode, l.DebitAmount, l.CreditAmount,
       l.DebitAmountGBP, l.CreditAmountGBP, l.DebitAmountGBP - l.CreditAmountGBP AS SignedGBP,
       CASE WHEN l.DebitAmountGBP > 0 THEN 'RECEIPT' ELSE 'PAYMENT' END AS MovementType, l.SubledgerType, l.SubledgerRef,
       l.ReconMatchId, CASE WHEN l.ReconMatchId IS NULL THEN 'OUTSTANDING' ELSE 'RECONCILED' END AS ReconState
  FROM fin.JournalLine l
  JOIN fin.Journal j ON j.JournalId = l.JournalId AND j.Status = 'POSTED'
  JOIN fin.BankAccount b ON b.CompanyCode = l.CompanyCode
                        AND (b.BankAccountCode = l.BankAccountCode OR (l.BankAccountCode IS NULL AND b.AccountCode = l.AccountCode));
GO
/*  fin.vw_VatBoxDetail - every posted VAT record expanded to the nine-box lines it feeds, so
    that each box total can be drilled back to its source documents.  Boxes 6 to 9 carry net
    amounts, boxes 1 to 5 VAT amounts; boxes 2, 8 and 9 are always zero (D-23).
    REQ: FIN-017, FIN-018, FIN-025 */
CREATE OR ALTER VIEW fin.vw_VatBoxDetail
AS
SELECT t.CompanyCode, t.VatPeriodKey, t.VatTxnId, t.DocumentType, t.DocumentRef, t.TaxPointDate, t.PostingDate,
       t.VatCode, t.Mechanism, t.Direction, t.IsLateEntry, t.PartyCode, t.NetAmountGBP, t.VatAmountGBP, x.BoxNo,
       x.Contribution, x.BoxKind
  FROM fin.VatTransaction t
 CROSS APPLY (VALUES
        (CASE WHEN t.Direction = 'OUTPUT' THEN t.VatBox END, t.VatAmountGBP, 'VAT'),
        (CASE WHEN t.Direction = 'INPUT'  THEN t.VatBox END, t.RecoverableVatGBP, 'VAT'),
        (t.NetBox, t.NetAmountGBP, 'NET')) AS x (BoxNo, Contribution, BoxKind)
 WHERE x.BoxNo IS NOT NULL;
GO
