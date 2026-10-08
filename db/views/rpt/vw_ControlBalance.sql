/*  rpt.vw_ControlBalance - the general ledger side of every control family, built from exactly the
    same definition that the RECONCTL batch uses in 1000-LOAD-GL: posted journal lines on accounts
    that carry a control type, summed in sterling as debit less credit, for one company at a cutoff
    posting date.  A view takes no parameter, so the posting date stays on the row and the caller
    filters it (WHERE CompanyCode = 'STC' AND PostingDate <= '20261031').  Every money column is
    additive at any level of the key, so a cutoff total is a plain SUM over the filtered rows.
    ReportingSign carries the RECONCTL presentation convention (debtors, inventory, work in progress
    and bank are presented positive; creditors, goods received not invoiced, VAT and accruals are
    presented positive when the ledger carries them as credits), so that the reconciliation report
    and this view describe a difference with the same sign.
    REQ: XMD-005, XMD-010, XMD-012, FIN-021, RPT-015 */
SET NOCOUNT ON;
GO
IF SCHEMA_ID(N'rpt') IS NULL EXEC (N'CREATE SCHEMA rpt');
GO
CREATE OR ALTER VIEW rpt.vw_ControlBalance
AS
SELECT l.CompanyCode,
       a.ControlType,
       k.ControlName,
       a.ControlSubledger,
       k.OwnerRole,
       k.ReportingSign,
       l.AccountCode,
       a.Name AS AccountName,
       a.ClassCode,
       j.PostingDate,
       j.FiscalYear,
       j.PeriodNo,
       CONVERT(varchar(4), j.FiscalYear) + '/' + RIGHT('0' + CONVERT(varchar(2), j.PeriodNo), 2) AS PeriodKey,
       SUM(l.DebitAmountGBP)                                    AS DebitGBP,
       SUM(l.CreditAmountGBP)                                   AS CreditGBP,
       SUM(l.DebitAmountGBP - l.CreditAmountGBP)                AS BalanceGBP,
       SUM(l.DebitAmountGBP - l.CreditAmountGBP) * k.ReportingSign AS ReportedBalanceGBP,
       COUNT_BIG(*)                                             AS LineCount,
       COUNT(DISTINCT j.JournalId)                              AS JournalCount
  FROM fin.JournalLine l
  JOIN fin.Journal  j ON j.JournalId = l.JournalId
  JOIN fin.Account  a ON a.CompanyCode = l.CompanyCode AND a.AccountCode = l.AccountCode
 CROSS APPLY (SELECT CASE a.ControlType WHEN 'AR' THEN 'Trade debtors control'
                                        WHEN 'AP' THEN 'Trade creditors control'
                                        WHEN 'INVENTORY' THEN 'Inventory control'
                                        WHEN 'WIP' THEN 'Work in progress control'
                                        WHEN 'GRNI' THEN 'Goods received not invoiced'
                                        WHEN 'VAT' THEN 'VAT control'
                                        WHEN 'BANK' THEN 'Bank and cash control'
                                        WHEN 'ACCRUAL' THEN 'Service accruals control'
                                        ELSE a.ControlType END,
                     CASE a.ControlType WHEN 'AP' THEN -1 WHEN 'GRNI' THEN -1 WHEN 'VAT' THEN -1
                                        WHEN 'ACCRUAL' THEN -1 ELSE 1 END,
                     CASE a.ControlType WHEN 'AR' THEN 'CREDMGR' WHEN 'AP' THEN 'APMGR'
                                        WHEN 'INVENTORY' THEN 'COSTACC' WHEN 'WIP' THEN 'COSTACC'
                                        WHEN 'GRNI' THEN 'PURMGR' WHEN 'VAT' THEN 'VATOWNER'
                                        WHEN 'BANK' THEN 'TREASURY' WHEN 'ACCRUAL' THEN 'FINCTRL'
                                        ELSE 'FINCTRL' END) AS k (ControlName, ReportingSign, OwnerRole)
 WHERE j.Status = 'POSTED'
   AND a.ControlType IS NOT NULL
   AND a.ControlType <> ''
 GROUP BY l.CompanyCode, a.ControlType, k.ControlName, a.ControlSubledger, k.OwnerRole, k.ReportingSign,
          l.AccountCode, a.Name, a.ClassCode, j.PostingDate, j.FiscalYear, j.PeriodNo;
GO
