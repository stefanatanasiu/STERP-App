/*  Creditors module seed: the purchase-ledger business events that the shared event map
    (10-fin-eventmap.sql, left untouched) does not already carry, plus the parameters the
    register and the matching engine read.  Everything AP posts goes through fin.JournalStage
    and fin.usp_PostJournal under one of these codes, so a supplier payment is never a
    free-hand journal.  Allocation itself has no event: applying a payment or a credit to an
    open item moves no value out of the creditors control and must have no second cash effect
    (AP-014); only its realised exchange difference posts, under the AP.FXGAIN and AP.FXLOSS
    codes the shared seed already carries.
    REQ: AP-010, AP-014, AP-017, AP-018, AP-021, AP-022, AP-025, AP-029, AP-030,
         D-08, D-24, D-25 */
SET NOCOUNT ON;
GO
DECLARE @e TABLE (EventCode varchar(30), Module varchar(6), EventName varchar(100));
INSERT @e VALUES
 ('AP.ADVANCE','AP','Advance or deposit paid to a supplier'),
 ('AP.ADVAPPLY','AP','Supplier advance applied to an invoice'),
 ('AP.ADVREFUND','AP','Unapplied supplier advance refunded to us'),
 ('AP.SETOFF','AP','Set-off of a purchase balance against a sales balance'),
 ('AP.LATEPAY','AP','Statutory late payment interest and compensation'),
 ('AP.REVCHARGE','AP','Reverse charge self-accounted on an overseas service'),
 ('AP.VATREPAY','AP','Input VAT repaid under the six month unpaid rule'),
 ('AP.VATRECLAIM','AP','Input VAT reclaimed once the debt is paid'),
 ('AP.GRNICLEAR','AP','Residual goods received not invoiced reclassified at period end'),
 ('AP.PAYRETURN','AP','Payment returned unpaid by the bank');
DECLARE @l TABLE (EventCode varchar(30), LegCode varchar(12), LegDescription varchar(100), DrCr char(2), AccountCode varchar(12),
                  CC varchar(20), PROJ varchar(20), IsConditional bit, Cond varchar(100), SortOrder int);
INSERT @l VALUES
 ('AP.ADVANCE','DEPOSIT','Deposits paid to suppliers','DR','1270',NULL,NULL,0,NULL,1),
 ('AP.ADVANCE','BANK','Bank current account','CR','1300',NULL,NULL,1,'Bank account the advance is paid from',2),
 ('AP.ADVAPPLY','CREDITOR','Trade creditors control','DR','2000',NULL,NULL,0,NULL,1),
 ('AP.ADVAPPLY','DEPOSIT','Deposits paid to suppliers, advance consumed','CR','1270',NULL,NULL,0,NULL,2),
 ('AP.ADVREFUND','BANK','Bank current account','DR','1300',NULL,NULL,1,'Bank account the refund is received into',1),
 ('AP.ADVREFUND','DEPOSIT','Deposits paid to suppliers','CR','1270',NULL,NULL,0,NULL,2),
 ('AP.SETOFF','CREDITOR','Trade creditors control','DR','2000',NULL,NULL,0,NULL,1),
 ('AP.SETOFF','DEBTOR','Trade debtors control, same counterparty','CR','1200',NULL,NULL,0,NULL,2),
 ('AP.LATEPAY','INTEREST','Interest payable and similar charges','DR','7000','ADMIN',NULL,0,NULL,1),
 ('AP.LATEPAY','CREDITOR','Trade creditors control','CR','2000',NULL,NULL,0,NULL,2),
 ('AP.REVCHARGE','VATIN','Input VAT self-accounted, Box 4','DR','1250',NULL,NULL,0,NULL,1),
 ('AP.REVCHARGE','VATOUT','Output VAT self-accounted, Box 1','CR','2040',NULL,NULL,0,NULL,2),
 ('AP.VATREPAY','VATCTL','VAT control, input tax repaid','DR','2050',NULL,NULL,0,NULL,1),
 ('AP.VATREPAY','VATIN','VAT recoverable, claim reversed','CR','1250',NULL,NULL,0,NULL,2),
 ('AP.VATRECLAIM','VATIN','VAT recoverable, claimed again on payment','DR','1250',NULL,NULL,0,NULL,1),
 ('AP.VATRECLAIM','VATCTL','VAT control, return in progress','CR','2050',NULL,NULL,0,NULL,2),
 ('AP.GRNICLEAR','GRNI','Goods received not invoiced, residual cleared','DR','2010',NULL,NULL,0,NULL,1),
 ('AP.GRNICLEAR','ACCRUAL','Accruals, reclassified at period end','CR','2020',NULL,NULL,0,NULL,2),
 ('AP.PAYRETURN','BANK','Bank current account, payment returned','DR','1300',NULL,NULL,0,NULL,1),
 ('AP.PAYRETURN','CREDITOR','Trade creditors control, liability reinstated','CR','2000',NULL,NULL,0,NULL,2);
INSERT fin.EventAccountMap (CompanyCode, EventCode, LegCode, Module, EventName, LegDescription, DrCr, AccountCode,
                            DimCostCentre, DimProject, IsConditional, Condition, SortOrder, IsActive, CreatedBy)
SELECT c.CompanyCode, l.EventCode, l.LegCode, e.Module, e.EventName, l.LegDescription, l.DrCr, l.AccountCode,
       l.CC, l.PROJ, l.IsConditional, l.Cond, l.SortOrder, 1, 'SEED'
  FROM core.Company c CROSS JOIN @l l JOIN @e e ON e.EventCode = l.EventCode
 WHERE EXISTS (SELECT 1 FROM fin.Account a WHERE a.CompanyCode = c.CompanyCode AND a.AccountCode = l.AccountCode)
   AND NOT EXISTS (SELECT 1 FROM fin.EventAccountMap m WHERE m.CompanyCode = c.CompanyCode
                     AND m.EventCode = l.EventCode AND m.LegCode = l.LegCode);
GO
/*  Registration and matching tuning, held as effective-dated reference data so a policy change
    needs no release (COM-025).  The values are the ones the COBOL falls back to when the
    parameter is absent, written down here so that the policy is visible and auditable.
    The four matching tolerances are deliberately separate figures and are never combined
    into one allowance (D-08).  REQ: AP-004, AP-005, AP-006, AP-007, AP-025, D-08 */
MERGE core.Parameter AS t
USING (VALUES
 ('AP.INVOICE.ROUNDING.GBP','*','2026-04-01','1.00','MONEY','GBP','FINCTRL','D-08','Difference between stated and calculated totals accepted as rounding'),
 ('AP.VAT.TOL.GBP','*','2026-04-01','0.05','MONEY','GBP','FINCTRL','D-08','Difference between stated and expected VAT accepted on a match'),
 ('AP.TOL.QTYPCT','*','2026-04-01','0.0000','PCT','percent','PURCHMGR','D-08','Quantity tolerance; no over-delivery is matchable without an accepted receipt'),
 ('AP.NONPO.CATEGORIES','*','2026-04-01','UTILITIES,RATES,RENT,SUBSCRIPTION,PROFESSIONAL','TEXT',NULL,'FINCTRL','D-08','Expense categories that may be invoiced without a purchase order'),
 ('AP.VAT.SIXMONTH.DAYS','*','2026-04-01','182','INT','days','FINCTRL','D-24','Days after the due date at which unpaid input VAT must be repaid'),
 ('AP.LATEPAY.DEDUCTION.ALLOWED','*','2026-04-01','0','BOOL',NULL,'FINCTRL','D-24','No deduction scheme is operated, so cash plus deduction never settles a liability')
) AS s (ParamCode, CompanyCode, EffectiveFrom, Value, DataType, UnitOfValue, OwnerRole, DecisionRef, Description)
ON t.ParamCode = s.ParamCode AND t.CompanyCode = s.CompanyCode AND t.EffectiveFrom = s.EffectiveFrom
WHEN MATCHED AND t.Value <> s.Value THEN UPDATE SET Value = s.Value, Description = s.Description
WHEN NOT MATCHED THEN INSERT (ParamCode, CompanyCode, EffectiveFrom, Value, DataType, UnitOfValue, OwnerRole, DecisionRef, Description)
     VALUES (s.ParamCode, s.CompanyCode, s.EffectiveFrom, s.Value, s.DataType, s.UnitOfValue, s.OwnerRole, s.DecisionRef, s.Description);
GO
PRINT 'Seeded AP business events and purchase ledger parameters';
GO
