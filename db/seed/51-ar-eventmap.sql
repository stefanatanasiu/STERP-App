/*  Debtors module seed: the business events the sales ledger originates that the shared event map
    (10-fin-eventmap.sql, left untouched) does not already carry, plus the two tuning parameters the
    collection sequence reads.  Every AR posting goes through fin.JournalStage and fin.usp_PostJournal
    under one of these codes, so a receipt is never a free-hand journal.  Allocation itself has no
    event: applying cash or a credit to an open item moves no value out of the debtors control and
    must have no second cash effect (AR-010); only its realised exchange difference posts, under the
    AR.FXGAIN and AR.FXLOSS codes that the shared seed already carries.
    REQ: AR-004, AR-006, AR-009, AR-010, AR-011, AR-012, AR-013, AR-014, AR-016, AR-019, AR-020,
    AR-023, AR-024, AR-025, AR-026, D-16, D-22, D-25, D-28 */
SET NOCOUNT ON;
GO
DECLARE @e TABLE (EventCode varchar(30), Module varchar(6), EventName varchar(100));
INSERT @e VALUES
 ('AR.SMALLDIFF','AR','Small settlement difference written off under D-22'),
 ('AR.DEPOSITAPPLY','AR','Customer deposit applied to an invoice'),
 ('AR.REFUND','AR','Unapplied credit refunded to the customer'),
 ('AR.DISHONOUR','AR','Receipt dishonoured by the paying bank'),
 ('AR.DDINDEMNITY','AR','Direct Debit indemnity claim met under the guarantee'),
 ('AR.RECEIPTREVERSE','AR','Receipt reversed and the debt reinstated'),
 ('AR.BADDEBTREPAY','AR','Bad debt relief repaid after later recovery');
DECLARE @l TABLE (EventCode varchar(30), LegCode varchar(12), LegDescription varchar(100), DrCr char(2), AccountCode varchar(12),
                  CC varchar(20), PROJ varchar(20), IsConditional bit, Cond varchar(100), SortOrder int);
INSERT @l VALUES
 ('AR.SMALLDIFF','DIFF','Rounding and settlement differences','DR','6900','ADMIN',NULL,0,NULL,1),
 ('AR.SMALLDIFF','DEBTOR','Trade debtors control','CR','1200',NULL,NULL,0,NULL,2),
 ('AR.DEPOSITAPPLY','DEFERRED','Customer deposits and deferred income','DR','2080',NULL,NULL,0,NULL,1),
 ('AR.DEPOSITAPPLY','DEBTOR','Trade debtors control','CR','1200',NULL,NULL,0,NULL,2),
 ('AR.REFUND','DEBTOR','Trade debtors control','DR','1200',NULL,NULL,0,NULL,1),
 ('AR.REFUND','BANK','Bank current account','CR','1300',NULL,NULL,1,'Bank account the refund is paid from',2),
 ('AR.DISHONOUR','DEBTOR','Trade debtors control, debt reinstated','DR','1200',NULL,NULL,0,NULL,1),
 ('AR.DISHONOUR','BANK','Bank current account','CR','1300',NULL,NULL,0,NULL,2),
 ('AR.DDINDEMNITY','DEBTOR','Trade debtors control, collection returned','DR','1200',NULL,NULL,0,NULL,1),
 ('AR.DDINDEMNITY','BANK','Bank current account','CR','1300',NULL,NULL,0,NULL,2),
 ('AR.RECEIPTREVERSE','DEBTOR','Trade debtors control, debt reinstated','DR','1200',NULL,NULL,0,NULL,1),
 ('AR.RECEIPTREVERSE','BANK','Bank current account','CR','1300',NULL,NULL,1,'Bank account the receipt was banked to',2),
 ('AR.BADDEBTREPAY','VATCTL','VAT control, relief repaid','DR','2050',NULL,NULL,0,NULL,1),
 ('AR.BADDEBTREPAY','VATIN','VAT recoverable on bad debt relief','CR','1250',NULL,NULL,0,NULL,2);
INSERT fin.EventAccountMap (CompanyCode, EventCode, LegCode, Module, EventName, LegDescription, DrCr, AccountCode,
                            DimCostCentre, DimProject, IsConditional, Condition, SortOrder, IsActive, CreatedBy)
SELECT c.CompanyCode, l.EventCode, l.LegCode, e.Module, e.EventName, l.LegDescription, l.DrCr, l.AccountCode,
       l.CC, l.PROJ, l.IsConditional, l.Cond, l.SortOrder, 1, 'SEED'
  FROM core.Company c CROSS JOIN @l l JOIN @e e ON e.EventCode = l.EventCode
 WHERE EXISTS (SELECT 1 FROM fin.Account a WHERE a.CompanyCode = c.CompanyCode AND a.AccountCode = l.AccountCode)
   AND NOT EXISTS (SELECT 1 FROM fin.EventAccountMap m WHERE m.CompanyCode = c.CompanyCode
                     AND m.EventCode = l.EventCode AND m.LegCode = l.LegCode);
GO
/*  Collection sequence timings, held as effective-dated reference data so that a policy change
    needs no release (COM-025).  The values are the D-06 baseline printed in section 8 of the
    specification.  REQ: AR-016, D-06, D-28 */
MERGE core.Parameter AS t
USING (VALUES
 ('AR.DUNNING.R1.DAYS','*','2026-04-01','7','INT','days','CREDMGR','D-06','Days past due at which the courteous reminder is issued'),
 ('AR.DUNNING.R2.DAYS','*','2026-04-01','21','INT','days','CREDMGR','D-06','Days past due at which the firm reminder is issued'),
 ('AR.DUNNING.STOP.DAYS','*','2026-04-01','30','INT','days','CREDMGR','D-06','Days past due at which new release is stopped'),
 ('AR.DUNNING.DEMAND.DAYS','*','2026-04-01','45','INT','days','CREDMGR','D-06','Days past due at which a letter before action is issued'),
 ('AR.LATEPAY.MARGIN.PCT','*','2026-04-01','8.0000','PCT','percent','FINCTRL','D-25','Statutory margin over the reference rate'),
 ('AR.LATEPAY.REFRATE.PCT','*','2026-07-01','4.0000','PCT','percent','FINCTRL','D-25','Bank of England base rate fixing for the half year')
) AS s (ParamCode, CompanyCode, EffectiveFrom, Value, DataType, UnitOfValue, OwnerRole, DecisionRef, Description)
ON t.ParamCode = s.ParamCode AND t.CompanyCode = s.CompanyCode AND t.EffectiveFrom = s.EffectiveFrom
WHEN MATCHED AND t.Value <> s.Value THEN UPDATE SET Value = s.Value, Description = s.Description
WHEN NOT MATCHED THEN INSERT (ParamCode, CompanyCode, EffectiveFrom, Value, DataType, UnitOfValue, OwnerRole, DecisionRef, Description)
     VALUES (s.ParamCode, s.CompanyCode, s.EffectiveFrom, s.Value, s.DataType, s.UnitOfValue, s.OwnerRole, s.DecisionRef, s.Description);
GO
PRINT 'Seeded AR business events and collection parameters';
GO
