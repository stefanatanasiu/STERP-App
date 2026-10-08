/*  Finance module seed: the business events the general ledger originates itself, the tuning
    parameters its thresholds read, and the VAT return calendar.  The shared event map seed
    (10-fin-eventmap.sql) is left untouched; these are additions owned by the fin module.
    Re-runnable.  REQ: FIN-003, FIN-015, FIN-018, FIN-020, FIN-022, FIN-024, FIN-027 */
SET NOCOUNT ON;
GO
DECLARE @e TABLE (EventCode varchar(30), Module varchar(6), EventName varchar(100));
INSERT @e VALUES
 ('FIN.BANKCHARGE','FIN','Bank charge recorded from the statement'),
 ('FIN.BANKINTEREST','FIN','Bank interest received'),
 ('FIN.BANKTRANSFER','FIN','Transfer between two company bank accounts'),
 ('FIN.REVALUATION','FIN','Period end revaluation of monetary balances'),
 ('FIN.WRITEOFF','FIN','Approved write-off or value only adjustment'),
 ('FIN.OPENING','FIN','Opening balances at cut over'),
 ('FIN.VATADJUST','FIN','Approved VAT adjustment posted as a digital journal');
DECLARE @l TABLE (EventCode varchar(30), LegCode varchar(12), LegDescription varchar(100), DrCr char(2), AccountCode varchar(12),
                  CC varchar(20), PROJ varchar(20), IsConditional bit, Cond varchar(100), SortOrder int);
INSERT @l VALUES
 ('FIN.BANKCHARGE','CHARGE','Bank charges, exempt from VAT unless invoiced','DR','6420','ADMIN',NULL,0,NULL,1),
 ('FIN.BANKCHARGE','BANK','Bank current account','CR','1300',NULL,NULL,0,NULL,2),
 ('FIN.BANKINTEREST','BANK','Bank current account','DR','1300',NULL,NULL,0,NULL,1),
 ('FIN.BANKINTEREST','INTEREST','Interest receivable','CR','7010','ADMIN',NULL,0,NULL,2),
 ('FIN.BANKTRANSFER','TOBANK','Receiving bank account','DR','1330',NULL,NULL,1,'Receiving account of the transfer',1),
 ('FIN.BANKTRANSFER','FROMBANK','Paying bank account','CR','1300',NULL,NULL,1,'Paying account of the transfer',2),
 ('FIN.REVALUATION','BALANCE','Monetary balance revalued','DR','1200',NULL,NULL,1,'Account carrying the foreign balance',1),
 ('FIN.REVALUATION','FX','Unrealised exchange differences','CR','7110','ADMIN',NULL,0,NULL,2),
 ('FIN.WRITEOFF','CHARGE','Approved write-off expense','DR','6700','ADMIN',NULL,1,'Expense account on the approved reason',1),
 ('FIN.WRITEOFF','CONTRA','Asset or control account written down','CR','1230',NULL,NULL,1,'Account being adjusted',2),
 ('FIN.OPENING','BALANCE','Opening ledger balance','DR','1200',NULL,NULL,1,'Each opening account in the batch',1),
 ('FIN.OPENING','EQUITY','Opening balance reserve, cut over differences','CR','3400',NULL,NULL,0,NULL,2),
 ('FIN.VATADJUST','VATCTL','VAT control, return in progress','DR','2050',NULL,NULL,1,'Direction depends on the adjustment',1),
 ('FIN.VATADJUST','CONTRA','Approved contra for the VAT adjustment','CR','6900','ADMIN',NULL,1,'Approved contra account',2);
INSERT fin.EventAccountMap (CompanyCode, EventCode, LegCode, Module, EventName, LegDescription, DrCr, AccountCode,
                            DimCostCentre, DimProject, IsConditional, Condition, SortOrder, IsActive, CreatedBy)
SELECT c.CompanyCode, l.EventCode, l.LegCode, e.Module, e.EventName, l.LegDescription, l.DrCr, l.AccountCode,
       l.CC, l.PROJ, l.IsConditional, l.Cond, l.SortOrder, 1, 'SEED'
  FROM core.Company c CROSS JOIN @l l JOIN @e e ON e.EventCode = l.EventCode
 WHERE NOT EXISTS (SELECT 1 FROM fin.EventAccountMap m WHERE m.CompanyCode = c.CompanyCode
                     AND m.EventCode = l.EventCode AND m.LegCode = l.LegCode);
GO
/*  Thresholds read by PERCLOSE, CTLRECON, GLADJUST and VATERRC.  Holding them as effective-dated
    parameters means an HMRC or policy change needs no new release (COM-025). */
MERGE core.Parameter AS t
USING (VALUES
 ('FIN.CLOSE.MATERIALITY','*','2026-04-01','1000.00','MONEY','GBP','FINCTRL','D-15','Difference above which a close checklist step must be formally accepted'),
 ('FIN.WRITEOFF.CLERK','*','2026-04-01','500.00','MONEY','GBP','FINCTRL','D-15','Write-off value a ledger clerk may post without approval'),
 ('FIN.WRITEOFF.MANAGER','*','2026-04-01','5000.00','MONEY','GBP','FINCTRL','D-15','Write-off value a manager may approve; above this the controller must'),
 ('VAT.ERROR.CAP','*','2026-04-01','50000.00','MONEY','GBP','VATOWNER','D-02','Cap on the one per cent of box 6 VAT error correction threshold'),
 ('FIN.REVAL.RATETYPE','*','2026-04-01','SPOT','TEXT',NULL,'TREASURY','D-14','Rate type used for the period end revaluation')
) AS s (ParamCode, CompanyCode, EffectiveFrom, Value, DataType, UnitOfValue, OwnerRole, DecisionRef, Description)
ON t.ParamCode = s.ParamCode AND t.CompanyCode = s.CompanyCode AND t.EffectiveFrom = s.EffectiveFrom
WHEN MATCHED AND t.Value <> s.Value THEN UPDATE SET Value = s.Value, Description = s.Description
WHEN NOT MATCHED THEN INSERT (ParamCode, CompanyCode, EffectiveFrom, Value, DataType, UnitOfValue, OwnerRole, DecisionRef, Description)
     VALUES (s.ParamCode, s.CompanyCode, s.EffectiveFrom, s.Value, s.DataType, s.UnitOfValue, s.OwnerRole, s.DecisionRef, s.Description);
GO
/*  VAT return calendar.  Both companies file quarterly on the March stagger, so the quarters end
    in June, September, December and March.  Filing and payment are due one calendar month and
    seven days after the period end.  Quarters up to September 2026 are finalised, the December
    2026 quarter is open.  REQ: FIN-018, FIN-025 */
DECLARE @q TABLE (VatPeriodKey varchar(10), StartDate date, EndDate date, Status varchar(14));
INSERT @q VALUES
 ('26A1','2026-04-01','2026-06-30','FINALISED'), ('26A2','2026-07-01','2026-09-30','FINALISED'),
 ('26A3','2026-10-01','2026-12-31','OPEN'),      ('26A4','2027-01-01','2027-03-31','OPEN'),
 ('27A1','2027-04-01','2027-06-30','OPEN');
INSERT fin.VatPeriod (CompanyCode, VatPeriodKey, VatRegNo, StartDate, EndDate, FilingDueDate, PaymentDueDate, Frequency, Status, FinalisedBy, FinalisedAt)
SELECT c.CompanyCode, q.VatPeriodKey, c.VatRegNo, q.StartDate, q.EndDate,
       DATEADD(day, 7, DATEADD(month, 1, q.EndDate)), DATEADD(day, 7, DATEADD(month, 1, q.EndDate)), 'QUARTERLY', q.Status,
       CASE WHEN q.Status = 'FINALISED' THEN 'VATOWNER' END,
       CASE WHEN q.Status = 'FINALISED' THEN DATEADD(day, 20, CAST(q.EndDate AS datetime2(3))) END
  FROM core.Company c CROSS JOIN @q q
 WHERE NOT EXISTS (SELECT 1 FROM fin.VatPeriod p WHERE p.CompanyCode = c.CompanyCode AND p.VatPeriodKey = q.VatPeriodKey);
GO
/*  Statutory obligations for the current and next accounting reference dates (FIN-030).
    AC-119: a 31 March 2027 year end gives 31 December 2027, 1 January 2028 and 31 March 2028. */
DECLARE @pe TABLE (PeriodEndDate date);
INSERT @pe VALUES ('2026-03-31'), ('2027-03-31');
INSERT fin.StatutoryObligation (CompanyCode, PeriodEndDate, ObligationType, Description, DueDate, EscalateFrom, ResponsibleRole, Status)
SELECT c.CompanyCode, p.PeriodEndDate, o.ObligationType, o.Description,
       CASE o.ObligationType WHEN 'ACCOUNTS' THEN DATEADD(month, 9, p.PeriodEndDate)
                             WHEN 'CTPAYMENT' THEN DATEADD(day, 1, DATEADD(month, 9, p.PeriodEndDate))
                             WHEN 'CTRETURN' THEN DATEADD(month, 12, p.PeriodEndDate)
                             ELSE DATEADD(day, 14, DATEADD(month, 12, p.PeriodEndDate)) END,
       DATEADD(day, -30, CASE o.ObligationType WHEN 'ACCOUNTS' THEN DATEADD(month, 9, p.PeriodEndDate)
                             WHEN 'CTPAYMENT' THEN DATEADD(day, 1, DATEADD(month, 9, p.PeriodEndDate))
                             WHEN 'CTRETURN' THEN DATEADD(month, 12, p.PeriodEndDate)
                             ELSE DATEADD(day, 14, DATEADD(month, 12, p.PeriodEndDate)) END),
       o.ResponsibleRole, 'OPEN'
  FROM core.Company c CROSS JOIN @pe p
 CROSS JOIN (VALUES ('ACCOUNTS','Annual accounts filed at Companies House','FINCTRL'),
                    ('CTPAYMENT','Corporation tax payment','FINCTRL'),
                    ('CTRETURN','Corporation tax return CT600','FINCTRL'),
                    ('CONFIRMATION','Confirmation statement','SYSADM')) AS o (ObligationType, Description, ResponsibleRole)
 WHERE NOT EXISTS (SELECT 1 FROM fin.StatutoryObligation x WHERE x.CompanyCode = c.CompanyCode
                     AND x.PeriodEndDate = p.PeriodEndDate AND x.ObligationType = o.ObligationType);
INSERT fin.PeriodOfAccount (CompanyCode, StartDate, EndDate, LengthMonths, Framework, IsArdChange, Status)
SELECT c.CompanyCode, DATEADD(year, -1, DATEADD(day, 1, p.PeriodEndDate)), p.PeriodEndDate, 12, c.AccountingFramework, 0,
       CASE WHEN p.PeriodEndDate < '2026-10-01' THEN 'CLOSED' ELSE 'OPEN' END
  FROM core.Company c CROSS JOIN @pe p
 WHERE NOT EXISTS (SELECT 1 FROM fin.PeriodOfAccount x WHERE x.CompanyCode = c.CompanyCode AND x.EndDate = p.PeriodEndDate);
GO
