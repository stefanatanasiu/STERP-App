/*  Purchasing business events added to the finance event map.  This file only ADDS the purchasing
    events that POPOST names on behalf of RECEIPT, INSPECT, SVCACC, SUPPRET, IMPVAT and LANDCOST;
    it never edits 10-fin-eventmap.sql (which already owns PO.RECEIPT and PO.SERVICEACCEPT) and it
    never removes or re-points an existing mapping.  Re-runnable: nothing is inserted when the
    (CompanyCode, EventCode, LegCode) row already exists.

    fin.usp_PostJournal refuses a posting whose event has no active mapping (STERP-E-0014), so the
    mapping is what makes the event legal - the legs below are also the documented intention even
    though POPOST supplies its own analysed lines.

      PO.LANDEDEST    estimated landed cost capitalised on receipt.  Only charges necessary to
                      bring the goods to their present location and condition are capitalised
                      (FRS 102 s13); the credit is the landed cost absorption account so the
                      freight invoice later clears against it instead of hitting cost twice.
      PO.LANDEDACT    actual landed cost true-up.  Only the ACTUAL MINUS ESTIMATE delta is posted.
                      The share attributable to units already sold or consumed goes to cost of
                      sales (5110) and not back into stock - stock that has gone cannot be
                      revalued (PO-016).
      PO.RETURN       supplier return before the invoice.  The return reverses the goods received
                      not invoiced accrual at the value the goods came in at, so GRNI never
                      carries a liability for goods we no longer hold (PO-013).  Any difference
                      between the attributable receipt cost and the stock carrying value is a
                      purchase price variance, not a silent stock gain.
      PO.INSPSCRAP    inspection disposition "scrap on site".  Rejected goods that are destroyed
                      rather than returned leave stock against the material usage variance; a
                      supplier claim is a separate commercial matter and never a stock credit.
      PO.RECEIPT.PPV  a standard costed item is taken into stock at standard; the difference
                      between the purchase order price and standard is recognised immediately as
                      purchase price variance rather than being buried in the stock value (D-25).
      PO.IMPORTDUTY   customs duty on an import entry.  Duty is an irrecoverable tax that forms
                      part of the cost of the goods, so it is capitalised into stock and credited
                      to goods received not invoiced until the agent's invoice arrives (PO-025).
      PO.IMPORTVAT    postponed import VAT.  Both legs are posted on the same journal and the net
                      effect on profit and on cash is nil: the output side is declared in box 1
                      and the input side recovered in box 4 of the same return.  Import VAT is
                      NEVER landed cost.

    Every account code below already exists in db\seed\08-fin-coa.sql for both companies:
      1100 Stock, raw materials and components (STOCK)   2010 Goods received not invoiced (CRED)
      5200 Landed cost absorbed (COS)                    5110 Material usage variance (VARI)
      5100 Purchase price variance (VARI)                1260 Postponed import VAT, input (DEBT)
      2040 VAT payable, output tax (TAXBS)
    REQ: PO-009, PO-010, PO-013, PO-014, PO-015, PO-016, PO-017, PO-025, FIN-003, FIN-004, COM-014, D-25 */
SET NOCOUNT ON;
GO
DECLARE @e TABLE (EventCode varchar(30), Module varchar(6), EventName varchar(100));
INSERT @e VALUES
 ('PO.LANDEDEST','PO','Estimated landed cost capitalised on receipt'),
 ('PO.LANDEDACT','PO','Actual landed cost true-up, delta only'),
 ('PO.RETURN','PO','Supplier return before invoice, clears GRNI'),
 ('PO.INSPSCRAP','PO','Rejected goods scrapped on site after inspection'),
 ('PO.RECEIPT.PPV','PO','Purchase price variance on a standard costed receipt'),
 ('PO.IMPORTDUTY','PO','Customs duty capitalised into the cost of the goods'),
 ('PO.IMPORTVAT','PO','Postponed import VAT, both legs on one journal');
DECLARE @l TABLE (EventCode varchar(30), LegCode varchar(20), LegDescription varchar(100), DrCr char(2),
                  AccountCode varchar(20), CC varchar(20), PROJ varchar(20), IsConditional bit, Cond varchar(60), SortOrder int);
INSERT @l VALUES
 ('PO.LANDEDEST','STOCK','Landed cost into stock','DR','1100',NULL,NULL,0,NULL,1),
 ('PO.LANDEDEST','ABSORB','Landed cost absorbed','CR','5200','PURCH',NULL,0,NULL,2),
 ('PO.LANDEDACT','STOCK','Landed cost delta into stock','DR','1100',NULL,NULL,1,'Units still held',1),
 ('PO.LANDEDACT','COGS','Landed cost delta on units already consumed','DR','5110','PURCH',NULL,1,'Units already sold or consumed',2),
 ('PO.LANDEDACT','ABSORB','Landed cost absorbed','CR','5200','PURCH',NULL,0,NULL,3),
 ('PO.RETURN','GRNI','Goods received not invoiced reversed','DR','2010',NULL,NULL,0,NULL,1),
 ('PO.RETURN','STOCK','Stock, raw materials and components','CR','1100',NULL,NULL,0,NULL,2),
 ('PO.RETURN','PPV','Difference between receipt cost and carrying value','DR','5100','PURCH',NULL,1,'Carrying value differs from receipt cost',3),
 ('PO.INSPSCRAP','SCRAP','Material usage variance, goods scrapped on site','DR','5110','PURCH',NULL,0,NULL,1),
 ('PO.INSPSCRAP','STOCK','Stock, raw materials and components','CR','1100',NULL,NULL,0,NULL,2),
 ('PO.RECEIPT.PPV','PPV','Purchase price variance on receipt','DR','5100','PURCH',NULL,1,'Order price above standard cost',1),
 ('PO.RECEIPT.PPV','GRNI','Goods received not invoiced','CR','2010',NULL,NULL,0,NULL,2),
 ('PO.IMPORTDUTY','STOCK','Customs duty capitalised into stock','DR','1100',NULL,NULL,1,'Goods still held',1),
 ('PO.IMPORTDUTY','DUTYCOS','Customs duty on goods already consumed','DR','5110','PURCH',NULL,1,'Goods already sold or consumed',2),
 ('PO.IMPORTDUTY','GRNI','Customs agent, goods received not invoiced','CR','2010',NULL,NULL,0,NULL,3),
 ('PO.IMPORTVAT','VATIN','Postponed import VAT recovered, box 4','DR','1260',NULL,NULL,0,NULL,1),
 ('PO.IMPORTVAT','VATOUT','Postponed import VAT declared, box 1','CR','2040',NULL,NULL,0,NULL,2);
INSERT fin.EventAccountMap (CompanyCode, EventCode, LegCode, Module, EventName, LegDescription, DrCr, AccountCode,
                            DimCostCentre, DimProject, IsConditional, Condition, SortOrder, IsActive, CreatedBy)
SELECT c.CompanyCode, l.EventCode, l.LegCode, e.Module, e.EventName, l.LegDescription, l.DrCr, l.AccountCode,
       l.CC, l.PROJ, l.IsConditional, l.Cond, l.SortOrder, 1, 'SEED'
  FROM core.Company c CROSS JOIN @l l JOIN @e e ON e.EventCode = l.EventCode
 WHERE NOT EXISTS (SELECT 1 FROM fin.EventAccountMap m WHERE m.CompanyCode = c.CompanyCode
                     AND m.EventCode = l.EventCode AND m.LegCode = l.LegCode);
GO
/*  A purchasing event that names an account which is not an active posting account would be
    refused by fin.usp_PostJournal at run time with STERP-E-0012, in the middle of a goods receipt.
    Fail the seed loudly instead, so a chart of accounts change is noticed at deployment. */
IF EXISTS (SELECT 1 FROM fin.EventAccountMap m
            WHERE m.EventCode IN ('PO.LANDEDEST','PO.LANDEDACT','PO.RETURN','PO.INSPSCRAP','PO.RECEIPT.PPV',
                                  'PO.IMPORTDUTY','PO.IMPORTVAT')
              AND NOT EXISTS (SELECT 1 FROM fin.Account a WHERE a.CompanyCode = m.CompanyCode
                                AND a.AccountCode = m.AccountCode AND a.Status = 'ACTIVE' AND a.AccountKind = 'POSTING'))
    THROW 50081, 'STERP-E-0081: A purchasing event mapping names an account that is not an active posting account.', 1;
GO
/*  The purchasing tolerance policy of D-08.  po.ToleranceRule overrides the compiled POACCNTS
    fallback at run time; the COMPANY scope row is the baseline every site starts from.  Over
    delivery is zero per cent by default - an item or supplier scoped row may raise it, but TOLCHK
    caps any override at five per cent whatever the table says. */
IF OBJECT_ID(N'po.ToleranceRule') IS NOT NULL
INSERT po.ToleranceRule (CompanyCode, ScopeType, ScopeCode, OverDeliveryPct, OverDeliveryQty, UnderDeliveryPct,
                         PriceVariancePct, PriceVarianceAbs, ChargeVarianceAbs, EarlyDeliveryDays, LateDeliveryDays,
                         ActionOverDelivery, IsActive, ModifiedBy, ModifiedAt)
SELECT c.CompanyCode, 'COMPANY', c.CompanyCode, 0, 0, 100, 2, 50, 25, 5, 0, 'BLOCK', 1, 'SEED', SYSDATETIME()
  FROM core.Company c
 WHERE NOT EXISTS (SELECT 1 FROM po.ToleranceRule t WHERE t.CompanyCode = c.CompanyCode
                     AND t.ScopeType = 'COMPANY' AND t.ScopeCode = c.CompanyCode);
GO
