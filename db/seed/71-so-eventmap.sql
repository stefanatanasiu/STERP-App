/*  Sales order business events added to the finance event map.  This file only ADDS the sales
    events that the sales order batch suite names when it posts; it never edits 10-fin-eventmap.sql
    and it never removes or re-points an existing mapping.  Re-runnable: nothing is inserted when
    the (CompanyCode, EventCode, LegCode) row already exists, so the script may be deployed as
    often as the rest of db\seed.

    Why these events exist separately from SO.COGS / SO.REBATEACCRUAL in 10-fin-eventmap.sql:
      SO.DESPATCH.COGS   named by the despatch confirmation path so that the cost of a sales
                         despatch is recognised when control passes (D-03), not when the invoice
                         is raised.  Debit cost of sales, credit finished goods stock.
      SO.REBATE.ACCRUAL  named by REBATE.cob.  A retrospective rebate is variable consideration
                         and therefore a REDUCTION OF REVENUE, never a cost: the debit is the
                         turnover class account 4120 "Rebates and retrospective discounts" and the
                         credit is the balance sheet accrual 2090 (SO-027, FRS 102 s23).
      SO.DROPSHIP.COGS   a drop shipped line never touches our stock, so the credit is goods
                         received not invoiced (2010) and not 1120; the supplier invoice clears
                         GRNI later through AP.INVOICE.

    Every account code below already exists in db\seed\08-fin-coa.sql for both companies:
      5010 Cost of goods sold, components (COS)      1120 Stock, finished goods (STOCK)
      4120 Rebates and retrospective discounts (TURN) 2090 Rebate and retrospective discount
      2010 Goods received not invoiced (CRED)              accrual (CRED)
    REQ: SO-013, SO-020, SO-026, SO-027, SO-028, FIN-003, FIN-013, COM-006, D-03 */
SET NOCOUNT ON;
GO
DECLARE @e TABLE (EventCode varchar(30), Module varchar(6), EventName varchar(100));
INSERT @e VALUES
 ('SO.DESPATCH.COGS','SO','Cost of goods sold on a confirmed sales despatch'),
 ('SO.REBATE.ACCRUAL','SO','Retrospective rebate accrued against revenue'),
 ('SO.DROPSHIP.COGS','SO','Cost of goods sold on a drop shipped sales line');
DECLARE @l TABLE (EventCode varchar(30), LegCode varchar(20), LegDescription varchar(100), DrCr char(2),
                  AccountCode varchar(20), CC varchar(20), PROJ varchar(20), IsConditional bit, Cond varchar(60), SortOrder int);
INSERT @l VALUES
 ('SO.DESPATCH.COGS','COGS','Cost of goods sold','DR','5010','PROD2',NULL,1,'Item valuation group',1),
 ('SO.DESPATCH.COGS','STOCK','Stock, finished goods','CR','1120',NULL,NULL,0,NULL,2),
 ('SO.REBATE.ACCRUAL','REBATE','Sales rebates, reduction of revenue','DR','4120','SALES',NULL,0,NULL,1),
 ('SO.REBATE.ACCRUAL','ACCRUAL','Accrued rebate liability','CR','2090',NULL,NULL,0,NULL,2),
 ('SO.DROPSHIP.COGS','COGS','Cost of goods sold, drop shipment','DR','5010','PROD2',NULL,1,'Item valuation group',1),
 ('SO.DROPSHIP.COGS','GRNI','Goods received not invoiced','CR','2010',NULL,NULL,0,NULL,2);
INSERT fin.EventAccountMap (CompanyCode, EventCode, LegCode, Module, EventName, LegDescription, DrCr, AccountCode,
                            DimCostCentre, DimProject, IsConditional, Condition, SortOrder, IsActive, CreatedBy)
SELECT c.CompanyCode, l.EventCode, l.LegCode, e.Module, e.EventName, l.LegDescription, l.DrCr, l.AccountCode,
       l.CC, l.PROJ, l.IsConditional, l.Cond, l.SortOrder, 1, 'SEED'
  FROM core.Company c CROSS JOIN @l l JOIN @e e ON e.EventCode = l.EventCode
 WHERE NOT EXISTS (SELECT 1 FROM fin.EventAccountMap m WHERE m.CompanyCode = c.CompanyCode
                     AND m.EventCode = l.EventCode AND m.LegCode = l.LegCode);
GO
/*  A sales event that names an account which is not an active posting account would be refused by
    fin.usp_PostJournal at run time with STERP-E-0012.  Fail the seed loudly instead, so that a
    chart of accounts change is noticed at deployment rather than at the first rebate run. */
IF EXISTS (SELECT 1 FROM fin.EventAccountMap m
            WHERE m.EventCode IN ('SO.DESPATCH.COGS','SO.REBATE.ACCRUAL','SO.DROPSHIP.COGS')
              AND NOT EXISTS (SELECT 1 FROM fin.Account a WHERE a.CompanyCode = m.CompanyCode
                                AND a.AccountCode = m.AccountCode AND a.Status = 'ACTIVE' AND a.AccountKind = 'POSTING'))
    THROW 50071, 'STERP-E-0071: A sales order event mapping names an account that is not an active posting account.', 1;
GO
