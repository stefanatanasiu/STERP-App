/* ---------------------------------------------------------------------------
   28-itm-eventmap - the business events that stock control posts and that the
   Finance seed (10-fin-eventmap.sql, never edited here) does not already map.
   Finance maps INV.TRANSFER as the despatch leg into stock in transit, and
   INV.QUARANTINE as the move into quarantine; the arrival leg, the release back
   out of quarantine and the shelf life write off had no mapping, and an
   unmapped posting is blocked rather than sent to a suspense account
   (fin.usp_PostJournal, STERP-E-0014).
   REQ: ITM-011, ITM-013, ITM-017, FIN-003, FIN-013, COM-006, D-22
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'fin.EventAccountMap') IS NOT NULL
BEGIN
    DECLARE @e table (EventCode varchar(30), Module varchar(6), EventName varchar(100));
    INSERT @e VALUES
     ('INV.TRANSFERIN','INV','Stock arrived from transit at the receiving site'),
     ('INV.QUALITYRELEASE','INV','Stock released from quarantine or inspection'),
     ('INV.EXPIRYWRITEOFF','INV','Stock written off because its shelf life has expired');

    DECLARE @l table (EventCode varchar(30), LegCode varchar(20), LegDescription varchar(100), DrCr char(2),
                      AccountCode varchar(12), CC varchar(20) NULL, PROJ varchar(20) NULL,
                      IsConditional bit, Cond varchar(60) NULL, SortOrder int);
    INSERT @l VALUES
     /*  A transfer between sites is two movements and therefore two postings.  INV.TRANSFER
         clears the despatching site into stock in transit; this event takes it out again when
         the goods arrive, so the stock is never in both places and never in neither (ITM-017). */
     ('INV.TRANSFERIN','STOCK','Stock, receiving site','DR','1100',NULL,NULL,0,NULL,1),
     ('INV.TRANSFERIN','TRANSIT','Stock in transit','CR','1130',NULL,NULL,0,NULL,2),
     /*  Quarantined stock is still ours and still an asset, but it is held apart so that the
         balance sheet shows what is not available for use.  Releasing it is the reverse of the
         Finance event INV.QUARANTINE (ITM-013).                                               */
     ('INV.QUALITYRELEASE','STOCK','Stock, raw materials and components','DR','1100',NULL,NULL,0,NULL,1),
     ('INV.QUALITYRELEASE','QUAR','Stock held in quarantine','CR','1140',NULL,NULL,0,NULL,2),
     /*  Expired stock is a loss of the period, never a silent reduction of a carrying value
         (ITM-011).                                                                            */
     ('INV.EXPIRYWRITEOFF','LOSS','Stock written off on expiry','DR','5300','WHSE',NULL,0,NULL,1),
     ('INV.EXPIRYWRITEOFF','STOCK','Stock, raw materials and components','CR','1100',NULL,NULL,0,NULL,2);

    INSERT fin.EventAccountMap (CompanyCode, EventCode, LegCode, Module, EventName, LegDescription, DrCr, AccountCode,
                                DimCostCentre, DimProject, IsConditional, Condition, SortOrder, IsActive, CreatedBy)
    SELECT c.CompanyCode, l.EventCode, l.LegCode, e.Module, e.EventName, l.LegDescription, l.DrCr, l.AccountCode,
           l.CC, l.PROJ, l.IsConditional, l.Cond, l.SortOrder, 1, 'SEED'
      FROM core.Company c CROSS JOIN @l l JOIN @e e ON e.EventCode = l.EventCode
     WHERE NOT EXISTS (SELECT 1 FROM fin.EventAccountMap m WHERE m.CompanyCode = c.CompanyCode
                         AND m.EventCode = l.EventCode AND m.LegCode = l.LegCode);

    PRINT 'Seeded fin.EventAccountMap (ITM stock events)';
END
GO
