/* ---------------------------------------------------------------------------
   85-kit-eventmap - the business events that kit execution posts and that the
   Finance seed (10-fin-eventmap.sql, never edited here) does not already map.

   Finance maps the stock movements themselves.  What had no mapping was the
   work in progress side of a kit: material leaving store for a job, material
   coming back, material scrapped ON the job, conversion absorbed, and the
   transfer of value out of work in progress when the parent is completed.
   An unmapped posting is BLOCKED rather than sent to a suspense account
   (fin.usp_PostJournal, STERP-E-0014), which is why each one is listed here.

   The pairs follow the rules the COBOL engine enforces:
     KIT.ISSUE      store  -> work in progress
     KIT.RETURN     work in progress -> store   (the exact reverse)
     KIT.SCRAP      work in progress -> loss of the period.  Scrap is a
                    DISPOSITION, not a reversal: it never goes back to store
                    and it never reduces the issued quantity (KIT-014, D-12).
     KIT.ABSORB     conversion recovery -> work in progress
     KIT.COMPLETE   work in progress -> finished stock
     KIT.DISASSY    the loss recognised when recovery is worth less than the
                    carrying value plus approved costs (KIT-021, D-12)

   REQ: KIT-010, KIT-013, KIT-014, KIT-015, KIT-020, KIT-021, FIN-003, D-12
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'fin.EventAccountMap') IS NOT NULL
BEGIN
    DECLARE @e table (EventCode varchar(30), Module varchar(6), EventName varchar(100));
    INSERT @e VALUES
     ('KIT.ISSUE','KIT','Material issued from store to a kit'),
     ('KIT.RETURN','KIT','Unused material returned from a kit to store'),
     ('KIT.SCRAP','KIT','Material scrapped on a kit'),
     ('KIT.ABSORB','KIT','Conversion and overhead absorbed on a kit'),
     ('KIT.COMPLETE','KIT','Completed assembly transferred from work in progress to stock'),
     ('KIT.DISASSY','KIT','Loss recognised on a controlled disassembly');

    DECLARE @l table (EventCode varchar(30), LegCode varchar(20), LegDescription varchar(100), DrCr char(2),
                      AccountCode varchar(12), CC varchar(20) NULL, PROJ varchar(20) NULL,
                      IsConditional bit, Cond varchar(60) NULL, SortOrder int);
    INSERT @l VALUES
     /*  Issue moves value, it does not create or destroy any (KIT-010).      */
     ('KIT.ISSUE','WIP','Work in progress, materials','DR','1150',NULL,NULL,0,NULL,1),
     ('KIT.ISSUE','STOCK','Stock, raw materials and components','CR','1100',NULL,NULL,0,NULL,2),
     /*  A return is the exact reverse of the issue it reverses, at the value
         attributable to that issue - not at today''s standard (KIT-013).     */
     ('KIT.RETURN','STOCK','Stock, raw materials and components','DR','1100',NULL,NULL,0,NULL,1),
     ('KIT.RETURN','WIP','Work in progress, materials','CR','1150',NULL,NULL,0,NULL,2),
     /*  Scrap leaves work in progress and becomes a loss of the period.  It
         does NOT go back to store and it does NOT raise the requirement:
         replacement material needs an approved amendment (KIT-014).          */
     ('KIT.SCRAP','LOSS','Material scrapped in production','DR','5310','PROD',NULL,0,NULL,1),
     ('KIT.SCRAP','WIP','Work in progress, materials','CR','1150',NULL,NULL,0,NULL,2),
     /*  Absorption credits the recovery account, never the cost accounts
         themselves, so the under or over recovery stays visible (KIT-020).   */
     ('KIT.ABSORB','WIP','Work in progress, conversion','DR','1155',NULL,NULL,0,NULL,1),
     ('KIT.ABSORB','RECOVERY','Conversion cost recovered','CR','5900','PROD',NULL,0,NULL,2),
     /*  Completion transfers value out of work in progress.  A subassembly is
         charged to its parent kit at the value of the subassembly only; its
         own materials were relieved here and charging them again would count
         the same pound twice (KIT-020, AC-081).                              */
     ('KIT.COMPLETE','STOCK','Stock, finished goods','DR','1120',NULL,NULL,0,NULL,1),
     ('KIT.COMPLETE','WIP','Work in progress, materials','CR','1150',NULL,NULL,0,NULL,2),
     /*  Disassembly: recovered value + recognised loss = carrying value +
         approved costs.  The loss leg is the balancing figure and is never
         allowed to be a credit (KIT-021).                                    */
     ('KIT.DISASSY','LOSS','Loss on disassembly','DR','5320','PROD',NULL,0,NULL,1),
     ('KIT.DISASSY','STOCK','Stock, raw materials and components','CR','1100',NULL,NULL,0,NULL,2);

    INSERT fin.EventAccountMap (CompanyCode, EventCode, LegCode, Module, EventName, LegDescription, DrCr, AccountCode,
                                DimCostCentre, DimProject, IsConditional, Condition, SortOrder, IsActive, CreatedBy)
    SELECT c.CompanyCode, l.EventCode, l.LegCode, e.Module, e.EventName, l.LegDescription, l.DrCr, l.AccountCode,
           l.CC, l.PROJ, l.IsConditional, l.Cond, l.SortOrder, 1, 'SEED'
      FROM core.Company c CROSS JOIN @l l JOIN @e e ON e.EventCode = l.EventCode
     WHERE NOT EXISTS (SELECT 1 FROM fin.EventAccountMap m WHERE m.CompanyCode = c.CompanyCode
                         AND m.EventCode = l.EventCode AND m.LegCode = l.LegCode)
       AND EXISTS (SELECT 1 FROM fin.Account a WHERE a.CompanyCode = c.CompanyCode
                     AND a.AccountCode = l.AccountCode);

    /*  Re-running the seed must also correct a leg that was already deployed:
        the accounts and the analysis dimensions are the mapping, and a WIP
        account that forbids a cost centre must not keep one (STERP-E-0013). */
    UPDATE m
       SET m.AccountCode = l.AccountCode, m.DrCr = l.DrCr, m.DimCostCentre = l.CC,
           m.DimProject = l.PROJ, m.LegDescription = l.LegDescription, m.SortOrder = l.SortOrder,
           m.IsConditional = l.IsConditional, m.Condition = l.Cond, m.IsActive = 1
      FROM fin.EventAccountMap m JOIN @l l ON l.EventCode = m.EventCode AND l.LegCode = m.LegCode
     WHERE m.CreatedBy = 'SEED'
       AND EXISTS (SELECT 1 FROM fin.Account a WHERE a.CompanyCode = m.CompanyCode
                     AND a.AccountCode = l.AccountCode);

    PRINT 'Seeded fin.EventAccountMap (kit execution events)';
END
GO
/*  The kit and disassembly number series.  KITCREAT asks NUMGEN for 'KIT' and
    DISASSY asks for 'DIS'; without the series the programs stop rather than
    invent a reference.                                                       */
IF OBJECT_ID(N'core.NumberSeries') IS NOT NULL
BEGIN
    DECLARE @n table (SeriesCode varchar(20), DocumentType varchar(20), Description varchar(100),
                      Prefix varchar(6), NumberLength tinyint, NextNumber bigint, MaxNumber bigint);
    INSERT @n VALUES
     ('KIT','EXECUTIONKIT','Execution kit reference','KIT',8,1001,99999999),
     ('DIS','DISASSEMBLY','Disassembly order reference','DIS',8,101,99999999);

    MERGE core.NumberSeries AS t
    USING (SELECT c.CompanyCode, n.* FROM core.Company c CROSS JOIN @n n) AS s
       ON t.CompanyCode = s.CompanyCode AND t.SeriesCode = s.SeriesCode
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (CompanyCode, SeriesCode, DocumentType, Description, Prefix, NumberLength,
                NextNumber, MinNumber, MaxNumber, IsActive, CreatedBy)
        VALUES (s.CompanyCode, s.SeriesCode, s.DocumentType, s.Description, s.Prefix,
                s.NumberLength, s.NextNumber, 1, s.MaxNumber, 1, 'SEED');

    PRINT 'Seeded core.NumberSeries (KIT, DIS)';
END
GO
