/*=================================================================
  kit - views for the execution screens and the extracts.
  REQ: KIT-022, KIT-020
  Views carry no rules of their own: they present what the COBOL
  engine has already decided, in the shape a grid wants.
=================================================================*/
IF SCHEMA_ID(N'kit') IS NULL EXEC(N'CREATE SCHEMA kit');
GO
/*-----------------------------------------------------------------
  kit.vw_KitExecution - one row per kit for the execution list.
  NetIssuedQty, RemainingQty and ExcessQty are the columns KITQTY
  wrote; they are not recomputed here, so a screen can never show
  arithmetic that disagrees with the engine.
-----------------------------------------------------------------*/
CREATE OR ALTER VIEW kit.vw_KitExecution
AS
SELECT  k.CompanyCode,
        k.SiteCode,
        k.KitRef,
        k.ParentItemCode,
        ParentDescription = ISNULL(i.Description, ''),
        k.Status,
        k.SourceDemandType,
        k.SourceDemandRef,
        k.PlannedGoodQty,
        k.CompletedGoodQty,
        k.RejectedQty,
        k.NeedDate,
        k.PlannedCompletionDate,
        k.ActualCompletionDate,
        k.OwnerUser,
        k.ReleasedBy,
        k.WipValueGBP,
        k.IsBackflush,
        LineCount     = ISNULL(a.LineCount, 0),
        ShortLines    = ISNULL(a.ShortLines, 0),
        ExcessLines   = ISNULL(a.ExcessLines, 0),
        IssuedQty     = ISNULL(a.IssuedQty, 0),
        ReturnedQty   = ISNULL(a.ReturnedQty, 0),
        ScrappedQty   = ISNULL(a.ScrappedQty, 0),
        RemainingQty  = ISNULL(a.RemainingQty, 0),
        IsReady       = CASE WHEN ISNULL(a.LineCount,0) > 0
                              AND ISNULL(a.ShortLines,0) = 0
                             THEN CONVERT(bit,1) ELSE CONVERT(bit,0) END,
        IsOpen        = CASE WHEN k.Status IN ('DRAFT','REVIEWED','RELEASED',
                                               'PARTISSUED','ISSUED',
                                               'PARTCOMPLETE')
                             THEN CONVERT(bit,1) ELSE CONVERT(bit,0) END
FROM    kit.ExecutionKit k
LEFT JOIN inv.Item i
       ON i.CompanyCode = k.CompanyCode
      AND i.ItemCode    = k.ParentItemCode
OUTER APPLY (
    SELECT  LineCount    = COUNT(*),
            ShortLines   = SUM(CASE WHEN l.RemainingQty > l.ReservedQty
                                    THEN 1 ELSE 0 END),
            ExcessLines  = SUM(CASE WHEN l.ExcessQty > 0 THEN 1 ELSE 0 END),
            IssuedQty    = SUM(l.GrossIssuedQty),
            ReturnedQty  = SUM(l.ReturnedQty),
            ScrappedQty  = SUM(l.ScrappedQty),
            RemainingQty = SUM(l.RemainingQty)
    FROM    kit.ExecutionKitLine l
    WHERE   l.ExecutionKitId = k.ExecutionKitId
      AND   l.Status <> 'CANCELLED') a;
GO
/*-----------------------------------------------------------------
  kit.vw_KitShortage - one row per material line that is still
  short, which is the list a storeman actually works from.
  A kit level summary cannot be picked (KIT-022).
-----------------------------------------------------------------*/
CREATE OR ALTER VIEW kit.vw_KitShortage
AS
SELECT  k.CompanyCode,
        k.SiteCode,
        k.KitRef,
        k.ParentItemCode,
        KitStatus         = k.Status,
        l.LineNum,
        l.ComponentItemCode,
        ComponentDesc     = ISNULL(i.Description, ''),
        PlannedItemCode   = l.ComponentItemCode,
        l.SubstituteItemCode,
        l.UomCode,
        l.CurrentRequirementQty,
        l.ReservedQty,
        l.PickedQty,
        l.GrossIssuedQty,
        l.NetIssuedQty,
        l.RemainingQty,
        l.ExcessQty,
        NeedDate          = ISNULL(l.NeedDate, k.NeedDate),
        ShortQty          = CASE WHEN l.RemainingQty > l.ReservedQty
                                 THEN l.RemainingQty - l.ReservedQty
                                 ELSE 0 END,
        LineStatus        = l.Status
FROM    kit.ExecutionKitLine l
JOIN    kit.ExecutionKit k
       ON k.ExecutionKitId = l.ExecutionKitId
LEFT JOIN inv.Item i
       ON i.CompanyCode = k.CompanyCode
      AND i.ItemCode    = l.ComponentItemCode
WHERE   k.Status IN ('RELEASED','PARTISSUED','ISSUED','PARTCOMPLETE')
  AND   l.Status <> 'CANCELLED'
  AND   l.RemainingQty > l.ReservedQty;
GO
/*-----------------------------------------------------------------
  kit.vw_KitGenealogyLine - the genealogy with the planned part
  beside the part actually fitted, so a substitution is evidence
  rather than something to be inferred (KIT-019).
-----------------------------------------------------------------*/
CREATE OR ALTER VIEW kit.vw_KitGenealogyLine
AS
SELECT  g.CompanyCode,
        g.GenealogyId,
        g.KitRef,
        g.CompletionId,
        g.ParentItemCode,
        g.ParentLotCode,
        g.ParentSerialNo,
        g.ComponentItemCode,
        g.ComponentLotCode,
        g.ComponentSerialNo,
        g.ConsumedQty,
        g.WasSubstitute,
        PlannedItemCode = ISNULL(g.PlannedItemCode, g.ComponentItemCode),
        IssueType       = ISNULL(i.IssueType, 'CONSUMED'),
        ActualDate      = i.ActualDate,
        OperatorUser    = i.OperatorUser,
        UomCode         = i.UomCode
FROM    kit.KitGenealogy g
LEFT JOIN kit.KitIssue i
       ON i.KitIssueId = g.KitIssueId;
GO
