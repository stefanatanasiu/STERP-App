/*=================================================================
  kit - stored procedures for enquiry and set based work.
  REQ: KIT-007, KIT-019, KIT-020, KIT-022
  The COBOL engine states the rules.  These procedures do the two
  things a set based engine does better than a record at a time
  program: aggregate for a screen, and allocate shared stock in
  one pass so two kits cannot both be promised the same units.
=================================================================*/
IF SCHEMA_ID(N'kit') IS NULL EXEC(N'CREATE SCHEMA kit');
GO
/*-----------------------------------------------------------------
  kit.usp_KitReadiness - the readiness picture behind RPT-013 and
  the kit execution screen.  A kit is ready only when every line
  is covered; the percentage is advisory (KIT-022).
-----------------------------------------------------------------*/
CREATE OR ALTER PROCEDURE kit.usp_KitReadiness
    @CompanyCode varchar(10),
    @SiteCode    varchar(10) = NULL,
    @Status      varchar(14) = NULL,
    @KitRef      varchar(30) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SELECT  k.KitRef,
            k.SiteCode,
            k.ParentItemCode,
            k.Status,
            k.PlannedGoodQty,
            k.CompletedGoodQty,
            k.NeedDate,
            k.WipValueGBP,
            LineCount    = COUNT(l.KitLineId),
            ShortLines   = SUM(CASE WHEN l.RemainingQty > l.ReservedQty
                                    THEN 1 ELSE 0 END),
            CoveredLines = SUM(CASE WHEN l.RemainingQty <= l.ReservedQty
                                    THEN 1 ELSE 0 END),
            ReadyPct     = CASE WHEN COUNT(l.KitLineId) = 0 THEN 0
                                ELSE CONVERT(decimal(5,1),
                                     100.0 * SUM(CASE WHEN l.RemainingQty
                                                      <= l.ReservedQty
                                                      THEN 1 ELSE 0 END)
                                     / COUNT(l.KitLineId)) END,
            IsReady      = CASE WHEN COUNT(l.KitLineId) > 0
                                 AND SUM(CASE WHEN l.RemainingQty
                                              > l.ReservedQty
                                              THEN 1 ELSE 0 END) = 0
                                THEN CONVERT(bit,1) ELSE CONVERT(bit,0) END,
            IssuedQty    = ISNULL(SUM(l.GrossIssuedQty),0),
            ReturnedQty  = ISNULL(SUM(l.ReturnedQty),0),
            ScrappedQty  = ISNULL(SUM(l.ScrappedQty),0)
    FROM    kit.ExecutionKit k
    LEFT JOIN kit.ExecutionKitLine l
           ON l.ExecutionKitId = k.ExecutionKitId
          AND l.Status <> 'CANCELLED'
    WHERE   k.CompanyCode = @CompanyCode
      AND  (@SiteCode IS NULL OR k.SiteCode = @SiteCode)
      AND  (@Status   IS NULL OR k.Status   = @Status)
      AND  (@KitRef   IS NULL OR k.KitRef   = @KitRef)
    GROUP BY k.KitRef, k.SiteCode, k.ParentItemCode, k.Status,
             k.PlannedGoodQty, k.CompletedGoodQty, k.NeedDate,
             k.WipValueGBP
    ORDER BY k.NeedDate, k.KitRef;
END
GO
/*-----------------------------------------------------------------
  kit.usp_TrialAllocate - allocate the free stock of one component
  across competing kits in priority order, ONCE.  KIT-007 exists
  because the old trial answered each kit on its own and two kits
  were both reported complete on the same units.
  Nothing is committed: this is arithmetic over a snapshot.
-----------------------------------------------------------------*/
CREATE OR ALTER PROCEDURE kit.usp_TrialAllocate
    @CompanyCode varchar(10),
    @SiteCode    varchar(10) = NULL,
    @ItemCode    varchar(40) = NULL
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE @demand TABLE (
        Seq         int IDENTITY(1,1) PRIMARY KEY,
        KitRef      varchar(30),
        LineNum     int,
        ItemCode    varchar(40),
        NeedDate    date,
        RequiredQty decimal(19,6));

    INSERT @demand (KitRef, LineNum, ItemCode, NeedDate, RequiredQty)
    SELECT  k.KitRef, l.LineNum, l.ComponentItemCode,
            ISNULL(l.NeedDate, k.NeedDate), l.RemainingQty
    FROM    kit.ExecutionKitLine l
    JOIN    kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId
    WHERE   k.CompanyCode = @CompanyCode
      AND  (@SiteCode IS NULL OR k.SiteCode = @SiteCode)
      AND  (@ItemCode IS NULL OR l.ComponentItemCode = @ItemCode)
      AND   k.Status IN ('RELEASED','PARTISSUED','ISSUED','PARTCOMPLETE')
      AND   l.Status <> 'CANCELLED'
      AND   l.RemainingQty > 0
    ORDER BY ISNULL(l.NeedDate, k.NeedDate), k.KitRef, l.LineNum;

    /* Free stock is read once per item and then consumed down the
       priority list, so the same unit is never promised twice. */
    SELECT  d.Seq, d.KitRef, d.LineNum, d.ItemCode, d.NeedDate,
            d.RequiredQty,
            FreeQty       = f.FreeQty,
            ClaimedBefore = ISNULL(e.Earlier, 0),
            AllocatedQty  =
                CASE WHEN f.FreeQty - ISNULL(e.Earlier,0) <= 0 THEN 0
                     WHEN f.FreeQty - ISNULL(e.Earlier,0) >= d.RequiredQty
                          THEN d.RequiredQty
                     ELSE f.FreeQty - ISNULL(e.Earlier,0) END,
            ShortQty      =
                d.RequiredQty -
                CASE WHEN f.FreeQty - ISNULL(e.Earlier,0) <= 0 THEN 0
                     WHEN f.FreeQty - ISNULL(e.Earlier,0) >= d.RequiredQty
                          THEN d.RequiredQty
                     ELSE f.FreeQty - ISNULL(e.Earlier,0) END
    FROM    @demand d
    OUTER APPLY (
        SELECT FreeQty = ISNULL(SUM(b.QtyOnHand - b.QtyReserved), 0)
        FROM   inv.StockBalance b
        WHERE  b.CompanyCode = @CompanyCode
          AND  b.ItemCode = d.ItemCode
          AND  b.QualityStatus = 'ACCEPTED'
          AND  b.OwnerType = 'OWNED'
          AND (@SiteCode IS NULL OR b.SiteCode = @SiteCode)) f
    OUTER APPLY (
        SELECT Earlier = SUM(d2.RequiredQty)
        FROM   @demand d2
        WHERE  d2.ItemCode = d.ItemCode
          AND  d2.Seq < d.Seq) e
    ORDER BY d.Seq;
END
GO
/*-----------------------------------------------------------------
  kit.usp_KitGenealogy - the backward (recall) trace as a set.
  GENEALOG answers the online enquiry; this answers "give me the
  whole recall list" for a report or an export (KIT-019).
-----------------------------------------------------------------*/
CREATE OR ALTER PROCEDURE kit.usp_KitGenealogy
    @CompanyCode varchar(10),
    @ItemCode    varchar(40),
    @LotCode     varchar(30) = NULL,
    @SerialNo    varchar(40) = NULL,
    @MaxDepth    int         = 5
AS
BEGIN
    SET NOCOUNT ON;
    SET @MaxDepth = CASE WHEN @MaxDepth BETWEEN 1 AND 12
                         THEN @MaxDepth ELSE 5 END;

    WITH trace AS (
        SELECT  Depth = 1, g.GenealogyId, g.KitRef, g.ParentItemCode,
                g.ParentLotCode, g.ParentSerialNo, g.ComponentItemCode,
                g.ComponentLotCode, g.ComponentSerialNo, g.ConsumedQty,
                g.WasSubstitute, g.PlannedItemCode
        FROM    kit.KitGenealogy g
        WHERE   g.CompanyCode = @CompanyCode
          AND   g.ComponentItemCode = @ItemCode
          AND  (@LotCode  IS NULL OR ISNULL(g.ComponentLotCode,'')  = @LotCode)
          AND  (@SerialNo IS NULL OR ISNULL(g.ComponentSerialNo,'') = @SerialNo)
        UNION ALL
        SELECT  Depth = t.Depth + 1, g.GenealogyId, g.KitRef,
                g.ParentItemCode, g.ParentLotCode, g.ParentSerialNo,
                g.ComponentItemCode, g.ComponentLotCode,
                g.ComponentSerialNo, g.ConsumedQty, g.WasSubstitute,
                g.PlannedItemCode
        FROM    kit.KitGenealogy g
        JOIN    trace t
               ON g.ComponentItemCode = t.ParentItemCode
              AND ISNULL(g.ComponentLotCode,'')  = ISNULL(t.ParentLotCode,'')
              AND ISNULL(g.ComponentSerialNo,'') = ISNULL(t.ParentSerialNo,'')
        WHERE   g.CompanyCode = @CompanyCode
          AND   t.Depth < @MaxDepth)
    SELECT  Depth, GenealogyId, KitRef, ParentItemCode, ParentLotCode,
            ParentSerialNo, ComponentItemCode, ComponentLotCode,
            ComponentSerialNo, ConsumedQty, WasSubstitute,
            PlannedItemCode
    FROM    trace
    ORDER BY Depth, KitRef, GenealogyId
    OPTION (MAXRECURSION 12);
END
GO
/*-----------------------------------------------------------------
  kit.usp_KitCostSummary - the seven values of KIT-020 as a set,
  for the screen and the close.  KITCOST remains the authority:
  this is the same arithmetic in one statement.
    residual = issued - returned + absorbed - completed
  Scrapped is part of issued and is reported, never deducted.
-----------------------------------------------------------------*/
CREATE OR ALTER PROCEDURE kit.usp_KitCostSummary
    @CompanyCode varchar(10),
    @KitRef      varchar(30) = NULL,
    @AsAt        date        = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SELECT  k.KitRef, k.ParentItemCode, k.Status,
            k.PlannedGoodQty, k.CompletedGoodQty,
            PlannedGBP   = ISNULL(p.PlannedGBP, 0),
            IssuedGBP    = ISNULL(m.IssuedGBP, 0),
            ReturnedGBP  = ISNULL(m.ReturnedGBP, 0),
            ScrappedGBP  = ISNULL(m.ScrappedGBP, 0),
            AbsorbedGBP  = ISNULL(m.AbsorbedGBP, 0),
            CompletedGBP = ISNULL(m.CompletedGBP, 0),
            ResidualGBP  = ISNULL(m.IssuedGBP,0) - ISNULL(m.ReturnedGBP,0)
                         + ISNULL(m.AbsorbedGBP,0) - ISNULL(m.CompletedGBP,0),
            VarianceGBP  = ISNULL(m.IssuedGBP,0) - ISNULL(m.ReturnedGBP,0)
                         + ISNULL(m.AbsorbedGBP,0) - ISNULL(p.PlannedGBP,0),
            WipValueGBP  = k.WipValueGBP
    FROM    kit.ExecutionKit k
    OUTER APPLY (
        SELECT PlannedGBP = SUM(l.CurrentRequirementQty
                                * ISNULL(l.PlannedUnitCostGBP,0))
        FROM   kit.ExecutionKitLine l
        WHERE  l.ExecutionKitId = k.ExecutionKitId
          AND  l.Status <> 'CANCELLED') p
    OUTER APPLY (
        SELECT IssuedGBP    = SUM(CASE WHEN i.IssueType IN ('ISSUE','SUBSTISSUE','BACKFLUSH')
                                       THEN i.ValueGBP ELSE 0 END),
               ReturnedGBP  = SUM(CASE WHEN i.IssueType = 'RETURN'   THEN i.ValueGBP ELSE 0 END),
               ScrappedGBP  = SUM(CASE WHEN i.IssueType = 'SCRAP'    THEN i.ValueGBP ELSE 0 END),
               AbsorbedGBP  = SUM(CASE WHEN i.IssueType IN ('LABOUR','OVERHEAD','SUBCON')
                                       THEN i.ValueGBP ELSE 0 END),
               CompletedGBP = SUM(CASE WHEN i.IssueType = 'COMPLETE' THEN i.ValueGBP ELSE 0 END)
        FROM   kit.KitIssue i
        WHERE  i.CompanyCode = k.CompanyCode
          AND  i.KitRef = k.KitRef
          AND (@AsAt IS NULL OR i.PostingDate <= @AsAt)) m
    WHERE   k.CompanyCode = @CompanyCode
      AND  (@KitRef IS NULL OR k.KitRef = @KitRef)
    ORDER BY k.KitRef;
END
GO
