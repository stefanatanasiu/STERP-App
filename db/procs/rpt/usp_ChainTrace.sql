/*  rpt.usp_ChainTrace - the set-based equivalent of the CHAINTRC online program, for the chain
    trace grid in the client.  One document reference in; the ordered steps of its order to cash,
    procure to pay or assembly chain out, operational steps first and then the financial effects of
    the same chain.  Reversals, credits and returns are listed as their own steps and are never
    folded back into the step they reverse (XMD-004).  The proc returns two result sets: the chain
    steps for the grid, and one summary row carrying the same judgement CHAINTRC puts in its
    commarea - complete, broken, truncated, the reversal count and the sterling total.
    REQ: XMD-001, XMD-002, XMD-003, XMD-004, XMD-012, RPT-015 */
SET NOCOUNT ON;
GO
IF SCHEMA_ID(N'rpt') IS NULL EXEC (N'CREATE SCHEMA rpt');
GO
CREATE OR ALTER PROCEDURE rpt.usp_ChainTrace
    @CompanyCode varchar(10),
    @DocType     varchar(12),
    @Reference   varchar(40),
    @UserCode    varchar(20) = NULL,
    @MaxSteps    int = 24,
    @StepsOnly   bit = 0,
    @SummaryOnly bit = 0
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;

    SET @DocType   = UPPER(LTRIM(RTRIM(ISNULL(@DocType, ''))));
    SET @Reference = LTRIM(RTRIM(ISNULL(@Reference, '')));
    SET @MaxSteps  = ISNULL(NULLIF(@MaxSteps, 0), 24);

    IF @CompanyCode IS NULL OR LEN(LTRIM(@CompanyCode)) = 0
        THROW 50801, 'STERP-E-0801: A company is required for a chain trace.', 1;
    IF LEN(@Reference) = 0
        THROW 50802, 'STERP-E-0802: A document reference is required for a chain trace.', 1;
    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'FIN.ENQUIRY.VIEW', @CompanyCode, NULL) = 0
        THROW 50804, 'STERP-E-0804: You are not permitted to trace business chains for that company.', 1;

    DECLARE @chainType varchar(12), @chainName varchar(40), @party varchar(20) = NULL;
    SELECT @chainType = CASE WHEN @DocType IN ('ORDER', 'SALESORDER') THEN 'O2C'
                             WHEN @DocType IN ('PO', 'PURCHASE')      THEN 'P2P'
                             WHEN @DocType = 'KIT'                    THEN 'ASSEMBLY' END,
           @chainName = CASE WHEN @DocType IN ('ORDER', 'SALESORDER') THEN 'Order to cash'
                             WHEN @DocType IN ('PO', 'PURCHASE')      THEN 'Procure to pay'
                             WHEN @DocType = 'KIT'                    THEN 'Make an assembly' END;
    IF @chainType IS NULL
        THROW 50803, 'STERP-E-0803: Document type must be ORDER, PO or KIT.', 1;

    DECLARE @s TABLE (RowId int IDENTITY(1,1) NOT NULL PRIMARY KEY, SortGroup tinyint NOT NULL, SortDate date NULL,
        SortRef varchar(40) NULL, Stage varchar(24) NOT NULL, StepKind varchar(10) NOT NULL, DocumentType varchar(12) NOT NULL,
        Reference varchar(40) NOT NULL, StepDate date NULL, Status varchar(12) NULL, Quantity decimal(19,6) NULL,
        AmountGBP decimal(18,2) NOT NULL, Note varchar(60) NULL);
    DECLARE @headerFound bit = 0;

    /*  The chain head: the accepted or approved obligation the rest of the chain answers to. */
    IF @chainType = 'O2C'
    BEGIN
        INSERT @s (SortGroup, SortDate, SortRef, Stage, StepKind, DocumentType, Reference, StepDate, Status, Quantity, AmountGBP, Note)
        SELECT 1, MIN(d.OriginalPromiseDate), @Reference, 'Sales order schedules', 'OPERATION', 'ORDER', @Reference,
               MIN(d.OriginalPromiseDate), 'ACCEPTED', SUM(d.Quantity), 0,
               LEFT('Scheduled ' + CONVERT(varchar(24), SUM(d.Quantity)) + ' despatched '
                    + CONVERT(varchar(24), SUM(ISNULL(d.DespatchedQty, 0))) + ' cancelled lines '
                    + CONVERT(varchar(8), SUM(CASE WHEN d.Status = 'CANCELLED' THEN 1 ELSE 0 END)), 60)
          FROM so.DeliverySchedule d
         WHERE d.CompanyCode = @CompanyCode AND d.OrderNumber = @Reference
        HAVING COUNT(*) > 0;
        SET @headerFound = CASE WHEN @@ROWCOUNT > 0 THEN 1 ELSE 0 END;

        INSERT @s (SortGroup, SortDate, SortRef, Stage, StepKind, DocumentType, Reference, StepDate, Status, Quantity, AmountGBP, Note)
        SELECT 2, h.DespatchDate, h.DespatchRef, 'Despatch', 'OPERATION', 'DESPATCH', h.DespatchRef, h.DespatchDate,
               h.Status, NULL, ISNULL(h.CogsPostedGBP, 0), LEFT('Customer ' + ISNULL(h.CustomerCode, ''), 60)
          FROM so.Despatch h
         WHERE h.CompanyCode = @CompanyCode AND h.OrderNumber = @Reference AND h.DespatchRef IS NOT NULL;

        SELECT @party = MIN(h.CustomerCode) FROM so.Despatch h
         WHERE h.CompanyCode = @CompanyCode AND h.OrderNumber = @Reference AND h.CustomerCode IS NOT NULL;
    END
    ELSE IF @chainType = 'P2P'
    BEGIN
        INSERT @s (SortGroup, SortDate, SortRef, Stage, StepKind, DocumentType, Reference, StepDate, Status, Quantity, AmountGBP, Note)
        SELECT 1, MIN(p.OriginalConfirmedDate), @Reference, 'Purchase order schedules', 'OPERATION', 'PO', @Reference,
               MIN(p.OriginalConfirmedDate), 'APPROVED', SUM(p.Quantity), 0,
               LEFT('Ordered ' + CONVERT(varchar(24), SUM(p.Quantity)) + ' received '
                    + CONVERT(varchar(24), SUM(ISNULL(p.ReceivedQty, 0))) + ' unreceived commitment remains open', 60)
          FROM po.PurchaseSchedule p
         WHERE p.CompanyCode = @CompanyCode AND p.OrderNumber = @Reference
        HAVING COUNT(*) > 0;
        SET @headerFound = CASE WHEN @@ROWCOUNT > 0 THEN 1 ELSE 0 END;

        INSERT @s (SortGroup, SortDate, SortRef, Stage, StepKind, DocumentType, Reference, StepDate, Status, Quantity, AmountGBP, Note)
        SELECT 2, r.PhysicalDate, r.ReceiptRef, 'Receipt and GRNI', 'OPERATION', 'RECEIPT', r.ReceiptRef, r.PhysicalDate,
               r.Status, NULL, ISNULL(r.GrniValueGBP, 0), LEFT('Supplier ' + ISNULL(r.SupplierCode, ''), 60)
          FROM po.Receipt r
         WHERE r.CompanyCode = @CompanyCode AND r.PurchaseOrderNumber = @Reference AND r.ReceiptRef IS NOT NULL;

        SELECT @party = MIN(r.SupplierCode) FROM po.Receipt r
         WHERE r.CompanyCode = @CompanyCode AND r.PurchaseOrderNumber = @Reference AND r.SupplierCode IS NOT NULL;
    END
    ELSE
    BEGIN
        INSERT @s (SortGroup, SortDate, SortRef, Stage, StepKind, DocumentType, Reference, StepDate, Status, Quantity, AmountGBP, Note)
        SELECT 1, CAST(k.ModifiedAt AS date), @Reference, 'Execution kit', 'OPERATION', 'KIT', @Reference,
               CAST(k.ModifiedAt AS date), k.Status, k.PlannedGoodQty, ISNULL(k.WipValueGBP, 0),
               LEFT('Parent ' + ISNULL(k.ParentItemCode, '') + ' planned ' + CONVERT(varchar(24), k.PlannedGoodQty)
                    + ' completed ' + CONVERT(varchar(24), ISNULL(k.CompletedGoodQty, 0)), 60)
          FROM kit.ExecutionKit k
         WHERE k.CompanyCode = @CompanyCode AND k.KitRef = @Reference;
        SET @headerFound = CASE WHEN @@ROWCOUNT > 0 THEN 1 ELSE 0 END;

        INSERT @s (SortGroup, SortDate, SortRef, Stage, StepKind, DocumentType, Reference, StepDate, Status, Quantity, AmountGBP, Note)
        SELECT 2, CAST(MAX(k.ModifiedAt) AS date), 'Component issues', 'Material issue', 'OPERATION', 'KITLINE',
               'Component issues', CAST(MAX(k.ModifiedAt) AS date), 'ISSUED', SUM(ISNULL(l.NetIssuedQty, 0)),
               SUM(ISNULL(l.IssuedValueGBP, 0)), LEFT('Components issued to ' + @Reference, 60)
          FROM kit.ExecutionKitLine l
          JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId
         WHERE k.CompanyCode = @CompanyCode AND k.KitRef = @Reference
        HAVING COUNT(*) > 0;

        SELECT @party = MIN(k.ParentItemCode) FROM kit.ExecutionKit k
         WHERE k.CompanyCode = @CompanyCode AND k.KitRef = @Reference;
    END

    /*  The financial consequences of the same reference, reversals and credits marked as their own
        kind so that the grid can show them beside, not inside, the step they answer. */
    INSERT @s (SortGroup, SortDate, SortRef, Stage, StepKind, DocumentType, Reference, StepDate, Status, Quantity, AmountGBP, Note)
    SELECT 3, j.PostingDate, j.JournalNumber,
           CASE WHEN ISNULL(j.IsReversal, 0) = 1 THEN 'Reversal or credit' ELSE 'Financial effect' END,
           CASE WHEN ISNULL(j.IsReversal, 0) = 1 THEN 'REVERSAL' ELSE 'FINANCIAL' END,
           'JOURNAL', j.JournalNumber, j.PostingDate, j.Status, NULL, ISNULL(j.TotalDebitGBP, 0),
           LEFT('Event ' + ISNULL(j.EventCode, ISNULL(j.SourceModule, '')) + ' raised by ' + ISNULL(j.SourceModule, ''), 60)
      FROM fin.Journal j
     WHERE j.CompanyCode = @CompanyCode AND ISNULL(j.SourceRef, '') = @Reference;

    /*  Judgement, stated and not implied (XMD-012). */
    DECLARE @steps int, @oper int, @fin int, @rev int, @total decimal(18,2), @trunc bit, @complete bit = 0, @broken bit = 0,
            @reason varchar(10) = '', @message varchar(200) = '';
    SELECT @steps = COUNT(*), @oper = SUM(CASE WHEN SortGroup < 3 THEN 1 ELSE 0 END),
           @fin = SUM(CASE WHEN SortGroup = 3 THEN 1 ELSE 0 END),
           @rev = SUM(CASE WHEN StepKind = 'REVERSAL' THEN 1 ELSE 0 END),
           @total = ISNULL(SUM(CASE WHEN SortGroup = 3 THEN AmountGBP ELSE 0 END), 0)
      FROM @s;
    SET @trunc = CASE WHEN @steps > @MaxSteps THEN 1 ELSE 0 END;
    IF @headerFound = 0
    BEGIN
        SET @broken = 1;
        SET @reason = 'XMD0016';
        SET @message = CASE @chainType WHEN 'O2C' THEN 'No sales order schedule carries that number.'
                                       WHEN 'P2P' THEN 'No purchase schedule carries that number.'
                                       ELSE 'No execution kit carries that reference.' END;
    END
    IF @steps = 0
    BEGIN
        SET @broken = 1; SET @reason = 'XMD0016'; SET @message = 'Nothing is recorded for that reference.';
    END
    ELSE
    BEGIN
        IF @oper > 1 AND @fin > 0 SET @complete = 1;
        IF @fin = 0
        BEGIN
            SET @broken = 1; SET @reason = 'XMD0004'; SET @message = 'The chain has no posted financial effect yet.';
        END
        IF @trunc = 1
        BEGIN
            SET @reason = 'XMD0005'; SET @message = 'More steps exist than the enquiry can return.';
        END
    END

    /*  A caller that reads the result sets one at a time (INSERT ... EXEC) asks for one of them. */
    IF @SummaryOnly = 0
        SELECT TOP (@MaxSteps)
               ROW_NUMBER() OVER (ORDER BY s.SortGroup, s.SortDate, s.SortRef, s.RowId) AS StepNo,
               s.Stage, s.StepKind, s.DocumentType, s.Reference, s.StepDate, s.Status, s.Quantity, s.AmountGBP, s.Note
          FROM @s s
         ORDER BY s.SortGroup, s.SortDate, s.SortRef, s.RowId;

    IF @StepsOnly = 1 RETURN;

    SELECT @CompanyCode AS CompanyCode, @DocType AS DocumentType, @Reference AS Reference, @chainType AS ChainType,
           @chainName AS ChainName, @party AS Party, @steps AS StepCount, ISNULL(@oper, 0) AS OperationalSteps,
           ISNULL(@fin, 0) AS FinancialSteps, ISNULL(@rev, 0) AS Reversals, @total AS TotalGBP,
           @complete AS IsComplete, @broken AS IsBroken, @trunc AS IsTruncated, @reason AS ReasonCode, @message AS Message;
END
GO
