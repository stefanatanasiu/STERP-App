SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE po.usp_ReceiptDeliveryState
    @CompanyCode varchar(10), @SiteCode varchar(10), @ReceiptRef varchar(30),
    @TargetStatus varchar(12), @GrniValueGBP decimal(18,2) = 0,
    @JournalCorrelationId varchar(64) = NULL, @ModifiedBy varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SET XACT_ABORT ON;
    IF @TargetStatus IS NULL OR @TargetStatus NOT IN ('CONFIRMED', 'REVERSED')
        THROW 50812, 'STERP-E-0812: Receipt delivery state must be CONFIRMED or REVERSED.', 1;

    BEGIN TRY
        BEGIN TRANSACTION;
        DECLARE @ReceiptId bigint, @CurrentStatus varchar(12), @OrderNumber varchar(30),
                @Sign int = CASE @TargetStatus WHEN 'CONFIRMED' THEN 1 ELSE -1 END;
        SELECT @ReceiptId = ReceiptId, @CurrentStatus = Status, @OrderNumber = PurchaseOrderNumber
          FROM po.Receipt WITH (UPDLOCK, HOLDLOCK)
         WHERE CompanyCode = @CompanyCode AND SiteCode = @SiteCode AND ReceiptRef = @ReceiptRef;
        IF @ReceiptId IS NULL
            THROW 50812, 'STERP-E-0812: Receipt does not belong to this company and site.', 1;
        IF @CurrentStatus = @TargetStatus
        BEGIN
            COMMIT TRANSACTION;
            RETURN;
        END
        IF (@TargetStatus = 'CONFIRMED' AND @CurrentStatus <> 'DRAFT')
            OR (@TargetStatus = 'REVERSED' AND @CurrentStatus <> 'CONFIRMED')
            THROW 50812, 'STERP-E-0812: Receipt is not in a valid state for this transition.', 1;

        IF @TargetStatus = 'CONFIRMED' AND NULLIF(@OrderNumber, '') IS NOT NULL
        BEGIN
            UPDATE r SET PurchaseOrderLineId = p.PurchaseOrderLineId
              FROM po.ReceiptLine r
              JOIN po.PurchaseOrderLine p ON p.CompanyCode = @CompanyCode
                                        AND p.OrderNumber = @OrderNumber
                                        AND p.LineNum = r.PurchaseOrderLine
              JOIN po.PurchaseOrder o ON o.PurchaseOrderId = p.PurchaseOrderId
                                    AND o.SiteCode = @SiteCode
             WHERE r.ReceiptId = @ReceiptId AND r.PurchaseOrderNumber = @OrderNumber
               AND ISNULL(r.ItemCode, '') = ISNULL(p.ItemCode, '');
            IF EXISTS (SELECT 1 FROM po.ReceiptLine
                        WHERE ReceiptId = @ReceiptId AND PurchaseOrderLineId IS NULL)
                THROW 50812, 'STERP-E-0812: Every receipt line must belong to the loaded order and site.', 1;
        END

        -- Linking and applying the quantities share this transaction. Older
        -- unlinked receipts have no applied order quantities to subtract.
        DECLARE @Delivery TABLE (
            PurchaseOrderLineId bigint NOT NULL, ScheduleNo int NULL,
            ReceivedQty decimal(19,6) NOT NULL, AcceptedQty decimal(19,6) NOT NULL);
        INSERT @Delivery
        SELECT PurchaseOrderLineId, ScheduleNo, SUM(ReceivedQty), SUM(AcceptedQty)
          FROM po.ReceiptLine
         WHERE ReceiptId = @ReceiptId AND PurchaseOrderLineId IS NOT NULL
         GROUP BY PurchaseOrderLineId, ScheduleNo;

        UPDATE p SET ReceivedQty = p.ReceivedQty + @Sign * d.ReceivedQty,
                     AcceptedQty = p.AcceptedQty + @Sign * d.AcceptedQty
          FROM po.PurchaseOrderLine p
          JOIN (SELECT PurchaseOrderLineId, SUM(ReceivedQty) AS ReceivedQty,
                       SUM(AcceptedQty) AS AcceptedQty
                  FROM @Delivery GROUP BY PurchaseOrderLineId) d
            ON d.PurchaseOrderLineId = p.PurchaseOrderLineId;
        IF EXISTS (SELECT 1 FROM po.PurchaseOrderLine p JOIN @Delivery d
                    ON d.PurchaseOrderLineId = p.PurchaseOrderLineId
                   WHERE p.ReceivedQty < 0 OR p.AcceptedQty < 0)
            THROW 50812, 'STERP-E-0812: Reversal would make the order delivery quantities negative.', 1;

        UPDATE s SET ReceivedQty = s.ReceivedQty + @Sign * d.ReceivedQty,
                     Status = CASE WHEN s.Status = 'CANCELLED' THEN s.Status
                                   WHEN s.ReceivedQty + @Sign * d.ReceivedQty >= s.Quantity THEN 'COMPLETE'
                                   WHEN s.ReceivedQty + @Sign * d.ReceivedQty > 0 THEN 'PARTIAL'
                                   ELSE 'OPEN' END
          FROM po.PurchaseSchedule s JOIN @Delivery d
            ON d.PurchaseOrderLineId = s.PurchaseOrderLineId AND d.ScheduleNo = s.ScheduleNo;
        IF EXISTS (SELECT 1 FROM po.PurchaseSchedule s JOIN @Delivery d
                    ON d.PurchaseOrderLineId = s.PurchaseOrderLineId AND d.ScheduleNo = s.ScheduleNo
                   WHERE s.ReceivedQty < 0)
            THROW 50812, 'STERP-E-0812: Reversal would make the schedule received quantity negative.', 1;

        UPDATE o SET DeliveryStatus =
            CASE WHEN o.DeliveryStatus = 'SHORTCLOSED' THEN o.DeliveryStatus
                 WHEN NOT EXISTS (SELECT 1 FROM po.PurchaseOrderLine p
                                   WHERE p.PurchaseOrderId = o.PurchaseOrderId
                                     AND p.Status NOT IN ('CANCELLED', 'SHORTCLOSED')
                                     AND p.OrderedQty - p.CancelledQty - p.ReceivedQty > 0)
                      THEN 'FULLRECEIVED'
                 WHEN EXISTS (SELECT 1 FROM po.PurchaseOrderLine p
                               WHERE p.PurchaseOrderId = o.PurchaseOrderId AND p.ReceivedQty > 0)
                      THEN 'PARTRECEIVED' ELSE 'NOTRECEIVED' END,
                     ModifiedBy = @ModifiedBy, ModifiedAt = SYSDATETIME()
          FROM po.PurchaseOrder o
         WHERE EXISTS (SELECT 1 FROM po.PurchaseOrderLine p JOIN @Delivery d
                        ON d.PurchaseOrderLineId = p.PurchaseOrderLineId
                       WHERE p.PurchaseOrderId = o.PurchaseOrderId);

        UPDATE po.Receipt
           SET Status = @TargetStatus,
               GrniValueGBP = CASE @TargetStatus WHEN 'CONFIRMED' THEN @GrniValueGBP ELSE GrniValueGBP END,
               JournalId = CASE @TargetStatus WHEN 'CONFIRMED' THEN
                   (SELECT TOP (1) JournalId FROM fin.Journal
                     WHERE CompanyCode = @CompanyCode AND CorrelationId = @JournalCorrelationId)
                   ELSE JournalId END,
               ModifiedBy = @ModifiedBy, ModifiedAt = SYSDATETIME()
         WHERE ReceiptId = @ReceiptId;
        COMMIT TRANSACTION;
    END TRY
    BEGIN CATCH
        IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
        THROW;
    END CATCH
END
GO
