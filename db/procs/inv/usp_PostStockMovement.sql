/*  inv.usp_PostStockMovement - the single stock contract.  A displayed balance is never edited:
    every change is an authorised movement with its source, reason, ownership, traceability and
    quality status.  A movement can never create negative physical stock (D-09).
    REQ: ITM-011, ITM-012, ITM-015, ITM-016, ITM-017, COM-009, COM-015, XMD-012 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE inv.usp_PostStockMovement @CompanyCode varchar(10), @SiteCode varchar(10), @WarehouseCode varchar(20), @LocationCode varchar(20),
    @ItemCode varchar(40), @MovementType varchar(14), @Quantity decimal(19,6), @MovementDate date, @UserCode varchar(20),
    @Revision varchar(4) = '', @LotCode varchar(30) = '', @SerialNo varchar(40) = '', @OwnerType varchar(10) = 'OWNED', @OwnerCode varchar(20) = '',
    @QualityStatus varchar(12) = 'ACCEPTED', @Direction char(1) = NULL, @UomCode varchar(10) = NULL, @EnteredQuantity decimal(19,6) = NULL,
    @ConversionFactor decimal(19,9) = NULL, @UnitCost decimal(19,6) = NULL, @PostingDate date = NULL, @SourceModule varchar(6) = 'ITM',
    @SourceDocType varchar(20) = NULL, @SourceDocRef varchar(40) = NULL, @SourceDocLine int = NULL, @CounterpartLocationCode varchar(20) = NULL,
    @ReasonCode varchar(20) = NULL, @Reason varchar(200) = NULL, @ApprovalRef varchar(40) = NULL, @CorrelationId varchar(64) = NULL,
    @ValueGBP decimal(18,2) = NULL, @CounterpartSiteCode varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @Revision = ISNULL(@Revision, ''); SET @LotCode = ISNULL(@LotCode, ''); SET @SerialNo = ISNULL(@SerialNo, ''); SET @OwnerCode = ISNULL(@OwnerCode, '');
    SET @PostingDate = ISNULL(@PostingDate, @MovementDate);
    IF @Quantity IS NULL OR @Quantity <= 0 THROW 50015, 'STERP-E-0015: A stock movement must state a positive quantity; the movement type gives its direction.', 1;
    IF @Direction IS NULL SET @Direction = CASE WHEN @MovementType IN ('RECEIPT','RETURNIN','TRANSFERIN','ADJUSTUP','COMPLETION','OPENING','DISASSEMBLY') THEN 'I'
                                                WHEN @MovementType IN ('ISSUE','DESPATCH','TRANSFEROUT','ADJUSTDOWN','SCRAP','SUPPRETURN') THEN 'O' END;
    IF @Direction IS NULL OR @Direction NOT IN ('I', 'O') THROW 50015, 'STERP-E-0015: The direction of this movement type is not defined. Supply I for an increase or O for a decrease.', 1;
    DECLARE @trace varchar(8), @quantum decimal(19,6), @indiv bit, @life varchar(10);
    SELECT @trace = TraceabilityType, @quantum = QuantumQty, @indiv = IsIndivisible, @life = LifecycleStatus FROM inv.Item WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode;
    IF @trace IS NULL THROW 50025, 'STERP-E-0025: That item does not exist in this company.', 1;
    IF @life IN ('DRAFT', 'REVIEW') THROW 50025, 'STERP-E-0025: A draft item cannot be used for a stock movement until it has been reviewed and released.', 1;
    IF @life = 'RETIRED' THROW 50025, 'STERP-E-0025: A retired item cannot receive new stock movements.', 1;
    IF NOT EXISTS (SELECT 1 FROM inv.Location WHERE CompanyCode = @CompanyCode AND SiteCode = @SiteCode AND WarehouseCode = @WarehouseCode AND LocationCode = @LocationCode AND IsActive = 1)
        THROW 50026, 'STERP-E-0026: That stock location is unknown or no longer active.', 1;
    IF @trace = 'LOT' AND @LotCode = '' THROW 50027, 'STERP-E-0027: This item is lot controlled, so a lot reference is required on every movement.', 1;
    IF @trace = 'SERIAL' AND @SerialNo = '' THROW 50027, 'STERP-E-0027: This item is serial controlled, so a serial number is required on every movement.', 1;
    IF @indiv = 1 AND @quantum > 0 AND ABS(@Quantity / @quantum - ROUND(@Quantity / @quantum, 0)) > 0.000001
        THROW 50028, 'STERP-E-0028: The quantity does not respect the item''s permitted quantity increment.', 1;
    DECLARE @fy smallint, @pn tinyint, @st varchar(8);
    SELECT @fy = FiscalYear, @pn = PeriodNo, @st = Status FROM fin.Period WHERE CompanyCode = @CompanyCode AND @PostingDate BETWEEN StartDate AND EndDate;
    IF @st IS NULL THROW 50009, 'STERP-E-0009: No financial period covers that stock posting date.', 1;
    IF @st IN ('HARD', 'FUTURE') THROW 50009, 'STERP-E-0009: That accounting period is closed, so the stock movement cannot be posted into it.', 1;
    DECLARE @signed decimal(19,6) = CASE WHEN @Direction = 'O' THEN -@Quantity ELSE @Quantity END;
    DECLARE @bid int, @before decimal(19,6), @after decimal(19,6), @mid bigint, @own bit = 0;
    IF @@TRANCOUNT = 0 BEGIN BEGIN TRANSACTION; SET @own = 1; END
    ELSE SAVE TRANSACTION StockMovement;
    BEGIN TRY
        DECLARE @lock int, @resource nvarchar(255);
        IF @CorrelationId IS NOT NULL
        BEGIN
            SET @resource = N'STK-CORR:' + @CorrelationId;
            EXEC @lock = sys.sp_getapplock @Resource = @resource, @LockMode = 'Exclusive', @LockOwner = 'Transaction', @LockTimeout = 30000;
            IF @lock < 0 THROW 50015, 'STERP-E-0015: Stock replay lock could not be acquired; retry the instruction.', 1;
            IF EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId AND Status = 'COMPLETED')
            BEGIN
                SELECT m.MovementId, m.CompanyCode, m.ItemCode, m.Quantity, b.QtyOnHand - m.Quantity AS QtyOnHandBefore,
                       b.QtyOnHand AS QtyOnHandAfter, b.StockBalanceId, m.QualityStatus, m.OwnerType, CAST(1 AS bit) AS WasDuplicate,
                       m.ValueGBP, m.UnitCost
                  FROM inv.StockMovement m LEFT JOIN inv.StockBalance b ON b.CompanyCode = m.CompanyCode AND b.SiteCode = m.SiteCode
                   AND b.WarehouseCode = m.WarehouseCode AND b.LocationCode = m.LocationCode AND b.ItemCode = m.ItemCode
                   AND b.Revision = m.Revision AND b.LotCode = m.LotCode AND b.SerialNo = m.SerialNo
                   AND b.OwnerType = m.OwnerType AND b.OwnerCode = m.OwnerCode AND b.QualityStatus = m.QualityStatus
                 WHERE m.CorrelationId = @CorrelationId AND m.CompanyCode = @CompanyCode;
                IF @own = 1 COMMIT TRANSACTION;
                RETURN;
            END
        END
        SET @resource = N'STK-POOL:' + @CompanyCode + ':' + @ItemCode;
        EXEC @lock = sys.sp_getapplock @Resource = @resource, @LockMode = 'Exclusive', @LockOwner = 'Transaction', @LockTimeout = 30000;
        IF @lock < 0 THROW 50015, 'STERP-E-0015: Stock valuation lock could not be acquired; retry the instruction.', 1;
        IF @ValueGBP < 0 OR @UnitCost < 0 THROW 50031, 'STERP-E-0031: Stock valuation evidence must be non-negative; direction supplies the sign.', 1;
        IF @SourceDocType = 'RECEIPTREVERSAL' AND @Direction = 'O'
        BEGIN
            DECLARE @originalQty decimal(19,6), @originalValue decimal(18,2);
            SELECT @originalQty = SUM(Quantity), @originalValue = SUM(ValueGBP) FROM inv.StockMovement
             WHERE CompanyCode = @CompanyCode AND SiteCode = @SiteCode AND ItemCode = @ItemCode AND Revision = @Revision
               AND SourceDocRef = @SourceDocRef AND (@SourceDocLine IS NULL OR SourceDocLine = @SourceDocLine)
               AND MovementType = 'RECEIPT' AND Quantity > 0 AND LotCode = @LotCode AND SerialNo = @SerialNo
               AND OwnerType = @OwnerType AND OwnerCode = @OwnerCode;
            IF @originalQty > 0 AND @originalValue IS NOT NULL
            BEGIN
                SET @ValueGBP = ROUND(@originalValue * (@Quantity / @originalQty), 2);
                SET @UnitCost = @originalValue / @originalQty;
            END
        END
        DECLARE @method varchar(4), @group varchar(20), @ps varchar(10) = @SiteCode, @pr varchar(4) = @Revision,
                @bySite bit = 1, @byRev bit = 1, @policy bit = 0, @pid int, @pq decimal(19,6) = 0,
                @pv decimal(18,2) = 0, @pa decimal(19,6) = 0, @std decimal(19,6), @allow decimal(18,2) = 0,
                @nrv decimal(19,6), @vq decimal(19,6), @vv decimal(18,2), @newq decimal(19,6),
                @newv decimal(18,2), @newa decimal(19,6), @valued bit = 0, @neutral bit = 0;
        SELECT @method = CostMethod, @group = ValuationGroup FROM inv.Item WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode;
        SELECT TOP (1) @method = CostMethod, @bySite = PoolBySite, @byRev = PoolByRevision, @policy = 1
          FROM cst.CostPolicy WHERE CompanyCode = @CompanyCode AND ValuationGroup = @group
           AND EffectiveFrom <= @PostingDate AND (EffectiveTo IS NULL OR EffectiveTo >= @PostingDate)
           AND ApprovedBy IS NOT NULL ORDER BY EffectiveFrom DESC;
        IF @bySite = 0 SET @ps = '*';
        IF @byRev = 0 SET @pr = '';
        /* Policy defines new pools; it must not silently migrate an established pool or
           strand its carrying value. Resolve the same exact/wildcard cost the callers use. */
        SELECT TOP (1) @ps = SiteCode, @pr = Revision FROM cst.ItemCost WITH (UPDLOCK, HOLDLOCK)
         WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode AND SiteCode IN (@SiteCode, '*') AND Revision IN (@Revision, '')
         ORDER BY CASE WHEN SiteCode = @SiteCode THEN 0 ELSE 1 END, CASE WHEN Revision = @Revision THEN 0 ELSE 1 END;
        SELECT @pid = ItemCostId, @method = CostMethod, @pq = QtyOnHand, @pv = CarryingValueGBP, @pa = AverageUnitCost,
               @std = StandardUnitCost, @allow = AllowanceGBP, @nrv = NrvUnitValue
          FROM cst.ItemCost WITH (UPDLOCK, HOLDLOCK)
         WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode AND SiteCode = @ps AND Revision = @pr;
        IF @pid IS NULL AND @method = 'STD'
            SELECT TOP (1) @std = sc.TotalUnitCost FROM cst.StandardCost sc JOIN cst.CostSet cs
              ON cs.CompanyCode = sc.CompanyCode AND cs.CostSetCode = sc.CostSetCode
             WHERE sc.CompanyCode = @CompanyCode AND sc.ItemCode = @ItemCode AND sc.Revision = @pr
               AND cs.Status = 'ACTIVE' AND cs.EffectiveFrom <= @PostingDate
             ORDER BY cs.EffectiveFrom DESC, cs.CostSetCode;
        IF @MovementType IN ('QUALITY', 'HOLD', 'RELEASE') SET @neutral = 1;
        IF @MovementType IN ('TRANSFERIN', 'TRANSFEROUT')
        BEGIN
            DECLARE @cpSite varchar(10) = ISNULL(@CounterpartSiteCode, @SiteCode),
                    @cpPoolSite varchar(10), @cpPoolRev varchar(4);
            SET @cpPoolSite = CASE WHEN @bySite = 0 THEN '*' ELSE @cpSite END;
            SET @cpPoolRev = CASE WHEN @byRev = 0 THEN '' ELSE @Revision END;
            SELECT TOP (1) @cpPoolSite = SiteCode, @cpPoolRev = Revision FROM cst.ItemCost WITH (UPDLOCK, HOLDLOCK)
             WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode AND SiteCode IN (@cpSite, '*') AND Revision IN (@Revision, '')
             ORDER BY CASE WHEN SiteCode = @cpSite THEN 0 ELSE 1 END, CASE WHEN Revision = @Revision THEN 0 ELSE 1 END;
            IF @ps = @cpPoolSite AND @pr = @cpPoolRev AND NOT EXISTS
               (SELECT 1 FROM inv.Location WHERE CompanyCode = @CompanyCode AND SiteCode = @cpSite
                 AND LocationCode = @CounterpartLocationCode AND IsOwnedStock = 0)
                SET @neutral = 1;
        END
        IF @OwnerType = 'OWNED' AND @neutral = 0 AND EXISTS
           (SELECT 1 FROM inv.Location WHERE CompanyCode = @CompanyCode AND SiteCode = @SiteCode
            AND WarehouseCode = @WarehouseCode AND LocationCode = @LocationCode AND IsOwnedStock = 1)
           AND NOT (@MovementType = 'OPENING' AND @ValueGBP IS NULL AND @UnitCost IS NULL)
        BEGIN
            SET @valued = 1;
            SET @vq = CASE WHEN @Direction = 'O' AND @Quantity > @pq THEN @pq ELSE @Quantity END;
            SET @vv = COALESCE(@ValueGBP, ROUND(@vq * COALESCE(@UnitCost,
                      CASE WHEN @method = 'STD' THEN COALESCE(@std, @pa) ELSE @pa END, 0), 2));
            IF @Direction = 'O' AND @method = 'FIFO'
            BEGIN
                DECLARE @left decimal(19,6) = @vq, @lid bigint, @lq decimal(19,6), @lv decimal(18,2),
                        @take decimal(19,6), @cost decimal(18,2);
                SET @vv = 0;
                WHILE @left > 0
                BEGIN
                    SET @lid = NULL;
                    SELECT TOP (1) @lid = LayerId, @lq = QtyRemaining, @lv = ValueRemainingGBP
                      FROM cst.CostLayer WITH (UPDLOCK, HOLDLOCK)
                     WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode AND Revision = @pr AND SiteCode = @ps
                       AND IsClosed = 0 AND QtyRemaining > 0
                       AND (@SourceDocType <> 'RECEIPTREVERSAL' OR @SourceDocType IS NULL OR SourceDocRef = @SourceDocRef)
                     ORDER BY ReceiptDate, LayerSeq, LayerId;
                    IF @lid IS NULL BREAK;
                    SET @take = CASE WHEN @left < @lq THEN @left ELSE @lq END;
                    SET @cost = CASE WHEN @take = @lq THEN @lv ELSE ROUND(@lv * (@take / @lq), 2) END;
                    UPDATE cst.CostLayer SET QtyRemaining = @lq - @take, ValueRemainingGBP = @lv - @cost,
                           IsClosed = CASE WHEN @take = @lq THEN 1 ELSE 0 END WHERE LayerId = @lid;
                    SET @vv += @cost;
                    SET @left -= @take;
                END
                SET @vq -= @left;
            END
            IF @Direction = 'O'
            BEGIN
                IF @vq = @pq SET @vv = @pv;
                IF @vv > @pv SET @vv = @pv;
                IF @Quantity > @vq
                    INSERT core.ExceptionItem (CompanyCode, SiteCode, Module, Severity, Category, DocumentType, DocumentRef,
                        Message, Quantity, RaisedBy, CorrelationId)
                    VALUES (@CompanyCode, @SiteCode, 'CST', 'WARNING', 'VALUATION SHORTFALL', @SourceDocType,
                        COALESCE(@SourceDocRef, @ItemCode), 'Physical issue exceeds valued stock; investigate the unvalued quantity.', @Quantity - @vq, @UserCode, @CorrelationId);
                SET @vq = -@vq; SET @vv = -@vv;
            END
            SET @newq = @pq + @vq; SET @newv = @pv + @vv;
            SET @newa = CASE WHEN @newq = 0 THEN 0 ELSE @newv / @newq END;
            SET @ValueGBP = @vv;
            SET @UnitCost = CASE WHEN @Direction = 'O' THEN ABS(@vv) / @Quantity
                                ELSE COALESCE(@UnitCost, ABS(@vv) / @Quantity) END;
            IF @pid IS NULL
            BEGIN
                INSERT cst.ItemCost (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand, CarryingValueGBP,
                    AverageUnitCost, StandardUnitCost, LastPurchaseCost, LastUpdatedAt, LastUpdatedBy)
                VALUES (@CompanyCode, @ItemCode, @pr, @ps, @method, @newq, @newv, @newa, @std,
                    CASE WHEN @Direction = 'I' THEN @UnitCost END, SYSDATETIME(), @UserCode);
                SET @pid = SCOPE_IDENTITY();
            END
            ELSE UPDATE cst.ItemCost SET QtyOnHand = @newq, CarryingValueGBP = @newv, AverageUnitCost = @newa,
                LastPurchaseCost = CASE WHEN @MovementType = 'RECEIPT' THEN @UnitCost ELSE LastPurchaseCost END,
                AllowanceGBP = CASE WHEN @newq = 0 THEN 0
                    WHEN @nrv IS NOT NULL THEN CASE WHEN @newv > ROUND(@newq * @nrv, 2) THEN @newv - ROUND(@newq * @nrv, 2) ELSE 0 END
                    WHEN @Direction = 'O' AND @pq > 0 THEN ROUND(@allow * (@newq / @pq), 2) ELSE @allow END,
                LastUpdatedAt = SYSDATETIME(), LastUpdatedBy = @UserCode WHERE ItemCostId = @pid;
        END
        ELSE
        BEGIN
            SET @method = NULL;
            SET @ValueGBP = CASE WHEN @ValueGBP IS NOT NULL THEN @signed / @Quantity * @ValueGBP
                                WHEN @UnitCost IS NOT NULL THEN ROUND(@signed * @UnitCost, 2) END;
        END
        SELECT @bid = StockBalanceId, @before = QtyOnHand FROM inv.StockBalance WITH (UPDLOCK, HOLDLOCK)
         WHERE CompanyCode = @CompanyCode AND SiteCode = @SiteCode AND WarehouseCode = @WarehouseCode AND LocationCode = @LocationCode AND ItemCode = @ItemCode
           AND Revision = @Revision AND LotCode = @LotCode AND SerialNo = @SerialNo AND OwnerType = @OwnerType AND OwnerCode = @OwnerCode AND QualityStatus = @QualityStatus;
        SET @before = ISNULL(@before, 0);
        SET @after = @before + @signed;
        IF @after < 0
            THROW 50015, 'STERP-E-0015: Insufficient physical stock. A movement may not create a negative balance; correct the quantity or receive stock first.', 1;
        IF @bid IS NULL
        BEGIN
            INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo, OwnerType, OwnerCode, QualityStatus, QtyOnHand, LastMovementAt)
            VALUES (@CompanyCode, @SiteCode, @WarehouseCode, @LocationCode, @ItemCode, @Revision, @LotCode, @SerialNo, @OwnerType, @OwnerCode, @QualityStatus, @after, SYSDATETIME());
            SET @bid = CAST(SCOPE_IDENTITY() AS int);
        END
        ELSE UPDATE inv.StockBalance SET QtyOnHand = @after, LastMovementAt = SYSDATETIME() WHERE StockBalanceId = @bid;
        INSERT inv.StockMovement (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo, OwnerType, OwnerCode, QualityStatus, MovementType,
                                  Quantity, UomCode, EnteredQuantity, ConversionFactor, UnitCost, ValueGBP, MovementDate, PostingDate, FiscalYear, PeriodNo, SourceModule,
                                  SourceDocType, SourceDocRef, SourceDocLine, CounterpartLocationCode, ReasonCode, Reason, ApprovalRef, CorrelationId, CreatedBy)
        VALUES (@CompanyCode, @SiteCode, @WarehouseCode, @LocationCode, @ItemCode, @Revision, @LotCode, @SerialNo, @OwnerType, @OwnerCode, @QualityStatus, @MovementType,
                @signed, @UomCode, @EnteredQuantity, @ConversionFactor, @UnitCost, @ValueGBP,
                @MovementDate, @PostingDate, @fy, @pn, @SourceModule, @SourceDocType, @SourceDocRef, @SourceDocLine, @CounterpartLocationCode, @ReasonCode, @Reason, @ApprovalRef, @CorrelationId, @UserCode);
        SET @mid = CAST(SCOPE_IDENTITY() AS bigint);
        UPDATE inv.StockMovement SET CostMethod = @method WHERE MovementId = @mid;
        IF @valued = 1
        BEGIN
            INSERT cst.ItemCostHistory (CompanyCode, ItemCode, Revision, SiteCode, MovementId, ChangeType,
                QtyBefore, ValueBeforeGBP, AvgBefore, QtyChange, ValueChangeGBP, QtyAfter, ValueAfterGBP, AvgAfter,
                SourceDocType, SourceDocRef, CreatedBy)
            VALUES (@CompanyCode, @ItemCode, @pr, @ps, @mid, CASE WHEN @MovementType = 'OPENING' THEN 'OPENING'
                    WHEN @Direction = 'I' THEN 'RECEIPT' ELSE 'ISSUE' END,
                @pq, @pv, @pa, @vq, @vv, @newq, @newv, @newa, @SourceDocType, @SourceDocRef, @UserCode);
            IF @method = 'FIFO' AND @Direction = 'I'
            BEGIN
                DECLARE @seq bigint;
                SELECT @seq = ISNULL(MAX(LayerSeq), 0) + 1 FROM cst.CostLayer WITH (UPDLOCK, HOLDLOCK)
                 WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode AND Revision = @pr;
                INSERT cst.CostLayer (CompanyCode, ItemCode, Revision, SiteCode, LayerSeq, ReceiptDate, SourceDocType,
                    SourceDocRef, MovementId, LotCode, QtyOriginal, QtyRemaining, UnitCost, ValueRemainingGBP)
                VALUES (@CompanyCode, @ItemCode, @pr, @ps, @seq, @MovementDate, @SourceDocType,
                    @SourceDocRef, @mid, @LotCode, @Quantity, @Quantity, @vv / @Quantity, @vv);
            END
        END
        IF @CorrelationId IS NOT NULL
        BEGIN
            IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId)
                INSERT core.IdempotencyKey (CorrelationId, Operation, CompanyCode, UserCode, Status) VALUES (@CorrelationId, 'inv.usp_PostStockMovement', @CompanyCode, @UserCode, 'INPROGRESS');
            UPDATE core.IdempotencyKey SET Status = 'COMPLETED', ResultRef = CAST(@mid AS varchar(30)), ResultMessage = 'Stock movement posted', EndedAt = SYSDATETIME() WHERE CorrelationId = @CorrelationId;
        END
        IF @MovementType IN ('ADJUSTUP', 'ADJUSTDOWN', 'SCRAP', 'COUNT')
            INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, FieldName, OldValue, NewValue, Reason, AuthorityRef, Source, CorrelationId)
            VALUES (@UserCode, @CompanyCode, 'inv.StockBalance', @ItemCode + '/' + @SiteCode + '/' + @LocationCode, @MovementType, 'QtyOnHand', CAST(@before AS varchar(30)), CAST(@after AS varchar(30)), @Reason, @ApprovalRef, 'SQL', @CorrelationId);
        IF @own = 1 COMMIT TRANSACTION;
    END TRY
    BEGIN CATCH
        IF @own = 1 AND XACT_STATE() <> 0 ROLLBACK TRANSACTION;
        ELSE IF @own = 0 AND XACT_STATE() = 1 ROLLBACK TRANSACTION StockMovement;
        THROW;
    END CATCH
    SELECT @mid AS MovementId, @CompanyCode AS CompanyCode, @ItemCode AS ItemCode, @signed AS Quantity, @before AS QtyOnHandBefore, @after AS QtyOnHandAfter,
           @bid AS StockBalanceId, @QualityStatus AS QualityStatus, @OwnerType AS OwnerType, CAST(0 AS bit) AS WasDuplicate,
           @ValueGBP AS ValueGBP, @UnitCost AS UnitCost;
END
GO
