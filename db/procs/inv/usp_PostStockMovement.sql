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
    @ReasonCode varchar(20) = NULL, @Reason varchar(200) = NULL, @ApprovalRef varchar(40) = NULL, @CorrelationId varchar(64) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @Revision = ISNULL(@Revision, ''); SET @LotCode = ISNULL(@LotCode, ''); SET @SerialNo = ISNULL(@SerialNo, ''); SET @OwnerCode = ISNULL(@OwnerCode, '');
    SET @PostingDate = ISNULL(@PostingDate, @MovementDate);
    IF @CorrelationId IS NOT NULL AND EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId AND Status = 'COMPLETED')
    BEGIN
        SELECT m.MovementId, m.CompanyCode, m.ItemCode, m.Quantity, b.QtyOnHand AS QtyOnHandAfter, b.StockBalanceId, CAST(1 AS bit) AS WasDuplicate
          FROM inv.StockMovement m LEFT JOIN inv.StockBalance b ON b.CompanyCode = m.CompanyCode AND b.SiteCode = m.SiteCode AND b.WarehouseCode = m.WarehouseCode
               AND b.LocationCode = m.LocationCode AND b.ItemCode = m.ItemCode AND b.Revision = m.Revision AND b.LotCode = m.LotCode AND b.SerialNo = m.SerialNo
               AND b.OwnerType = m.OwnerType AND b.OwnerCode = m.OwnerCode AND b.QualityStatus = m.QualityStatus
         WHERE m.CorrelationId = @CorrelationId;
        RETURN;
    END
    IF @Quantity IS NULL OR @Quantity <= 0 THROW 50015, 'STERP-E-0015: A stock movement must state a positive quantity; the movement type gives its direction.', 1;
    IF @Direction IS NULL SET @Direction = CASE WHEN @MovementType IN ('RECEIPT','RETURNIN','TRANSFERIN','ADJUSTUP','COMPLETION','OPENING','DISASSEMBLY') THEN 'I'
                                                WHEN @MovementType IN ('ISSUE','DESPATCH','TRANSFEROUT','ADJUSTDOWN','SCRAP','SUPPRETURN') THEN 'O' END;
    IF @Direction NOT IN ('I', 'O') THROW 50015, 'STERP-E-0015: The direction of this movement type is not defined. Supply I for an increase or O for a decrease.', 1;
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
    BEGIN TRY
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
                @signed, @UomCode, @EnteredQuantity, @ConversionFactor, @UnitCost, CASE WHEN @UnitCost IS NULL THEN NULL ELSE ROUND(@signed * @UnitCost, 2) END,
                @MovementDate, @PostingDate, @fy, @pn, @SourceModule, @SourceDocType, @SourceDocRef, @SourceDocLine, @CounterpartLocationCode, @ReasonCode, @Reason, @ApprovalRef, @CorrelationId, @UserCode);
        SET @mid = CAST(SCOPE_IDENTITY() AS bigint);
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
        THROW;
    END CATCH
    SELECT @mid AS MovementId, @CompanyCode AS CompanyCode, @ItemCode AS ItemCode, @signed AS Quantity, @before AS QtyOnHandBefore, @after AS QtyOnHandAfter,
           @bid AS StockBalanceId, @QualityStatus AS QualityStatus, @OwnerType AS OwnerType, CAST(0 AS bit) AS WasDuplicate;
END
GO
