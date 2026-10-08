/*  inv.usp_ReserveStock - the reservation primitive.  A reservation is demand specific and
    quantity bounded; it is not consumption, so the physical stock stays where it is until an
    issue or a despatch moves it.  The free quantity is recomputed under UPDLOCK, HOLDLOCK
    inside the transaction so that two users cannot both reserve the last unit (COM-005).
    Called by the COBOL service STKRESV, which owns the messages the user sees.
    REQ: ITM-014, ITM-013, COM-005, COM-016 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE inv.usp_ReserveStock
    @CompanyCode varchar(10), @ItemCode varchar(40), @Action varchar(10), @DemandType varchar(12),
    @DemandRef varchar(40), @UserCode varchar(20), @SiteCode varchar(10) = NULL, @DemandLine int = NULL,
    @Revision varchar(4) = NULL, @WarehouseCode varchar(20) = NULL, @LocationCode varchar(20) = NULL,
    @LotCode varchar(30) = NULL, @SerialNo varchar(40) = NULL, @Quantity decimal(19,6) = 0,
    @AllowPartial bit = 0, @Firmness varchar(10) = 'FIRM', @RequiredDate date = NULL,
    @Priority tinyint = 5, @AsOf date = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @AsOf = ISNULL(@AsOf, CAST(SYSDATETIME() AS date));
    IF NOT EXISTS (SELECT 1 FROM inv.Item WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode)
        THROW 50025, 'STERP-E-0025: That item does not exist in this company.', 1;
    IF @DemandRef IS NULL OR @DemandRef = ''
        THROW 50015, 'STERP-E-0015: A reservation must name the demand it is held for.', 1;
    DECLARE @eligible decimal(19,6) = 0, @reserved decimal(19,6) = 0, @own decimal(19,6) = 0,
            @free decimal(19,6) = 0, @take decimal(19,6) = 0, @id bigint = NULL, @invalid int = 0,
            @existing decimal(19,6) = 0, @consumed decimal(19,6) = 0, @minlife int;
    DECLARE @invalidated TABLE (DemandType varchar(12), DemandRef varchar(40), DemandLine int, OwnerUser varchar(20));
    SELECT @minlife = ISNULL(MinRemainingShelfLifeDays, 0) FROM inv.Item WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode;
    BEGIN TRANSACTION;
    BEGIN TRY
        /*  eligible = owned, accepted, in an available location, not expired and still inside the
            minimum remaining shelf life, restricted to the scope the demand asked for.          */
        SELECT @eligible = ISNULL(SUM(b.QtyOnHand), 0)
          FROM inv.StockBalance b WITH (UPDLOCK, HOLDLOCK)
          JOIN inv.Location l ON l.CompanyCode = b.CompanyCode AND l.SiteCode = b.SiteCode
                             AND l.WarehouseCode = b.WarehouseCode AND l.LocationCode = b.LocationCode
          LEFT JOIN inv.Lot lt ON lt.CompanyCode = b.CompanyCode AND lt.ItemCode = b.ItemCode AND lt.LotCode = b.LotCode AND b.LotCode <> ''
         WHERE b.CompanyCode = @CompanyCode AND b.ItemCode = @ItemCode AND b.OwnerType = 'OWNED'
           AND b.QualityStatus = 'ACCEPTED' AND l.IsAvailable = 1 AND l.IsOwnedStock = 1
           AND (@SiteCode IS NULL OR b.SiteCode = @SiteCode)
           AND (@WarehouseCode IS NULL OR b.WarehouseCode = @WarehouseCode)
           AND (@LocationCode IS NULL OR b.LocationCode = @LocationCode)
           AND (@LotCode IS NULL OR @LotCode = '' OR b.LotCode = @LotCode)
           AND (@SerialNo IS NULL OR @SerialNo = '' OR b.SerialNo = @SerialNo)
           AND (@Revision IS NULL OR @Revision = '' OR b.Revision = @Revision)
           AND (lt.ExpiryDate IS NULL OR DATEADD(day, @minlife, @AsOf) <= lt.ExpiryDate)
           AND (lt.QualityStatus IS NULL OR lt.QualityStatus = 'ACCEPTED');
        SELECT @reserved = ISNULL(SUM(r.QtyReserved - r.QtyConsumed), 0),
               @own = ISNULL(SUM(CASE WHEN r.DemandType = @DemandType AND r.DemandRef = @DemandRef
                                       AND (@DemandLine IS NULL OR r.DemandLine = @DemandLine)
                                      THEN r.QtyReserved - r.QtyConsumed ELSE 0 END), 0)
          FROM inv.StockReservation r WITH (UPDLOCK, HOLDLOCK)
         WHERE r.CompanyCode = @CompanyCode AND r.ItemCode = @ItemCode AND r.Status = 'ACTIVE'
           AND (@SiteCode IS NULL OR r.SiteCode = @SiteCode);
        SET @free = @eligible - @reserved;
        SELECT @id = ReservationId, @existing = QtyReserved, @consumed = QtyConsumed
          FROM inv.StockReservation WITH (UPDLOCK, HOLDLOCK)
         WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode AND DemandType = @DemandType
           AND DemandRef = @DemandRef AND ISNULL(DemandLine, -1) = ISNULL(@DemandLine, -1) AND Status = 'ACTIVE';
        IF @Action = 'RESERVE'
        BEGIN
            IF @Quantity <= 0 THROW 50015, 'STERP-E-0015: A reservation must be for a positive quantity.', 1;
            SET @take = @Quantity;
            IF @take > @free
            BEGIN
                IF @AllowPartial = 0
                    THROW 50015, 'STERP-E-0015: There is not enough free stock to reserve that quantity for this demand.', 1;
                SET @take = CASE WHEN @free > 0 THEN @free ELSE 0 END;
            END
            IF @id IS NULL
            BEGIN
                INSERT inv.StockReservation (CompanyCode, SiteCode, ItemCode, Revision, WarehouseCode, LocationCode,
                       LotCode, SerialNo, DemandType, DemandRef, DemandLine, QtyReserved, Firmness, RequiredDate,
                       Priority, OwnerUser, Status, CreatedBy)
                VALUES (@CompanyCode, ISNULL(@SiteCode, ''), @ItemCode, @Revision, @WarehouseCode, @LocationCode,
                       @LotCode, @SerialNo, @DemandType, @DemandRef, @DemandLine, @take, @Firmness, @RequiredDate,
                       @Priority, @UserCode, 'ACTIVE', @UserCode);
                SET @id = CAST(SCOPE_IDENTITY() AS bigint);
            END
            ELSE UPDATE inv.StockReservation SET QtyReserved = QtyReserved + @take, ModifiedBy = @UserCode,
                        ModifiedAt = SYSDATETIME() WHERE ReservationId = @id;
        END
        ELSE IF @Action = 'RELEASE'
        BEGIN
            UPDATE inv.StockReservation SET Status = 'RELEASED', ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME()
             WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode AND DemandType = @DemandType
               AND DemandRef = @DemandRef AND ISNULL(DemandLine, -1) = ISNULL(@DemandLine, -1) AND Status = 'ACTIVE';
            SET @take = 0;
        END
        ELSE IF @Action = 'CONSUME'
        BEGIN
            IF @id IS NULL THROW 50015, 'STERP-E-0015: No active reservation was found for that demand.', 1;
            IF @consumed + @Quantity > @existing + 0.000001
                THROW 50015, 'STERP-E-0015: The quantity issued is more than this demand reserved.', 1;
            UPDATE inv.StockReservation SET QtyConsumed = QtyConsumed + @Quantity, QtyPicked = QtyPicked + @Quantity,
                   Status = CASE WHEN QtyConsumed + @Quantity >= QtyReserved - 0.000001 THEN 'CONSUMED' ELSE 'ACTIVE' END,
                   ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME() WHERE ReservationId = @id;
            SET @take = @Quantity;
        END
        ELSE IF @Action = 'REVALID'
        BEGIN
            /*  A change that invalidates a reservation creates an exception for the demand owner;
                it never silently permits the use of blocked, quarantined or expired material.
                One exception per invalidated demand, naming that demand and its own owner, so a
                bulk revalidation (@DemandRef = '*') still tells every affected owner which
                document to reallocate (COM-016, COM-017).                                      */
            UPDATE r SET Status = 'INVALID', InvalidReason = 'The reserved material is no longer eligible: check condition, expiry and item status.',
                   ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME()
              OUTPUT inserted.DemandType, inserted.DemandRef, inserted.DemandLine, inserted.OwnerUser INTO @invalidated
              FROM inv.StockReservation r
              JOIN inv.Item i ON i.CompanyCode = r.CompanyCode AND i.ItemCode = r.ItemCode
             WHERE r.CompanyCode = @CompanyCode AND r.ItemCode = @ItemCode AND r.Status = 'ACTIVE'
               AND (@DemandRef = '*' OR (r.DemandType = @DemandType AND r.DemandRef = @DemandRef))
               AND (i.LifecycleStatus IN ('BLOCKED', 'RETIRED')
                 OR EXISTS (SELECT 1 FROM inv.Lot lt WHERE lt.CompanyCode = r.CompanyCode AND lt.ItemCode = r.ItemCode
                             AND lt.LotCode = r.LotCode AND r.LotCode IS NOT NULL AND r.LotCode <> ''
                             AND (lt.QualityStatus <> 'ACCEPTED' OR (lt.ExpiryDate IS NOT NULL AND lt.ExpiryDate < DATEADD(day, @minlife, @AsOf)))));
            SET @invalid = @@ROWCOUNT;
            IF @invalid > 0
                INSERT core.ExceptionItem (CompanyCode, Module, Severity, Category, DocumentType, DocumentRef, Message,
                       OwnerUser, Priority, Status)
                SELECT @CompanyCode, 'ITM', 'ERROR', 'RESERVATION', v.DemandType, v.DemandRef,
                       'A reservation for ' + @ItemCode + ' is no longer valid and the demand must be reallocated.',
                       ISNULL(v.OwnerUser, @UserCode), 2, 'OPEN'
                  FROM @invalidated v;
        END
        ELSE THROW 50015, 'STERP-E-0015: That reservation action is not supported.', 1;
        COMMIT TRANSACTION;
    END TRY
    BEGIN CATCH
        IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
        THROW;
    END CATCH
    SELECT @Action AS Action, ISNULL(@id, 0) AS ReservationId, @take AS QtyReserved,
           CASE WHEN @Action = 'RESERVE' THEN @Quantity - @take ELSE 0 END AS QtyShortfall,
           @free AS QtyFreeBefore,
           CASE WHEN @Action = 'RESERVE' THEN @free - @take ELSE @free END AS QtyFreeAfter,
           @eligible AS QtyEligible, @reserved AS QtyReservedTotal, @own AS QtyOwnDemand, @invalid AS InvalidCount;
END
GO
