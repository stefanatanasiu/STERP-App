/* ---------------------------------------------------------------------------
   91-itm-demo - demonstration stock history so that the stock screens and the
   stock reports show real data: an opening load, receipts, issues, a transfer
   that is still in transit, a quality hold and its release, a lot controlled
   item with shelf life, firm reservations against named demand, and a stock
   count with a variance and an approved adjustment.

   Every balance reconciles to its movements: each bucket that the demo touches
   first gets an OPENING movement for the quantity that 65-itm-partners-stock
   left there, then the demo movements, and the balances are moved by exactly
   the net of those movements.  Quantities are signed the way
   inv.usp_PostStockMovement signs them (increase positive, decrease negative).
   The whole script is keyed on the DEMO- document references so it re-runs
   cleanly.            REQ: ITM-012, ITM-013, ITM-014, ITM-015, ITM-016, ITM-017, ITM-018
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'inv.StockMovement') IS NOT NULL
   AND NOT EXISTS (SELECT 1 FROM inv.StockMovement WHERE CompanyCode = 'STC' AND SourceDocRef LIKE 'DEMO-%')
BEGIN
    /*  The cast of items: three ordinary parts at Reading, the lot controlled adhesive with its
        shelf life, and the part that is transferred to Birmingham.  Everything is resolved from
        the seeded masters so the script survives a change of the item range.                  */
    DECLARE @lotItem varchar(40), @lotCode varchar(30), @lotLoc varchar(20), @lotExpiry date;
    SELECT TOP (1) @lotItem = l.ItemCode, @lotCode = l.LotCode, @lotExpiry = l.ExpiryDate,
           @lotLoc = b.LocationCode
      FROM inv.Lot l
      JOIN inv.Item i ON i.CompanyCode = l.CompanyCode AND i.ItemCode = l.ItemCode
      JOIN inv.StockBalance b ON b.CompanyCode = l.CompanyCode AND b.ItemCode = l.ItemCode
                             AND b.LotCode = l.LotCode AND b.SiteCode = 'RDG'
     WHERE l.CompanyCode = 'STC' AND i.ShelfLifeDays IS NOT NULL AND l.QualityStatus = 'ACCEPTED'
       AND b.QtyOnHand >= 20
     ORDER BY CASE WHEN i.Category = 'CONSUMABLE' THEN 0 ELSE 1 END, l.ExpiryDate, l.LotCode;

    DECLARE @mv table (Seq int IDENTITY(1,1), SiteCode varchar(10), WarehouseCode varchar(20), LocationCode varchar(20),
                       ItemCode varchar(40), LotCode varchar(30), QualityStatus varchar(12), MovementType varchar(14),
                       Qty decimal(19,6), MovementDate date, UnitCost decimal(19,6), SourceDocType varchar(20),
                       SourceDocRef varchar(40), CounterpartLocationCode varchar(20), ReasonCode varchar(20),
                       Reason varchar(200), ApprovalRef varchar(40), CreatedBy varchar(20));

    /*  Goods received from the approved supplier, then issued to the shop floor over the
        quarter.  The receipts and the issues net to nothing, so the carried balances that the
        master seed left on file stay exactly as they are.                                     */
    INSERT @mv (SiteCode, WarehouseCode, LocationCode, ItemCode, LotCode, QualityStatus, MovementType, Qty,
                MovementDate, UnitCost, SourceDocType, SourceDocRef, CounterpartLocationCode, ReasonCode, Reason, ApprovalRef, CreatedBy)
    VALUES
     ('RDG','MAIN','STKA01','SCREW-M4X10-SS','','ACCEPTED','RECEIPT',   600.0,'2026-07-14',0.070000,'PURCHRECEIPT','DEMO-GRN-1041',NULL,'PORCPT','Goods received against purchase order, supplier delivery note 88214',NULL,'WHSE'),
     ('RDG','MAIN','STKA01','SCREW-M4X10-SS','','ACCEPTED','ISSUE',    -240.0,'2026-08-03',0.070000,'WORKSORDER',  'DEMO-WO-5512', NULL,'WOISSUE','Issued to frame assembly work order',NULL,'WHSE'),
     ('RDG','MAIN','STKA01','SCREW-M4X10-SS','','ACCEPTED','ISSUE',    -360.0,'2026-09-22',0.070000,'WORKSORDER',  'DEMO-WO-5587', NULL,'WOISSUE','Issued to frame assembly work order',NULL,'WHSE'),
     ('RDG','MAIN','STKA01','WASHER-M6-SS','','ACCEPTED','RECEIPT',  900.0,'2026-08-11',0.090000,'PURCHRECEIPT','DEMO-GRN-1077',NULL,'PORCPT','Goods received against purchase order, supplier delivery note 88330',NULL,'WHSE'),
     ('RDG','MAIN','STKA01','WASHER-M6-SS','','ACCEPTED','ISSUE',   -540.0,'2026-09-08',0.090000,'WORKSORDER',  'DEMO-WO-5571', NULL,'WOISSUE','Issued to wheel build work order',NULL,'WHSE'),
     ('RDG','MAIN','STKA01','WASHER-M6-SS','','ACCEPTED','ISSUE',   -360.0,'2026-10-01',0.090000,'WORKSORDER',  'DEMO-WO-5604', NULL,'WOISSUE','Issued to wheel build work order',NULL,'WHSE'),
     /*  A scrapped part: damaged in handling, written off with a reason and an authority.      */
     ('RDG','MAIN','STKA01','WASHER-M6-SS','','ACCEPTED','RECEIPT',   48.0,'2026-09-15',0.090000,'PURCHRECEIPT','DEMO-GRN-1081',NULL,'PORCPT','Replacement washers received',NULL,'WHSE'),
     ('RDG','MAIN','STKA01','WASHER-M6-SS','','ACCEPTED','SCRAP',    -48.0,'2026-09-16',0.090000,'SCRAPNOTE',  'DEMO-SCRAP-118','SCRAP01','DAMAGE','Plating damaged in handling, scrapped with the inventory manager''s authority','DEMO-APPR-118','WHSE');

    /*  A transfer from Reading to Birmingham that has left but has not arrived: the despatching
        location is relieved and the quantity sits in the in-transit location, where it is
        physical stock but not eligible stock (ITM-013, ITM-017).                              */
    INSERT @mv (SiteCode, WarehouseCode, LocationCode, ItemCode, LotCode, QualityStatus, MovementType, Qty,
                MovementDate, UnitCost, SourceDocType, SourceDocRef, CounterpartLocationCode, ReasonCode, Reason, ApprovalRef, CreatedBy)
    VALUES
     ('RDG','MAIN','STKA01','SCREW-M4X10-SS','','ACCEPTED','TRANSFEROUT', -240.0,'2026-10-02',0.070000,'TRANSFER','DEMO-TRF-204','TRANSIT','SITETRF','Transfer to Birmingham fabrication, consignment DEMO-TRF-204','DEMO-APPR-204','WHSE'),
     ('RDG','MAIN','TRANSIT','SCREW-M4X10-SS','','ACCEPTED','TRANSFERIN',  240.0,'2026-10-02',0.070000,'TRANSFER','DEMO-TRF-204','RAW01','SITETRF','In transit to Birmingham, not yet received','DEMO-APPR-204','WHSE');

    /*  A quality hold: material taken out of usable stock into quarantine pending a supplier
        investigation, and part of it released back once the certificate arrived.              */
    INSERT @mv (SiteCode, WarehouseCode, LocationCode, ItemCode, LotCode, QualityStatus, MovementType, Qty,
                MovementDate, UnitCost, SourceDocType, SourceDocRef, CounterpartLocationCode, ReasonCode, Reason, ApprovalRef, CreatedBy)
    VALUES
     ('RDG','MAIN','STKA01','NUT-M6-NYLOC','','ACCEPTED','QUALITY',   -180.0,'2026-09-24',0.090000,'QUALITYHOLD','DEMO-QH-66','QUAR01','SUPPQUERY','Thread gauge failure reported, batch quarantined pending supplier investigation',NULL,'ENGINEER'),
     ('RDG','MAIN','QUAR01','NUT-M6-NYLOC','','QUARANTINE','QUALITY',  180.0,'2026-09-24',0.090000,'QUALITYHOLD','DEMO-QH-66','STKA01','SUPPQUERY','Received into quarantine cage 1',NULL,'ENGINEER'),
     ('RDG','MAIN','QUAR01','NUT-M6-NYLOC','','QUARANTINE','QUALITY',  -120.0,'2026-10-05',0.090000,'QUALITYHOLD','DEMO-QH-66','STKA01','QCRELEASE','Supplier certificate accepted, 120 released back to usable stock','DEMO-APPR-66','QUALITY'),
     ('RDG','MAIN','STKA01','NUT-M6-NYLOC','','ACCEPTED','QUALITY',     120.0,'2026-10-05',0.090000,'QUALITYHOLD','DEMO-QH-66','QUAR01','QCRELEASE','Released from quarantine, 60 remain held for the supplier',NULL,'QUALITY');

    /*  The lot controlled item with a shelf life: a receipt into its own lot and a first
        expiry, first out issue against it.                                                    */
    IF @lotItem IS NOT NULL
        INSERT @mv (SiteCode, WarehouseCode, LocationCode, ItemCode, LotCode, QualityStatus, MovementType, Qty,
                    MovementDate, UnitCost, SourceDocType, SourceDocRef, CounterpartLocationCode, ReasonCode, Reason, ApprovalRef, CreatedBy)
        VALUES
         ('RDG','MAIN',@lotLoc,@lotItem,@lotCode,'ACCEPTED','RECEIPT',  30.0,'2026-08-19',8.400000,'PURCHRECEIPT','DEMO-GRN-1090',NULL,'PORCPT','Lot received with the supplier certificate of conformity',NULL,'WHSE'),
         ('RDG','MAIN',@lotLoc,@lotItem,@lotCode,'ACCEPTED','ISSUE',   -18.0,'2026-09-29',8.400000,'WORKSORDER',  'DEMO-WO-5595',NULL,'FEFO','Issued first expiry first out against the shortest dated lot',NULL,'WHSE'),
         ('RDG','MAIN',@lotLoc,@lotItem,@lotCode,'ACCEPTED','ISSUE',   -12.0,'2026-10-06',8.400000,'WORKSORDER',  'DEMO-WO-5612',NULL,'FEFO','Issued first expiry first out against the shortest dated lot',NULL,'WHSE');

    /*  1 - an OPENING movement per bucket for the balance the master seed left there, so that
        every balance on file has a movement behind it (XMD-012).                              */
    INSERT inv.StockMovement (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo,
                              OwnerType, OwnerCode, QualityStatus, MovementType, Quantity, UomCode, EnteredQuantity,
                              ConversionFactor, UnitCost, ValueGBP, MovementDate, PostingDate, FiscalYear, PeriodNo,
                              SourceModule, SourceDocType, SourceDocRef, ReasonCode, Reason, CreatedBy, CreatedAt)
    SELECT DISTINCT 'STC', b.SiteCode, b.WarehouseCode, b.LocationCode, b.ItemCode, b.Revision, b.LotCode, b.SerialNo,
           b.OwnerType, b.OwnerCode, b.QualityStatus, 'OPENING', b.QtyOnHand, i.StockUomCode, b.QtyOnHand, 1,
           NULL, NULL, '2026-04-01', '2026-04-01', 2026, 1, 'ITM', 'OPENINGLOAD', 'DEMO-OPEN-2026',
           'CUTOVER', 'Opening stock loaded at the start of the financial year', 'SYSADM', '2026-04-01T08:00:00'
      FROM inv.StockBalance b
      JOIN inv.Item i ON i.CompanyCode = b.CompanyCode AND i.ItemCode = b.ItemCode
     WHERE b.CompanyCode = 'STC' AND b.QtyOnHand > 0
       AND EXISTS (SELECT 1 FROM @mv m WHERE m.SiteCode = b.SiteCode AND m.LocationCode = b.LocationCode
                     AND m.ItemCode = b.ItemCode AND ISNULL(m.LotCode, '') = b.LotCode
                     AND m.QualityStatus = b.QualityStatus);

    /*  2 - the demo movements themselves.                                                     */
    INSERT inv.StockMovement (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo,
                              OwnerType, OwnerCode, QualityStatus, MovementType, Quantity, UomCode, EnteredQuantity,
                              ConversionFactor, UnitCost, ValueGBP, MovementDate, PostingDate, FiscalYear, PeriodNo,
                              SourceModule, SourceDocType, SourceDocRef, CounterpartLocationCode, ReasonCode, Reason,
                              ApprovalRef, CorrelationId, CreatedBy, CreatedAt)
    SELECT 'STC', m.SiteCode, m.WarehouseCode, m.LocationCode, m.ItemCode, i.CurrentRevision, ISNULL(m.LotCode, ''), '',
           'OWNED', '', m.QualityStatus, m.MovementType, m.Qty, i.StockUomCode, m.Qty, 1, m.UnitCost,
           ROUND(m.Qty * m.UnitCost, 2), m.MovementDate, m.MovementDate,
           CASE WHEN m.MovementDate >= '2026-04-01' THEN 2026 ELSE 2025 END,
           DATEDIFF(month, '2026-04-01', m.MovementDate) + 1, 'ITM', m.SourceDocType, m.SourceDocRef,
           m.CounterpartLocationCode, m.ReasonCode, m.Reason, m.ApprovalRef,
           'DEMO-ITM-' + RIGHT('0000' + CAST(m.Seq AS varchar(4)), 4), m.CreatedBy,
           CAST(m.MovementDate AS datetime2(3))
      FROM @mv m
      JOIN inv.Item i ON i.CompanyCode = 'STC' AND i.ItemCode = m.ItemCode;

    /*  3 - move the balances by exactly the net of those movements, creating the quarantine,
        scrap and in-transit buckets that the demo needs.                                      */
    MERGE inv.StockBalance AS t
    USING (SELECT SiteCode, WarehouseCode, LocationCode, ItemCode, ISNULL(LotCode, '') AS LotCode,
                  QualityStatus, SUM(Qty) AS NetQty
             FROM @mv GROUP BY SiteCode, WarehouseCode, LocationCode, ItemCode, ISNULL(LotCode, ''), QualityStatus) AS s
       ON t.CompanyCode = 'STC' AND t.SiteCode = s.SiteCode AND t.WarehouseCode = s.WarehouseCode
      AND t.LocationCode = s.LocationCode AND t.ItemCode = s.ItemCode AND t.LotCode = s.LotCode
      AND t.SerialNo = '' AND t.OwnerType = 'OWNED' AND t.QualityStatus = s.QualityStatus
    WHEN MATCHED THEN UPDATE SET QtyOnHand = t.QtyOnHand + s.NetQty, LastMovementAt = '2026-10-06T17:30:00'
    WHEN NOT MATCHED BY TARGET AND s.NetQty > 0 THEN
        INSERT (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo,
                OwnerType, OwnerCode, QualityStatus, QtyOnHand, QtyReserved, QtyInTransit, LastMovementAt)
        VALUES ('STC', s.SiteCode, s.WarehouseCode, s.LocationCode, s.ItemCode,
                (SELECT CurrentRevision FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode = s.ItemCode),
                s.LotCode, '', 'OWNED', '', s.QualityStatus, s.NetQty, 0,
                0, '2026-10-06T17:30:00');

    PRINT 'Seeded inv.StockMovement (demonstration stock history)';
END
GO
/*  Firm reservations against named demand.  A reservation is not consumption: the physical
    stock stays where it is until an issue or a despatch moves it, but it is no longer free
    (ITM-014).                                                                               */
IF OBJECT_ID(N'inv.StockReservation') IS NOT NULL
   AND NOT EXISTS (SELECT 1 FROM inv.StockReservation WHERE CompanyCode = 'STC' AND DemandRef LIKE 'DEMO-%')
BEGIN
    INSERT inv.StockReservation (CompanyCode, SiteCode, ItemCode, Revision, WarehouseCode, LocationCode, DemandType,
                                 DemandRef, DemandLine, QtyReserved, QtyPicked, QtyConsumed, Firmness, RequiredDate,
                                 Priority, OwnerUser, Status, CreatedBy, CreatedAt)
    SELECT 'STC', v.SiteCode, v.ItemCode, i.CurrentRevision, 'MAIN', v.LocationCode, v.DemandType, v.DemandRef,
           v.DemandLine, v.Qty, 0, 0, 'FIRM', v.RequiredDate, v.Priority, v.OwnerUser, 'ACTIVE', 'SEED', '2026-10-05T09:15:00'
      FROM (VALUES
        ('RDG','STKA01','SCREW-M4X10-SS',   'SALESORDER','DEMO-SO-9001',1, 400.0,CAST('2026-10-16' AS date),3,'SALESREP'),
        ('RDG','STKA01','WASHER-M6-SS',  'KIT',       'DEMO-KIT-311',1, 140.0,CAST('2026-10-13' AS date),2,'WHSE'),
        ('RDG','STKA01','NUT-M6-NYLOC',  'JOB',       'DEMO-WO-5620',1,  60.0,CAST('2026-10-20' AS date),5,'ENGINEER')
       ) AS v (SiteCode, LocationCode, ItemCode, DemandType, DemandRef, DemandLine, Qty, RequiredDate, Priority, OwnerUser)
      JOIN inv.Item i ON i.CompanyCode = 'STC' AND i.ItemCode = v.ItemCode;

    UPDATE b SET b.QtyReserved = b.QtyReserved + r.QtyReserved
      FROM inv.StockBalance b
      JOIN inv.StockReservation r ON r.CompanyCode = b.CompanyCode AND r.SiteCode = b.SiteCode
                                 AND r.ItemCode = b.ItemCode AND r.LocationCode = b.LocationCode
     WHERE r.DemandRef LIKE 'DEMO-%' AND b.LotCode = '' AND b.SerialNo = '' AND b.QualityStatus = 'ACCEPTED';

    PRINT 'Seeded inv.StockReservation (demonstration reservations)';
END
GO
/*  A cycle count with a variance and an approved adjustment.  The count is frozen at its
    cut off, the counted quantity differs from the book quantity on one line, and the approved
    difference is posted as a COUNT movement - the balance is never typed over (ITM-018).     */
IF OBJECT_ID(N'inv.StockCount') IS NOT NULL
   AND NOT EXISTS (SELECT 1 FROM inv.StockCount WHERE CompanyCode = 'STC' AND CountRef = 'CNT-2026-001')
BEGIN
    INSERT inv.StockCount (CompanyCode, SiteCode, CountRef, CountType, ScopeDescription, WarehouseCode, CutoffAt,
                           IsFrozen, Status, CountedBy, ApprovedBy, ApprovedAt, CreatedBy, CreatedAt)
    VALUES ('STC','RDG','CNT-2026-001','CYCLE','October cycle count, small parts bins and component shelving',
            'MAIN','2026-10-05T18:00:00',1,'APPROVED','WHSE','INVMGR','2026-10-06T10:20:00','SEED','2026-10-05T08:00:00');

    DECLARE @countId int = SCOPE_IDENTITY();

    /*  Four lines: three counted exactly to book, one short by six bearings.                  */
    INSERT inv.StockCountLine (CountId, LineNum, ItemCode, Revision, WarehouseCode, LocationCode, LotCode, SerialNo,
                              ExpectedQty, CountedQty, RecountQty, InterveningMovementQty, AdjustmentQty,
                              AdjustmentValueGBP, ReasonCode, Status)
    SELECT @countId, v.LineNum, v.ItemCode, i.CurrentRevision, 'MAIN', v.LocationCode, '', '',
           b.QtyOnHand, b.QtyOnHand + v.Variance, CASE WHEN v.Variance <> 0 THEN b.QtyOnHand + v.Variance END, 0,
           CASE WHEN v.Variance <> 0 THEN v.Variance END,
           CASE WHEN v.Variance <> 0 THEN ROUND(v.Variance * 0.09, 2) END,
           CASE WHEN v.Variance <> 0 THEN 'MISPICK' END,
           CASE WHEN v.Variance <> 0 THEN 'POSTED' ELSE 'COUNTED' END
      FROM (VALUES
        (1,'STKA01','SCREW-M4X10-SS',    CAST(0.0 AS decimal(19,6))),
        (2,'STKA01','RIVET-4.8X12-AL',  CAST(0.0 AS decimal(19,6))),
        (3,'STKA01','WASHER-M6-SS',     CAST(-6.0 AS decimal(19,6))),
        (4,'STKA01','NUT-M6-NYLOC',     CAST(0.0 AS decimal(19,6)))
       ) AS v (LineNum, LocationCode, ItemCode, Variance)
      JOIN inv.Item i ON i.CompanyCode = 'STC' AND i.ItemCode = v.ItemCode
      JOIN inv.StockBalance b ON b.CompanyCode = 'STC' AND b.SiteCode = 'RDG' AND b.ItemCode = v.ItemCode
                             AND b.LocationCode = v.LocationCode AND b.LotCode = '' AND b.SerialNo = ''
                             AND b.QualityStatus = 'ACCEPTED';

    /*  The approved difference becomes a movement, and only then does the balance move.       */
    INSERT inv.StockMovement (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo,
                              OwnerType, OwnerCode, QualityStatus, MovementType, Quantity, UomCode, EnteredQuantity,
                              ConversionFactor, UnitCost, ValueGBP, MovementDate, PostingDate, FiscalYear, PeriodNo,
                              SourceModule, SourceDocType, SourceDocRef, SourceDocLine, ReasonCode, Reason,
                              ApprovalRef, CorrelationId, CreatedBy, CreatedAt)
    SELECT 'STC','RDG','MAIN', l.LocationCode, l.ItemCode, l.Revision, '', '', 'OWNED','', 'ACCEPTED','COUNT',
           l.AdjustmentQty, i.StockUomCode, l.AdjustmentQty, 1, 0.090000, l.AdjustmentValueGBP,
           '2026-10-06','2026-10-06',2026,7,'ITM','STOCKCOUNT','CNT-2026-001', l.LineNum, l.ReasonCode,
           'Approved stock count adjustment, counted short against the frozen book quantity',
           'DEMO-APPR-CNT-001','DEMO-ITM-CNT0001','INVMGR','2026-10-06T10:25:00'
      FROM inv.StockCountLine l
      JOIN inv.Item i ON i.CompanyCode = 'STC' AND i.ItemCode = l.ItemCode
     WHERE l.CountId = @countId AND l.AdjustmentQty IS NOT NULL;

    UPDATE b SET b.QtyOnHand = b.QtyOnHand + l.AdjustmentQty, b.LastCountedAt = '2026-10-06T10:20:00',
                 b.LastMovementAt = '2026-10-06T10:25:00'
      FROM inv.StockBalance b
      JOIN inv.StockCountLine l ON l.ItemCode = b.ItemCode AND l.LocationCode = b.LocationCode
     WHERE l.CountId = @countId AND l.AdjustmentQty IS NOT NULL AND b.CompanyCode = 'STC' AND b.SiteCode = 'RDG'
       AND b.LotCode = '' AND b.SerialNo = '' AND b.QualityStatus = 'ACCEPTED';

    UPDATE l SET l.MovementId = m.MovementId
      FROM inv.StockCountLine l
      JOIN inv.StockMovement m ON m.SourceDocRef = 'CNT-2026-001' AND m.SourceDocLine = l.LineNum
     WHERE l.CountId = @countId;

    PRINT 'Seeded inv.StockCount (demonstration count with an approved adjustment)';
END
GO
/*  A quality hold record for the quarantined nuts, so that the quality worklist and the stock
    enquiry tell the same story.                                                              */
IF OBJECT_ID(N'inv.QualityHold') IS NOT NULL
   AND NOT EXISTS (SELECT 1 FROM inv.QualityHold WHERE CompanyCode = 'STC' AND SourceDocRef = 'DEMO-QH-66')
BEGIN
    INSERT inv.QualityHold (CompanyCode, SiteCode, ItemCode, LocationCode, Quantity, HoldType, ReasonCode, Reason,
                            RaisedBy, RaisedAt, SourceDocType, SourceDocRef, Outcome, OutcomeQty, ReleasedBy,
                            ReleasedAt, AuthorityRef, Status)
    SELECT 'STC','RDG','NUT-M6-NYLOC','QUAR01',180.0,'QUARANTINE','SUPPQUERY',
           'Thread gauge failure reported on the supplier batch; 120 released on the certificate, 60 held for the supplier',
           'ENGINEER','2026-09-24T14:40:00','QUALITYHOLD','DEMO-QH-66','ACCEPT',120.0,'QUALITY','2026-10-05T11:05:00',
           'DEMO-APPR-66','OPEN'
     WHERE EXISTS (SELECT 1 FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode = 'NUT-M6-NYLOC');

    PRINT 'Seeded inv.QualityHold (demonstration quality hold)';
END
GO
