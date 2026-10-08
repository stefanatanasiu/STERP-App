>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMSUPPL
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : The eligible supply pool of one netted calculation.
*>              BOM-020 is a single sentence with a large
*>              consequence: eligible stock is consumed ONCE.  Two
*>              demands cannot both be satisfied by the same unit,
*>              and a subassembly taken from stock must not also
*>              have its materials requisitioned for the same
*>              units.  Keeping one pool for the whole demand
*>              scope is how that is enforced here.
*>
*>              TRIAL supply is declared by the caller (what-if
*>              planning); LIVE supply is read from
*>              inv.StockBalance net of inv.StockReservation,
*>              counting accepted, owned, unreserved quantity only.
*> CALLED BY  : BOMEXPL (through the netting path) and BOMNET
*> INTERFACE  : CALL "BOMSUPPL" USING BOM-SUP-CTL
*>              COPYBOOK BOMSUPCT  (not LINKable - one parameter,
*>              but deliberately kept out of the program directory
*>              because it is a calculation internal)
*> CALLS      : SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-02-06 AJS  CR2295 NETTED PLANNING ADDED
*> 2015-04-28 AJS  CR2312 SNAP / RESET SO THAT THE MAXIMUM
*>                 BUILDABLE SEARCH REUSES ONE READING OF STOCK
*> 2026-10-05 STERP RESERVATIONS DEDUCTED, QUARANTINE EXCLUDED
*>================================================================
*> REQ: BOM-016, BOM-020, BOM-029
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMSUPPL.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 06/02/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 01  Ws-Pool.
     05  Ws-Pool-Cnt             PIC S9(4) VALUE 0.
     05  Ws-Pool-Max             PIC S9(4) VALUE 200.
     05  Ws-Entry OCCURS 200 TIMES.
         10  Wp-Item             PIC X(40).
         10  Wp-Rev              PIC X(4).
         10  Wp-Free             PIC S9(15)V9(6).
         10  Wp-Inbound          PIC S9(15)V9(6).
         10  Wp-Snap-Free        PIC S9(15)V9(6).
         10  Wp-Snap-Inbound     PIC S9(15)V9(6).
         10  Wp-Loaded           PIC X.
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-Hit                  PIC S9(4) VALUE 0.
     05  Ws-Found                PIC X VALUE "N".
     05  Ws-Take                 PIC S9(15)V9(6) VALUE 0.
 01  Ws-Stock-Row.
     05  St-Free                 PIC X(24).
     05  St-Inbound              PIC X(24).
 LINKAGE SECTION.
 COPY BOMSUPCT.
 PROCEDURE DIVISION USING BOM-SUP-CTL.
 Main-Entry.
     MOVE "OK" TO BP-STATUS
     MOVE SPACES TO BP-MESSAGE
     EVALUATE BP-FUNC
         WHEN "CLEAR"  PERFORM Clear-Pool
         WHEN "SET"    PERFORM Set-Supply
         WHEN "FETCH"  PERFORM Fetch-Live-Supply
         WHEN "TAKE"   PERFORM Take-Supply
         WHEN "AVAIL"  PERFORM Report-Available
         WHEN "SNAP"   PERFORM Snapshot-Pool
         WHEN "RESET"  PERFORM Restore-Pool
         WHEN OTHER    MOVE "NOTFOUND" TO BP-STATUS
     END-EVALUATE
     GOBACK.

 Clear-Pool.
     MOVE 0 TO Ws-Pool-Cnt
     MOVE 0 TO BP-COUNT
     MOVE 0 TO BP-TAKEN BP-TAKEN-INBOUND BP-REMAIN.

*> A pool entry recorded without a revision covers every revision
*> of that item - the planner declared "five braces", not "five
*> braces at revision A".
 Find-Entry.
     MOVE "N" TO Ws-Found
     MOVE 0 TO Ws-Hit
     PERFORM VARYING Ws-I FROM 1 BY 1
         UNTIL Ws-I > Ws-Pool-Cnt OR Ws-Found = "Y"
         IF Wp-Item(Ws-I) = BP-ITEM
            AND (Wp-Rev(Ws-I) = BP-REV OR BP-REV = SPACES
                 OR Wp-Rev(Ws-I) = SPACES)
             MOVE "Y" TO Ws-Found
             MOVE Ws-I TO Ws-Hit
         END-IF
     END-PERFORM.

 Add-Entry.
     IF Ws-Pool-Cnt >= Ws-Pool-Max
         MOVE "FULL" TO BP-STATUS
         MOVE "The eligible supply pool is full" TO BP-MESSAGE
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Pool-Cnt
     MOVE Ws-Pool-Cnt TO Ws-Hit
     MOVE BP-ITEM TO Wp-Item(Ws-Hit)
     MOVE BP-REV TO Wp-Rev(Ws-Hit)
     MOVE 0 TO Wp-Free(Ws-Hit) Wp-Inbound(Ws-Hit)
     MOVE 0 TO Wp-Snap-Free(Ws-Hit) Wp-Snap-Inbound(Ws-Hit)
     MOVE "N" TO Wp-Loaded(Ws-Hit).

 Set-Supply.
     PERFORM Find-Entry
     IF Ws-Found = "N"
         PERFORM Add-Entry
         IF BP-STATUS = "FULL"
             EXIT PARAGRAPH
         END-IF
     END-IF
     MOVE BP-FREE TO Wp-Free(Ws-Hit)
     MOVE BP-INBOUND TO Wp-Inbound(Ws-Hit)
     MOVE "Y" TO Wp-Loaded(Ws-Hit)
     MOVE Ws-Pool-Cnt TO BP-COUNT.

*>----------------------------------------------------------------
*> Live free stock: accepted, owned, not reserved and not held in
*> a quarantine or inspection location.  Expected receipts are
*> offered separately so the caller can decide whether to plan on
*> them (BN-USE-INBOUND).
 Fetch-Live-Supply.
     PERFORM Find-Entry
     IF Ws-Found = "Y" AND Wp-Loaded(Ws-Hit) = "Y"
         MOVE Wp-Free(Ws-Hit) TO BP-FREE
         MOVE Wp-Inbound(Ws-Hit) TO BP-INBOUND
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT ISNULL(SUM(CASE WHEN b.QtyOnHand - "
            "b.QtyReserved > 0 THEN b.QtyOnHand - b.QtyReserved "
            "ELSE 0 END),0), ISNULL(SUM(b.QtyInTransit),0) "
            "FROM inv.StockBalance b "
            "JOIN inv.Location l ON l.CompanyCode = b.CompanyCode "
            "AND l.SiteCode = b.SiteCode "
            "AND l.WarehouseCode = b.WarehouseCode "
            "AND l.LocationCode = b.LocationCode "
            "WHERE b.CompanyCode = ? AND b.ItemCode = ? "
            "AND (? = '' OR b.SiteCode = ?) "
            "AND b.QualityStatus = 'ACCEPTED' "
            "AND b.OwnerType = 'OWNED' "
            "AND l.LocationType = 'STOCK'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 4 TO SQLIO-PARM-CNT
     MOVE BP-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BP-ITEM TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BP-SITE TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE BP-SITE TO SQLIO-PARM-VAL(4)
     MOVE "N" TO SQLIO-PARM-NULL(4)
     MOVE 2 TO SQLIO-COL-CNT
     MOVE 24 TO SQLIO-COL-WIDTH(1)
     MOVE 24 TO SQLIO-COL-WIDTH(2)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Stock-Row
     IF SQLIO-ERROR
         MOVE "SQLFAIL" TO BP-STATUS
         MOVE SQLIO-ERRMSG TO BP-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         MOVE 0 TO BP-FREE BP-INBOUND
     ELSE
         COMPUTE BP-FREE = FUNCTION NUMVAL(St-Free)
         COMPUTE BP-INBOUND = FUNCTION NUMVAL(St-Inbound)
     END-IF
     PERFORM Set-Supply.

*>----------------------------------------------------------------
*> TAKE - the only way stock leaves the pool.  Free stock first,
*> then expected receipts when the caller allows them.
 Take-Supply.
     MOVE 0 TO BP-TAKEN BP-TAKEN-INBOUND
     COMPUTE BP-REMAIN = BP-WANT
     IF BP-WANT <= 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Find-Entry
     IF Ws-Found = "N"
         IF BP-SOURCE = "LIVE"
             PERFORM Fetch-Live-Supply
             PERFORM Find-Entry
         END-IF
     END-IF
     IF Ws-Found = "N"
         EXIT PARAGRAPH
     END-IF
     IF Wp-Free(Ws-Hit) > 0
         IF Wp-Free(Ws-Hit) >= BP-WANT
             MOVE BP-WANT TO Ws-Take
         ELSE
             MOVE Wp-Free(Ws-Hit) TO Ws-Take
         END-IF
         SUBTRACT Ws-Take FROM Wp-Free(Ws-Hit)
         MOVE Ws-Take TO BP-TAKEN
     END-IF
     COMPUTE BP-REMAIN = BP-WANT - BP-TAKEN
     IF BP-REMAIN > 0 AND BP-USE-INBOUND = "Y"
        AND Wp-Inbound(Ws-Hit) > 0
         IF Wp-Inbound(Ws-Hit) >= BP-REMAIN
             MOVE BP-REMAIN TO Ws-Take
         ELSE
             MOVE Wp-Inbound(Ws-Hit) TO Ws-Take
         END-IF
         SUBTRACT Ws-Take FROM Wp-Inbound(Ws-Hit)
         MOVE Ws-Take TO BP-TAKEN-INBOUND
         COMPUTE BP-REMAIN = BP-REMAIN - Ws-Take
     END-IF.

 Report-Available.
     PERFORM Find-Entry
     IF Ws-Found = "N"
         MOVE 0 TO BP-FREE BP-INBOUND
         MOVE "NOTFOUND" TO BP-STATUS
         EXIT PARAGRAPH
     END-IF
     MOVE Wp-Free(Ws-Hit) TO BP-FREE
     MOVE Wp-Inbound(Ws-Hit) TO BP-INBOUND.

 Snapshot-Pool.
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Pool-Cnt
         MOVE Wp-Free(Ws-I) TO Wp-Snap-Free(Ws-I)
         MOVE Wp-Inbound(Ws-I) TO Wp-Snap-Inbound(Ws-I)
     END-PERFORM
     MOVE Ws-Pool-Cnt TO BP-COUNT.

 Restore-Pool.
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Pool-Cnt
         MOVE Wp-Snap-Free(Ws-I) TO Wp-Free(Ws-I)
         MOVE Wp-Snap-Inbound(Ws-I) TO Wp-Inbound(Ws-I)
     END-PERFORM
     MOVE Ws-Pool-Cnt TO BP-COUNT.
