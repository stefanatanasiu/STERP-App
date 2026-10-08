>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITRESV
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Reserve, revalidate and pick material for an
*>              execution kit (KIT-008, KIT-009, KIT-022).
*>
*>              Three states are kept apart on purpose, because
*>              conflating them is how a stores system starts
*>              lying to the shop floor:
*>                RESERVED  the stock is spoken for but has not
*>                          moved and is still owned by stores;
*>                PICKED    it has been taken off the shelf for
*>                          this kit but is NOT yet work in
*>                          progress and no value has moved;
*>                ISSUED    KITISSUE/STKMOVE has consumed it.
*>              A picked component is therefore still visible as
*>              stock, still revalidated, and still returnable
*>              without a kit return.
*>
*>              The reservation arithmetic itself belongs to
*>              STKRESV / inv.usp_ReserveStock, which holds the
*>              UPDLOCK.  KITRESV never adds up free stock for
*>              itself - two kits reserving at the same instant
*>              must be serialised by the database, not by COBOL.
*> CALLED BY  : frmKitExecution, KITRDY batch, KITCLOSE (RELEASE)
*> INTERFACE  : CALL "KITRESV" USING WS-KITRESV-CA
*>              COPYBOOK KITRESVCA
*> CALLS      : KITDB, STKRESV, EXCPRAIS, AUDITLOG
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1995-11-07 DRN  CR0412 ALLOCATION TO WORKS ORDERS
*> 2015-04-27 PMK  CR2320 PICK SEPARATED FROM ISSUE
*> 2016-08-02 PMK  CR2494 REVALIDATION AFTER A MATERIAL CHANGE
*> 2026-10-06 STERP KIT-008, KIT-009, KIT-022
*>================================================================
*> REQ: KIT-008, KIT-009, KIT-022
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITRESV.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 27/04/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY KITDBW.
 COPY KITWORK.
 COPY STKRESVCA REPLACING ==DFHCOMMAREA== BY ==Ws-Resv-Ca==
      LEADING ==CA-== BY ==RA-==.
 COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==Ws-Exc-Ca==
      LEADING ==CA-== BY ==XA-==.
 COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Audit-Ca==
      LEADING ==CA-== BY ==AA-==.
 01  Ws-Work.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Want                 PIC S9(13)V9(6) VALUE 0.
     05  Ws-Outstanding          PIC S9(13)V9(6) VALUE 0.
     05  Ws-To-Pick              PIC S9(13)V9(6) VALUE 0.
     05  Ws-Picked-Now           PIC S9(13)V9(6) VALUE 0.
     05  Ws-Row                  PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Skip                 PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Committed            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Need-Date            PIC 9(8) VALUE 0.
     05  Ws-Edit                 PIC -(13)9.9(6).
 LINKAGE SECTION.
 COPY KITRESVCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITRESV" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-008" TO CA-REQ-REF
     MOVE 0 TO KR-RESERVED-QTY
     MOVE 0 TO KR-SHORTFALL-QTY
     MOVE 0 TO KR-PICKED-QTY
     MOVE 0 TO KR-OWN-RESERVED
     MOVE 0 TO KR-LINES-DONE
     MOVE 0 TO KR-LINES-SHORT
     MOVE 0 TO KR-INVALID-CNT
     MOVE 0 TO KR-EXCEPTION-ID
     MOVE SPACES TO KR-MESSAGE-TEXT
     MOVE 0 TO KR-SUPPLY-COMMITTED
     MOVE 0 TO KR-COMMIT-ID
     MOVE 0 TO KR-COVERED-QTY
     MOVE 0 TO KR-UNCOVERED-QTY
     MOVE "N" TO KR-SUPPLY-LATE
     IF KR-REQUIRED-DATE = 0
         MOVE CA-BUSINESS-DATE TO KR-REQUIRED-DATE
     END-IF
     IF KR-PRIORITY = 0
         MOVE 5 TO KR-PRIORITY
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "RESERVE"  PERFORM Reserve-Material
         WHEN "SUPPLY"   PERFORM Commit-Future-Supply
         WHEN "RELEASE"  PERFORM Release-Material
         WHEN "REVALID"  PERFORM Revalidate-Material
         WHEN "PICKLIST" PERFORM Build-Pick-List
         WHEN "PICK"     PERFORM Pick-One-Line
         WHEN "SHORTPCK" PERFORM Pick-One-Line
         WHEN "UNPICK"   PERFORM Unpick-One-Line
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITRESV does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE KD-ERRMSG TO CA-MESSAGE.

*>----------------------------------------------------------------
 Load-Kit.
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KR-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KR-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE
     IF KR-SITE = SPACES
         MOVE KH-SITE TO KR-SITE
     END-IF.

 Load-Line.
     MOVE "LINGET" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE KR-LINE-NUM TO KD-LINE-NUM
     INITIALIZE KIT-LINE-REC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0007" TO CA-REASON-CODE
             MOVE "That material line is not on this kit." TO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE.

 Save-Line.
     MOVE "LINSAVE" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     END-IF.

*>   What is still worth reserving.  Material already issued is
*>   gone from the requirement, and material already reserved is
*>   recognised once - KIT-008 is explicit that a demand's own
*>   reservations are not double counted.  Material covered by an
*>   identified inbound order is not reserved from free stock
*>   either: that cover is already a commitment.
 Outstanding-For-Line.
     PERFORM Supply-Committed-For-Line
     COMPUTE Ws-Outstanding = KL-CURRENT-REQ - KL-NET-ISSUED
                            - KL-RESERVED-QTY - Ws-Committed
     IF Ws-Outstanding < 0
         MOVE 0 TO Ws-Outstanding
     END-IF.

*>   How much of this line is already covered by named future
*>   supply.  A missing or undeployed commitment table means no
*>   cover, never a failed reservation: the stock side of KIT-008
*>   must keep working on its own.
 Supply-Committed-For-Line.
     MOVE 0 TO Ws-Committed
     MOVE "SUPSUM" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KR-KIT-REF TO KD-KIT-REF
     MOVE KL-LINE-NUM TO KD-LINE-NUM
     MOVE 0 TO KD-S-COMMITTED
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC = 0
         MOVE KD-S-COMMITTED TO Ws-Committed
     END-IF.

*>----------------------------------------------------------------
*> RESERVE - one line when KR-LINE-NUM is given, otherwise the
*> whole kit.
 Reserve-Material.
     PERFORM Load-Kit
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     IF KH-IS-CANCELLED OR KH-IS-FINCLOSED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "Material cannot be reserved for a kit in status "
                FUNCTION TRIM(KH-STATUS) "."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KR-LINE-NUM > 0
         PERFORM Load-Line
         IF CA-RETURN-CODE NOT = 0
             EXIT PARAGRAPH
         END-IF
         PERFORM Reserve-This-Line
     ELSE
         PERFORM Reserve-Every-Line
     END-IF
     PERFORM Summarise-Reservation.

 Reserve-Every-Line.
     MOVE "LINOPEN" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE "Y" TO KD-ONLY-OPEN
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "LINNEXT" TO KD-FUNC
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         EVALUATE KD-RC
             WHEN 0
                 PERFORM Reserve-This-Line
             WHEN 100
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Set-Sql-Failure
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC.

 Reserve-This-Line.
     IF KL-IS-CANCELLED OR KL-IS-CLOSED
         EXIT PARAGRAPH
     END-IF
     PERFORM Outstanding-For-Line
     IF KR-LINE-NUM > 0 AND KR-QUANTITY > 0
         MOVE KR-QUANTITY TO Ws-Want
         IF Ws-Want > Ws-Outstanding
             MOVE Ws-Outstanding TO Ws-Want
         END-IF
     ELSE
         MOVE Ws-Outstanding TO Ws-Want
     END-IF
     IF Ws-Want <= 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Call-Reserve-Service
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     COMPUTE KL-RESERVED-QTY = KL-RESERVED-QTY + SR-QTY-RESERVED
     COMPUTE KR-RESERVED-QTY = KR-RESERVED-QTY + SR-QTY-RESERVED
     MOVE KL-RESERVED-QTY TO KR-OWN-RESERVED
     MOVE SR-RESERVATION-ID TO KR-RESERVATION-ID
     PERFORM Save-Line
     ADD 1 TO KR-LINES-DONE
     IF SR-QTY-SHORTFALL > 0
         ADD 1 TO KR-LINES-SHORT
         COMPUTE KR-SHORTFALL-QTY = KR-SHORTFALL-QTY
                                  + SR-QTY-SHORTFALL
         MOVE KL-ITEM-CODE TO KR-ITEM-CODE
         PERFORM Raise-Shortage-Exception
     END-IF.

 Call-Reserve-Service.
     INITIALIZE Ws-Resv-Ca
     MOVE "RESERVE" TO RA-FUNCTION
     MOVE CA-COMPANY TO RA-COMPANY
     MOVE KR-SITE TO RA-SITE
     MOVE CA-USER TO RA-USER
     MOVE CA-BUSINESS-DATE TO RA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO RA-CORRELATION-ID
     MOVE KL-ITEM-CODE TO SR-ITEM-CODE
     MOVE KL-REVISION TO SR-REVISION
     MOVE KR-SITE TO SR-SITE
     MOVE KR-WAREHOUSE TO SR-WAREHOUSE
     MOVE KR-LOCATION TO SR-LOCATION
     MOVE KR-LOT-CODE TO SR-LOT-CODE
     MOVE KR-SERIAL-NO TO SR-SERIAL-NO
     MOVE "KIT" TO SR-DEMAND-TYPE
     MOVE KR-KIT-REF TO SR-DEMAND-REF
     MOVE KL-LINE-NUM TO SR-DEMAND-LINE
     MOVE "FIRM" TO SR-FIRMNESS
     MOVE KR-PRIORITY TO SR-PRIORITY
     IF KL-NEED-DATE > 0
         MOVE KL-NEED-DATE TO SR-REQUIRED-DATE
     ELSE
         MOVE KR-REQUIRED-DATE TO SR-REQUIRED-DATE
     END-IF
     MOVE Ws-Want TO SR-QTY-REQUESTED
*>   A kit planner nearly always wants what is there rather than
*>   nothing at all, but the caller decides and the shortfall is
*>   always reported either way.
     IF KR-PARTIAL-OK
         MOVE "Y" TO SR-ALLOW-PARTIAL
     ELSE
         MOVE "N" TO SR-ALLOW-PARTIAL
     END-IF
     CALL "STKRESV" USING Ws-Resv-Ca
     IF RA-RETURN-CODE > 4
         MOVE RA-RETURN-CODE TO CA-RETURN-CODE
         MOVE RA-REASON-CODE TO CA-REASON-CODE
         MOVE RA-MESSAGE TO CA-MESSAGE
     END-IF.

 Summarise-Reservation.
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KR-LINES-SHORT > 0
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0010" TO CA-REASON-CODE
         MOVE KR-SHORTFALL-QTY TO Ws-Edit
         STRING "Reserved what was free; short by "
                FUNCTION TRIM(Ws-Edit)
                " across " KR-LINES-SHORT " line(s)."
             DELIMITED BY SIZE INTO CA-MESSAGE
     ELSE
         MOVE KR-RESERVED-QTY TO Ws-Edit
         STRING "Reserved " FUNCTION TRIM(Ws-Edit)
                ". Nothing has moved: reservation is not an issue."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     MOVE CA-MESSAGE TO KR-MESSAGE-TEXT.

*>----------------------------------------------------------------
*> SUPPLY - the other half of KIT-008.  A reservation claims stock
*> that exists; this claims an inbound order that does not exist
*> yet, which is what makes KITTRIAL's "inbound" column something
*> a planner may act on.  Nothing is taken from the free pool, no
*> inv row is written, and a commitment can never be mistaken for
*> an issue: only KITISSUE issues.
 Commit-Future-Supply.
     PERFORM Load-Kit
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     IF KH-IS-CANCELLED OR KH-IS-FINCLOSED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "Future supply cannot be committed to a kit in status "
                FUNCTION TRIM(KH-STATUS) "."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KR-LINE-NUM <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0007" TO CA-REASON-CODE
         MOVE "A material line number is required to commit future supply."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     IF KL-IS-CANCELLED OR KL-IS-CLOSED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         MOVE "That material line is closed and needs nothing."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Validate-Supply-Request
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Outstanding-For-Line
     IF KR-SUPPLY-QTY > Ws-Outstanding
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE Ws-Outstanding TO Ws-Edit
         STRING "Only " FUNCTION TRIM(Ws-Edit)
                " of this line is still uncovered."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Write-Supply-Commitment
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Summarise-Supply-Cover
     PERFORM Audit-Reservation-Event.

*>   An identified supply means identified: a type, a document and
*>   a date the planner can be held to.  "Something will turn up"
*>   is not cover and KIT-008 does not pretend otherwise.
 Validate-Supply-Request.
     IF NOT KR-SUPPLY-KNOWN
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "Future supply must be identified as PURCHASE, WORKSORDER, KIT or TRANSFER."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KR-SUPPLY-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "The supply order reference that covers this line must be stated."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KR-SUPPLY-QTY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The committed supply quantity must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KR-SUPPLY-DUE = 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "The date the supply is due must be stated before it can cover a need."
              TO CA-MESSAGE
     END-IF.

 Write-Supply-Commitment.
     IF KL-NEED-DATE > 0
         MOVE KL-NEED-DATE TO Ws-Need-Date
     ELSE
         MOVE KR-REQUIRED-DATE TO Ws-Need-Date
     END-IF
     IF KR-SUPPLY-DUE > Ws-Need-Date
         MOVE "Y" TO KR-SUPPLY-LATE
     ELSE
         MOVE "N" TO KR-SUPPLY-LATE
     END-IF
     MOVE "SUPINS" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KR-KIT-REF TO KD-KIT-REF
     MOVE KR-SUPPLY-TYPE TO KD-S-TYPE
     MOVE KR-SUPPLY-REF TO KD-S-REF
     MOVE KR-SUPPLY-LINE TO KD-S-LINE
     MOVE KR-SUPPLY-QTY TO KD-S-QTY
     MOVE KL-UOM-CODE TO KD-S-UOM
     MOVE KR-SUPPLY-DUE TO KD-S-DUE-DATE
     MOVE Ws-Need-Date TO KD-S-REQ-DATE
     MOVE KR-SUPPLY-LATE TO KD-S-LATE
     MOVE KR-REASON-CODE TO KD-S-REASON-CODE
     MOVE KR-REASON-TEXT (1:200) TO KD-S-REASON
     MOVE CA-CORRELATION-ID TO KD-REASON
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE KD-NEW-ID TO KR-COMMIT-ID
     ADD 1 TO KR-LINES-DONE.

 Summarise-Supply-Cover.
     COMPUTE KR-SUPPLY-COMMITTED = Ws-Committed + KR-SUPPLY-QTY
     COMPUTE KR-COVERED-QTY = KL-RESERVED-QTY + KL-NET-ISSUED
                            + KR-SUPPLY-COMMITTED
     COMPUTE KR-UNCOVERED-QTY = KL-CURRENT-REQ - KR-COVERED-QTY
     IF KR-UNCOVERED-QTY < 0
         MOVE 0 TO KR-UNCOVERED-QTY
     END-IF
     MOVE KL-RESERVED-QTY TO KR-OWN-RESERVED
     IF KR-SUPPLY-LATE = "Y"
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0010" TO CA-REASON-CODE
         MOVE KR-SUPPLY-QTY TO Ws-Edit
         STRING "Committed " FUNCTION TRIM(Ws-Edit) " to "
                FUNCTION TRIM(KR-SUPPLY-REF)
                ", which is due after the line needs it."
             DELIMITED BY SIZE INTO CA-MESSAGE
     ELSE
         MOVE KR-SUPPLY-QTY TO Ws-Edit
         STRING "Committed " FUNCTION TRIM(Ws-Edit) " to "
                FUNCTION TRIM(KR-SUPPLY-REF)
                ". No stock was reserved and nothing was issued."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     MOVE CA-MESSAGE TO KR-MESSAGE-TEXT.

*>----------------------------------------------------------------
*> RELEASE - give the stock back to the free pool.  KITCLOSE
*> calls this when a kit is cancelled; the material that was only
*> reserved never moved, so releasing it is honest.  Material
*> that was ISSUED is a different problem and needs KITRET.
 Release-Material.
     PERFORM Load-Kit
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Free-Supply-Cover
     INITIALIZE Ws-Resv-Ca
     MOVE "RELEASE" TO RA-FUNCTION
     MOVE CA-COMPANY TO RA-COMPANY
     MOVE KR-SITE TO RA-SITE
     MOVE CA-USER TO RA-USER
     MOVE CA-BUSINESS-DATE TO RA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO RA-CORRELATION-ID
     MOVE "KIT" TO SR-DEMAND-TYPE
     MOVE KR-KIT-REF TO SR-DEMAND-REF
     MOVE KR-LINE-NUM TO SR-DEMAND-LINE
     MOVE KR-ITEM-CODE TO SR-ITEM-CODE
     MOVE KR-SITE TO SR-SITE
     CALL "STKRESV" USING Ws-Resv-Ca
     IF RA-RETURN-CODE > 4
         MOVE RA-RETURN-CODE TO CA-RETURN-CODE
         MOVE RA-REASON-CODE TO CA-REASON-CODE
         MOVE RA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Zero-Line-Reservations
     MOVE KR-LINES-DONE TO Ws-Row
     STRING "Reservations released for kit "
            FUNCTION TRIM(KR-KIT-REF)
            ". Issued material is unaffected and still needs a return."
         DELIMITED BY SIZE INTO CA-MESSAGE
     MOVE CA-MESSAGE TO KR-MESSAGE-TEXT
     PERFORM Audit-Reservation-Event.

 Zero-Line-Reservations.
     MOVE "LINOPEN" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE "N" TO KD-ONLY-OPEN
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "LINNEXT" TO KD-FUNC
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         EVALUATE KD-RC
             WHEN 0
                 IF KR-LINE-NUM = 0 OR KR-LINE-NUM = KL-LINE-NUM
                     IF KL-RESERVED-QTY NOT = 0
                         COMPUTE KR-RESERVED-QTY = KR-RESERVED-QTY
                                                 + KL-RESERVED-QTY
                         MOVE 0 TO KL-RESERVED-QTY
                         PERFORM Save-Line
                         ADD 1 TO KR-LINES-DONE
                     END-IF
                 END-IF
             WHEN 100
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Set-Sql-Failure
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC.

*>----------------------------------------------------------------
*> REVALID - the material a reservation points at can go blocked,
*> quarantined or out of date after the reservation was made.
*> The kit owner is told and must reallocate; the system must not
*> silently move the reservation somewhere else (ITM-014).
 Revalidate-Material.
     PERFORM Load-Kit
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Resv-Ca
     MOVE "REVALID" TO RA-FUNCTION
     MOVE CA-COMPANY TO RA-COMPANY
     MOVE KR-SITE TO RA-SITE
     MOVE CA-USER TO RA-USER
     MOVE CA-BUSINESS-DATE TO RA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO RA-CORRELATION-ID
     MOVE "KIT" TO SR-DEMAND-TYPE
     MOVE KR-KIT-REF TO SR-DEMAND-REF
     MOVE KR-SITE TO SR-SITE
     CALL "STKRESV" USING Ws-Resv-Ca
     IF RA-RETURN-CODE > 4
         MOVE RA-RETURN-CODE TO CA-RETURN-CODE
         MOVE RA-REASON-CODE TO CA-REASON-CODE
         MOVE RA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE SR-INVALID-CNT TO KR-INVALID-CNT
     IF KR-INVALID-CNT > 0
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0010" TO CA-REASON-CODE
         STRING KR-INVALID-CNT
                " reservation(s) no longer point at eligible stock "
                "and must be reallocated before picking."
             DELIMITED BY SIZE INTO CA-MESSAGE
         MOVE SR-INVALID-REASON TO KR-REASON-TEXT
         PERFORM Raise-Invalid-Exception
     ELSE
         MOVE "Every reservation for this kit still points at eligible stock."
              TO CA-MESSAGE
     END-IF
     MOVE CA-MESSAGE TO KR-MESSAGE-TEXT.

*>----------------------------------------------------------------
*> PICKLIST - what stores must physically fetch.  KIT-022 wants
*> the exact component, quantity, location and lot/serial, and
*> the outstanding and shortage columns beside them.  Reprinting
*> this list has no stock effect whatsoever.
 Build-Pick-List.
     PERFORM Load-Kit
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     IF KR-WIN-MAX <= 0 OR KR-WIN-MAX > 20
         MOVE 20 TO KR-WIN-MAX
     END-IF
     MOVE 0 TO KR-WIN-CNT
     MOVE "N" TO KR-MORE-LINES
     MOVE 0 TO Ws-Row
     MOVE "LINOPEN" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE "Y" TO KD-ONLY-OPEN
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "LINNEXT" TO KD-FUNC
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         EVALUATE KD-RC
             WHEN 0
                 PERFORM Add-Pick-Row
             WHEN 100
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Set-Sql-Failure
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     MOVE KR-KIT-REF TO KR-DOC-REF
     MOVE "Picking list produced. Printing it moves no stock."
          TO KR-MESSAGE-TEXT.

 Add-Pick-Row.
     COMPUTE Ws-To-Pick = KL-RESERVED-QTY - KL-PICKED-QTY
     IF Ws-To-Pick <= 0 AND KL-PICKED-QTY = 0
         COMPUTE Ws-To-Pick = KL-CURRENT-REQ - KL-NET-ISSUED
     END-IF
     IF Ws-To-Pick <= 0
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Row
     IF Ws-Row <= KR-FROM-LINE
         EXIT PARAGRAPH
     END-IF
     IF KR-WIN-CNT >= KR-WIN-MAX
         MOVE "Y" TO KR-MORE-LINES
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO KR-WIN-CNT
     MOVE KL-LINE-NUM TO KR-P-LINE-NUM (KR-WIN-CNT)
     MOVE KL-ITEM-CODE TO KR-P-ITEM (KR-WIN-CNT)
     MOVE KL-REVISION TO KR-P-REV (KR-WIN-CNT)
     MOVE KL-UOM-CODE TO KR-P-UOM (KR-WIN-CNT)
     MOVE Ws-To-Pick TO KR-P-TO-PICK (KR-WIN-CNT)
     MOVE KL-RESERVED-QTY TO KR-P-RESERVED (KR-WIN-CNT)
     MOVE KL-PICKED-QTY TO KR-P-PICKED (KR-WIN-CNT)
     MOVE KR-WAREHOUSE TO KR-P-WAREHOUSE (KR-WIN-CNT)
     MOVE KL-LOCATION TO KR-P-LOCATION (KR-WIN-CNT)
     MOVE SPACES TO KR-P-LOT (KR-WIN-CNT)
     MOVE SPACES TO KR-P-SERIAL (KR-WIN-CNT)
     MOVE KL-NEED-DATE TO KR-P-NEED-DATE (KR-WIN-CNT)
     MOVE KL-STATUS TO KR-P-STATUS (KR-WIN-CNT)
     PERFORM Describe-Pick-Row.

*>   Stores must be told, on the paper, when a component is
*>   traced: a lot or serial must be captured at pick time or the
*>   genealogy at completion will be a guess (KIT-019).
 Describe-Pick-Row.
     MOVE "ITEMGET" TO KD-FUNC
     MOVE KL-ITEM-CODE TO KD-ITEM-CODE
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC = 0
         MOVE KI-DESCRIPTION TO KR-P-DESC (KR-WIN-CNT)
         MOVE KI-TRACED TO KR-P-TRACED (KR-WIN-CNT)
     ELSE
         MOVE SPACES TO KR-P-DESC (KR-WIN-CNT)
         MOVE "N" TO KR-P-TRACED (KR-WIN-CNT)
     END-IF
     COMPUTE KR-P-SHORTAGE (KR-WIN-CNT) =
         KL-CURRENT-REQ - KL-NET-ISSUED - KL-RESERVED-QTY
     IF KR-P-SHORTAGE (KR-WIN-CNT) < 0
         MOVE 0 TO KR-P-SHORTAGE (KR-WIN-CNT)
     END-IF
     IF KR-P-TRACED (KR-WIN-CNT) = "Y"
         MOVE "Record the lot or serial actually picked."
              TO KR-P-NOTE (KR-WIN-CNT)
     ELSE
         IF KR-P-SHORTAGE (KR-WIN-CNT) > 0
             MOVE "Not fully reserved - expect a short pick."
                  TO KR-P-NOTE (KR-WIN-CNT)
         ELSE
             MOVE SPACES TO KR-P-NOTE (KR-WIN-CNT)
         END-IF
     END-IF.

*>----------------------------------------------------------------
*> PICK / SHORTPCK - the shelf is emptied but the ledger is not
*> touched.  A picked quantity can never exceed what is reserved
*> plus what is still required, and it never becomes an issue
*> here: KITISSUE does that, and only that.
 Pick-One-Line.
     PERFORM Load-Kit
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     IF KR-LINE-NUM <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0007" TO CA-REASON-CODE
         MOVE "A material line number is required to pick."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     IF KR-QUANTITY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The picked quantity must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-To-Pick = KL-CURRENT-REQ - KL-NET-ISSUED
                        - KL-PICKED-QTY
     IF Ws-To-Pick < 0
         MOVE 0 TO Ws-To-Pick
     END-IF
     MOVE KR-QUANTITY TO Ws-Picked-Now
     IF Ws-Picked-Now > Ws-To-Pick
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE Ws-To-Pick TO Ws-Edit
         STRING "At most " FUNCTION TRIM(Ws-Edit)
                " may still be picked for this line."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF CA-FUNCTION = "SHORTPCK" AND KR-REASON-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "A short pick must say why the full quantity was not found."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     COMPUTE KL-PICKED-QTY = KL-PICKED-QTY + Ws-Picked-Now
     IF KL-LOCATION = SPACES AND KR-LOCATION NOT = SPACES
         MOVE KR-LOCATION TO KL-LOCATION
     END-IF
     PERFORM Save-Line
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE KL-PICKED-QTY TO KR-PICKED-QTY
     MOVE KL-RESERVED-QTY TO KR-OWN-RESERVED
     ADD 1 TO KR-LINES-DONE
     MOVE Ws-Picked-Now TO Ws-Want
     IF CA-FUNCTION = "SHORTPCK"
         MOVE "SHORTPICK" TO KD-P-TYPE
     ELSE
         MOVE "PICK" TO KD-P-TYPE
     END-IF
     PERFORM Record-The-Pick
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF CA-FUNCTION = "SHORTPCK"
         COMPUTE KR-SHORTFALL-QTY = Ws-To-Pick - Ws-Picked-Now
         ADD 1 TO KR-LINES-SHORT
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0010" TO CA-REASON-CODE
         MOVE KR-SHORTFALL-QTY TO Ws-Edit
         STRING "Short pick recorded; " FUNCTION TRIM(Ws-Edit)
                " still outstanding. The kit is not issued."
             DELIMITED BY SIZE INTO CA-MESSAGE
         MOVE KL-ITEM-CODE TO KR-ITEM-CODE
         PERFORM Raise-Shortage-Exception
     ELSE
         MOVE Ws-Picked-Now TO Ws-Edit
         STRING "Picked " FUNCTION TRIM(Ws-Edit)
                " for this line. It is not issued until KITISSUE runs."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     MOVE CA-MESSAGE TO KR-MESSAGE-TEXT
     PERFORM Audit-Reservation-Event.

 Unpick-One-Line.
     PERFORM Load-Kit
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     MOVE KR-QUANTITY TO Ws-Picked-Now
     IF Ws-Picked-Now <= 0 OR Ws-Picked-Now > KL-PICKED-QTY
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE KL-PICKED-QTY TO Ws-Edit
         STRING "Only " FUNCTION TRIM(Ws-Edit)
                " is currently picked on this line."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     COMPUTE KL-PICKED-QTY = KL-PICKED-QTY - Ws-Picked-Now
     PERFORM Save-Line
     MOVE KL-PICKED-QTY TO KR-PICKED-QTY
     ADD 1 TO KR-LINES-DONE
     COMPUTE Ws-Want = 0 - Ws-Picked-Now
     MOVE "UNPICK" TO KD-P-TYPE
     PERFORM Record-The-Pick
     MOVE "Returned to the shelf. No stock movement was reversed, because none was made."
          TO CA-MESSAGE
     MOVE CA-MESSAGE TO KR-MESSAGE-TEXT
     PERFORM Audit-Reservation-Event.

*>   The pick trail, which is the only place that remembers which
*>   box the storeman actually took.  KITCOMPL reads it back so
*>   that genealogy can name the lot in the unit rather than a
*>   proportion of everything issued (KIT-009, KIT-019).  An
*>   UNPICK is written as a negative row: the trail keeps the
*>   mistake rather than hiding it.
 Record-The-Pick.
     MOVE "PCKINS" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KR-KIT-REF TO KD-KIT-REF
     MOVE Ws-Want TO KD-P-QTY
     MOVE KL-UOM-CODE TO KD-P-UOM
     MOVE KR-WAREHOUSE TO KD-P-WAREHOUSE
     IF KR-LOCATION NOT = SPACES
         MOVE KR-LOCATION TO KD-P-LOCATION
     ELSE
         MOVE KL-LOCATION TO KD-P-LOCATION
     END-IF
     MOVE KR-LOT-CODE TO KD-P-LOT
     MOVE KR-SERIAL-NO TO KD-P-SERIAL
     MOVE CA-BUSINESS-DATE TO KD-P-PICK-DATE
     MOVE KR-REASON-CODE TO KD-P-REASON-CODE
     MOVE KR-REASON-TEXT (1:200) TO KD-P-REASON
     MOVE CA-CORRELATION-ID TO KD-REASON
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     END-IF.

*>   Cover from a named inbound order is released with the
*>   reservations, but a failure here never stops the stock being
*>   freed: the shelf matters more than the paperwork.
 Free-Supply-Cover.
     MOVE "SUPFREE" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KR-KIT-REF TO KD-KIT-REF
     MOVE KR-LINE-NUM TO KD-LINE-NUM
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC = 0
         MOVE 0 TO KR-SUPPLY-COMMITTED
     END-IF.

*>----------------------------------------------------------------
 Raise-Shortage-Exception.
     INITIALIZE Ws-Exc-Ca
     MOVE "RAISE" TO XA-FUNCTION
     MOVE CA-COMPANY TO XA-COMPANY
     MOVE KR-SITE TO XA-SITE
     MOVE CA-USER TO XA-USER
     MOVE CA-BUSINESS-DATE TO XA-BUSINESS-DATE
     MOVE "KIT" TO EX-MODULE
     MOVE "WARNING" TO EX-SEVERITY
     MOVE "KIT MATERIAL SHORTAGE" TO EX-CATEGORY
     MOVE CA-MESSAGE (1:200) TO EX-MESSAGE-TEXT
     MOVE "EXECUTIONKIT" TO EX-DOCUMENT-TYPE
     MOVE KR-KIT-REF TO EX-DOCUMENT-REF
     MOVE KH-OWNER-USER TO EX-OWNER-USER
     MOVE 3 TO EX-PRIORITY
     MOVE KR-SHORTFALL-QTY TO EX-QUANTITY
     CALL "EXCPRAIS" USING Ws-Exc-Ca
     MOVE EX-EXCEPTION-ID TO KR-EXCEPTION-ID.

 Raise-Invalid-Exception.
     INITIALIZE Ws-Exc-Ca
     MOVE "RAISE" TO XA-FUNCTION
     MOVE CA-COMPANY TO XA-COMPANY
     MOVE KR-SITE TO XA-SITE
     MOVE CA-USER TO XA-USER
     MOVE CA-BUSINESS-DATE TO XA-BUSINESS-DATE
     MOVE "KIT" TO EX-MODULE
     MOVE "ERROR" TO EX-SEVERITY
     MOVE "KIT RESERVATION INVALID" TO EX-CATEGORY
     MOVE CA-MESSAGE (1:200) TO EX-MESSAGE-TEXT
     MOVE "EXECUTIONKIT" TO EX-DOCUMENT-TYPE
     MOVE KR-KIT-REF TO EX-DOCUMENT-REF
     MOVE KH-OWNER-USER TO EX-OWNER-USER
     MOVE 2 TO EX-PRIORITY
     CALL "EXCPRAIS" USING Ws-Exc-Ca
     MOVE EX-EXCEPTION-ID TO KR-EXCEPTION-ID.

*>   KIT-024 - who did it, when, why, and against which kit.
 Audit-Reservation-Event.
     INITIALIZE Ws-Audit-Ca
     MOVE "WRITE" TO AA-FUNCTION
     MOVE CA-COMPANY TO AA-COMPANY
     MOVE KR-SITE TO AA-SITE
     MOVE CA-USER TO AA-USER
     MOVE CA-BUSINESS-DATE TO AA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO AA-CORRELATION-ID
     MOVE "KIT.EXECUTIONKIT" TO AU-ENTITY-TYPE
     MOVE KR-KIT-REF TO AU-ENTITY-KEY
     MOVE CA-FUNCTION TO AU-ACTION
     MOVE "ReservedOrPicked" TO AU-FIELD-NAME
     MOVE KR-ITEM-CODE TO AU-OLD-VALUE
     MOVE KR-QUANTITY TO Ws-Edit
     MOVE Ws-Edit TO AU-NEW-VALUE
     MOVE KR-REASON-TEXT (1:200) TO AU-REASON
     MOVE KR-KIT-REF TO AU-AUTHORITY-REF
     MOVE "KITRESV" TO AU-SOURCE
     MOVE "N" TO AU-SENSITIVE
     CALL "AUDITLOG" USING Ws-Audit-Ca.
