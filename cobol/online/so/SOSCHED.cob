      *>================================================================
      *> PROGRAM-ID : SOSCHED
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : DELIVERY SCHEDULES FOR ONE SALES ORDER LINE.  A
      *>              LINE MAY BE SPLIT ACROSS DATES AND DESTINATIONS,
      *>              BUT THE SCHEDULE TOTAL ALWAYS EQUALS THE ACTIVE
      *>              ORDER QUANTITY.  THE REQUESTED DATE AND THE
      *>              ORIGINAL PROMISE ARE SERVICE EVIDENCE: ONLY THE
      *>              CURRENT PROMISE MOVES, AND ONLY WITH A REASON
      *>              AND A BUMPED CHANGE COUNT, SO LATE DELIVERY
      *>              CANNOT BE IMPROVED BY EDITING A DATE.
      *> CALLED BY  : ORDER ENTRY, CUSTOMER SERVICE, .NET CLIENT
      *> INTERFACE  : CALL "SOSCHED" USING WS-SOSCHED-CA
      *>              COPYBOOK SOSCHEDCA
      *> CALLS      : SQLIO, AUDITLOG
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-11-20 RJH  CR0392 CALL-OFF SCHEDULES BY DATE
      *> 1999-11-30 KMP  Y2K - PROMISE DATES HOLD FOUR DIGIT YEARS
      *> 2013-06-03 DLS  CR2688 KEEP THE ORIGINAL PROMISE FOR OTIF
      *> 2026-10-06 STERP SO-008 SCHEDULE TOTAL EQUALS ACTIVE QUANTITY
      *>================================================================
      *> REQ: SO-008
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOSCHED.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 20/11/1995.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUD-CA==.
       01  WS-CTL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BUSDATE              PIC 9(8).
           05  WS-CORREL               PIC X(36).
           05  WS-RC                   PIC 9(2).
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
           05  WS-MSGCNT               PIC 9(2).
           05  WS-TRAN-OPEN            PIC X.
           05  WS-I                    PIC S9(4).
           05  WS-J                    PIC S9(4).
           05  WS-P                    PIC S9(4).
           05  WS-NEXT-NO              PIC S9(9).
           05  WS-MATCHED              PIC X.
           05  WS-SPLIT-TOT            PIC S9(13)V9(6).
           05  WS-KEEP-NO              PIC S9(9).
       01  WS-EDITS.
           05  WS-QTY-ED               PIC -(13)9.999999.
           05  WS-INT-ED               PIC -(9)9.
           05  WS-UK-QTY               PIC Z,ZZZ,ZZ9.999999-.
           05  WS-UK-QTY2              PIC Z,ZZZ,ZZ9.999999-.
           05  WS-DATE-ED              PIC 9(8).
      *>   the order line this schedule belongs to
       01  WS-LROW.
           05  WS-L-ID                 PIC X(24).
           05  WS-L-ACCEPTED           PIC X(24).
           05  WS-L-CANCELLED          PIC X(24).
           05  WS-L-DESPATCHED         PIC X(24).
           05  WS-L-ITEM               PIC X(40).
           05  WS-L-UOM                PIC X(10).
      *>   one so.DeliverySchedule row as fetched
       01  WS-SROW.
           05  WS-S-ID                 PIC X(24).
           05  WS-S-NO                 PIC X(10).
           05  WS-S-QTY                PIC X(24).
           05  WS-S-DESP               PIC X(24).
           05  WS-S-SITE               PIC X(10).
           05  WS-S-ADDR               PIC X(10).
           05  WS-S-REQ                PIC X(8).
           05  WS-S-ORIG               PIC X(8).
           05  WS-S-CURR               PIC X(8).
           05  WS-S-CHGCNT             PIC X(10).
           05  WS-S-REASON             PIC X(200).
           05  WS-S-METHOD             PIC X(12).
           05  WS-S-PRIO               PIC X(3).
           05  WS-S-STATUS             PIC X(10).
       01  WS-TWO.
           05  WS-T1                   PIC X(30).
           05  WS-T2                   PIC X(30).
       LINKAGE SECTION.
       COPY SOSCHEDCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-INPUT
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-ORDER-LINE
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           EVALUATE WS-FUNC
               WHEN "SPLIT"     PERFORM 1000-SPLIT-LINE
               WHEN "REPROMIS"  PERFORM 2000-REPROMISE
               WHEN "LIST"      PERFORM 0400-LIST-SCHEDULES
               WHEN "CANCEL"    PERFORM 4000-CANCEL-SCHEDULE
               WHEN OTHER       PERFORM 9100-BAD-FUNCTION
           END-EVALUATE
           IF WS-RC > 4
               PERFORM 9500-ROLLBACK
           END-IF.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-CTL
           INITIALIZE WS-EDITS
           INITIALIZE WS-LROW
           INITIALIZE WS-SROW
           INITIALIZE WS-TWO
           MOVE "N" TO WS-TRAN-OPEN
           MOVE 0 TO WS-RC WS-MSGCNT
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE 0 TO WS-SPLIT-TOT WS-NEXT-NO WS-KEEP-NO
           MOVE CA-FUNCTION OF DFHCOMMAREA TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORREL
           MOVE "SOSCHED" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE "SO-008" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO SS-SCHED-CREATED SS-SCHED-CLOSED
           MOVE 0 TO SS-VARIANCE-QTY SS-SCHEDULE-TOTAL
           MOVE 0 TO SS-LIST-CNT
           IF SS-SPLIT-CNT > 20
               MOVE 20 TO SS-SPLIT-CNT
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE-INPUT SECTION.
       0200-START.
           IF WS-CO = SPACES OR SS-ORDER-NUMBER = SPACES OR
              SS-LINE-NUM = 0
               MOVE "SCH0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A company, an order number and a line number "
                      "are required" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   the ACTIVE order quantity is accepted less cancelled; the
      *>   schedules must always add up to exactly that
       0300-READ-ORDER-LINE SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), l.SalesOrderLineId), "
                  "CONVERT(varchar(24), l.AcceptedQty), "
                  "CONVERT(varchar(24), l.CancelledQty), "
                  "CONVERT(varchar(24), l.DespatchedQty), "
                  "ISNULL(l.ItemCode,''), ISNULL(l.UomCode,'') FROM "
                  "so.SalesOrderLine l WHERE l.CompanyCode = ? AND "
                  "l.OrderNumber = ? AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SS-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE SS-LINE-NUM TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 40 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "SCH0003" TO WS-REASON
               MOVE SS-LINE-NUM TO WS-INT-ED
               MOVE SPACES TO WS-MSG
               STRING "Line " FUNCTION TRIM(WS-INT-ED) " of order "
                      FUNCTION TRIM(SS-ORDER-NUMBER)
                      " is not held" DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 0300-EXIT
           END-IF
           COMPUTE SS-ORDER-LINE-ID = FUNCTION NUMVAL(WS-L-ID)
           COMPUTE SS-ACCEPTED-QTY = FUNCTION NUMVAL(WS-L-ACCEPTED)
           COMPUTE SS-CANCELLED-QTY = FUNCTION NUMVAL(WS-L-CANCELLED)
           COMPUTE SS-DESPATCHED-QTY =
                   FUNCTION NUMVAL(WS-L-DESPATCHED)
           COMPUTE SS-ACTIVE-QTY = SS-ACCEPTED-QTY - SS-CANCELLED-QTY
           MOVE WS-L-ITEM TO SS-ITEM-CODE
           MOVE WS-L-UOM TO SS-UOM-CODE.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   LIST also refreshes the totals used by every other entry
       0400-LIST-SCHEDULES SECTION.
       0400-START.
           MOVE 0 TO SS-LIST-CNT SS-SCHEDULE-TOTAL
           MOVE 0 TO WS-NEXT-NO
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), s.ScheduleId), "
                  "CONVERT(varchar(10), s.ScheduleNo), "
                  "CONVERT(varchar(24), s.Quantity), "
                  "CONVERT(varchar(24), s.DespatchedQty), "
                  "ISNULL(s.SiteCode,''), "
                  "ISNULL(s.DeliveryAddressRef,''), "
                  "CONVERT(char(8), s.RequestedDate, 112), "
                  "CONVERT(char(8), s.OriginalPromiseDate, 112), "
                  "CONVERT(char(8), s.CurrentPromiseDate, 112), "
                  "CONVERT(varchar(10), s.ChangeCount), "
                  "ISNULL(s.ChangeReason,''), s.FulfilmentMethod, "
                  "CONVERT(varchar(3), s.Priority), s.Status FROM "
                  "so.DeliverySchedule s WHERE s.SalesOrderLineId = ?"
                  " ORDER BY s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE SS-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 8 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-COL-WIDTH(9)
           MOVE 10 TO SQLIO-COL-WIDTH(10)
           MOVE 200 TO SQLIO-COL-WIDTH(11)
           MOVE 12 TO SQLIO-COL-WIDTH(12)
           MOVE 3 TO SQLIO-COL-WIDTH(13)
           MOVE 10 TO SQLIO-COL-WIDTH(14)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SROW
           PERFORM UNTIL SQLIO-SQLCODE NOT = 0 OR SS-LIST-CNT >= 20
               ADD 1 TO SS-LIST-CNT
               MOVE SS-LIST-CNT TO WS-I
               PERFORM 0450-STORE-ONE-SCHEDULE
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-SROW
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SROW
           COMPUTE SS-VARIANCE-QTY =
                   SS-SCHEDULE-TOTAL - SS-ACTIVE-QTY
           IF WS-FUNC = "LIST"
               PERFORM 0500-REPORT-THE-TOTAL
           END-IF.
       0400-EXIT.
           EXIT.

       0450-STORE-ONE-SCHEDULE SECTION.
       0450-START.
           COMPUTE SS-X-SCHED-ID(WS-I) = FUNCTION NUMVAL(WS-S-ID)
           COMPUTE SS-X-SCHED-NO(WS-I) = FUNCTION NUMVAL(WS-S-NO)
           COMPUTE SS-X-QTY(WS-I) = FUNCTION NUMVAL(WS-S-QTY)
           COMPUTE SS-X-DESPATCHED(WS-I) = FUNCTION NUMVAL(WS-S-DESP)
           MOVE WS-S-SITE TO SS-X-SITE(WS-I)
           MOVE WS-S-ADDR TO SS-X-ADDRESS(WS-I)
           COMPUTE SS-X-REQUESTED(WS-I) = FUNCTION NUMVAL(WS-S-REQ)
           COMPUTE SS-X-ORIG-PROM(WS-I) = FUNCTION NUMVAL(WS-S-ORIG)
           COMPUTE SS-X-CURR-PROM(WS-I) = FUNCTION NUMVAL(WS-S-CURR)
           COMPUTE SS-X-CHANGE-CNT(WS-I) =
                   FUNCTION NUMVAL(WS-S-CHGCNT)
           MOVE WS-S-REASON TO SS-X-REASON(WS-I)
           MOVE WS-S-METHOD TO SS-X-METHOD(WS-I)
           COMPUTE SS-X-PRIORITY(WS-I) = FUNCTION NUMVAL(WS-S-PRIO)
           MOVE WS-S-STATUS TO SS-X-STATUS(WS-I)
           IF SS-X-SCHED-NO(WS-I) > WS-NEXT-NO
               MOVE SS-X-SCHED-NO(WS-I) TO WS-NEXT-NO
           END-IF
           IF WS-S-STATUS NOT = "CANCELLED"
               ADD SS-X-QTY(WS-I) TO SS-SCHEDULE-TOTAL
           END-IF.
       0450-EXIT.
           EXIT.

       0500-REPORT-THE-TOTAL SECTION.
       0500-START.
           MOVE SS-SCHEDULE-TOTAL TO WS-UK-QTY
           MOVE SS-ACTIVE-QTY TO WS-UK-QTY2
           MOVE SPACES TO WS-MSG
           STRING "Schedules total " FUNCTION TRIM(WS-UK-QTY)
                  " against an active order quantity of "
                  FUNCTION TRIM(WS-UK-QTY2)
               DELIMITED BY SIZE INTO WS-MSG
           IF SS-VARIANCE-QTY NOT = 0
               MOVE 4 TO WS-RC
               MOVE "SCH0002" TO WS-REASON
               ADD 1 TO WS-MSGCNT
           END-IF.
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   SPLIT - SO-008.  The requested rows become the line's
      *>   schedules.  Nothing is written until the split adds up.
       1000-SPLIT-LINE SECTION.
       1000-START.
           IF SS-SPLIT-CNT = 0
               MOVE "SCH0007" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "At least one schedule row is required to "
                      "split a line" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           PERFORM 0400-LIST-SCHEDULES
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-VALIDATE-SPLITS
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-CHECK-SPLIT-TOTAL
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 1300-PROTECT-DESPATCHED
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 8000-BEGIN-TRAN
           PERFORM 1400-CLOSE-UNWANTED
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > SS-SPLIT-CNT OR WS-RC > 4
               PERFORM 1500-WRITE-ONE-SPLIT
           END-PERFORM
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           PERFORM 0400-LIST-SCHEDULES
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SS-SCHEDULE-TOTAL TO WS-UK-QTY
           MOVE SS-SPLIT-CNT TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           STRING "Line split across " FUNCTION TRIM(WS-INT-ED)
                  " schedules totalling " FUNCTION TRIM(WS-UK-QTY)
               DELIMITED BY SIZE INTO WS-MSG.
       1000-EXIT.
           EXIT.

       1100-VALIDATE-SPLITS SECTION.
       1100-START.
           MOVE 0 TO WS-SPLIT-TOT
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > SS-SPLIT-CNT OR WS-RC > 4
               IF SS-S-QTY(WS-I) <= 0
                   MOVE "SCH0007" TO WS-REASON
                   MOVE WS-I TO WS-INT-ED
                   MOVE SPACES TO WS-MSG
                   STRING "Schedule row " FUNCTION TRIM(WS-INT-ED)
                          " has a zero or negative quantity"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT
               END-IF
               IF SS-S-REQUESTED(WS-I) = 0
                   IF SS-REQUESTED-DATE = 0
                       MOVE WS-BUSDATE TO SS-S-REQUESTED(WS-I)
                   ELSE
                       MOVE SS-REQUESTED-DATE TO SS-S-REQUESTED(WS-I)
                   END-IF
               END-IF
               IF SS-S-PROMISE(WS-I) = 0
                   MOVE SS-S-REQUESTED(WS-I) TO SS-S-PROMISE(WS-I)
               END-IF
               IF SS-S-PROMISE(WS-I) < SS-S-REQUESTED(WS-I) AND
                  SS-S-REASON(WS-I) = SPACES
                   PERFORM 1150-EARLY-WITHOUT-REASON
               END-IF
               IF SS-S-SITE(WS-I) = SPACES
                   MOVE WS-SITE TO SS-S-SITE(WS-I)
               END-IF
               PERFORM 1180-CHECK-SITE
               IF SS-S-METHOD(WS-I) = SPACES
                   MOVE "WAREHOUSE" TO SS-S-METHOD(WS-I)
               END-IF
               IF SS-S-PRIORITY(WS-I) = 0
                   MOVE 5 TO SS-S-PRIORITY(WS-I)
               END-IF
               ADD SS-S-QTY(WS-I) TO WS-SPLIT-TOT
           END-PERFORM.
       1100-EXIT.
           EXIT.

      *>   SCH0005 - a promise earlier than the customer's requested
      *>   date is not an error, but it must say why
       1150-EARLY-WITHOUT-REASON SECTION.
       1150-START.
           MOVE "SCH0005" TO WS-REASON
           MOVE SS-S-PROMISE(WS-I) TO WS-DATE-ED
           MOVE SPACES TO WS-MSG
           STRING "Promise date " WS-DATE-ED
                  " is before the requested date on schedule row "
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-I TO WS-INT-ED
           STRING FUNCTION TRIM(WS-MSG) " "
                  FUNCTION TRIM(WS-INT-ED) " - state a reason"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT.
       1150-EXIT.
           EXIT.

      *>   SCH0006 - a destination must be a site of this company
       1180-CHECK-SITE SECTION.
       1180-START.
           IF WS-RC > 4
               GO TO 1180-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SiteCode, '' FROM core.Site WHERE "
                  "CompanyCode = ? AND SiteCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SS-S-SITE(WS-I) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1180-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "SCH0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Destination " FUNCTION TRIM(SS-S-SITE(WS-I))
                      " is not a site of company "
                      FUNCTION TRIM(WS-CO)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       1180-EXIT.
           EXIT.

      *>   SCH0002 - the rule that makes schedules trustworthy
       1200-CHECK-SPLIT-TOTAL SECTION.
       1200-START.
           IF WS-SPLIT-TOT = SS-ACTIVE-QTY
               GO TO 1200-EXIT
           END-IF
           MOVE "SCH0002" TO WS-REASON
           MOVE WS-SPLIT-TOT TO WS-UK-QTY
           MOVE SS-ACTIVE-QTY TO WS-UK-QTY2
           MOVE SPACES TO WS-MSG
           STRING "The schedule total " FUNCTION TRIM(WS-UK-QTY)
                  " does not equal the active order quantity "
                  FUNCTION TRIM(WS-UK-QTY2)
               DELIMITED BY SIZE INTO WS-MSG
           COMPUTE SS-VARIANCE-QTY = WS-SPLIT-TOT - SS-ACTIVE-QTY
           PERFORM 9000-REJECT.
       1200-EXIT.
           EXIT.

      *>   SCH0010 - a confirmed despatch is never re-planned away
       1300-PROTECT-DESPATCHED SECTION.
       1300-START.
           PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > SS-LIST-CNT OR WS-RC > 4
               IF SS-X-DESPATCHED(WS-J) > 0
                   MOVE "N" TO WS-MATCHED
                   PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I > SS-SPLIT-CNT
                       IF SS-S-SCHED-NO(WS-I) =
                          SS-X-SCHED-NO(WS-J) AND
                          SS-S-QTY(WS-I) >= SS-X-DESPATCHED(WS-J)
                           MOVE "Y" TO WS-MATCHED
                       END-IF
                   END-PERFORM
                   IF WS-MATCHED NOT = "Y"
                       PERFORM 1350-DESPATCH-PROTECTED
                   END-IF
               END-IF
           END-PERFORM.
       1300-EXIT.
           EXIT.

       1350-DESPATCH-PROTECTED SECTION.
       1350-START.
           MOVE "SCH0010" TO WS-REASON
           MOVE SS-X-SCHED-NO(WS-J) TO WS-INT-ED
           MOVE SS-X-DESPATCHED(WS-J) TO WS-UK-QTY
           MOVE SPACES TO WS-MSG
           STRING "Schedule " FUNCTION TRIM(WS-INT-ED)
                  " has already despatched "
                  FUNCTION TRIM(WS-UK-QTY)
                  " - the split must retain at least that quantity"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT.
       1350-EXIT.
           EXIT.

      *>   open schedules the split no longer mentions are cancelled,
      *>   never silently deleted
       1400-CLOSE-UNWANTED SECTION.
       1400-START.
           PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > SS-LIST-CNT OR WS-RC > 4
               IF SS-X-STATUS(WS-J) NOT = "CANCELLED"
                   MOVE "N" TO WS-MATCHED
                   PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I > SS-SPLIT-CNT
                       IF SS-S-SCHED-NO(WS-I) = SS-X-SCHED-NO(WS-J)
                           MOVE "Y" TO WS-MATCHED
                       END-IF
                   END-PERFORM
                   IF WS-MATCHED NOT = "Y"
                       MOVE SS-X-SCHED-ID(WS-J) TO SS-SCHEDULE-ID
                       PERFORM 4200-CLOSE-ONE-SCHEDULE
                       ADD 1 TO SS-SCHED-CLOSED
                   END-IF
               END-IF
           END-PERFORM.
       1400-EXIT.
           EXIT.

      *>   an existing schedule number is updated in place so its
      *>   requested and original promise dates survive; a new one is
      *>   inserted with all three dates set for the first time
       1500-WRITE-ONE-SPLIT SECTION.
       1500-START.
           IF SS-S-SCHED-NO(WS-I) > 0
               MOVE "N" TO WS-MATCHED
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > SS-LIST-CNT
                   IF SS-X-SCHED-NO(WS-J) = SS-S-SCHED-NO(WS-I)
                       MOVE "Y" TO WS-MATCHED
                   END-IF
               END-PERFORM
               IF WS-MATCHED = "Y"
                   PERFORM 1600-UPDATE-SPLIT-ROW
                   GO TO 1500-EXIT
               END-IF
           ELSE
               ADD 1 TO WS-NEXT-NO
               MOVE WS-NEXT-NO TO SS-S-SCHED-NO(WS-I)
           END-IF
           PERFORM 1700-INSERT-SPLIT-ROW.
       1500-EXIT.
           EXIT.

       1600-UPDATE-SPLIT-ROW SECTION.
       1600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET Quantity = "
                  "CONVERT(decimal(19,6), ?), SiteCode = ?, "
                  "DeliveryAddressRef = NULLIF(?,''), "
                  "CurrentPromiseDate = CONVERT(date, ?, 112), "
                  "FulfilmentMethod = ?, Priority = CONVERT(tinyint, "
                  "?), Status = 'OPEN' WHERE SalesOrderLineId = ? "
                  "AND ScheduleNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE SS-S-QTY(WS-I) TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(1)
           MOVE SS-S-SITE(WS-I) TO SQLIO-PARM-VAL(2)
           MOVE SS-S-ADDRESS(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE SS-S-PROMISE(WS-I) TO SQLIO-PARM-VAL(4)
           MOVE SS-S-METHOD(WS-I) TO SQLIO-PARM-VAL(5)
           MOVE SS-S-PRIORITY(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(6)
           MOVE SS-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE SS-S-SCHED-NO(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(8)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       1600-EXIT.
           EXIT.

       1700-INSERT-SPLIT-ROW SECTION.
       1700-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.DeliverySchedule ("
                  "SalesOrderLineId, CompanyCode, OrderNumber, "
                  "LineNum, ScheduleNo, Quantity, DespatchedQty, "
                  "SiteCode, DeliveryAddressRef, RequestedDate, "
                  "OriginalPromiseDate, CurrentPromiseDate, "
                  "ChangeReason, ChangeCount, FulfilmentMethod, "
                  "Priority, Status) VALUES (?, ?, ?, "
                  "CONVERT(int, ?), CONVERT(int, ?), "
                  "CONVERT(decimal(19,6), ?), 0, ?, NULLIF(?,''), "
                  "CONVERT(date, ?, 112), CONVERT(date, ?, 112), "
                  "CONVERT(date, ?, 112), NULLIF(?,''), 0, ?, "
                  "CONVERT(tinyint, ?), 'OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 14 TO SQLIO-PARM-CNT
           MOVE SS-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE SS-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE SS-LINE-NUM TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE SS-S-SCHED-NO(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE SS-S-QTY(WS-I) TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(6)
           MOVE SS-S-SITE(WS-I) TO SQLIO-PARM-VAL(7)
           MOVE SS-S-ADDRESS(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE SS-S-REQUESTED(WS-I) TO SQLIO-PARM-VAL(9)
           MOVE SS-S-PROMISE(WS-I) TO SQLIO-PARM-VAL(10)
           MOVE SS-S-PROMISE(WS-I) TO SQLIO-PARM-VAL(11)
           MOVE SS-S-REASON(WS-I) TO SQLIO-PARM-VAL(12)
           MOVE SS-S-METHOD(WS-I) TO SQLIO-PARM-VAL(13)
           MOVE SS-S-PRIORITY(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(14)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 14
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC < 8
               ADD 1 TO SS-SCHED-CREATED
           END-IF.
       1700-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REPROMISE - SO-008.  The UPDATE deliberately names only
      *>   CurrentPromiseDate, ChangeReason and ChangeCount.
      *>   RequestedDate and OriginalPromiseDate are never in the SET
      *>   list, so the original service evidence survives every
      *>   re-promise and OTIF cannot be improved by editing a date.
       2000-REPROMISE SECTION.
       2000-START.
           IF SS-SCHEDULE-NO = 0
               MOVE "SCH0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A schedule number is required to re-promise"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           PERFORM 0400-LIST-SCHEDULES
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-FIND-SCHEDULE
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           IF SS-NEW-PROMISE = 0
               MOVE "SCH0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A new promise date is required"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF SS-NEW-PROMISE < SS-REQUESTED-DATE AND
              SS-CHANGE-REASON = SPACES
               MOVE "SCH0005" TO WS-REASON
               MOVE SS-NEW-PROMISE TO WS-DATE-ED
               MOVE SPACES TO WS-MSG
               STRING "Promise date " WS-DATE-ED
                      " is before the customer's requested date - "
                      "state a reason"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF SS-CHANGE-REASON = SPACES
               MOVE "SCH0009" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A change reason is required before a promise "
                      "date is moved" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET "
                  "CurrentPromiseDate = CONVERT(date, ?, 112), "
                  "ChangeReason = ?, ChangeCount = ChangeCount + 1 "
                  "WHERE SalesOrderLineId = ? AND ScheduleNo = ? AND "
                  "Status <> 'CANCELLED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE SS-NEW-PROMISE TO SQLIO-PARM-VAL(1)
           MOVE SS-CHANGE-REASON TO SQLIO-PARM-VAL(2)
           MOVE SS-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE SS-SCHEDULE-NO TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE SS-NEW-PROMISE TO SS-CURR-PROMISE
           ADD 1 TO SS-CHANGE-COUNT
           PERFORM 0400-LIST-SCHEDULES
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SS-ORIG-PROMISE TO WS-DATE-ED
           MOVE SPACES TO WS-MSG
           STRING "Promise moved to " SS-NEW-PROMISE
                  "; requested and original promise " WS-DATE-ED
                  " are unchanged"
               DELIMITED BY SIZE INTO WS-MSG.
       2000-EXIT.
           EXIT.

       2100-FIND-SCHEDULE SECTION.
       2100-START.
           MOVE "N" TO WS-MATCHED
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > SS-LIST-CNT
               IF SS-X-SCHED-NO(WS-J) = SS-SCHEDULE-NO
                   MOVE "Y" TO WS-MATCHED
                   MOVE SS-X-SCHED-ID(WS-J) TO SS-SCHEDULE-ID
                   MOVE SS-X-QTY(WS-J) TO SS-QUANTITY
                   MOVE SS-X-REQUESTED(WS-J) TO SS-REQUESTED-DATE
                   MOVE SS-X-ORIG-PROM(WS-J) TO SS-ORIG-PROMISE
                   MOVE SS-X-CURR-PROM(WS-J) TO SS-CURR-PROMISE
                   MOVE SS-X-CHANGE-CNT(WS-J) TO SS-CHANGE-COUNT
                   MOVE SS-X-SITE(WS-J) TO SS-SITE-CODE
                   MOVE SS-X-STATUS(WS-J) TO SS-STATUS
                   MOVE SS-X-DESPATCHED(WS-J) TO SS-DESPATCHED-QTY
               END-IF
           END-PERFORM
           IF WS-MATCHED NOT = "Y"
               MOVE 16 TO WS-RC
               MOVE "SCH0004" TO WS-REASON
               MOVE SS-SCHEDULE-NO TO WS-INT-ED
               MOVE SPACES TO WS-MSG
               STRING "Schedule " FUNCTION TRIM(WS-INT-ED)
                      " is not held on this order line"
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CANCEL - only the undespatched part of one schedule
       4000-CANCEL-SCHEDULE SECTION.
       4000-START.
           IF SS-SCHEDULE-NO = 0
               MOVE "SCH0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A schedule number is required to cancel"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           PERFORM 0400-LIST-SCHEDULES
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           PERFORM 2100-FIND-SCHEDULE
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           IF SS-STATUS = "CANCELLED"
               MOVE 4 TO WS-RC
               MOVE "SCH0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "That schedule is already cancelled"
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 4000-EXIT
           END-IF
           PERFORM 8000-BEGIN-TRAN
           PERFORM 4200-CLOSE-ONE-SCHEDULE
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           ADD 1 TO SS-SCHED-CLOSED
           PERFORM 0400-LIST-SCHEDULES
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SS-DESPATCHED-QTY TO WS-UK-QTY
           MOVE SPACES TO WS-MSG
           STRING "Schedule cancelled; the despatched quantity "
                  FUNCTION TRIM(WS-UK-QTY) " is retained"
               DELIMITED BY SIZE INTO WS-MSG.
       4000-EXIT.
           EXIT.

      *>   a schedule that has despatched something is completed at
      *>   that quantity; one that has not is cancelled outright
       4200-CLOSE-ONE-SCHEDULE SECTION.
       4200-START.
           IF WS-RC > 4
               GO TO 4200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET Quantity = "
                  "DespatchedQty, Status = CASE WHEN DespatchedQty > "
                  "0 THEN 'COMPLETE' ELSE 'CANCELLED' END, "
                  "ChangeReason = ISNULL(NULLIF(?,''), ChangeReason) "
                  "WHERE ScheduleId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE SS-CANCEL-REASON TO SQLIO-PARM-VAL(1)
           MOVE SS-SCHEDULE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 8200-EXEC-NO-ROWS.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       7500-AUDIT-THE-CHANGE SECTION.
       7500-START.
           IF WS-RC > 4
               GO TO 7500-EXIT
           END-IF
           INITIALIZE WS-AUD-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUD-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUD-CA
           MOVE WS-SITE TO CA-SITE OF WS-AUD-CA
           MOVE WS-USER TO CA-USER OF WS-AUD-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-AUD-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-AUD-CA
           MOVE "so.DeliverySchedule" TO AU-ENTITY-TYPE
           MOVE SS-LINE-NUM TO WS-INT-ED
           MOVE SPACES TO AU-ENTITY-KEY
           STRING FUNCTION TRIM(WS-CO) "/"
                  FUNCTION TRIM(SS-ORDER-NUMBER) "/"
                  FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO AU-ENTITY-KEY
           MOVE WS-FUNC TO AU-ACTION
           MOVE "CurrentPromiseDate" TO AU-FIELD-NAME
           MOVE SS-ORIG-PROMISE TO AU-OLD-VALUE
           MOVE SS-CURR-PROMISE TO AU-NEW-VALUE
           MOVE SS-CHANGE-REASON TO AU-REASON
           IF AU-REASON = SPACES
               MOVE SS-CANCEL-REASON TO AU-REASON
           END-IF
           MOVE SPACES TO AU-AUTHORITY-REF
           MOVE "SOSCHED" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUD-CA.
       7500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-BEGIN-TRAN SECTION.
       8000-START.
           IF WS-RC > 4 OR WS-TRAN-OPEN = "Y"
               GO TO 8000-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           ELSE
               MOVE "Y" TO WS-TRAN-OPEN
           END-IF.
       8000-EXIT.
           EXIT.

       8100-COMMIT-TRAN SECTION.
       8100-START.
           IF WS-TRAN-OPEN NOT = "Y"
               GO TO 8100-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE "COMT" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           END-IF
           MOVE "N" TO WS-TRAN-OPEN.
       8100-EXIT.
           EXIT.

       8200-EXEC-NO-ROWS SECTION.
       8200-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 8200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO.
       8200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO WS-RC
           ADD 1 TO WS-MSGCNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO WS-RC
           MOVE "SCH0008" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "SOSCHED does not support function ["
                  FUNCTION TRIM(WS-FUNC) "]"
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG TO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9300-EXIT.
           EXIT.

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TWO
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9500-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE WS-MSGCNT TO CA-MSG-COUNT OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
