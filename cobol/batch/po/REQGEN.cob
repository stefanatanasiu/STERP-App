      *>================================================================
      *> PROGRAM-ID : REQGEN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : REPLENISHMENT AND SHORTAGE REQUISITION
      *>              GENERATION (PO-001).
      *>              THE RUN LOOKS FOR TWO KINDS OF DEMAND:
      *>                REPLEN    free stock plus goods already on
      *>                          order has fallen to or below the
      *>                          re-order point for the site
      *>                SHORTAGE  more stock is already reserved than
      *>                          is physically on hand
      *>              AND PROPOSES A REQUISITION LINE FOR EACH.
      *>              A REQUISITION IS A JUSTIFIED NEED, NOT A
      *>              PURCHASE.  THIS PROGRAM THEREFORE WRITES
      *>              NOTHING TO po.PurchaseOrder AND NOTHING TO
      *>              po.Commitment.  EVERY LINE IS PUT THROUGH THE
      *>              POREQ SERVICE, SO THE SAME VALIDATION, DEMAND
      *>              OWNERSHIP AND NUMBERING APPLY AS WHEN A
      *>              REQUISITION IS RAISED BY HAND, AND A BUYER
      *>              STILL HAS TO SOURCE IT AND APPROVE IT BEFORE
      *>              ANYONE IS COMMITTED TO A SUPPLIER.
      *>              MODE=PREVIEW prints the proposal and writes
      *>              nothing at all; MODE=COMMIT creates the
      *>              requisitions.  An item that already has an open
      *>              requisition line is skipped either way, so the
      *>              run may be repeated without stacking up
      *>              duplicate demand.
      *> RUN AS     : REQGEN CO=STC [SITE=RDG] [ITEM=...]
      *>              [MODE=PREVIEW|COMMIT] USER=BUYER1
      *> OUTPUT     : run\spool\REQGEN-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, DATECALC, SQLIO,
      *>              POREQ
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-01-25 RJH  CR0812 MIN/MAX SUGGESTED ORDER RUN
      *> 2004-08-11 KMP  CR1101 PREVIEW MODE ADDED AFTER A NIGHT RUN
      *>                 RAISED 1,400 REQUISITIONS NOBODY WANTED
      *> 2026-10-06 STERP PO-001 THROUGH POREQ, NO COMMITMENT
      *>================================================================
      *> REQ: PO-001
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REQGEN.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 25/01/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-REQGEN.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY STDATES.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC==.
       COPY POREQCA REPLACING ==DFHCOMMAREA== BY ==WS-POREQ==.
       01  PO-ROW.
           05  PO-COL OCCURS 30 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(8) VALUE "PREVIEW".
               88  WS-PREVIEW                    VALUE "PREVIEW".
               88  WS-COMMIT                     VALUE "COMMIT".
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-ROWS-READ            PIC S9(9) VALUE 0.
           05  WS-WORK-DATE            PIC 9(8) VALUE 0.
           05  WS-GROUP-KEY            PIC X(20) VALUE SPACES.
           05  WS-PREV-GROUP           PIC X(20) VALUE SPACES.
           05  WS-REQ-OPEN             PIC X VALUE "N".
           05  WS-CUR-REQ              PIC X(30) VALUE SPACES.
           05  WS-CUR-LINE             PIC S9(9) VALUE 0.
           05  WS-LEAD-DAYS            PIC S9(4) VALUE 0.
           05  WS-FREE                 PIC S9(13)V9(6) VALUE 0.
           05  WS-ON-ORDER             PIC S9(13)V9(6) VALUE 0.
           05  WS-ROP                  PIC S9(13)V9(6) VALUE 0.
           05  WS-MAXQ                 PIC S9(13)V9(6) VALUE 0.
           05  WS-MINQ                 PIC S9(13)V9(6) VALUE 0.
           05  WS-MULT                 PIC S9(13)V9(6) VALUE 0.
           05  WS-PROJECTED            PIC S9(13)V9(6) VALUE 0.
           05  WS-TARGET               PIC S9(13)V9(6) VALUE 0.
           05  WS-PACKS                PIC S9(13) VALUE 0.
           05  WS-PCNT                 PIC S9(5) VALUE 0.
           05  WS-PIX                  PIC S9(5) VALUE 0.
      *>   Proposals are extracted first and raised afterwards.  The
      *>   SQLIO cursors are a shared resource, so POREQ must never be
      *>   called while this program is still holding one open.
       01  WS-PROP-TABLE.
           05  WS-PROP OCCURS 2000 TIMES.
               10  P-SITE              PIC X(10).
               10  P-ITEM              PIC X(20).
               10  P-DESC              PIC X(22).
               10  P-FREE              PIC S9(11)V9(3).
               10  P-ON-ORDER          PIC S9(11)V9(3).
               10  P-ROP               PIC S9(11)V9(3).
               10  P-SUGGEST           PIC S9(11)V9(3).
               10  P-NEEDED-N          PIC 9(8).
               10  P-EST-VALUE         PIC S9(11)V99.
               10  P-PRICE             PIC S9(11)V9(6).
               10  P-SUPPLIER          PIC X(12).
               10  P-DEMAND            PIC X(8).
               10  P-UOM               PIC X(10).
       01  WS-DET.
           05  D-SITE                  PIC X(10) VALUE SPACES.
           05  D-ITEM                  PIC X(20) VALUE SPACES.
           05  D-DESC                  PIC X(22) VALUE SPACES.
           05  D-FREE                  PIC S9(11)V9(3) VALUE 0.
           05  D-ON-ORDER              PIC S9(11)V9(3) VALUE 0.
           05  D-ROP                   PIC S9(11)V9(3) VALUE 0.
           05  D-SUGGEST               PIC S9(11)V9(3) VALUE 0.
           05  D-NEEDED-BY             PIC X(10) VALUE SPACES.
           05  D-EST-VALUE             PIC S9(11)V99 VALUE 0.
           05  D-SUPPLIER              PIC X(12) VALUE SPACES.
           05  D-DEMAND                PIC X(8) VALUE SPACES.
           05  D-DEMAND-TX             PIC X(6) VALUE SPACES.
           05  D-OUTCOME               PIC X(14) VALUE SPACES.
           05  D-ONE                   PIC 9 VALUE 1.
       01  WS-TOT.
           05  T-PROPOSED              PIC S9(7) VALUE 0.
           05  T-RAISED                PIC S9(7) VALUE 0.
           05  T-REFUSED               PIC S9(7) VALUE 0.
           05  T-SHORTAGE              PIC S9(7) VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
           05  T-REQS                  PIC S9(7) VALUE 0.
       01  WS-DATE-ED.
           05  WS-ED-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-YYYY              PIC 9(4) VALUE 0.
       REPORT SECTION.
       RD  R-REQGEN
           CONTROLS ARE FINAL D-SITE
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-SITE TYPE IS CONTROL HEADING D-SITE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(6) VALUE "Site: ".
               10  COLUMN 7    PIC X(10) SOURCE D-SITE.
               10  COLUMN 22   PIC X(52) VALUE
                   "A requisition is a need, not a purchase.  Nothing".
               10  COLUMN 75   PIC X(52) VALUE
                   "here commits the company to any supplier.".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(4) VALUE "Item".
               10  COLUMN 22   PIC X(11) VALUE "Description".
               10  COLUMN 45   PIC X(11) VALUE " Free stock".
               10  COLUMN 57   PIC X(11) VALUE "   On order".
               10  COLUMN 69   PIC X(11) VALUE " Reorder pt".
               10  COLUMN 81   PIC X(11) VALUE "  Suggested".
               10  COLUMN 93   PIC X(10) VALUE "Needed by ".
               10  COLUMN 104  PIC X(12) VALUE "   Est value".
               10  COLUMN 117  PIC X(8) VALUE "Supplier".
               10  COLUMN 126  PIC X(6) VALUE "Demand".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) SOURCE D-ITEM.
               10  COLUMN 22   PIC X(22) SOURCE D-DESC.
               10  COLUMN 45   PIC Z(6)9.999 SOURCE D-FREE.
               10  COLUMN 57   PIC Z(6)9.999 SOURCE D-ON-ORDER.
               10  COLUMN 69   PIC Z(6)9.999 SOURCE D-ROP.
               10  COLUMN 81   PIC Z(6)9.999 SOURCE D-SUGGEST.
               10  COLUMN 93   PIC X(10) SOURCE D-NEEDED-BY.
               10  COLUMN 104  PIC Z(8)9.99- SOURCE D-EST-VALUE.
               10  COLUMN 117  PIC X(8) SOURCE D-SUPPLIER.
               10  COLUMN 126  PIC X(6) SOURCE D-DEMAND-TX.
           05  LINE PLUS 1.
               10  COLUMN 22   PIC X(10) VALUE "Outcome:  ".
               10  COLUMN 32   PIC X(14) SOURCE D-OUTCOME.
               10  COLUMN 48   PIC X(30) SOURCE WS-CUR-REQ.
       01  CF-SITE TYPE IS CONTROL FOOTING D-SITE.
           05  LINE PLUS 1.
               10  COLUMN 22   PIC X(22) VALUE
                   "Site proposed lines : ".
               10  COLUMN 44   PIC ZZZZ9 SUM D-ONE.
               10  COLUMN 56   PIC X(22) VALUE
                   "Estimated value     : ".
               10  COLUMN 78   PIC Z(11)9.99- SUM D-EST-VALUE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(30) VALUE
                   "Lines proposed              : ".
               10  COLUMN 35   PIC ZZZZ9 SOURCE T-PROPOSED.
               10  COLUMN 48   PIC X(30) VALUE
                   "of which shortages          : ".
               10  COLUMN 78   PIC ZZZZ9 SOURCE T-SHORTAGE.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(30) VALUE
                   "Requisition lines raised    : ".
               10  COLUMN 35   PIC ZZZZ9 SOURCE T-RAISED.
               10  COLUMN 48   PIC X(30) VALUE
                   "Requisitions created        : ".
               10  COLUMN 78   PIC ZZZZ9 SOURCE T-REQS.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(30) VALUE
                   "Refused by POREQ            : ".
               10  COLUMN 35   PIC ZZZZ9 SOURCE T-REFUSED.
               10  COLUMN 48   PIC X(30) VALUE
                   "Estimated value             : ".
               10  COLUMN 78   PIC Z(11)9.99- SOURCE T-VALUE.
           05  LINE PLUS 2.
               10  COLUMN 5    PIC X(62) VALUE
                   "No purchase order, order line, schedule or".
               10  COLUMN 68   PIC X(60) VALUE
                   "commitment movement was written by this run.".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(62) VALUE
                   "Each proposal still needs sourcing (PO-002) and".
               10  COLUMN 68   PIC X(60) VALUE
                   "approval (PO-006) before a supplier is told.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START THRU 0200-EXIT
           PERFORM 0300-OPEN-SPOOL THRU 0300-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-PRODUCE THRU 1000-EXIT
           PERFORM 0400-CLOSE-SPOOL THRU 0400-EXIT.
       0000-FINISH.
           PERFORM 0500-JOB-END THRU 0500-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.
      *>----------------------------------------------------------------
       0100-READ-ARGS SECTION.
       0100-START.
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-SITE TO WS-SITE
           MOVE CM-USER TO WS-USER
           MOVE "ITEM" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-ITEM
           END-IF
           MOVE "MODE" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-MODE
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "BUYER1" TO WS-USER
           END-IF
      *>     CR1101: anything that is not plainly COMMIT is PREVIEW.
           IF NOT WS-COMMIT
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           DISPLAY "STERP-I-1750 REQGEN requisition generation "
                   FUNCTION TRIM(WS-CO) " mode "
                   FUNCTION TRIM(WS-MODE) UPON SYSOUT.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "REQGEN" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "REQGEN" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1751 cannot build the spool path"
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1752 cannot open " WS-SPOOL-NAME
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-REQGEN.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-001" TO RPT-REPORT-ID
           MOVE "Requisition generation - replenishment and shortage"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-COMMIT
               STRING "MODE=COMMIT - requisitions are created "
                      "through POREQ"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               STRING "MODE=PREVIEW - proposal only, nothing is "
                      "written"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-NOW(7:2) "/" WS-NOW(5:2) "/"
                  WS-NOW(1:4)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           PERFORM 0360-READ-COMPANY THRU 0360-EXIT.
       0350-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0360-READ-COMPANY SECTION.
       0360-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-OK
               MOVE PO-COL(1) TO RPT-COMPANY-NAME
               MOVE PO-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REQ: PO-001 - the demand scan.  Free stock counts only
      *> OWNED, ACCEPTED stock: quarantined goods and stock belonging
      *> to a customer are not available to cover a re-order point.
      *> Goods already on order count towards the position, so a
      *> second requisition is not raised for demand that has already
      *> been answered.  An item that still has an open requisition
      *> line is excluded, which is what makes the run repeatable.
      *>----------------------------------------------------------------
       1000-PRODUCE SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT isi.SiteCode, isi.ItemCode, "
                  "LEFT(it.Description,22), CAST(ISNULL((SELECT "
                  "SUM(sb.QtyOnHand - sb.QtyReserved) FROM "
                  "inv.StockBalance sb WHERE sb.CompanyCode = "
                  "isi.CompanyCode AND sb.SiteCode = isi.SiteCode "
                  "AND sb.ItemCode = isi.ItemCode AND "
                  "sb.OwnerType = 'OWNED' AND sb.QualityStatus = "
                  "'ACCEPTED'),0) AS varchar(30)), "
                  "CAST(ISNULL((SELECT SUM(pl.OrderedQty - "
                  "pl.CancelledQty - pl.ReceivedQty) FROM "
                  "po.PurchaseOrderLine pl JOIN po.PurchaseOrder "
                  "po2 ON po2.PurchaseOrderId = pl.PurchaseOrderId "
                  "WHERE pl.CompanyCode = isi.CompanyCode AND "
                  "pl.ItemCode = isi.ItemCode AND pl.Status = "
                  "'OPEN' AND po2.Status IN ('APPROVED','ISSUED',"
                  "'ACKNOWLEDGED','AMENDED')),0) AS varchar(30)), "
                  "CAST(ISNULL(isi.ReorderPointQty,0) AS "
                  "varchar(30)), CAST(ISNULL(isi.MaxStockQty,0) AS "
                  "varchar(30)), CAST(ISNULL(isi.MinOrderQty,0) AS "
                  "varchar(30)), CAST(ISNULL("
                  "isi.PurchaseMultipleQty,0) AS varchar(30)), "
                  "CAST(isi.LeadTimeDays AS varchar(10)), "
                  "ISNULL((SELECT TOP 1 ss.SupplierCode FROM "
                  "po.SupplierSource ss WHERE ss.CompanyCode = "
                  "isi.CompanyCode AND ss.ItemCode = isi.ItemCode "
                  "AND ss.IsActive = 1 AND ss.ApprovalStatus IN "
                  "('APPROVED','CONDITION') ORDER BY ss.Priority),"
                  "''), CAST(ISNULL((SELECT TOP 1 pp.UnitPrice / "
                  "NULLIF(pp.PricePerQty,0) FROM po.PurchasePrice "
                  "pp WHERE pp.CompanyCode = isi.CompanyCode AND "
                  "pp.ItemCode = isi.ItemCode AND pp.IsActive = 1 "
                  "ORDER BY pp.Priority),0) AS varchar(30)), "
                  "it.StockUomCode, ISNULL(it.PurchaseUomCode,"
                  "it.StockUomCode) FROM inv.ItemSite isi JOIN "
                  "inv.Item it ON it.CompanyCode = isi.CompanyCode "
                  "AND it.ItemCode = isi.ItemCode WHERE "
                  "isi.CompanyCode = ? AND isi.IsActive = 1 AND "
                  "it.ItemClass IN ('BOUGHT','PACKAGING') AND "
                  "it.MakeBuy IN ('BUY','DUAL') AND "
                  "ISNULL(isi.ReorderPointQty,0) > 0 AND "
                  "(? = '' OR isi.SiteCode = ?) AND (? = '' OR "
                  "isi.ItemCode = ?) AND NOT EXISTS (SELECT 1 FROM "
                  "po.RequisitionLine rl JOIN po.Requisition rq ON "
                  "rq.RequisitionId = rl.RequisitionId WHERE "
                  "rq.CompanyCode = isi.CompanyCode AND "
                  "rq.SiteCode = isi.SiteCode AND rl.ItemCode = "
                  "isi.ItemCode AND rl.Status = 'OPEN' AND "
                  "rq.Status IN ('DRAFT','SUBMITTED','APPROVED',"
                  "'PARTCONV')) ORDER BY isi.SiteCode, isi.ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 40
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE TO SQLIO-PARM-VAL(3)
           MOVE WS-ITEM TO SQLIO-PARM-VAL(4)
           MOVE WS-ITEM TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE 14 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1753 " FUNCTION TRIM(SQLIO-ERRMSG)
                   UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA PO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   ADD 1 TO WS-ROWS-READ
                   PERFORM 1100-ONE-ITEM THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           PERFORM 1500-PROCESS THRU 1500-EXIT.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-ONE-ITEM SECTION.
       1100-START.
           COMPUTE WS-FREE = FUNCTION NUMVAL(PO-COL(4))
           COMPUTE WS-ON-ORDER = FUNCTION NUMVAL(PO-COL(5))
           COMPUTE WS-ROP = FUNCTION NUMVAL(PO-COL(6))
           COMPUTE WS-MAXQ = FUNCTION NUMVAL(PO-COL(7))
           COMPUTE WS-MINQ = FUNCTION NUMVAL(PO-COL(8))
           COMPUTE WS-MULT = FUNCTION NUMVAL(PO-COL(9))
           COMPUTE WS-PROJECTED = WS-FREE + WS-ON-ORDER
           IF WS-PROJECTED > WS-ROP
               GO TO 1100-EXIT
           END-IF
      *>     Order up to the maximum when one is set, otherwise back
      *>     to the re-order point.  Buying only the shortfall would
      *>     put the item straight back on the next run.
           IF WS-MAXQ > WS-ROP
               MOVE WS-MAXQ TO WS-TARGET
           ELSE
               MOVE WS-ROP TO WS-TARGET
           END-IF
           COMPUTE D-SUGGEST = WS-TARGET - WS-PROJECTED
           IF D-SUGGEST < WS-MINQ
               MOVE WS-MINQ TO D-SUGGEST
           END-IF
           IF WS-MULT > 0
               COMPUTE WS-PACKS = D-SUGGEST / WS-MULT
               IF WS-PACKS * WS-MULT < D-SUGGEST
                   ADD 1 TO WS-PACKS
               END-IF
               COMPUTE D-SUGGEST = WS-PACKS * WS-MULT
           END-IF
           IF D-SUGGEST <= 0
               GO TO 1100-EXIT
           END-IF
           MOVE PO-COL(1) TO D-SITE
           MOVE PO-COL(2) TO D-ITEM
           MOVE PO-COL(3) TO D-DESC
           MOVE WS-FREE TO D-FREE
           MOVE WS-ON-ORDER TO D-ON-ORDER
           MOVE WS-ROP TO D-ROP
           MOVE PO-COL(11) TO D-SUPPLIER
           COMPUTE D-EST-VALUE ROUNDED =
               D-SUGGEST * FUNCTION NUMVAL(PO-COL(12))
      *>     Negative free stock means the reservations cannot all be
      *>     met from what is physically here.  That is a shortage,
      *>     not routine replenishment, and it is owned differently.
           IF WS-FREE < 0
               MOVE "SHORTAGE" TO D-DEMAND
               ADD 1 TO T-SHORTAGE
           ELSE
               MOVE "REPLEN" TO D-DEMAND
           END-IF
           COMPUTE WS-LEAD-DAYS = FUNCTION NUMVAL(PO-COL(10))
           PERFORM 1200-NEEDED-BY THRU 1200-EXIT
           ADD 1 TO T-PROPOSED
           ADD D-EST-VALUE TO T-VALUE
           IF WS-PCNT >= 2000
               DISPLAY "STERP-W-1760 proposal table full at 2000"
                   UPON SYSOUT
               GO TO 1100-EXIT
           END-IF
           ADD 1 TO WS-PCNT
           MOVE D-SITE TO P-SITE(WS-PCNT)
           MOVE D-ITEM TO P-ITEM(WS-PCNT)
           MOVE D-DESC TO P-DESC(WS-PCNT)
           MOVE D-FREE TO P-FREE(WS-PCNT)
           MOVE D-ON-ORDER TO P-ON-ORDER(WS-PCNT)
           MOVE D-ROP TO P-ROP(WS-PCNT)
           MOVE D-SUGGEST TO P-SUGGEST(WS-PCNT)
           MOVE WS-WORK-DATE TO P-NEEDED-N(WS-PCNT)
           MOVE D-EST-VALUE TO P-EST-VALUE(WS-PCNT)
           COMPUTE P-PRICE(WS-PCNT) = FUNCTION NUMVAL(PO-COL(12))
           MOVE D-SUPPLIER TO P-SUPPLIER(WS-PCNT)
           MOVE D-DEMAND TO P-DEMAND(WS-PCNT)
           MOVE PO-COL(14) TO P-UOM(WS-PCNT).
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The proposals are now raised, with no database cursor of our
      *> own left open.
      *>----------------------------------------------------------------
       1500-PROCESS SECTION.
       1500-START.
           PERFORM VARYING WS-PIX FROM 1 BY 1 UNTIL WS-PIX > WS-PCNT
               MOVE P-SITE(WS-PIX) TO D-SITE
               MOVE P-ITEM(WS-PIX) TO D-ITEM
               MOVE P-DESC(WS-PIX) TO D-DESC
               MOVE P-FREE(WS-PIX) TO D-FREE
               MOVE P-ON-ORDER(WS-PIX) TO D-ON-ORDER
               MOVE P-ROP(WS-PIX) TO D-ROP
               MOVE P-SUGGEST(WS-PIX) TO D-SUGGEST
               MOVE P-EST-VALUE(WS-PIX) TO D-EST-VALUE
               MOVE P-SUPPLIER(WS-PIX) TO D-SUPPLIER
               MOVE P-DEMAND(WS-PIX) TO D-DEMAND
               IF D-DEMAND = "SHORTAGE"
                   MOVE "SHORT" TO D-DEMAND-TX
               ELSE
                   MOVE "REPLEN" TO D-DEMAND-TX
               END-IF
               MOVE P-NEEDED-N(WS-PIX) TO WS-WORK-DATE
               PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
               MOVE WS-DATE-ED TO D-NEEDED-BY
               MOVE SPACES TO D-OUTCOME
               IF WS-COMMIT
                   PERFORM 1300-RAISE THRU 1300-EXIT
               ELSE
                   MOVE "PREVIEW ONLY" TO D-OUTCOME
                   MOVE SPACES TO WS-CUR-REQ
               END-IF
               GENERATE DE-LINE
           END-PERFORM.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1200-NEEDED-BY SECTION.
       1200-START.
           INITIALIZE WS-DATECALC
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATECALC
           MOVE WS-CO TO CA-COMPANY OF WS-DATECALC
           MOVE WS-TODAY-N TO DC-DATE-1
           MOVE WS-LEAD-DAYS TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC > 4
               MOVE WS-TODAY-N TO WS-WORK-DATE
           ELSE
               MOVE DC-RESULT-DATE TO WS-WORK-DATE
           END-IF
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO D-NEEDED-BY.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Every proposal goes through POREQ.  The generator never
      *> writes po.Requisition directly, so a line raised by the night
      *> run is validated, numbered and budget-aware in exactly the
      *> same way as one typed in by a storeman.
      *>----------------------------------------------------------------
       1300-RAISE SECTION.
       1300-START.
           MOVE SPACES TO WS-GROUP-KEY
           STRING D-SITE(1:10) D-DEMAND(1:8)
               DELIMITED BY SIZE INTO WS-GROUP-KEY
           IF WS-REQ-OPEN = "Y" AND WS-GROUP-KEY NOT = WS-PREV-GROUP
               MOVE "N" TO WS-REQ-OPEN
           END-IF
           IF WS-REQ-OPEN NOT = "Y"
               PERFORM 1310-CREATE-HEADER THRU 1310-EXIT
               IF WS-REQ-OPEN NOT = "Y"
                   GO TO 1300-EXIT
               END-IF
           END-IF
           PERFORM 1320-ADD-LINE THRU 1320-EXIT
           MOVE WS-GROUP-KEY TO WS-PREV-GROUP.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1310-CREATE-HEADER SECTION.
       1310-START.
           INITIALIZE WS-POREQ
           MOVE "POREQ" TO CA-PROGRAM OF WS-POREQ
           MOVE "CREATE" TO CA-FUNCTION OF WS-POREQ
           MOVE WS-CO TO CA-COMPANY OF WS-POREQ
           MOVE D-SITE TO CA-SITE OF WS-POREQ
           MOVE WS-USER TO CA-USER OF WS-POREQ
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-POREQ
           MOVE WS-USER TO RQ-REQUESTER
           MOVE WS-TODAY-N TO RQ-REQ-DATE
           MOVE P-NEEDED-N(WS-PIX) TO RQ-NEEDED-BY
           MOVE D-DEMAND TO RQ-DEMAND-TYPE
           IF D-DEMAND = "SHORTAGE"
               MOVE "Shortage cover - reservations exceed stock"
                   TO RQ-PURPOSE
               MOVE "REQGEN SHORTAGE SCAN" TO RQ-DEMAND-REF
           ELSE
               MOVE "Replenishment - below the site re-order point"
                   TO RQ-PURPOSE
               MOVE "REQGEN REORDER POINT" TO RQ-DEMAND-REF
           END-IF
           CALL "POREQ" USING WS-POREQ
           IF CA-FAILED OF WS-POREQ
               MOVE "REQ REFUSED" TO D-OUTCOME
               MOVE SPACES TO WS-CUR-REQ
               ADD 1 TO T-REFUSED
               DISPLAY "STERP-W-1754 POREQ CREATE "
                       CA-REASON-CODE OF WS-POREQ " "
                       FUNCTION TRIM(CA-MESSAGE OF WS-POREQ)
                   UPON SYSOUT
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               GO TO 1310-EXIT
           END-IF
           MOVE RQ-REQ-NUMBER TO WS-CUR-REQ
           MOVE 0 TO WS-CUR-LINE
           MOVE "Y" TO WS-REQ-OPEN
           ADD 1 TO T-REQS.
       1310-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1320-ADD-LINE SECTION.
       1320-START.
           ADD 1 TO WS-CUR-LINE
           INITIALIZE WS-POREQ
           MOVE "POREQ" TO CA-PROGRAM OF WS-POREQ
           MOVE "ADDLINE" TO CA-FUNCTION OF WS-POREQ
           MOVE WS-CO TO CA-COMPANY OF WS-POREQ
           MOVE D-SITE TO CA-SITE OF WS-POREQ
           MOVE WS-USER TO CA-USER OF WS-POREQ
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-POREQ
           MOVE WS-CUR-REQ TO RQ-REQ-NUMBER
           MOVE WS-CUR-LINE TO RQ-LINE-NUM
           MOVE D-ITEM TO RQ-ITEM-CODE
           MOVE D-DESC TO RQ-DESCRIPTION
           MOVE D-SUGGEST TO RQ-QUANTITY
           MOVE P-UOM(WS-PIX) TO RQ-UOM-CODE
           MOVE P-PRICE(WS-PIX) TO RQ-EST-UNIT-PRICE
           MOVE D-SUPPLIER TO RQ-SUGGEST-SUPP
           MOVE P-NEEDED-N(WS-PIX) TO RQ-LINE-NEEDED-BY
           CALL "POREQ" USING WS-POREQ
           IF CA-FAILED OF WS-POREQ
               MOVE "LINE REFUSED" TO D-OUTCOME
               ADD 1 TO T-REFUSED
               SUBTRACT 1 FROM WS-CUR-LINE
               DISPLAY "STERP-W-1755 POREQ ADDLINE "
                       CA-REASON-CODE OF WS-POREQ " "
                       FUNCTION TRIM(CA-MESSAGE OF WS-POREQ)
                   UPON SYSOUT
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           ELSE
               MOVE "RAISED" TO D-OUTCOME
               ADD 1 TO T-RAISED
           END-IF.
       1320-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1450-FORMAT-DATE SECTION.
       1450-START.
           IF WS-WORK-DATE = 0
               MOVE 0 TO WS-ED-DD WS-ED-MM WS-ED-YYYY
           ELSE
               MOVE WS-WORK-DATE TO STD-DATE-N
               MOVE STD-DD TO WS-ED-DD
               MOVE STD-MM TO WS-ED-MM
               MOVE STD-YYYY TO WS-ED-YYYY
           END-IF.
       1450-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-REQGEN
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1756 " T-PROPOSED
                   " proposal(s) from " WS-ROWS-READ
                   " planned item(s)" UPON SYSOUT
           IF WS-COMMIT
               DISPLAY "STERP-I-1757 " T-RAISED
                       " requisition line(s) in " T-REQS
                       " requisition(s)" UPON SYSOUT
           ELSE
               DISPLAY "STERP-I-1758 PREVIEW - no requisition was "
                       "created" UPON SYSOUT
           END-IF
           DISPLAY "STERP-I-1759 no purchase order or commitment "
                   "was written by this run" UPON SYSOUT
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME)
               UPON SYSOUT.
       0400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "REQGEN" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "OK" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE T-PROPOSED TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
