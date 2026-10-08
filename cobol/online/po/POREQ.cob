      *>================================================================
      *> PROGRAM-ID : POREQ
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : PURCHASE REQUISITION LIFECYCLE.  A REQUISITION
      *>              IS A JUSTIFIED NEED WITH AN OWNER, NOT A
      *>              SUPPLIER COMMITMENT: NOTHING HERE WRITES TO
      *>              po.Commitment OR po.PurchaseOrder (PO-001).
      *> CALLED BY  : REQGEN (batch), the .NET requisition form
      *> INTERFACE  : CALL "POREQ" USING DFHCOMMAREA (POREQCA)
      *> CALLS      : NUMGEN, BUDGCHK, SQLIO -> po.Requisition,
      *>              po.RequisitionLine
      *>----------------------------------------------------------------
      *> DEMAND OWNERSHIP.  EVERY REQUISITION CARRIES WHO ASKED, WHY,
      *> AND WHAT DEMAND IT ANSWERS.  A SHORTAGE OR REPLENISHMENT
      *> REQUISITION RAISED BY REQGEN THEREFORE STILL NAMES THE SALES
      *> ORDER, KIT OR MIN/MAX RULE BEHIND IT, SO THE CHAIN FROM THE
      *> CUSTOMER PROMISE TO THE SUPPLIER ORDER IS NEVER BROKEN.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-21 RJH  CR0791 REQUISITION CONTROL
      *> 2005-11-30 DJM  CR1288 DEMAND OWNERSHIP ON SHORTAGE REQS
      *> 2026-10-06 STERP BUDGET CALL, CONVERSION TRACKING
      *> 2026-10-07 STERP COM-003 SELF APPROVAL REFUSED (REQ0013)
      *>================================================================
      *> REQ: PO-001, PO-005, COM-003
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POREQ.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 21/04/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==
                      LEADING ==CA-== BY ==NM-==.
       COPY BUDGCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-BUD-CA==
                      LEADING ==CA-== BY ==BG-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-HDR-ROW.
           05  WS-H-ID                 PIC X(20).
           05  WS-H-STATUS             PIC X(12).
           05  WS-H-TOTAL              PIC X(24).
           05  WS-H-REQUESTER          PIC X(20).
           05  WS-H-COST-CENTRE        PIC X(20).
           05  WS-H-BUDGET             PIC X(20).
           05  WS-H-LINES              PIC X(12).
           05  WS-H-OPEN               PIC X(24).
       01  WS-ONE-COL.
           05  WS-V1                   PIC X(24).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-SOD-WHO              PIC X(20) VALUE SPACES.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-QTY-ED               PIC -(9)9.999999.
       LINKAGE SECTION.
       COPY POREQCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CREATE"
                   PERFORM 2000-CREATE THRU 2000-EXIT
               WHEN "ADDLINE"
                   PERFORM 3000-ADDLINE THRU 3000-EXIT
               WHEN "SUBMIT"
                   PERFORM 4000-SUBMIT THRU 4000-EXIT
               WHEN "APPROVE"
                   PERFORM 5000-APPROVE THRU 5000-EXIT
               WHEN "CANCEL"
                   PERFORM 6000-CANCEL THRU 6000-EXIT
               WHEN "CONVERT"
                   PERFORM 7000-CONVERT THRU 7000-EXIT
               WHEN "ENQUIRE"
                   PERFORM 8000-READ-HDR THRU 8000-EXIT
               WHEN OTHER
                   MOVE "REQ0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by POREQ"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "POREQ" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO RQ-REQ-ID RQ-LINE-ID RQ-LINE-VALUE-GBP
           MOVE 0 TO RQ-TOTAL-GBP RQ-OPEN-GBP RQ-LINE-CNT
           MOVE 0 TO RQ-BUDGET-USED-PCT RQ-BUDGET-AVAIL-GBP
           MOVE SPACES TO RQ-STATUS RQ-BUDGET-OUTCOME RQ-OUTCOME
           IF RQ-REQ-DATE = 0
               MOVE CA-BUSINESS-DATE TO RQ-REQ-DATE
           END-IF
           IF RQ-DEMAND-TYPE = SPACES
               MOVE "MANUAL" TO RQ-DEMAND-TYPE
           END-IF
           IF RQ-REQUESTER = SPACES
               MOVE CA-USER TO RQ-REQUESTER
           END-IF
           IF CA-COMPANY = SPACES OR CA-SITE = SPACES OR
              RQ-REQUESTER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A company, a site and a named requester are "
                      "needed - a requisition must have an owner"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 1000-EXIT
           END-IF
           IF NOT RQ-DEM-KNOWN
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Demand type " FUNCTION TRIM(RQ-DEMAND-TYPE)
                      " is not recognised"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2000-CREATE SECTION.
       2000-START.
           IF RQ-NEEDED-BY NOT = 0 AND RQ-NEEDED-BY < RQ-REQ-DATE
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The needed-by date is before the "
                      "requisition date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           IF RQ-REQ-NUMBER = SPACES
               PERFORM 9100-NUMBER THRU 9100-EXIT
               IF WS-FATAL
                   GO TO 2000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.Requisition (CompanyCode, SiteCode, "
                  "RequisitionNumber, RequesterUser, Purpose, "
                  "RequisitionDate, NeededByDate, DimCostCentre, "
                  "DimProject, JobRef, SourceDemandType, "
                  "SourceDemandRef, BudgetCode, Status, CreatedBy) "
                  "OUTPUT CAST(INSERTED.RequisitionId AS "
                  "varchar(20)) VALUES (?, ?, ?, ?, NULLIF(?, ''), "
                  "TRY_CONVERT(date, ?), TRY_CONVERT(date, "
                  "NULLIF(?, '00000000')), NULLIF(?, ''), "
                  "NULLIF(?, ''), NULLIF(?, ''), ?, NULLIF(?, ''), "
                  "NULLIF(?, ''), 'DRAFT', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CA-SITE TO SQLIO-PARM-VAL(2)
           MOVE RQ-REQ-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE RQ-REQUESTER TO SQLIO-PARM-VAL(4)
           MOVE RQ-PURPOSE TO SQLIO-PARM-VAL(5)
           MOVE RQ-REQ-DATE TO SQLIO-PARM-VAL(6)
           MOVE RQ-NEEDED-BY TO SQLIO-PARM-VAL(7)
           MOVE RQ-COST-CENTRE TO SQLIO-PARM-VAL(8)
           MOVE RQ-PROJECT TO SQLIO-PARM-VAL(9)
           MOVE RQ-JOB-REF TO SQLIO-PARM-VAL(10)
           MOVE RQ-DEMAND-TYPE TO SQLIO-PARM-VAL(11)
           MOVE RQ-DEMAND-REF TO SQLIO-PARM-VAL(12)
           MOVE RQ-BUDGET-CODE TO SQLIO-PARM-VAL(13)
           MOVE CA-USER TO SQLIO-PARM-VAL(14)
           MOVE 14 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           COMPUTE RQ-REQ-ID = FUNCTION NUMVAL(WS-V1)
           MOVE "DRAFT" TO RQ-STATUS
           MOVE "DRAFT" TO RQ-OUTCOME
           MOVE SPACES TO CA-MESSAGE
           STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                  " raised by " FUNCTION TRIM(RQ-REQUESTER)
                  " for " FUNCTION TRIM(RQ-DEMAND-TYPE)
                  " demand " FUNCTION TRIM(RQ-DEMAND-REF)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ADD A LINE.  A FREE TEXT LINE WITHOUT AN ITEM CODE MUST
      *> CARRY A NOMINAL ACCOUNT - OTHERWISE NOBODY KNOWS WHAT THE
      *> SPEND IS FOR AND IT CANNOT BE BUDGET CHECKED (PO-003).
      *>----------------------------------------------------------------
       3000-ADDLINE SECTION.
       3000-START.
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           IF RQ-STATUS NOT = "DRAFT" AND RQ-STATUS NOT = "REJECTED"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                      " is " FUNCTION TRIM(RQ-STATUS)
                      " and can no longer be changed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF RQ-DESCRIPTION = SPACES OR RQ-QUANTITY <= 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A requisition line needs a description and "
                      "a quantity greater than zero"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF RQ-ITEM-CODE = SPACES AND RQ-ACCOUNT-CODE = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A free text line needs a nominal account "
                      "code so that the spend can be coded and "
                      "budget checked"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           COMPUTE RQ-LINE-VALUE-GBP ROUNDED =
                   RQ-QUANTITY * RQ-EST-UNIT-PRICE
           IF RQ-LINE-NUM = 0
               PERFORM 3100-NEXT-LINE THRU 3100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE po.RequisitionLine AS t USING (SELECT ? AS "
                  "RequisitionId, ? AS LineNum) AS s ON "
                  "t.RequisitionId = s.RequisitionId AND t.LineNum ="
                  " s.LineNum WHEN MATCHED THEN UPDATE SET ItemCode "
                  "= NULLIF(?, ''), Description = ?, Quantity = ?, "
                  "UomCode = NULLIF(?, ''), EstimatedUnitPrice = ?, "
                  "EstimatedValueGBP = ?, SuggestedSupplierCode = "
                  "NULLIF(?, ''), NeededByDate = TRY_CONVERT(date, "
                  "NULLIF(?, '00000000')), AccountCode = NULLIF(?, "
                  "'') WHEN NOT MATCHED THEN INSERT (RequisitionId, "
                  "LineNum, ItemCode, Description, Quantity, "
                  "UomCode, EstimatedUnitPrice, EstimatedValueGBP, "
                  "SuggestedSupplierCode, NeededByDate, AccountCode,"
                  " Status) VALUES (s.RequisitionId, s.LineNum, "
                  "NULLIF(?, ''), ?, ?, NULLIF(?, ''), ?, ?, "
                  "NULLIF(?, ''), TRY_CONVERT(date, NULLIF(?, "
                  "'00000000')), NULLIF(?, ''), 'OPEN') OUTPUT "
                  "CAST(INSERTED.RequisitionLineId AS varchar(20));"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RQ-REQ-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE RQ-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           PERFORM 3200-LINE-PARMS THRU 3200-EXIT
           MOVE 20 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE RQ-LINE-ID = FUNCTION NUMVAL(WS-V1)
           END-IF
           PERFORM 3300-RETOTAL THRU 3300-EXIT
           MOVE RQ-LINE-VALUE-GBP TO WS-AMT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Line " FUNCTION TRIM(RQ-DESCRIPTION)
                  " valued at " FUNCTION TRIM(WS-AMT-ED)
                  " added to requisition "
                  FUNCTION TRIM(RQ-REQ-NUMBER)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3100-NEXT-LINE SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(MAX(LineNum), 0) + 1 AS "
                  "varchar(24)) FROM po.RequisitionLine WHERE "
                  "RequisitionId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RQ-REQ-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3100-EXIT
           END-IF
           COMPUTE RQ-LINE-NUM = FUNCTION NUMVAL(WS-V1).
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE SAME TEN VALUES ARE BOUND TWICE - ONCE FOR THE UPDATE
      *> BRANCH AND ONCE FOR THE INSERT BRANCH OF THE MERGE.
      *>----------------------------------------------------------------
       3200-LINE-PARMS SECTION.
       3200-START.
           PERFORM VARYING WS-I FROM 0 BY 9 UNTIL WS-I > 9
               MOVE RQ-ITEM-CODE TO SQLIO-PARM-VAL(WS-I + 3)
               MOVE RQ-DESCRIPTION TO SQLIO-PARM-VAL(WS-I + 4)
               MOVE RQ-QUANTITY TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                 TO SQLIO-PARM-VAL(WS-I + 5)
               MOVE RQ-UOM-CODE TO SQLIO-PARM-VAL(WS-I + 6)
               MOVE RQ-EST-UNIT-PRICE TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                 TO SQLIO-PARM-VAL(WS-I + 7)
               MOVE RQ-LINE-VALUE-GBP TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                 TO SQLIO-PARM-VAL(WS-I + 8)
               MOVE RQ-SUGGEST-SUPP TO SQLIO-PARM-VAL(WS-I + 9)
               IF RQ-LINE-NEEDED-BY = 0
                   MOVE RQ-NEEDED-BY TO SQLIO-PARM-VAL(WS-I + 10)
               ELSE
                   MOVE RQ-LINE-NEEDED-BY
                     TO SQLIO-PARM-VAL(WS-I + 10)
               END-IF
               MOVE RQ-ACCOUNT-CODE TO SQLIO-PARM-VAL(WS-I + 11)
           END-PERFORM.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3300-RETOTAL SECTION.
       3300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE r SET TotalValueGBP = (SELECT "
                  "ISNULL(SUM(l.EstimatedValueGBP), 0) FROM "
                  "po.RequisitionLine l WHERE l.RequisitionId = "
                  "r.RequisitionId AND l.Status <> 'CANCELLED'), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() FROM "
                  "po.Requisition r WHERE r.RequisitionId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE RQ-REQ-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3300-EXIT
           END-IF
           PERFORM 8000-READ-HDR THRU 8000-EXIT.
       3300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SUBMIT.  THE BUDGET IS CHECKED BEFORE ANYBODY IS ASKED TO
      *> APPROVE, SO AN UNFUNDED REQUEST NEVER REACHES AN APPROVER.
      *> NOTHING IS COMMITTED HERE: A REQUISITION IS NOT A SUPPLIER
      *> COMMITMENT.
      *>----------------------------------------------------------------
       4000-SUBMIT SECTION.
       4000-START.
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           IF RQ-STATUS NOT = "DRAFT" AND RQ-STATUS NOT = "REJECTED"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                      " is " FUNCTION TRIM(RQ-STATUS)
                      " and cannot be submitted again"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           IF RQ-LINE-CNT = 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                      " has no lines to submit"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-BUDGET THRU 4100-EXIT
           IF CA-RETURN-CODE >= 8
               GO TO 4000-EXIT
           END-IF
           IF RQ-BUDGET-OUTCOME = "BLOCK" AND
              RQ-OVERRIDE-BY = SPACES
               MOVE 6 TO CA-RETURN-CODE
               MOVE "REQ0007" TO CA-REASON-CODE
               MOVE "SUBMITTED" TO RQ-OUTCOME
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-SET-STATUS THRU 4200-EXIT.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4100-BUDGET SECTION.
       4100-START.
           IF RQ-BUDGET-CODE = SPACES AND RQ-COST-CENTRE = SPACES
               MOVE "WITHIN" TO RQ-BUDGET-OUTCOME
               GO TO 4100-EXIT
           END-IF
           INITIALIZE WS-BUD-CA
           MOVE "CHECK" TO BG-FUNCTION
           MOVE CA-COMPANY TO BG-COMPANY
           MOVE CA-SITE TO BG-SITE
           MOVE CA-USER TO BG-USER
           MOVE RQ-REQ-DATE TO BG-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO BG-CORRELATION-ID
           MOVE RQ-BUDGET-CODE TO BC-BUDGET-CODE
           MOVE RQ-ACCOUNT-CODE TO BC-ACCOUNT-CODE
           MOVE RQ-COST-CENTRE TO BC-COST-CENTRE
           MOVE RQ-PROJECT TO BC-PROJECT
           MOVE "REQUISITION" TO BC-DOC-TYPE
           MOVE RQ-REQ-NUMBER TO BC-DOC-REF
           MOVE RQ-REQUESTER TO BC-REQUESTER
           MOVE RQ-ITEM-CODE TO BC-ITEM-CODE
           MOVE RQ-REQ-DATE TO BC-MOVEMENT-DATE
           MOVE RQ-TOTAL-GBP TO BC-NET-GBP
           MOVE RQ-OVERRIDE-BY TO BC-OVERRIDE-BY
           MOVE RQ-OVERRIDE-REASON TO BC-OVERRIDE-REASON
           CALL "BUDGCHK" USING WS-BUD-CA
           MOVE BC-OUTCOME TO RQ-BUDGET-OUTCOME
           MOVE BC-USED-AFTER-PCT TO RQ-BUDGET-USED-PCT
           MOVE BC-AVAILABLE-GBP TO RQ-BUDGET-AVAIL-GBP
           IF BG-RETURN-CODE >= 4
               MOVE BG-RETURN-CODE TO CA-RETURN-CODE
               MOVE BG-REASON-CODE TO CA-REASON-CODE
               MOVE BG-MESSAGE TO CA-MESSAGE
           END-IF
           IF BG-RETURN-CODE = 16
      *>         A missing budget line is a warning on a requisition;
      *>         it only blocks when the order is raised.
               MOVE 4 TO CA-RETURN-CODE
           END-IF.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4200-SET-STATUS SECTION.
       4200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.Requisition SET Status = 'SUBMITTED', "
                  "BudgetUsedPct = ?, BudgetOverrideBy = NULLIF(?, "
                  "''), BudgetOverrideReason = NULLIF(?, ''), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "RequisitionId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RQ-BUDGET-USED-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE RQ-OVERRIDE-BY TO SQLIO-PARM-VAL(2)
           MOVE RQ-OVERRIDE-REASON TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE RQ-REQ-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4200-EXIT
           END-IF
           MOVE "SUBMITTED" TO RQ-STATUS
           MOVE "SUBMITTED" TO RQ-OUTCOME
           IF CA-RETURN-CODE = 0
               MOVE SPACES TO CA-MESSAGE
               MOVE RQ-TOTAL-GBP TO WS-AMT-ED
               STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                      " for " FUNCTION TRIM(WS-AMT-ED)
                      " submitted for approval"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       4200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5000-APPROVE SECTION.
       5000-START.
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           IF RQ-STATUS NOT = "SUBMITTED"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Only a submitted requisition can be decided;"
                      " this one is " FUNCTION TRIM(RQ-STATUS)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           IF RQ-DECISION = SPACES
               MOVE "APPROVE" TO RQ-DECISION
           END-IF
           IF RQ-APPROVER = SPACES
               MOVE CA-USER TO RQ-APPROVER
           END-IF
      *>     THE REQUISITION'S OWNER IS WHOEVER THE FILE SAYS RAISED
      *>     IT, NOT WHOEVER THE CALLER PUT IN THE COMMAREA.
           MOVE WS-H-REQUESTER TO RQ-REQUESTER
           PERFORM 5100-SOD THRU 5100-EXIT
           IF CA-RETURN-CODE NOT = 0
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.Requisition SET Status = ?, ApprovedBy "
                  "= ?, ApprovedAt = SYSDATETIME(), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE RequisitionId = "
                  "?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           IF RQ-DECISION = "REJECT"
               MOVE "REJECTED" TO SQLIO-PARM-VAL(1)
               MOVE "REJECTED" TO RQ-STATUS
           ELSE
               MOVE "APPROVED" TO SQLIO-PARM-VAL(1)
               MOVE "APPROVED" TO RQ-STATUS
           END-IF
           MOVE RQ-APPROVER TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE RQ-REQ-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE RQ-STATUS TO RQ-OUTCOME
           IF RQ-STATUS = "REJECTED"
               MOVE 4 TO CA-RETURN-CODE
           END-IF
           MOVE SPACES TO CA-MESSAGE
           STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER) " "
                  FUNCTION TRIM(RQ-STATUS) " by "
                  FUNCTION TRIM(RQ-APPROVER)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> COM-003 SEPARATION OF DUTIES.  THE PERSON WHO ASKED FOR THE
      *> GOODS MAY NEVER BE THE PERSON WHO AUTHORISES THEM, WHICHEVER
      *> WAY ROUND THE CALLER FILLS THE COMMAREA IN: THE SIGNED ON
      *> USER AND THE NAMED APPROVER ARE BOTH MEASURED AGAINST THE
      *> REQUESTER ON FILE.  THE SAME INVARIANT IS HELD CENTRALLY BY
      *> core.usp_DecideApproval FOR ORDERS AND PAYMENTS; A
      *> REQUISITION IS DECIDED IN po.Requisition ITSELF, SO IT IS
      *> HELD HERE.  A REJECTION IS NOT CAUGHT - WITHDRAWING YOUR OWN
      *> REQUEST IS NOT A CONTROL FAILURE.
      *>----------------------------------------------------------------
       5100-SOD SECTION.
       5100-START.
           IF RQ-DECISION = "REJECT" OR RQ-DECISION = "REJECTED"
               GO TO 5100-EXIT
           END-IF
           IF WS-H-REQUESTER = SPACES
               GO TO 5100-EXIT
           END-IF
           MOVE SPACES TO WS-SOD-WHO
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(RQ-APPROVER)) =
              FUNCTION UPPER-CASE(FUNCTION TRIM(WS-H-REQUESTER))
               MOVE RQ-APPROVER TO WS-SOD-WHO
           END-IF
           IF WS-SOD-WHO = SPACES AND
              FUNCTION UPPER-CASE(FUNCTION TRIM(CA-USER)) =
              FUNCTION UPPER-CASE(FUNCTION TRIM(WS-H-REQUESTER))
               MOVE CA-USER TO WS-SOD-WHO
           END-IF
           IF WS-SOD-WHO = SPACES
               GO TO 5100-EXIT
           END-IF
           MOVE 8 TO CA-RETURN-CODE
           MOVE "REQ0013" TO CA-REASON-CODE
           MOVE "REFUSED" TO RQ-OUTCOME
           MOVE SPACES TO CA-MESSAGE
           STRING "Separation of duties: "
                  FUNCTION TRIM(WS-SOD-WHO)
                  " raised requisition "
                  FUNCTION TRIM(RQ-REQ-NUMBER)
                  " and may not approve it; an independent approver"
                  " is required"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-CANCEL SECTION.
       6000-START.
           IF RQ-REASON = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A requisition cancellation needs a reason"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 6000-EXIT
           END-IF
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           IF RQ-STATUS = "CONVERTED" OR RQ-STATUS = "CANCELLED"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                      " is " FUNCTION TRIM(RQ-STATUS)
                      " and cannot be cancelled"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.Requisition SET Status = 'CANCELLED', "
                  "Purpose = LEFT(ISNULL(Purpose, '') + ' "
                  "[cancelled: ' + ? + ']', 200), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE RequisitionId = "
                  "? AND Status NOT IN ('CONVERTED', 'CANCELLED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RQ-REASON TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE RQ-REQ-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           MOVE "CANCELLED" TO RQ-STATUS
           MOVE "CANCELLED" TO RQ-OUTCOME
           MOVE SPACES TO CA-MESSAGE
           STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                  " cancelled: " FUNCTION TRIM(RQ-REASON)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CONVERT.  THE LINE REMEMBERS WHICH ORDER ANSWERED IT AND
      *> HOW MUCH OF IT WAS ANSWERED, SO A PART CONVERSION LEAVES A
      *> VISIBLE REMAINDER RATHER THAN DISAPPEARING.
      *>----------------------------------------------------------------
       7000-CONVERT SECTION.
       7000-START.
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           IF WS-FATAL
               GO TO 7000-EXIT
           END-IF
           IF RQ-STATUS NOT = "APPROVED" AND RQ-STATUS NOT = "PARTCONV"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Only an approved requisition can be "
                      "converted to an order; this one is "
                      FUNCTION TRIM(RQ-STATUS)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 7000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.RequisitionLine SET ConvertedQty = "
                  "ConvertedQty + ?, PurchaseOrderNumber = ?, "
                  "PurchaseOrderLine = ?, Status = CASE WHEN "
                  "ConvertedQty + ? >= Quantity THEN 'CONVERTED' "
                  "ELSE 'PARTCONV' END OUTPUT "
                  "CAST(INSERTED.ConvertedQty AS varchar(24)) WHERE "
                  "RequisitionId = ? AND LineNum = ? AND "
                  "ConvertedQty + ? <= Quantity"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RQ-CONVERT-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE RQ-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE RQ-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(4)
           MOVE RQ-REQ-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE RQ-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(7)
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 7000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO CA-RETURN-CODE
               MOVE "REQ0011" TO CA-REASON-CODE
               MOVE RQ-CONVERT-QTY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Converting " FUNCTION TRIM(WS-QTY-ED)
                      " would take line "
                      " above the requisitioned quantity"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 7000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE r SET Status = CASE WHEN NOT EXISTS "
                  "(SELECT 1 FROM po.RequisitionLine l WHERE "
                  "l.RequisitionId = r.RequisitionId AND l.Status IN"
                  " ('OPEN', 'PARTCONV')) THEN 'CONVERTED' ELSE "
                  "'PARTCONV' END, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() FROM po.Requisition r WHERE "
                  "r.RequisitionId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE RQ-REQ-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 7000-EXIT
           END-IF
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           MOVE "CONVERTED" TO RQ-OUTCOME
           MOVE SPACES TO CA-MESSAGE
           STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                  " line answered by order "
                  FUNCTION TRIM(RQ-ORDER-NUMBER)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-READ-HDR SECTION.
       8000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(r.RequisitionId AS varchar(20)), "
                  "r.Status, CAST(r.TotalValueGBP AS varchar(24)), "
                  "r.RequesterUser, ISNULL(r.DimCostCentre, ''), "
                  "ISNULL(r.BudgetCode, ''), CAST((SELECT COUNT(*) "
                  "FROM po.RequisitionLine l WHERE l.RequisitionId ="
                  " r.RequisitionId AND l.Status <> 'CANCELLED') AS "
                  "varchar(12)), CAST((SELECT "
                  "ISNULL(SUM(l2.EstimatedValueGBP * CASE WHEN "
                  "l2.Quantity > 0 THEN (l2.Quantity - "
                  "l2.ConvertedQty) / l2.Quantity ELSE 0 END), 0) "
                  "FROM po.RequisitionLine l2 WHERE l2.RequisitionId"
                  " = r.RequisitionId AND l2.Status IN ('OPEN', "
                  "'PARTCONV')) AS varchar(24)) FROM po.Requisition "
                  "r WHERE r.CompanyCode = ? AND "
                  "r.RequisitionNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE RQ-REQ-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "REQ0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Requisition " FUNCTION TRIM(RQ-REQ-NUMBER)
                      " is not on file for company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 8000-EXIT
           END-IF
           COMPUTE RQ-REQ-ID = FUNCTION NUMVAL(WS-H-ID)
           MOVE WS-H-STATUS TO RQ-STATUS
           COMPUTE RQ-TOTAL-GBP = FUNCTION NUMVAL(WS-H-TOTAL)
           IF RQ-REQUESTER = SPACES OR CA-FUNCTION = "ENQUIRE"
               MOVE WS-H-REQUESTER TO RQ-REQUESTER
           END-IF
           IF RQ-COST-CENTRE = SPACES
               MOVE WS-H-COST-CENTRE TO RQ-COST-CENTRE
           END-IF
           IF RQ-BUDGET-CODE = SPACES
               MOVE WS-H-BUDGET TO RQ-BUDGET-CODE
           END-IF
           COMPUTE RQ-LINE-CNT = FUNCTION NUMVAL(WS-H-LINES)
           COMPUTE RQ-OPEN-GBP = FUNCTION NUMVAL(WS-H-OPEN).
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-SQL-FAIL SECTION.
       9000-START.
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           MOVE "Y" TO WS-FATAL-SW.
       9000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9100-NUMBER SECTION.
       9100-START.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO NM-FUNCTION
           MOVE CA-COMPANY TO NM-COMPANY
           MOVE CA-SITE TO NM-SITE
           MOVE CA-USER TO NM-USER
           MOVE RQ-REQ-DATE TO NM-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO NM-CORRELATION-ID
           MOVE "POREQ" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF NM-RETURN-CODE > 4
               MOVE 12 TO CA-RETURN-CODE
               MOVE "REQ0010" TO CA-REASON-CODE
               MOVE NM-MESSAGE TO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 9100-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO RQ-REQ-NUMBER.
       9100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9300-NO-NULLS SECTION.
       9300-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM.
       9300-EXIT.
           EXIT.
       END PROGRAM POREQ.
