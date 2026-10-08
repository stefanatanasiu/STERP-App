      *>================================================================
      *> PROGRAM-ID : VATERRC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (VALUE ADDED TAX)
      *> PURPOSE    : VAT ERROR CORRECTION.  A NET ERROR WITHIN THE
      *>              APPROVED THRESHOLD MAY BE CORRECTED ON THE NEXT
      *>              RETURN; ANYTHING LARGER, AND ANY DELIBERATE
      *>              ERROR, IS ROUTED FOR SEPARATE DISCLOSURE TO HMRC.
      *>              THE THRESHOLD IS THE GREATER OF THE FLAT LIMIT
      *>              (BASELINE GBP 10,000) AND ONE PER CENT OF BOX 6
      *>              TURNOVER, CAPPED AT GBP 50,000.
      *>              AC-116: 8,000 ON BOX 6 400,000 MAY BE CORRECTED;
      *>              30,000 ON BOX 6 2,000,000 MUST BE DISCLOSED;
      *>              30,000 ON BOX 6 4,000,000 MAY BE CORRECTED.
      *>              THE CORRECTION STAYS LINKED TO THE ORIGINAL
      *>              TRANSACTIONS AND TO ITS DISCLOSURE REFERENCE.
      *> CALLED BY  : frmVatReturn, .NET CLIENT
      *> INTERFACE  : CALL "VATERRC" USING WS-VATERRC-CA
      *>              COPYBOOK VATERRCCA
      *> CALLS      : PARMGET, PERMCHK, AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2009-07-01 RJH CR2301 ORIGINAL - VAT 652 THRESHOLD RULES
      *> 2026-10-05 STERP ONE PER CENT TEST AND 50,000 CAP
      *>================================================================
      *> REQ: FIN-027
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VATERRC.
       AUTHOR. R J HOLBROOK.
       DATE-WRITTEN. 01/07/2009.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET-CA==
                      LEADING ==CA-== BY ==MH-==.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERMCHK-CA==
                      LEADING ==CA-== BY ==KH-==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDITLOG-CA==
                      LEADING ==CA-== BY ==AH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-ABS-ERROR            PIC S9(13)V99 VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-PARM-CODE            PIC X(40).
       01  WS-ROW.
           05  WS-E-ID                 PIC X(18).
           05  WS-E-STATUS             PIC X(12).
           05  WS-E-METHOD             PIC X(14).
       LINKAGE SECTION.
       COPY VATERRCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           EVALUATE CA-FUNCTION
               WHEN "ASSESS"   PERFORM 1000-ASSESS THRU 1000-EXIT
               WHEN "RECORD"   PERFORM 2000-RECORD THRU 2000-EXIT
               WHEN "APPROVE"  PERFORM 3000-APPROVE THRU 3000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "VATERRC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-027" TO CA-REQ-REF
           MOVE SPACES TO VE-METHOD
           MOVE SPACES TO VE-EXPLANATION
           MOVE "N" TO VE-WITHIN-THRESHOLD
           MOVE 0 TO VE-THRESHOLD-USED
           MOVE 0 TO VE-ONE-PCT-LIMIT
           MOVE 0 TO VE-FLAT-LIMIT
           MOVE 0 TO VE-CAP-LIMIT
           IF VE-DISCOVERED-DATE = 0
               MOVE CA-BUSINESS-DATE TO VE-DISCOVERED-DATE
           END-IF
           IF VE-IS-DELIBERATE NOT = "Y"
               MOVE "N" TO VE-IS-DELIBERATE
           END-IF.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - DECIDE THE METHOD.  NOTHING IS WRITTEN.
      *>----------------------------------------------------------------
       1000-ASSESS.
           PERFORM 1100-READ-LIMITS THRU 1100-EXIT
           COMPUTE WS-ABS-ERROR = FUNCTION ABS(VE-NET-ERROR)
           COMPUTE VE-ONE-PCT-LIMIT ROUNDED =
               VE-BOX6-TURNOVER / 100
           IF VE-ONE-PCT-LIMIT > VE-CAP-LIMIT
               MOVE VE-CAP-LIMIT TO VE-ONE-PCT-LIMIT
           END-IF
           IF VE-ONE-PCT-LIMIT > VE-FLAT-LIMIT
               MOVE VE-ONE-PCT-LIMIT TO VE-THRESHOLD-USED
           ELSE
               MOVE VE-FLAT-LIMIT TO VE-THRESHOLD-USED
           END-IF
           IF VE-IS-DELIBERATE = "Y"
               MOVE "DISCLOSURE" TO VE-METHOD
               MOVE "N" TO VE-WITHIN-THRESHOLD
               MOVE "A deliberate error is always disclosed separately"
                   TO VE-EXPLANATION
               MOVE 6 TO CA-RETURN-CODE
               MOVE "VEC0010" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 1050-MESSAGE
           END-IF
           IF WS-ABS-ERROR NOT > VE-THRESHOLD-USED
               MOVE "NEXTRETURN" TO VE-METHOD
               MOVE "Y" TO VE-WITHIN-THRESHOLD
               MOVE "Within the approved threshold, so it may be corre"
                   TO VE-EXPLANATION
           ELSE
               MOVE "DISCLOSURE" TO VE-METHOD
               MOVE "N" TO VE-WITHIN-THRESHOLD
               MOVE "Above the approved threshold, so it is routed for"
                   TO VE-EXPLANATION
               MOVE 6 TO CA-RETURN-CODE
               MOVE "VEC0011" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1050-MESSAGE.
           MOVE SPACES TO CA-MESSAGE
           MOVE VE-THRESHOLD-USED TO WS-MONEY-ED
           IF VE-WITHIN-THRESHOLD = "Y"
               STRING "The net error may be corrected on the next "
                      "return. The threshold applied is "
                      FUNCTION TRIM(WS-MONEY-ED) " GBP"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               STRING "The net error exceeds the threshold of "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP and must be disclosed separately to HMRC"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.

      *> The limits are tuning parameters so that a change in HMRC
      *> policy does not need a new release (COM-025).
       1100-READ-LIMITS.
           MOVE "VAT.ERROR.THRESHOLD" TO WS-PARM-CODE
           MOVE 10000 TO VE-FLAT-LIMIT
           PERFORM 1150-ONE-PARM THRU 1150-EXIT
           IF PM-FOUND = "Y"
               MOVE PM-VALUE-NUM TO VE-FLAT-LIMIT
           END-IF
           MOVE "VAT.ERROR.CAP" TO WS-PARM-CODE
           MOVE 50000 TO VE-CAP-LIMIT
           PERFORM 1150-ONE-PARM THRU 1150-EXIT
           IF PM-FOUND = "Y"
               MOVE PM-VALUE-NUM TO VE-CAP-LIMIT
           END-IF.
       1100-EXIT.
           EXIT.

       1150-ONE-PARM.
           INITIALIZE WS-PARMGET-CA
           MOVE "GETNUM" TO MH-FUNCTION
           MOVE CA-COMPANY TO MH-COMPANY
           MOVE CA-USER TO MH-USER
           MOVE CA-BUSINESS-DATE TO MH-BUSINESS-DATE
           MOVE WS-PARM-CODE TO PM-PARAM-CODE
           MOVE VE-DISCOVERED-DATE TO PM-AS-OF
           MOVE "N" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARMGET-CA.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - RECORD THE CORRECTION WITH ITS METHOD AND SOURCES
      *>----------------------------------------------------------------
       2000-RECORD.
           PERFORM 1000-ASSESS THRU 1000-EXIT
           IF CA-RETURN-CODE = 12
               GO TO 2000-EXIT
           END-IF
           IF VE-REASON = SPACES OR VE-REASON = LOW-VALUES
               MOVE "VEC0001" TO CA-REASON-CODE
               MOVE "A VAT error correction must record how the error "
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF VE-ORIG-PERIOD-KEY = SPACES
               MOVE "VEC0002" TO CA-REASON-CODE
               MOVE "The VAT period the error arose in is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF VE-METHOD = "DISCLOSURE"
               MOVE SPACES TO VE-CORR-PERIOD-KEY
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatErrorCorrection (CompanyCode, "
                  "OriginalVatPeriodKey, CorrectionVatPeriodKey, "
                  "DiscoveredDate, NetErrorAmount, Box6Turnover, "
                  "IsDeliberate, Method, Reason, SourceDocumentRefs, "
                  "DisclosureRef, PreparedBy, Status, "
                  "ThresholdUsedGBP) OUTPUT inserted.ErrorId, "
                  "inserted.Status, inserted.Method VALUES (?, ?, "
                  "NULLIF(?,''), ?, ?, ?, ?, ?, ?, NULLIF(?,''), "
                  "NULLIF(?,''), ?, 'OPEN', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VE-ORIG-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE VE-CORR-PERIOD-KEY TO SQLIO-PARM-VAL(3)
           MOVE VE-DISCOVERED-DATE TO SQLIO-PARM-VAL(4)
           MOVE VE-NET-ERROR TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE VE-BOX6-TURNOVER TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           IF VE-IS-DELIBERATE = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(7)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(7)
           END-IF
           MOVE VE-METHOD TO SQLIO-PARM-VAL(8)
           MOVE VE-REASON TO SQLIO-PARM-VAL(9)
           MOVE VE-SOURCE-REFS TO SQLIO-PARM-VAL(10)
           MOVE VE-DISCLOSURE-REF TO SQLIO-PARM-VAL(11)
           MOVE CA-USER TO SQLIO-PARM-VAL(12)
           MOVE VE-THRESHOLD-USED TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 14 TO SQLIO-COL-WIDTH(3)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2000-EXIT
           END-IF
           COMPUTE VE-ERROR-ID = FUNCTION NUMVAL(WS-E-ID)
           MOVE WS-E-STATUS TO VE-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - APPROVE.  ONLY THEN DOES A NEXT RETURN CORRECTION REACH
      *>        BOX 1 OR BOX 4 (FIN-027).
      *>----------------------------------------------------------------
       3000-APPROVE.
           INITIALIZE WS-PERMCHK-CA
           MOVE "CHECK" TO KH-FUNCTION
           MOVE CA-COMPANY TO KH-COMPANY
           MOVE CA-USER TO KH-USER
           MOVE CA-BUSINESS-DATE TO KH-BUSINESS-DATE
           MOVE "FIN.VAT.CORRECT" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERMCHK-CA
           IF KH-RETURN-CODE > 4
               MOVE KH-REASON-CODE TO CA-REASON-CODE
               MOVE KH-MESSAGE TO CA-MESSAGE
               MOVE KH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           IF VE-ERROR-ID = 0
               MOVE "VEC0003" TO CA-REASON-CODE
               MOVE "The error correction to approve must be identifie"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.VatErrorCorrection SET Status = "
                  "'APPROVED', ApprovedBy = ?, ApprovedAt = "
                  "SYSDATETIME(), CorrectionVatPeriodKey = "
                  "CASE WHEN Method = 'NEXTRETURN' THEN NULLIF(?,'') "
                  "ELSE NULL END, DisclosureRef = CASE WHEN Method = "
                  "'DISCLOSURE' THEN NULLIF(?,'') ELSE DisclosureRef "
                  "END WHERE ErrorId = ? AND CompanyCode = ? "
                  "AND Status = 'OPEN'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE VE-CORR-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE VE-DISCLOSURE-REF TO SQLIO-PARM-VAL(3)
           MOVE VE-ERROR-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(5)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-ROWCOUNT = 0
               MOVE "VEC0004" TO CA-REASON-CODE
               MOVE "No open error correction was found to approve"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE "APPROVED" TO VE-STATUS
           MOVE "The error correction is approved and linked to its or"
               TO CA-MESSAGE
           PERFORM 8200-AUDIT THRU 8200-EXIT.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-IDX)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       8200-AUDIT.
           INITIALIZE WS-AUDITLOG-CA
           MOVE "WRITE" TO AH-FUNCTION
           MOVE CA-COMPANY TO AH-COMPANY
           MOVE CA-USER TO AH-USER
           MOVE CA-BUSINESS-DATE TO AH-BUSINESS-DATE
           MOVE "fin.VatErrorCorrection" TO AU-ENTITY-TYPE
           MOVE VE-ORIG-PERIOD-KEY TO AU-ENTITY-KEY
           MOVE CA-FUNCTION TO AU-ACTION
           MOVE VE-METHOD TO AU-NEW-VALUE
           MOVE VE-REASON TO AU-REASON
           MOVE VE-SOURCE-REFS(1:60) TO AU-AUTHORITY-REF
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDITLOG-CA.
       8200-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "VEC0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "VATERRC does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in VATERRC" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
