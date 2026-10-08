      *>================================================================
      *> PROGRAM-ID : POSUPCHK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : SUPPLIER DUE DILIGENCE FOR ORDER ISSUE.  READS
      *>              THE MOST RECENT PIECE OF EVIDENCE OF EACH TYPE
      *>              FROM po.SupplierDiligence, DECIDES WHETHER THE
      *>              SUPPLIER MAY BE ORDERED FROM, AND RECORDS NEW
      *>              EVIDENCE AND AUTHORISED EXCEPTIONS (PO-026, D-29).
      *> CALLED BY  : POORDER (ISSUE), POAPPRV, supplier maintenance
      *> INTERFACE  : CALL "POSUPCHK" USING DFHCOMMAREA (POSUPCHKCA)
      *> CALLS      : SQLIO -> po.SupplierDiligence
      *>----------------------------------------------------------------
      *> EVIDENCE IS NEVER OVERWRITTEN.  EACH CHECK IS A NEW ROW WITH
      *> ITS OWN CheckedAt, SO WHAT WE KNEW AND WHEN WE KNEW IT CAN
      *> ALWAYS BE RECONSTRUCTED.  A BLOCKING SCREENING RESULT - A
      *> SANCTIONS HIT - CANNOT BE EXCEPTED BY ANYONE IN THIS SYSTEM.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2004-02-09 DJM  CR1152 SUPPLIER APPROVAL EVIDENCE
      *> 2016-04-18 KMP  CR2290 MODERN SLAVERY STATEMENT
      *> 2026-10-06 STERP SANCTIONS BLOCK, D-29 MANDATORY SET
      *> 2026-10-07 STERP CR9016 EXCEPTION FUNCTION WAS UNREACHABLE
      *>                 - CA-FUNCTION ONLY HOLDS EIGHT BYTES
      *>================================================================
      *> REQ: PO-026, D-29
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSUPCHK.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 09/02/2004.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
           05  WS-FOUND-SW             PIC X VALUE "N".
               88  WS-FOUND                      VALUE "Y".
      *>  THE BASELINE MANDATORY SET OF D-29.  BUSINESSSIZE IS IN THE
      *>  SET BECAUSE THE STATUTORY PAYMENT TERMS MAXIMUM OF PO-027
      *>  DEPENDS ON IT - WE CANNOT CLAIM TO HAVE CHECKED THE TERMS
      *>  IF WE DO NOT KNOW HOW BIG THE SUPPLIER IS.
       01  WS-MANDATORY-TABLE.
           05  FILLER                  PIC X(20) VALUE "SANCTIONS".
           05  FILLER                  PIC X(20) VALUE "COMPANIESHSE".
           05  FILLER                  PIC X(20) VALUE "VATNUMBER".
           05  FILLER                  PIC X(20) VALUE "BUSINESSSIZE".
       01  WS-MANDATORY-LIST REDEFINES WS-MANDATORY-TABLE.
           05  WS-MAND-TYPE            PIC X(20) OCCURS 4 TIMES.
       01  WS-MAND-MAX                 PIC S9(4) COMP-5 VALUE 4.
       01  WS-ROW.
           05  WS-E-TYPE               PIC X(20).
           05  WS-E-OUTCOME            PIC X(12).
           05  WS-E-VALID-TO           PIC X(8).
           05  WS-E-BLOCKING           PIC X(1).
           05  WS-E-MANDATORY          PIC X(1).
           05  WS-E-EXCEPTED           PIC X(1).
           05  WS-E-NEXT-REVIEW        PIC X(8).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(20).
       01  WS-MISS-WORK                PIC X(200) VALUE SPACES.
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-K                    PIC S9(4) VALUE 0.
           05  WS-VALID-TO             PIC 9(8) VALUE 0.
           05  WS-REVIEW-DATE          PIC 9(8) VALUE 0.
           05  WS-TYPE-OK              PIC X VALUE "N".
           05  WS-HIT                  PIC X VALUE "N".
           05  WS-CNT-ED               PIC Z(3)9.
       LINKAGE SECTION.
       COPY POSUPCHKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CHECK"
                   PERFORM 2000-CHECK THRU 2000-EXIT
               WHEN "ENQUIRE"
                   PERFORM 2000-CHECK THRU 2000-EXIT
               WHEN "RECORD"
                   PERFORM 3000-RECORD THRU 3000-EXIT
      *>         CA-FUNCTION IS EIGHT BYTES, SO THE DOCUMENTED NAME
      *>         "EXCEPTION" ARRIVED TRUNCATED AND NEVER MATCHED -
      *>         THE AUTHORISED EXCEPTION OF PO-026 COULD NOT BE
      *>         RECORDED AT ALL.  BOTH SPELLINGS ARE TAKEN NOW.
      *>         2026-10-07 STERP CR9016
               WHEN "EXCEPT"
               WHEN "EXCEPTIO"
                   PERFORM 4000-EXCEPTION THRU 4000-EXIT
               WHEN OTHER
                   MOVE "DIL0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by POSUPCHK"
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
           MOVE "POSUPCHK" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO DD-FAIL-CNT DD-REVIEW-CNT DD-EXPIRED-CNT
           MOVE 0 TO DD-MISSING-CNT DD-EXCEPTED-CNT DD-HELD-CNT
           MOVE 0 TO DD-DETAIL-CNT DD-DILIGENCE-ID DD-APPROVAL-ID
           MOVE 0 TO DD-NEXT-REVIEW-DUE
           MOVE SPACES TO DD-MISSING-LIST DD-WORST-TYPE
           MOVE "PASS" TO DD-OVERALL
           MOVE "Y" TO DD-CAN-ORDER
           IF DD-CHECK-DATE = 0
               MOVE CA-BUSINESS-DATE TO DD-CHECK-DATE
           END-IF
           IF CA-COMPANY = SPACES OR DD-SUPPLIER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "DIL0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A company code and a supplier code are "
                      "needed for a due diligence check"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "N" TO DD-CAN-ORDER
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CHECK.  READ THE LATEST EVIDENCE OF EACH TYPE, THEN JUDGE.
      *>----------------------------------------------------------------
       2000-CHECK SECTION.
       2000-START.
           PERFORM 2100-READ-EVIDENCE THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF DD-HELD-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "DIL0002" TO CA-REASON-CODE
               MOVE "EXCEPTION" TO DD-OVERALL
               MOVE "N" TO DD-CAN-ORDER
               MOVE WS-MAND-TYPE(1) TO DD-WORST-TYPE
               MOVE SPACES TO CA-MESSAGE
               STRING "No due diligence evidence is held for "
                      "supplier " FUNCTION TRIM(DD-SUPPLIER)
                      " - onboarding is not complete"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 2400-MISSING-LIST THRU 2400-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-JUDGE-HELD THRU 2200-EXIT
           PERFORM 2300-JUDGE-MANDATORY THRU 2300-EXIT
           PERFORM 2500-VERDICT THRU 2500-EXIT.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE INNER QUERY PICKS THE LATEST CheckedAt PER EVIDENCE TYPE,
      *> SO SUPERSEDED EVIDENCE NEITHER BLOCKS NOR EXCUSES.
      *>----------------------------------------------------------------
       2100-READ-EVIDENCE SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT d.EvidenceType, d.Outcome, "
                  "ISNULL(CONVERT(varchar(8), d.ValidTo, 112), "
                  "'00000000'), CASE WHEN d.IsBlocking = 1 THEN 'Y' "
                  "ELSE 'N' END, CASE WHEN d.IsMandatory = 1 THEN "
                  "'Y' ELSE 'N' END, CASE WHEN d.ExceptionBy IS NULL "
                  "THEN 'N' ELSE 'Y' END, "
                  "ISNULL(CONVERT(varchar(8), d.NextReviewDate, 112)"
                  ", '00000000') FROM po.SupplierDiligence d JOIN "
                  "(SELECT EvidenceType, MAX(CheckedAt) AS MaxAt "
                  "FROM po.SupplierDiligence WHERE CompanyCode = ? "
                  "AND SupplierCode = ? GROUP BY EvidenceType) x ON "
                  "x.EvidenceType = d.EvidenceType AND x.MaxAt = "
                  "d.CheckedAt WHERE d.CompanyCode = ? AND "
                  "d.SupplierCode = ? ORDER BY d.EvidenceType"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE DD-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE DD-SUPPLIER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 1 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-COL-WIDTH(5)
           MOVE 1 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2100-EXIT
           END-IF
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
                      OR DD-DETAIL-CNT >= 8
               ADD 1 TO DD-DETAIL-CNT
               ADD 1 TO DD-HELD-CNT
               MOVE DD-DETAIL-CNT TO WS-I
               MOVE WS-E-TYPE TO DD-D-TYPE(WS-I)
               MOVE WS-E-OUTCOME TO DD-D-RESULT(WS-I)
               MOVE WS-E-MANDATORY TO DD-D-MANDATORY(WS-I)
               MOVE WS-E-EXCEPTED TO DD-D-EXCEPTED(WS-I)
               MOVE 0 TO DD-D-VALID-TO(WS-I)
               IF WS-E-VALID-TO IS NUMERIC
                   MOVE WS-E-VALID-TO TO DD-D-VALID-TO(WS-I)
               END-IF
               IF WS-E-NEXT-REVIEW IS NUMERIC
                   MOVE WS-E-NEXT-REVIEW TO WS-REVIEW-DATE
                   IF WS-REVIEW-DATE > 0 AND
                      (DD-NEXT-REVIEW-DUE = 0 OR
                       WS-REVIEW-DATE < DD-NEXT-REVIEW-DUE)
                       MOVE WS-REVIEW-DATE TO DD-NEXT-REVIEW-DUE
                   END-IF
               END-IF
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> JUDGE WHAT IS HELD.  AN EXCEPTION FORGIVES EXPIRY AND A
      *> REVIEW RESULT; IT NEVER FORGIVES A BLOCKING FAILURE.
      *>----------------------------------------------------------------
       2200-JUDGE-HELD SECTION.
       2200-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > DD-DETAIL-CNT
               MOVE DD-D-VALID-TO(WS-I) TO WS-VALID-TO
               IF DD-D-EXCEPTED(WS-I) = "Y"
                   ADD 1 TO DD-EXCEPTED-CNT
               END-IF
               EVALUATE TRUE
                   WHEN DD-D-RESULT(WS-I) = "FAIL"
                       ADD 1 TO DD-FAIL-CNT
                       IF DD-WORST-TYPE = SPACES
                           MOVE DD-D-TYPE(WS-I) TO DD-WORST-TYPE
                       END-IF
                   WHEN DD-D-RESULT(WS-I) = "NOTHELD"
                       IF DD-D-MANDATORY(WS-I) = "Y" AND
                          DD-D-EXCEPTED(WS-I) NOT = "Y"
                           ADD 1 TO DD-MISSING-CNT
                           PERFORM 2450-ADD-MISSING THRU 2450-EXIT
                       END-IF
                   WHEN DD-D-RESULT(WS-I) = "EXPIRED"
                       IF DD-D-EXCEPTED(WS-I) NOT = "Y"
                           ADD 1 TO DD-EXPIRED-CNT
                           PERFORM 2460-NOTE-WORST THRU 2460-EXIT
                       END-IF
                   WHEN DD-D-RESULT(WS-I) = "REVIEW"
                       ADD 1 TO DD-REVIEW-CNT
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
      *>         A PASS THAT HAS RUN OUT OF DATE IS NOT A PASS.
               IF DD-D-RESULT(WS-I) = "PASS" AND WS-VALID-TO > 0 AND
                  WS-VALID-TO < DD-CHECK-DATE AND
                  DD-D-MANDATORY(WS-I) = "Y" AND
                  DD-D-EXCEPTED(WS-I) NOT = "Y"
                   ADD 1 TO DD-EXPIRED-CNT
                   MOVE "EXPIRED" TO DD-D-RESULT(WS-I)
                   PERFORM 2460-NOTE-WORST THRU 2460-EXIT
               END-IF
           END-PERFORM.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> JUDGE WHAT IS NOT HELD AT ALL.  THE BASELINE MANDATORY SET
      *> MUST BE PRESENT EVEN IF NOBODY EVER CREATED A ROW FOR IT.
      *>----------------------------------------------------------------
       2300-JUDGE-MANDATORY SECTION.
       2300-START.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-MAND-MAX
               MOVE "N" TO WS-HIT
               PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I > DD-DETAIL-CNT
                   IF DD-D-TYPE(WS-I) = WS-MAND-TYPE(WS-J) AND
                      DD-D-RESULT(WS-I) NOT = "NOTHELD"
                       MOVE "Y" TO WS-HIT
                   END-IF
               END-PERFORM
               IF WS-HIT NOT = "Y"
                   ADD 1 TO DD-MISSING-CNT
                   MOVE WS-MAND-TYPE(WS-J) TO WS-E-TYPE
                   PERFORM 2470-APPEND-MISSING THRU 2470-EXIT
                   IF DD-WORST-TYPE = SPACES
                       MOVE WS-MAND-TYPE(WS-J) TO DD-WORST-TYPE
                   END-IF
               END-IF
           END-PERFORM.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2400-MISSING-LIST SECTION.
       2400-START.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-MAND-MAX
               ADD 1 TO DD-MISSING-CNT
               MOVE WS-MAND-TYPE(WS-J) TO WS-E-TYPE
               PERFORM 2470-APPEND-MISSING THRU 2470-EXIT
           END-PERFORM.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2450-ADD-MISSING SECTION.
       2450-START.
           MOVE DD-D-TYPE(WS-I) TO WS-E-TYPE
           PERFORM 2470-APPEND-MISSING THRU 2470-EXIT
           IF DD-WORST-TYPE = SPACES
               MOVE DD-D-TYPE(WS-I) TO DD-WORST-TYPE
           END-IF.
       2450-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2460-NOTE-WORST SECTION.
       2460-START.
           IF DD-WORST-TYPE = SPACES
               MOVE DD-D-TYPE(WS-I) TO DD-WORST-TYPE
           END-IF.
       2460-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2470-APPEND-MISSING SECTION.
       2470-START.
           IF DD-MISSING-LIST = SPACES
               MOVE FUNCTION TRIM(WS-E-TYPE) TO DD-MISSING-LIST
           ELSE
               IF FUNCTION LENGTH(FUNCTION TRIM(DD-MISSING-LIST)) < 170
                   MOVE SPACES TO WS-MISS-WORK
                   STRING FUNCTION TRIM(DD-MISSING-LIST) ", "
                          FUNCTION TRIM(WS-E-TYPE)
                       DELIMITED BY SIZE INTO WS-MISS-WORK
                   MOVE WS-MISS-WORK TO DD-MISSING-LIST
               END-IF
           END-IF.
       2470-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE VERDICT.  WORST RESULT WINS.
      *>----------------------------------------------------------------
       2500-VERDICT SECTION.
       2500-START.
           EVALUATE TRUE
               WHEN DD-FAIL-CNT > 0
                   MOVE "BLOCKED" TO DD-OVERALL
                   MOVE "N" TO DD-CAN-ORDER
                   MOVE 8 TO CA-RETURN-CODE
                   MOVE "DIL0003" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Supplier " FUNCTION TRIM(DD-SUPPLIER)
                          " has a blocking screening result ("
                          FUNCTION TRIM(DD-WORST-TYPE)
                          ") and no order may be issued to them"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN DD-EXPIRED-CNT > 0
                   MOVE "EXCEPTION" TO DD-OVERALL
                   MOVE "N" TO DD-CAN-ORDER
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "DIL0004" TO CA-REASON-CODE
                   MOVE DD-EXPIRED-CNT TO WS-CNT-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Mandatory due diligence evidence has "
                          "expired (" FUNCTION TRIM(WS-CNT-ED)
                          " item(s), worst "
                          FUNCTION TRIM(DD-WORST-TYPE)
                          ") - an authorised exception is required"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN DD-MISSING-CNT > 0
                   MOVE "EXCEPTION" TO DD-OVERALL
                   MOVE "N" TO DD-CAN-ORDER
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "DIL0005" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Mandatory due diligence evidence has "
                          "never been held: "
                          FUNCTION TRIM(DD-MISSING-LIST)
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN DD-REVIEW-CNT > 0
                   MOVE "REVIEW" TO DD-OVERALL
                   MOVE "Y" TO DD-CAN-ORDER
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "DIL0006" TO CA-REASON-CODE
                   MOVE DD-REVIEW-CNT TO WS-CNT-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING FUNCTION TRIM(WS-CNT-ED)
                          " due diligence result(s) for supplier "
                          FUNCTION TRIM(DD-SUPPLIER)
                          " need review before the next order"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN OTHER
                   MOVE "PASS" TO DD-OVERALL
                   MOVE "Y" TO DD-CAN-ORDER
                   MOVE 0 TO CA-RETURN-CODE
           END-EVALUATE.
       2500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> RECORD ONE PIECE OF EVIDENCE.
      *>----------------------------------------------------------------
       3000-RECORD SECTION.
       3000-START.
           IF NOT DD-TYPE-KNOWN
               MOVE 8 TO CA-RETURN-CODE
               MOVE "DIL0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Evidence type "
                      FUNCTION TRIM(DD-EVIDENCE-TYPE)
                      " is not a recognised due diligence check"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF DD-RESULT = SPACES
               MOVE "PASS" TO DD-RESULT
           END-IF
           IF DD-SANCTIONS
               MOVE "Y" TO DD-IS-BLOCKING
               MOVE "Y" TO DD-IS-MANDATORY
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.SupplierDiligence (CompanyCode, "
                  "SupplierCode, EvidenceType, EvidenceRef, Outcome, "
                  "IsBlocking, IsMandatory, CheckedAt, CheckedBy, "
                  "ValidFrom, ValidTo, NextReviewDate, Note) OUTPUT "
                  "CAST(INSERTED.DiligenceId AS varchar(20)) VALUES "
                  "(?, ?, ?, ?, ?, ?, ?, SYSDATETIME(), ?, "
                  "TRY_CONVERT(date, NULLIF(?, '00000000')), "
                  "TRY_CONVERT(date, NULLIF(?, '00000000')), "
                  "TRY_CONVERT(date, NULLIF(?, '00000000')), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE DD-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE DD-EVIDENCE-TYPE TO SQLIO-PARM-VAL(3)
           MOVE DD-EVIDENCE-REF TO SQLIO-PARM-VAL(4)
           MOVE DD-RESULT TO SQLIO-PARM-VAL(5)
           IF DD-BLOCKING
               MOVE "1" TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(6)
           END-IF
           IF DD-MANDATORY
               MOVE "1" TO SQLIO-PARM-VAL(7)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(7)
           END-IF
           IF DD-CHECKED-BY = SPACES
               MOVE CA-USER TO SQLIO-PARM-VAL(8)
           ELSE
               MOVE DD-CHECKED-BY TO SQLIO-PARM-VAL(8)
           END-IF
           MOVE DD-VALID-FROM TO SQLIO-PARM-VAL(9)
           MOVE DD-VALID-TO TO SQLIO-PARM-VAL(10)
           MOVE DD-NEXT-REVIEW TO SQLIO-PARM-VAL(11)
           MOVE DD-NOTE TO SQLIO-PARM-VAL(12)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           COMPUTE DD-DILIGENCE-ID = FUNCTION NUMVAL(WS-NEW-ID)
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(DD-EVIDENCE-TYPE) " evidence "
                  FUNCTION TRIM(DD-RESULT) " recorded for supplier "
                  FUNCTION TRIM(DD-SUPPLIER)
               DELIMITED BY SIZE INTO CA-MESSAGE
           IF DD-RES-FAIL AND DD-BLOCKING
               MOVE 4 TO CA-RETURN-CODE
               MOVE "DIL0003" TO CA-REASON-CODE
               MOVE "BLOCKED" TO DD-OVERALL
               MOVE "N" TO DD-CAN-ORDER
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN AUTHORISED EXCEPTION FOR EXPIRED OR MISSING EVIDENCE.  A
      *> BLOCKING FAILURE IS NOT EXCEPTABLE HERE AT ALL.
      *>----------------------------------------------------------------
       4000-EXCEPTION SECTION.
       4000-START.
           IF DD-EXCEPTION-BY = SPACES OR
              DD-EXCEPTION-REASON = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "DIL0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An exception to the due diligence policy "
                      "needs both an approver and a reason"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           IF NOT DD-TYPE-KNOWN
               MOVE 8 TO CA-RETURN-CODE
               MOVE "DIL0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Evidence type "
                      FUNCTION TRIM(DD-EVIDENCE-TYPE)
                      " is not a recognised due diligence check"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE d SET ExceptionBy = ?, ExceptionReason = ?"
                  " OUTPUT CAST(INSERTED.DiligenceId AS varchar(20))"
                  " FROM po.SupplierDiligence d WHERE "
                  "d.CompanyCode = ? AND d.SupplierCode = ? AND "
                  "d.EvidenceType = ? AND d.IsBlocking = 0 AND "
                  "d.CheckedAt = (SELECT MAX(CheckedAt) FROM "
                  "po.SupplierDiligence WHERE CompanyCode = "
                  "d.CompanyCode AND SupplierCode = d.SupplierCode "
                  "AND EvidenceType = d.EvidenceType)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE DD-EXCEPTION-BY TO SQLIO-PARM-VAL(1)
           MOVE DD-EXCEPTION-REASON TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE DD-SUPPLIER TO SQLIO-PARM-VAL(4)
           MOVE DD-EVIDENCE-TYPE TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO CA-RETURN-CODE
               MOVE "DIL0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "There is no exceptable "
                      FUNCTION TRIM(DD-EVIDENCE-TYPE)
                      " evidence for supplier "
                      FUNCTION TRIM(DD-SUPPLIER)
                      " - a blocking result cannot be excepted"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "N" TO DD-CAN-ORDER
               GO TO 4000-EXIT
           END-IF
           COMPUTE DD-DILIGENCE-ID = FUNCTION NUMVAL(WS-NEW-ID)
           ADD 1 TO DD-EXCEPTED-CNT
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(DD-EVIDENCE-TYPE)
                  " exception authorised by "
                  FUNCTION TRIM(DD-EXCEPTION-BY)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-SQL-FAIL SECTION.
       9000-START.
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           MOVE "N" TO DD-CAN-ORDER
           MOVE "Y" TO WS-FATAL-SW.
       9000-EXIT.
           EXIT.
       END PROGRAM POSUPCHK.
