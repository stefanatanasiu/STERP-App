      *>================================================================
      *> PROGRAM-ID : NUMGEN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : DOCUMENT NUMBERING (COM-005, D-15).  ONE UNIQUE
      *>              CONTINUOUS SEQUENCE PER COMPANY AND DOCUMENT
      *>              TYPE.  THE NUMBER IS ALLOCATED BY
      *>              core.usp_NextDocumentNumber, WHICH TAKES THE
      *>              SERIES ROW UNDER UPDLOCK / HOLDLOCK SO THAT TWO
      *>              SIMULTANEOUS CONFIRMATIONS CANNOT PRODUCE THE
      *>              SAME CONFIRMED REFERENCE.  A VOIDED NUMBER STAYS
      *>              TRACEABLE AND IS NEVER REISSUED.
      *>              THE CORRELATION ID IS PASSED THROUGH SO THAT A
      *>              REPEATED INSTRUCTION RETURNS THE NUMBER ALREADY
      *>              ISSUED RATHER THAN BURNING A SECOND ONE
      *>              (COM-015).
      *> CALLED BY  : AR, AP, SO, PO, FIN, KIT AND THE .NET CLIENT
      *> INTERFACE  : CALL "NUMGEN" USING WS-NUMGEN-CA
      *>              COPYBOOK NUMGENCA
      *> CALLS      : SQLIO -> core.usp_NextDocumentNumber
      *>                       core.usp_VoidDocumentNumber
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-02-04 RJH  CR0011 INVOICE NUMBER ALLOCATION
      *> 1996-09-17 DJM  CR0633 SEPARATE SERIES PER DOCUMENT TYPE AFTER
      *>                 THE 1996 VAT INSPECTION
      *> 2026-10-05 STERP core.NumberSeries, VOID WITH A REASON
      *>================================================================
      *> REQ: COM-005, COM-015
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMGEN.
       AUTHOR. R J HARGREAVES.
       DATE-WRITTEN. 04/02/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-REPLAYED             PIC X VALUE "N".
       01  WS-REPLAY-ROW.
           05  WS-R-NUMBER             PIC X(30).
           05  WS-R-SEQ                PIC X(12).
           05  WS-R-STATUS             PIC X(12).
       01  WS-NEXT-ROW.
           05  WS-N-COMPANY            PIC X(10).
           05  WS-N-SERIES             PIC X(20).
           05  WS-N-NUMBER             PIC X(30).
           05  WS-N-SEQ                PIC X(12).
       01  WS-VOID-ROW.
           05  WS-V-COMPANY            PIC X(10).
           05  WS-V-SERIES             PIC X(20).
           05  WS-V-NUMBER             PIC X(30).
           05  WS-V-STATUS             PIC X(12).
           05  WS-V-REASON             PIC X(200).
       LINKAGE SECTION.
       COPY NUMGENCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "NEXT"     PERFORM 1000-NEXT-NUMBER
               WHEN "VOID"     PERFORM 2000-VOID-NUMBER
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR
           MOVE "NUMGEN" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-005" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO NG-STATUS
           MOVE FUNCTION UPPER-CASE(NG-SERIES-CODE) TO NG-SERIES-CODE.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "NUM0001" TO CA-REASON-CODE
               MOVE "A company code is required for numbering"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF NG-SERIES-CODE = SPACES OR NG-SERIES-CODE = LOW-VALUES
               MOVE "NUM0002" TO CA-REASON-CODE
               MOVE "A document series code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "VOID"
               IF NG-DOCUMENT-NUMBER = SPACES
                   MOVE "NUM0003" TO CA-REASON-CODE
                   MOVE "The document number to void is required"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
               IF NG-REASON-TEXT = SPACES
      *>           COM-013 - a void without a reason leaves no evidence
                   MOVE "NUM0004" TO CA-REASON-CODE
                   MOVE "A reason is required to void a reference"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-NEXT-NUMBER SECTION.
       1000-START.
           MOVE SPACES TO NG-DOCUMENT-NUMBER
           MOVE 0 TO NG-SEQUENCE
      *>     COM-015 - a repeated instruction must not burn a second
      *>     reference.  core.usp_NextDocumentNumber records the
      *>     correlation id but never looks one up, so the replay is
      *>     decided here before any number is drawn.
           MOVE "N" TO WS-REPLAYED
           IF CA-CORRELATION-ID NOT = SPACES
               PERFORM 1050-REPLAY-ISSUED
               IF WS-REPLAYED = "Y"
                   GO TO 1000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_NextDocumentNumber ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE NG-SERIES-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           IF CA-CORRELATION-ID = SPACES
               MOVE SPACES TO SQLIO-PARM-VAL(4)
               MOVE "Y" TO SQLIO-PARM-NULL(4)
           ELSE
               MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(4)
               MOVE "N" TO SQLIO-PARM-NULL(4)
           END-IF
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-NEXT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "NUM0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No active number series "
                      FUNCTION TRIM(NG-SERIES-CODE)
                      " for company " FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 1000-EXIT
           END-IF
           MOVE WS-N-NUMBER TO NG-DOCUMENT-NUMBER
           COMPUTE NG-SEQUENCE = FUNCTION NUMVAL(WS-N-SEQ)
           MOVE "ISSUED" TO NG-STATUS
           MOVE SPACES TO CA-MESSAGE
           STRING "Reference " FUNCTION TRIM(NG-DOCUMENT-NUMBER)
                  " issued from series "
                  FUNCTION TRIM(NG-SERIES-CODE)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1050 - COM-015.  Has this very instruction already drawn a
      *> reference?  The correlation id is stored on core.DocumentNumber
      *> when the number is issued, so a confirmed instruction that is
      *> sent twice hands back the first reference instead of consuming
      *> another one and leaving an unexplained gap in a legal sequence.
      *>----------------------------------------------------------------
      *> REQ: COM-015, COM-005
       1050-REPLAY-ISSUED SECTION.
       1050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) DocumentNumber, "
                  "CAST(SequenceNo AS varchar(12)), Status "
                  "FROM core.DocumentNumber "
                  "WHERE CompanyCode = ? AND SeriesCode = ? "
                  "AND CorrelationId = ? ORDER BY SequenceNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE NG-SERIES-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-REPLAY-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               MOVE "Y" TO WS-REPLAYED
               GO TO 1050-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 1050-EXIT
           END-IF
           MOVE WS-R-NUMBER TO NG-DOCUMENT-NUMBER
           COMPUTE NG-SEQUENCE = FUNCTION NUMVAL(WS-R-SEQ)
           MOVE WS-R-STATUS TO NG-STATUS
           MOVE 4 TO CA-RETURN-CODE
           MOVE "NUM0007" TO CA-REASON-CODE
           ADD 1 TO CA-MSG-COUNT
           MOVE SPACES TO CA-MESSAGE
           STRING "Reference " FUNCTION TRIM(NG-DOCUMENT-NUMBER)
                  " was already issued for this instruction"
               DELIMITED BY SIZE INTO CA-MESSAGE
           MOVE "Y" TO WS-REPLAYED.
       1050-EXIT.
           EXIT.

      *>   D-15 - a voided number is retained with its reason and is
      *>   never reused, so that the sequence stays explainable
       2000-VOID-NUMBER SECTION.       2000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_VoidDocumentNumber ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE NG-SERIES-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE NG-DOCUMENT-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE NG-REASON-TEXT TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 200 TO SQLIO-COL-WIDTH(5)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VOID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "NUM0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Reference " FUNCTION TRIM(NG-DOCUMENT-NUMBER)
                      " was never issued from this series"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           MOVE WS-V-STATUS TO NG-STATUS
           MOVE SPACES TO CA-MESSAGE
           STRING "Reference " FUNCTION TRIM(NG-DOCUMENT-NUMBER)
                  " voided and retained - it will not be reissued"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "NUM0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "NUMGEN does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

      *>   business refusals arrive as THROW 5xxxx 'STERP-E-nnnn: ...'
       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in NUMGEN" TO CA-MESSAGE
           END-IF
           IF CA-MESSAGE(1:8) = "STERP-E-"
              OR CA-MESSAGE(1:8) = "STERP-W-"
               MOVE "NUM0008" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
