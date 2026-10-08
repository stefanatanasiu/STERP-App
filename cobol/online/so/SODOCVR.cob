      *>================================================================
      *> PROGRAM-ID : SODOCVR
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : DOCUMENT VERSIONS AND REPRINTS.  QUOTES,
      *>              ACKNOWLEDGEMENTS, PICK NOTES, DESPATCH NOTES AND
      *>              INVOICES ARE SNAPSHOTTED WITH A CONTENT HASH SO
      *>              THAT WHAT THE CUSTOMER ACCEPTED CAN BE
      *>              REPRODUCED EXACTLY YEARS LATER (SO-022).
      *>              A REISSUE REPRODUCES A HELD VERSION AND CHANGES
      *>              NO BUSINESS STATE AT ALL.  IT CONCEALS NOTHING:
      *>              THE REPRINT COUNT RISES AND AN AUDIT RECORD IS
      *>              WRITTEN THROUGH AUDITLOG.
      *>              THE SNAPSHOT TRAVELS AS 24 LINES OF 200
      *>              CHARACTERS BECAUSE SQLIO BINDS PARAMETERS OF 256
      *>              BYTES.  EACH LINE IS STORED PADDED TO EXACTLY
      *>              200 CHARACTERS, SO READING IT BACK IN 200
      *>              CHARACTER SLICES REPRODUCES THE ORIGINAL LINES
      *>              EXACTLY, AND SQL SERVER'S SHA2_256 OVER THE
      *>              STORED TEXT PROVES IT.
      *> CALLED BY  : STERP DESKTOP CLIENT (DOCUMENT HISTORY), SOACK
      *> INTERFACE  : CALL "SODOCVR" USING DFHCOMMAREA (COPY SODOCVRCA)
      *> CALLS      : AUDITLOG, SQLIO -> so.DocumentVersion
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-05-30 RJH  CR0352 DOCUMENT REPRINT CONTROL
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON ISSUE STAMPS
      *> 2012-04-18 DJM  CR2210 CONTENT HASH ON ACCEPTED DOCUMENTS
      *> 2026-10-06 STERP SO-022 VERSIONS, REPRINTS AND COMPARISON
      *>================================================================
      *> REQ: SO-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SODOCVR.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 30/05/1995.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       01  WS-CONTROL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BDATE                PIC 9(8).
           05  WS-CORR                 PIC X(36).
           05  WS-ERROR                PIC X VALUE "N".
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-K                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(15)V9(6) VALUE 0.
           05  WS-ROWS                 PIC S9(4) VALUE 0.
           05  WS-DIFFS                PIC S9(4) VALUE 0.
           05  WS-READ-VERSION         PIC S9(4) VALUE 0.
           05  WS-VER-A                PIC S9(4) VALUE 0.
           05  WS-HASH-A               PIC X(64) VALUE SPACES.
           05  WS-INT-ED               PIC -(8)9.
           05  WS-SQL-BUILD            PIC X(4000).
       01  WS-VER-ROW.
           05  WV-ID                   PIC X(24).
           05  WV-VERSION              PIC X(8).
           05  WV-ISSUED-AT            PIC X(23).
           05  WV-ISSUED-BY            PIC X(20).
           05  WV-HASH                 PIC X(64).
           05  WV-REPRINTS             PIC X(12).
           05  WV-SUMMARY              PIC X(100).
           05  WV-SNAP-TAB.
               10  WV-SNAP             PIC X(200) OCCURS 24 TIMES.
       01  WS-LIST-ROW.
           05  WM-VERSION              PIC X(8).
           05  WM-ISSUED-AT            PIC X(23).
           05  WM-ISSUED-BY            PIC X(20).
           05  WM-HASH                 PIC X(64).
           05  WM-REPRINTS             PIC X(12).
           05  WM-SUMMARY              PIC X(100).
       01  WS-SNAP-ROW.
           05  WS-S-ID                 PIC X(24).
           05  WS-S-VERSION            PIC X(8).
           05  WS-S-HASH               PIC X(64).
           05  WS-S-WRITTEN            PIC X(2).
           05  WS-S-PRIOR              PIC X(8).
       01  WS-ONE-COL.
           05  WS-C1                   PIC X(40).
       LINKAGE SECTION.
       COPY SODOCVRCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 1000-INITIALISE THRU 1000-EXIT
           IF WS-ERROR = "N"
               EVALUATE WS-FUNC
                   WHEN "SNAPSHOT"
                       PERFORM 2000-SNAPSHOT THRU 2000-EXIT
                   WHEN "REISSUE"
                       PERFORM 3000-REISSUE THRU 3000-EXIT
                   WHEN "LIST"
                       PERFORM 4000-LIST THRU 4000-EXIT
                   WHEN "COMPARE"
                       PERFORM 5000-COMPARE THRU 5000-EXIT
                   WHEN OTHER
                       PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
               END-EVALUATE
           END-IF
           PERFORM 9400-REPORT THRU 9400-EXIT.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INITIALISE SECTION.
       1000-START.
           MOVE "N" TO WS-ERROR
           MOVE 0 TO WS-RC WS-I WS-J WS-K WS-ROWS WS-DIFFS
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE "SODOCVR" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-022" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORR
      *>   a reissue is evidence; it never changes business state
           MOVE "N" TO DV-STATE-CHANGED
           MOVE "N" TO DV-WAS-WRITTEN
           MOVE "N" TO DV-IS-IDENTICAL
           MOVE "N" TO DV-LIST-TRUNCATED
           MOVE 0 TO DV-DIFF-CNT
           MOVE 0 TO DV-LIST-CNT
           IF WS-BDATE = 0
               MOVE 20261006 TO WS-BDATE
               MOVE WS-BDATE TO CA-BUSINESS-DATE OF DFHCOMMAREA
           END-IF
           IF DV-SNAP-CNT > 24
               MOVE 24 TO DV-SNAP-CNT
           END-IF
           IF WS-CO = SPACES
               MOVE "DOC0001" TO WS-REASON
               MOVE "A company code is required" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF DV-DOCUMENT-TYPE = SPACES OR DV-DOCUMENT-REF = SPACES
               MOVE "DOC0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A document type and reference are required"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           MOVE FUNCTION UPPER-CASE(DV-DOCUMENT-TYPE)
               TO DV-DOCUMENT-TYPE
           IF NOT DV-TYPE-QUOTE AND NOT DV-TYPE-ACK AND
              NOT DV-TYPE-PICK AND NOT DV-TYPE-DESPATCH AND
              NOT DV-TYPE-INVOICE
               MOVE "DOC0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Document type "
                      FUNCTION TRIM(DV-DOCUMENT-TYPE)
                      " is not QUOTE, ACKNOWLEDGE, PICKNOTE, "
                      "DESPATCHNOTE or INVOICE"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2000  SNAPSHOT.  THE TEXT IS HASHED BY SQL SERVER SO THE SAME
      *>       CONTENT ALWAYS GIVES THE SAME HASH WHOEVER ASKS.  WHEN
      *>       THE HASH MATCHES THE LATEST HELD VERSION NOTHING IS
      *>       WRITTEN - A REPRINT OF AN UNCHANGED DOCUMENT IS NOT A
      *>       NEW VERSION OF IT (DOC0008).
      *>----------------------------------------------------------------
       2000-SNAPSHOT SECTION.
       2000-START.
           IF DV-SNAP-CNT = 0
               MOVE "DOC0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A snapshot must carry at least one line of "
                      "the document that was issued"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-BUILD-STATEMENT THRU 2100-EXIT
           PERFORM 2200-BIND-AND-RUN THRU 2200-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           IF DV-WAS-WRITTEN = "N"
               MOVE 4 TO WS-RC
               MOVE "DOC0008" TO WS-REASON
               MOVE DV-VERSION-NO TO WS-INT-ED
               MOVE SPACES TO WS-MSG
               STRING "The document is unchanged from version "
                      FUNCTION TRIM(WS-INT-ED)
                      "; no new version has been written"
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE "Y" TO DV-IS-IDENTICAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE DV-VERSION-NO TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           STRING "Version " FUNCTION TRIM(WS-INT-ED) " of "
                  FUNCTION TRIM(DV-DOCUMENT-TYPE) " "
                  FUNCTION TRIM(DV-DOCUMENT-REF)
                  " stored with content hash "
                  DV-CONTENT-HASH(1:16)
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2100  THE STATEMENT IS BUILT TO THE NUMBER OF LINES ACTUALLY
      *>       SUPPLIED.  EACH LINE IS STORED PADDED TO 200
      *>       CHARACTERS, WHICH IS WHAT MAKES THE READ BACK EXACT.
      *>----------------------------------------------------------------
       2100-BUILD-STATEMENT SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DECLARE @t varchar(max) = CONCAT("
                  "LEFT(?+REPLICATE(' ',200),200)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > DV-SNAP-CNT
               MOVE SPACES TO WS-SQL-BUILD
               STRING FUNCTION TRIM(SQLIO-STMT)
                      ",LEFT(?+REPLICATE(' ',200),200)"
                   DELIMITED BY SIZE INTO WS-SQL-BUILD
               MOVE WS-SQL-BUILD TO SQLIO-STMT
           END-PERFORM
           MOVE SPACES TO WS-SQL-BUILD
           STRING FUNCTION TRIM(SQLIO-STMT)
                  "); DECLARE @h varchar(64) = CONVERT(varchar(64), "
                  "HASHBYTES('SHA2_256', @t), 2); DECLARE @v "
                  "smallint = ?, @pv smallint, @ph varchar(64); "
                  "SELECT TOP (1) @pv = VersionNo, @ph = ContentHash "
                  "FROM so.DocumentVersion WHERE CompanyCode = ? AND "
                  "DocumentType = ? AND DocumentRef = ? ORDER BY "
                  "VersionNo DESC; "
               DELIMITED BY SIZE INTO WS-SQL-BUILD
           MOVE WS-SQL-BUILD TO SQLIO-STMT
           MOVE SPACES TO WS-SQL-BUILD
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " IF @ph IS NOT NULL AND @ph = @h AND @v = 0 "
                  "SELECT '0', CAST(@pv AS varchar(8)), @h, 'N', "
                  "CAST(@pv AS varchar(8)); ELSE BEGIN IF @v = 0 SET "
                  "@v = ISNULL(@pv,0) + 1; INSERT so.DocumentVersion "
                  "(CompanyCode, DocumentType, DocumentRef, "
                  "VersionNo, IssuedAt, IssuedBy, ContentHash, "
                  "Snapshot, ReprintCount, ChangeSummary) VALUES "
                  "(?, ?, ?, @v, SYSDATETIME(), NULLIF(?,''), @h, "
                  "@t, 0, NULLIF(?,'')); SELECT "
                  "CAST(SCOPE_IDENTITY() AS varchar(24)), CAST(@v AS "
                  "varchar(8)), @h, 'Y', CAST(ISNULL(@pv,0) AS "
                  "varchar(8)); END"
               DELIMITED BY SIZE INTO WS-SQL-BUILD
           MOVE WS-SQL-BUILD TO SQLIO-STMT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2200-BIND-AND-RUN SECTION.
       2200-START.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > DV-SNAP-CNT
               MOVE DV-S-TEXT(WS-I) TO SQLIO-PARM-VAL(WS-I)
           END-PERFORM
           COMPUTE WS-J = DV-SNAP-CNT + 1
           MOVE DV-VERSION-NO TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(WS-J)
           ADD 1 TO WS-J
           MOVE WS-CO TO SQLIO-PARM-VAL(WS-J)
           ADD 1 TO WS-J
           MOVE DV-DOCUMENT-TYPE TO SQLIO-PARM-VAL(WS-J)
           ADD 1 TO WS-J
           MOVE DV-DOCUMENT-REF TO SQLIO-PARM-VAL(WS-J)
           ADD 1 TO WS-J
           MOVE WS-CO TO SQLIO-PARM-VAL(WS-J)
           ADD 1 TO WS-J
           MOVE DV-DOCUMENT-TYPE TO SQLIO-PARM-VAL(WS-J)
           ADD 1 TO WS-J
           MOVE DV-DOCUMENT-REF TO SQLIO-PARM-VAL(WS-J)
           ADD 1 TO WS-J
           IF DV-ISSUED-BY = SPACES
               MOVE WS-USER TO DV-ISSUED-BY
           END-IF
           MOVE DV-ISSUED-BY TO SQLIO-PARM-VAL(WS-J)
           ADD 1 TO WS-J
           MOVE DV-CHANGE-SUMMARY(1:200) TO SQLIO-PARM-VAL(WS-J)
           MOVE WS-J TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 64 TO SQLIO-COL-WIDTH(3)
           MOVE 2 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SNAP-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 2200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "Y" TO WS-ERROR
               MOVE 12 TO WS-RC
               MOVE "DOC0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "The document version store returned no "
                      "result for that snapshot"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 2200-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-ID)
           MOVE WS-N TO DV-VERSION-ID
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-VERSION)
           MOVE WS-N TO DV-VERSION-NO
           MOVE WS-S-HASH TO DV-CONTENT-HASH
           MOVE WS-S-WRITTEN(1:1) TO DV-WAS-WRITTEN
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-PRIOR)
           MOVE WS-N TO DV-LATEST-VERSION
           IF DV-WAS-WRITTEN = "Y"
               MOVE DV-VERSION-NO TO DV-LATEST-VERSION
           END-IF.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 3000  REISSUE.  THE HELD VERSION IS READ BACK UNCHANGED.  THE
      *>       ONLY THING THAT MOVES IS THE REPRINT COUNT, AND THE
      *>       REPRINT IS WRITTEN TO THE AUDIT LOG, SO A REISSUE CAN
      *>       NEVER BE USED TO QUIETLY REWRITE HISTORY (SO-022).
      *>----------------------------------------------------------------
       3000-REISSUE SECTION.
       3000-START.
           MOVE DV-VERSION-NO TO WS-READ-VERSION
           PERFORM 8000-READ-VERSION THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               MOVE WV-SNAP(WS-I) TO DV-S-TEXT(WS-I)
           END-PERFORM
           MOVE 24 TO DV-SNAP-CNT
           PERFORM 8100-TRIM-SNAPSHOT THRU 8100-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DocumentVersion SET ReprintCount = "
                  "ReprintCount + 1 WHERE DocVersionId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE DV-VERSION-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           ADD 1 TO DV-REPRINT-COUNT
           MOVE "N" TO DV-STATE-CHANGED
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE DV-VERSION-NO TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           STRING "Version " FUNCTION TRIM(WS-INT-ED) " of "
                  FUNCTION TRIM(DV-DOCUMENT-TYPE) " "
                  FUNCTION TRIM(DV-DOCUMENT-REF)
                  " reissued exactly as it was accepted; no "
                  "business state has changed"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4000-LIST SECTION.
       4000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(VersionNo AS varchar(8)), "
                  "CONVERT(varchar(23), IssuedAt, 121), "
                  "ISNULL(IssuedBy,''), ISNULL(ContentHash,''), "
                  "CAST(ReprintCount AS varchar(12)), "
                  "ISNULL(ChangeSummary,'') FROM so.DocumentVersion "
                  "WHERE CompanyCode = ? AND DocumentType = ? AND "
                  "DocumentRef = ? ORDER BY VersionNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE DV-DOCUMENT-TYPE TO SQLIO-PARM-VAL(2)
           MOVE DV-DOCUMENT-REF TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 23 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 64 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 100 TO SQLIO-COL-WIDTH(6)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LIST-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO WS-ROWS
           PERFORM 4100-ONE-VERSION THRU 4100-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR OR WS-ROWS >= 20
           IF NOT SQLIO-NOTFOUND AND NOT SQLIO-ERROR
               MOVE "Y" TO DV-LIST-TRUNCATED
           END-IF
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LIST-ROW
           MOVE WS-ROWS TO DV-LIST-CNT
           IF WS-ROWS = 0
               MOVE 16 TO WS-RC
               MOVE "DOC0005" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "No version of "
                      FUNCTION TRIM(DV-DOCUMENT-TYPE) " "
                      FUNCTION TRIM(DV-DOCUMENT-REF) " is held"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 4000-EXIT
           END-IF
           MOVE WS-ROWS TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           STRING FUNCTION TRIM(WS-INT-ED)
                  " version(s) are held for "
                  FUNCTION TRIM(DV-DOCUMENT-REF)
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       4000-EXIT.
           EXIT.

       4100-ONE-VERSION SECTION.
       4100-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LIST-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 4100-EXIT
           END-IF
           ADD 1 TO WS-ROWS
           MOVE WS-ROWS TO WS-I
           COMPUTE WS-N = FUNCTION NUMVAL(WM-VERSION)
           MOVE WS-N TO DV-V-VERSION(WS-I)
           MOVE WS-N TO DV-LATEST-VERSION
           MOVE WM-ISSUED-AT TO DV-V-ISSUED-AT(WS-I)
           MOVE WM-ISSUED-BY TO DV-V-ISSUED-BY(WS-I)
           MOVE WM-HASH TO DV-V-HASH(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WM-REPRINTS)
           MOVE WS-N TO DV-V-REPRINTS(WS-I)
           MOVE WM-SUMMARY TO DV-V-SUMMARY(WS-I).
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 5000  COMPARE.  THE HASHES ANSWER "IS IT THE SAME?" AND THE
      *>       LINES ANSWER "WHAT CHANGED?".
      *>----------------------------------------------------------------
       5000-COMPARE SECTION.
       5000-START.
           IF DV-VERSION-NO = 0 OR DV-COMPARE-VERSION = 0 OR
              DV-VERSION-NO = DV-COMPARE-VERSION
               MOVE "DOC0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Name two different versions to compare"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE DV-VERSION-NO TO WS-VER-A
           MOVE WS-VER-A TO WS-READ-VERSION
           PERFORM 8000-READ-VERSION THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               MOVE WV-SNAP(WS-I) TO DV-S-TEXT(WS-I)
           END-PERFORM
           MOVE DV-CONTENT-HASH TO WS-HASH-A
           MOVE DV-COMPARE-VERSION TO WS-READ-VERSION
           PERFORM 8000-READ-VERSION THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               MOVE WV-SNAP(WS-I) TO DV-C-TEXT(WS-I)
           END-PERFORM
           MOVE DV-CONTENT-HASH TO DV-COMPARE-HASH
           MOVE WS-HASH-A TO DV-CONTENT-HASH
           MOVE WS-VER-A TO DV-VERSION-NO
           MOVE 24 TO DV-SNAP-CNT
           MOVE 24 TO DV-CMP-CNT
           MOVE 0 TO WS-DIFFS
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               IF DV-S-TEXT(WS-I) NOT = DV-C-TEXT(WS-I)
                   IF WS-DIFFS < 24
                       ADD 1 TO WS-DIFFS
                       MOVE WS-DIFFS TO WS-K
                       MOVE WS-I TO WS-INT-ED
                       MOVE SPACES TO DV-F-FIELD(WS-K)
                       STRING "Snapshot line "
                              FUNCTION TRIM(WS-INT-ED)
                           DELIMITED BY SIZE INTO DV-F-FIELD(WS-K)
                       MOVE DV-S-TEXT(WS-I)(1:100)
                           TO DV-F-OLD(WS-K)
                       MOVE DV-C-TEXT(WS-I)(1:100)
                           TO DV-F-NEW(WS-K)
                   END-IF
               END-IF
           END-PERFORM
           MOVE WS-DIFFS TO DV-DIFF-CNT
           IF DV-CONTENT-HASH = DV-COMPARE-HASH
               MOVE "Y" TO DV-IS-IDENTICAL
           ELSE
               MOVE "N" TO DV-IS-IDENTICAL
           END-IF
           MOVE DV-VERSION-NO TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           IF DV-IS-IDENTICAL = "Y"
               STRING "The two versions have the same content hash; "
                      "nothing changed"
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               MOVE WS-DIFFS TO WS-INT-ED
               STRING FUNCTION TRIM(WS-INT-ED)
                      " line(s) differ between the two versions"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-READ-VERSION SECTION.
       8000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(DocVersionId AS varchar(24)), "
                  "CAST(VersionNo AS varchar(8)), "
                  "CONVERT(varchar(23), IssuedAt, 121), "
                  "ISNULL(IssuedBy,''), ISNULL(ContentHash,''), "
                  "CAST(ReprintCount AS varchar(12)), "
                  "ISNULL(ChangeSummary,'')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               COMPUTE WS-K = (WS-I - 1) * 200 + 1
               MOVE WS-K TO WS-INT-ED
               MOVE SPACES TO WS-SQL-BUILD
               STRING FUNCTION TRIM(SQLIO-STMT)
                      ", ISNULL(SUBSTRING(Snapshot, "
                      FUNCTION TRIM(WS-INT-ED) ", 200),'')"
                   DELIMITED BY SIZE INTO WS-SQL-BUILD
               MOVE WS-SQL-BUILD TO SQLIO-STMT
           END-PERFORM
           MOVE SPACES TO WS-SQL-BUILD
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " FROM so.DocumentVersion WHERE CompanyCode = ? "
                  "AND DocumentType = ? AND DocumentRef = ? AND "
                  "(? = '0' OR VersionNo = ?) ORDER BY VersionNo DESC"
               DELIMITED BY SIZE INTO WS-SQL-BUILD
           MOVE WS-SQL-BUILD TO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE DV-DOCUMENT-TYPE TO SQLIO-PARM-VAL(2)
           MOVE DV-DOCUMENT-REF TO SQLIO-PARM-VAL(3)
           MOVE WS-READ-VERSION TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 31 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 23 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 64 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 100 TO SQLIO-COL-WIDTH(7)
           PERFORM VARYING WS-I FROM 8 BY 1 UNTIL WS-I > 31
               MOVE 200 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "Y" TO WS-ERROR
               MOVE "DOC0004" TO WS-REASON
               MOVE WS-READ-VERSION TO WS-INT-ED
               MOVE SPACES TO WS-MSG
               STRING "Version " FUNCTION TRIM(WS-INT-ED) " of "
                      FUNCTION TRIM(DV-DOCUMENT-TYPE) " "
                      FUNCTION TRIM(DV-DOCUMENT-REF)
                      " is not held"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 8000-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WV-ID)
           MOVE WS-N TO DV-VERSION-ID
           COMPUTE WS-N = FUNCTION NUMVAL(WV-VERSION)
           MOVE WS-N TO DV-VERSION-NO
           MOVE WV-ISSUED-AT TO DV-ISSUED-AT
           MOVE WV-ISSUED-BY TO DV-ISSUED-BY
           MOVE WV-HASH TO DV-CONTENT-HASH
           COMPUTE WS-N = FUNCTION NUMVAL(WV-REPRINTS)
           MOVE WS-N TO DV-REPRINT-COUNT
           MOVE WV-SUMMARY TO DV-CHANGE-SUMMARY(1:100).
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8100-TRIM-SNAPSHOT SECTION.
       8100-START.
           MOVE 0 TO DV-SNAP-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               IF DV-S-TEXT(WS-I) NOT = SPACES
                   MOVE WS-I TO DV-SNAP-CNT
               END-IF
           END-PERFORM.
       8100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           END-IF
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8650-NO-NULLS SECTION.
       8650-START.
           PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-K)
           END-PERFORM.
       8650-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8700-AUDIT SECTION.
       8700-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE WS-SITE TO CA-SITE OF WS-AUDIT-CA
           MOVE WS-USER TO CA-USER OF WS-AUDIT-CA
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-AUDIT-CA
           MOVE "so.DocumentVersion" TO AU-ENTITY-TYPE
           MOVE SPACES TO AU-ENTITY-KEY
           MOVE DV-VERSION-NO TO WS-INT-ED
           STRING FUNCTION TRIM(DV-DOCUMENT-TYPE) "/"
                  FUNCTION TRIM(DV-DOCUMENT-REF) "/v"
                  FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO AU-ENTITY-KEY
           MOVE WS-FUNC TO AU-ACTION
           IF WS-FUNC = "REISSUE"
               MOVE "ReprintCount" TO AU-FIELD-NAME
               MOVE DV-REPRINT-COUNT TO WS-INT-ED
               MOVE FUNCTION TRIM(WS-INT-ED) TO AU-NEW-VALUE
               MOVE SPACES TO AU-REASON
               IF DV-REPRINT-REASON NOT = SPACES
                   MOVE DV-REPRINT-REASON TO AU-REASON
               ELSE
                   STRING "Reissue of a held version; no business "
                          "state changed"
                       DELIMITED BY SIZE INTO AU-REASON
               END-IF
           ELSE
               MOVE "ContentHash" TO AU-FIELD-NAME
               MOVE DV-CONTENT-HASH TO AU-NEW-VALUE
               MOVE DV-CHANGE-SUMMARY(1:200) TO AU-REASON
           END-IF
           MOVE "SODOCVR" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUDIT-CA
           MOVE AU-AUDIT-ID TO DV-AUDIT-ID.
       8700-EXIT.
           EXIT.

       8900-SQL-FAILED SECTION.
       8900-START.
           MOVE "Y" TO WS-ERROR
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
           IF WS-MSG = SPACES
               MOVE "The database refused the request" TO WS-MSG
           END-IF.
       8900-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           IF WS-RC < 8
               MOVE 8 TO WS-RC
           END-IF.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "DOC0007" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "Function " FUNCTION TRIM(WS-FUNC)
                  " is not supported by SODOCVR; use SNAPSHOT, "
                  "REISSUE, LIST or COMPARE"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9400-REPORT SECTION.
       9400-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           IF WS-REASON NOT = SPACES
               MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           END-IF
           IF WS-MSG NOT = SPACES
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           IF CA-MESSAGE OF DFHCOMMAREA NOT = SPACES
               MOVE 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF
      *>   nothing this program does is a business change
           MOVE "N" TO DV-STATE-CHANGED.
       9400-EXIT.
           EXIT.
