      *>================================================================
      *> PROGRAM-ID : ARVATWRT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : VAT CODE LOOKUP AND THE DIGITAL VAT RECORD FOR
      *>              SALES DOCUMENTS.  LOOKUP ANSWERS THE EFFECTIVE
      *>              RATE, LIABILITY, MECHANISM, NINE BOX MAPPING AND
      *>              THE STATUTORY INVOICE WORDING; WRITE PUTS ONE
      *>              ROW PER DOCUMENT AND CODE INTO fin.VatTransaction
      *>              SO THAT THE RETURN AND THE VAT EXCEPTION REPORTS
      *>              SEE EXACTLY WHAT THE INVOICE SAID.
      *> CALLED BY  : ARINVPST, ARCRNOTE, ARDEPOS, BADDEBT, EINVUBL
      *> INTERFACE  : CALL "ARVATWRT" USING WS-VATWRT-CA
      *>              COPYBOOK ARVATWRTCA
      *> CALLS      : SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL VAT ANALYSIS FILE WRITER (MVS)
      *> 2021-01-01 KMP  BREXIT - NORTHERN IRELAND PROTOCOL CODES
      *> 2026-10-05 STERP fin.VatTransaction AND MTD BOX MAPPING
      *>================================================================
      *> REQ: AR-023, AR-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARVATWRT.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 16/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       01  WS-VAT-ROW.
           05  WS-V-LIABILITY          PIC X(12).
           05  WS-V-MECHANISM          PIC X(12).
           05  WS-V-WORDING            PIC X(200).
           05  WS-V-OUT-VAT-BOX        PIC X(4).
           05  WS-V-OUT-NET-BOX        PIC X(4).
           05  WS-V-IN-VAT-BOX         PIC X(4).
           05  WS-V-IN-NET-BOX         PIC X(4).
           05  WS-V-SELF               PIC X(2).
           05  WS-V-EVIDENCE           PIC X(2).
           05  WS-V-RATE               PIC X(20).
           05  WS-V-RECOVER            PIC X(2).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
       01  WS-QTR                      PIC S9(2) VALUE 0.
       01  WS-MM                       PIC 9(2) VALUE 0.
       LINKAGE SECTION.
       COPY ARVATWRTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "LOOKUP"   PERFORM 1000-LOOKUP-CODE
               WHEN "WRITE"    PERFORM 1000-LOOKUP-CODE
                               PERFORM 2000-WRITE-RECORD
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-VAT-ROW
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE "ARVATWRT" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-023" TO AR-REQ-REF
           MOVE "N" TO VW-RATE-FOUND
           MOVE FUNCTION UPPER-CASE(VW-VAT-CODE) TO VW-VAT-CODE
           IF VW-DIRECTION = SPACES
               MOVE "OUTPUT" TO VW-DIRECTION
           END-IF
           IF VW-CURRENCY = SPACES
               MOVE "GBP" TO VW-CURRENCY
           END-IF
           IF VW-FX-RATE = 0
               MOVE 1 TO VW-FX-RATE
           END-IF
           IF VW-TAX-POINT = 0
               MOVE AR-BUSDATE TO VW-TAX-POINT
           END-IF
           IF VW-POSTING-DATE = 0
               MOVE VW-TAX-POINT TO VW-POSTING-DATE
           END-IF
           IF VW-SOURCE-MODULE = SPACES
               MOVE "AR" TO VW-SOURCE-MODULE
           END-IF
           IF VW-PARTY-TYPE = SPACES
               MOVE "CUSTOMER" TO VW-PARTY-TYPE
           END-IF
           PERFORM 0150-PERIOD-KEY.
       0100-EXIT.
           EXIT.

      *>   quarterly stagger 1: the key is the calendar quarter of the
      *>   tax point, matching core.StatutoryObligation
       0150-PERIOD-KEY SECTION.
       0150-START.
           IF VW-PERIOD-KEY NOT = SPACES
               GO TO 0150-EXIT
           END-IF
           MOVE VW-TAX-POINT TO AR-NUM-DATE
           MOVE AR-NUM-DATE TO AR-COMPACT-DATE
           MOVE AR-CMP-M TO WS-MM
           COMPUTE WS-QTR = ((WS-MM - 1) / 3) + 1
           MOVE SPACES TO VW-PERIOD-KEY
           STRING AR-CMP-Y "Q" DELIMITED BY SIZE
                  WS-QTR(2:1) DELIMITED BY SIZE
               INTO VW-PERIOD-KEY.
       0150-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF VW-VAT-CODE = SPACES OR VW-VAT-CODE = LOW-VALUES
               MOVE "VAT0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Every invoice line needs a VAT code"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "WRITE"
              AND (VW-DOC-REF = SPACES OR AR-CO = SPACES)
               MOVE "VAT0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A VAT record needs a company and a document "
                      "reference"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-LOOKUP-CODE SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT v.Liability, v.Mechanism, "
                  "ISNULL(v.InvoiceWording,''), "
                  "CONVERT(varchar(4), ISNULL(v.OutputVatBox,0)), "
                  "CONVERT(varchar(4), ISNULL(v.OutputNetBox,0)), "
                  "CONVERT(varchar(4), ISNULL(v.InputVatBox,0)), "
                  "CONVERT(varchar(4), ISNULL(v.InputNetBox,0)), "
                  "CONVERT(char(1), v.SelfAccounts), "
                  "CONVERT(char(1), v.EvidenceRequired), "
                  "CONVERT(varchar(20), ISNULL((SELECT TOP (1) "
                  "r.RatePct FROM fin.VatRate r "
                  "WHERE r.VatCode = v.VatCode "
                  "AND r.EffectiveFrom <= ? "
                  "ORDER BY r.EffectiveFrom DESC), -1)), "
                  "CONVERT(char(1), v.IsRecoverable) "
                  "FROM fin.VatCode v "
                  "WHERE v.VatCode = ? AND v.IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE VW-TAX-POINT TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE VW-VAT-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 200 TO SQLIO-COL-WIDTH(3)
           MOVE 4 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 4 TO SQLIO-COL-WIDTH(6)
           MOVE 4 TO SQLIO-COL-WIDTH(7)
           MOVE 2 TO SQLIO-COL-WIDTH(8)
           MOVE 2 TO SQLIO-COL-WIDTH(9)
           MOVE 20 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-COL-WIDTH(11)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VAT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "VAT0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "VAT code " FUNCTION TRIM(VW-VAT-CODE)
                      " is not an active code"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 1000-EXIT
           END-IF
           MOVE WS-V-LIABILITY TO VW-LIABILITY
           MOVE WS-V-MECHANISM TO VW-MECHANISM
           MOVE WS-V-WORDING TO VW-WORDING
           MOVE WS-V-SELF(1:1) TO VW-SELF-ACCOUNTS
           MOVE WS-V-EVIDENCE(1:1) TO VW-EVIDENCE-REQ
           COMPUTE AR-PCT1 = FUNCTION NUMVAL(WS-V-RATE)
           IF AR-PCT1 < 0
               MOVE 8 TO AR-RC
               MOVE "VAT0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "No VAT rate is held for code "
                      FUNCTION TRIM(VW-VAT-CODE)
                      " on that tax point"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 1000-EXIT
           END-IF
           MOVE AR-PCT1 TO VW-RATE-PCT
           MOVE "Y" TO VW-RATE-FOUND
           IF VW-DIRECTION = "OUTPUT"
               COMPUTE VW-VAT-BOX = FUNCTION NUMVAL(WS-V-OUT-VAT-BOX)
               COMPUTE VW-NET-BOX = FUNCTION NUMVAL(WS-V-OUT-NET-BOX)
           ELSE
               COMPUTE VW-VAT-BOX = FUNCTION NUMVAL(WS-V-IN-VAT-BOX)
               COMPUTE VW-NET-BOX = FUNCTION NUMVAL(WS-V-IN-NET-BOX)
           END-IF.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-WRITE-RECORD SECTION.
       2000-START.
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           IF VW-NET-GBP = 0 AND VW-VAT-GBP = 0
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatTransaction (CompanyCode, "
                  "SourceModule, DocumentType, DocumentRef, LineNum, "
                  "PartyType, PartyCode, PartyVatRegNo, TaxPointDate, "
                  "PostingDate, VatPeriodKey, JournalId, VatCode, "
                  "RatePct, Mechanism, Direction, CurrencyCode, "
                  "FxRate, NetAmount, VatAmount, NetAmountGBP, "
                  "VatAmountGBP, RecoverableVatGBP, VatBox, NetBox, "
                  "PlaceOfSupply, EvidenceRef, IsLateEntry, "
                  "AdjustmentType) VALUES (?, ?, ?, ?, NULLIF(?,0), "
                  "?, NULLIF(?,''), NULLIF(?,''), ?, ?, ?, "
                  "NULLIF(?,0), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,0), NULLIF(?,0), NULLIF(?,''), "
                  "NULLIF(?,''), ?, NULLIF(?,''))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 29 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE VW-SOURCE-MODULE TO SQLIO-PARM-VAL(2)
           MOVE VW-DOC-TYPE TO SQLIO-PARM-VAL(3)
           MOVE VW-DOC-REF TO SQLIO-PARM-VAL(4)
           MOVE VW-LINE-NUM TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE VW-PARTY-TYPE TO SQLIO-PARM-VAL(6)
           MOVE VW-PARTY-CODE TO SQLIO-PARM-VAL(7)
           MOVE VW-PARTY-VAT-NO TO SQLIO-PARM-VAL(8)
           MOVE VW-TAX-POINT TO SQLIO-PARM-VAL(9)
           MOVE VW-POSTING-DATE TO SQLIO-PARM-VAL(10)
           MOVE VW-PERIOD-KEY TO SQLIO-PARM-VAL(11)
           MOVE VW-JOURNAL-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(12)
           MOVE VW-VAT-CODE TO SQLIO-PARM-VAL(13)
           MOVE VW-RATE-PCT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE VW-MECHANISM TO SQLIO-PARM-VAL(15)
           MOVE VW-DIRECTION TO SQLIO-PARM-VAL(16)
           MOVE VW-CURRENCY TO SQLIO-PARM-VAL(17)
           MOVE VW-FX-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(18)
           MOVE VW-NET TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(19)
           MOVE VW-VAT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(20)
           MOVE VW-NET-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(21)
           MOVE VW-VAT-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(22)
           MOVE VW-RECOVERABLE-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(23)
           MOVE VW-VAT-BOX TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(24)
           MOVE VW-NET-BOX TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(25)
           MOVE VW-PLACE-OF-SUPPLY TO SQLIO-PARM-VAL(26)
           MOVE VW-EVIDENCE-REF TO SQLIO-PARM-VAL(27)
           IF VW-LATE-ENTRY = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(28)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(28)
           END-IF
           MOVE VW-ADJUST-TYPE TO SQLIO-PARM-VAL(29)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 29
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE SPACES TO AR-MSG
           STRING "VAT record written for "
                  FUNCTION TRIM(VW-DOC-REF) " code "
                  FUNCTION TRIM(VW-VAT-CODE)
               DELIMITED BY SIZE INTO AR-MSG.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO AR-RC
           MOVE "Y" TO AR-ERROR
           ADD 1 TO AR-MSG-CNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO AR-RC
           MOVE "VAT0008" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARVATWRT does not support function ["
                  FUNCTION TRIM(AR-FUNC) "]"
               DELIMITED BY SIZE INTO AR-MSG.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO AR-RC
           MOVE "SQL0001" TO AR-REASON
           MOVE SQLIO-ERRMSG TO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       9300-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE AR-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE AR-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE AR-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE AR-MSG-CNT TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE AR-REQ-REF TO CA-REQ-REF OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
