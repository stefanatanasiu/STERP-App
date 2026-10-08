      *>================================================================
      *> PROGRAM-ID : APHASH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : CONTENT HASH OF THE TEXT THAT AN APPROVAL IS
      *>              GIVEN FOR.  IT IS A CHANGE DETECTOR, NOT A
      *>              SECURITY DEVICE: IF ANY PART OF AN APPROVED
      *>              PAYMENT OR BANK DESTINATION CHANGES, THE HASH
      *>              CHANGES AND core.usp_RequestApproval TREATS THE
      *>              EARLIER APPROVAL AS NO LONGER APPLICABLE.
      *>              EIGHT INDEPENDENT 32 BIT ACCUMULATORS WITH
      *>              DIFFERENT MULTIPLIERS AND SEEDS, RENDERED AS 64
      *>              HEXADECIMAL CHARACTERS.
      *>              NOT A LINKABLE PROGRAM - TWO PARAMETERS.
      *> CALLED BY  : APBANKVR, APPAYAPR, APPAYPRP
      *> INTERFACE  : CALL "APHASH" USING WS-TEXT WS-HASH
      *>              WS-TEXT  PIC X(300)   WS-HASH  PIC X(64)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-02-27 DJM  ORIGINAL CHECKSUM FOR THE AUTHORISATION LOG
      *> 2026-10-05 STERP WIDENED TO 64 CHARACTERS FOR core.Approval
      *>================================================================
      *> REQ: AP-002, AP-012, COM-003
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APHASH.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 27/02/1995.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TABLES.
           05  WS-HEX-DIGITS           PIC X(16)
                                       VALUE "0123456789abcdef".
           05  WS-SEEDS.
               10  FILLER              PIC 9(9) VALUE 2166136261.
               10  FILLER              PIC 9(9) VALUE 0005381000.
               10  FILLER              PIC 9(9) VALUE 0000000017.
               10  FILLER              PIC 9(9) VALUE 0987654321.
               10  FILLER              PIC 9(9) VALUE 0000065599.
               10  FILLER              PIC 9(9) VALUE 0001000003.
               10  FILLER              PIC 9(9) VALUE 0000000131.
               10  FILLER              PIC 9(9) VALUE 0049979687.
           05  WS-SEED-TAB REDEFINES WS-SEEDS.
               10  WS-SEED             PIC 9(9) OCCURS 8 TIMES.
           05  WS-MULTS.
               10  FILLER              PIC 9(9) VALUE 0016777619.
               10  FILLER              PIC 9(9) VALUE 0000000033.
               10  FILLER              PIC 9(9) VALUE 0000000131.
               10  FILLER              PIC 9(9) VALUE 0000065599.
               10  FILLER              PIC 9(9) VALUE 0000001099.
               10  FILLER              PIC 9(9) VALUE 0000016411.
               10  FILLER              PIC 9(9) VALUE 0000000037.
               10  FILLER              PIC 9(9) VALUE 0000002166.
           05  WS-MULT-TAB REDEFINES WS-MULTS.
               10  WS-MULT             PIC 9(9) OCCURS 8 TIMES.
       01  WS-WORK.
           05  WS-ACC                  PIC 9(18) OCCURS 8 TIMES.
           05  WS-I                    PIC S9(5) VALUE 0.
           05  WS-J                    PIC S9(5) VALUE 0.
           05  WS-K                    PIC S9(5) VALUE 0.
           05  WS-LEN                  PIC S9(5) VALUE 0.
           05  WS-BYTE                 PIC 9(5) VALUE 0.
           05  WS-TEMP                 PIC 9(18) VALUE 0.
           05  WS-NIBBLE               PIC 9(3) VALUE 0.
           05  WS-DIV                  PIC 9(18) VALUE 0.
           05  WS-MODULUS              PIC 9(18) VALUE 4294967296.
       LINKAGE SECTION.
       01  LK-TEXT                     PIC X(300).
       01  LK-HASH                     PIC X(64).
       PROCEDURE DIVISION USING LK-TEXT LK-HASH.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           PERFORM 2000-ACCUMULATE THRU 2000-EXIT
           PERFORM 3000-RENDER THRU 3000-EXIT.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE SPACES TO LK-HASH
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-SEED(WS-J) TO WS-ACC(WS-J)
           END-PERFORM
           MOVE 300 TO WS-LEN
           PERFORM UNTIL WS-LEN < 1
                      OR LK-TEXT(WS-LEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-LEN
           END-PERFORM
           IF WS-LEN < 1
               MOVE 1 TO WS-LEN
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> EACH ACCUMULATOR IS AN INDEPENDENT 32 BIT ROLLING PRODUCT.
      *> THE POSITION IS MIXED IN SO THAT TRANSPOSED FIELDS DO NOT
      *> PRODUCE THE SAME ANSWER.
      *>----------------------------------------------------------------
       2000-ACCUMULATE SECTION.
       2000-START.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE FUNCTION ORD(LK-TEXT(WS-I:1)) TO WS-BYTE
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
                   COMPUTE WS-TEMP =
                       (WS-ACC(WS-J) * WS-MULT(WS-J))
                       + (WS-BYTE * (WS-I + WS-J))
                   DIVIDE WS-TEMP BY WS-MODULUS GIVING WS-DIV
                       REMAINDER WS-ACC(WS-J)
               END-PERFORM
           END-PERFORM.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-RENDER SECTION.
       3000-START.
           MOVE 1 TO WS-K
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-ACC(WS-J) TO WS-TEMP
               PERFORM VARYING WS-I FROM 8 BY -1 UNTIL WS-I < 1
                   DIVIDE WS-TEMP BY 16 GIVING WS-DIV
                       REMAINDER WS-NIBBLE
                   ADD 1 TO WS-NIBBLE
                   COMPUTE WS-K = ((WS-J - 1) * 8) + WS-I
                   MOVE WS-HEX-DIGITS(WS-NIBBLE:1) TO LK-HASH(WS-K:1)
                   MOVE WS-DIV TO WS-TEMP
               END-PERFORM
           END-PERFORM.
       3000-EXIT.
           EXIT.
