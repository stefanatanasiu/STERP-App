      *>================================================================
      *> PROGRAM-ID : DDMAND
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : BACS DIRECT DEBIT MANDATES AND COLLECTION
      *>              PLANNING UNDER THE DIRECT DEBIT SCHEME.
      *>              LODGEMENT (AUDDIS 0N), ACTIVATION WITH THE
      *>              DIRECT DEBIT GUARANTEE, ADVANCE NOTICE OF
      *>              AMOUNT AND DATE, COLLECTION ON A BACS
      *>              PROCESSING DAY AND CANCELLATION (AUDDIS 0C,
      *>              OR AN ADDACS ADVICE FROM THE PAYING BANK).
      *>              NO COLLECTION IS MADE WITHOUT AN ACTIVE
      *>              MANDATE OR BEFORE THE ADVANCE NOTICE HAS
      *>              EXPIRED (AR-025).
      *>              UNPAID (ARUDD) AND INDEMNITY (DDICA) RETURNS
      *>              REINSTATE THE DEBT AND ARE HANDLED BY DISHONOR.
      *> CALLED BY  : ARCTL (.NET), DDCOLBAT, BACSOUT
      *> INTERFACE  : CALL "DDMAND" USING WS-DDMAND-CA
      *>              COPYBOOK DDMANDCA
      *> CALLS      : SQLIO, PARMGET, PERMCHK, NUMGEN, DATECALC,
      *>              AUDITLOG, EXCPRAIS
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL SALES LEDGER PORT (MVS)
      *> 1997-10-02 DJM  CR0888 BACS DIRECT DEBIT SCHEME
      *> 1999-04-19 KMP  CR0955 AUDDIS - PAPERLESS LODGEMENT
      *> 2001-03-05 DJM  CR1201 ADDACS ADVICES APPLIED AUTOMATICALLY
      *> 2026-10-05 STERP ADVANCE NOTICE, GUARANTEE EVIDENCE, D-16
      *> 2026-10-06 STERP AUTHORITY CODES ALIGNED TO THE PERMISSION
      *>                  CATALOGUE (AR.DD.MANDATE.MAINT AND
      *>                  AR.DD.COLLECT.RUN) - THE OLD CODES WERE
      *>                  NOT HELD BY ANY ROLE, SO EVERY FUNCTION
      *>                  WAS HELD AT 06 FOR EVERY USER
      *>================================================================
      *> REQ: AR-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DDMAND.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 02/10/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXC-CA==.
       01  WS-CUS.
           05  WS-CU-NAME              PIC X(120).
           05  WS-CU-STATUS            PIC X(12).
           05  WS-CU-METHOD            PIC X(10).
       01  WS-MAN.
           05  WS-MA-ID                PIC X(24).
           05  WS-MA-REF               PIC X(18).
           05  WS-MA-SUN               PIC X(6).
           05  WS-MA-SORT              PIC X(6).
           05  WS-MA-ACCT              PIC X(8).
           05  WS-MA-STATUS            PIC X(12).
           05  WS-MA-ADVDAYS           PIC X(24).
           05  WS-MA-WRITTEN           PIC X(4).
           05  WS-MA-FIRST             PIC X(10).
           05  WS-MA-LODGED            PIC X(10).
       01  WS-COL.
           05  WS-CO-ID                PIC X(24).
           05  WS-CO-NOTICE            PIC X(10).
           05  WS-CO-STATUS            PIC X(12).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
      *>   the fourteen digits of sort code plus account number and
      *>   the standard modulus weights.  The real scheme holds a
      *>   weighting table per sort code range; the conversion kept
      *>   the standard rows and treats the rest as unweighted.
       01  WS-BANK.
           05  WS-BK-DIGITS            PIC X(14).
           05  FILLER REDEFINES WS-BK-DIGITS.
               10  WS-BK-D             PIC 9 OCCURS 14 TIMES.
       01  WS-W11-TAB.
           05  FILLER                  PIC 99 VALUE 06.
           05  FILLER                  PIC 99 VALUE 03.
           05  FILLER                  PIC 99 VALUE 07.
           05  FILLER                  PIC 99 VALUE 09.
           05  FILLER                  PIC 99 VALUE 10.
           05  FILLER                  PIC 99 VALUE 05.
           05  FILLER                  PIC 99 VALUE 08.
           05  FILLER                  PIC 99 VALUE 04.
           05  FILLER                  PIC 99 VALUE 02.
           05  FILLER                  PIC 99 VALUE 01.
           05  FILLER                  PIC 99 VALUE 06.
           05  FILLER                  PIC 99 VALUE 03.
           05  FILLER                  PIC 99 VALUE 07.
           05  FILLER                  PIC 99 VALUE 09.
       01  FILLER REDEFINES WS-W11-TAB.
           05  WS-W11                  PIC 99 OCCURS 14 TIMES.
       01  WS-W10-TAB.
           05  FILLER                  PIC 99 VALUE 00.
           05  FILLER                  PIC 99 VALUE 00.
           05  FILLER                  PIC 99 VALUE 00.
           05  FILLER                  PIC 99 VALUE 00.
           05  FILLER                  PIC 99 VALUE 00.
           05  FILLER                  PIC 99 VALUE 00.
           05  FILLER                  PIC 99 VALUE 07.
           05  FILLER                  PIC 99 VALUE 01.
           05  FILLER                  PIC 99 VALUE 03.
           05  FILLER                  PIC 99 VALUE 07.
           05  FILLER                  PIC 99 VALUE 01.
           05  FILLER                  PIC 99 VALUE 03.
           05  FILLER                  PIC 99 VALUE 07.
           05  FILLER                  PIC 99 VALUE 01.
       01  FILLER REDEFINES WS-W10-TAB.
           05  WS-W10                  PIC 99 OCCURS 14 TIMES.
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-CUST-FOUND           PIC X VALUE "N".
           05  WS-MAN-FOUND            PIC X VALUE "N".
           05  WS-MOD-OK               PIC X VALUE "N".
           05  WS-TOT11                PIC S9(7) VALUE 0.
           05  WS-TOT10                PIC S9(7) VALUE 0.
           05  WS-REM                  PIC S9(4) VALUE 0.
           05  WS-ADV                  PIC S9(4) VALUE 10.
           05  WS-LEAD                 PIC S9(4) VALUE 3.
           05  WS-SUM                  PIC S9(11)V99 VALUE 0.
           05  WS-PTR                  PIC S9(4) VALUE 1.
       01  WS-ITEMLIST                 PIC X(256) VALUE SPACES.
       LINKAGE SECTION.
       COPY DDMANDCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-VALIDATE THRU 0200-EXIT
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-CUSTOMER THRU 0300-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0400-READ-MANDATE THRU 0400-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "LODGE"    PERFORM 1000-LODGE THRU 1000-EXIT
               WHEN "ACTIVATE" PERFORM 2000-ACTIVATE THRU 2000-EXIT
               WHEN "NOTICE"   PERFORM 3000-NOTICE THRU 3000-EXIT
               WHEN "SCHEDULE" PERFORM 4000-SCHEDULE THRU 4000-EXIT
               WHEN "CANCEL"   PERFORM 5000-CANCEL THRU 5000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-CUS
           INITIALIZE WS-MAN
           INITIALIZE WS-COL
           MOVE "N" TO WS-TRAN-OPEN WS-CUST-FOUND WS-MAN-FOUND
           MOVE "N" TO WS-MOD-OK
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "DDMAND" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-025" TO AR-REQ-REF
           MOVE SPACES TO DM-BLOCK-REASON
           MOVE FUNCTION UPPER-CASE(DM-MANDATE-REF) TO DM-MANDATE-REF
           MOVE FUNCTION UPPER-CASE(DM-SORT-CODE) TO DM-SORT-CODE
           PERFORM 0150-PARAMETERS THRU 0150-EXIT.
       0100-EXIT.
           EXIT.

      *>   D-16 / specification 03 - the baseline advance notice is
      *>   ten working days; a shorter period is only valid when the
      *>   payer has agreed it in writing.  Bacs runs a three day
      *>   cycle: submit, process, debit.
       0150-PARAMETERS SECTION.
       0150-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.DD.ADVANCE.NOTICE.DAYS" TO PM-PARAM-CODE
           MOVE "10" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF CA-RETURN-CODE OF WS-PARM-CA < 8
               MOVE PM-VALUE-NUM TO WS-ADV
           ELSE
               MOVE 10 TO WS-ADV
           END-IF
           IF WS-ADV < 1
               MOVE 10 TO WS-ADV
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.DD.BACS.LEAD.DAYS" TO PM-PARAM-CODE
           MOVE "3" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF CA-RETURN-CODE OF WS-PARM-CA < 8
               MOVE PM-VALUE-NUM TO WS-LEAD
           ELSE
               MOVE 3 TO WS-LEAD
           END-IF
           IF WS-LEAD < 2
               MOVE 3 TO WS-LEAD
           END-IF
           MOVE WS-LEAD TO DM-BACS-LEAD-DAYS
           IF DM-SERVICE-USER-NO = SPACES
               INITIALIZE WS-PARM-CA
               MOVE "GET" TO CA-FUNCTION OF WS-PARM-CA
               MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
               MOVE AR-USERID TO CA-USER OF WS-PARM-CA
               MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
               MOVE "AR.DD.SERVICE.USER.NUMBER" TO PM-PARAM-CODE
               MOVE "912345" TO PM-DEFAULT-TEXT
               MOVE "Y" TO PM-USE-DEFAULT
               CALL "PARMGET" USING WS-PARM-CA
               MOVE PM-VALUE-TEXT(1:6) TO DM-SERVICE-USER-NO
           END-IF.
       0150-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "DDM0001" TO AR-REASON
               MOVE "A company code is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF DM-CUSTOMER = SPACES AND DM-MANDATE-REF = SPACES
              AND DM-MANDATE-ID = 0
               MOVE "DDM0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A customer account or a mandate reference "
                      "is required"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "LODGE"
               PERFORM 0250-LODGE-EDITS THRU 0250-EXIT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "CANCEL" AND DM-CANCEL-REASON = SPACES
               MOVE "DDM0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A mandate is cancelled with a reason so "
                      "that the payer's bank advice is traceable"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "SCHEDULE" AND DM-REQUEST-DATE = 0
               MOVE AR-BUSDATE TO DM-REQUEST-DATE
           END-IF.
       0200-EXIT.
           EXIT.

       0250-LODGE-EDITS SECTION.
       0250-START.
           IF DM-PAYER-NAME = SPACES
               MOVE "DDM0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The name of the account holder is a "
                      "mandatory part of the instruction"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0250-EXIT
           END-IF
           IF DM-SIGNED-DATE = 0 OR DM-SIGNED-DATE > AR-BUSDATE
               MOVE "DDM0005" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A mandate needs the date the payer signed "
                      "it, and it cannot be in the future"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0250-EXIT
           END-IF
           IF DM-SERVICE-USER-NO = SPACES
              OR DM-SERVICE-USER-NO NOT NUMERIC
               MOVE "DDM0006" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The service user number must be the six "
                      "digits allocated by the sponsoring bank"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0250-EXIT
           END-IF
           PERFORM 0500-BANK-DETAILS THRU 0500-EXIT.
       0250-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-READ-CUSTOMER SECTION.
       0300-START.
           IF DM-CUSTOMER = SPACES
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.Status, c.CollectionMethod "
                  "FROM ar.Customer c WHERE c.CompanyCode = ? "
                  "AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DM-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUS
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "DDM0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(DM-CUSTOMER)
                      " is not on the sales ledger"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 0300-EXIT
           END-IF
           MOVE "Y" TO WS-CUST-FOUND
           IF DM-PAYER-NAME = SPACES
               MOVE WS-CU-NAME TO DM-PAYER-NAME
           END-IF.
       0300-EXIT.
           EXIT.

       0400-READ-MANDATE SECTION.
       0400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), m.MandateId), "
                  "m.MandateReference, m.ServiceUserNumber, "
                  "m.SortCode, m.AccountNumber, m.Status, "
                  "CONVERT(varchar(24), m.AdvanceNoticeDays), "
                  "CONVERT(varchar(4), "
                  "m.AdvanceNoticeAgreedInWriting), "
                  "ISNULL(CONVERT(varchar(10), "
                  "m.FirstCollectionDate, 112),''), "
                  "ISNULL(CONVERT(varchar(10), m.LodgedDate, "
                  "112),'') FROM ar.DirectDebitMandate m "
                  "WHERE m.CompanyCode = ? "
                  "AND (m.MandateId = NULLIF(?,0) "
                  "OR m.MandateReference = NULLIF(?,'') "
                  "OR (m.CustomerCode = NULLIF(?,'') "
                  "AND m.Status IN ('PENDING','LODGED','ACTIVE'))) "
                  "ORDER BY CASE m.Status WHEN 'ACTIVE' THEN 1 "
                  "WHEN 'LODGED' THEN 2 ELSE 3 END, m.MandateId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DM-MANDATE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE DM-MANDATE-REF TO SQLIO-PARM-VAL(3)
           MOVE DM-CUSTOMER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 18 TO SQLIO-COL-WIDTH(2)
           MOVE 6 TO SQLIO-COL-WIDTH(3)
           MOVE 6 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-COL-WIDTH(8)
           MOVE 10 TO SQLIO-COL-WIDTH(9)
           MOVE 10 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAN
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 0400-EXIT
           END-IF
           MOVE "Y" TO WS-MAN-FOUND
           COMPUTE DM-MANDATE-ID = FUNCTION NUMVAL(WS-MA-ID)
           MOVE WS-MA-REF TO DM-MANDATE-REF
           MOVE WS-MA-STATUS TO DM-STATUS
           IF DM-SORT-CODE = SPACES
               MOVE WS-MA-SORT TO DM-SORT-CODE
               MOVE WS-MA-ACCT TO DM-ACCOUNT-NO
           END-IF
           IF DM-SERVICE-USER-NO = SPACES
               MOVE WS-MA-SUN TO DM-SERVICE-USER-NO
           END-IF
           COMPUTE DM-ADVANCE-DAYS = FUNCTION NUMVAL(WS-MA-ADVDAYS)
           IF WS-MA-WRITTEN(1:1) = "1"
               MOVE "Y" TO DM-ADVANCE-WRITTEN
           END-IF
           IF WS-MA-FIRST = SPACES
               MOVE "Y" TO DM-FIRST-COLLECT
           ELSE
               MOVE "N" TO DM-FIRST-COLLECT
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   sort code and account number edits, then the modulus check
      *>   with the standard weights.  A failure is a plain refusal
      *>   with an explanation - never a silent acceptance that only
      *>   fails days later in the Bacs return file.
      *>   REQ: AR-025
       0500-BANK-DETAILS SECTION.
       0500-START.
           MOVE "N" TO WS-MOD-OK
           IF DM-SORT-CODE = SPACES OR DM-SORT-CODE NOT NUMERIC
               MOVE "DDM0011" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The sorting code must be the six digits of "
                      "the payer's branch"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0500-EXIT
           END-IF
           IF DM-ACCOUNT-NO = SPACES OR DM-ACCOUNT-NO NOT NUMERIC
               MOVE "DDM0012" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The account number must be eight digits; "
                      "a shorter number is padded by the bank"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0500-EXIT
           END-IF
           MOVE SPACES TO WS-BK-DIGITS
           STRING DM-SORT-CODE DM-ACCOUNT-NO
               DELIMITED BY SIZE INTO WS-BK-DIGITS
           PERFORM 0550-MODULUS THRU 0550-EXIT
           IF WS-MOD-OK NOT = "Y"
               MOVE "DDM0013" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Sorting code " FUNCTION TRIM(DM-SORT-CODE)
                      " and account " FUNCTION TRIM(DM-ACCOUNT-NO)
                      " fail the modulus check; check the paper "
                      "instruction before lodging it"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0500-EXIT.
           EXIT.

      *>   modulus 11 with the standard weights, then modulus 10 for
      *>   the ranges that use it.  Either pass is accepted, which is
      *>   how the 1997 conversion handled the weighting table it did
      *>   not have room to hold.
       0550-MODULUS SECTION.
       0550-START.
           MOVE 0 TO WS-TOT11 WS-TOT10
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 14
               COMPUTE WS-TOT11 =
                   WS-TOT11 + (WS-BK-D(AR-I) * WS-W11(AR-I))
               COMPUTE WS-TOT10 =
                   WS-TOT10 + (WS-BK-D(AR-I) * WS-W10(AR-I))
           END-PERFORM
           COMPUTE WS-REM = FUNCTION MOD(WS-TOT11, 11)
           IF WS-REM = 0
               MOVE "Y" TO WS-MOD-OK
               GO TO 0550-EXIT
           END-IF
           COMPUTE WS-REM = FUNCTION MOD(WS-TOT10, 10)
           IF WS-REM = 0
               MOVE "Y" TO WS-MOD-OK
           END-IF.
       0550-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   LODGE - the signed instruction is recorded and offered to
      *>   the paying bank as an AUDDIS 0N new instruction.  It is
      *>   not usable until it has been lodged and activated.
      *>   REQ: AR-025
       1000-LODGE SECTION.
       1000-START.
           MOVE "AR.DD.MANDATE.MAINT" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 1000-EXIT
           END-IF
           IF WS-MAN-FOUND = "Y" AND WS-MA-STATUS = "ACTIVE"
               MOVE 4 TO AR-RC
               MOVE "DDM0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Mandate " FUNCTION TRIM(WS-MA-REF)
                      " is already active for this payer"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-MANDATE-REF THRU 1100-EXIT
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           IF DM-ADVANCE-DAYS = 0
               MOVE WS-ADV TO DM-ADVANCE-DAYS
           END-IF
           IF DM-ADVANCE-DAYS < WS-ADV AND DM-ADVANCE-WRITTEN NOT = "Y"
               MOVE WS-ADV TO DM-ADVANCE-DAYS
               MOVE 4 TO AR-RC
               MOVE "DDM0021" TO AR-REASON
               MOVE SPACES TO AR-MSG
               MOVE WS-ADV TO AR-INT-ED
               STRING "A notice period shorter than "
                      FUNCTION TRIM(AR-INT-ED)
                      " working days needs the payer's written "
                      "agreement; the baseline has been applied"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.DirectDebitMandate (CompanyCode, "
                  "CustomerCode, MandateReference, "
                  "ServiceUserNumber, PayerName, SortCode, "
                  "AccountNumber, RollNumber, SignedDate, "
                  "AdvanceNoticeDays, "
                  "AdvanceNoticeAgreedInWriting, Status, "
                  "IsSensitive, CreatedBy) "
                  "OUTPUT CONVERT(varchar(24), INSERTED.MandateId) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, NULLIF(?,''), ?, "
                  "?, ?, 'PENDING', 1, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DM-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE DM-MANDATE-REF TO SQLIO-PARM-VAL(3)
           MOVE DM-SERVICE-USER-NO TO SQLIO-PARM-VAL(4)
           MOVE DM-PAYER-NAME TO SQLIO-PARM-VAL(5)
           MOVE DM-SORT-CODE TO SQLIO-PARM-VAL(6)
           MOVE DM-ACCOUNT-NO TO SQLIO-PARM-VAL(7)
           MOVE DM-ROLL-NUMBER TO SQLIO-PARM-VAL(8)
           MOVE DM-SIGNED-DATE TO SQLIO-PARM-VAL(9)
           MOVE DM-ADVANCE-DAYS TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(10)
           IF DM-ADVANCE-WRITTEN = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(11)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(11)
           END-IF
           MOVE AR-USERID TO SQLIO-PARM-VAL(12)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 12
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           COMPUTE DM-MANDATE-ID = FUNCTION NUMVAL(WS-NEW-ID)
           MOVE "PENDING" TO DM-STATUS
           MOVE "0N" TO DM-TXN-CODE
           MOVE "Y" TO DM-FIRST-COLLECT
           MOVE "LODGE" TO AR-TEXT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           IF AR-RC-OK
               MOVE SPACES TO AR-MSG
               STRING "Mandate " FUNCTION TRIM(DM-MANDATE-REF)
                      " recorded for lodgement as an AUDDIS 0N "
                      "new instruction"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       1000-EXIT.
           EXIT.

      *>   the reference goes on the payer's bank statement, so it
      *>   is upper case, eighteen characters and never reused
       1100-MANDATE-REF SECTION.
       1100-START.
           IF DM-MANDATE-REF NOT = SPACES
               GO TO 1100-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE "DDMAN" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE "DDM0022" TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1100-EXIT
           END-IF
           MOVE FUNCTION UPPER-CASE(NG-DOCUMENT-NUMBER(1:18))
               TO DM-MANDATE-REF
           INSPECT DM-MANDATE-REF REPLACING ALL "/" BY "-"
           INSPECT DM-MANDATE-REF REPLACING ALL "." BY "-".
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   ACTIVATE - the instruction has been lodged with the paying
      *>   bank and the Direct Debit Guarantee has been supplied, so
      *>   the mandate may now be collected against.
      *>   REQ: AR-025
       2000-ACTIVATE SECTION.
       2000-START.
           MOVE "AR.DD.MANDATE.MAINT" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 2000-EXIT
           END-IF
           PERFORM 8600-NEED-MANDATE THRU 8600-EXIT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           IF WS-MA-STATUS = "CANCELLED" OR WS-MA-STATUS = "EXPIRED"
               MOVE 8 TO AR-RC
               MOVE "DDM0030" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A " FUNCTION TRIM(WS-MA-STATUS)
                      " mandate cannot be activated; the payer "
                      "must sign a new instruction"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2000-EXIT
           END-IF
           IF DM-GUARANTEE-SENT NOT = "Y"
               MOVE 6 TO AR-RC
               MOVE "DDM0031" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The Direct Debit Guarantee must be supplied "
                      "to the payer before the mandate is activated"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2000-EXIT
           END-IF
           IF DM-LODGED-DATE = 0
               MOVE AR-BUSDATE TO DM-LODGED-DATE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.DirectDebitMandate SET "
                  "Status = 'ACTIVE', LodgedDate = ?, "
                  "GuaranteeIssuedAt = SYSDATETIME(), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE MandateId = ? AND CompanyCode = ? "
                  "AND Status IN ('PENDING','LODGED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE DM-LODGED-DATE TO SQLIO-PARM-VAL(1)
           MOVE AR-USERID TO SQLIO-PARM-VAL(2)
           MOVE DM-MANDATE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           MOVE "ACTIVE" TO DM-STATUS
           MOVE "ACTIVATE" TO AR-TEXT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO AR-MSG
           STRING "Mandate " FUNCTION TRIM(DM-MANDATE-REF)
                  " is active and may be collected against"
               DELIMITED BY SIZE INTO AR-MSG.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   NOTICE - advance notice of the amount and the date.  The
      *>   earliest collection is the notice date plus the agreed
      *>   notice period in working days.
      *>   REQ: AR-025
       3000-NOTICE SECTION.
       3000-START.
           PERFORM 8600-NEED-MANDATE THRU 8600-EXIT
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           IF WS-MA-STATUS NOT = "ACTIVE"
               MOVE 6 TO AR-RC
               MOVE "DDM0040" TO AR-REASON
               MOVE SPACES TO DM-BLOCK-REASON
               STRING "Mandate " FUNCTION TRIM(DM-MANDATE-REF)
                      " is " FUNCTION TRIM(WS-MA-STATUS)
                      ", so no advance notice may be given"
                   DELIMITED BY SIZE INTO DM-BLOCK-REASON
               MOVE DM-BLOCK-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 3000-EXIT
           END-IF
           IF DM-NOTICE-SENT = 0
               MOVE AR-BUSDATE TO DM-NOTICE-SENT
           END-IF
           IF DM-AMOUNT-GBP = 0
               PERFORM 4100-SUM-ITEMS THRU 4100-EXIT
           END-IF
           IF DM-AMOUNT-GBP <= 0
               MOVE 8 TO AR-RC
               MOVE "DDM0041" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "An advance notice states the amount and the "
                      "date; the amount here is nil"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-EARLIEST THRU 3100-EXIT
           IF DM-COLLECT-REF NOT = SPACES OR DM-COLLECT-ID NOT = 0
               PERFORM 3200-STAMP-NOTICE THRU 3200-EXIT
               IF AR-RC-BAD
                   GO TO 3000-EXIT
               END-IF
           END-IF
           MOVE "NOTICE" TO AR-TEXT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO AR-MSG
           MOVE DM-AMOUNT-GBP TO AR-MONEY-UK
           MOVE DM-EARLIEST-DATE TO AR-DATE-ED
           STRING "Advance notice of GBP "
                  FUNCTION TRIM(AR-MONEY-UK)
                  " given; the earliest collection date is "
                  AR-DATE-ED(7:2) "/" AR-DATE-ED(5:2) "/"
                  AR-DATE-ED(1:4)
               DELIMITED BY SIZE INTO AR-MSG.
       3000-EXIT.
           EXIT.

       3100-EARLIEST SECTION.
       3100-START.
           IF DM-ADVANCE-DAYS = 0
               MOVE WS-ADV TO DM-ADVANCE-DAYS
           END-IF
           INITIALIZE WS-DATE-CA
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE DM-NOTICE-SENT TO DC-DATE-1
           MOVE DM-ADVANCE-DAYS TO DC-NUMBER
           MOVE "BACS" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               COMPUTE AR-DAYS = DM-ADVANCE-DAYS
               MOVE DM-NOTICE-SENT TO DM-EARLIEST-DATE
           ELSE
               MOVE DC-RESULT-DATE TO DM-EARLIEST-DATE
           END-IF.
       3100-EXIT.
           EXIT.

       3200-STAMP-NOTICE SECTION.
       3200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.DirectDebitCollection SET "
                  "AdvanceNoticeSentAt = ? "
                  "WHERE CompanyCode = ? "
                  "AND (CollectionId = NULLIF(?,0) "
                  "OR CollectionRef = NULLIF(?,'')) "
                  "AND Status = 'PLANNED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE DM-NOTICE-SENT TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE DM-COLLECT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE DM-COLLECT-REF TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   SCHEDULE - plan the collection.  Refused without an active
      *>   mandate, and refused before the advance notice has run
      *>   its course (AR-025).  The three day Bacs cycle is submit,
      *>   process, debit; every leg falls on a Bacs processing day.
      *>   REQ: AR-025
       4000-SCHEDULE SECTION.
       4000-START.
           MOVE "AR.DD.COLLECT.RUN" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 4000-EXIT
           END-IF
           PERFORM 8600-NEED-MANDATE THRU 8600-EXIT
           IF AR-RC-BAD
               GO TO 4000-EXIT
           END-IF
           IF WS-MA-STATUS NOT = "ACTIVE"
               MOVE 6 TO AR-RC
               MOVE "DDM0050" TO AR-REASON
               MOVE SPACES TO DM-BLOCK-REASON
               STRING "No collection may be made: mandate "
                      FUNCTION TRIM(DM-MANDATE-REF) " is "
                      FUNCTION TRIM(WS-MA-STATUS)
                      ", not active"
                   DELIMITED BY SIZE INTO DM-BLOCK-REASON
               MOVE DM-BLOCK-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4000-EXIT
           END-IF
           PERFORM 4050-NOTICE-CHECK THRU 4050-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 4000-EXIT
           END-IF
           IF DM-AMOUNT-GBP = 0
               PERFORM 4100-SUM-ITEMS THRU 4100-EXIT
           END-IF
           IF DM-AMOUNT-GBP <= 0
               MOVE 8 TO AR-RC
               MOVE "DDM0051" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A Direct Debit collection of nil or less "
                      "cannot be submitted"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-BACS-DATES THRU 4200-EXIT
           IF AR-RC-BAD
               GO TO 4000-EXIT
           END-IF
           PERFORM 4300-TXN-CODE THRU 4300-EXIT
           PERFORM 4350-COLLECT-REF THRU 4350-EXIT
           IF AR-RC-BAD
               GO TO 4000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 4400-WRITE-COLLECTION THRU 4400-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 4000-EXIT
           END-IF
           IF DM-FIRST-COLLECT = "Y"
               PERFORM 4500-STAMP-FIRST THRU 4500-EXIT
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 4000-EXIT
               END-IF
           END-IF
           PERFORM 9400-COMMIT
           MOVE "SCHEDULE" TO AR-TEXT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO AR-MSG
           MOVE DM-AMOUNT-GBP TO AR-MONEY-UK
           MOVE DM-DUE-DATE TO AR-DATE-ED
           STRING "Collection " FUNCTION TRIM(DM-COLLECT-REF)
                  " of GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " planned for " AR-DATE-ED(7:2) "/"
                  AR-DATE-ED(5:2) "/" AR-DATE-ED(1:4)
                  " on transaction code "
                  FUNCTION TRIM(DM-TXN-CODE)
               DELIMITED BY SIZE INTO AR-MSG.
       4000-EXIT.
           EXIT.

      *>   the advance notice must have been given and must have run
      *>   its full period before the money is taken
       4050-NOTICE-CHECK SECTION.
       4050-START.
           IF DM-NOTICE-SENT = 0
               MOVE 6 TO AR-RC
               MOVE "DDM0052" TO AR-REASON
               MOVE SPACES TO DM-BLOCK-REASON
               STRING "No advance notice of the amount and date "
                      "has been given to the payer"
                   DELIMITED BY SIZE INTO DM-BLOCK-REASON
               MOVE DM-BLOCK-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4050-EXIT
           END-IF
           PERFORM 3100-EARLIEST THRU 3100-EXIT
           IF DM-REQUEST-DATE < DM-EARLIEST-DATE
               MOVE 6 TO AR-RC
               MOVE "DDM0053" TO AR-REASON
               MOVE DM-EARLIEST-DATE TO AR-DATE-ED
               MOVE SPACES TO DM-BLOCK-REASON
               STRING "The advance notice has not expired; the "
                      "earliest collection date is "
                      AR-DATE-ED(7:2) "/" AR-DATE-ED(5:2) "/"
                      AR-DATE-ED(1:4)
                   DELIMITED BY SIZE INTO DM-BLOCK-REASON
               MOVE DM-BLOCK-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       4050-EXIT.
           EXIT.

      *>   the collection may cover up to twenty open items; the
      *>   list is kept on the collection row for the remittance
       4100-SUM-ITEMS SECTION.
       4100-START.
           MOVE 0 TO WS-SUM
           MOVE SPACES TO WS-ITEMLIST
           MOVE 1 TO WS-PTR
           IF DM-ITEM-CNT > 20
               MOVE 20 TO DM-ITEM-CNT
           END-IF
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > DM-ITEM-CNT
               ADD DM-I-AMOUNT(AR-I) TO WS-SUM
               MOVE DM-I-ITEM-ID(AR-I) TO AR-INT-ED
               IF WS-PTR < 240
                   IF WS-PTR > 1
                       MOVE "," TO WS-ITEMLIST(WS-PTR:1)
                       ADD 1 TO WS-PTR
                   END-IF
                   MOVE FUNCTION TRIM(AR-INT-ED)
                       TO WS-ITEMLIST(WS-PTR:12)
                   COMPUTE WS-PTR = WS-PTR +
                       FUNCTION LENGTH(FUNCTION TRIM(AR-INT-ED))
               END-IF
           END-PERFORM
           MOVE WS-SUM TO DM-AMOUNT-GBP.
       4100-EXIT.
           EXIT.

      *>   submission, processing and debit all fall on Bacs working
      *>   days.  The request date is moved forward if it is not one.
       4200-BACS-DATES SECTION.
       4200-START.
           INITIALIZE WS-DATE-CA
           MOVE "ADJUST" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE DM-REQUEST-DATE TO DC-DATE-1
           MOVE "NEXT" TO DC-DIRECTION
           MOVE "BACS" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE DM-REQUEST-DATE TO DM-PROCESS-DATE
           ELSE
               MOVE DC-RESULT-DATE TO DM-PROCESS-DATE
           END-IF
           INITIALIZE WS-DATE-CA
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE DM-PROCESS-DATE TO DC-DATE-1
           MOVE -1 TO DC-NUMBER
           MOVE "BACS" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE DM-PROCESS-DATE TO DM-SUBMIT-DATE
           ELSE
               MOVE DC-RESULT-DATE TO DM-SUBMIT-DATE
           END-IF
           INITIALIZE WS-DATE-CA
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE DM-PROCESS-DATE TO DC-DATE-1
           COMPUTE DC-NUMBER = WS-LEAD - 2
           MOVE "BACS" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE DM-PROCESS-DATE TO DM-DUE-DATE
           ELSE
               MOVE DC-RESULT-DATE TO DM-DUE-DATE
           END-IF
           IF DM-SUBMIT-DATE < AR-BUSDATE
               MOVE 4 TO AR-RC
               MOVE "DDM0054" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The submission day for that collection has "
                      "already passed; choose a later date"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       4200-EXIT.
           EXIT.

      *>   Bacs transaction codes: 01 first collection on a new
      *>   instruction, 17 regular, 18 re-presented, 19 final
       4300-TXN-CODE SECTION.
       4300-START.
           IF DM-TXN-CODE = "18" OR DM-TXN-CODE = "19"
               GO TO 4300-EXIT
           END-IF
           IF DM-FIRST-COLLECT = "Y"
               MOVE "01" TO DM-TXN-CODE
           ELSE
               MOVE "17" TO DM-TXN-CODE
           END-IF.
       4300-EXIT.
           EXIT.

       4350-COLLECT-REF SECTION.
       4350-START.
           IF DM-COLLECT-REF NOT = SPACES
               GO TO 4350-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE "DDCOL" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE "DDM0055" TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4350-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO DM-COLLECT-REF.
       4350-EXIT.
           EXIT.

       4400-WRITE-COLLECTION SECTION.
       4400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.DirectDebitCollection (MandateId, "
                  "CompanyCode, CustomerCode, CollectionRef, "
                  "AdvanceNoticeSentAt, SubmissionDate, "
                  "ProcessingDate, DueDate, AmountGBP, "
                  "TransactionCode, Status, OpenItemIds, "
                  "CorrelationId, CreatedBy) "
                  "OUTPUT CONVERT(varchar(24), "
                  "INSERTED.CollectionId) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "'PLANNED', NULLIF(?,''), NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE DM-MANDATE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE DM-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE DM-COLLECT-REF TO SQLIO-PARM-VAL(4)
           MOVE DM-NOTICE-SENT TO SQLIO-PARM-VAL(5)
           MOVE DM-SUBMIT-DATE TO SQLIO-PARM-VAL(6)
           MOVE DM-PROCESS-DATE TO SQLIO-PARM-VAL(7)
           MOVE DM-DUE-DATE TO SQLIO-PARM-VAL(8)
           MOVE DM-AMOUNT-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(9)
           MOVE DM-TXN-CODE TO SQLIO-PARM-VAL(10)
           MOVE WS-ITEMLIST TO SQLIO-PARM-VAL(11)
           MOVE AR-CORREL TO SQLIO-PARM-VAL(12)
           MOVE AR-USERID TO SQLIO-PARM-VAL(13)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 13
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4400-EXIT
           END-IF
           COMPUTE DM-COLLECT-ID = FUNCTION NUMVAL(WS-NEW-ID).
       4400-EXIT.
           EXIT.

       4500-STAMP-FIRST SECTION.
       4500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.DirectDebitMandate SET "
                  "FirstCollectionDate = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE MandateId = ? "
                  "AND CompanyCode = ? "
                  "AND FirstCollectionDate IS NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE DM-DUE-DATE TO SQLIO-PARM-VAL(1)
           MOVE AR-USERID TO SQLIO-PARM-VAL(2)
           MOVE DM-MANDATE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       4500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CANCEL - the payer, the paying bank (an ADDACS advice) or
      *>   the business may cancel.  The instruction is withdrawn
      *>   with an AUDDIS 0C and every planned collection is pulled.
      *>   REQ: AR-025
       5000-CANCEL SECTION.
       5000-START.
           MOVE "AR.DD.MANDATE.MAINT" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 5000-EXIT
           END-IF
           PERFORM 8600-NEED-MANDATE THRU 8600-EXIT
           IF AR-RC-BAD
               GO TO 5000-EXIT
           END-IF
           IF WS-MA-STATUS = "CANCELLED"
               MOVE 4 TO AR-RC
               MOVE "DDM0060" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Mandate " FUNCTION TRIM(DM-MANDATE-REF)
                      " was already cancelled"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 5000-EXIT
           END-IF
           IF DM-CANCEL-DATE = 0
               MOVE AR-BUSDATE TO DM-CANCEL-DATE
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.DirectDebitMandate SET "
                  "Status = 'CANCELLED', CancelledDate = ?, "
                  "CancelReason = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE MandateId = ? "
                  "AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE DM-CANCEL-DATE TO SQLIO-PARM-VAL(1)
           MOVE DM-CANCEL-REASON TO SQLIO-PARM-VAL(2)
           MOVE AR-USERID TO SQLIO-PARM-VAL(3)
           MOVE DM-MANDATE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE AR-CO TO SQLIO-PARM-VAL(5)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 5000-EXIT
           END-IF
           PERFORM 5100-PULL-PLANNED THRU 5100-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 5000-EXIT
           END-IF
           PERFORM 9400-COMMIT
           MOVE "CANCELLED" TO DM-STATUS
           MOVE "0C" TO DM-TXN-CODE
           MOVE "CANCEL" TO AR-TEXT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           PERFORM 5200-EXCEPTION THRU 5200-EXIT
           MOVE SPACES TO AR-MSG
           STRING "Mandate " FUNCTION TRIM(DM-MANDATE-REF)
                  " cancelled as an AUDDIS 0C; any planned "
                  "collection has been withdrawn"
               DELIMITED BY SIZE INTO AR-MSG.
       5000-EXIT.
           EXIT.

       5100-PULL-PLANNED SECTION.
       5100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.DirectDebitCollection SET "
                  "Status = 'CANCELLED' WHERE MandateId = ? "
                  "AND CompanyCode = ? AND Status = 'PLANNED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE DM-MANDATE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 8500-EXEC THRU 8500-EXIT
           MOVE SQLIO-ROWCOUNT TO AR-ROWS.
       5100-EXIT.
           EXIT.

      *>   the customer can no longer be collected by Direct Debit,
      *>   so credit control is told to agree another method
       5200-EXCEPTION SECTION.
       5200-START.
           INITIALIZE WS-EXC-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXC-CA
           MOVE AR-CO TO CA-COMPANY OF WS-EXC-CA
           MOVE AR-SITE TO CA-SITE OF WS-EXC-CA
           MOVE AR-USERID TO CA-USER OF WS-EXC-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-EXC-CA
           MOVE "AR" TO EX-MODULE
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "AR.DD.MANDATE.CANCELLED" TO EX-CATEGORY
           MOVE SPACES TO EX-MESSAGE-TEXT
           STRING "Direct Debit mandate "
                  FUNCTION TRIM(DM-MANDATE-REF) " for "
                  FUNCTION TRIM(DM-CUSTOMER) " was cancelled: "
                  FUNCTION TRIM(DM-CANCEL-REASON)
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           MOVE "DDMANDATE" TO EX-DOCUMENT-TYPE
           MOVE DM-MANDATE-REF TO EX-DOCUMENT-REF
           MOVE "CREDITCONTROL" TO EX-OWNER-ROLE
           MOVE +40 TO EX-PRIORITY
           CALL "EXCPRAIS" USING WS-EXC-CA.
       5200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-PERMISSION SECTION.
       8000-START.
           MOVE PK-PERMISSION TO AR-TEXT2
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE AR-TEXT2 TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "DDM0070" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       8000-EXIT.
           EXIT.

       8200-AUDIT SECTION.
       8200-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE AR-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE AR-USERID TO CA-USER OF WS-AUDIT-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE "ar.DirectDebitMandate" TO AU-ENTITY-TYPE
           MOVE DM-MANDATE-REF TO AU-ENTITY-KEY
           MOVE AR-TEXT(1:20) TO AU-ACTION
           MOVE "Status" TO AU-FIELD-NAME
           MOVE WS-MA-STATUS TO AU-OLD-VALUE
           MOVE DM-STATUS TO AU-NEW-VALUE
           IF DM-CANCEL-REASON NOT = SPACES
               MOVE DM-CANCEL-REASON TO AU-REASON
           END-IF
           MOVE DM-SERVICE-USER-NO TO AU-AUTHORITY-REF
           MOVE "COBOL" TO AU-SOURCE
           MOVE "Y" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUDIT-CA.
       8200-EXIT.
           EXIT.

       8500-EXEC SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       8500-EXIT.
           EXIT.

       8600-NEED-MANDATE SECTION.
       8600-START.
           IF WS-MAN-FOUND = "Y"
               GO TO 8600-EXIT
           END-IF
           MOVE 16 TO AR-RC
           MOVE "DDM0014" TO AR-REASON
           MOVE SPACES TO DM-BLOCK-REASON
           STRING "No Direct Debit mandate was found for "
                  FUNCTION TRIM(DM-CUSTOMER)
                  FUNCTION TRIM(DM-MANDATE-REF)
               DELIMITED BY SIZE INTO DM-BLOCK-REASON
           MOVE DM-BLOCK-REASON TO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       8600-EXIT.
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
           MOVE "DDM0009" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "DDMAND does not support function ["
                  FUNCTION TRIM(AR-FUNC) "]"
               DELIMITED BY SIZE INTO AR-MSG
           ADD 1 TO AR-MSG-CNT.
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

       9400-COMMIT SECTION.
       9400-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE "COMT" TO SQLIO-FUNC
               MOVE 0 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9400-EXIT.
           EXIT.

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               MOVE 0 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9500-EXIT.
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
