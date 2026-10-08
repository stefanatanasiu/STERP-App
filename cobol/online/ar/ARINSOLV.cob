      *>================================================================
      *> PROGRAM-ID : ARINSOLV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : INSOLVENCY, RETENTION OF TITLE AND DEBT
      *>              RECOVERY.  RECORDS THE EVENT, STOPS THE
      *>              ACCOUNT, FREEZES THE CHASE CYCLE, LISTS THE
      *>              UNPAID GOODS SUPPLIED UNDER A RETENTION OF
      *>              TITLE CLAUSE, EVIDENCES THE PRE-ACTION
      *>              PROTOCOL LETTER OF CLAIM FOR AN INDIVIDUAL
      *>              DEBTOR AND AUTHORISES A HAND OFF TO AN AGENCY
      *>              OR SOLICITOR.
      *>              NOTHING HERE REMOVES DEBT FROM THE LEDGER: A
      *>              DEBT LEAVES ONLY BY WRITE OFF UNDER AUTHORITY
      *>              THROUGH ARADJUST (AR-027).
      *> CALLED BY  : ARCTL (.NET), ARWORKQ, BADDEBT
      *> INTERFACE  : CALL "ARINSOLV" USING WS-INSOLV-CA
      *>              COPYBOOK ARINSOLVCA
      *> CALLS      : SQLIO, PARMGET, PERMCHK, NUMGEN, DATECALC,
      *>              AUDITLOG, EXCPRAIS, FXCONV
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL SALES LEDGER PORT (MVS)
      *> 1994-10-03 DJM  CR0410 RECEIVERSHIP STOP FLAG
      *> 2003-06-30 DJM  CR1601 INSOLVENCY AND ROT CLAIM REGISTER
      *> 2009-02-16 KMP  CR1988 ADMINISTRATION REPLACES RECEIVERSHIP
      *> 2026-10-05 STERP PRE-ACTION PROTOCOL FOR DEBT CLAIMS
      *> 2026-10-07 STERP CR2614 A DIVIDEND RECEIVED FROM THE OFFICE
      *>                 HOLDER IS CONVERTED AND REPORTED IN STERLING
      *>================================================================
      *> REQ: AR-027, AR-016
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARINSOLV.
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
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXC-CA==.
       COPY FXCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-FX-CA==.
       01  WS-CUS.
           05  WS-CU-NAME              PIC X(120).
           05  WS-CU-STATUS            PIC X(12).
           05  WS-CU-FORM              PIC X(14).
           05  WS-CU-INSTYPE           PIC X(16).
           05  WS-CU-INSDATE           PIC X(10).
           05  WS-CU-OUTST             PIC X(24).
           05  WS-CU-CCY               PIC X(3).
       01  WS-CSE.
           05  WS-CS-ID                PIC X(24).
           05  WS-CS-REF               PIC X(30).
           05  WS-CS-STATUS            PIC X(12).
           05  WS-CS-PAP               PIC X(10).
      *>   one retention of title line as it comes back from SQL
       01  WS-ROT.
           05  WS-RT-INV               PIC X(30).
           05  WS-RT-ITEM              PIC X(40).
           05  WS-RT-DESC              PIC X(60).
           05  WS-RT-DESPREF           PIC X(40).
           05  WS-RT-QTY               PIC X(24).
           05  WS-RT-DATE              PIC X(10).
           05  WS-RT-VALUE             PIC X(24).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-CUST-FOUND           PIC X VALUE "N".
           05  WS-CASE-FOUND           PIC X VALUE "N".
           05  WS-MAX-ROT              PIC S9(4) VALUE 20.
           05  WS-PAPREQ               PIC S9(4) VALUE 30.
           05  WS-ELAPSED              PIC S9(7) VALUE 0.
           05  WS-PAPDATE              PIC 9(8) VALUE 0.
       LINKAGE SECTION.
       COPY ARINSOLVCA.
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
           PERFORM 0400-FIND-CASE THRU 0400-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 1600-DIVIDEND THRU 1600-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "RECORD"   PERFORM 1000-RECORD THRU 1000-EXIT
               WHEN "ROTLIST"  PERFORM 2000-ROT-LIST THRU 2000-EXIT
               WHEN "PAPSENT"  PERFORM 3000-PAP-SENT THRU 3000-EXIT
               WHEN "HANDOFF"  PERFORM 4000-HANDOFF THRU 4000-EXIT
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
           INITIALIZE WS-CSE
           MOVE "N" TO WS-TRAN-OPEN WS-CUST-FOUND WS-CASE-FOUND
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "ARINSOLV" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-027" TO AR-REQ-REF
           MOVE FUNCTION UPPER-CASE(IS-EVENT-TYPE) TO IS-EVENT-TYPE
           MOVE FUNCTION UPPER-CASE(IS-HANDOFF-TYPE)
               TO IS-HANDOFF-TYPE
           MOVE SPACES TO IS-BLOCK-REASON
           MOVE "N" TO IS-HANDOFF-ALLOWED
           IF IS-EVENT-DATE = 0
               MOVE AR-BUSDATE TO IS-EVENT-DATE
           END-IF
           MOVE 0 TO IS-ROT-CNT IS-ROT-VALUE IS-ORDERS-HELD
           MOVE 0 TO IS-DIVIDEND-GBP
           MOVE FUNCTION UPPER-CASE(IS-DIVIDEND-CCY)
               TO IS-DIVIDEND-CCY
           IF IS-DIVIDEND-DATE = 0
               MOVE IS-EVENT-DATE TO IS-DIVIDEND-DATE
           END-IF
           PERFORM 0150-PAP-DAYS THRU 0150-EXIT.
       0100-EXIT.
           EXIT.

      *>   the Pre-Action Protocol for Debt Claims gives the debtor
      *>   at least 30 days to reply to the letter of claim
       0150-PAP-DAYS SECTION.
       0150-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.PAP.RESPONSE.DAYS" TO PM-PARAM-CODE
           MOVE "30" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF CA-RETURN-CODE OF WS-PARM-CA < 8
               MOVE PM-VALUE-NUM TO WS-PAPREQ
           ELSE
               MOVE 30 TO WS-PAPREQ
           END-IF
           IF WS-PAPREQ < 30
               MOVE 30 TO WS-PAPREQ
           END-IF
           MOVE WS-PAPREQ TO IS-PAP-DAYS-REQ.
       0150-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "INS0001" TO AR-REASON
               MOVE "A company code is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF IS-CUSTOMER = SPACES OR IS-CUSTOMER = LOW-VALUES
               MOVE "INS0002" TO AR-REASON
               MOVE "A customer account is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "RECORD"
              AND NOT (IS-ADMINISTRATION OR IS-LIQUIDATION
                       OR IS-CVA OR IS-BANKRUPTCY)
               MOVE "INS0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Event type [" FUNCTION TRIM(IS-EVENT-TYPE)
                      "] is not ADMIN, LIQUIDATION, CVA or "
                      "BANKRUPTCY"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "RECORD" AND IS-OFFICE-HOLDER = SPACES
               MOVE "INS0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The appointed office holder must be named "
                      "so that the proof of debt can be lodged"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "RECORD" AND IS-EVENT-DATE > AR-BUSDATE
               MOVE "INS0005" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "An insolvency event cannot be dated in the "
                      "future"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "HANDOFF" AND IS-HANDOFF-REF = SPACES
               MOVE "INS0006" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A hand off needs the agency or solicitor "
                      "reference it is passed under"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>   the legal form decides whether the Pre-Action Protocol
      *>   for Debt Claims applies - it governs claims against an
      *>   individual, which includes a sole trader
       0300-READ-CUSTOMER SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.Status, c.LegalForm, "
                  "ISNULL(c.InsolvencyType,''), "
                  "ISNULL(CONVERT(varchar(10), c.InsolvencyDate, "
                  "112),''), CONVERT(varchar(24), ISNULL((SELECT "
                  "SUM(o.ResidualAmountGBP) FROM ar.OpenItem o "
                  "WHERE o.CompanyCode = c.CompanyCode "
                  "AND o.CustomerCode = c.CustomerCode "
                  "AND o.ResidualAmountGBP > 0),0)), "
                  "ISNULL(c.CurrencyCode,'GBP') "
                  "FROM ar.Customer c WHERE c.CompanyCode = ? "
                  "AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 14 TO SQLIO-COL-WIDTH(3)
           MOVE 16 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 3 TO SQLIO-COL-WIDTH(7)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUS
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "INS0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(IS-CUSTOMER)
                      " is not on the sales ledger"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 0300-EXIT
           END-IF
           MOVE "Y" TO WS-CUST-FOUND
           MOVE WS-CU-FORM TO IS-LEGAL-FORM
           COMPUTE IS-OUTSTANDING = FUNCTION NUMVAL(WS-CU-OUTST)
           IF WS-CU-FORM = "SOLETRADER" OR WS-CU-FORM = "PARTNERSHIP"
              OR IS-BANKRUPTCY
               MOVE "Y" TO IS-IS-INDIVIDUAL
           ELSE
               MOVE "N" TO IS-IS-INDIVIDUAL
           END-IF
           IF WS-CU-STATUS = "SUSPENDED"
               MOVE "Y" TO IS-ACCOUNT-STOPPED
           ELSE
               MOVE "N" TO IS-ACCOUNT-STOPPED
           END-IF.
       0300-EXIT.
           EXIT.

       0400-FIND-CASE SECTION.
       0400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), k.CaseId), k.CaseRef, "
                  "k.Status, ISNULL(CONVERT(varchar(10), "
                  "k.PreActionProtocolSentAt, 112),'') "
                  "FROM ar.CollectionCase k WHERE k.CompanyCode = ? "
                  "AND k.CustomerCode = ? "
                  "AND k.CaseType IN ('INSOLVENCY','LEGAL') "
                  "AND k.Status IN ('OPEN','ESCALATED') "
                  "ORDER BY k.CaseId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CSE
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 0400-EXIT
           END-IF
           MOVE "Y" TO WS-CASE-FOUND
           COMPUTE IS-CASE-ID = FUNCTION NUMVAL(WS-CS-ID)
           IF WS-CS-PAP NOT = SPACES
               MOVE WS-CS-PAP(1:8) TO AR-COMPACT-DATE
               MOVE AR-COMPACT-DATE TO AR-NUM-DATE
               MOVE AR-NUM-DATE TO IS-PAP-SENT-DATE
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   RECORD - the event, the stop, the freeze and the register
      *>   REQ: AR-027, AR-016
       1000-RECORD SECTION.
       1000-START.
           MOVE "AR.INSOLVENCY.RECORD" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 1000-EXIT
           END-IF
           IF WS-CU-INSTYPE NOT = SPACES
              AND WS-CU-INSTYPE NOT = IS-EVENT-TYPE
               MOVE 4 TO AR-RC
               MOVE "INS0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The account already carries a "
                      FUNCTION TRIM(WS-CU-INSTYPE)
                      " event; the later event replaces it"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 1100-STOP-ACCOUNT THRU 1100-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-CREDIT-HOLD THRU 1200-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1300-FREEZE-DUNNING THRU 1300-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1400-OPEN-CASE THRU 1400-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 9400-COMMIT
           PERFORM 1500-HELD-ORDERS THRU 1500-EXIT
           PERFORM 2000-ROT-LIST THRU 2000-EXIT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           PERFORM 8300-EXCEPTION THRU 8300-EXIT
           MOVE "Y" TO IS-ACCOUNT-STOPPED
           MOVE SPACES TO AR-MSG
           MOVE IS-OUTSTANDING TO AR-MONEY-UK
           STRING FUNCTION TRIM(IS-EVENT-TYPE) " recorded for "
                  FUNCTION TRIM(IS-CUSTOMER)
                  "; the account is on stop with GBP "
                  FUNCTION TRIM(AR-MONEY-UK)
                  " outstanding, which stays on the ledger"
               DELIMITED BY SIZE INTO AR-MSG
           IF IS-DIVIDEND-GBP NOT = 0
               MOVE IS-DIVIDEND-GBP TO AR-MONEY-UK
               MOVE SPACES TO AR-TEXT2
               STRING FUNCTION TRIM(AR-MSG) "; dividend of GBP "
                      FUNCTION TRIM(AR-MONEY-UK) " received"
                   DELIMITED BY SIZE INTO AR-TEXT2
               MOVE AR-TEXT2 TO AR-MSG
           END-IF.
       1000-EXIT.
           EXIT.

       1100-STOP-ACCOUNT SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.Customer SET InsolvencyType = ?, "
                  "InsolvencyDate = ?, OfficeHolder = ?, "
                  "ProofOfDebtRef = NULLIF(?,''), "
                  "Status = 'SUSPENDED', StatusReason = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE IS-EVENT-TYPE TO SQLIO-PARM-VAL(1)
           MOVE IS-EVENT-DATE TO SQLIO-PARM-VAL(2)
           MOVE IS-OFFICE-HOLDER TO SQLIO-PARM-VAL(3)
           MOVE IS-PROOF-REF TO SQLIO-PARM-VAL(4)
           MOVE SPACES TO AR-TEXT
           MOVE IS-EVENT-DATE TO AR-DATE-ED
           STRING FUNCTION TRIM(IS-EVENT-TYPE) " notified "
                  AR-DATE-ED(7:2) "/" AR-DATE-ED(5:2) "/"
                  AR-DATE-ED(1:4) " - office holder "
                  FUNCTION TRIM(IS-OFFICE-HOLDER)
               DELIMITED BY SIZE INTO AR-TEXT
           MOVE AR-TEXT TO SQLIO-PARM-VAL(5)
           MOVE AR-USERID TO SQLIO-PARM-VAL(6)
           MOVE AR-CO TO SQLIO-PARM-VAL(7)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(8)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 8
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       1100-EXIT.
           EXIT.

      *>   the credit hold is what CREDCHK reads when a new order or
      *>   despatch asks to be released
       1200-CREDIT-HOLD SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE ar.CustomerCredit AS t USING (SELECT ? AS "
                  "CompanyCode, ? AS CustomerCode) AS s "
                  "ON t.CompanyCode = s.CompanyCode "
                  "AND t.CustomerCode = s.CustomerCode "
                  "WHEN MATCHED THEN UPDATE SET t.HoldFlag = 1, "
                  "t.HoldReason = ?, t.HoldSetBy = ?, "
                  "t.HoldSetAt = SYSDATETIME(), "
                  "t.RequiresPrepayment = 1, t.ProFormaOnly = 1, "
                  "t.ModifiedBy = ?, t.ModifiedAt = SYSDATETIME() "
                  "WHEN NOT MATCHED THEN INSERT (CompanyCode, "
                  "CustomerCode, CreditLimitGBP, HoldFlag, "
                  "HoldReason, HoldSetBy, HoldSetAt, "
                  "RequiresPrepayment, ProFormaOnly, CreatedBy) "
                  "VALUES (s.CompanyCode, s.CustomerCode, 0, 1, ?, "
                  "?, SYSDATETIME(), 1, 1, ?);"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE SPACES TO AR-TEXT2
           STRING "Insolvency: " FUNCTION TRIM(IS-EVENT-TYPE)
                  " - no further supply on credit"
               DELIMITED BY SIZE INTO AR-TEXT2
           MOVE AR-TEXT2 TO SQLIO-PARM-VAL(3)
           MOVE AR-USERID TO SQLIO-PARM-VAL(4)
           MOVE AR-USERID TO SQLIO-PARM-VAL(5)
           MOVE AR-TEXT2 TO SQLIO-PARM-VAL(6)
           MOVE AR-USERID TO SQLIO-PARM-VAL(7)
           MOVE AR-USERID TO SQLIO-PARM-VAL(8)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 8
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       1200-EXIT.
           EXIT.

      *>   AR-016 - the chase cycle stops; chasing a company in
      *>   administration is pointless and improper.  The debt is
      *>   untouched.
       1300-FREEZE-DUNNING SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.OpenItem SET "
                  "CollectionStatus = 'FROZEN', ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND CustomerCode = ? AND ResidualAmountGBP <> 0 "
                  "AND WrittenOff = 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(3)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       1300-EXIT.
           EXIT.

       1400-OPEN-CASE SECTION.
       1400-START.
           IF WS-CASE-FOUND = "Y"
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE ar.CollectionCase SET Reason = ?, "
                      "ReasonCode = ?, Status = 'ESCALATED', "
                      "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                      "WHERE CaseId = ? AND CompanyCode = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 5 TO SQLIO-PARM-CNT
               MOVE IS-OFFICE-HOLDER TO SQLIO-PARM-VAL(1)
               MOVE IS-EVENT-TYPE TO SQLIO-PARM-VAL(2)
               MOVE AR-USERID TO SQLIO-PARM-VAL(3)
               MOVE IS-CASE-ID TO AR-INT-ED
               MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
               MOVE AR-CO TO SQLIO-PARM-VAL(5)
               PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 5
                   MOVE "N" TO SQLIO-PARM-NULL(AR-I)
               END-PERFORM
               PERFORM 8500-EXEC THRU 8500-EXIT
               GO TO 1400-EXIT
           END-IF
           PERFORM 8100-CASE-REF THRU 8100-EXIT
           IF AR-RC-BAD
               GO TO 1400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.CollectionCase (CompanyCode, "
                  "CustomerCode, CaseRef, CaseType, ReasonCode, "
                  "Reason, DisputedAmountGBP, OwnerUser, "
                  "CollectionStep, Status, CreatedBy) "
                  "OUTPUT CONVERT(varchar(24), INSERTED.CaseId) "
                  "VALUES (?, ?, ?, 'INSOLVENCY', ?, ?, 0, ?, 0, "
                  "'ESCALATED', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE WS-CS-REF TO SQLIO-PARM-VAL(3)
           MOVE IS-EVENT-TYPE TO SQLIO-PARM-VAL(4)
           MOVE SPACES TO AR-TEXT
           STRING FUNCTION TRIM(IS-EVENT-TYPE) " - office holder "
                  FUNCTION TRIM(IS-OFFICE-HOLDER) " - proof ref "
                  FUNCTION TRIM(IS-PROOF-REF)
               DELIMITED BY SIZE INTO AR-TEXT
           IF IS-DIVIDEND-GBP NOT = 0
               MOVE IS-DIVIDEND-GBP TO AR-MONEY-UK
               MOVE SPACES TO AR-TEXT2
               STRING FUNCTION TRIM(AR-TEXT) " - dividend GBP "
                      FUNCTION TRIM(AR-MONEY-UK)
                   DELIMITED BY SIZE INTO AR-TEXT2
               MOVE AR-TEXT2 TO AR-TEXT
           END-IF
           MOVE AR-TEXT TO SQLIO-PARM-VAL(5)
           MOVE AR-USERID TO SQLIO-PARM-VAL(6)
           MOVE AR-USERID TO SQLIO-PARM-VAL(7)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 7
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1400-EXIT
           END-IF
           COMPUTE IS-CASE-ID = FUNCTION NUMVAL(WS-NEW-ID)
           MOVE "Y" TO WS-CASE-FOUND.
       1400-EXIT.
           EXIT.

      *>   open orders are counted here and held by the credit hold
      *>   set above; the distribution module owns their release
       1500-HELD-ORDERS SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), COUNT(*)) "
                  "FROM so.SalesOrder s WHERE s.CompanyCode = ? "
                  "AND s.CustomerCode = ? "
                  "AND s.CommercialStatus NOT IN "
                  "('CANCELLED','CLOSED','COMPLETE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               MOVE 0 TO IS-ORDERS-HELD
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 1500-EXIT
           END-IF
           COMPUTE IS-ORDERS-HELD = FUNCTION NUMVAL(WS-NEW-ID).
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   ROTLIST - unpaid goods supplied under a retention of title
      *>   clause, by invoice and item, so that a claim can be made
      *>   against the office holder.  Twenty lines, as the original
      *>   screen held - a larger claim is produced by the batch
      *>   report ARROTRPT.
      *>   REQ: AR-027
      *>----------------------------------------------------------------
      *>   AR-027 - a dividend received from the office holder is
      *>   stated in the currency it arrived in and reported in
      *>   sterling; the debt itself stays on the ledger until it is
      *>   written off under authority
       1600-DIVIDEND SECTION.
       1600-START.
           MOVE 0 TO IS-DIVIDEND-GBP
           IF IS-DIVIDEND-AMT = 0
               GO TO 1600-EXIT
           END-IF
           IF IS-DIVIDEND-AMT < 0
               MOVE 8 TO AR-RC
               MOVE "INS0041" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A dividend received cannot be negative"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1600-EXIT
           END-IF
           IF IS-DIVIDEND-CCY = SPACES
               MOVE WS-CU-CCY TO IS-DIVIDEND-CCY
           END-IF
           IF IS-DIVIDEND-CCY = SPACES
               MOVE "GBP" TO IS-DIVIDEND-CCY
           END-IF
           IF IS-DIVIDEND-DATE = 0
               MOVE IS-EVENT-DATE TO IS-DIVIDEND-DATE
           END-IF
           IF IS-DIVIDEND-CCY = "GBP"
               MOVE IS-DIVIDEND-AMT TO IS-DIVIDEND-GBP
               GO TO 1600-EXIT
           END-IF
           INITIALIZE WS-FX-CA
           MOVE "TOGBP" TO CA-FUNCTION OF WS-FX-CA
           MOVE AR-CO TO CA-COMPANY OF WS-FX-CA
           MOVE AR-SITE TO CA-SITE OF WS-FX-CA
           MOVE AR-USERID TO CA-USER OF WS-FX-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-FX-CA
           MOVE IS-DIVIDEND-CCY TO FX-CURRENCY
           MOVE IS-DIVIDEND-DATE TO FX-RATE-DATE
           MOVE "SPOT" TO FX-RATE-TYPE
           MOVE IS-DIVIDEND-AMT TO FX-FOREIGN-AMOUNT
           MOVE 2 TO FX-PLACES
           MOVE "ASSET" TO FX-POSITION
           CALL "FXCONV" USING WS-FX-CA
           IF CA-RETURN-CODE OF WS-FX-CA > 4
               MOVE CA-RETURN-CODE OF WS-FX-CA TO AR-RC
               MOVE "INS0040" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The " FUNCTION TRIM(IS-DIVIDEND-CCY)
                      " dividend cannot be converted - "
                      FUNCTION TRIM(CA-MESSAGE OF WS-FX-CA)
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1600-EXIT
           END-IF
           COMPUTE IS-DIVIDEND-GBP ROUNDED = FX-GBP-AMOUNT.
       1600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   ROTLIST - unpaid goods supplied under retention of title
       2000-ROT-LIST SECTION.
       2000-START.
           MOVE 0 TO IS-ROT-CNT IS-ROT-VALUE
           INITIALIZE IS-ROT-TAB
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (20) i.InvoiceNumber, "
                  "ISNULL(l.ItemCode,''), l.Description, "
                  "ISNULL(i.DespatchRef,''), "
                  "CONVERT(varchar(24), l.Quantity), "
                  "CONVERT(varchar(10), i.DocumentDate, 112), "
                  "CONVERT(varchar(24), l.NetAmountGBP) "
                  "FROM ar.OpenItem o "
                  "INNER JOIN ar.SalesInvoice i "
                  "ON i.InvoiceId = o.SourceInvoiceId "
                  "INNER JOIN ar.SalesInvoiceLine l "
                  "ON l.InvoiceId = i.InvoiceId "
                  "WHERE o.CompanyCode = ? AND o.CustomerCode = ? "
                  "AND o.RetentionOfTitle = 1 "
                  "AND o.ResidualAmountGBP > 0 "
                  "AND o.WrittenOff = 0 "
                  "ORDER BY i.DocumentDate, l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 60 TO SQLIO-COL-WIDTH(3)
           MOVE 40 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROT
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE "N" TO AR-DONE
           PERFORM UNTIL AR-FINISHED OR AR-RC-BAD
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROT
               IF SQLIO-ERROR
                   PERFORM 9300-SQL-FAILED
               ELSE
                   IF SQLIO-NOTFOUND
                       MOVE "Y" TO AR-DONE
                   ELSE
                       PERFORM 2100-ONE-ROT THRU 2100-EXIT
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           IF IS-ROT-CNT = 0
               IF AR-FUNC = "ROTLIST"
                   MOVE 4 TO AR-RC
                   MOVE "INS0030" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "No unpaid goods supplied under "
                          "retention of title were found"
                       DELIMITED BY SIZE INTO AR-MSG
                   ADD 1 TO AR-MSG-CNT
               END-IF
               GO TO 2000-EXIT
           END-IF
           IF AR-FUNC = "ROTLIST"
               MOVE SPACES TO AR-MSG
               MOVE IS-ROT-VALUE TO AR-MONEY-UK
               MOVE IS-ROT-CNT TO AR-INT-ED
               STRING FUNCTION TRIM(AR-INT-ED)
                      " retention of title line(s) worth GBP "
                      FUNCTION TRIM(AR-MONEY-UK)
                      " remain unpaid and may be claimed"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       2000-EXIT.
           EXIT.

       2100-ONE-ROT SECTION.
       2100-START.
           IF IS-ROT-CNT >= WS-MAX-ROT
               MOVE "Y" TO AR-DONE
               GO TO 2100-EXIT
           END-IF
           ADD 1 TO IS-ROT-CNT
           MOVE IS-ROT-CNT TO AR-I
           MOVE WS-RT-INV TO IS-R-INVOICE(AR-I)
           MOVE WS-RT-ITEM TO IS-R-ITEM(AR-I)
           MOVE WS-RT-DESC TO IS-R-DESC(AR-I)
           MOVE WS-RT-DESPREF TO IS-R-SERIAL(AR-I)
           COMPUTE IS-R-QTY(AR-I) = FUNCTION NUMVAL(WS-RT-QTY)
           IF WS-RT-DATE NOT = SPACES
               MOVE WS-RT-DATE(1:8) TO AR-COMPACT-DATE
               MOVE AR-COMPACT-DATE TO AR-NUM-DATE
               MOVE AR-NUM-DATE TO IS-R-DESPATCH-DT(AR-I)
           END-IF
           COMPUTE IS-R-VALUE(AR-I) = FUNCTION NUMVAL(WS-RT-VALUE)
           ADD IS-R-VALUE(AR-I) TO IS-ROT-VALUE.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   PAPSENT - evidence the Pre-Action Protocol letter of claim.
      *>   It governs a claim against an individual, which for this
      *>   ledger means a sole trader or a partnership.
      *>   REQ: AR-027
       3000-PAP-SENT SECTION.
       3000-START.
           MOVE "AR.RECOVERY.PAP" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 3000-EXIT
           END-IF
           IF IS-IS-INDIVIDUAL NOT = "Y"
               MOVE 4 TO AR-RC
               MOVE "INS0040" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The Pre-Action Protocol for Debt Claims "
                      "applies to an individual debtor; "
                      FUNCTION TRIM(IS-CUSTOMER) " is a "
                      FUNCTION TRIM(IS-LEGAL-FORM)
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF
           IF IS-PAP-SENT-DATE = 0
               MOVE AR-BUSDATE TO IS-PAP-SENT-DATE
           END-IF
           IF IS-PAP-SENT-DATE > AR-BUSDATE
               MOVE 8 TO AR-RC
               MOVE "INS0041" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A letter of claim cannot be evidenced "
                      "before it has been sent"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 3000-EXIT
           END-IF
           IF WS-CASE-FOUND NOT = "Y"
               PERFORM 3100-LEGAL-CASE THRU 3100-EXIT
               IF AR-RC-BAD
                   GO TO 3000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CollectionCase SET "
                  "PreActionProtocolSentAt = ?, Status = "
                  "'ESCALATED', ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CaseId = ? "
                  "AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE IS-PAP-SENT-DATE TO SQLIO-PARM-VAL(1)
           MOVE AR-USERID TO SQLIO-PARM-VAL(2)
           MOVE IS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-ELAPSED THRU 3200-EXIT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           IF AR-RC-OK
               MOVE SPACES TO AR-MSG
               MOVE WS-PAPREQ TO AR-INT-ED
               STRING "Letter of claim evidenced; the debtor has "
                      FUNCTION TRIM(AR-INT-ED)
                      " days to reply before proceedings"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       3000-EXIT.
           EXIT.

       3100-LEGAL-CASE SECTION.
       3100-START.
           PERFORM 8100-CASE-REF THRU 8100-EXIT
           IF AR-RC-BAD
               GO TO 3100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.CollectionCase (CompanyCode, "
                  "CustomerCode, CaseRef, CaseType, ReasonCode, "
                  "Reason, DisputedAmountGBP, OwnerUser, "
                  "CollectionStep, Status, CreatedBy) "
                  "OUTPUT CONVERT(varchar(24), INSERTED.CaseId) "
                  "VALUES (?, ?, ?, 'LEGAL', 'RECOVERY', ?, 0, ?, "
                  "5, 'ESCALATED', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE IS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE WS-CS-REF TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO AR-TEXT
           STRING "Debt recovery - outstanding balance referred "
                  "for legal action"
               DELIMITED BY SIZE INTO AR-TEXT
           MOVE AR-TEXT TO SQLIO-PARM-VAL(4)
           MOVE AR-USERID TO SQLIO-PARM-VAL(5)
           MOVE AR-USERID TO SQLIO-PARM-VAL(6)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3100-EXIT
           END-IF
           COMPUTE IS-CASE-ID = FUNCTION NUMVAL(WS-NEW-ID)
           MOVE "Y" TO WS-CASE-FOUND.
       3100-EXIT.
           EXIT.

       3200-ELAPSED SECTION.
       3200-START.
           MOVE 0 TO IS-PAP-DAYS-ELAPSE
           IF IS-PAP-SENT-DATE = 0
               GO TO 3200-EXIT
           END-IF
           INITIALIZE WS-DATE-CA
           MOVE "DAYSBTWN" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE IS-PAP-SENT-DATE TO DC-DATE-1
           MOVE AR-BUSDATE TO DC-DATE-2
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE 0 TO WS-ELAPSED
           ELSE
               MOVE DC-RESULT-NUM TO WS-ELAPSED
           END-IF
           IF WS-ELAPSED < 0
               MOVE 0 TO WS-ELAPSED
           END-IF
           MOVE WS-ELAPSED TO IS-PAP-DAYS-ELAPSE.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   HANDOFF - an authorised referral to an agency or a
      *>   solicitor.  The debt stays on the sales ledger; only a
      *>   write off under authority removes it (AR-027).
      *>   REQ: AR-027, AR-016
       4000-HANDOFF SECTION.
       4000-START.
           MOVE "AR.RECOVERY.HANDOFF" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 4000-EXIT
           END-IF
           MOVE "N" TO IS-HANDOFF-ALLOWED
           IF IS-HANDOFF-TYPE = SPACES
               MOVE "AGENCY" TO IS-HANDOFF-TYPE
           END-IF
           PERFORM 3200-ELAPSED THRU 3200-EXIT
           IF IS-IS-INDIVIDUAL = "Y" AND IS-PAP-SENT-DATE = 0
               MOVE SPACES TO IS-BLOCK-REASON
               STRING "No Pre-Action Protocol letter of claim has "
                      "been evidenced for this individual debtor"
                   DELIMITED BY SIZE INTO IS-BLOCK-REASON
               MOVE 6 TO AR-RC
               MOVE "INS0050" TO AR-REASON
               MOVE IS-BLOCK-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4000-EXIT
           END-IF
           IF IS-IS-INDIVIDUAL = "Y"
              AND IS-PAP-DAYS-ELAPSE < WS-PAPREQ
               MOVE SPACES TO IS-BLOCK-REASON
               MOVE WS-PAPREQ TO AR-INT-ED
               STRING "The debtor's reply period of "
                      FUNCTION TRIM(AR-INT-ED)
                      " days has not yet expired"
                   DELIMITED BY SIZE INTO IS-BLOCK-REASON
               MOVE 6 TO AR-RC
               MOVE "INS0051" TO AR-REASON
               MOVE IS-BLOCK-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4000-EXIT
           END-IF
           IF IS-OUTSTANDING <= 0
               MOVE 8 TO AR-RC
               MOVE "INS0052" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "There is nothing outstanding to hand over "
                      "for recovery"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4000-EXIT
           END-IF
           IF WS-CASE-FOUND NOT = "Y"
               PERFORM 3100-LEGAL-CASE THRU 3100-EXIT
               IF AR-RC-BAD
                   GO TO 4000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CollectionCase SET "
                  "ExternalRecoveryRef = ?, EscalatedTo = ?, "
                  "Status = 'ESCALATED', CollectionStep = 6, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CaseId = ? AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE IS-HANDOFF-REF TO SQLIO-PARM-VAL(1)
           MOVE IS-HANDOFF-TYPE TO SQLIO-PARM-VAL(2)
           MOVE AR-USERID TO SQLIO-PARM-VAL(3)
           MOVE IS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE AR-CO TO SQLIO-PARM-VAL(5)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT
           IF AR-RC-BAD
               GO TO 4000-EXIT
           END-IF
           MOVE "Y" TO IS-HANDOFF-ALLOWED
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO AR-MSG
           MOVE IS-OUTSTANDING TO AR-MONEY-UK
           STRING "GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " handed to " FUNCTION TRIM(IS-HANDOFF-TYPE)
                  " under " FUNCTION TRIM(IS-HANDOFF-REF)
                  "; the debt remains on the sales ledger"
               DELIMITED BY SIZE INTO AR-MSG.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-PERMISSION SECTION.
       8000-START.
           MOVE PK-PERMISSION TO AR-TEXT
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE AR-TEXT TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "INS0060" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       8000-EXIT.
           EXIT.

       8100-CASE-REF SECTION.
       8100-START.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE "ARCASE" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE "INS0061" TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 8100-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO WS-CS-REF.
       8100-EXIT.
           EXIT.

       8200-AUDIT SECTION.
       8200-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE AR-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE AR-USERID TO CA-USER OF WS-AUDIT-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE "ar.Customer" TO AU-ENTITY-TYPE
           MOVE IS-CUSTOMER TO AU-ENTITY-KEY
           MOVE AR-FUNC TO AU-ACTION
           MOVE "InsolvencyType" TO AU-FIELD-NAME
           MOVE WS-CU-INSTYPE TO AU-OLD-VALUE
           MOVE IS-EVENT-TYPE TO AU-NEW-VALUE
           MOVE SPACES TO AU-REASON
           STRING FUNCTION TRIM(IS-EVENT-TYPE) " - "
                  FUNCTION TRIM(IS-OFFICE-HOLDER)
               DELIMITED BY SIZE INTO AU-REASON
           MOVE IS-PROOF-REF TO AU-AUTHORITY-REF
           MOVE "COBOL" TO AU-SOURCE
           MOVE "Y" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUDIT-CA.
       8200-EXIT.
           EXIT.

       8300-EXCEPTION SECTION.
       8300-START.
           INITIALIZE WS-EXC-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXC-CA
           MOVE AR-CO TO CA-COMPANY OF WS-EXC-CA
           MOVE AR-SITE TO CA-SITE OF WS-EXC-CA
           MOVE AR-USERID TO CA-USER OF WS-EXC-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-EXC-CA
           MOVE "AR" TO EX-MODULE
           MOVE "HOLD" TO EX-SEVERITY
           MOVE "AR.CUSTOMER.INSOLVENT" TO EX-CATEGORY
           MOVE SPACES TO EX-MESSAGE-TEXT
           MOVE IS-OUTSTANDING TO AR-MONEY-UK
           STRING FUNCTION TRIM(IS-CUSTOMER) " is in "
                  FUNCTION TRIM(IS-EVENT-TYPE) " with GBP "
                  FUNCTION TRIM(AR-MONEY-UK)
                  " outstanding; review bad debt relief and "
                  "retention of title"
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           MOVE "CUSTOMER" TO EX-DOCUMENT-TYPE
           MOVE IS-CUSTOMER TO EX-DOCUMENT-REF
           MOVE "CREDITMANAGER" TO EX-OWNER-ROLE
           MOVE +90 TO EX-PRIORITY
           MOVE IS-OUTSTANDING TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXC-CA
           MOVE EX-EXCEPTION-ID TO IS-EXCEPTION-ID.
       8300-EXIT.
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
           MOVE "INS0009" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARINSOLV does not support function ["
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
