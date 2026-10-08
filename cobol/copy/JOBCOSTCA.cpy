      *>================================================================
      *> JOBCOSTCA - COMMAREA FOR JOBCOST, JOB AND SUB-JOB COSTING
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *> JOBS ARE TWO LEVELS ONLY (D-13): A JOB AND ITS SUB-JOBS.  A
      *> SUB-JOB ROLLS UP INTO ITS PARENT; BOOKS OF DIFFERENT COMPANIES
      *> ARE NEVER MERGED (CST-014).  A SOURCE DOCUMENT CONTRIBUTES ITS
      *> ECONOMIC COST EXACTLY ONCE EVEN WHEN SEVERAL REPORTS SHOW IT.
      *>
      *>   estimate at completion = actual recognised cost to date
      *>                          + remaining recognised commitments
      *>                          + uncommitted estimate to complete
      *>   forecast cost variance = estimate at completion - budget
      *>
      *> A POSITIVE COST VARIANCE IS ADVERSE IN THIS PACK.
      *>
      *> WORKED EXAMPLE C5 OF THE SPECIFICATION
      *>   BUDGET 5,000.00, ACTUAL 2,100.00, COMMITMENTS 1,600.00,
      *>   ESTIMATE TO COMPLETE 1,500.00 -> EAC 5,200.00, ADVERSE 200.00
      *>
      *> CA-FUNCTION VALUES
      *>   CREATE    CREATE A JOB OR SUB-JOB
      *>   BUDGET    LODGE A BUDGET VERSION (VERSION 1 IS THE ORIGINAL
      *>             BASELINE AND IS NEVER OVERWRITTEN - CST-015)
      *>   APPRBUDG  APPROVE A BUDGET VERSION
      *>   POSTCOST  BOOK AN APPROVED COST TRANSACTION (CST-017)
      *>   REVERSE   REVERSE A COST TRANSACTION THROUGH A LINKED ENTRY
      *>   ENQUIRE   BUDGET, ACTUAL, COMMITMENT AND VARIANCE BY ELEMENT
      *>   FORECAST  ESTIMATE AT COMPLETION (CST-016)
      *>   COMPLETE  OPERATIONAL COMPLETION WITH A DISPOSITION
      *>   CLOSE     FINANCIAL CLOSURE AFTER REVIEW (CST-020)
      *>   REOPEN    AUTHORISED REOPENING FOR A LATE COST
      *>
      *> REASON CODES
      *>   CST0801 COMPANY OR JOB REFERENCE MISSING
      *>   CST0802 JOB NOT FOUND
      *>   CST0803 A SUB-JOB MUST NAME AN EXISTING LEVEL ONE PARENT
      *>   CST0804 ONLY TWO JOB LEVELS ARE SUPPORTED (D-13)
      *>   CST0805 THE ORIGINAL BUDGET MAY NOT BE OVERWRITTEN
      *>   CST0806 BUDGET VERSION ALREADY EXISTS
      *>   CST0807 COST ELEMENT NOT KNOWN
      *>   CST0808 JOB IS FINANCIALLY CLOSED - REOPEN FIRST
      *>   CST0809 RESIDUAL WIP MUST BE DISPOSED BEFORE CLOSURE
      *>   CST0810 OPERATIONAL COMPLETION REQUIRED BEFORE CLOSURE
      *>   CST0811 UNSUPPORTED FUNCTION
      *>   CST0812 AMOUNT MUST NOT BE ZERO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-02-16 DJM  CR0634 ORIGINAL JOB COSTING
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT JOB DATES
      *> 2013-06-03 RJH  CR1988 OPERATIONAL AND FINANCIAL STATES SPLIT
      *> 2026-10-06 STERP CST-014 TO CST-017 CST-020 FORECAST
      *>================================================================
      *> REQ: CST-014, CST-015, CST-016, CST-017, CST-020   AC: AC-C5
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  JC-BODY.
               10  JC-JOB-REF          PIC X(20).
               10  JC-PARENT-JOB       PIC X(20).
               10  JC-JOB-LEVEL        PIC 9(1).
               10  JC-DESCRIPTION      PIC X(150).
               10  JC-PURPOSE          PIC X(12).
               10  JC-CUSTOMER         PIC X(20).
               10  JC-SALES-ORDER      PIC X(30).
               10  JC-COST-CENTRE      PIC X(20).
               10  JC-OWNER-USER       PIC X(20).
               10  JC-START-DATE       PIC 9(8).
               10  JC-TARGET-DATE      PIC 9(8).
               10  JC-INCLUDE-SUBJOBS  PIC X.
                   88  JC-ROLL-UP-SUBS           VALUE "Y".
      *>       ---- budget ----
               10  JC-BUDGET-VERSION   PIC S9(4).
               10  JC-BUDGET-TOTAL     PIC S9(13)V9(2).
               10  JC-CHANGE-REASON    PIC X(200).
               10  JC-ORIGINAL-BUDGET  PIC S9(13)V9(2).
               10  JC-APPROVED-BUDGET  PIC S9(13)V9(2).
      *>       ---- a cost transaction ----
               10  JC-ELEMENT-CODE     PIC X(10).
               10  JC-SOURCE-MODULE    PIC X(6).
               10  JC-DOC-TYPE         PIC X(20).
               10  JC-DOC-REF          PIC X(40).
               10  JC-ITEM-CODE        PIC X(40).
               10  JC-WORK-CENTRE      PIC X(20).
               10  JC-QUANTITY         PIC S9(13)V9(6).
               10  JC-HOURS            PIC S9(11)V9(6).
               10  JC-RATE             PIC S9(11)V9(6).
               10  JC-CURRENCY         PIC X(3).
               10  JC-AMOUNT           PIC S9(13)V9(2).
               10  JC-AMOUNT-GBP       PIC S9(13)V9(2).
               10  JC-COST-NATURE      PIC X(10).
                   88  JC-IS-ACTUAL              VALUE "ACTUAL".
                   88  JC-IS-ACCRUED             VALUE "ACCRUED".
                   88  JC-IS-COMMITTED           VALUE "COMMITTED".
                   88  JC-IS-ESTIMATE            VALUE "ESTIMATE".
               10  JC-RECOGNITION-DT   PIC 9(8).
               10  JC-IS-APPROVED      PIC X.
               10  JC-NARRATIVE        PIC X(200).
               10  JC-JOB-COST-ID      PIC S9(15).
               10  JC-REVERSE-OF-ID    PIC S9(15).
               10  JC-COMMIT           PIC X.
      *>       ---- forecast and enquiry results ----
               10  JC-ACTUAL-GBP       PIC S9(13)V9(2).
               10  JC-ACCRUED-GBP      PIC S9(13)V9(2).
               10  JC-COMMITTED-GBP    PIC S9(13)V9(2).
               10  JC-ETC-GBP          PIC S9(13)V9(2).
               10  JC-EAC-GBP          PIC S9(13)V9(2).
               10  JC-FCST-VAR-GBP     PIC S9(13)V9(2).
               10  JC-FCST-ADVERSE     PIC X.
               10  JC-REVENUE-GBP      PIC S9(13)V9(2).
               10  JC-WIP-GBP          PIC S9(13)V9(2).
               10  JC-SUBJOB-COUNT     PIC S9(4).
               10  JC-TXN-COUNT        PIC S9(9).
               10  JC-OPER-STATUS      PIC X(14).
               10  JC-FIN-STATUS       PIC X(14).
               10  JC-LATE-COST-GBP    PIC S9(13)V9(2).
               10  JC-DISPOSITION      PIC X(60).
               10  JC-WARNING          PIC X(80).
      *>       ---- element window ----
               10  JC-FROM-LINE        PIC S9(9).
               10  JC-LINE-CNT         PIC S9(4).
               10  JC-MORE-LINES       PIC X.
               10  JC-LINES OCCURS 20 TIMES.
                   15  JC-L-KEY        PIC X(20).
                   15  JC-L-DESC       PIC X(60).
                   15  JC-L-ELEMENT    PIC X(10).
                   15  JC-L-BUDGET     PIC S9(13)V9(2).
                   15  JC-L-ACTUAL     PIC S9(13)V9(2).
                   15  JC-L-COMMITTED  PIC S9(13)V9(2).
                   15  JC-L-ETC        PIC S9(13)V9(2).
                   15  JC-L-EAC        PIC S9(13)V9(2).
                   15  JC-L-VARIANCE   PIC S9(13)V9(2).
                   15  JC-L-ADVERSE    PIC X.
