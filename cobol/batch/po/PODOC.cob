      *>================================================================
      *> PROGRAM-ID : PODOC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : PURCHASE ORDER DOCUMENT PRINT (PO-024).
      *>              THE PAPER THAT GOES TO THE SUPPLIER, PLUS THE
      *>              STATUS APPENDIX THAT MAKES A CLOSED ORDER STILL
      *>              EXPLAINABLE: OUTSTANDING RECEIPTS, COMMITMENT
      *>              VALUE, GRNI, RETURNS, LANDED COST AND EVERY
      *>              AMENDMENT.
      *>              WHAT IS PRINTED IS THE APPROVED VERSION.  THE
      *>              CONTENT HASH, THE APPROVER AND THE APPROVAL
      *>              TIMESTAMP ARE PRINTED ON THE DOCUMENT, AND IF AN
      *>              AMENDMENT IS WAITING FOR RE-APPROVAL THE
      *>              DOCUMENT SAYS SO INSTEAD OF QUIETLY PRINTING
      *>              THE UNAPPROVED TEXT.
      *>              A REPRINT IS NEVER PASSED OFF AS AN ORIGINAL.
      *>              PRIOR SUCCESSFUL RUNS FOR THE SAME ORDER ARE
      *>              COUNTED IN core.JobRun AND THE DOCUMENT IS
      *>              MARKED "REPRINT n" WITH THE DATE OF THE FIRST
      *>              ISSUE.
      *> RUN AS     : PODOC CO=STC ORDER=PO0000001 [USER=BUYER1]
      *> OUTPUT     : run\spool\PODOC-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-05-30 RJH  CR0530 ORDER PRINT
      *> 2001-02-19 KMP  CR0902 REPRINT MARKING AFTER TWO SUPPLIERS
      *>                 SHIPPED THE SAME ORDER TWICE
      *> 2009-07-01 DJM  CR1455 APPROVED VERSION BLOCK
      *> 2026-10-06 STERP PO-024
      *>================================================================
      *> REQ: PO-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PODOC.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 30/05/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-PODOC.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY STDATES.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  PO-ROW.
           05  PO-COL OCCURS 30 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-ORDER                PIC X(30) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-WORK-DATE            PIC 9(8) VALUE 0.
           05  WS-PRIOR-PRINTS         PIC S9(5) VALUE 0.
           05  WS-FIRST-ISSUE          PIC X(8) VALUE SPACES.
           05  WS-NUM-ED               PIC Z(4)9.
           05  WS-LIKE                 PIC X(60) VALUE SPACES.
           05  WS-LINE-COUNT           PIC S9(5) VALUE 0.
           05  WS-EVENT-COUNT          PIC S9(5) VALUE 0.
       01  WS-HDR.
           05  H-SUPP-CODE             PIC X(20) VALUE SPACES.
           05  H-SUPP-NAME             PIC X(60) VALUE SPACES.
           05  H-ORDER-TYPE            PIC X(12) VALUE SPACES.
           05  H-ORDER-DATE            PIC X(10) VALUE SPACES.
           05  H-BUYER                 PIC X(20) VALUE SPACES.
           05  H-CURRENCY              PIC X(3) VALUE SPACES.
           05  H-TERMS                 PIC X(60) VALUE SPACES.
           05  H-INCOTERMS             PIC X(40) VALUE SPACES.
           05  H-DELIVERY              PIC X(80) VALUE SPACES.
           05  H-TCS-VERSION           PIC X(20) VALUE SPACES.
           05  H-SUPP-REF              PIC X(40) VALUE SPACES.
           05  H-STATUS                PIC X(14) VALUE SPACES.
           05  H-DELIV-STATUS          PIC X(14) VALUE SPACES.
           05  H-MATCH-STATUS          PIC X(14) VALUE SPACES.
           05  H-APPROVED-BY           PIC X(20) VALUE SPACES.
           05  H-APPROVED-AT           PIC X(10) VALUE SPACES.
           05  H-CONTENT-HASH          PIC X(64) VALUE SPACES.
           05  H-APPROVAL-ID           PIC X(20) VALUE SPACES.
           05  H-NET                   PIC S9(13)V99 VALUE 0.
           05  H-VAT                   PIC S9(13)V99 VALUE 0.
           05  H-GROSS                 PIC S9(13)V99 VALUE 0.
           05  H-COMMIT                PIC S9(13)V99 VALUE 0.
           05  H-PENDING-AMEND         PIC S9(5) VALUE 0.
           05  H-ISSUE-MARK            PIC X(60) VALUE SPACES.
           05  H-VERSION-NOTE          PIC X(80) VALUE SPACES.
       01  WS-SECT.
           05  S-TEXT                  PIC X(80) VALUE SPACES.
       01  WS-LINE.
           05  L-NUM                   PIC 9(4) VALUE 0.
           05  L-ITEM                  PIC X(20) VALUE SPACES.
           05  L-DESC                  PIC X(40) VALUE SPACES.
           05  L-ORDERED               PIC S9(11)V9(3) VALUE 0.
           05  L-RECEIVED              PIC S9(11)V9(3) VALUE 0.
           05  L-OUTSTANDING           PIC S9(11)V9(3) VALUE 0.
           05  L-PRICE                 PIC S9(9)V9(4) VALUE 0.
           05  L-NET                   PIC S9(11)V99 VALUE 0.
           05  L-VAT-CODE              PIC X(6) VALUE SPACES.
           05  L-PROMISE               PIC X(10) VALUE SPACES.
           05  L-STATUS                PIC X(12) VALUE SPACES.
           05  L-ACCOUNT               PIC X(12) VALUE SPACES.
       01  WS-EVT.
           05  E-TYPE                  PIC X(12) VALUE SPACES.
           05  E-REF                   PIC X(20) VALUE SPACES.
           05  E-DATE                  PIC X(10) VALUE SPACES.
           05  E-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  E-VALUE                 PIC S9(11)V99 VALUE 0.
           05  E-NOTE                  PIC X(58) VALUE SPACES.
       01  WS-DATE-ED.
           05  WS-ED-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-YYYY              PIC 9(4) VALUE 0.
       REPORT SECTION.
       RD  R-PODOC
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-DOCHDR TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(60) SOURCE H-ISSUE-MARK.
               10  COLUMN 70   PIC X(60) SOURCE H-VERSION-NOTE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Supplier  : ".
               10  COLUMN 13   PIC X(20) SOURCE H-SUPP-CODE.
               10  COLUMN 34   PIC X(50) SOURCE H-SUPP-NAME.
               10  COLUMN 90   PIC X(12) VALUE "Order type: ".
               10  COLUMN 102  PIC X(12) SOURCE H-ORDER-TYPE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Order date: ".
               10  COLUMN 13   PIC X(10) SOURCE H-ORDER-DATE.
               10  COLUMN 34   PIC X(8) VALUE "Buyer:  ".
               10  COLUMN 42   PIC X(20) SOURCE H-BUYER.
               10  COLUMN 90   PIC X(12) VALUE "Currency  : ".
               10  COLUMN 102  PIC X(3) SOURCE H-CURRENCY.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Terms     : ".
               10  COLUMN 13   PIC X(60) SOURCE H-TERMS.
               10  COLUMN 90   PIC X(12) VALUE "Supp ref  : ".
               10  COLUMN 102  PIC X(30) SOURCE H-SUPP-REF.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Incoterms : ".
               10  COLUMN 13   PIC X(40) SOURCE H-INCOTERMS.
               10  COLUMN 90   PIC X(12) VALUE "T and Cs  : ".
               10  COLUMN 102  PIC X(20) SOURCE H-TCS-VERSION.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Deliver to: ".
               10  COLUMN 13   PIC X(80) SOURCE H-DELIVERY.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Approved  : ".
               10  COLUMN 13   PIC X(20) SOURCE H-APPROVED-BY.
               10  COLUMN 34   PIC X(10) SOURCE H-APPROVED-AT.
               10  COLUMN 46   PIC X(12) VALUE "Approval  : ".
               10  COLUMN 58   PIC X(20) SOURCE H-APPROVAL-ID.
               10  COLUMN 90   PIC X(12) VALUE "Status    : ".
               10  COLUMN 102  PIC X(14) SOURCE H-STATUS.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Version   : ".
               10  COLUMN 13   PIC X(64) SOURCE H-CONTENT-HASH.
               10  COLUMN 90   PIC X(12) VALUE "Delivery  : ".
               10  COLUMN 102  PIC X(14) SOURCE H-DELIV-STATUS.
       01  DE-SECT TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(80) SOURCE S-TEXT.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC ZZZ9 SOURCE L-NUM.
               10  COLUMN 6    PIC X(20) SOURCE L-ITEM.
               10  COLUMN 27   PIC X(40) SOURCE L-DESC.
               10  COLUMN 68   PIC Z(6)9.999 SOURCE L-ORDERED.
               10  COLUMN 79   PIC Z(6)9.999 SOURCE L-RECEIVED.
               10  COLUMN 90   PIC Z(6)9.999 SOURCE L-OUTSTANDING.
               10  COLUMN 101  PIC Z(5)9.9999 SOURCE L-PRICE.
               10  COLUMN 113  PIC Z(9)9.99- SOURCE L-NET.
               10  COLUMN 127  PIC X(6) SOURCE L-VAT-CODE.
           05  LINE PLUS 1.
               10  COLUMN 6    PIC X(9) VALUE "Promise: ".
               10  COLUMN 15   PIC X(10) SOURCE L-PROMISE.
               10  COLUMN 27   PIC X(9) VALUE "Account: ".
               10  COLUMN 36   PIC X(12) SOURCE L-ACCOUNT.
               10  COLUMN 50   PIC X(8) VALUE "Status: ".
               10  COLUMN 58   PIC X(12) SOURCE L-STATUS.
       01  DE-EVENT TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) SOURCE E-TYPE.
               10  COLUMN 14   PIC X(20) SOURCE E-REF.
               10  COLUMN 35   PIC X(10) SOURCE E-DATE.
               10  COLUMN 46   PIC Z(6)9.999 SOURCE E-QTY.
               10  COLUMN 58   PIC Z(9)9.99- SOURCE E-VALUE.
               10  COLUMN 73   PIC X(58) SOURCE E-NOTE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 60   PIC X(18) VALUE "Net of order    : ".
               10  COLUMN 78   PIC Z(11)9.99- SOURCE H-NET.
           05  LINE PLUS 1.
               10  COLUMN 60   PIC X(18) VALUE "Expected VAT    : ".
               10  COLUMN 78   PIC Z(11)9.99- SOURCE H-VAT.
           05  LINE PLUS 1.
               10  COLUMN 60   PIC X(18) VALUE "Gross of order  : ".
               10  COLUMN 78   PIC Z(11)9.99- SOURCE H-GROSS.
           05  LINE PLUS 1.
               10  COLUMN 60   PIC X(18) VALUE "Commitment held : ".
               10  COLUMN 78   PIC Z(11)9.99- SOURCE H-COMMIT.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(62) VALUE
                   "Goods are supplied on the terms and conditions".
               10  COLUMN 64   PIC X(60) VALUE
                   "named above.  Quote the order number on the".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
                   "delivery note and the invoice.  An invoice that".
               10  COLUMN 64   PIC X(60) VALUE
                   "does not match an approved order is held.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START THRU 0200-EXIT
           PERFORM 0600-READ-ORDER THRU 0600-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0650-COUNT-PRINTS THRU 0650-EXIT
           PERFORM 0300-OPEN-SPOOL THRU 0300-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           GENERATE DE-DOCHDR
           PERFORM 1000-PRINT-LINES THRU 1000-EXIT
           PERFORM 2000-APPENDIX THRU 2000-EXIT
           PERFORM 0400-CLOSE-SPOOL THRU 0400-EXIT.
       0000-FINISH.
           PERFORM 0500-JOB-END THRU 0500-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.
      *>----------------------------------------------------------------
       0100-READ-ARGS SECTION.
       0100-START.
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-USER TO WS-USER
           MOVE "ORDER" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-ORDER
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "BUYER1" TO WS-USER
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-ORDER = SPACES
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1740 ORDER=<order number> is "
                       "required" UPON SYSOUT
           END-IF
           DISPLAY "STERP-I-1741 PODOC order document "
                   FUNCTION TRIM(WS-ORDER) UPON SYSOUT.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "PODOC" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REQ: PO-024 - the approved version.  The hash and approver
      *> are printed, and a material amendment that has not been
      *> re-approved turns the document into a draft: the supplier is
      *> told, rather than being sent changed text under an old
      *> approval.
      *>----------------------------------------------------------------
       0600-READ-ORDER SECTION.
       0600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.SupplierCode, ISNULL((SELECT TOP 1 "
                  "s.LegalName FROM ap.Supplier s WHERE "
                  "s.CompanyCode = o.CompanyCode AND "
                  "s.SupplierCode = o.SupplierCode),''), "
                  "o.OrderType, CONVERT(char(8),o.OrderDate,112), "
                  "ISNULL(o.BuyerUser,''), o.CurrencyCode, "
                  "ISNULL(o.TermsSnapshot,ISNULL(o.TermsCode,'')), "
                  "ISNULL(o.IncotermsRule,'') + ' ' + "
                  "ISNULL(o.IncotermsNamedPlace,''), "
                  "ISNULL(o.DeliveryAddressSnapshot,''), "
                  "ISNULL(o.TermsAndConditionsVersion,''), "
                  "ISNULL(o.SupplierReference,''), o.Status, "
                  "o.DeliveryStatus, o.MatchStatus, "
                  "ISNULL(o.ApprovedBy,''), ISNULL(CONVERT(char(8),"
                  "o.ApprovedAt,112),''), ISNULL(o.ContentHash,''), "
                  "ISNULL(CAST(o.ApprovalId AS varchar(20)),''), "
                  "CAST(o.NetAmount AS varchar(30)), "
                  "CAST(o.VatAmount AS varchar(30)), "
                  "CAST(o.GrossAmount AS varchar(30)), "
                  "CAST(o.CommitmentGBP AS varchar(30)), "
                  "CAST((SELECT COUNT(*) FROM po.OrderAmendment a "
                  "WHERE a.CompanyCode = o.CompanyCode AND "
                  "a.OrderNumber = o.OrderNumber AND "
                  "a.RequiresReapproval = 1 AND a.Status = 'OPEN') "
                  "AS varchar(10)) FROM po.PurchaseOrder o WHERE "
                  "o.CompanyCode = ? AND o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 40
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-ORDER TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 23 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF NOT SQLIO-OK
               MOVE 16 TO WS-RC
               DISPLAY "STERP-E-1742 order " FUNCTION TRIM(WS-ORDER)
                       " not found in " FUNCTION TRIM(WS-CO)
                   UPON SYSOUT
               GO TO 0600-EXIT
           END-IF
           MOVE PO-COL(1) TO H-SUPP-CODE
           MOVE PO-COL(2) TO H-SUPP-NAME
           MOVE PO-COL(3) TO H-ORDER-TYPE
           MOVE PO-COL(4)(1:8) TO WS-WORK-DATE
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO H-ORDER-DATE
           MOVE PO-COL(5) TO H-BUYER
           MOVE PO-COL(6) TO H-CURRENCY
           MOVE PO-COL(7) TO H-TERMS
           MOVE PO-COL(8) TO H-INCOTERMS
           MOVE PO-COL(9) TO H-DELIVERY
           MOVE PO-COL(10) TO H-TCS-VERSION
           MOVE PO-COL(11) TO H-SUPP-REF
           MOVE PO-COL(12) TO H-STATUS
           MOVE PO-COL(13) TO H-DELIV-STATUS
           MOVE PO-COL(14) TO H-MATCH-STATUS
           MOVE PO-COL(15) TO H-APPROVED-BY
           MOVE 0 TO WS-WORK-DATE
           IF PO-COL(16)(1:8) NOT = SPACES
               MOVE PO-COL(16)(1:8) TO WS-WORK-DATE
           END-IF
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO H-APPROVED-AT
           MOVE PO-COL(17) TO H-CONTENT-HASH
           MOVE PO-COL(18) TO H-APPROVAL-ID
           COMPUTE H-NET = FUNCTION NUMVAL(PO-COL(19))
           COMPUTE H-VAT = FUNCTION NUMVAL(PO-COL(20))
           COMPUTE H-GROSS = FUNCTION NUMVAL(PO-COL(21))
           COMPUTE H-COMMIT = FUNCTION NUMVAL(PO-COL(22))
           COMPUTE H-PENDING-AMEND = FUNCTION NUMVAL(PO-COL(23))
           IF H-APPROVED-BY = SPACES
               MOVE "NOT APPROVED - this is a draft, not an order"
                   TO H-VERSION-NOTE
               MOVE 4 TO WS-RC
           ELSE
               IF H-PENDING-AMEND > 0
                   MOVE "AMENDED SINCE APPROVAL - re-approval due"
                       TO H-VERSION-NOTE
                   MOVE 4 TO WS-RC
               ELSE
                   MOVE "Approved version as shown by the hash"
                       TO H-VERSION-NOTE
               END-IF
           END-IF
           IF H-CONTENT-HASH = SPACES
               MOVE "(no content hash held)" TO H-CONTENT-HASH
           END-IF.
       0600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A reprint must be distinguishable from the original issue
      *> (CR0902).  Successful earlier runs of PODOC for this order
      *> are counted in core.JobRun; the current run is still RUNNING
      *> and is therefore not counted.
      *>----------------------------------------------------------------
       0650-COUNT-PRINTS SECTION.
       0650-START.
           MOVE SPACES TO WS-LIKE
           STRING "%ORDER=" FUNCTION TRIM(WS-ORDER) "%"
               DELIMITED BY SIZE INTO WS-LIKE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(10)), "
                  "ISNULL(CONVERT(char(8),MIN(StartedAt),112),'') "
                  "FROM core.JobRun WHERE JobName = 'PODOC' AND "
                  "CompanyCode = ? AND Arguments LIKE ? AND "
                  "Status IN ('OK','WARNING')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-LIKE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-OK
               COMPUTE WS-PRIOR-PRINTS = FUNCTION NUMVAL(PO-COL(1))
               MOVE PO-COL(2)(1:8) TO WS-FIRST-ISSUE
           END-IF
           MOVE SPACES TO H-ISSUE-MARK
           IF WS-PRIOR-PRINTS = 0
               STRING "ORIGINAL ISSUE - order "
                      FUNCTION TRIM(WS-ORDER)
                   DELIMITED BY SIZE INTO H-ISSUE-MARK
           ELSE
               MOVE WS-PRIOR-PRINTS TO WS-NUM-ED
               MOVE 0 TO WS-WORK-DATE
               IF WS-FIRST-ISSUE NOT = SPACES
                   MOVE WS-FIRST-ISSUE TO WS-WORK-DATE
               END-IF
               PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
               STRING "*** REPRINT No " FUNCTION TRIM(WS-NUM-ED)
                      " - first issued " WS-DATE-ED " ***"
                   DELIMITED BY SIZE INTO H-ISSUE-MARK
           END-IF.
       0650-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "PODOC" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1743 cannot build the spool path"
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1744 cannot open " WS-SPOOL-NAME
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-PODOC.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-024" TO RPT-REPORT-ID
           MOVE SPACES TO RPT-TITLE
           STRING "Purchase order " FUNCTION TRIM(WS-ORDER)
               DELIMITED BY SIZE INTO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Supplier " FUNCTION TRIM(H-SUPP-CODE) " - "
                  FUNCTION TRIM(H-SUPP-NAME)
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Printed " WS-NOW(7:2) "/" WS-NOW(5:2) "/"
                  WS-NOW(1:4)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           PERFORM 0360-READ-COMPANY THRU 0360-EXIT.
       0350-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0360-READ-COMPANY SECTION.
       0360-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-OK
               MOVE PO-COL(1) TO RPT-COMPANY-NAME
               MOVE PO-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-PRINT-LINES SECTION.
       1000-START.
           MOVE SPACES TO S-TEXT
           STRING "ORDER LINES   Line / item / description / "
                  "ordered / received / outstanding / price / net"
               DELIMITED BY SIZE INTO S-TEXT
           GENERATE DE-SECT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT l.LineNum, ISNULL(l.ItemCode,''), "
                  "l.Description, CAST(l.OrderedQty AS "
                  "varchar(30)), CAST(l.ReceivedQty AS "
                  "varchar(30)), CAST(l.OrderedQty - "
                  "l.CancelledQty - l.ReceivedQty AS varchar(30)), "
                  "CAST(l.UnitPrice AS varchar(30)), "
                  "CAST(l.NetAmount AS varchar(30)), "
                  "ISNULL(l.VatCode,''), ISNULL(CONVERT(char(8),"
                  "(SELECT MIN(s.CurrentConfirmedDate) FROM "
                  "po.PurchaseSchedule s WHERE "
                  "s.PurchaseOrderLineId = l.PurchaseOrderLineId),"
                  "112),''), l.Status, ISNULL(l.AccountCode,'') "
                  "FROM po.PurchaseOrderLine l WHERE "
                  "l.CompanyCode = ? AND l.OrderNumber = ? "
                  "ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ORDER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 12 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1745 " FUNCTION TRIM(SQLIO-ERRMSG)
                   UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA PO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   COMPUTE L-NUM = FUNCTION NUMVAL(PO-COL(1))
                   MOVE PO-COL(2) TO L-ITEM
                   MOVE PO-COL(3) TO L-DESC
                   COMPUTE L-ORDERED = FUNCTION NUMVAL(PO-COL(4))
                   COMPUTE L-RECEIVED = FUNCTION NUMVAL(PO-COL(5))
                   COMPUTE L-OUTSTANDING =
                       FUNCTION NUMVAL(PO-COL(6))
                   COMPUTE L-PRICE = FUNCTION NUMVAL(PO-COL(7))
                   COMPUTE L-NET = FUNCTION NUMVAL(PO-COL(8))
                   MOVE PO-COL(9) TO L-VAT-CODE
                   MOVE 0 TO WS-WORK-DATE
                   IF PO-COL(10)(1:8) NOT = SPACES
                       MOVE PO-COL(10)(1:8) TO WS-WORK-DATE
                   END-IF
                   PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
                   MOVE WS-DATE-ED TO L-PROMISE
                   MOVE PO-COL(11) TO L-STATUS
                   MOVE PO-COL(12) TO L-ACCOUNT
                   ADD 1 TO WS-LINE-COUNT
                   GENERATE DE-LINE
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REQ: PO-024 - the appendix.  Receipts, returns, landed cost
      *> and amendments are read in one union so that a CLOSED order
      *> still explains itself: what arrived, what went back, what it
      *> finally cost and who changed what.
      *>----------------------------------------------------------------
       2000-APPENDIX SECTION.
       2000-START.
           MOVE SPACES TO S-TEXT
           STRING "ORDER HISTORY   receipts, returns, landed cost "
                  "and amendments"
               DELIMITED BY SIZE INTO S-TEXT
           GENERATE DE-SECT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT 'RECEIPT', r.ReceiptRef, "
                  "CONVERT(char(8),r.PhysicalDate,112), "
                  "CAST(rl.ReceivedQty AS varchar(30)), "
                  "CAST(rl.ProvisionalValueGBP AS varchar(30)), "
                  "LEFT('line ' + CAST(rl.PurchaseOrderLine AS "
                  "varchar(6)) + ' ' + rl.QualityStatus + ' note '"
                  " + ISNULL(r.DeliveryNoteRef,'-'),58) FROM "
                  "po.ReceiptLine rl JOIN po.Receipt r ON "
                  "r.ReceiptId = rl.ReceiptId WHERE "
                  "r.CompanyCode = ? AND rl.PurchaseOrderNumber = ?"
                  " UNION ALL SELECT 'RETURN', x.ReturnRef, "
                  "CONVERT(char(8),x.ReturnDate,112), "
                  "CAST(ISNULL((SELECT SUM(xl.ReturnedQty) FROM "
                  "po.SupplierReturnLine xl WHERE "
                  "xl.SupplierReturnId = x.SupplierReturnId),0) AS "
                  "varchar(30)), "
                  "CAST(x.ExpectedCreditGBP AS varchar(30)), "
                  "LEFT(x.Status + ' ' + ISNULL(x.ReasonCode,'') + "
                  "' credit ' + ISNULL(x.CreditNoteRef,'awaited'),"
                  "58) FROM po.SupplierReturn x WHERE "
                  "x.CompanyCode = ? AND "
                  "ISNULL(x.PurchaseOrderNumber,'') = ? "
                  "UNION ALL SELECT 'IMPORT', ie.ImportEntryRef, "
                  "CONVERT(char(8),ie.EntryDate,112), 0, "
                  "CAST(ie.DutyAmountGBP AS varchar(30)), "
                  "LEFT('duty capitalised, import VAT ' + "
                  "ie.ImportVatMethod + ' ' + ie.StatementStatus,"
                  "58) FROM po.ImportEntry ie WHERE "
                  "ie.CompanyCode = ? AND "
                  "ISNULL(ie.PurchaseOrderNumber,'') = ? "
                  "UNION ALL SELECT 'LANDEDCOST', lc.ChargeRef, "
                  "ISNULL(CONVERT(char(8),lc.AllocationDate,112),"
                  "CONVERT(char(8),lc.CreatedAt,112)), 0, "
                  "CAST(ISNULL(lc.ActualAmountGBP,"
                  "lc.EstimatedAmountGBP) AS varchar(30)), "
                  "LEFT(lc.Category + ' ' + lc.Status + ' basis ' "
                  "+ lc.AllocationBasis,58) FROM cst.LandedCost lc "
                  "WHERE lc.CompanyCode = ? AND EXISTS (SELECT 1 "
                  "FROM cst.LandedCostAllocation la JOIN "
                  "po.Receipt rr ON rr.ReceiptRef = la.ReceiptRef "
                  "AND rr.CompanyCode = lc.CompanyCode WHERE "
                  "la.LandedCostId = lc.LandedCostId AND "
                  "ISNULL(rr.PurchaseOrderNumber,'') = ?) "
                  "UNION ALL SELECT 'AMENDMENT', 'AMD ' + "
                  "CAST(a.AmendmentNo AS varchar(6)), "
                  "CONVERT(char(8),a.AmendedAt,112), 0, 0, "
                  "LEFT(a.FieldName + ' ' + "
                  "ISNULL(a.OriginalValue,'') + ' -> ' + "
                  "ISNULL(a.RevisedValue,''),58) FROM "
                  "po.OrderAmendment a WHERE a.CompanyCode = ? AND "
                  "a.OrderNumber = ? ORDER BY 3, 1, 2"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 40
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-ORDER TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE WS-ORDER TO SQLIO-PARM-VAL(4)
           MOVE WS-CO TO SQLIO-PARM-VAL(5)
           MOVE WS-ORDER TO SQLIO-PARM-VAL(6)
           MOVE WS-CO TO SQLIO-PARM-VAL(7)
           MOVE WS-ORDER TO SQLIO-PARM-VAL(8)
           MOVE WS-CO TO SQLIO-PARM-VAL(9)
           MOVE WS-ORDER TO SQLIO-PARM-VAL(10)
           MOVE 10 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-ERROR
               DISPLAY "STERP-W-1746 " FUNCTION TRIM(SQLIO-ERRMSG)
                   UPON SYSOUT
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               GO TO 2000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA PO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   MOVE PO-COL(1) TO E-TYPE
                   MOVE PO-COL(2) TO E-REF
                   MOVE 0 TO WS-WORK-DATE
                   IF PO-COL(3)(1:8) NOT = SPACES
                       MOVE PO-COL(3)(1:8) TO WS-WORK-DATE
                   END-IF
                   PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
                   MOVE WS-DATE-ED TO E-DATE
                   COMPUTE E-QTY = FUNCTION NUMVAL(PO-COL(4))
                   COMPUTE E-VALUE = FUNCTION NUMVAL(PO-COL(5))
                   MOVE PO-COL(6) TO E-NOTE
                   ADD 1 TO WS-EVENT-COUNT
                   GENERATE DE-EVENT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF WS-EVENT-COUNT = 0
               MOVE "NONE" TO E-TYPE
               MOVE SPACES TO E-REF E-DATE
               MOVE 0 TO E-QTY E-VALUE
               MOVE "Nothing has happened to this order yet"
                   TO E-NOTE
               GENERATE DE-EVENT
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1450-FORMAT-DATE SECTION.
       1450-START.
           IF WS-WORK-DATE = 0
               MOVE 0 TO WS-ED-DD WS-ED-MM WS-ED-YYYY
           ELSE
               MOVE WS-WORK-DATE TO STD-DATE-N
               MOVE STD-DD TO WS-ED-DD
               MOVE STD-MM TO WS-ED-MM
               MOVE STD-YYYY TO WS-ED-YYYY
           END-IF.
       1450-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-PODOC
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1747 " WS-LINE-COUNT " order line(s), "
                   WS-EVENT-COUNT " history event(s)" UPON SYSOUT
           IF WS-PRIOR-PRINTS > 0
               DISPLAY "STERP-W-1748 reprint number "
                       WS-PRIOR-PRINTS " of this order"
                   UPON SYSOUT
           END-IF
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME)
               UPON SYSOUT.
       0400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "PODOC" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "OK" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-LINE-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
