      *>================================================================
      *> PROGRAM-ID : VATAUDIT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : VAT DETAIL AND MAKING TAX DIGITAL AUDIT TRAIL
      *>              FOR ONE VAT PERIOD.  MODE=DETAIL LISTS EVERY
      *>              fin.VatTransaction OF THE PERIOD UNDER THE VAT
      *>              RETURN BOX IT FEEDS, WITH TAX POINT, MECHANISM,
      *>              NET, VAT, THE RECOVERABLE, LATE ENTRY AND
      *>              ADJUSTMENT FLAGS AND A SUBTOTAL PER BOX.
      *>              MODE=MTD LISTS THE NINE RETURN BOXES HELD ON
      *>              fin.VatReturnBox WITH THEIR CONTRIBUTING
      *>              COUNTS FROM fin.VatReturnDetail, THE ERROR
      *>              CORRECTIONS OF fin.VatErrorCorrection, THE
      *>              POSTPONED IMPORT VAT (MECHANISM PIV) AND THE
      *>              SUBMISSION REFERENCE AND HMRC RESPONSE HELD ON
      *>              fin.VatReturn.  EITHER WAY THE VAT THE PERIOD
      *>              DECLARES IS PROVED AGAINST THE MOVEMENT ON THE
      *>              VAT CONTROL ACCOUNTS THE SAME WAY RECONCTL
      *>              PROVES A CONTROL: THE DIFFERENCE IS PRINTED,
      *>              NEVER NETTED AWAY, AND A DIFFERENCE OUTSIDE
      *>              TOLERANCE RETURNS 04.
      *> RUN AS     : VATAUDIT CO=STC VATPERIOD=26A3 MODE=DETAIL
      *>              USER=FINCTRL [TOL=0.01]
      *> OUTPUT     : run\spool\VATAUDIT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, PARMGET, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-07-26 DJM  CR0191 ORIGINAL VAT DETAIL LISTING
      *> 1996-02-14 RJH  CR0502 BOX SUBTOTALS AND THE CONTROL PROOF
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2010-04-06 KMP  CR2104 REVERSE CHARGE AND IMPORT MECHANISMS
      *> 2026-10-06 STERP RPT-007 / RPT-017 MTD AUDIT TRAIL
      *>================================================================
      *> REQ: RPT-007, RPT-017, XMD-005, XMD-006, XMD-010, COM-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VATAUDIT.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 26/07/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC XB-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS XB-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-VATAUDIT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY XMDBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
      *>   the nine boxes of the return, in the wording HMRC uses.
      *>   fin.VatReturnBox carries its own caption; this table is
      *>   what the listing falls back on when it does not
       01  WS-BOX-TABLE.
           05  FILLER PIC X(48) VALUE
               "VAT due on sales and other outputs".
           05  FILLER PIC X(48) VALUE
               "VAT due on acquisitions from EU to N Ireland".
           05  FILLER PIC X(48) VALUE
               "Total VAT due - boxes 1 and 2".
           05  FILLER PIC X(48) VALUE
               "VAT reclaimed on purchases and other inputs".
           05  FILLER PIC X(48) VALUE
               "Net VAT to pay to HMRC or to reclaim".
           05  FILLER PIC X(48) VALUE
               "Total value of sales, excluding VAT".
           05  FILLER PIC X(48) VALUE
               "Total value of purchases, excluding VAT".
           05  FILLER PIC X(48) VALUE
               "Total value of dispatches to N Ireland from EU".
           05  FILLER PIC X(48) VALUE
               "Total value of acquisitions from EU to N Ireland".
       01  WS-BOX-TAB REDEFINES WS-BOX-TABLE.
           05  WS-BOX-CAPTION OCCURS 9 TIMES PIC X(48).
       01  WS-HDR-BOX.
           05  WS-HB-VALUE OCCURS 9 TIMES PIC S9(13)V99.
       01  WS-SCOPE.
           05  S-PERIOD-KEY            PIC X(10) VALUE SPACES.
           05  S-START                 PIC X(10) VALUE SPACES.
           05  S-END                   PIC X(10) VALUE SPACES.
           05  S-START8                PIC X(8) VALUE SPACES.
           05  S-END8                  PIC X(8) VALUE SPACES.
           05  S-DUE                   PIC X(10) VALUE SPACES.
           05  S-PAYDUE                PIC X(10) VALUE SPACES.
           05  S-STATUS                PIC X(12) VALUE SPACES.
           05  S-FREQ                  PIC X(12) VALUE SPACES.
           05  S-REGNO                 PIC X(20) VALUE SPACES.
           05  S-SCHEME                PIC X(12) VALUE SPACES.
           05  S-MODE                  PIC X(10) VALUE SPACES.
           05  S-TOL                   PIC X(12) VALUE SPACES.
           05  S-PERIOD-OK             PIC X VALUE "N".
       01  WS-DET.
           05  D-BOX                   PIC X(3) VALUE SPACES.
           05  D-BOX-NO                PIC S9(4) VALUE 0.
           05  D-PREV-BOX              PIC S9(4) VALUE -1.
           05  D-CODE                  PIC X(8) VALUE SPACES.
           05  D-TAXPOINT              PIC X(10) VALUE SPACES.
           05  D-MECH                  PIC X(8) VALUE SPACES.
           05  D-DOC                   PIC X(18) VALUE SPACES.
           05  D-PARTY                 PIC X(8) VALUE SPACES.
           05  D-NET                   PIC S9(13)V99 VALUE 0.
           05  D-VAT                   PIC S9(13)V99 VALUE 0.
           05  D-REC-FLAG              PIC X(10) VALUE SPACES.
           05  D-LATE-FLAG             PIC X(5) VALUE SPACES.
           05  D-ADJ-FLAG              PIC X(14) VALUE SPACES.
       01  WS-BOXSUB.
           05  B-LABEL                 PIC X(22) VALUE SPACES.
           05  B-NET                   PIC S9(13)V99 VALUE 0.
           05  B-VAT                   PIC S9(13)V99 VALUE 0.
           05  B-CNT                   PIC S9(7) VALUE 0.
       01  WS-MTD.
           05  M-BOX                   PIC X(3) VALUE SPACES.
           05  M-BOX-NO                PIC S9(4) VALUE 0.
           05  M-CAPTION               PIC X(48) VALUE SPACES.
           05  M-VALUE                 PIC S9(13)V99 VALUE 0.
           05  M-CNT                   PIC S9(7) VALUE 0.
           05  M-LATE                  PIC S9(7) VALUE 0.
           05  M-NOTE                  PIC X(60) VALUE SPACES.
           05  M-SOURCE                PIC X(14) VALUE SPACES.
       01  WS-RET.
           05  R-ID                    PIC X(12) VALUE SPACES.
           05  R-ID-NUM                PIC S9(9) VALUE 0.
           05  R-VERSION               PIC X(6) VALUE SPACES.
           05  R-STATUS                PIC X(14) VALUE SPACES.
           05  R-SUBREF                PIC X(40) VALUE SPACES.
           05  R-HMRC                  PIC X(60) VALUE SPACES.
           05  R-PREPARED              PIC X(20) VALUE SPACES.
           05  R-APPROVED              PIC X(20) VALUE SPACES.
           05  R-SUBMITTED             PIC X(10) VALUE SPACES.
           05  R-REGNO                 PIC X(20) VALUE SPACES.
           05  R-FOUND                 PIC X VALUE "N".
       01  WS-ERRC.
           05  E-ORIG                  PIC X(10) VALUE SPACES.
           05  E-FOUND                 PIC X(10) VALUE SPACES.
           05  E-NET                   PIC S9(13)V99 VALUE 0.
           05  E-TURNOVER              PIC S9(13)V99 VALUE 0.
           05  E-METHOD                PIC X(12) VALUE SPACES.
           05  E-STATUS                PIC X(12) VALUE SPACES.
           05  E-DELIB                 PIC X(10) VALUE SPACES.
           05  E-DISC                  PIC X(20) VALUE SPACES.
           05  E-REASON                PIC X(54) VALUE SPACES.
       01  WS-PIV.
           05  P-CODE                  PIC X(8) VALUE SPACES.
           05  P-TAXPOINT              PIC X(10) VALUE SPACES.
           05  P-DOC                   PIC X(18) VALUE SPACES.
           05  P-PARTY                 PIC X(10) VALUE SPACES.
           05  P-NET                   PIC S9(13)V99 VALUE 0.
           05  P-VAT                   PIC S9(13)V99 VALUE 0.
           05  P-REC                   PIC S9(13)V99 VALUE 0.
           05  P-EVIDENCE              PIC X(20) VALUE SPACES.
           05  P-COUNT                 PIC S9(7) VALUE 0.
           05  P-TOT-VAT               PIC S9(13)V99 VALUE 0.
       01  WS-TOT.
           05  T-NET                   PIC S9(13)V99 VALUE 0.
           05  T-VAT                   PIC S9(13)V99 VALUE 0.
           05  T-CNT                   PIC S9(7) VALUE 0.
           05  T-LATE                  PIC S9(7) VALUE 0.
           05  T-ADJ                   PIC S9(7) VALUE 0.
           05  T-BLOCKED               PIC S9(7) VALUE 0.
       01  WS-PROOF.
           05  PF-LEDGER               PIC S9(13)V99 VALUE 0.
           05  PF-GL                   PIC S9(13)V99 VALUE 0.
           05  PF-DIFF                 PIC S9(13)V99 VALUE 0.
           05  PF-BOXES                PIC S9(13)V99 VALUE 0.
           05  PF-BOXDIFF              PIC S9(13)V99 VALUE 0.
           05  PF-ACCTS                PIC X(16) VALUE SPACES.
           05  PF-STATUS               PIC X(14) VALUE SPACES.
           05  PF-BOXSTAT              PIC X(14) VALUE SPACES.
           05  PF-TXNS                 PIC S9(7) VALUE 0.
       01  WS-WORK.
           05  W-BOXES-SEEN            PIC S9(4) VALUE 0.
           05  W-ERRS-SEEN             PIC S9(4) VALUE 0.
           05  W-HAVE-ROW              PIC X VALUE "N".
       REPORT SECTION.
       RD  R-VATAUDIT
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
      *>   the scope block is an ordinary DETAIL group: this compiler
      *>   does not emit CONTROL HEADING FINAL (see tech-debt xmd.md)
       01  DE-SCOPE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(18) VALUE "VAT period key    ".
               10  COLUMN 20   PIC X(10) SOURCE S-PERIOD-KEY.
               10  COLUMN 32   PIC X(6) VALUE "From  ".
               10  COLUMN 38   PIC X(10) SOURCE S-START.
               10  COLUMN 50   PIC X(4) VALUE "To  ".
               10  COLUMN 54   PIC X(10) SOURCE S-END.
               10  COLUMN 66   PIC X(8) VALUE "Status  ".
               10  COLUMN 74   PIC X(12) SOURCE S-STATUS.
               10  COLUMN 88   PIC X(6) VALUE "Mode  ".
               10  COLUMN 94   PIC X(10) SOURCE S-MODE.
               10  COLUMN 106  PIC X(11) VALUE "Frequency  ".
               10  COLUMN 117  PIC X(12) SOURCE S-FREQ.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(18) VALUE "VAT registration  ".
               10  COLUMN 20   PIC X(20) SOURCE S-REGNO.
               10  COLUMN 42   PIC X(8) VALUE "Scheme  ".
               10  COLUMN 50   PIC X(12) SOURCE S-SCHEME.
               10  COLUMN 64   PIC X(13) VALUE "Filing due   ".
               10  COLUMN 77   PIC X(10) SOURCE S-DUE.
               10  COLUMN 89   PIC X(14) VALUE "Payment due   ".
               10  COLUMN 103  PIC X(10) SOURCE S-PAYDUE.
               10  COLUMN 117  PIC X(5) VALUE "Tol  ".
               10  COLUMN 122  PIC X(11) SOURCE S-TOL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
                   "Basis: tax point within the VAT period, sterling".
               10  COLUMN 50   PIC X(60) VALUE
                   "carrying values, boxes proved to the VAT control.".
      *>----------------------------------------------------------------
      *> REQ: RPT-007  the detail listing - one line per VAT
      *> transaction under the box it feeds
      *>----------------------------------------------------------------
       01  DE-DHEAD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(3) VALUE "Box".
               10  COLUMN 5    PIC X(8) VALUE "VAT code".
               10  COLUMN 14   PIC X(10) VALUE "Tax point ".
               10  COLUMN 25   PIC X(9) VALUE "Mechanism".
               10  COLUMN 35   PIC X(8) VALUE "Document".
               10  COLUMN 54   PIC X(5) VALUE "Party".
               10  COLUMN 64   PIC X(16) VALUE "         Net GBP".
               10  COLUMN 83   PIC X(16) VALUE "         VAT GBP".
               10  COLUMN 101  PIC X(10) VALUE "Recovery  ".
               10  COLUMN 112  PIC X(5) VALUE "Late ".
               10  COLUMN 118  PIC X(10) VALUE "Adjustment".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-TXN TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(3) SOURCE D-BOX.
               10  COLUMN 5    PIC X(8) SOURCE D-CODE.
               10  COLUMN 14   PIC X(10) SOURCE D-TAXPOINT.
               10  COLUMN 25   PIC X(8) SOURCE D-MECH.
               10  COLUMN 35   PIC X(18) SOURCE D-DOC.
               10  COLUMN 54   PIC X(8) SOURCE D-PARTY.
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-NET.
               10  COLUMN 81   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-VAT.
               10  COLUMN 101  PIC X(10) SOURCE D-REC-FLAG.
               10  COLUMN 112  PIC X(5) SOURCE D-LATE-FLAG.
               10  COLUMN 118  PIC X(14) SOURCE D-ADJ-FLAG.
       01  DE-BOXSUB TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(22) SOURCE B-LABEL.
               10  COLUMN 29   PIC X(12) VALUE "entries     ".
               10  COLUMN 41   PIC ZZZ,ZZ9 SOURCE B-CNT.
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE B-NET.
               10  COLUMN 81   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE B-VAT.
           05  LINE PLUS 1.
               10  COLUMN 62   PIC X(18) VALUE ALL "-".
               10  COLUMN 81   PIC X(18) VALUE ALL "-".
      *>----------------------------------------------------------------
      *> REQ: RPT-017  the MTD block - nine boxes, their contributing
      *> counts, corrections, postponed import VAT and the evidence
      *>----------------------------------------------------------------
       01  DE-MHEAD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(3) VALUE "Box".
               10  COLUMN 5    PIC X(48) VALUE
                   "Description of the box as filed".
               10  COLUMN 55   PIC X(16) VALUE "       Value GBP".
               10  COLUMN 75   PIC X(13) VALUE "Contributing ".
               10  COLUMN 90   PIC X(13) VALUE "Late entries ".
               10  COLUMN 105  PIC X(26) VALUE "Source of value".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-MBOX TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(3) SOURCE M-BOX.
               10  COLUMN 5    PIC X(48) SOURCE M-CAPTION.
               10  COLUMN 54   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE M-VALUE.
               10  COLUMN 77   PIC ZZZ,ZZ9 SOURCE M-CNT.
               10  COLUMN 92   PIC ZZZ,ZZ9 SOURCE M-LATE.
               10  COLUMN 105  PIC X(26) SOURCE M-NOTE.
       01  DE-SUBHD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(40) VALUE
                   "Submission evidence held for this return".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(18) VALUE "Return identity   ".
               10  COLUMN 21   PIC X(12) SOURCE R-ID.
               10  COLUMN 35   PIC X(8) VALUE "Version ".
               10  COLUMN 43   PIC X(6) SOURCE R-VERSION.
               10  COLUMN 52   PIC X(7) VALUE "Status ".
               10  COLUMN 59   PIC X(14) SOURCE R-STATUS.
               10  COLUMN 75   PIC X(10) VALUE "Prepared  ".
               10  COLUMN 85   PIC X(20) SOURCE R-PREPARED.
               10  COLUMN 107  PIC X(10) VALUE "Approved  ".
               10  COLUMN 117  PIC X(15) SOURCE R-APPROVED.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(18) VALUE "Submission ref    ".
               10  COLUMN 21   PIC X(40) SOURCE R-SUBREF.
               10  COLUMN 63   PIC X(11) VALUE "Submitted  ".
               10  COLUMN 74   PIC X(10) SOURCE R-SUBMITTED.
               10  COLUMN 86   PIC X(14) VALUE "VAT number    ".
               10  COLUMN 100  PIC X(20) SOURCE R-REGNO.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(18) VALUE "HMRC response     ".
               10  COLUMN 21   PIC X(60) SOURCE R-HMRC.
       01  DE-ERRHD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(44) VALUE
                   "Error corrections touching this VAT period".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(16) VALUE "Original period ".
               10  COLUMN 20   PIC X(10) VALUE "Discovered".
               10  COLUMN 33   PIC X(16) VALUE "   Net error GBP".
               10  COLUMN 53   PIC X(16) VALUE "   Box 6 turnov.".
               10  COLUMN 72   PIC X(8) VALUE "Method  ".
               10  COLUMN 82   PIC X(10) VALUE "Deliberate".
               10  COLUMN 94   PIC X(10) VALUE "Status    ".
               10  COLUMN 106  PIC X(14) VALUE "Disclosure ref".
       01  DE-ERR TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(10) SOURCE E-ORIG.
               10  COLUMN 20   PIC X(10) SOURCE E-FOUND.
               10  COLUMN 31   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE E-NET.
               10  COLUMN 51   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE E-TURNOVER.
               10  COLUMN 72   PIC X(8) SOURCE E-METHOD.
               10  COLUMN 82   PIC X(10) SOURCE E-DELIB.
               10  COLUMN 94   PIC X(10) SOURCE E-STATUS.
               10  COLUMN 106  PIC X(20) SOURCE E-DISC.
           05  LINE PLUS 1.
               10  COLUMN 6    PIC X(54) SOURCE E-REASON.
       01  DE-PIVHD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(46) VALUE
                   "Postponed import VAT accounted in this period".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(8) VALUE "VAT code".
               10  COLUMN 13   PIC X(10) VALUE "Tax point ".
               10  COLUMN 25   PIC X(8) VALUE "Document".
               10  COLUMN 45   PIC X(8) VALUE "Supplier".
               10  COLUMN 58   PIC X(16) VALUE "         Net GBP".
               10  COLUMN 77   PIC X(16) VALUE "         VAT GBP".
               10  COLUMN 96   PIC X(16) VALUE " Recoverable GBP".
               10  COLUMN 115  PIC X(14) VALUE "Evidence (C79)".
       01  DE-PIV TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(8) SOURCE P-CODE.
               10  COLUMN 13   PIC X(10) SOURCE P-TAXPOINT.
               10  COLUMN 25   PIC X(18) SOURCE P-DOC.
               10  COLUMN 45   PIC X(10) SOURCE P-PARTY.
               10  COLUMN 56   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE P-NET.
               10  COLUMN 75   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE P-VAT.
               10  COLUMN 94   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE P-REC.
               10  COLUMN 115  PIC X(18) SOURCE P-EVIDENCE.
       01  DE-NONE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(60) SOURCE M-NOTE.
      *>----------------------------------------------------------------
      *> REQ: RPT-007, XMD-005  the proof against the VAT control
      *> account - the difference is printed, never netted away
      *>----------------------------------------------------------------
       01  DE-PROOF TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(44) VALUE
                   "Proof of the period against the VAT control".
               10  COLUMN 46   PIC X(18) VALUE "Control accounts  ".
               10  COLUMN 64   PIC X(16) SOURCE PF-ACCTS.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(50) VALUE
                   "Net VAT declared by the transactions of the period".
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE PF-LEDGER.
               10  COLUMN 84   PIC X(12) VALUE "entries     ".
               10  COLUMN 96   PIC ZZZ,ZZ9 SOURCE PF-TXNS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(50) VALUE
                   "Movement on the VAT control accounts in the period".
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE PF-GL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(50) VALUE
                   "Difference carried as unexplained".
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE PF-DIFF.
               10  COLUMN 84   PIC X(14) SOURCE PF-STATUS.
       01  DE-BOXPRF TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(50) VALUE
                   "Boxes 3 less 4 as held on the return".
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE PF-BOXES.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(50) VALUE
                   "Boxes against the transactions of the period".
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE PF-BOXDIFF.
               10  COLUMN 84   PIC X(14) SOURCE PF-BOXSTAT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Lines listed".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-CNT.
               10  COLUMN 34   PIC X(16) VALUE "Late entries    ".
               10  COLUMN 50   PIC ZZZ,ZZ9 SOURCE T-LATE.
               10  COLUMN 60   PIC X(14) VALUE "Adjustments   ".
               10  COLUMN 74   PIC ZZZ,ZZ9 SOURCE T-ADJ.
               10  COLUMN 84   PIC X(20) VALUE "Blocked recovery    ".
               10  COLUMN 104  PIC ZZZ,ZZ9 SOURCE T-BLOCKED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Period totals".
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-NET.
               10  COLUMN 81   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-VAT.
               10  COLUMN 104  PIC X(20) SOURCE RPT-STATUS-TEXT.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "Keep this listing with the return: it is the".
               10  COLUMN 56   PIC X(62) VALUE
                   "digital link between ledger and figures filed.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-LOAD-PERIOD THRU 0200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "VATAUDIT" TO RPT-REPORT-ID
           IF S-MODE = "MTD"
               MOVE "VAT return and MTD audit trail" TO RPT-TITLE
               MOVE "Boxes 1 to 9, corrections and the evidence"
                   TO RPT-SUBTITLE
           ELSE
               MOVE "VAT detail and return review" TO RPT-TITLE
               MOVE "Every taxable entry under the box it feeds"
                   TO RPT-SUBTITLE
           END-IF
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-VATAUDIT
           GENERATE DE-SCOPE
           IF S-MODE = "MTD"
               PERFORM 2000-MTD-RUN THRU 2000-EXIT
           ELSE
               PERFORM 1000-DETAIL-RUN THRU 1000-EXIT
           END-IF
           PERFORM 3000-PROVE THRU 3000-EXIT
           TERMINATE R-VATAUDIT
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE XB-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
      *> REQ: RPT-007, RPT-017  CO, VATPERIOD, MODE and USER - the
      *> VAT period key is the scope, not a pair of loose dates
      *>----------------------------------------------------------------
       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "VATPERIOD" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           MOVE XB-TEXT(1:10) TO S-PERIOD-KEY
           IF S-PERIOD-KEY = SPACES
               MOVE "PERIOD" TO XB-TEXT
               PERFORM 9550-GET-ARG THRU 9550-EXIT
               MOVE XB-TEXT(1:10) TO S-PERIOD-KEY
           END-IF
           MOVE FUNCTION UPPER-CASE(S-PERIOD-KEY) TO S-PERIOD-KEY
           IF XB-MODE = "PREVIEW" OR XB-MODE = SPACES
               MOVE "DETAIL" TO XB-MODE
           END-IF
           IF XB-MODE NOT = "DETAIL" AND XB-MODE NOT = "MTD"
               DISPLAY "STERP-W-0102 mode " FUNCTION TRIM(XB-MODE)
                       " is not DETAIL or MTD - DETAIL assumed"
               MOVE "DETAIL" TO XB-MODE
           END-IF
           MOVE XB-MODE TO S-MODE
           MOVE "XMD.RECON.TOLERANCE" TO XB-CODE
           MOVE 0.01 TO XB-AMT
           PERFORM 9650-GET-PARM-NUM THRU 9650-EXIT
           MOVE XB-AMT TO XB-TOLERANCE
           MOVE "TOL" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT NOT = SPACES
               COMPUTE XB-TOLERANCE = FUNCTION NUMVAL(XB-TEXT)
           END-IF
           MOVE XB-TOLERANCE TO XB-MON-ED
           MOVE FUNCTION TRIM(XB-MON-ED) TO S-TOL
           IF S-PERIOD-KEY = SPACES
               MOVE 8 TO XB-RC
               DISPLAY "STERP-E-0103 VATPERIOD= is required, for "
                       "example VATPERIOD=26A3"
           END-IF
           DISPLAY "STERP-I-0100 VATAUDIT company "
                   FUNCTION TRIM(XB-CO) " VAT period "
                   FUNCTION TRIM(S-PERIOD-KEY) " mode "
                   FUNCTION TRIM(S-MODE).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-017  the VAT period carries its own dates, due
      *> dates, frequency and status - nothing here is derived twice
      *>----------------------------------------------------------------
       0200-LOAD-PERIOD.
           MOVE "N" TO S-PERIOD-OK
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(p.VatRegNo,''), CONVERT(char(8),"
                  "p.StartDate,112), CONVERT(char(8),p.EndDate,112"
                  "), ISNULL(CONVERT(char(8),p.FilingDueDate,112),"
                  "''), ISNULL(CONVERT(char(8),p.PaymentDueDate,"
                  "112),''), ISNULL(p.Frequency,''), ISNULL("
                  "p.Status,''), ISNULL(p.SubmissionRef,''), "
                  "ISNULL(c.VatScheme,'INVOICE') FROM fin.VatPeriod"
                  " p JOIN core.Company c ON c.CompanyCode = "
                  "p.CompanyCode WHERE p.CompanyCode = ? AND "
                  "p.VatPeriodKey = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE S-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 9 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-0201 " SQLIO-ERRMSG(1:100)
               GO TO 0200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO XB-RC
               DISPLAY "STERP-E-0202 no VAT period "
                       FUNCTION TRIM(S-PERIOD-KEY) " for company "
                       FUNCTION TRIM(XB-CO)
               GO TO 0200-EXIT
           END-IF
           MOVE "Y" TO S-PERIOD-OK
           MOVE BR-COL(1) TO S-REGNO
           MOVE BR-COL(2)(1:8) TO S-START8
           MOVE BR-COL(3)(1:8) TO S-END8
           MOVE BR-COL(6) TO S-FREQ
           MOVE BR-COL(7) TO S-STATUS
           MOVE BR-COL(9) TO S-SCHEME
           MOVE S-START8 TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-START
           MOVE S-END8 TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-END
      *>   the as-at date of the run is the last day of the period:
      *>   job logging and the spool stamp follow the return, not
      *>   the clock of the machine that happens to print it
           MOVE XB-NUM-DATE TO XB-ASAT
           MOVE BR-COL(4)(1:8) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-DUE
           IF XB-NUM-DATE = 0
               MOVE "not set" TO S-DUE
           END-IF
           MOVE BR-COL(5)(1:8) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-PAYDUE
           IF XB-NUM-DATE = 0
               MOVE "not set" TO S-PAYDUE
           END-IF
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "VAT period " FUNCTION TRIM(S-PERIOD-KEY)
                  " to " S-END
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-007  MODE=DETAIL - fin.VatTransaction of the period
      *> in box order, with the tax point, mechanism, net, VAT and
      *> the recoverable, late entry and adjustment flags
      *>----------------------------------------------------------------
       1000-DETAIL-RUN.
           GENERATE DE-DHEAD
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(4),ISNULL(v.VatBox,0)), "
                  "ISNULL(v.VatCode,''), CONVERT(char(8),"
                  "v.TaxPointDate,112), ISNULL(v.Mechanism,"
                  "'NORMAL'), ISNULL(v.Direction,''), ISNULL("
                  "v.DocumentType,'') + ' ' + ISNULL(v.DocumentRef,"
                  "''), ISNULL(v.PartyCode,''), CONVERT(varchar(24)"
                  ",ISNULL(v.NetAmountGBP,0)), CONVERT(varchar(24),"
                  "ISNULL(v.VatAmountGBP,0)), CONVERT(varchar(24),"
                  "ISNULL(v.RecoverableVatGBP,0)), CONVERT(char(1),"
                  "ISNULL(v.IsLateEntry,0)), ISNULL("
                  "v.AdjustmentType,''), CONVERT(char(1),ISNULL("
                  "c.IsRecoverable,0)), CONVERT(varchar(4),ISNULL("
                  "v.NetBox,0)) FROM fin.VatTransaction v LEFT JOIN"
                  " fin.VatCode c ON c.VatCode = v.VatCode WHERE "
                  "v.CompanyCode = ? AND v.VatPeriodKey = ? ORDER "
                  "BY ISNULL(v.VatBox,0), v.TaxPointDate, "
                  "v.DocumentRef, v.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE S-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 15 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE -1 TO D-PREV-BOX
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 1100-PRINT-TXN THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF D-PREV-BOX NOT = -1
               PERFORM 1200-BOX-SUBTOTAL THRU 1200-EXIT
           END-IF
           IF T-CNT = 0
               MOVE "No VAT transaction is held for this period"
                   TO M-NOTE
               GENERATE DE-NONE
               IF XB-RC < 4
                   MOVE 4 TO XB-RC
               END-IF
           END-IF
           MOVE T-CNT TO XB-SEQ-ED
           DISPLAY "STERP-I-VATROWS " XB-SEQ-ED.
       1000-EXIT.
           EXIT.

       1100-PRINT-TXN.
           COMPUTE D-BOX-NO = FUNCTION NUMVAL(BR-COL(1))
           IF D-PREV-BOX NOT = D-BOX-NO AND D-PREV-BOX NOT = -1
               PERFORM 1200-BOX-SUBTOTAL THRU 1200-EXIT
           END-IF
           MOVE D-BOX-NO TO XB-SEQ-ED
           MOVE XB-SEQ-ED(8:2) TO D-BOX
           IF D-BOX-NO = 0
               MOVE " - " TO D-BOX
           END-IF
           MOVE BR-COL(2) TO D-CODE
           MOVE BR-COL(3)(1:8) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO D-TAXPOINT
           MOVE BR-COL(4) TO D-MECH
           MOVE BR-COL(6) TO D-DOC
           MOVE BR-COL(7) TO D-PARTY
           COMPUTE D-NET = FUNCTION NUMVAL(BR-COL(8))
           COMPUTE D-VAT = FUNCTION NUMVAL(BR-COL(9))
           COMPUTE XB-AMT = FUNCTION NUMVAL(BR-COL(10))
           EVALUATE TRUE
               WHEN BR-COL(5)(1:6) = "OUTPUT"
                   MOVE "OUTPUT" TO D-REC-FLAG
               WHEN XB-AMT = 0 AND D-VAT NOT = 0
                   MOVE "BLOCKED" TO D-REC-FLAG
                   ADD 1 TO T-BLOCKED
               WHEN BR-COL(13)(1:1) = "1"
                   MOVE "RECOVER" TO D-REC-FLAG
               WHEN OTHER
                   MOVE "PART REC" TO D-REC-FLAG
           END-EVALUATE
           IF BR-COL(11)(1:1) = "1"
               MOVE "LATE" TO D-LATE-FLAG
               ADD 1 TO T-LATE
           ELSE
               MOVE SPACES TO D-LATE-FLAG
           END-IF
           MOVE BR-COL(12) TO D-ADJ-FLAG
           IF D-ADJ-FLAG NOT = SPACES
               ADD 1 TO T-ADJ
           END-IF
           ADD 1 TO T-CNT
           ADD 1 TO B-CNT
           ADD 1 TO XB-ROWS-OUT
           ADD D-NET TO B-NET
           ADD D-VAT TO B-VAT
           ADD D-NET TO T-NET
           ADD D-VAT TO T-VAT
           MOVE D-BOX-NO TO D-PREV-BOX
           GENERATE DE-TXN.
       1100-EXIT.
           EXIT.

       1200-BOX-SUBTOTAL.
           MOVE SPACES TO B-LABEL
           IF D-PREV-BOX = 0
               MOVE "Outside the nine boxes" TO B-LABEL
           ELSE
               MOVE D-PREV-BOX TO XB-SEQ-ED
               STRING "Subtotal box " XB-SEQ-ED(8:2)
                   DELIMITED BY SIZE INTO B-LABEL
           END-IF
           GENERATE DE-BOXSUB
           MOVE 0 TO B-NET
           MOVE 0 TO B-VAT
           MOVE 0 TO B-CNT.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-017  MODE=MTD - the nine boxes as filed, the
      *> corrections, the postponed import VAT and the evidence of
      *> the submission.  the digital link, on one listing
      *>----------------------------------------------------------------
       2000-MTD-RUN.
           PERFORM 2100-LOAD-RETURN THRU 2100-EXIT
           GENERATE DE-MHEAD
           PERFORM 2200-PRINT-BOXES THRU 2200-EXIT
           PERFORM 2300-CORRECTIONS THRU 2300-EXIT
           PERFORM 2400-POSTPONED THRU 2400-EXIT
           PERFORM 2500-SUBMISSION THRU 2500-EXIT.
       2000-EXIT.
           EXIT.

       2100-LOAD-RETURN.
           MOVE "N" TO R-FOUND
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CONVERT(varchar(12),r.VatReturnId)"
                  ", CONVERT(varchar(6),r.Version), ISNULL("
                  "r.Status,''), ISNULL(r.SubmissionRef,''), "
                  "ISNULL(r.HmrcResponse,''), ISNULL(r.PreparedBy,"
                  "''), ISNULL(r.ApprovedBy,''), ISNULL(CONVERT("
                  "char(8),r.SubmittedAt,112),''), ISNULL("
                  "r.VatRegNo,''), CONVERT(varchar(24),ISNULL("
                  "r.Box1,0)), CONVERT(varchar(24),ISNULL(r.Box2,0"
                  ")), CONVERT(varchar(24),ISNULL(r.Box3,0)), "
                  "CONVERT(varchar(24),ISNULL(r.Box4,0)), CONVERT("
                  "varchar(24),ISNULL(r.Box5,0)), CONVERT(varchar("
                  "24),ISNULL(r.Box6,0)), CONVERT(varchar(24),"
                  "ISNULL(r.Box7,0)), CONVERT(varchar(24),ISNULL("
                  "r.Box8,0)), CONVERT(varchar(24),ISNULL(r.Box9,0"
                  ")) FROM fin.VatReturn r WHERE r.CompanyCode = ? "
                  "AND r.VatPeriodKey = ? ORDER BY r.Version DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE S-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 18 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-2101 " SQLIO-ERRMSG(1:100)
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               IF XB-RC < 4
                   MOVE 4 TO XB-RC
               END-IF
               DISPLAY "STERP-W-2102 no VAT return is prepared for "
                       FUNCTION TRIM(S-PERIOD-KEY)
               GO TO 2100-EXIT
           END-IF
           MOVE "Y" TO R-FOUND
           MOVE BR-COL(1) TO R-ID
           COMPUTE R-ID-NUM = FUNCTION NUMVAL(BR-COL(1))
           MOVE BR-COL(2) TO R-VERSION
           MOVE BR-COL(3) TO R-STATUS
           MOVE BR-COL(4) TO R-SUBREF
           MOVE BR-COL(5) TO R-HMRC
           MOVE BR-COL(6) TO R-PREPARED
           MOVE BR-COL(7) TO R-APPROVED
           MOVE BR-COL(9) TO R-REGNO
           MOVE BR-COL(8)(1:8) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO R-SUBMITTED
           IF XB-NUM-DATE = 0
               MOVE "not yet" TO R-SUBMITTED
           END-IF
           PERFORM VARYING XB-I FROM 1 BY 1 UNTIL XB-I > 9
               COMPUTE XB-J = XB-I + 9
               COMPUTE WS-HB-VALUE(XB-I) =
                   FUNCTION NUMVAL(BR-COL(XB-J))
           END-PERFORM
           IF R-SUBREF = SPACES
               MOVE "Not yet submitted to HMRC" TO R-SUBREF
           END-IF
           IF R-HMRC = SPACES
               MOVE "No HMRC receipt is held against this version"
                   TO R-HMRC
           END-IF.
       2100-EXIT.
           EXIT.

       2200-PRINT-BOXES.
           MOVE 0 TO W-BOXES-SEEN
           IF R-FOUND NOT = "Y"
               MOVE "No return - boxes cannot be listed" TO M-NOTE
               GENERATE DE-NONE
               GO TO 2200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(4),b.BoxNo), CONVERT("
                  "varchar(24),ISNULL(b.BoxValue,0)), ISNULL("
                  "b.Caption,''), CONVERT(varchar(12),ISNULL("
                  "b.TransactionCount,0)), CONVERT(varchar(12),("
                  "SELECT COUNT(*) FROM fin.VatReturnDetail d WHERE"
                  " d.VatReturnId = b.VatReturnId AND d.BoxNo = "
                  "b.BoxNo)), CONVERT(varchar(12),(SELECT COUNT(*)"
                  " FROM fin.VatReturnDetail d WHERE d.VatReturnId "
                  "= b.VatReturnId AND d.BoxNo = b.BoxNo AND "
                  "d.IsLateEntry = 1)) FROM fin.VatReturnBox b "
                  "WHERE b.VatReturnId = ? ORDER BY b.BoxNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE R-ID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-2201 " SQLIO-ERRMSG(1:100)
               GO TO 2200-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 2250-ONE-BOX THRU 2250-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
      *>   fin.VatReturnBox is the filed breakdown; when it has not
      *>   been written the header figures are listed instead and
      *>   the listing says so rather than printing nine zeroes
           IF W-BOXES-SEEN = 0
               PERFORM 2270-HEADER-BOXES THRU 2270-EXIT
           END-IF
           COMPUTE PF-BOXES = WS-HB-VALUE(3) - WS-HB-VALUE(4).
       2200-EXIT.
           EXIT.

       2250-ONE-BOX.
           COMPUTE M-BOX-NO = FUNCTION NUMVAL(BR-COL(1))
           IF M-BOX-NO < 1 OR M-BOX-NO > 9
               GO TO 2250-EXIT
           END-IF
           MOVE M-BOX-NO TO XB-SEQ-ED
           MOVE XB-SEQ-ED(8:2) TO M-BOX
           COMPUTE M-VALUE = FUNCTION NUMVAL(BR-COL(2))
           IF BR-COL(3) = SPACES
               MOVE WS-BOX-CAPTION(M-BOX-NO) TO M-CAPTION
           ELSE
               MOVE BR-COL(3) TO M-CAPTION
           END-IF
           COMPUTE M-CNT = FUNCTION NUMVAL(BR-COL(5))
           IF M-CNT = 0
               COMPUTE M-CNT = FUNCTION NUMVAL(BR-COL(4))
           END-IF
           COMPUTE M-LATE = FUNCTION NUMVAL(BR-COL(6))
           MOVE "fin.VatReturnBox" TO M-NOTE
           ADD 1 TO W-BOXES-SEEN
           ADD 1 TO T-CNT
           ADD 1 TO XB-ROWS-OUT
           ADD M-LATE TO T-LATE
           MOVE M-VALUE TO WS-HB-VALUE(M-BOX-NO)
           GENERATE DE-MBOX
           PERFORM 2260-TRACE-BOX THRU 2260-EXIT.
       2250-EXIT.
           EXIT.

      *>   the machine readable echo of each box, used by TXMDVAT to
      *>   prove the listing against fin.VatReturnBox
       2260-TRACE-BOX.
           MOVE M-BOX-NO TO XB-SEQ-ED
           MOVE M-VALUE TO XB-MON-ED
           DISPLAY "STERP-I-MTDBOX " XB-SEQ-ED(8:2) " " XB-MON-ED.
       2260-EXIT.
           EXIT.

       2270-HEADER-BOXES.
           PERFORM VARYING XB-I FROM 1 BY 1 UNTIL XB-I > 9
               MOVE XB-I TO M-BOX-NO
               MOVE XB-I TO XB-SEQ-ED
               MOVE XB-SEQ-ED(8:2) TO M-BOX
               MOVE WS-BOX-CAPTION(XB-I) TO M-CAPTION
               MOVE WS-HB-VALUE(XB-I) TO M-VALUE
               MOVE 0 TO M-CNT
               MOVE 0 TO M-LATE
               MOVE "fin.VatReturn header" TO M-NOTE
               ADD 1 TO T-CNT
               ADD 1 TO XB-ROWS-OUT
               GENERATE DE-MBOX
               PERFORM 2260-TRACE-BOX THRU 2260-EXIT
           END-PERFORM
           MOVE "Box breakdown not stored - header values listed"
               TO M-NOTE
           GENERATE DE-NONE
           IF XB-RC < 4
               MOVE 4 TO XB-RC
           END-IF.
       2270-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-017  an error correction is disclosed on the face
      *> of the audit trail, with its method and its approval state
      *>----------------------------------------------------------------
       2300-CORRECTIONS.
           GENERATE DE-ERRHD
           MOVE 0 TO W-ERRS-SEEN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(e.OriginalVatPeriodKey,''), "
                  "ISNULL(CONVERT(char(8),e.DiscoveredDate,112),''"
                  "), CONVERT(varchar(24),ISNULL(e.NetErrorAmount,"
                  "0)), CONVERT(varchar(24),ISNULL(e.Box6Turnover,"
                  "0)), ISNULL(e.Method,''), ISNULL(e.Status,''), "
                  "CONVERT(char(1),ISNULL(e.IsDeliberate,0)), "
                  "ISNULL(e.DisclosureRef,''), ISNULL(e.Reason,'') "
                  "FROM fin.VatErrorCorrection e WHERE "
                  "e.CompanyCode = ? AND (e.CorrectionVatPeriodKey "
                  "= ? OR e.OriginalVatPeriodKey = ?) ORDER BY "
                  "e.DiscoveredDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE S-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE S-PERIOD-KEY TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
                       SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 9 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE "Corrections unavailable - input error"
                   TO M-NOTE
               GENERATE DE-NONE
               IF XB-RC < 4
                   MOVE 4 TO XB-RC
               END-IF
               GO TO 2300-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 2350-ONE-ERROR THRU 2350-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF W-ERRS-SEEN = 0
               MOVE "None disclosed against this period" TO M-NOTE
               GENERATE DE-NONE
           END-IF.
       2300-EXIT.
           EXIT.

       2350-ONE-ERROR.
           MOVE BR-COL(1) TO E-ORIG
           MOVE BR-COL(2)(1:8) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO E-FOUND
           COMPUTE E-NET = FUNCTION NUMVAL(BR-COL(3))
           COMPUTE E-TURNOVER = FUNCTION NUMVAL(BR-COL(4))
           MOVE BR-COL(5) TO E-METHOD
           MOVE BR-COL(6) TO E-STATUS
           IF BR-COL(7)(1:1) = "1"
               MOVE "DELIBERATE" TO E-DELIB
           ELSE
               MOVE "careless" TO E-DELIB
           END-IF
           MOVE BR-COL(8) TO E-DISC
           MOVE BR-COL(9) TO E-REASON
           ADD 1 TO W-ERRS-SEEN
           ADD 1 TO XB-ROWS-OUT
           GENERATE DE-ERR.
       2350-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-017  postponed import VAT is declared and recovered
      *> in the same return - both sides are shown with the evidence
      *>----------------------------------------------------------------
       2400-POSTPONED.
           GENERATE DE-PIVHD
           MOVE 0 TO P-COUNT
           MOVE 0 TO P-TOT-VAT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(v.VatCode,''), CONVERT(char(8),"
                  "v.TaxPointDate,112), ISNULL(v.DocumentType,'') "
                  "+ ' ' + ISNULL(v.DocumentRef,''), ISNULL("
                  "v.PartyCode,''), CONVERT(varchar(24),ISNULL("
                  "v.NetAmountGBP,0)), CONVERT(varchar(24),ISNULL("
                  "v.VatAmountGBP,0)), CONVERT(varchar(24),ISNULL("
                  "v.RecoverableVatGBP,0)), ISNULL(v.EvidenceRef,''"
                  ") FROM fin.VatTransaction v WHERE v.CompanyCode "
                  "= ? AND v.VatPeriodKey = ? AND v.Mechanism = "
                  "'PIV' ORDER BY v.TaxPointDate, v.DocumentRef"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE S-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 8 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE "Postponed import VAT unavailable" TO M-NOTE
               GENERATE DE-NONE
               IF XB-RC < 4
                   MOVE 4 TO XB-RC
               END-IF
               GO TO 2400-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 2450-ONE-PIV THRU 2450-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF P-COUNT = 0
               MOVE "No postponed import VAT in this period"
                   TO M-NOTE
               GENERATE DE-NONE
           END-IF.
       2400-EXIT.
           EXIT.

       2450-ONE-PIV.
           MOVE BR-COL(1) TO P-CODE
           MOVE BR-COL(2)(1:8) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO P-TAXPOINT
           MOVE BR-COL(3) TO P-DOC
           MOVE BR-COL(4) TO P-PARTY
           COMPUTE P-NET = FUNCTION NUMVAL(BR-COL(5))
           COMPUTE P-VAT = FUNCTION NUMVAL(BR-COL(6))
           COMPUTE P-REC = FUNCTION NUMVAL(BR-COL(7))
           MOVE BR-COL(8) TO P-EVIDENCE
           ADD 1 TO P-COUNT
           ADD 1 TO XB-ROWS-OUT
           ADD P-VAT TO P-TOT-VAT
           GENERATE DE-PIV.
       2450-EXIT.
           EXIT.

       2500-SUBMISSION.
           IF R-FOUND NOT = "Y"
               GO TO 2500-EXIT
           END-IF
           GENERATE DE-SUBHD.
       2500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-007, RPT-017, XMD-005  the same proof RECONCTL
      *> applies to a control account, narrowed to the VAT period:
      *> the VAT the period declares against the movement on the VAT
      *> control accounts.  outside tolerance the job returns 04
      *>----------------------------------------------------------------
       3000-PROVE.
           PERFORM 3100-LEDGER-SIDE THRU 3100-EXIT
           PERFORM 3200-GL-SIDE THRU 3200-EXIT
           COMPUTE PF-DIFF = PF-LEDGER - PF-GL
           IF PF-DIFF > XB-TOLERANCE OR PF-DIFF < 0 - XB-TOLERANCE
               MOVE "DIFFERENCE" TO PF-STATUS
               ADD 1 TO XB-DIFFS
           ELSE
               MOVE "AGREED" TO PF-STATUS
           END-IF
           GENERATE DE-PROOF
           IF S-MODE = "MTD"
               COMPUTE PF-BOXDIFF = PF-BOXES - PF-LEDGER
               IF PF-BOXDIFF > XB-TOLERANCE OR
                  PF-BOXDIFF < 0 - XB-TOLERANCE
                   MOVE "DIFFERENCE" TO PF-BOXSTAT
                   ADD 1 TO XB-DIFFS
               ELSE
                   MOVE "AGREED" TO PF-BOXSTAT
               END-IF
               GENERATE DE-BOXPRF
           END-IF
           IF XB-DIFFS = 0
               MOVE "RECONCILED" TO RPT-STATUS-TEXT
           ELSE
               MOVE "UNRECONCILED" TO RPT-STATUS-TEXT
               IF XB-RC < 4
                   MOVE 4 TO XB-RC
               END-IF
           END-IF
           MOVE PF-DIFF TO XB-MON-ED
           MOVE XB-DIFFS TO XB-INT-ED
           DISPLAY "STERP-I-3001 VAT control difference "
                   FUNCTION TRIM(XB-MON-ED) " on "
                   FUNCTION TRIM(XB-INT-ED) " comparison(s)".
       3000-EXIT.
           EXIT.

       3100-LEDGER-SIDE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM(CASE WHEN "
                  "v.Direction = 'OUTPUT' THEN v.VatAmountGBP ELSE "
                  "-ISNULL(v.RecoverableVatGBP,v.VatAmountGBP) END)"
                  ",0)), CONVERT(varchar(12),COUNT(*)) FROM "
                  "fin.VatTransaction v WHERE v.CompanyCode = ? AND"
                  " v.VatPeriodKey = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE S-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE 0 TO PF-LEDGER
           MOVE 0 TO PF-TXNS
           IF SQLIO-OK
               COMPUTE PF-LEDGER = FUNCTION NUMVAL(BR-COL(1))
               COMPUTE PF-TXNS = FUNCTION NUMVAL(BR-COL(2))
           END-IF.
       3100-EXIT.
           EXIT.

       3200-GL-SIDE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM("
                  "l.CreditAmountGBP - l.DebitAmountGBP),0)), MIN("
                  "a.AccountCode) + CASE WHEN COUNT(DISTINCT "
                  "a.AccountCode) > 1 THEN '..' + MAX(a.AccountCode"
                  ") ELSE '' END FROM fin.JournalLine l JOIN "
                  "fin.Journal j ON j.JournalId = l.JournalId JOIN "
                  "fin.Account a ON a.CompanyCode = l.CompanyCode "
                  "AND a.AccountCode = l.AccountCode WHERE "
                  "l.CompanyCode = ? AND j.Status = 'POSTED' AND "
                  "a.ControlType = 'VAT' AND CONVERT(char(8),"
                  "j.PostingDate,112) BETWEEN ? AND ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE S-START8 TO SQLIO-PARM-VAL(2)
           MOVE S-END8 TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
                       SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE 0 TO PF-GL
           MOVE SPACES TO PF-ACCTS
           IF SQLIO-OK
               COMPUTE PF-GL = FUNCTION NUMVAL(BR-COL(1))
               MOVE BR-COL(2) TO PF-ACCTS
           END-IF
           IF PF-ACCTS = SPACES
               MOVE "none posted" TO PF-ACCTS
           END-IF.
       3200-EXIT.
           EXIT.

       COPY XMDBATPR.
