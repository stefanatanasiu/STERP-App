      *>================================================================
      *> GLVALIDCA - COMMAREA for GLVALID, chart of accounts and
      *>             dimension validation (FIN-001, FIN-002, FIN-003).
      *> Functions: ACCOUNT  validate one account on a date
      *>            LINE     validate account + dimensions for a posting
      *>            EVENT    validate a business event mapping
      *>            USAGE    may the account be deleted or reclassified
      *> 1994-06-14 A P WHITTAKER   2026-10-05 STERP DIMENSIONS
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  GV-BODY.
               10  GV-ACCOUNT          PIC X(12).
               10  GV-POSTING-DATE     PIC 9(8).
               10  GV-DIM-CC           PIC X(20).
               10  GV-DIM-PROJ         PIC X(20).
               10  GV-EVENT-CODE       PIC X(30).
               10  GV-SOURCE-MODULE    PIC X(6).
               10  GV-VAT-CODE         PIC X(4).
               10  GV-DR-CR            PIC X(2).
               10  GV-AMOUNT           PIC S9(11)V99 COMP-3.
               10  GV-MANUAL-ENTRY     PIC X.
               10  GV-LINE-NUM         PIC 9(4).
      *>     ---- answers ----
               10  GV-ACCOUNT-NAME     PIC X(80).
               10  GV-CLASS-CODE       PIC X(10).
               10  GV-CLASS-TYPE       PIC X(10).
               10  GV-NORMAL-BALANCE   PIC X(2).
               10  GV-STATEMENT        PIC X(2).
               10  GV-ACCOUNT-KIND     PIC X(8).
               10  GV-ACCOUNT-STATUS   PIC X(10).
               10  GV-CONTROL-TYPE     PIC X(10).
               10  GV-CONTROL-SUBLDG   PIC X(6).
               10  GV-ALLOW-MANUAL     PIC X.
               10  GV-VAT-ELIGIBLE     PIC X.
               10  GV-STAT-LINE        PIC X(20).
               10  GV-MGMT-GROUP       PIC X(30).
               10  GV-TAX-ANALYSIS     PIC X(20).
               10  GV-CC-REQUIREMENT   PIC X(10).
               10  GV-PROJ-REQUIRED    PIC X(10).
               10  GV-CC-NAME          PIC X(80).
               10  GV-PROJ-NAME        PIC X(80).
               10  GV-USE-COUNT        PIC S9(9) COMP-5.
               10  GV-MAY-RETIRE       PIC X.
               10  GV-MAY-DELETE       PIC X.
               10  GV-VALID            PIC X.
               10  GV-FAIL-FIELD       PIC X(20).
               10  GV-LEG-CNT          PIC 9(2).
               10  GV-LEGS OCCURS 8 TIMES.
                   15  GV-L-LEG-CODE   PIC X(12).
                   15  GV-L-DR-CR      PIC X(2).
                   15  GV-L-ACCOUNT    PIC X(12).
                   15  GV-L-DIM-CC     PIC X(20).
                   15  GV-L-DIM-PROJ   PIC X(20).
                   15  GV-L-DESC       PIC X(60).
