      *>================================================================
      *> BOMBULKCA - COMMAREA of BOMBULK, bulk component replacement,
      *>             addition, removal and quantity change.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : BILLS OF MATERIAL
      *> PURPOSE   : BOM-018/BOM-024/BOM-030.  Nothing is ever changed
      *>             before the engineer has seen what would be
      *>             changed.  PREVIEW builds every proposed revision,
      *>             validates it and throws the lot away; APPLY does
      *>             exactly the same work and keeps it, but only when
      *>             every single proposed revision passed.  One
      *>             failed bill therefore refuses the whole run
      *>             instead of being quietly counted as changed, and
      *>             a released original is never written to - it gets
      *>             a new draft revision beside it.
      *> FUNCTIONS :
      *>   PREVIEW  per-bill outcome, before and after, nothing kept
      *>   APPLY    the same run, kept, all or nothing
      *>   GETLINE  page the per-bill outcome of the last run
      *>   (COMMIT and RESULT are accepted as the 2016 spellings of
      *>    APPLY and GETLINE - CR2442 named them that way.)
      *> ACTIONS   : REPLACE  swap component BB-OLD-ITEM for
      *>                      BB-NEW-ITEM on every target bill
      *>             ADD      add BB-NEW-ITEM to every target bill
      *>             REMOVE   remove BB-OLD-ITEM from every target
      *>             QTYCHG   change usage / scrap / unit / basis of
      *>                      BB-OLD-ITEM on every target
      *> SCOPE     : ITEM    every bill that uses BB-OLD-ITEM
      *>             PARENT  every bill of parent BB-SCOPE-ITEM
      *>             BOMREF  the named bill reference in BB-SCOPE-ITEM
      *> A repeated APPLY with the same change reference returns the
      *> first outcome instead of creating a second set of revisions
      *> (BOM-030), which is what BB-DUPLICATE reports.
      *> Reason codes BOM1401..BOM1420 (BOMBULK), BOM1430..BOM1445
      *> (BOMBREV, the proposal builder behind it).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2016-03-21 AJS  CR2442 BULK ENGINEERING CHANGE ADDED
      *> 2017-02-28 AJS  CR2601 RELEASED TARGETS GET A NEW REVISION
      *> 2026-10-06 STERP REFUSE THE RUN WHEN ANY PROPOSAL FAILS
      *>================================================================
      *> REQ: BOM-018, BOM-024, BOM-029, BOM-030
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BB-BODY.
               10  BB-CHANGE-REF       PIC X(30).
               10  BB-TITLE            PIC X(100).
               10  BB-ACTION           PIC X(10).
               10  BB-SCOPE            PIC X(10).
               10  BB-SCOPE-ITEM       PIC X(40).
               10  BB-OLD-ITEM         PIC X(40).
               10  BB-NEW-ITEM         PIC X(40).
               10  BB-NEW-QTY          PIC S9(13)V9(6).
               10  BB-NEW-UOM          PIC X(10).
               10  BB-NEW-BASIS        PIC X(12).
               10  BB-NEW-SCRAP        PIC S9(5)V9(6).
               10  BB-NEW-LINE-NUM     PIC S9(4).
               10  BB-EFF-DATE         PIC 9(8).
               10  BB-INCLUDE-DRAFT    PIC X.
               10  BB-REQUESTED-BY     PIC X(20).
               10  BB-CHANGE-ID        PIC S9(9) COMP-5.
               10  BB-TARGET-CNT       PIC S9(4).
               10  BB-APPLIED-CNT      PIC S9(4).
               10  BB-FAILED-CNT       PIC S9(4).
               10  BB-SKIPPED-CNT      PIC S9(4).
               10  BB-NEWREV-CNT       PIC S9(4).
               10  BB-AMEND-CNT        PIC S9(4).
               10  BB-ERROR-CNT        PIC S9(4).
               10  BB-WARNING-CNT      PIC S9(4).
               10  BB-STATUS           PIC X(12).
               10  BB-MAY-APPLY        PIC X.
               10  BB-DUPLICATE        PIC X.
               10  BB-COMPLETENESS     PIC X(12).
               10  BB-SUMMARY          PIC X(200).
               10  BB-REFUSAL          PIC X(200).
               10  BB-FROM-LINE        PIC S9(9) COMP-5.
               10  BB-WIN-MAX          PIC S9(4).
               10  BB-WIN-CNT          PIC S9(4).
               10  BB-MORE-LINES       PIC X.
               10  BB-LINES OCCURS 25 TIMES.
                   15  BB-L-SEQ        PIC S9(9) COMP-5.
                   15  BB-L-BOM-ID     PIC S9(9) COMP-5.
                   15  BB-L-NEW-BOM-ID PIC S9(9) COMP-5.
                   15  BB-L-BOM-REF    PIC X(30).
                   15  BB-L-BOM-REV    PIC X(4).
                   15  BB-L-NEW-REV    PIC X(4).
                   15  BB-L-PARENT     PIC X(40).
                   15  BB-L-STATUS     PIC X(12).
                   15  BB-L-TREATMENT  PIC X(10).
                   15  BB-L-LINE-NUM   PIC S9(4).
                   15  BB-L-OLD-QTY    PIC S9(13)V9(6).
                   15  BB-L-NEW-QTY    PIC S9(13)V9(6).
                   15  BB-L-OUTCOME    PIC X(10).
                   15  BB-L-REASON     PIC X(8).
                   15  BB-L-ERRORS     PIC S9(4).
                   15  BB-L-DETAIL     PIC X(120).
