      *>================================================================
      *> BOMSELCTCA - COMMAREA of BOMSELCT, deterministic revision and
      *>              bill selection (specification section 4, D-10).
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : BILLS OF MATERIAL
      *> FUNCTIONS  :
      *>   SELECT  one applicable released definition for company /
      *>           site / purpose / variant at BS-EFF-DATE.  Effectivity
      *>           is half-open [start, end): active on the start date,
      *>           inactive on the end date.  Nothing eligible is 16;
      *>           more than one eligible release is 08 with every
      *>           candidate listed - never an arbitrary "latest row".
      *>   PIN     a deliberately selected revision (BS-WANT-REV); the
      *>           revision must exist, be usable and cover the date.
      *>   LIST    every candidate for the context, with the reason
      *>           each was or was not chosen (engineering enquiry).
      *> Reason codes BOM0201..BOM0212.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-04-02 AJS  CR2203 ENGINEERING REWRITE
      *> 2015-01-19 AJS  CR2287 HALF OPEN EFFECTIVITY [start, end)
      *> 2026-10-05 STERP JOB BILLS ARE SELECTED BY EXPLICIT REFERENCE
      *>                 ONLY, NEVER SEARCHED FOR A CONVENIENT MATCH
      *>================================================================
      *> REQ: BOM-003, BOM-004, BOM-005, BOM-016
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BS-BODY.
               10  BS-PARENT-ITEM      PIC X(40).
               10  BS-PARENT-REV       PIC X(4).
               10  BS-PURPOSE          PIC X(10).
               10  BS-JOB-REF          PIC X(20).
               10  BS-VARIANT          PIC X(20).
               10  BS-WANT-REV         PIC X(4).
               10  BS-EFF-DATE         PIC 9(8).
               10  BS-ALLOW-DRAFT      PIC X.
               10  BS-BOM-ID           PIC S9(9) COMP-5.
               10  BS-BOM-REF          PIC X(30).
               10  BS-BOM-REV          PIC X(4).
               10  BS-BOM-STATUS       PIC X(12).
               10  BS-BOM-SITE         PIC X(10).
               10  BS-EFF-FROM         PIC 9(8).
               10  BS-EFF-TO           PIC 9(8).
               10  BS-NOMINAL-BASIS    PIC S9(13)V9(6).
               10  BS-YIELD            PIC S9(5)V9(6).
               10  BS-QUANTUM          PIC S9(13)V9(6).
               10  BS-BATCH-SIZE       PIC S9(13)V9(6).
               10  BS-IS-PHANTOM       PIC X.
               10  BS-CONSOLIDATE      PIC X.
               10  BS-COMPONENT-CNT    PIC S9(4).
               10  BS-CAND-CNT         PIC S9(4).
               10  BS-CAND-MAX         PIC S9(4).
               10  BS-CANDIDATES OCCURS 20 TIMES.
                   15  BS-C-BOM-ID     PIC S9(9) COMP-5.
                   15  BS-C-BOM-REF    PIC X(30).
                   15  BS-C-BOM-REV    PIC X(4).
                   15  BS-C-STATUS     PIC X(12).
                   15  BS-C-SITE       PIC X(10).
                   15  BS-C-VARIANT    PIC X(20).
                   15  BS-C-PURPOSE    PIC X(10).
                   15  BS-C-EFF-FROM   PIC 9(8).
                   15  BS-C-EFF-TO     PIC 9(8).
                   15  BS-C-ELIGIBLE   PIC X.
                   15  BS-C-REASON     PIC X(40).
