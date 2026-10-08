      *>================================================================
      *> KITSCRAPCA - COMMAREA for KITSCRAP, scrap on a kit.
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> Scrap is not a stock return (KIT-014).  Scrapped material was
      *> issued, it stays issued, and its cost stays in the history of
      *> the kit.  What scrap changes is the DISPOSITION of material
      *> already on the job and, where the output is still wanted, the
      *> need for an approved replacement requirement.
      *>
      *> Three different events share this program because they share
      *> the evidence, but they have different quantity and cost
      *> consequences:
      *>   COMPONENT  component scrapped on the job.  Issued quantity
      *>              is unchanged, scrapped quantity rises, and the
      *>              remaining requirement is NOT automatically
      *>              increased - that needs KITAMEND and authority.
      *>   OUTPUT     rejected parent output.  It never becomes
      *>              saleable stock and never silently removes the
      *>              component cost already absorbed.
      *>   SUBCON     loss at a subcontractor, recorded against the
      *>              subcontract reference.
      *>
      *> CA-FUNCTION values  SCRAP, SCRAPCK (dry run)
      *>
      *> REASON CODES  KIT0001..KIT0003, KIT0005, KIT0007, KIT0008,
      *>   KIT0009.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-02-10 PMK  CR2290 SCRAP RECORDING
      *> 2016-09-14 PMK  CR2497 SCRAP NO LONGER TRIGGERS AN AUTOMATIC
      *>                 REPLACEMENT ISSUE - D-12 WANTS AUTHORITY
      *> 2026-10-06 STERP KIT-014
      *>================================================================
      *> REQ: KIT-014
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KP-BODY.
               10  KP-KIT-REF          PIC X(30).
               10  KP-LINE-NUM         PIC S9(9) COMP-5.
               10  KP-SCRAP-KIND       PIC X(12).
                   88  KP-COMPONENT              VALUE "COMPONENT".
                   88  KP-OUTPUT                 VALUE "OUTPUT".
                   88  KP-SUBCON                 VALUE "SUBCON".
               10  KP-ITEM-CODE        PIC X(40).
               10  KP-REVISION         PIC X(4).
               10  KP-SITE             PIC X(10).
               10  KP-LOT-CODE         PIC X(30).
               10  KP-SERIAL-NO        PIC X(40).
               10  KP-QUANTITY         PIC S9(13)V9(6).
               10  KP-UOM-CODE         PIC X(10).
               10  KP-CAUSE-CODE       PIC X(20).
               10  KP-CAUSE-TEXT       PIC X(300).
               10  KP-SUBCON-REF       PIC X(40).
               10  KP-ACTUAL-DATE      PIC 9(8).
               10  KP-POSTING-DATE     PIC 9(8).
               10  KP-APPROVAL-ID      PIC S9(17).
               10  KP-REPLACE-WANTED   PIC X.
                   88  KP-WANTS-REPLACE          VALUE "Y".
               10  KP-CHECK-ONLY       PIC X.
      *>       ---- result ----
               10  KP-UNIT-COST        PIC S9(11)V9(6).
               10  KP-VALUE-GBP        PIC S9(11)V99.
               10  KP-KIT-ISSUE-ID     PIC S9(17).
               10  KP-MOVEMENT-ID      PIC S9(17).
               10  KP-LINE-SCRAPPED    PIC S9(13)V9(6).
               10  KP-LINE-NET         PIC S9(13)V9(6).
               10  KP-LINE-REMAINING   PIC S9(13)V9(6).
               10  KP-LINE-EXCESS      PIC S9(13)V9(6).
               10  KP-ON-JOB-QTY       PIC S9(13)V9(6).
               10  KP-REJECTED-TOTAL   PIC S9(13)V9(6).
               10  KP-REPLACE-NEEDED   PIC S9(13)V9(6).
               10  KP-NEEDS-AUTH       PIC X.
               10  KP-EXCEPTION-ID     PIC S9(17).
               10  KP-IMPACT-TEXT      PIC X(200).
