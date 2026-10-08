      *>================================================================
      *> KITISSUECA - COMMAREA for KITISSUE, material issue to a kit.
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> An issue is the moment the material stops being stock and
      *> starts being work in progress.  KITISSUE validates in COBOL
      *> (kit state, line, eligible quantity, traceability, unit,
      *> substitution authority, overissue authority) and then posts
      *> exactly ONE stock effect through STKMOVE, so a refused issue
      *> leaves nothing behind (KIT-010).
      *>
      *> Backflush is a conditional capability (D-20): it is allowed
      *> only for components that are neither lot nor serial
      *> controlled, and only when the kit was explicitly flagged for
      *> it.  Backflush may not guess a mandatory serial, may not
      *> bypass a shortage, and may not half succeed while presenting
      *> a completed parent receipt.
      *>
      *> Overissue and excess need a visible reason and the relevant
      *> approval (KIT-011).  An approval raises the PERMITTED issue
      *> quantity; it never creates physical stock, so an issue that
      *> would drive the balance negative is still refused.
      *>
      *> CA-FUNCTION values
      *>   ISSUE     issue KS-QUANTITY of KS-ITEM-CODE to the kit line
      *>   SUBST     issue an approved substitute instead of the
      *>             planned component (KIT-012).  The standard bill
      *>             is NOT changed; the substitution is disclosed on
      *>             the kit, the genealogy and the documents.
      *>   BACKFLSH  issue the calculated requirement for
      *>             KS-GOOD-QTY good parents, for untraced lines only
      *>   CHECK     dry run: report what ISSUE or BACKFLSH would do,
      *>             including shortages, without posting anything
      *>
      *> REASON CODES  KIT0001..KIT0003, KIT0005, KIT0007, KIT0008,
      *>   KIT0009, KIT0010, KIT0012, KIT0013, KIT0014, KIT0022, and
      *>   STK* / RSV* passed through from the stock services.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-12-08 PMK  CR2271 KIT ISSUE THROUGH STKMOVE
      *> 2015-09-30 PMK  CR2388 BACKFLUSH RESTRICTED TO UNTRACED (D-20)
      *> 2016-05-30 PMK  CR2488 OVERISSUE NEEDS AUTHORITY AND A REASON
      *> 2026-10-06 STERP KIT-010, KIT-011, KIT-012
      *>================================================================
      *> REQ: KIT-010, KIT-011, KIT-012
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KS-BODY.
               10  KS-KIT-REF          PIC X(30).
               10  KS-LINE-NUM         PIC S9(9) COMP-5.
               10  KS-ITEM-CODE        PIC X(40).
               10  KS-REVISION         PIC X(4).
               10  KS-PLANNED-ITEM     PIC X(40).
               10  KS-SITE             PIC X(10).
               10  KS-WAREHOUSE        PIC X(20).
               10  KS-LOCATION         PIC X(20).
               10  KS-LOT-CODE         PIC X(30).
               10  KS-SERIAL-NO        PIC X(40).
               10  KS-QUANTITY         PIC S9(13)V9(6).
               10  KS-UOM-CODE         PIC X(10).
               10  KS-GOOD-QTY         PIC S9(13)V9(6).
               10  KS-ACTUAL-DATE      PIC 9(8).
               10  KS-POSTING-DATE     PIC 9(8).
               10  KS-REASON-CODE      PIC X(20).
               10  KS-REASON-TEXT      PIC X(300).
               10  KS-APPROVAL-ID      PIC S9(17).
               10  KS-SUB-APPROVAL     PIC S9(17).
               10  KS-SUB-RATIO        PIC S9(9)V9(6).
               10  KS-ALLOW-OVER       PIC X.
                   88  KS-OVER-OK                VALUE "Y".
               10  KS-CHECK-ONLY       PIC X.
                   88  KS-IS-CHECK               VALUE "Y".
      *>       ---- result ----
               10  KS-STOCK-QTY        PIC S9(13)V9(6).
               10  KS-FACTOR           PIC S9(9)V9(9).
               10  KS-UNIT-COST        PIC S9(11)V9(6).
               10  KS-VALUE-GBP        PIC S9(11)V99.
               10  KS-KIT-ISSUE-ID     PIC S9(17).
               10  KS-MOVEMENT-ID      PIC S9(17).
               10  KS-LINE-GROSS       PIC S9(13)V9(6).
               10  KS-LINE-NET         PIC S9(13)V9(6).
               10  KS-LINE-REMAINING   PIC S9(13)V9(6).
               10  KS-LINE-EXCESS      PIC S9(13)V9(6).
               10  KS-LINE-STATUS      PIC X(12).
               10  KS-KIT-STATUS       PIC X(14).
               10  KS-WIP-VALUE        PIC S9(11)V99.
               10  KS-SHORTAGE-QTY     PIC S9(13)V9(6).
               10  KS-FREE-QTY         PIC S9(13)V9(6).
               10  KS-LINES-DONE       PIC S9(4).
               10  KS-LINES-FAILED     PIC S9(4).
               10  KS-FAILED-ITEM      PIC X(40).
               10  KS-TRACED           PIC X.
               10  KS-DUPLICATE        PIC X.
               10  KS-IMPACT-TEXT      PIC X(200).
