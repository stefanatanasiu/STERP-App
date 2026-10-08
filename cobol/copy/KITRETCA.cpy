      *>================================================================
      *> KITRETCA - COMMAREA for KITRET, return of issued material.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : KIT MAINTENANCE AND EXECUTION
      *>
      *> A return references the ORIGINAL issue (KX-ORIG-ISSUE-ID or
      *> the kit line), is bounded by the unreversed issued quantity,
      *> carries the attributable cost of that issue and states the
      *> condition of the material (KIT-013, D-12).
      *>
      *> Returned material that needs inspection does NOT become
      *> available: it is returned in INSPECTION condition and only an
      *> inspection outcome can release it.  Returning more than was
      *> issued is refused - there is no such thing as a negative
      *> issue history.
      *>
      *> CA-FUNCTION values
      *>   RETURN    return KX-QUANTITY of the issued component
      *>   RETURNCK  dry run: report the net returnable quantity and
      *>             the attributable value without posting
      *>   LISTISS   list the issues of one kit line that still have
      *>             an unreversed quantity, so the operator can pick
      *>             the right one
      *>
      *> REASON CODES  KIT0001, KIT0002, KIT0003, KIT0005, KIT0007,
      *>   KIT0008, KIT0011, KIT0012, plus STK* from STKMOVE.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-01-26 PMK  CR2284 RETURNS FROM THE SHOP FLOOR
      *> 2015-10-12 PMK  CR2392 RETURN AT THE ISSUE VALUE, NOT TODAY'S
      *> 2026-10-06 STERP KIT-013, D-12
      *>================================================================
      *> REQ: KIT-013
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KX-BODY.
               10  KX-KIT-REF          PIC X(30).
               10  KX-LINE-NUM         PIC S9(9) COMP-5.
               10  KX-ORIG-ISSUE-ID    PIC S9(17).
               10  KX-ITEM-CODE        PIC X(40).
               10  KX-REVISION         PIC X(4).
               10  KX-SITE             PIC X(10).
               10  KX-WAREHOUSE        PIC X(20).
               10  KX-LOCATION         PIC X(20).
               10  KX-LOT-CODE         PIC X(30).
               10  KX-SERIAL-NO        PIC X(40).
               10  KX-QUANTITY         PIC S9(13)V9(6).
               10  KX-UOM-CODE         PIC X(10).
               10  KX-CONDITION        PIC X(12).
               10  KX-NEEDS-INSPECT    PIC X.
                   88  KX-INSPECT-REQD           VALUE "Y".
               10  KX-ACTUAL-DATE      PIC 9(8).
               10  KX-POSTING-DATE     PIC 9(8).
               10  KX-REASON-CODE      PIC X(20).
               10  KX-REASON-TEXT      PIC X(300).
      *>       ---- result ----
               10  KX-RETURNABLE-QTY   PIC S9(13)V9(6).
               10  KX-RETURNED-QTY     PIC S9(13)V9(6).
               10  KX-UNIT-COST        PIC S9(11)V9(6).
               10  KX-VALUE-GBP        PIC S9(11)V99.
               10  KX-KIT-ISSUE-ID     PIC S9(17).
               10  KX-MOVEMENT-ID      PIC S9(17).
               10  KX-LINE-GROSS       PIC S9(13)V9(6).
               10  KX-LINE-NET         PIC S9(13)V9(6).
               10  KX-LINE-REMAINING   PIC S9(13)V9(6).
               10  KX-LINE-EXCESS      PIC S9(13)V9(6).
               10  KX-STOCK-CONDITION  PIC X(12).
               10  KX-AVAILABLE        PIC X.
               10  KX-ISSUE-CNT        PIC S9(4).
               10  KX-ISSUES OCCURS 15 TIMES.
                   15  KX-I-ISSUE-ID   PIC S9(17).
                   15  KX-I-ITEM       PIC X(40).
                   15  KX-I-LOT        PIC X(30).
                   15  KX-I-SERIAL     PIC X(40).
                   15  KX-I-DATE       PIC 9(8).
                   15  KX-I-ISSUED     PIC S9(13)V9(6).
                   15  KX-I-RETURNED   PIC S9(13)V9(6).
                   15  KX-I-OPEN-QTY   PIC S9(13)V9(6).
                   15  KX-I-UNIT-COST  PIC S9(11)V9(6).
                   15  KX-I-OPERATOR   PIC X(20).
