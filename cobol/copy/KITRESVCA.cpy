      *>================================================================
      *> KITRESVCA - COMMAREA for KITRESV, kit reservation and picking.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : KIT MAINTENANCE AND EXECUTION
      *>
      *> Reservation, picking and issue are three different states of
      *> the same material and must stay distinguishable (KIT-008,
      *> KIT-009).  A reservation is a claim on eligible stock; a pick
      *> is a physical selection; only an ISSUE changes the stock
      *> balance and charges work in progress.
      *>
      *> Reservations for THIS demand are recognised once and are never
      *> reserved a second time.  Reservations belonging to another
      *> demand are simply unavailable - reassignment is an authorised
      *> act, not a side effect of kitting.
      *>
      *> CA-FUNCTION values
      *>   RESERVE   reserve the remaining requirement of one line
      *>             (KR-LINE-NUM) or of every open line (KR-LINE-NUM
      *>             = 0) through STKRESV.  KR-ALLOW-PARTIAL = "Y"
      *>             takes what is free and reports the shortfall with
      *>             return code 04.
      *>   RELEASE   release this kit's outstanding reservations
      *>   REVALID   revalidate reservations after a material change
      *>   PICKLIST  build the picking list: exact component,
      *>             quantity, location and required lot or serial
      *>   PICK      confirm a pick (quantity, location, lot, serial).
      *>             Picked quantity is NOT issued quantity.
      *>   SHORTPCK  confirm a short pick with a reason; the shortfall
      *>             stays visible on the line and raises an exception
      *>   UNPICK    reverse a confirmed pick before issue
      *>   SUPPLY    cover the line from explicitly identified future
      *>             supply (KR-SUPPLY-TYPE / KR-SUPPLY-REF /
      *>             KR-SUPPLY-LINE) instead of from free stock.
      *>             Nothing in inv moves and nothing is reserved:
      *>             the named inbound is spoken for and the line's
      *>             outstanding requirement falls by KR-SUPPLY-QTY.
      *>             Return code 04 + KIT0010 means the supply is due
      *>             after the line needs it - it is still a cover,
      *>             but a late one, and it is flagged as such.
      *>
      *> REASON CODES  KIT0001, KIT0002, KIT0003, KIT0005, KIT0007,
      *>   KIT0008, KIT0010, KIT0012, and RSV* passed through.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-11-11 PMK  CR2266 RESERVE AND PICK FOR KITS
      *> 2016-07-19 PMK  CR2492 PICKED IS NOT ISSUED.  WAREHOUSE USED
      *>                 TO "PICK" STRAIGHT INTO WIP AND THE SHOP
      *>                 FLOOR COULD NOT FIND THE BOXES.
      *> 2026-10-06 STERP KIT-008, KIT-009, KIT-022
      *>================================================================
      *> REQ: KIT-008, KIT-009
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KR-BODY.
               10  KR-KIT-REF          PIC X(30).
               10  KR-LINE-NUM         PIC S9(9) COMP-5.
               10  KR-ITEM-CODE        PIC X(40).
               10  KR-REVISION         PIC X(4).
               10  KR-SITE             PIC X(10).
               10  KR-WAREHOUSE        PIC X(20).
               10  KR-LOCATION         PIC X(20).
               10  KR-LOT-CODE         PIC X(30).
               10  KR-SERIAL-NO        PIC X(40).
               10  KR-QUANTITY         PIC S9(13)V9(6).
               10  KR-UOM-CODE         PIC X(10).
               10  KR-ALLOW-PARTIAL    PIC X.
                   88  KR-PARTIAL-OK             VALUE "Y".
               10  KR-PRIORITY         PIC 9(2).
               10  KR-REQUIRED-DATE    PIC 9(8).
               10  KR-REASON-CODE      PIC X(20).
               10  KR-REASON-TEXT      PIC X(300).
      *>       ---- result ----
               10  KR-RESERVED-QTY     PIC S9(13)V9(6).
               10  KR-SHORTFALL-QTY    PIC S9(13)V9(6).
               10  KR-PICKED-QTY       PIC S9(13)V9(6).
               10  KR-OWN-RESERVED     PIC S9(13)V9(6).
               10  KR-RESERVATION-ID   PIC S9(17).
               10  KR-LINES-DONE       PIC S9(4).
               10  KR-LINES-SHORT      PIC S9(4).
               10  KR-INVALID-CNT      PIC S9(4).
               10  KR-EXCEPTION-ID     PIC S9(17).
               10  KR-DOC-REF          PIC X(30).
               10  KR-MESSAGE-TEXT     PIC X(200).
      *>       ---- picking list window ----
               10  KR-FROM-LINE        PIC S9(9) COMP-5.
               10  KR-WIN-MAX          PIC S9(4).
               10  KR-WIN-CNT          PIC S9(4).
               10  KR-MORE-LINES       PIC X.
               10  KR-PICKS OCCURS 20 TIMES.
                   15  KR-P-LINE-NUM   PIC S9(9) COMP-5.
                   15  KR-P-ITEM       PIC X(40).
                   15  KR-P-REV        PIC X(4).
                   15  KR-P-DESC       PIC X(40).
                   15  KR-P-UOM        PIC X(10).
                   15  KR-P-TO-PICK    PIC S9(13)V9(6).
                   15  KR-P-RESERVED   PIC S9(13)V9(6).
                   15  KR-P-PICKED     PIC S9(13)V9(6).
                   15  KR-P-WAREHOUSE  PIC X(20).
                   15  KR-P-LOCATION   PIC X(20).
                   15  KR-P-LOT        PIC X(30).
                   15  KR-P-SERIAL     PIC X(40).
                   15  KR-P-TRACED     PIC X.
                   15  KR-P-NEED-DATE  PIC 9(8).
                   15  KR-P-SHORTAGE   PIC S9(13)V9(6).
                   15  KR-P-STATUS     PIC X(12).
                   15  KR-P-NOTE       PIC X(60).
      *>       ---- future supply cover (KIT-008), appended in 2026
      *>       so that existing offsets did not move ----
               10  KR-SUPPLY-TYPE      PIC X(12).
                   88  KR-SUPPLY-KNOWN           VALUE "PURCHASE"
                                                       "WORKSORDER"
                                                       "KIT"
                                                       "TRANSFER".
               10  KR-SUPPLY-REF       PIC X(40).
               10  KR-SUPPLY-LINE      PIC S9(9) COMP-5.
               10  KR-SUPPLY-DUE       PIC 9(8).
               10  KR-SUPPLY-QTY       PIC S9(13)V9(6).
               10  KR-SUPPLY-COMMITTED PIC S9(13)V9(6).
               10  KR-SUPPLY-LATE      PIC X.
               10  KR-COMMIT-ID        PIC S9(17).
               10  KR-COVERED-QTY      PIC S9(13)V9(6).
               10  KR-UNCOVERED-QTY    PIC S9(13)V9(6).
