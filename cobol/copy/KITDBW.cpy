      *>================================================================
      *> KITDBW  - control block for KITDB, the kit data access
      *>           subprogram.  KITDB is NOT linkable from .NET: it
      *>           takes four parameters.
      *>
      *>   CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
      *>                      KIT-LINE-REC, KIT-ITEM-REC
      *>
      *> FUNCTIONS (KD-FUNC)
      *>   HDRGET   read kit.ExecutionKit by KD-COMPANY + KD-KIT-REF
      *>   HDRINS   insert a draft execution kit from KIT-HEADER-REC
      *>   HDRSTAT  update the status (and release stamp)
      *>   HDRQTY   update completed, rejected and WIP columns
      *>   HDRFRZ   write the frozen release assumptions
      *>   LINOPEN  open a cursor over the kit's material lines
      *>   LINNEXT  fetch the next line into KIT-LINE-REC
      *>   LINCLOS  close the line cursor
      *>   LINGET   read one line by KD-LINE-NUM
      *>   LININS   insert a material line from KIT-LINE-REC
      *>   LINSAVE  update the quantity, status and substitution
      *>            columns of KIT-LINE-REC
      *>   ITEMGET  read inv.Item and cst.ItemCost into KIT-ITEM-REC
      *>   ISSINS   insert a kit.KitIssue movement record
      *>   GENINS   insert a kit.KitGenealogy record
      *>   PCKINS   insert a kit.KitPick row (what was taken off the
      *>            shelf, with the lot or serial actually selected)
      *>   PCKUSE   consume the open picks of a line, oldest first, by
      *>            KD-P-QTY stock units; KD-P-CONSUMED says how much
      *>            was actually covered by picks and KD-ROWS how many
      *>            pick rows were touched
      *>   SUPINS   insert a kit.KitSupplyCommit row (a kit line
      *>            covered by explicitly identified future supply)
      *>   SUPSUM   sum the ACTIVE committed quantity for the kit, or
      *>            for one line when KD-LINE-NUM is not zero, into
      *>            KD-S-COMMITTED
      *>   SUPFREE  release this kit's ACTIVE supply commitments
      *>            (KD-ROWS says how many)
      *>
      *> KD-RC  0 ok, 100 not found, -1 SQL failure (KD-ERRMSG).
      *> Cursors 5 and 6 are reserved for KITDB so that a caller may
      *> keep its own cursor open across a KITDB call.
      *>================================================================
      *> REQ: KIT-002, KIT-024
       01  KIT-DB-CTL.
           05  KD-FUNC                 PIC X(8) VALUE SPACES.
           05  KD-COMPANY              PIC X(10) VALUE SPACES.
           05  KD-SITE                 PIC X(10) VALUE SPACES.
           05  KD-USER                 PIC X(20) VALUE SPACES.
           05  KD-KIT-REF              PIC X(30) VALUE SPACES.
           05  KD-KIT-ID               PIC S9(17) VALUE 0.
           05  KD-LINE-NUM             PIC S9(9) COMP-5 VALUE 0.
           05  KD-ITEM-CODE            PIC X(40) VALUE SPACES.
           05  KD-NEW-STATUS           PIC X(14) VALUE SPACES.
           05  KD-REASON               PIC X(200) VALUE SPACES.
           05  KD-ONLY-OPEN            PIC X VALUE "N".
           05  KD-RC                   PIC S9(9) COMP-5 VALUE 0.
           05  KD-ROWS                 PIC S9(9) COMP-5 VALUE 0.
           05  KD-ERRMSG               PIC X(200) VALUE SPACES.
           05  KD-NEW-ID               PIC S9(17) VALUE 0.
      *>   ---- kit.KitIssue insert (ISSINS) ----
           05  KD-ISS.
               10  KD-I-TYPE           PIC X(12) VALUE SPACES.
               10  KD-I-ITEM           PIC X(40) VALUE SPACES.
               10  KD-I-REV            PIC X(4) VALUE SPACES.
               10  KD-I-QTY            PIC S9(13)V9(6) VALUE 0.
               10  KD-I-UOM            PIC X(10) VALUE SPACES.
               10  KD-I-STOCK-QTY      PIC S9(13)V9(6) VALUE 0.
               10  KD-I-WAREHOUSE      PIC X(20) VALUE SPACES.
               10  KD-I-LOCATION       PIC X(20) VALUE SPACES.
               10  KD-I-LOT            PIC X(30) VALUE SPACES.
               10  KD-I-SERIAL         PIC X(40) VALUE SPACES.
               10  KD-I-ACT-DATE       PIC 9(8) VALUE 0.
               10  KD-I-POST-DATE      PIC 9(8) VALUE 0.
               10  KD-I-UNIT-COST      PIC S9(11)V9(6) VALUE 0.
               10  KD-I-VALUE          PIC S9(11)V99 VALUE 0.
               10  KD-I-REASON-CODE    PIC X(20) VALUE SPACES.
               10  KD-I-REASON         PIC X(200) VALUE SPACES.
               10  KD-I-APPROVAL       PIC S9(17) VALUE 0.
               10  KD-I-ORIG-ISSUE     PIC S9(17) VALUE 0.
               10  KD-I-MOVEMENT       PIC S9(17) VALUE 0.
      *>   ---- kit.KitGenealogy insert (GENINS) ----
           05  KD-GEN.
               10  KD-G-COMPLETION     PIC S9(17) VALUE 0.
               10  KD-G-PARENT         PIC X(40) VALUE SPACES.
               10  KD-G-PAR-LOT        PIC X(30) VALUE SPACES.
               10  KD-G-PAR-SERIAL     PIC X(40) VALUE SPACES.
               10  KD-G-ITEM           PIC X(40) VALUE SPACES.
               10  KD-G-LOT            PIC X(30) VALUE SPACES.
               10  KD-G-SERIAL         PIC X(40) VALUE SPACES.
               10  KD-G-QTY            PIC S9(13)V9(6) VALUE 0.
               10  KD-G-SUBSTITUTE     PIC X VALUE "N".
               10  KD-G-PLANNED        PIC X(40) VALUE SPACES.
               10  KD-G-ISSUE-ID       PIC S9(17) VALUE 0.
      *>   ---- kit.KitPick insert (PCKINS) ----
           05  KD-PCK.
               10  KD-P-TYPE           PIC X(10) VALUE SPACES.
               10  KD-P-QTY            PIC S9(13)V9(6) VALUE 0.
               10  KD-P-UOM            PIC X(10) VALUE SPACES.
               10  KD-P-WAREHOUSE      PIC X(20) VALUE SPACES.
               10  KD-P-LOCATION       PIC X(20) VALUE SPACES.
               10  KD-P-LOT            PIC X(30) VALUE SPACES.
               10  KD-P-SERIAL         PIC X(40) VALUE SPACES.
               10  KD-P-PICK-DATE      PIC 9(8) VALUE 0.
               10  KD-P-REASON-CODE    PIC X(20) VALUE SPACES.
               10  KD-P-REASON         PIC X(200) VALUE SPACES.
               10  KD-P-CONSUMED       PIC S9(13)V9(6) VALUE 0.
      *>   ---- kit.KitSupplyCommit (SUPINS/SUPSUM/SUPFREE) ----
           05  KD-SUP.
               10  KD-S-TYPE           PIC X(12) VALUE SPACES.
               10  KD-S-REF            PIC X(40) VALUE SPACES.
               10  KD-S-LINE           PIC S9(9) COMP-5 VALUE 0.
               10  KD-S-QTY            PIC S9(13)V9(6) VALUE 0.
               10  KD-S-UOM            PIC X(10) VALUE SPACES.
               10  KD-S-DUE-DATE       PIC 9(8) VALUE 0.
               10  KD-S-REQ-DATE       PIC 9(8) VALUE 0.
               10  KD-S-LATE           PIC X VALUE "N".
               10  KD-S-REASON-CODE    PIC X(20) VALUE SPACES.
               10  KD-S-REASON         PIC X(200) VALUE SPACES.
               10  KD-S-COMMITTED      PIC S9(13)V9(6) VALUE 0.
