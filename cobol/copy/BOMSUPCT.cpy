      *>================================================================
      *> BOMSUPCT - control record of the eligible supply pool.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : BILLS OF MATERIAL
      *> PURPOSE  : BOM-020 says eligible stock is consumed ONCE.  A
      *>            netted calculation therefore keeps one pool of
      *>            eligible free supply for the whole demand scope,
      *>            and every node that wants stock takes from it.
      *>            A stock unit or expected receipt cannot satisfy
      *>            two demands, and a supplied subassembly is not
      *>            also exploded for new material for the same units.
      *>              CALL "BOMSUPPL" USING BOM-SUP-CTL
      *>            Functions (BP-FUNC):
      *>              CLEAR  empty the pool and set the source
      *>              SET    declare BP-FREE / BP-INBOUND for an item
      *>              FETCH  read the live free stock of BP-ITEM from
      *>                     inv.StockBalance less inv.StockReservation
      *>              TAKE   consume up to BP-WANT, BP-TAKEN says how
      *>                     much was available
      *>              AVAIL  report the remaining free quantity
      *>              SNAP   remember the pool, RESET puts it back, so
      *>                     a maximum-buildable search can retry the
      *>                     same supply without reading it again
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-02-06 AJS  CR2295 NETTED PLANNING ADDED
      *> 2015-04-28 AJS  CR2312 SNAP / RESET FOR THE BUILDABLE SEARCH
      *>================================================================
      *> REQ: BOM-016, BOM-020
       01  BOM-SUP-CTL.
           05  BP-FUNC                 PIC X(8).
           05  BP-COMPANY              PIC X(10).
           05  BP-SITE                 PIC X(10).
           05  BP-SOURCE               PIC X(8).
           05  BP-ITEM                 PIC X(40).
           05  BP-REV                  PIC X(4).
           05  BP-FREE                 PIC S9(15)V9(6).
           05  BP-INBOUND              PIC S9(15)V9(6).
           05  BP-WANT                 PIC S9(15)V9(6).
           05  BP-TAKEN                PIC S9(15)V9(6).
           05  BP-TAKEN-INBOUND        PIC S9(15)V9(6).
           05  BP-REMAIN               PIC S9(15)V9(6).
           05  BP-USE-INBOUND          PIC X.
           05  BP-COUNT                PIC S9(4).
           05  BP-STATUS               PIC X(8).
               88  BP-OK                         VALUE "OK".
               88  BP-FULL                       VALUE "FULL".
               88  BP-NOTFOUND                   VALUE "NOTFOUND".
               88  BP-SQLFAIL                    VALUE "SQLFAIL".
           05  BP-MESSAGE              PIC X(200).
