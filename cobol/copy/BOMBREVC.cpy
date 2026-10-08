      *>================================================================
      *> BOMBREVC - parameter group of BOMBREV, the internal builder
      *>            that turns ONE bill plus ONE proposed change into
      *>            ONE proposed revision and validates it.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : BILLS OF MATERIAL
      *> PURPOSE  : BOM-024.  BOMBULK owns the transaction and the
      *>            all-or-nothing decision; BOMBREV owns the bill.
      *>              CALL "BOMBREV" USING BOM-BR-CTL BOM-BR-MSG
      *>            BOMBREV is deliberately NOT LINKable - it must not
      *>            be called from outside the run that wraps it in
      *>            BEGN / COMT, or a half-made revision could escape.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2016-03-21 AJS  CR2442 SPLIT OUT OF BOMBULK (SIZE)
      *> 2026-10-06 STERP VALIDATION OUTCOME RETURNED PER BILL
      *>================================================================
      *> REQ: BOM-013, BOM-024, BOM-029
       01  BOM-BR-CTL.
           05  PR-FUNC                 PIC X(8).
           05  PR-COMPANY              PIC X(10).
           05  PR-SITE                 PIC X(10).
           05  PR-USER                 PIC X(20).
           05  PR-CHANGE-REF           PIC X(30).
           05  PR-CHANGE-ID            PIC S9(9) COMP-5.
           05  PR-SEQ                  PIC S9(4).
           05  PR-BOM-ID               PIC S9(9) COMP-5.
           05  PR-ACTION               PIC X(10).
           05  PR-OLD-ITEM             PIC X(40).
           05  PR-NEW-ITEM             PIC X(40).
           05  PR-NEW-QTY              PIC S9(13)V9(6).
           05  PR-NEW-UOM              PIC X(10).
           05  PR-NEW-BASIS            PIC X(12).
           05  PR-NEW-SCRAP            PIC S9(5)V9(6).
           05  PR-NEW-LINE-NUM         PIC S9(4).
           05  PR-EFF-DATE             PIC 9(8).
           05  PR-RECORD-VALID         PIC X.
           05  PR-BOM-REF              PIC X(30).
           05  PR-BOM-REV              PIC X(4).
           05  PR-PARENT               PIC X(40).
           05  PR-SRC-STATUS           PIC X(12).
           05  PR-TREATMENT            PIC X(10).
           05  PR-NEW-REV              PIC X(4).
           05  PR-NEW-BOM-ID           PIC S9(9) COMP-5.
           05  PR-LINE-NUM             PIC S9(4).
           05  PR-OLD-QTY              PIC S9(13)V9(6).
           05  PR-RESULT-QTY           PIC S9(13)V9(6).
           05  PR-ERROR-CNT            PIC S9(4).
           05  PR-WARNING-CNT          PIC S9(4).
           05  PR-OUTCOME              PIC X(10).
               88  PR-APPLIED                    VALUE "APPLIED".
               88  PR-FAILED                     VALUE "FAILED".
               88  PR-SKIPPED                    VALUE "SKIPPED".
           05  PR-REASON               PIC X(8).
           05  PR-SQL-DEAD             PIC X.
       01  BOM-BR-MSG                  PIC X(120).
