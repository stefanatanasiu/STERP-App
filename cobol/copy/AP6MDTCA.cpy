      *>================================================================
      *> AP6MDTCA - COMMAREA for AP6MDATE (six month unpaid input VAT
      *>            date rule and repayment arithmetic).
      *> FUNCTIONS  : TRIGGER  later of supply/due date plus six months
      *>              REPAY    VAT to repay on the unpaid proportion
      *>              RECLAIM  VAT to reclaim once the invoice is paid
      *>              PERIOD   VAT return period key for a date
      *> 2026-10-06 STERP AP-029 ORIGINAL
      *>================================================================
      *> REQ: AP-029
       01  DFHCOMMAREA.
           05  SM-FUNC                 PIC X(8).
               88  SM-F-TRIGGER                  VALUE "TRIGGER".
               88  SM-F-REPAY                    VALUE "REPAY".
               88  SM-F-RECLAIM                  VALUE "RECLAIM".
               88  SM-F-PERIOD                   VALUE "PERIOD".
           05  SM-RC                   PIC 9(2).
               88  SM-OK                         VALUE 00.
               88  SM-WARNING                    VALUE 04.
               88  SM-ERROR                      VALUE 08.
           05  SM-REASON               PIC X(8).
           05  SM-MESSAGE              PIC X(120).
      *>   dates are the YYYYMMDD numbers the purchase ledger carries
           05  SM-SUPPLY-DATE          PIC 9(8).
           05  SM-DUE-DATE             PIC 9(8).
           05  SM-AS-AT                PIC 9(8).
           05  SM-PAID-DATE            PIC 9(8).
           05  SM-LATER-DATE           PIC 9(8).
           05  SM-TRIGGER-DATE         PIC 9(8).
           05  SM-MONTHS               PIC S9(3).
           05  SM-DUE-FOR-REPAY        PIC X.
               88  SM-REPAYABLE                  VALUE "Y".
           05  SM-DAYS-OVER            PIC S9(7).
           05  SM-CLAIMED-VAT          PIC S9(13)V99.
           05  SM-GROSS                PIC S9(13)V99.
           05  SM-RESIDUAL             PIC S9(13)V99.
           05  SM-ALREADY-REPAID       PIC S9(13)V99.
           05  SM-REPAY-VAT            PIC S9(13)V99.
           05  SM-RECLAIM-VAT          PIC S9(13)V99.
           05  SM-UNPAID-PCT           PIC S9(5)V9(4).
           05  SM-PERIOD-DATE          PIC 9(8).
           05  SM-PERIOD-KEY           PIC X(10).
