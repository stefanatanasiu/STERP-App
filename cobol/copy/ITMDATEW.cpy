      *>================================================================
      *> ITMDATEW - parameter block for the internal subprogram ITMDATE.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE
      *>
      *> ITMDATE is a thin wrapper over the common service DATECALC.
      *> The stock control programs keep only one standard COMMAREA
      *> header in scope at a time, so they call ITMDATE with plain
      *> parameters instead of copying DATECALCCA next to their own
      *> COMMAREA and qualifying every CA- field.
      *>
      *>     CALL "ITMDATE" USING ITD-BLOCK
      *>
      *> ITD-FUNC : DAYSBTWN  ITD-DATE-2 minus ITD-DATE-1 -> ITD-NUM
      *>            ADDDAYS   ITD-DATE-1 + ITD-NUMBER     -> ITD-DATE-R
      *>            ADDMONTH  ITD-DATE-1 + ITD-NUMBER months
      *>            VALIDATE  is ITD-DATE-1 a real date?  (ITD-RC 8)
      *>            FORMAT    ITD-DATE-1 -> ITD-TEXT as dd/mm/yyyy
      *>            TODAY     the system date              -> ITD-DATE-R
      *> ITD-RC   : 00 ok, 04 adjusted, 08 invalid, 12 severe
      *>================================================================
      *> REQ: ITM-011, COM-018
       01  ITD-BLOCK.
           05  ITD-FUNC                PIC X(8) VALUE SPACES.
           05  ITD-DATE-1              PIC 9(8) VALUE 0.
           05  ITD-DATE-2              PIC 9(8) VALUE 0.
           05  ITD-NUMBER              PIC S9(9) VALUE 0.
           05  ITD-DATE-R              PIC 9(8) VALUE 0.
           05  ITD-NUM-R               PIC S9(9) VALUE 0.
           05  ITD-TEXT                PIC X(10) VALUE SPACES.
           05  ITD-RC                  PIC 9(2) VALUE 0.
           05  ITD-MSG                 PIC X(80) VALUE SPACES.
