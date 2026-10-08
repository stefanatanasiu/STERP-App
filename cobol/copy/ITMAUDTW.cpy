      *>================================================================
      *> ITMAUDTW - parameter block for the internal subprogram
      *>            ITMAUDIT (audit evidence helper).
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE
      *>
      *> ITMAUDIT is a thin wrapper over the common service AUDITLOG.
      *> The stock control programs keep one standard COMMAREA header
      *> in scope at a time, so they pass plain parameters instead of
      *> copying AUDITLOGCA next to their own COMMAREA.
      *>
      *>     CALL "ITMAUDIT" USING ITA-BLOCK
      *>
      *> Every material change to item master data, every lifecycle
      *> decision and every authorised stock correction writes one row
      *> so that a reviewer can see who changed what, when and why.
      *>================================================================
      *> REQ: COM-013, ITM-004
       01  ITA-BLOCK.
           05  ITA-COMPANY             PIC X(10) VALUE SPACES.
           05  ITA-USER                PIC X(20) VALUE SPACES.
           05  ITA-ENTITY-TYPE         PIC X(40) VALUE SPACES.
           05  ITA-ENTITY-KEY          PIC X(60) VALUE SPACES.
           05  ITA-ACTION              PIC X(20) VALUE SPACES.
           05  ITA-FIELD-NAME          PIC X(40) VALUE SPACES.
           05  ITA-OLD-VALUE           PIC X(100) VALUE SPACES.
           05  ITA-NEW-VALUE           PIC X(100) VALUE SPACES.
           05  ITA-REASON              PIC X(200) VALUE SPACES.
           05  ITA-AUTHORITY-REF       PIC X(40) VALUE SPACES.
           05  ITA-CORRELATION         PIC X(36) VALUE SPACES.
           05  ITA-RC                  PIC 9(2) VALUE 0.
           05  ITA-MSG                 PIC X(200) VALUE SPACES.
