      *>================================================================
      *> STATPROFCA - COMMAREA for STATPROF, the company statutory
      *>              profile, accounting reference date and filing
      *>              deadlines (FIN-030, XMD-013).
      *> AC-119: for a private company with a 31 March 2027 year end the
      *> obligations are accounts filing 31 December 2027, corporation
      *> tax payment 1 January 2028 and the CT600 by 31 March 2028.
      *> Functions: PROFILE   company framework, ARD and VAT profile
      *>            DEADLINE  derive the statutory obligations
      *>            ARDCHANG  validate a change of reference date
      *>            MAPCHECK  unmapped posting accounts block a
      *>                      statutory format statement
      *> 2001-06-05 R J HOLBROOK   2026-10-05 STERP OBLIGATION DIARY
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SP-BODY.
               10  SP-PERIOD-END       PIC 9(8).
               10  SP-NEW-PERIOD-END   PIC 9(8).
               10  SP-PERIOD-START     PIC 9(8).
               10  SP-COMPANY-TYPE     PIC X(12).
               10  SP-FIRST-PERIOD     PIC X.
               10  SP-INCORP-DATE      PIC 9(8).
               10  SP-REASON           PIC X(200).
      *>     ---- answers ----
               10  SP-FRAMEWORK        PIC X(10).
               10  SP-ARD              PIC X(5).
               10  SP-REG-NUMBER       PIC X(10).
               10  SP-VAT-REG-NO       PIC X(15).
               10  SP-VAT-STAGGER      PIC X(12).
               10  SP-VAT-SCHEME       PIC X(12).
               10  SP-JURISDICTION     PIC X(30).
               10  SP-LENGTH-MONTHS    PIC S9(4).
               10  SP-ARD-VALID        PIC X.
               10  SP-ACCOUNTS-DUE     PIC 9(8).
               10  SP-CTPAY-DUE        PIC 9(8).
               10  SP-CTRETURN-DUE     PIC 9(8).
               10  SP-CONFIRM-DUE      PIC 9(8).
               10  SP-UNMAPPED-CNT     PIC 9(5).
               10  SP-MAP-COMPLETE     PIC X.
               10  SP-DISALLOWABLE-GBP PIC S9(13)V99 COMP-3.
               10  SP-OBLIG-CNT        PIC 9(2).
               10  SP-OBLIG OCCURS 8 TIMES.
                   15  SP-O-TYPE       PIC X(16).
                   15  SP-O-DESC       PIC X(60).
                   15  SP-O-DUE-DATE   PIC 9(8).
                   15  SP-O-STATUS     PIC X(12).
                   15  SP-O-ROLE       PIC X(20).
