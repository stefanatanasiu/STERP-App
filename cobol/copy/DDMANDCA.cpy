      *>================================================================
      *> DDMANDCA - COMMAREA FOR DDMAND, BACS DIRECT DEBIT MANDATES
      *>   CA-FUNCTION: LODGE     record a signed mandate for lodgement
      *>                ACTIVATE  mandate lodged and usable
      *>                NOTICE    send the advance notice of a
      *>                          collection (baseline 10 working days)
      *>                SCHEDULE  plan a collection on a Bacs day
      *>                CANCEL    cancel the mandate with a reason
      *>   No collection is made without an active mandate or before
      *>   the advance notice has expired (AR-025).
      *>   REQ: AR-025, D-16
      *> 1997-10-02 DJM CR0888 BACS DIRECT DEBIT SCHEME
      *> 2026-10-05 STERP ADVANCE NOTICE AND GUARANTEE EVIDENCE
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DM-BODY.
               10  DM-CUSTOMER         PIC X(20).
               10  DM-MANDATE-REF      PIC X(18).
               10  DM-MANDATE-ID       PIC S9(9) COMP-5.
               10  DM-SERVICE-USER-NO  PIC X(6).
               10  DM-PAYER-NAME       PIC X(120).
               10  DM-SORT-CODE        PIC X(6).
               10  DM-ACCOUNT-NO       PIC X(8).
               10  DM-ROLL-NUMBER      PIC X(18).
               10  DM-SIGNED-DATE      PIC 9(8).
               10  DM-LODGED-DATE      PIC 9(8).
               10  DM-ADVANCE-DAYS     PIC S9(4).
               10  DM-ADVANCE-WRITTEN  PIC X.
               10  DM-GUARANTEE-SENT   PIC X.
               10  DM-STATUS           PIC X(12).
               10  DM-CANCEL-DATE      PIC 9(8).
               10  DM-CANCEL-REASON    PIC X(200).
      *>       collection planning
               10  DM-COLLECT-REF      PIC X(30).
               10  DM-COLLECT-ID       PIC S9(9) COMP-5.
               10  DM-NOTICE-SENT      PIC 9(8).
               10  DM-EARLIEST-DATE    PIC 9(8).
               10  DM-REQUEST-DATE     PIC 9(8).
               10  DM-PROCESS-DATE     PIC 9(8).
               10  DM-SUBMIT-DATE      PIC 9(8).
               10  DM-DUE-DATE         PIC 9(8).
               10  DM-AMOUNT-GBP       PIC S9(11)V99 COMP-3.
               10  DM-TXN-CODE         PIC X(4).
               10  DM-ITEM-CNT         PIC 9(3).
               10  DM-ITEM-TAB.
                   15  DM-ITEMS        OCCURS 20 TIMES.
                       20  DM-I-ITEM-ID        PIC S9(9) COMP-5.
                       20  DM-I-DOC-NO         PIC X(30).
                       20  DM-I-AMOUNT         PIC S9(11)V99 COMP-3.
               10  DM-FIRST-COLLECT    PIC X.
               10  DM-BLOCK-REASON     PIC X(200).
               10  DM-BACS-LEAD-DAYS   PIC S9(4).
               10  FILLER              PIC X(40).
