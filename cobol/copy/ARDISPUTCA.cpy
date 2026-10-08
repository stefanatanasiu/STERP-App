      *>================================================================
      *> ARDISPUTCA - COMMAREA FOR ARDISPUT, DISPUTES AND COLLECTION
      *>   CA-FUNCTION: DISPUTE   raise/amend a disputed amount
      *>                PROMISE   record a promise to pay
      *>                ACTION    record a contact or a suppression
      *>                RESOLVE   resolve a case
      *>                REVIEW    sweep promises, mark broken ones
      *>   A dispute does not erase debt or automatically reduce
      *>   credit exposure (AR-015).
      *>   REQ: AR-015, AR-016, D-06
      *> 1995-09-04 DJM CR0501 QUERY LOG ADDED TO SALES LEDGER
      *> 2026-10-05 STERP PROMISES, OWNERSHIP, CONTACT RESTRICTION
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DS-BODY.
               10  DS-CASE-REF         PIC X(30).
               10  DS-CASE-ID          PIC S9(9) COMP-5.
               10  DS-CUSTOMER         PIC X(20).
               10  DS-CASE-TYPE        PIC X(12).
                   88  DS-IS-DISPUTE             VALUE "DISPUTE".
                   88  DS-IS-COLLECTION          VALUE "COLLECTION".
                   88  DS-IS-INSOLVENCY          VALUE "INSOLVENCY".
                   88  DS-IS-LEGAL               VALUE "LEGAL".
               10  DS-DOCUMENT-NO      PIC X(30).
               10  DS-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  DS-REASON-CODE      PIC X(20).
               10  DS-REASON           PIC X(300).
               10  DS-DISPUTED-AMT     PIC S9(11)V99 COMP-3.
               10  DS-RESIDUAL         PIC S9(11)V99 COMP-3.
               10  DS-UNDISPUTED-AMT   PIC S9(11)V99 COMP-3.
               10  DS-OWNER-USER       PIC X(20).
               10  DS-COLLECTION-STEP  PIC 9(2).
               10  DS-NEXT-ACTION-DT   PIC 9(8).
               10  DS-NEXT-ACTION      PIC X(200).
               10  DS-CONTACT-RESTRICT PIC X(100).
      *>       actions and promises
               10  DS-ACTION-TYPE      PIC X(14).
               10  DS-ACTION-DATE      PIC 9(8).
               10  DS-ACTION-SEQ       PIC S9(9) COMP-5.
               10  DS-CONTACT-NAME     PIC X(80).
               10  DS-NARRATIVE        PIC X(400).
               10  DS-PROMISE-AMT      PIC S9(11)V99 COMP-3.
               10  DS-PROMISE-DATE     PIC 9(8).
               10  DS-PROMISE-STATUS   PIC X(10).
               10  DS-SUPPRESS-REASON  PIC X(200).
               10  DS-BROKEN-COUNT     PIC S9(9) COMP-5.
      *>       resolution
               10  DS-STATUS           PIC X(12).
               10  DS-RESOLUTION       PIC X(400).
               10  DS-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  FILLER              PIC X(40).
