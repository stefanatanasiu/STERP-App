      *>================================================================
      *> ARINSOLVCA - COMMAREA FOR ARINSOLV, INSOLVENCY, RETENTION OF
      *>              TITLE AND DEBT RECOVERY
      *>   CA-FUNCTION: RECORD    record an insolvency event, stop the
      *>                          account and hold open orders
      *>                ROTLIST   list unpaid goods supplied under a
      *>                          retention of title clause
      *>                PAPSENT   evidence a Pre-Action Protocol
      *>                          letter of claim to an individual
      *>                HANDOFF   hand off to agency or solicitor
      *>   Debts are not removed from the ledger until written off
      *>   under authority (AR-027).
      *>   REQ: AR-027, AR-016
      *> 2003-06-30 DJM CR1601 INSOLVENCY AND ROT CLAIM REGISTER
      *> 2026-10-05 STERP PRE-ACTION PROTOCOL FOR DEBT CLAIMS
      *> 2026-10-07 STERP CR2614 DIVIDEND RECEIVED IS STATED IN ITS OWN
      *>                 CURRENCY AND REPORTED IN STERLING
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  IS-BODY.
               10  IS-CUSTOMER         PIC X(20).
               10  IS-EVENT-TYPE       PIC X(16).
                   88  IS-ADMINISTRATION         VALUE "ADMIN".
                   88  IS-LIQUIDATION            VALUE "LIQUIDATION".
                   88  IS-CVA                    VALUE "CVA".
                   88  IS-BANKRUPTCY             VALUE "BANKRUPTCY".
               10  IS-EVENT-DATE       PIC 9(8).
               10  IS-OFFICE-HOLDER    PIC X(120).
               10  IS-PROOF-REF        PIC X(40).
               10  IS-LEGAL-FORM       PIC X(14).
               10  IS-IS-INDIVIDUAL    PIC X.
               10  IS-OUTSTANDING      PIC S9(11)V99 COMP-3.
      *>       dividend received from the office holder.  The caller
      *>       states the amount and the currency it arrived in; AR
      *>       converts it and reports IS-DIVIDEND-GBP (AR-027).
               10  IS-DIVIDEND-AMT     PIC S9(11)V99 COMP-3.
               10  IS-DIVIDEND-CCY     PIC X(3).
               10  IS-DIVIDEND-DATE    PIC 9(8).
               10  IS-DIVIDEND-GBP     PIC S9(11)V99 COMP-3.
               10  IS-ORDERS-HELD      PIC S9(9) COMP-5.
               10  IS-ACCOUNT-STOPPED  PIC X.
      *>       retention of title
               10  IS-ROT-CNT          PIC 9(3).
               10  IS-ROT-VALUE        PIC S9(11)V99 COMP-3.
               10  IS-ROT-TAB.
                   15  IS-ROTS         OCCURS 20 TIMES.
                       20  IS-R-INVOICE        PIC X(30).
                       20  IS-R-ITEM           PIC X(40).
                       20  IS-R-DESC           PIC X(60).
                       20  IS-R-SERIAL         PIC X(40).
                       20  IS-R-QTY            PIC S9(9)V9(6) COMP-3.
                       20  IS-R-DESPATCH-DT    PIC 9(8).
                       20  IS-R-VALUE          PIC S9(11)V99 COMP-3.
      *>       pre-action protocol and recovery hand-off
               10  IS-PAP-SENT-DATE    PIC 9(8).
               10  IS-PAP-DAYS-ELAPSE  PIC S9(5).
               10  IS-PAP-DAYS-REQ     PIC S9(5).
               10  IS-HANDOFF-TYPE     PIC X(12).
               10  IS-HANDOFF-REF      PIC X(60).
               10  IS-HANDOFF-ALLOWED  PIC X.
               10  IS-BLOCK-REASON     PIC X(200).
               10  IS-CASE-ID          PIC S9(9) COMP-5.
               10  IS-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  FILLER              PIC X(22).
