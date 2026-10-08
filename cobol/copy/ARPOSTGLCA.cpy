      *>================================================================
      *> ARPOSTGLCA - COMMAREA FOR ARPOSTGL, THE SALES LEDGER ROUTE
      *>              INTO THE GENERAL LEDGER
      *>   CA-FUNCTION: ACCOUNT   resolve one event leg to an account
      *>                POST      stage the legs and post the journal
      *>   Lines are staged in fin.JournalStage under the correlation
      *>   id and consumed by fin.usp_PostJournal, which owns the
      *>   balance, period, dimension and mapping rules.  Set
      *>   PG-OWN-TRAN to "N" when the caller already holds a BEGN.
      *>   REQ: AR-004, AR-012, AR-020, FIN-004, COM-014
      *> 1991-05-02 ORIGINAL MVS SALES LEDGER TO NOMINAL INTERFACE
      *> 2026-10-05 STERP fin.JournalStage / fin.usp_PostJournal
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PG-BODY.
               10  PG-EVENT-CODE       PIC X(30).
               10  PG-JOURNAL-TYPE     PIC X(12).
               10  PG-DESCRIPTION      PIC X(200).
               10  PG-SOURCE-MODULE    PIC X(6).
               10  PG-SOURCE-REF       PIC X(40).
               10  PG-DOC-DATE         PIC 9(8).
               10  PG-POSTING-DATE     PIC 9(8).
               10  PG-CURRENCY         PIC X(3).
               10  PG-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  PG-EVIDENCE-REF     PIC X(120).
               10  PG-SERIES-CODE      PIC X(20).
               10  PG-OWN-TRAN         PIC X.
               10  PG-LOOKUP-LEG       PIC X(12).
               10  PG-LOOKUP-ACCOUNT   PIC X(12).
               10  PG-LEG-CNT          PIC 9(3).
               10  PG-LEG-TAB.
                   15  PG-LEGS         OCCURS 30 TIMES.
                       20  PG-G-LEG-CODE       PIC X(12).
                       20  PG-G-ACCOUNT        PIC X(12).
                       20  PG-G-DEBIT          PIC S9(11)V99 COMP-3.
                       20  PG-G-CREDIT         PIC S9(11)V99 COMP-3.
                       20  PG-G-NARRATIVE      PIC X(100).
                       20  PG-G-VAT-CODE       PIC X(4).
                       20  PG-G-COST-CENTRE    PIC X(20).
                       20  PG-G-PROJECT        PIC X(20).
                       20  PG-G-SUBLEDGER      PIC X(6).
                       20  PG-G-SUBREF         PIC X(40).
                       20  PG-G-BANK-ACCT      PIC X(20).
               10  PG-JOURNAL-NO       PIC X(30).
               10  PG-JOURNAL-ID       PIC S9(9) COMP-5.
               10  PG-TOTAL-DEBIT      PIC S9(11)V99 COMP-3.
               10  PG-TOTAL-CREDIT     PIC S9(11)V99 COMP-3.
               10  PG-WAS-DUPLICATE    PIC X.
               10  FILLER              PIC X(40).
