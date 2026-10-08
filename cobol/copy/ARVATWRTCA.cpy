      *>================================================================
      *> ARVATWRTCA - COMMAREA FOR ARVATWRT, THE VAT RECORD WRITER
      *>   CA-FUNCTION: LOOKUP  rate, liability, mechanism, boxes and
      *>                        the statutory invoice wording
      *>                WRITE   one fin.VatTransaction digital record
      *>   Every sales document that carries VAT writes its analysis
      *>   here, so the nine box return and the VAT exception reports
      *>   see one record per document and code.
      *>   REQ: AR-023, AR-026, FIN-017, FIN-018
      *> 2019-04-01 KMP CR2410 MAKING TAX DIGITAL - DIGITAL RECORDS
      *> 2026-10-05 STERP fin.VatTransaction
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  VW-BODY.
               10  VW-SOURCE-MODULE    PIC X(6).
               10  VW-DOC-TYPE         PIC X(20).
               10  VW-DOC-REF          PIC X(40).
               10  VW-LINE-NUM         PIC S9(9) COMP-5.
               10  VW-PARTY-TYPE       PIC X(10).
               10  VW-PARTY-CODE       PIC X(20).
               10  VW-PARTY-VAT-NO     PIC X(15).
               10  VW-TAX-POINT        PIC 9(8).
               10  VW-POSTING-DATE     PIC 9(8).
               10  VW-PERIOD-KEY       PIC X(10).
               10  VW-JOURNAL-ID       PIC S9(9) COMP-5.
               10  VW-VAT-CODE         PIC X(4).
               10  VW-RATE-PCT         PIC S9(3)V9(4) COMP-3.
               10  VW-LIABILITY        PIC X(12).
               10  VW-MECHANISM        PIC X(12).
               10  VW-DIRECTION        PIC X(6).
               10  VW-SELF-ACCOUNTS    PIC X.
               10  VW-EVIDENCE-REQ     PIC X.
               10  VW-WORDING          PIC X(200).
               10  VW-CURRENCY         PIC X(3).
               10  VW-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  VW-NET              PIC S9(11)V99 COMP-3.
               10  VW-VAT              PIC S9(11)V99 COMP-3.
               10  VW-NET-GBP          PIC S9(11)V99 COMP-3.
               10  VW-VAT-GBP          PIC S9(11)V99 COMP-3.
               10  VW-RECOVERABLE-GBP  PIC S9(11)V99 COMP-3.
               10  VW-VAT-BOX          PIC 9(2).
               10  VW-NET-BOX          PIC 9(2).
               10  VW-ADJUST-TYPE      PIC X(16).
               10  VW-LATE-ENTRY       PIC X.
               10  VW-EVIDENCE-REF     PIC X(60).
               10  VW-PLACE-OF-SUPPLY  PIC X(20).
               10  VW-TXN-ID           PIC S9(9) COMP-5.
               10  VW-RATE-FOUND       PIC X.
               10  FILLER              PIC X(40).
