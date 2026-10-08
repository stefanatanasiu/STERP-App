      *>================================================================
      *> DESPATCHCA - COMMAREA for DESPATCH, despatch confirmation,
      *>              cost of sale and the transfer of control.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> SO-013, SO-007, SO-022 and D-03.  CONFIRM is one transaction
      *> (BEGN..COMT).  Inside it the credit position is re-tested, the
      *> quantity is checked against the permitted remaining demand and
      *> against physical availability, the period is tested, the
      *> reference is drawn from NUMGEN series SODESP, so.Despatch and
      *> so.DespatchLine are written, owned stock is reduced through
      *> STKMOVE, the reservation is consumed through STKRESV and the
      *> cost of sale is posted ONCE by staging fin.JournalStage and
      *> calling fin.usp_PostJournal under the business event
      *> SO.DESPATCH.COGS.  Anything that fails rolls the lot back:
      *> a failed confirmation must never look partly shipped.
      *>
      *> Repeating the instruction with the same CA-CORRELATION-ID
      *> returns the despatch that already exists with return code 04
      *> and consumes no stock and posts no second journal (COM-015).
      *>
      *> D-03 transfer of control.  Domestic sales pass control at
      *> despatch confirmation.  Exports follow the Incoterms rule:
      *> EXW, FCA and FOB pass at handover to the carrier; CPT, CIP,
      *> DAP, DPU and DDP pass on delivery.  The recognition point is
      *> recorded, not assumed.  Goods leaving on delivered terms in
      *> the last two working days of a period are flagged for cut off
      *> review because control has probably not passed by period end.
      *>
      *> CA-FUNCTION values
      *>   CONFIRM   confirm the despatch (the transaction above)
      *>   REVERSE   an authorised reversal.  It writes a compensating
      *>             movement and a compensating journal; nothing is
      *>             deleted and the original stays visible.
      *>   ENQUIRE   read a despatch and its lines
      *>
      *> REASON CODES
      *>   DSP0001 company, order or despatch reference required
      *>   DSP0002 order or order line not known
      *>   DSP0003 nothing outstanding to despatch
      *>   DSP0004 quantity exceeds the remaining demand or the
      *>           physical availability
      *>   DSP0005 the posting period is not open
      *>   DSP0006 credit check holds this despatch
      *>   DSP0007 the cost of sale could not be posted
      *>   DSP0008 unsupported function
      *>   DSP0009 despatch not known
      *>   DSP0010 a reversal needs an approval reference and a reason
      *>   DSP0011 the despatch is already reversed
      *>   DSP0012 another confirmation of this instruction is running
      *>   DSP0013 more lines than the table can carry
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-07-20 RJH  CR0951 DESPATCH CONFIRMATION (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - DESPATCH AND POSTING DATES
      *> 2004-10-11 DJM  CR1204 COGS POSTED FROM THE DESPATCH, ONCE
      *> 2021-01-04 DLS  CR3004 BREXIT - INCOTERMS CONTROL POINT
      *> 2026-10-06 STERP SO-013, SO-007, SO-022, D-03
      *>================================================================
      *> REQ: SO-013, SO-007, SO-022
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DS-BODY.
               10  DS-DESPATCH-REF     PIC X(30).
               10  DS-DESPATCH-ID      PIC S9(17).
               10  DS-ORDER-NUMBER     PIC X(30).
               10  DS-CUSTOMER         PIC X(20).
               10  DS-SITE             PIC X(10).
               10  DS-PICKLIST-REF     PIC X(30).
               10  DS-DESPATCH-DATE    PIC 9(8).
               10  DS-POSTING-DATE     PIC 9(8).
               10  DS-FISCAL-YEAR      PIC S9(4) SIGN LEADING SEPARATE.
               10  DS-PERIOD-NO        PIC S9(4) SIGN LEADING SEPARATE.
               10  DS-PERIOD-STATUS    PIC X(10).
               10  DS-STATUS           PIC X(12).
      *>       carrier and export evidence (SO-026 is PROOFEXP's job;
      *>       what is captured here is what the despatch knows)
               10  DS-INCOTERMS        PIC X(3).
               10  DS-INCOTERMS-PLACE  PIC X(60).
               10  DS-CARRIER-CODE     PIC X(20).
               10  DS-CARRIER-REF      PIC X(60).
               10  DS-PACK-COUNT       PIC S9(9) COMP-5.
               10  DS-GROSS-WEIGHT     PIC S9(13)V9(6).
               10  DS-IS-EXPORT        PIC X.
                   88  DS-EXPORT                 VALUE "Y".
               10  DS-DEST-COUNTRY     PIC X(2).
               10  DS-CUSTOMER-EORI    PIC X(17).
               10  DS-EXPORT-DECL-REF  PIC X(40).
               10  DS-ADDRESS-SNAP     PIC X(300).
      *>       D-03 transfer of control
               10  DS-CONTROL-POINT    PIC X(12).
                   88  DS-AT-DESPATCH            VALUE "DESPATCH".
                   88  DS-AT-HANDOVER            VALUE "HANDOVER".
                   88  DS-AT-DELIVERY            VALUE "DELIVERY".
               10  DS-CONTROL-DATE     PIC 9(8).
               10  DS-CONTROL-BASIS    PIC X(100).
               10  DS-IS-CUTOFF        PIC X.
                   88  DS-NEEDS-CUTOFF           VALUE "Y".
               10  DS-PERIOD-END-DATE  PIC 9(8).
               10  DS-WORKDAYS-LEFT    PIC S9(5).
      *>       SO-007 - the credit position is tested again here
               10  DS-CREDIT-DECISION  PIC X(10).
               10  DS-CREDIT-EXPOSURE  PIC S9(11)V99.
               10  DS-CREDIT-EXCESS    PIC S9(11)V99.
               10  DS-CREDIT-TEXT      PIC X(200).
               10  DS-CREDIT-OVERRIDE  PIC S9(9) COMP-5.
      *>       the cost of sale, posted exactly once
               10  DS-COGS-EVENT       PIC X(30).
               10  DS-COGS-GBP         PIC S9(11)V99.
               10  DS-JOURNAL-ID       PIC S9(17).
               10  DS-JOURNAL-NO       PIC X(30).
               10  DS-WAS-DUPLICATE    PIC X.
                   88  DS-IS-DUPLICATE           VALUE "Y".
               10  DS-IDEM-OUTCOME     PIC X(12).
      *>       REVERSE - compensating, never destructive
               10  DS-REVERSAL-OF      PIC X(30).
               10  DS-REVERSE-APPR     PIC X(40).
               10  DS-REVERSE-REASON   PIC X(200).
               10  DS-REVERSED-BY      PIC X(30).
               10  DS-LINE-CNT         PIC 9(3).
               10  DS-LINES OCCURS 30 TIMES.
                   15  DS-L-LINE-NUM   PIC S9(9) COMP-5.
                   15  DS-L-ORDER-LINE PIC S9(9) COMP-5.
                   15  DS-L-SCHEDULE   PIC S9(9) COMP-5.
                   15  DS-L-SOL-ID     PIC S9(17).
                   15  DS-L-ITEM       PIC X(40).
                   15  DS-L-REVISION   PIC X(4).
                   15  DS-L-DESC       PIC X(60).
                   15  DS-L-PICKED     PIC S9(13)V9(6).
                   15  DS-L-QTY        PIC S9(13)V9(6).
                   15  DS-L-UOM        PIC X(10).
                   15  DS-L-FACTOR     PIC S9(9)V9(9).
                   15  DS-L-STOCK-QTY  PIC S9(13)V9(6).
                   15  DS-L-WHSE       PIC X(20).
                   15  DS-L-LOCATION   PIC X(20).
                   15  DS-L-LOT        PIC X(30).
                   15  DS-L-SERIAL     PIC X(40).
                   15  DS-L-UNIT-COST  PIC S9(11)V9(6).
                   15  DS-L-COGS-GBP   PIC S9(11)V99.
                   15  DS-L-COST-METH  PIC X(4).
                   15  DS-L-ALLOWED    PIC S9(13)V9(6).
                   15  DS-L-AVAILABLE  PIC S9(13)V9(6).
                   15  DS-L-REMAINING  PIC S9(13)V9(6).
                   15  DS-L-MOVE-ID    PIC S9(17).
                   15  DS-L-RES-ID     PIC S9(17).
                   15  DS-L-KIT-REF    PIC X(30).
                   15  DS-L-STATUS     PIC X(12).
                   15  DS-L-MESSAGE    PIC X(100).
