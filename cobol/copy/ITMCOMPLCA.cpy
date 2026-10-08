      *>================================================================
      *> ITMCOMPLCA - COMMAREA for ITMCOMPL, UK VAT classification,
      *>              customs data and product compliance.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *>
      *> CA-FUNCTION values
      *>   VATCODE   the effective VAT code for CO-DIRECTION at the tax
      *>             point CO-TAX-POINT.  A liability change applies to
      *>             new documents by tax point only, and an item with
      *>             no classification may not be invoiced (ITM-025).
      *>   VATSET    record an effective dated liability with the
      *>             evidence or HMRC reference behind it
      *>   CUSTOMS   the customs data for a declaration: commodity
      *>             code, origin, preference evidence, export control,
      *>             supplementary unit and net weight (ITM-026)
      *>   CUSTCHK   validate the customs data before an international
      *>             despatch or import receipt.  A missing or badly
      *>             formed commodity code or origin is a blocking
      *>             exception for the customs hand off.
      *>   COMPLY    product compliance: UKCA/CE and the declaration of
      *>             conformity, dangerous goods classification, UK
      *>             REACH, WEEE, batteries (ITM-027)
      *>   DGCHECK   the dangerous goods detail a despatch note must
      *>             show, or 16 when the item is not dangerous goods
      *>   PACKAGE   packaging composition by material and weight with
      *>             recycled plastic content, for the packaging data
      *>             report and Plastic Packaging Tax
      *>
      *> CO-DIRECTION : SALE or PURCH.  CO-DRC-GOODS is "Y" for the
      *> domestic reverse charge specified goods (mobile telephones and
      *> computer chips), which drives the DRC code and the wording.
      *>
      *> REASON CODES
      *>   CMP0001 item not known in this company
      *>   CMP0002 no VAT classification effective at that tax point
      *>   CMP0003 non standard liability has no supporting evidence
      *>   CMP0004 no customs data effective at that date
      *>   CMP0005 commodity code is missing or badly formed
      *>   CMP0006 country of origin is missing or badly formed
      *>   CMP0007 export licence required but not recorded
      *>   CMP0008 compliance flag enabled but the data is missing
      *>   CMP0009 unsupported function
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-01-30 RJH  CR0402 VAT LIABILITY BY ITEM
      *> 2007-06-01 KMP  CR1760 MOBILE PHONES DOMESTIC REVERSE CHARGE
      *> 2021-01-01 ANW  CR4410 BREXIT - UK GLOBAL TARIFF, UKCA
      *> 2022-04-01 ANW  CR4702 PLASTIC PACKAGING TAX
      *> 2026-10-05 STERP ITM-025 TO ITM-027
      *>================================================================
      *> REQ: ITM-025, ITM-026, ITM-027
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CO-BODY.
               10  CO-ITEM-CODE        PIC X(40).
               10  CO-DIRECTION        PIC X(6).
               10  CO-TAX-POINT        PIC 9(8).
               10  CO-VAT-CODE         PIC X(4).
               10  CO-VAT-RATE         PIC S9(3)V9(4).
               10  CO-VAT-EVIDENCE     PIC X(100).
               10  CO-VAT-FROM         PIC 9(8).
               10  CO-VAT-TO           PIC 9(8).
               10  CO-DRC-GOODS        PIC X.
               10  CO-DRC-WORDING      PIC X(100).
               10  CO-COMMODITY-CODE   PIC X(10).
               10  CO-ORIGIN-COUNTRY   PIC X(2).
               10  CO-PREFERENTIAL     PIC X.
               10  CO-PREF-EVIDENCE    PIC X(60).
               10  CO-DUTY-RATE        PIC S9(3)V9(4).
               10  CO-EXPORT-CONTROL   PIC X.
               10  CO-EXPORT-LICENCE   PIC X(40).
               10  CO-SUPP-UNIT        PIC X(10).
               10  CO-NET-WEIGHT-KG    PIC S9(9)V9(6).
               10  CO-CUSTOMS-FROM     PIC 9(8).
               10  CO-UKCA-MARKED      PIC X.
               10  CO-CONFORMITY-REF   PIC X(40).
               10  CO-DG-UN-NUMBER     PIC X(8).
               10  CO-DG-CLASS         PIC X(8).
               10  CO-DG-PACK-GROUP    PIC X(4).
               10  CO-REACH-STATUS     PIC X(20).
               10  CO-WEEE-CATEGORY    PIC X(20).
               10  CO-BATTERY-CATEGORY PIC X(20).
               10  CO-DESPATCH-QTY     PIC S9(13)V9(6).
               10  CO-PKG-TOTAL-G      PIC S9(13)V9(6).
               10  CO-PKG-PLASTIC-G    PIC S9(13)V9(6).
               10  CO-PKG-RECYCLED-G   PIC S9(13)V9(6).
               10  CO-PPT-LIABLE       PIC X.
               10  CO-VALID            PIC X.
                   88  CO-IS-VALID               VALUE "Y".
               10  CO-FAULT-CNT        PIC 9(3).
               10  CO-LINE-CNT         PIC 9(3).
               10  CO-LINES OCCURS 12 TIMES.
                   15  CO-L-MATERIAL   PIC X(20).
                   15  CO-L-TYPE       PIC X(20).
                   15  CO-L-WEIGHT-G   PIC S9(13)V9(6).
                   15  CO-L-RECYCLED   PIC S9(3)V9(4).
                   15  CO-L-TOTAL-G    PIC S9(13)V9(6).
                   15  CO-L-RECYC-G    PIC S9(13)V9(6).
                   15  CO-L-PPT        PIC X.
