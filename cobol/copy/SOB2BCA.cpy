      *>================================================================
      *> SOB2BCA  - COMMAREA FOR SOB2B, BACK TO BACK AND DROP SHIPMENT
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> A LINK BETWEEN CUSTOMER DEMAND AND A SUPPLIER COMMITMENT IS
      *> EVIDENCE OF INTENT, NOT A GUARANTEE.  IT DOES NOT PROMISE THE
      *> SUPPLIER WILL DELIVER AND IT DOES NOT AUTHORISE AN UNAPPROVED
      *> SUPPLIER INVOICE (SO-021).
      *>
      *> A CHANGED OR CANCELLED SUPPLIER PROMISE IDENTIFIES THE
      *> AFFECTED CUSTOMER SCHEDULES FOR REVIEW AND RAISES AN
      *> EXCEPTION.  IT NEVER SILENTLY REWRITES THE CUSTOMER PROMISE -
      *> THE ORIGINAL PROMISE IS SERVICE EVIDENCE (SO-008, SO-021).
      *>
      *> FOR A DROP SHIPMENT, CONFIRMED DIRECT DELIVERY EVIDENCE
      *> RECORDS THE QUANTITY DELIVERED TO THE CUSTOMER AND CREATES NO
      *> WAREHOUSE ON HAND QUANTITY AT ALL.  UNDER THE APPROVED
      *> OWNERSHIP POLICY THE DIRECT FULFILMENT COST IS ACCRUED TO
      *> COGS AGAINST GRNI AND LATER CLEARED BY THE SUPPLIER INVOICE,
      *> RECOGNISED ON THE SUPPLIER CONFIRMED DELIVERY DATE (D-03).
      *> CUSTOMER INVOICING AND SUPPLIER SETTLEMENT STAY SEPARATE.
      *>
      *> CA-FUNCTION VALUES
      *>   LINK       LINK A SALES ORDER LINE TO A PURCHASE ORDER LINE
      *>              OR AN ASSEMBLY COMMITMENT AND SET SupplyType.
      *>   SUPPLYCHG  RECORD A CHANGED OR CANCELLED SUPPLIER PROMISE
      *>              AND LIST THE CUSTOMER SCHEDULES FOR REVIEW.
      *>   DIRECTDEL  RECORD CONFIRMED DIRECT DELIVERY EVIDENCE AND
      *>              ACCRUE THE DIRECT FULFILMENT COST.
      *>   ENQUIRE    READ BACK THE LINK AND ITS SUPPLY STATE.
      *>
      *> REASON CODES
      *>   B2B0001 ORDER NUMBER OR LINE NUMBER MISSING
      *>   B2B0002 SALES ORDER LINE NOT KNOWN
      *>   B2B0003 SUPPLY TYPE NOT RECOGNISED
      *>   B2B0004 LINKED PURCHASE ORDER OR LINE MISSING
      *>   B2B0005 A LINKED LINE MAY NOT BE SUPPLIED FROM FREE STOCK
      *>   B2B0006 DELIVERED QUANTITY IS ZERO OR NEGATIVE
      *>   B2B0007 DELIVERED QUANTITY EXCEEDS THE REMAINING DEMAND
      *>   B2B0008 NO DELIVERY EVIDENCE REFERENCE SUPPLIED
      *>   B2B0009 UNSUPPORTED FUNCTION
      *>   B2B0010 THE NOMINAL LEDGER REFUSED THE ACCRUAL
      *>   B2B0011 CHANGE TYPE NOT RECOGNISED
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-07-23 RJH  CR0498 BACK TO BACK PURCHASE LINKS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON PROMISE DATES
      *> 2008-02-11 DJM  CR1750 DIRECT DELIVERY ACCRUAL TO GRNI
      *> 2026-10-06 STERP SO-021 D-03 DROP SHIP OWNERSHIP POLICY
      *>================================================================
      *> REQ: SO-021
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BB-BODY.
               10  BB-ORDER-NUMBER     PIC X(30).
               10  BB-LINE-NUM         PIC S9(9) COMP-5.
               10  BB-SCHEDULE-NO      PIC S9(9) COMP-5.
               10  BB-ORDER-LINE-ID    PIC S9(17).
               10  BB-CUSTOMER         PIC X(20).
               10  BB-ITEM             PIC X(40).
               10  BB-DESCRIPTION      PIC X(60).
               10  BB-SUPPLY-TYPE      PIC X(12).
                   88  BB-STOCK                  VALUE "STOCK".
                   88  BB-BACKTOBACK             VALUE "BACKTOBACK".
                   88  BB-DROPSHIP               VALUE "DROPSHIP".
                   88  BB-MAKE                   VALUE "MAKE".
               10  BB-LINKED-PO        PIC X(30).
               10  BB-LINKED-PO-LINE   PIC S9(9) COMP-5.
               10  BB-LINKED-KIT-REF   PIC X(30).
               10  BB-SUPPLIER         PIC X(20).
               10  BB-SUPPLIER-PROMISE PIC 9(8).
               10  BB-NEW-PROMISE      PIC 9(8).
               10  BB-CHANGE-TYPE      PIC X(12).
                   88  BB-CHG-DELAY              VALUE "DELAY".
                   88  BB-CHG-CANCEL             VALUE "CANCEL".
                   88  BB-CHG-QTY                VALUE "QTYCHG".
                   88  BB-CHG-EARLY              VALUE "EARLY".
               10  BB-CHANGE-REASON    PIC X(200).
               10  BB-DELIVERY-DATE    PIC 9(8).
               10  BB-POSTING-DATE     PIC 9(8).
               10  BB-DELIVERED-QTY    PIC S9(9)V9(6) COMP-3.
               10  BB-REMAINING-QTY    PIC S9(9)V9(6) COMP-3.
               10  BB-UOM              PIC X(10).
               10  BB-UNIT-COST-GBP    PIC S9(11)V9(6) COMP-3.
               10  BB-COST-GBP         PIC S9(11)V99 COMP-3.
               10  BB-COGS-ACCOUNT     PIC X(12).
               10  BB-GRNI-ACCOUNT     PIC X(12).
               10  BB-EVENT-CODE       PIC X(30).
               10  BB-EVIDENCE-REF     PIC X(120).
               10  BB-CARRIER-REF      PIC X(60).
               10  BB-JOURNAL-ID       PIC S9(17).
               10  BB-JOURNAL-NUMBER   PIC X(30).
               10  BB-WAS-DUPLICATE    PIC X.
      *>       evidence that the promises of SO-021 have been kept
               10  BB-ONHAND-CREATED   PIC X.
               10  BB-MOVEMENT-ID      PIC S9(17).
               10  BB-SUPPLY-GUARANTEE PIC X.
               10  BB-INVOICE-APPROVED PIC X.
               10  BB-PROMISE-CHANGED  PIC X.
               10  BB-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  BB-EXCEPTION-COUNT  PIC S9(4) SIGN LEADING SEPARATE.
               10  BB-AFFECTED-CNT     PIC 9(3).
               10  BB-AFFECT-TRUNC     PIC X.
               10  BB-SCHEDULE-TAB.
                   15  BB-SCHEDULES    OCCURS 20 TIMES.
                       20  BB-S-ORDER      PIC X(30).
                       20  BB-S-LINE       PIC S9(9) COMP-5.
                       20  BB-S-SCHEDULE   PIC S9(9) COMP-5.
                       20  BB-S-CUSTOMER   PIC X(20).
                       20  BB-S-ITEM       PIC X(40).
                       20  BB-S-QTY        PIC S9(9)V9(6) COMP-3.
                       20  BB-S-DESP-QTY   PIC S9(9)V9(6) COMP-3.
                       20  BB-S-ORIG-PROM  PIC 9(8).
                       20  BB-S-CURR-PROM  PIC 9(8).
                       20  BB-S-STATUS     PIC X(10).
                       20  BB-S-ACTION     PIC X(20).
                       20  BB-S-MESSAGE    PIC X(80).
