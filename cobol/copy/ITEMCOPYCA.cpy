      *>================================================================
      *> ITEMCOPYCA - COMMAREA for ITEMCOPY, item copy and bulk
      *>              maintenance with preview.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *>
      *> A copy creates a new identity.  Balances, movement history,
      *> approvals, releases and serial identities are never inherited;
      *> only the descriptive, unit, supply, finance, compliance and
      *> substitute definitions may be carried across (ITM-021).
      *>
      *> CA-FUNCTION values
      *>   PREVIEW   show what a copy would create and what it would
      *>             exclude, without writing anything
      *>   COPY      perform the copy (preview first, then write)
      *>   BULKPREV  preview a bulk attribute change over IC-TARGETS
      *>   BULKAPPL  apply it: every target reports its own outcome, so
      *>             a mixed valid/invalid selection is partly applied
      *>             and fully explained.  CA-FUNCTION is eight bytes,
      *>             so the long form "BULKAPPLY" arrives as this.
      *>
      *> IC-INCLUDE-xxx switches select the groups to carry across.
      *> IC-FIELD-NAME / IC-NEW-VALUE drive the bulk change and are
      *> restricted to descriptive and planning attributes; a bulk
      *> change may not rewrite identity, units or traceability.
      *>
      *> REASON CODES
      *>   CPY0001 source item not known
      *>   CPY0002 target item code already exists
      *>   CPY0003 target item code is not a valid code (D-21)
      *>   CPY0004 field is not available for bulk maintenance
      *>   CPY0005 value is not valid for that field
      *>   CPY0006 nothing selected
      *>   CPY0007 unsupported function
      *>   CPY0008 some targets were rejected (see the lines)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-17 RJH  CR0855 COPY ITEM
      *> 2010-06-11 ANW  CR2402 BULK DESCRIPTIVE MAINTENANCE + PREVIEW
      *> 2026-10-05 STERP ITM-021 PER TARGET RESULTS AND HISTORY
      *> 2026-10-07 STERP ITM-021 PARTNER AND SUBSTITUTE GROUPS ARE
      *>                 CARRIED ACROSS WHEN SELECTED
      *>================================================================
      *> REQ: ITM-021
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  IC-BODY.
               10  IC-SOURCE-ITEM      PIC X(40).
               10  IC-TARGET-ITEM      PIC X(40).
               10  IC-TARGET-DESC      PIC X(100).
               10  IC-INCLUDE-UOM      PIC X.
               10  IC-INCLUDE-SITE     PIC X.
               10  IC-INCLUDE-VAT      PIC X.
               10  IC-INCLUDE-CUSTOMS  PIC X.
               10  IC-INCLUDE-PACKAGE  PIC X.
               10  IC-INCLUDE-PARTNER  PIC X.
               10  IC-INCLUDE-SUBST    PIC X.
               10  IC-INCLUDE-REVISION PIC X.
               10  IC-FIELD-NAME       PIC X(24).
               10  IC-NEW-VALUE        PIC X(100).
               10  IC-TARGET-CNT       PIC 9(3).
               10  IC-COPIED-CNT       PIC 9(3).
               10  IC-REJECTED-CNT     PIC 9(3).
               10  IC-EXCLUDE-CNT      PIC 9(3).
               10  IC-EXCLUDES OCCURS 10 TIMES.
                   15  IC-X-GROUP      PIC X(24).
                   15  IC-X-TEXT       PIC X(80).
               10  IC-PREVIEW-CNT      PIC 9(3).
               10  IC-PREVIEWS OCCURS 20 TIMES.
                   15  IC-P-GROUP      PIC X(24).
                   15  IC-P-ROWS       PIC S9(9) COMP-5.
                   15  IC-P-TEXT       PIC X(80).
               10  IC-TARGETS OCCURS 30 TIMES.
                   15  IC-T-ITEM       PIC X(40).
                   15  IC-T-OLD-VALUE  PIC X(100).
                   15  IC-T-NEW-VALUE  PIC X(100).
                   15  IC-T-OUTCOME    PIC X(10).
                   15  IC-T-REASON     PIC X(80).
