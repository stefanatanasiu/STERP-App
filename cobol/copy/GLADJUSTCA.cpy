      *>================================================================
      *> GLADJUSTCA - COMMAREA for GLADJUST, write-offs, impairments and
      *>              other value only corrections (FIN-022).
      *> A value adjustment never changes a physical quantity: AC-017
      *> leaves 100 units and a carrying value of 900.00.
      *> Functions: ASSESS   threshold and approval level for a value
      *>            REQUEST  record the request with reason and evidence
      *>            APPROVE  approve at the required level
      *>            POST     post the approved adjustment
      *> 1999-02-18 R J HOLBROOK   2026-10-05 STERP THRESHOLD LADDER
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  GA-BODY.
               10  GA-WRITEOFF-REF     PIC X(30).
               10  GA-TYPE             PIC X(14).
               10  GA-REASON-CODE      PIC X(20).
               10  GA-REASON           PIC X(300).
               10  GA-ACCOUNT          PIC X(12).
               10  GA-CONTRA-ACCOUNT   PIC X(12).
               10  GA-SUBLEDGER-TYPE   PIC X(6).
               10  GA-SUBLEDGER-REF    PIC X(40).
               10  GA-AMOUNT           PIC S9(11)V99 COMP-3.
               10  GA-CARRYING-AMOUNT  PIC S9(13)V99 COMP-3.
               10  GA-QUANTITY         PIC S9(11)V9(6) COMP-3.
               10  GA-POSTING-DATE     PIC 9(8).
               10  GA-DIM-CC           PIC X(20).
               10  GA-EVIDENCE-REF     PIC X(200).
               10  GA-APPROVER         PIC X(20).
               10  GA-WRITEOFF-ID      PIC S9(9) COMP-5.
      *>     ---- answers ----
               10  GA-THRESHOLD        PIC S9(11)V99 COMP-3.
               10  GA-APPROVAL-LEVEL   PIC X(12).
               10  GA-PERMISSION       PIC X(40).
               10  GA-NET-CARRYING     PIC S9(13)V99 COMP-3.
               10  GA-QTY-AFTER        PIC S9(11)V9(6) COMP-3.
               10  GA-STATUS           PIC X(12).
               10  GA-JOURNAL-NUMBER   PIC X(30).
               10  GA-NEEDS-APPROVAL   PIC X.
               10  GA-EVIDENCE-OK      PIC X.
