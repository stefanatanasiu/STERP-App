      *>================================================================
      *> KITCLOSECA - COMMAREA for KITCLOSE: operational close,
      *>              cancellation, reopening and status history.
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> Operational close (KIT-017) must resolve, not hide: unused
      *> reservations are released, material still on the job must be
      *> returned or given an approved disposition, excess issues and
      *> shortages are stated and uncompleted output is accounted for.
      *> Cancelling a partly issued kit does NOT put material back
      *> into stores - only a real return movement does that.
      *>
      *> Reopening (KIT-018) needs authority, a reason and an impact
      *> review of finished stock, costs, jobs and financial periods.
      *> It can never delete a completed receipt and can never bypass
      *> a locked accounting period.
      *>
      *> Every state change keeps actor, time, reason, approval and
      *> source document links (KIT-024), which is what HISTORY reads
      *> back from core.AuditLog.
      *>
      *> CA-FUNCTION values
      *>   CLOSECK   readiness check for operational close: what is
      *>             unresolved and what authority would be required
      *>   CLOSE     operationally close (ISSUED/PARTCOMPLETE ->
      *>             OPCOMPLETE)
      *>   FINCLOSE  financially close (OPCOMPLETE -> FINCLOSED)
      *>   CANCEL    cancel the kit; unissued lines and reservations
      *>             are released, issued material is listed as
      *>             outstanding disposition
      *>   REOPEN    reopen with authority and reason
      *>   HISTORY   the status history of the kit
      *>
      *> REASON CODES  KIT0001, KIT0002, KIT0005, KIT0008, KIT0009,
      *>   KIT0016, KIT0017, KIT0023.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-05-18 PMK  CR2341 CLOSE AND CANCEL
      *> 2016-03-30 PMK  CR2470 REOPEN NEEDS A PERIOD CHECK
      *> 2026-10-06 STERP KIT-017, KIT-018, KIT-024
      *>================================================================
      *> REQ: KIT-017, KIT-018, KIT-024
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KZ-BODY.
               10  KZ-KIT-REF          PIC X(30).
               10  KZ-REASON-CODE      PIC X(20).
               10  KZ-REASON-TEXT      PIC X(300).
               10  KZ-APPROVAL-ID      PIC S9(17).
               10  KZ-AUTHORITY-REF    PIC X(40).
               10  KZ-EFFECT-DATE      PIC 9(8).
               10  KZ-FORCE            PIC X.
               10  KZ-DISPOSITION      PIC X(12).
      *>       ---- result ----
               10  KZ-OLD-STATUS       PIC X(14).
               10  KZ-NEW-STATUS       PIC X(14).
               10  KZ-RESV-RELEASED    PIC S9(4).
               10  KZ-LINES-CLOSED     PIC S9(4).
               10  KZ-ON-JOB-LINES     PIC S9(4).
               10  KZ-ON-JOB-QTY       PIC S9(13)V9(6).
               10  KZ-ON-JOB-VALUE     PIC S9(11)V99.
               10  KZ-EXCESS-LINES     PIC S9(4).
               10  KZ-SHORT-LINES      PIC S9(4).
               10  KZ-UNCOMPLETED-QTY  PIC S9(13)V9(6).
               10  KZ-WIP-RESIDUAL     PIC S9(11)V99.
               10  KZ-PERIOD-STATUS    PIC X(8).
               10  KZ-PERIOD-YYYYPP    PIC 9(6).
               10  KZ-NEEDS-AUTH       PIC X.
               10  KZ-READY            PIC X.
                   88  KZ-IS-READY               VALUE "Y".
               10  KZ-EXCEPTION-ID     PIC S9(17).
               10  KZ-IMPACT-TEXT      PIC X(300).
      *>       ---- status history window ----
               10  KZ-FROM-LINE        PIC S9(9) COMP-5.
               10  KZ-WIN-CNT          PIC S9(4).
               10  KZ-MORE-LINES       PIC X.
               10  KZ-EVENTS OCCURS 15 TIMES.
                   15  KZ-E-WHEN       PIC X(20).
                   15  KZ-E-USER       PIC X(20).
                   15  KZ-E-ACTION     PIC X(20).
                   15  KZ-E-FROM       PIC X(14).
                   15  KZ-E-TO         PIC X(14).
                   15  KZ-E-REASON     PIC X(80).
                   15  KZ-E-AUTHORITY  PIC X(40).
                   15  KZ-E-SOURCE     PIC X(20).
