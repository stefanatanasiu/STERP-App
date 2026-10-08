>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITCLOSE
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Operational close, cancellation and reopening of
*>              an execution kit (KIT-017, KIT-018, KIT-024).
*>
*>              Closing a kit is a statement that everything has
*>              been accounted for, so the program's real job is
*>              to refuse to let anybody say that when it is not
*>              true.  CLOSECK lists, for the kit:
*>                unused reservations, material still on the job,
*>                excess issues, shortages and uncompleted output
*>              and CLOSE will not run while material is still on
*>              the job unless a disposition is stated.
*>
*>              Cancelling a partly issued kit releases what was
*>              only RESERVED - that stock never moved - but it
*>              does NOT put issued material back into stores.
*>              Somebody has to make a real return (KIT-017).
*>
*>              Reopening needs authority, a reason and an open
*>              period, and it never deletes a completion: the
*>              output that was received stays received, and the
*>              history records who reopened it and why (KIT-018,
*>              KIT-024).
*> CALLED BY  : frmKitExecution, KITBATCH
*> INTERFACE  : CALL "KITCLOSE" USING WS-KITCLOSE-CA
*>              COPYBOOK KITCLOSECA
*> CALLS      : KITDB, KITRESV, PERMCHK, PERIODCK, AUDITLOG,
*>              EXCPRAIS, SQLIO (status history from core.AuditLog)
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-05-18 PMK  CR2341 CLOSE AND CANCEL
*> 2016-03-30 PMK  CR2470 REOPEN NEEDS A PERIOD CHECK
*> 2026-10-06 STERP KIT-017, KIT-018, KIT-024
*>================================================================
*> REQ: KIT-017, KIT-018, KIT-024
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITCLOSE.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 18/05/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY KITDBW.
 COPY KITWORK.
 COPY KITRESVCA REPLACING ==DFHCOMMAREA== BY ==Ws-Resv-Ca==
      LEADING ==CA-== BY ==RA-==.
 COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Perm-Ca==
      LEADING ==CA-== BY ==PA-==.
 COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Period-Ca==
      LEADING ==CA-== BY ==DA-==.
 COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Audit-Ca==
      LEADING ==CA-== BY ==AA-==.
 COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==Ws-Exc-Ca==
      LEADING ==CA-== BY ==XA-==.
 01  Ws-Hist-Row.
     05  Ws-H-When               PIC X(20).
     05  Ws-H-User               PIC X(20).
     05  Ws-H-Action             PIC X(20).
     05  Ws-H-From               PIC X(14).
     05  Ws-H-To                 PIC X(14).
     05  Ws-H-Reason             PIC X(80).
     05  Ws-H-Authority          PIC X(40).
     05  Ws-H-Source             PIC X(20).
 01  Ws-Work.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Row                  PIC S9(4) VALUE 0.
     05  Ws-J                    PIC S9(4) VALUE 0.
     05  Ws-On-Job               PIC S9(13)V9(6) VALUE 0.
     05  Ws-Perm-Code            PIC X(40) VALUE SPACES.
     05  Ws-Edit                 PIC -(13)9.9(6).
     05  Ws-Money                PIC -(11)9.99.
 LINKAGE SECTION.
 COPY KITCLOSECA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITCLOSE" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-017" TO CA-REQ-REF
     MOVE 0 TO KZ-RESV-RELEASED
     MOVE 0 TO KZ-LINES-CLOSED
     MOVE 0 TO KZ-ON-JOB-LINES
     MOVE 0 TO KZ-ON-JOB-QTY
     MOVE 0 TO KZ-ON-JOB-VALUE
     MOVE 0 TO KZ-EXCESS-LINES
     MOVE 0 TO KZ-SHORT-LINES
     MOVE 0 TO KZ-UNCOMPLETED-QTY
     MOVE 0 TO KZ-EXCEPTION-ID
     MOVE "N" TO KZ-NEEDS-AUTH
     MOVE "N" TO KZ-READY
     MOVE SPACES TO KZ-IMPACT-TEXT
     IF KZ-EFFECT-DATE = 0
         MOVE CA-BUSINESS-DATE TO KZ-EFFECT-DATE
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "CLOSECK"  PERFORM Assess-The-Kit
         WHEN "CLOSE"    PERFORM Close-Operationally
         WHEN "FINCLOSE" PERFORM Close-Financially
         WHEN "CANCEL"   PERFORM Cancel-The-Kit
         WHEN "REOPEN"   PERFORM Reopen-The-Kit
         WHEN "HISTORY"  PERFORM Read-Status-History
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITCLOSE does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE KD-ERRMSG TO CA-MESSAGE.

*>----------------------------------------------------------------
 Load-Kit.
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KZ-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KZ-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE
     IF CA-RETURN-CODE = 0
         MOVE KH-STATUS TO KZ-OLD-STATUS
         MOVE KH-STATUS TO KZ-NEW-STATUS
         MOVE KH-WIP-VALUE TO KZ-WIP-RESIDUAL
         COMPUTE KZ-UNCOMPLETED-QTY = KH-PLANNED-QTY
                                    - KH-COMPLETED-QTY
                                    - KH-REJECTED-QTY
         IF KZ-UNCOMPLETED-QTY < 0
             MOVE 0 TO KZ-UNCOMPLETED-QTY
         END-IF
     END-IF.

*>----------------------------------------------------------------
*> CLOSECK - the honest statement of what is still outstanding.
*> Nothing is changed; the same walk is used by CLOSE and CANCEL
*> so the screen and the action can never disagree.
 Assess-The-Kit.
     PERFORM Load-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Walk-The-Lines
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-The-Period
     PERFORM Decide-Readiness.

 Walk-The-Lines.
     MOVE "LINOPEN" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE "N" TO KD-ONLY-OPEN
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "LINNEXT" TO KD-FUNC
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         EVALUATE KD-RC
             WHEN 0
                 PERFORM Assess-One-Line
             WHEN 100
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Set-Sql-Failure
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC.

*>   "On the job" is the material that left stores for this kit
*>   and has not been consumed by output, returned or scrapped.
*>   It is the number that stops a close.
 Assess-One-Line.
     IF KL-IS-CANCELLED
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO KZ-LINES-CLOSED
     IF KL-RESERVED-QTY > 0
         ADD 1 TO KZ-RESV-RELEASED
     END-IF
     IF KL-EXCESS-QTY > 0
         ADD 1 TO KZ-EXCESS-LINES
     END-IF
     IF KL-REMAINING-QTY > 0
         ADD 1 TO KZ-SHORT-LINES
     END-IF
     COMPUTE Ws-On-Job = KL-NET-ISSUED - KL-SCRAPPED-QTY
     IF KZ-UNCOMPLETED-QTY <= 0 AND KH-COMPLETED-QTY > 0
*>       Everything planned has been produced, so the material
*>       that was issued has been consumed by the output.
         MOVE 0 TO Ws-On-Job
     END-IF
     IF Ws-On-Job > 0
         ADD 1 TO KZ-ON-JOB-LINES
         COMPUTE KZ-ON-JOB-QTY = KZ-ON-JOB-QTY + Ws-On-Job
         COMPUTE KZ-ON-JOB-VALUE ROUNDED = KZ-ON-JOB-VALUE
             + (Ws-On-Job * KL-PLAN-UNIT-COST)
     END-IF.

 Check-The-Period.
     INITIALIZE Ws-Period-Ca
     MOVE "CHECK" TO DA-FUNCTION
     MOVE CA-COMPANY TO DA-COMPANY
     MOVE KH-SITE TO DA-SITE
     MOVE CA-USER TO DA-USER
     MOVE CA-BUSINESS-DATE TO DA-BUSINESS-DATE
     MOVE KZ-EFFECT-DATE TO PC-POSTING-DATE
     MOVE KZ-EFFECT-DATE TO PC-DOCUMENT-DATE
     MOVE "POST" TO PC-ACTION
     CALL "PERIODCK" USING Ws-Period-Ca
     MOVE PC-PERIOD-STATUS TO KZ-PERIOD-STATUS
     IF PC-PERIOD-YYYYPP NOT = SPACES
         COMPUTE KZ-PERIOD-YYYYPP = FUNCTION NUMVAL (PC-PERIOD-YYYYPP)
     END-IF.

 Decide-Readiness.
     MOVE SPACES TO KZ-IMPACT-TEXT
     IF KZ-ON-JOB-LINES > 0
         MOVE "N" TO KZ-READY
         MOVE KZ-ON-JOB-QTY TO Ws-Edit
         STRING FUNCTION TRIM(Ws-Edit)
                " of material is still on the job across "
                " lines; it must be returned, scrapped or written off"
                " before the kit can close."
             DELIMITED BY SIZE INTO KZ-IMPACT-TEXT
     ELSE
         IF KZ-UNCOMPLETED-QTY > 0 AND KH-COMPLETED-QTY = 0
             MOVE "N" TO KZ-READY
             MOVE "Nothing has been completed on this kit; cancel it rather than close it."
                  TO KZ-IMPACT-TEXT
         ELSE
             MOVE "Y" TO KZ-READY
             MOVE "Every line is accounted for; the kit may be closed."
                  TO KZ-IMPACT-TEXT
         END-IF
     END-IF
     IF KZ-IS-READY
         MOVE 0 TO CA-RETURN-CODE
     ELSE
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0017" TO CA-REASON-CODE
     END-IF
     MOVE KZ-IMPACT-TEXT (1:200) TO CA-MESSAGE.

*>----------------------------------------------------------------
*> CLOSE - operational closure.  The kit stops consuming and
*> producing; the accounts may still be open, which is why
*> FINCLOSED is a separate state (KIT-017).
 Close-Operationally.
     PERFORM Assess-The-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KH-IS-CANCELLED OR KH-IS-FINCLOSED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "A kit in status " FUNCTION TRIM(KH-STATUS)
                " cannot be closed again."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
*>   KIT-017 - unconsumed material has to be dealt with.  A
*>   stated disposition (RETURN / SCRAP / WRITEOFF) plus a reason
*>   lets a supervisor close it anyway, and that decision is
*>   recorded against them.
     IF NOT KZ-IS-READY AND KZ-ON-JOB-LINES > 0
         IF KZ-DISPOSITION = SPACES OR KZ-REASON-TEXT = SPACES
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0017" TO CA-REASON-CODE
             MOVE KZ-IMPACT-TEXT (1:200) TO CA-MESSAGE
             EXIT PARAGRAPH
         END-IF
         MOVE "KIT.EXEC.CLOSE.FORCE" TO Ws-Perm-Code
         PERFORM Check-Authority
         IF CA-RETURN-CODE NOT = 0
             EXIT PARAGRAPH
         END-IF
     END-IF
     PERFORM Release-Reservations
     MOVE "OPCOMPLETE" TO KD-NEW-STATUS
     PERFORM Change-Status
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE "OPCOMPLETE" TO KZ-NEW-STATUS
     PERFORM Audit-Status-Change
     MOVE KZ-WIP-RESIDUAL TO Ws-Money
     STRING "Kit closed operationally. "
            FUNCTION TRIM(Ws-Money)
            " of work in progress remains for the cost ledger to settle."
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>   FINCLOSE - the accounting close.  It is refused while the
*>   period is shut, and it is the last state: nothing may be
*>   posted to the kit afterwards without a reopen.
 Close-Financially.
     PERFORM Load-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KH-IS-FINCLOSED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0023" TO CA-REASON-CODE
         MOVE "This kit has already been financially closed."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF NOT KH-IS-OPCOMPLETE AND NOT KH-IS-CANCELLED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         MOVE "A kit must be operationally complete or cancelled before it is financially closed."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-The-Period
     IF PC-ALLOWED NOT = "Y"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0016" TO CA-REASON-CODE
         MOVE "The accounting period for the effective date is closed."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE "KIT.EXEC.FINCLOSE" TO Ws-Perm-Code
     PERFORM Check-Authority
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     MOVE "FINCLOSED" TO KD-NEW-STATUS
     PERFORM Change-Status
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE "FINCLOSED" TO KZ-NEW-STATUS
     PERFORM Audit-Status-Change
     MOVE "Kit financially closed; no further cost may be posted to it."
          TO CA-MESSAGE.

*>----------------------------------------------------------------
*> CANCEL - the kit will not be made.  Reservations go back to
*> the free pool because that stock never moved.  Issued material
*> stays issued and stays charged to the kit until a real return
*> movement is made, which is the whole point of KIT-017.
 Cancel-The-Kit.
     PERFORM Assess-The-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF NOT KH-IS-OPEN
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "A kit in status " FUNCTION TRIM(KH-STATUS)
                " can no longer be cancelled."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KH-COMPLETED-QTY > 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         MOVE "Output has already been received from this kit; close it instead of cancelling it."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KZ-REASON-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "A cancellation needs a reason." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KZ-ON-JOB-LINES > 0
         MOVE "KIT.EXEC.CANCEL.ISSUED" TO Ws-Perm-Code
         PERFORM Check-Authority
         IF CA-RETURN-CODE NOT = 0
             EXIT PARAGRAPH
         END-IF
     END-IF
     PERFORM Release-Reservations
     MOVE "CANCELLED" TO KD-NEW-STATUS
     PERFORM Change-Status
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE "CANCELLED" TO KZ-NEW-STATUS
     PERFORM Audit-Status-Change
     IF KZ-ON-JOB-LINES > 0
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0017" TO CA-REASON-CODE
         MOVE KZ-ON-JOB-QTY TO Ws-Edit
         STRING "Cancelled. Reservations released, but "
                FUNCTION TRIM(Ws-Edit)
                " of issued material is still charged to this kit "
                "and needs a return movement."
             DELIMITED BY SIZE INTO CA-MESSAGE
         PERFORM Raise-Leftover-Exception
     ELSE
         MOVE "Cancelled. All reservations were released; nothing had been issued."
              TO CA-MESSAGE
     END-IF.

 Release-Reservations.
     IF KZ-RESV-RELEASED = 0
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Resv-Ca
     MOVE "RELEASE" TO RA-FUNCTION
     MOVE CA-COMPANY TO RA-COMPANY
     MOVE KH-SITE TO RA-SITE
     MOVE CA-USER TO RA-USER
     MOVE CA-BUSINESS-DATE TO RA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO RA-CORRELATION-ID
     MOVE KZ-KIT-REF TO KR-KIT-REF
     MOVE KH-SITE TO KR-SITE
     MOVE 0 TO KR-LINE-NUM
     CALL "KITRESV" USING Ws-Resv-Ca
     IF RA-RETURN-CODE > 4
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0010" TO CA-REASON-CODE
         MOVE RA-MESSAGE TO KZ-IMPACT-TEXT
     END-IF.

*>----------------------------------------------------------------
*> REOPEN - KIT-018.  Authority, reason, period, and an explicit
*> statement of what cannot be undone.
 Reopen-The-Kit.
     PERFORM Load-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KH-IS-OPEN
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         MOVE "This kit is already open." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KZ-REASON-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "Reopening a kit needs a reason that will be kept with it."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE "KIT.EXEC.REOPEN" TO Ws-Perm-Code
     PERFORM Check-Authority
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
*>   A locked accounting period is not negotiable, and KZ-FORCE
*>   deliberately does not override it (KIT-018).
     PERFORM Check-The-Period
     IF PC-ALLOWED NOT = "Y"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0016" TO CA-REASON-CODE
         STRING "Period " KZ-PERIOD-YYYYPP " is "
                FUNCTION TRIM(KZ-PERIOD-STATUS)
                "; a kit cannot be reopened into a locked period."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KH-IS-FINCLOSED
         MOVE "KIT.EXEC.FINCLOSE" TO Ws-Perm-Code
         PERFORM Check-Authority
         IF CA-RETURN-CODE NOT = 0
             EXIT PARAGRAPH
         END-IF
     END-IF
*>   What the kit goes back to depends on what really happened to
*>   it, never on what the caller would like it to be.
     EVALUATE TRUE
         WHEN KH-COMPLETED-QTY > 0
             MOVE "PARTCOMPLETE" TO KD-NEW-STATUS
         WHEN KH-GROSS-STARTS > 0
             MOVE "PARTISSUED" TO KD-NEW-STATUS
         WHEN OTHER
             MOVE "RELEASED" TO KD-NEW-STATUS
     END-EVALUATE
     PERFORM Change-Status
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE KD-NEW-STATUS TO KZ-NEW-STATUS
     PERFORM Audit-Status-Change
     MOVE KH-COMPLETED-QTY TO Ws-Edit
     STRING "Reopened as " FUNCTION TRIM(KZ-NEW-STATUS)
            ". The " FUNCTION TRIM(Ws-Edit)
            " unit(s) already received stay received; history is not deleted."
         DELIMITED BY SIZE INTO CA-MESSAGE
     MOVE CA-MESSAGE TO KZ-IMPACT-TEXT.

*>----------------------------------------------------------------
 Check-Authority.
     INITIALIZE Ws-Perm-Ca
     MOVE "CHECK" TO PA-FUNCTION
     MOVE CA-COMPANY TO PA-COMPANY
     MOVE KH-SITE TO PA-SITE
     MOVE CA-USER TO PA-USER
     MOVE CA-BUSINESS-DATE TO PA-BUSINESS-DATE
     MOVE Ws-Perm-Code TO PK-PERMISSION
     MOVE CA-USER TO PK-CHECK-USER
     MOVE KH-SITE TO PK-CHECK-SITE
     CALL "PERMCHK" USING Ws-Perm-Ca
     IF PA-RETURN-CODE NOT = 0
         MOVE 6 TO CA-RETURN-CODE
         MOVE "KIT0009" TO CA-REASON-CODE
         MOVE "Y" TO KZ-NEEDS-AUTH
         IF PK-REASON-TEXT = SPACES
             STRING "This action needs the "
                    FUNCTION TRIM(Ws-Perm-Code) " authority."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         ELSE
             MOVE PK-REASON-TEXT TO CA-MESSAGE
         END-IF
     END-IF.

 Change-Status.
     MOVE "HDRSTAT" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KH-KIT-REF TO KD-KIT-REF
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE KZ-REASON-TEXT (1:200) TO KD-REASON
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     END-IF.

*>   KIT-024 - actor, time, reason, authority and the source
*>   document link, on every single status change.
 Audit-Status-Change.
     INITIALIZE Ws-Audit-Ca
     MOVE "WRITE" TO AA-FUNCTION
     MOVE CA-COMPANY TO AA-COMPANY
     MOVE KH-SITE TO AA-SITE
     MOVE CA-USER TO AA-USER
     MOVE CA-BUSINESS-DATE TO AA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO AA-CORRELATION-ID
     MOVE "KIT.EXECUTIONKIT" TO AU-ENTITY-TYPE
     MOVE KZ-KIT-REF TO AU-ENTITY-KEY
     MOVE CA-FUNCTION TO AU-ACTION
     MOVE "Status" TO AU-FIELD-NAME
     MOVE KZ-OLD-STATUS TO AU-OLD-VALUE
     MOVE KZ-NEW-STATUS TO AU-NEW-VALUE
     MOVE KZ-REASON-TEXT (1:200) TO AU-REASON
     IF KZ-AUTHORITY-REF = SPACES
         MOVE KH-SOURCE-REF TO AU-AUTHORITY-REF
     ELSE
         MOVE KZ-AUTHORITY-REF TO AU-AUTHORITY-REF
     END-IF
     MOVE "KITCLOSE" TO AU-SOURCE
     MOVE "N" TO AU-SENSITIVE
     CALL "AUDITLOG" USING Ws-Audit-Ca.

 Raise-Leftover-Exception.
     INITIALIZE Ws-Exc-Ca
     MOVE "RAISE" TO XA-FUNCTION
     MOVE CA-COMPANY TO XA-COMPANY
     MOVE KH-SITE TO XA-SITE
     MOVE CA-USER TO XA-USER
     MOVE CA-BUSINESS-DATE TO XA-BUSINESS-DATE
     MOVE "KIT" TO EX-MODULE
     MOVE "ERROR" TO EX-SEVERITY
     MOVE "KIT MATERIAL NOT RETURNED" TO EX-CATEGORY
     MOVE CA-MESSAGE (1:200) TO EX-MESSAGE-TEXT
     MOVE "EXECUTIONKIT" TO EX-DOCUMENT-TYPE
     MOVE KZ-KIT-REF TO EX-DOCUMENT-REF
     MOVE KH-OWNER-USER TO EX-OWNER-USER
     MOVE 2 TO EX-PRIORITY
     MOVE KZ-ON-JOB-QTY TO EX-QUANTITY
     MOVE KZ-ON-JOB-VALUE TO EX-AMOUNT-GBP
     CALL "EXCPRAIS" USING Ws-Exc-Ca
     MOVE EX-EXCEPTION-ID TO KZ-EXCEPTION-ID.

*>----------------------------------------------------------------
*> HISTORY - the status trail, read straight out of the audit log
*> so that it cannot drift from what was actually recorded.
 Read-Status-History.
     PERFORM Load-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO KZ-WIN-CNT
     MOVE "N" TO KZ-MORE-LINES
     MOVE 0 TO Ws-Row
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(20), a.EventTime, 120), "
            "CONVERT(varchar(20), ISNULL(a.UserCode,'')), "
            "CONVERT(varchar(20), ISNULL(a.Action,'')), "
            "CONVERT(varchar(14), ISNULL(a.OldValue,'')), "
            "CONVERT(varchar(14), ISNULL(a.NewValue,'')), "
            "CONVERT(varchar(80), ISNULL(a.Reason,'')), "
            "CONVERT(varchar(40), ISNULL(a.AuthorityRef,'')), "
            "CONVERT(varchar(20), ISNULL(a.Source,'')) "
            "FROM core.AuditLog a WHERE a.CompanyCode = ? AND "
            "a.EntityType = 'KIT.EXECUTIONKIT' AND "
            "a.EntityKey = ? ORDER BY a.EventTime"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KZ-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE "N" TO SQLIO-PARM-NULL (1)
     MOVE "N" TO SQLIO-PARM-NULL (2)
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE 8 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE 20 TO SQLIO-COL-WIDTH (2)
     MOVE 20 TO SQLIO-COL-WIDTH (3)
     MOVE 14 TO SQLIO-COL-WIDTH (4)
     MOVE 14 TO SQLIO-COL-WIDTH (5)
     MOVE 80 TO SQLIO-COL-WIDTH (6)
     MOVE 40 TO SQLIO-COL-WIDTH (7)
     MOVE 20 TO SQLIO-COL-WIDTH (8)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Hist-Row
     IF SQLIO-ERROR
         MOVE 12 TO CA-RETURN-CODE
         MOVE "SQL0001" TO CA-REASON-CODE
         MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE 7 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA, Ws-Hist-Row
         IF SQLIO-ERROR OR SQLIO-NOTFOUND
             MOVE "N" TO Ws-More
         ELSE
             PERFORM Add-History-Row
         END-IF
     END-PERFORM
     MOVE 7 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Hist-Row
     MOVE KZ-WIN-CNT TO Ws-Row
     STRING "Status history read; " Ws-Row " event(s) in the window."
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Add-History-Row.
     ADD 1 TO Ws-Row
     IF Ws-Row <= KZ-FROM-LINE
         EXIT PARAGRAPH
     END-IF
     IF KZ-WIN-CNT >= 15
         MOVE "Y" TO KZ-MORE-LINES
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO KZ-WIN-CNT
     MOVE Ws-H-When TO KZ-E-WHEN (KZ-WIN-CNT)
     MOVE Ws-H-User TO KZ-E-USER (KZ-WIN-CNT)
     MOVE Ws-H-Action TO KZ-E-ACTION (KZ-WIN-CNT)
     MOVE Ws-H-From TO KZ-E-FROM (KZ-WIN-CNT)
     MOVE Ws-H-To TO KZ-E-TO (KZ-WIN-CNT)
     MOVE Ws-H-Reason TO KZ-E-REASON (KZ-WIN-CNT)
     MOVE Ws-H-Authority TO KZ-E-AUTHORITY (KZ-WIN-CNT)
     MOVE Ws-H-Source TO KZ-E-SOURCE (KZ-WIN-CNT).
