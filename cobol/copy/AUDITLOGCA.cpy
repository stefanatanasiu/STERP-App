      *>================================================================
      *> AUDITLOGCA - COMMAREA for AUDITLOG (attributable history)
      *> CA-FUNCTION value
      *>   WRITE     insert one core.AuditLog row through
      *>             core.usp_WriteAudit -> AU-AUDIT-ID
      *> COM-013 - material creation, change, approval, rejection,
      *> release, posting, reversal and override must leave a history
      *> that says who changed what, when, why and with whose
      *> authority.  AU-AUTHORITY-REF carries the approval or override
      *> reference; AU-SENSITIVE marks rows that only an authorised
      *> reviewer may read (COM-019).
      *> RETURN CODES 00 ok, 08 missing mandatory evidence, 12 SQL.
      *>================================================================
      *> REQ: COM-013
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AU-BODY.
               10  AU-ENTITY-TYPE      PIC X(60).
               10  AU-ENTITY-KEY       PIC X(120).
               10  AU-ACTION           PIC X(20).
               10  AU-FIELD-NAME       PIC X(60).
               10  AU-OLD-VALUE        PIC X(200).
               10  AU-NEW-VALUE        PIC X(200).
               10  AU-REASON           PIC X(200).
               10  AU-AUTHORITY-REF    PIC X(60).
               10  AU-SOURCE           PIC X(20).
               10  AU-SENSITIVE        PIC X.
               10  AU-AUDIT-ID         PIC S9(9) COMP-5.
