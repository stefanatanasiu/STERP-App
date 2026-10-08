      *>================================================================
      *> STFNARGS - argument area for the STERP user-defined FUNCTIONs
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES
      *> PURPOSE  : GnuCOBOL passes user-defined FUNCTION arguments BY
      *>            REFERENCE and does NOT convert them, so an argument
      *>            whose PICTURE differs from the parameter is read as
      *>            raw storage and silently produces nonsense.  Every
      *>            caller of the STFUNCS library therefore moves its
      *>            value into one of the fields below and passes that
      *>            field - never a literal, an expression or a field of
      *>            its own.  See docs\COMMON-SERVICES.md "User-defined
      *>            functions" and docs\tech-debt\common.md.
      *>
      *>            MOVE WS-AMOUNT   TO FNA-VALUE
      *>            MOVE 2           TO FNA-PLACES
      *>            COMPUTE FNA-RESULT =
      *>                FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
      *>            MOVE FNA-RESULT  TO WS-AMOUNT
      *>
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-05 STERP INITIAL - FUNCTION LIBRARY CALLING CONVENTION
      *>================================================================
      *> REQ: COM-011
       01  FNA-ARGS.
           05  FNA-VALUE               PIC S9(18)V9(12) VALUE 0.
           05  FNA-VALUE2              PIC S9(18)V9(12) VALUE 0.
           05  FNA-RESULT              PIC S9(18)V9(12) VALUE 0.
           05  FNA-PLACES              PIC S9(4) VALUE 0.
           05  FNA-YEAR                PIC S9(4) VALUE 0.
           05  FNA-MONTH               PIC S9(4) VALUE 0.
           05  FNA-FLAG                PIC S9(4) VALUE 0.
           05  FNA-DATE                PIC 9(8) VALUE 0.
           05  FNA-DATE-RESULT         PIC 9(8) VALUE 0.
