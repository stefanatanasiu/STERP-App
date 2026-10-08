      *>================================================================
      *> NUMGENCA - COMMAREA for NUMGEN (document numbering, COM-005)
      *> CA-FUNCTION values
      *>   NEXT      next number in NG-SERIES-CODE for CA-COMPANY
      *>             (core.usp_NextDocumentNumber).  The procedure takes
      *>             the series row under UPDLOCK, HOLDLOCK so that two
      *>             simultaneous confirmations cannot produce the same
      *>             reference.
      *>   VOID      record NG-DOCUMENT-NUMBER as void with a reason
      *>             (core.usp_VoidDocumentNumber).  A voided reference
      *>             stays traceable and is never reissued (D-15).
      *> CA-CORRELATION-ID is passed to the procedure so that a repeated
      *> instruction returns the number already issued (COM-015).
      *> RETURN CODES 00 ok, 08 invalid input or a business refusal
      *>   reported by the procedure, 12 SQL failure, 16 no such series.
      *>================================================================
      *> REQ: COM-005, COM-015
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  NG-BODY.
               10  NG-SERIES-CODE      PIC X(20).
               10  NG-DOCUMENT-NUMBER  PIC X(30).
               10  NG-SEQUENCE         PIC S9(9) COMP-5.
               10  NG-REASON-TEXT      PIC X(200).
               10  NG-STATUS           PIC X(12).
