      *>================================================================
      *> SODOCVRCA - COMMAREA FOR SODOCVR, DOCUMENT VERSIONS AND
      *>             REPRINTS
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> QUOTES, ACKNOWLEDGEMENTS, PICK NOTES, DESPATCH NOTES AND
      *> INVOICES ARE VERSIONED SO THAT WHAT THE CUSTOMER ACCEPTED CAN
      *> BE REPRODUCED EXACTLY YEARS LATER (SO-022).  A REISSUE
      *> REPRODUCES A HELD VERSION; IT CHANGES NO BUSINESS STATE AND
      *> IT CONCEALS NOTHING - THE REPRINT COUNT RISES AND AN AUDIT
      *> RECORD IS WRITTEN THROUGH AUDITLOG.
      *>
      *> THE SNAPSHOT IS CARRIED AS NUMBERED TEXT LINES RATHER THAN AS
      *> ONE LONG FIELD BECAUSE SQLIO BINDS PARAMETERS OF 256 BYTES.
      *> SODOCVR JOINS THE LINES WITH CARRIAGE RETURN LINE FEED, AND
      *> SQL SERVER COMPUTES THE SHA2_256 CONTENT HASH OF THE RESULT,
      *> SO THE SAME CONTENT ALWAYS HASHES THE SAME WAY.
      *>
      *> CA-FUNCTION VALUES
      *>   SNAPSHOT   STORE A NEW VERSION OF A DOCUMENT WITH ITS
      *>              CONTENT HASH.  THE VERSION NUMBER IS ALLOCATED
      *>              BY THE SERVICE WHEN DV-VERSION-NO IS ZERO.
      *>   REISSUE    REPRODUCE A HELD VERSION EXACTLY, INCREMENT THE
      *>              REPRINT COUNT AND WRITE THE AUDIT RECORD.
      *>   LIST       LIST THE HELD VERSIONS OF ONE DOCUMENT.
      *>   COMPARE    REPORT WHAT CHANGED BETWEEN TWO VERSIONS.
      *>
      *> REASON CODES
      *>   DOC0001 DOCUMENT TYPE OR REFERENCE MISSING
      *>   DOC0002 DOCUMENT TYPE NOT RECOGNISED
      *>   DOC0003 NO SNAPSHOT LINES SUPPLIED
      *>   DOC0004 THAT VERSION IS NOT HELD
      *>   DOC0005 NO VERSIONS ARE HELD FOR THAT DOCUMENT
      *>   DOC0006 TWO DIFFERENT VERSIONS ARE NEEDED TO COMPARE
      *>   DOC0007 UNSUPPORTED FUNCTION
      *>   DOC0008 THE SNAPSHOT IS UNCHANGED - NO NEW VERSION WRITTEN
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-05-30 RJH  CR0352 DOCUMENT REPRINT CONTROL
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON ISSUE STAMPS
      *> 2012-04-18 DJM  CR2210 CONTENT HASH ON ACCEPTED DOCUMENTS
      *> 2026-10-06 STERP SO-022 VERSIONS, REPRINTS AND COMPARISON
      *>================================================================
      *> REQ: SO-022
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DV-BODY.
               10  DV-DOCUMENT-TYPE    PIC X(20).
                   88  DV-TYPE-QUOTE             VALUE "QUOTE".
                   88  DV-TYPE-ACK               VALUE "ACKNOWLEDGE".
                   88  DV-TYPE-PICK              VALUE "PICKNOTE".
                   88  DV-TYPE-DESPATCH          VALUE "DESPATCHNOTE".
                   88  DV-TYPE-INVOICE           VALUE "INVOICE".
               10  DV-DOCUMENT-REF     PIC X(30).
               10  DV-VERSION-NO       PIC S9(4) SIGN LEADING SEPARATE.
               10  DV-COMPARE-VERSION  PIC S9(4) SIGN LEADING SEPARATE.
               10  DV-LATEST-VERSION   PIC S9(4) SIGN LEADING SEPARATE.
               10  DV-VERSION-ID       PIC S9(17).
               10  DV-ISSUED-AT        PIC X(23).
               10  DV-ISSUED-BY        PIC X(20).
               10  DV-CONTENT-HASH     PIC X(64).
               10  DV-COMPARE-HASH     PIC X(64).
               10  DV-CHANGE-SUMMARY   PIC X(400).
               10  DV-REPRINT-COUNT    PIC S9(9) COMP-5.
               10  DV-REPRINT-REASON   PIC X(200).
               10  DV-AUDIT-ID         PIC S9(9) COMP-5.
      *>       a reissue is evidence, never a business change
               10  DV-STATE-CHANGED    PIC X.
               10  DV-IS-IDENTICAL     PIC X.
               10  DV-WAS-WRITTEN      PIC X.
               10  DV-SNAP-CNT         PIC 9(3).
               10  DV-SNAP-TAB.
                   15  DV-SNAP-LINES   OCCURS 24 TIMES.
                       20  DV-S-TEXT       PIC X(200).
               10  DV-CMP-CNT          PIC 9(3).
               10  DV-CMP-TAB.
                   15  DV-CMP-LINES    OCCURS 24 TIMES.
                       20  DV-C-TEXT       PIC X(200).
               10  DV-LIST-CNT         PIC 9(3).
               10  DV-LIST-TRUNCATED   PIC X.
               10  DV-LIST-TAB.
                   15  DV-VERSIONS     OCCURS 20 TIMES.
                       20  DV-V-VERSION    PIC S9(4) SIGN LEADING
                                           SEPARATE.
                       20  DV-V-ISSUED-AT  PIC X(23).
                       20  DV-V-ISSUED-BY  PIC X(20).
                       20  DV-V-HASH       PIC X(64).
                       20  DV-V-REPRINTS   PIC S9(9) COMP-5.
                       20  DV-V-SUMMARY    PIC X(100).
               10  DV-DIFF-CNT         PIC 9(3).
               10  DV-DIFF-TAB.
                   15  DV-DIFFS        OCCURS 24 TIMES.
                       20  DV-F-FIELD      PIC X(40).
                       20  DV-F-OLD        PIC X(100).
                       20  DV-F-NEW        PIC X(100).
