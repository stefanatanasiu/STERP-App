      *>================================================================
      *> PROGRAM-ID : TAPEINV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (AP) UNIT TESTS
      *> PURPOSE    : APXMLGET - THE RECORD SCANNER APEINV USES TO
      *>              TAKE A PEPPOL BIS BILLING 3.0 UBL E-INVOICE
      *>              APART (AP-031, DECISION D-26).  THE FRAGMENTS
      *>              BELOW ARE LIFTED FROM
      *>              data\einvoice-samples\INV-2026-5541.xml.
      *>              THE SCANNER TOUCHES NO DATABASE, SO THE WHOLE
      *>              SUITE RUNS WITH THE SERVER DOWN.
      *>              THE LAST TEST IS THE POINT OF AC-125: THE
      *>              SCANNER HANDS THE REFERENCE OVER EXACTLY AS THE
      *>              SUPPLIER SENT IT, SO APINVREG - NOT THE
      *>              TRANSLATOR - DECIDES WHAT IS A DUPLICATE.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AP-031, AP-004   AC: AC-125
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAPEINV.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY APXMLGTC REPLACING ==DFHCOMMAREA== BY ==WS-XML-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAPEINV UBL record scanner" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-SIMPLE-ELEMENT
           PERFORM 1100-CONTAINER-OPEN-AND-CLOSE
           PERFORM 1200-ATTRIBUTES-IGNORED
           PERFORM 1300-SELF-CLOSING
           PERFORM 1400-NOT-AN-ELEMENT
           PERFORM 1500-DATES
           PERFORM 1600-NUMBERS
           PERFORM 1700-ENTITIES
           PERFORM 1800-FIND-A-NAMED-TAG
           PERFORM 1900-REFERENCE-IS-NOT-TIDIED
           PERFORM TK-END
           STOP RUN.

       0200-SCAN.
           MOVE "ELEMENT" TO XG-FUNC
           CALL "APXMLGET" USING WS-XML-CA.

       1000-SIMPLE-ELEMENT.
           INITIALIZE WS-XML-CA
           MOVE "  <cbc:ID>INV-2026-5541</cbc:ID>" TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "the element is recognised" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE XG-RC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the tag is cbc:ID" TO TK-NAME
           MOVE "cbc:ID" TO TK-EXPECTED-X
           MOVE XG-TAG-FOUND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the invoice number is the value" TO TK-NAME
           MOVE "INV-2026-5541" TO TK-EXPECTED-X
           MOVE XG-VALUE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the value is 13 characters long" TO TK-NAME
           MOVE 13 TO TK-EXPECTED-N
           MOVE XG-VALUE-LEN TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "it is not a container" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE XG-IS-OPEN TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       1100-CONTAINER-OPEN-AND-CLOSE.
           INITIALIZE WS-XML-CA
           MOVE "  <cac:InvoiceLine>" TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "an invoice line opens a container" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE XG-IS-OPEN TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the container is named" TO TK-NAME
           MOVE "cac:InvoiceLine" TO TK-EXPECTED-X
           MOVE XG-TAG-FOUND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-XML-CA
           MOVE "  </cac:InvoiceLine>" TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "the closing tag closes the container" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE XG-IS-CLOSE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a closing tag names the same container" TO TK-NAME
           MOVE "cac:InvoiceLine" TO TK-EXPECTED-X
           MOVE XG-TAG-FOUND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a closing tag carries no value" TO TK-NAME
           MOVE SPACES TO TK-EXPECTED-X
           MOVE XG-VALUE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   the currency and unit attributes are read but not used -
      *>   the document currency governs
       1200-ATTRIBUTES-IGNORED.
           INITIALIZE WS-XML-CA
           MOVE "    <cbc:InvoicedQuantity unitCode=""EA"">500</cbc:I"
               TO XG-RECORD
           MOVE "nvoicedQuantity>" TO XG-RECORD(52:16)
           PERFORM 0200-SCAN
           MOVE "the attribute does not become part of the tag"
               TO TK-NAME
           MOVE "cbc:InvoicedQuantity" TO TK-EXPECTED-X
           MOVE XG-TAG-FOUND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the quantity is the value" TO TK-NAME
           MOVE "500" TO TK-EXPECTED-X
           MOVE XG-VALUE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the attributes are kept for the audit" TO TK-NAME
           MOVE " unitCode=""EA""" TO TK-EXPECTED-X
           MOVE XG-ATTRIBUTES TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       1300-SELF-CLOSING.
           INITIALIZE WS-XML-CA
           MOVE "  <cbc:Note/>" TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "an empty element is flagged" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE XG-IS-EMPTY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "an empty element does not open a container"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE XG-IS-OPEN TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       1400-NOT-AN-ELEMENT.
           INITIALIZE WS-XML-CA
           MOVE "<?xml version=""1.0"" encoding=""UTF-8""?>"
               TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "the XML declaration is skipped" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE XG-RC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-XML-CA
           MOVE SPACES TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "a blank record is skipped" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE XG-RC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1500-DATES.
           INITIALIZE WS-XML-CA
           MOVE "  <cbc:DueDate>2027-01-31</cbc:DueDate>" TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "DATE" TO XG-FUNC
           CALL "APXMLGET" USING WS-XML-CA
           MOVE "the due date becomes 20270131" TO TK-NAME
           MOVE 20270131 TO TK-EXPECTED-N
           MOVE XG-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-XML-CA
           MOVE "  <cbc:DueDate>31/01/2027</cbc:DueDate>" TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "DATE" TO XG-FUNC
           CALL "APXMLGET" USING WS-XML-CA
           MOVE "a British date in a UBL file is rejected" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE XG-RC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1600-NUMBERS.
           INITIALIZE WS-XML-CA
           MOVE "    <cbc:TaxInclusiveAmount>6000.00</cbc:TaxInclusi"
               TO XG-RECORD
           MOVE "veAmount>" TO XG-RECORD(52:9)
           PERFORM 0200-SCAN
           MOVE "NUM" TO XG-FUNC
           CALL "APXMLGET" USING WS-XML-CA
           MOVE "the gross total is 6000.00" TO TK-NAME
           MOVE 6000.00 TO TK-EXPECTED-N
           MOVE XG-NUMERIC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-XML-CA
           MOVE "  <cbc:TaxAmount>1,000.00</cbc:TaxAmount>"
               TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "NUM" TO XG-FUNC
           CALL "APXMLGET" USING WS-XML-CA
           MOVE "a thousands separator is not a UBL number"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE XG-RC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1700-ENTITIES.
           INITIALIZE WS-XML-CA
           MOVE "  <cbc:Name>Smith &amp; Sons &lt;Ltd&gt;</cbc:Name>"
               TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "the five XML entities are decoded" TO TK-NAME
           MOVE "Smith & Sons <Ltd>" TO TK-EXPECTED-X
           MOVE XG-VALUE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the decoded length is 18" TO TK-NAME
           MOVE 18 TO TK-EXPECTED-N
           MOVE XG-VALUE-LEN TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   some access points deliver the whole document on one line
       1800-FIND-A-NAMED-TAG.
           INITIALIZE WS-XML-CA
           MOVE "<cbc:ID>INV-1</cbc:ID><cbc:IssueDate>2026-09-18</cb"
               TO XG-RECORD
           MOVE "c:IssueDate>" TO XG-RECORD(51:12)
           MOVE "cbc:IssueDate" TO XG-TAG-WANTED
           MOVE "FIND" TO XG-FUNC
           CALL "APXMLGET" USING WS-XML-CA
           MOVE "the second element on the record is found" TO TK-NAME
           MOVE "2026-09-18" TO TK-EXPECTED-X
           MOVE XG-VALUE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-XML-CA
           MOVE "<cbc:ID>INV-1</cbc:ID>" TO XG-RECORD
           MOVE "cbc:DueDate" TO XG-TAG-WANTED
           MOVE "FIND" TO XG-FUNC
           CALL "APXMLGET" USING WS-XML-CA
           MOVE "a tag that is not there is not an error" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE XG-RC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AC-125 - the resent copy of INV-2026-5541 arrives with a
      *>   different case and padding.  The scanner must NOT tidy it
      *>   up: APINVREG normalises and decides it is the same
      *>   document, so a structured invoice and its paper copy are
      *>   one document, not two.
       1900-REFERENCE-IS-NOT-TIDIED.
           INITIALIZE WS-XML-CA
           MOVE "  <cbc:ID> inv-2026-5541 </cbc:ID>" TO XG-RECORD
           PERFORM 0200-SCAN
           MOVE "the reference is handed over as it arrived"
               TO TK-NAME
           MOVE " inv-2026-5541" TO TK-EXPECTED-X
           MOVE XG-VALUE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the scanner does not change the case" TO TK-NAME
           MOVE "i" TO TK-EXPECTED-X
           MOVE XG-VALUE(2:1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       COPY TESTKITP.
