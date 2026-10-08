>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TBOMNET
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL - UNIT TESTS
*> PURPOSE    : Specification B2 - netting subassembly stock and
*>              buildable output - reproduced exactly.
*>              Eligible supply: five good braces, 8.000 kg plate,
*>              60 free pins, 0.400 litres resin, 0.500 litres
*>              cleaner and 12 labels.
*>              Ten frames need 19 new brace starts, two brace
*>              batches, 8.36 kg plate (short 0.36), 58 pins
*>              (spare two), 0.4 litres resin, 0.5 litres cleaner
*>              and ten labels - so ten frames are NOT buildable.
*>              Nine frames need 17 brace starts, 7.48 kg plate,
*>              52 pins, 0.4 litres resin, 0.5 litres cleaner and
*>              nine labels - every constraint passes.
*>              The exact maximum whole frame output is nine.
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2026-10-05 STERP B2 AND AC-091
*>================================================================
*> REQ: BOM-016, BOM-020, BOM-029, BOM-032
*> AC: AC-091
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TBOMNET.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY BOMNETCA REPLACING ==DFHCOMMAREA== BY ==Ws-Net==
      LEADING ==CA-== BY ==NA-==
      LEADING ==BN-== BY ==NN-==.
 01  Ws-Flags.
     05  Ws-Db-Ok                PIC X VALUE "Y".
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-Found                PIC S9(15)V9(6) VALUE 0.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TBOMNET" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Plan-Ten-Frames
     IF Ws-Db-Ok = "N"
         MOVE "BOM demonstration data or database not available"
             TO TK-DETAIL
         MOVE "B2 netted plan" TO TK-NAME
         PERFORM TK-SKIP
         PERFORM TK-END
         STOP RUN
     END-IF
     PERFORM Check-Ten-Frames
     PERFORM Plan-Nine-Frames
     PERFORM Check-Nine-Frames
     PERFORM Maximum-Buildable
     PERFORM Stock-Is-Consumed-Once
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
*> The eligible supply of B2, declared as a trial so the figures
*> are exactly the published ones and no live balance is touched.
 Set-Up-B2-Supply.
     INITIALIZE Ws-Net
     MOVE "STC" TO NA-COMPANY
     MOVE "RDG" TO NA-SITE
     MOVE "ENGINEER" TO NA-USER
     MOVE 20261001 TO NA-BUSINESS-DATE
     MOVE "DEMO-FRAME" TO NN-PARENT-ITEM
     MOVE "STANDARD" TO NN-PURPOSE
     MOVE 20261001 TO NN-EFF-DATE
     MOVE 25 TO NN-MAX-DEPTH
     MOVE "TRIAL" TO NN-STOCK-SOURCE
     MOVE "N" TO NN-USE-INBOUND
     MOVE "Free, unreserved and compatible; no other supply"
         TO NN-PRIORITY-NOTE
     MOVE 6 TO NN-SUPPLY-CNT
     MOVE "DEMO-BRACE" TO NN-S-ITEM(1)
     MOVE 5 TO NN-S-FREE-QTY(1)
     MOVE "DEMO-PLATE" TO NN-S-ITEM(2)
     MOVE 8.000 TO NN-S-FREE-QTY(2)
     MOVE "DEMO-PIN" TO NN-S-ITEM(3)
     MOVE 60 TO NN-S-FREE-QTY(3)
     MOVE "DEMO-RESIN" TO NN-S-ITEM(4)
     MOVE 0.400 TO NN-S-FREE-QTY(4)
     MOVE "DEMO-CLEAN" TO NN-S-ITEM(5)
     MOVE 0.500 TO NN-S-FREE-QTY(5)
     MOVE "DEMO-LABEL" TO NN-S-ITEM(6)
     MOVE 12 TO NN-S-FREE-QTY(6).

 Plan-Ten-Frames.
     PERFORM Set-Up-B2-Supply
     MOVE "NET" TO NA-FUNCTION
     MOVE 10 TO NN-REQ-QTY
     CALL "BOMNET" USING Ws-Net
     IF NA-RETURN-CODE = 12 OR NN-OCC-COUNT = 0
         MOVE "N" TO Ws-Db-Ok
     END-IF.

 Ask-Line.
     MOVE 0 TO Ws-Found
     MOVE "GETLINE" TO NA-FUNCTION
     MOVE 1 TO NN-FROM-LINE
     MOVE 25 TO NN-WIN-MAX
     CALL "BOMNET" USING Ws-Net.

*>----------------------------------------------------------------
 Check-Ten-Frames.
     MOVE "B2 ten frames: five stocked braces are used" TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE NN-S-USED(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Ask-Line
     MOVE "B2 ten frames: nineteen new brace starts" TO TK-NAME
     MOVE 19 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-BRACE"
             MOVE NN-L-GROSS(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM
     MOVE "B2 ten frames: two brace batches" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-BRACE"
             MOVE NN-L-BATCHES(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM
     PERFORM Sum-Requirement-Plate
     MOVE "B2 ten frames: plate requirement is 8.36 kg" TO TK-NAME
     MOVE 8.36 TO TK-EXPECTED-N
     MOVE Ws-Found TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Sum-Shortage-Plate
     MOVE "B2 ten frames: plate is short by 0.36 kg" TO TK-NAME
     MOVE 0.36 TO TK-EXPECTED-N
     MOVE Ws-Found TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Sum-Requirement-Pin
     MOVE "B2 ten frames: pin requirement is 58" TO TK-NAME
     MOVE 58 TO TK-EXPECTED-N
     MOVE Ws-Found TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B2 ten frames are not buildable now" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE NN-BUILDABLE-NOW TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Sum-Requirement-Plate.
     MOVE 0 TO Ws-Found
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-PLATE"
             ADD NN-L-REQ-QTY(Ws-I) TO Ws-Found
         END-IF
     END-PERFORM.

 Sum-Shortage-Plate.
     MOVE 0 TO Ws-Found
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-PLATE"
             ADD NN-L-SHORTAGE(Ws-I) TO Ws-Found
         END-IF
     END-PERFORM.

 Sum-Requirement-Pin.
     MOVE 0 TO Ws-Found
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-PIN"
             ADD NN-L-REQ-QTY(Ws-I) TO Ws-Found
         END-IF
     END-PERFORM.

 Sum-Requirement-Resin.
     MOVE 0 TO Ws-Found
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-RESIN"
             ADD NN-L-REQ-QTY(Ws-I) TO Ws-Found
         END-IF
     END-PERFORM.

 Sum-Requirement-Label.
     MOVE 0 TO Ws-Found
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-LABEL"
             ADD NN-L-REQ-QTY(Ws-I) TO Ws-Found
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
 Plan-Nine-Frames.
     PERFORM Set-Up-B2-Supply
     MOVE "NET" TO NA-FUNCTION
     MOVE 9 TO NN-REQ-QTY
     CALL "BOMNET" USING Ws-Net.

 Check-Nine-Frames.
     PERFORM Ask-Line
     MOVE "B2 nine frames: seventeen new brace starts" TO TK-NAME
     MOVE 17 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-BRACE"
             MOVE NN-L-GROSS(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM
     PERFORM Sum-Requirement-Plate
     MOVE "B2 nine frames: plate requirement is 7.48 kg" TO TK-NAME
     MOVE 7.48 TO TK-EXPECTED-N
     MOVE Ws-Found TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Sum-Requirement-Pin
     MOVE "B2 nine frames: pin requirement is 52" TO TK-NAME
     MOVE 52 TO TK-EXPECTED-N
     MOVE Ws-Found TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Sum-Requirement-Resin
     MOVE "B2 nine frames: resin requirement is 0.4 litres"
         TO TK-NAME
     MOVE 0.4 TO TK-EXPECTED-N
     MOVE Ws-Found TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Sum-Requirement-Label
     MOVE "B2 nine frames: nine labels" TO TK-NAME
     MOVE 9 TO TK-EXPECTED-N
     MOVE Ws-Found TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B2 nine frames have no shortage" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE NN-SHORTAGE-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> The exact maximum whole frame output under these assumptions.
 Maximum-Buildable.
     PERFORM Set-Up-B2-Supply
     MOVE "MAXBUILD" TO NA-FUNCTION
     MOVE 10 TO NN-REQ-QTY
     CALL "BOMNET" USING Ws-Net
     MOVE "B2 maximum whole frame output is nine" TO TK-NAME
     MOVE 9 TO TK-EXPECTED-N
     MOVE NN-MAX-BUILDABLE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B2 the search needed two trials" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE NN-TRIALS TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*> A stocked subassembly and the materials embodied in it are not
*> both required for the same units.
 Stock-Is-Consumed-Once.
     PERFORM Set-Up-B2-Supply
     MOVE "NET" TO NA-FUNCTION
     MOVE 9 TO NN-REQ-QTY
     CALL "BOMNET" USING Ws-Net
     MOVE "B2 only thirteen braces are made, five come from stock"
         TO TK-NAME
     PERFORM Ask-Line
     MOVE 13 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-BRACE"
             MOVE NN-L-TO-MAKE(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM
     MOVE "B2 eighteen braces are demanded in total" TO TK-NAME
     MOVE 18 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > NN-WIN-CNT
         IF NN-L-ITEM(Ws-I) = "DEMO-BRACE"
             MOVE NN-L-REQ-QTY(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM
     MOVE "B2 the five stocked braces are used once" TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE NN-S-USED(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

 COPY TESTKITP.
