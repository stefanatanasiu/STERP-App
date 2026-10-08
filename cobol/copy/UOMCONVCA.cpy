      *>================================================================
      *> UOMCONVCA - COMMAREA for UOMCONV, the unit of measure service.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *>
      *> Every module that turns an entered quantity into a stock
      *> quantity calls UOMCONV.  Nothing else may assume a factor of
      *> one: a missing or zero conversion is an error, never an
      *> implied one-for-one (ITM-005).
      *>
      *>   stock quantity = entered quantity x stock units per unit
      *>   sterling unit cost = foreign price
      *>            / (price-per qty x stock units per price unit)
      *>            / foreign currency units per GBP
      *>
      *> CA-FUNCTION values
      *>   TOSTOCK   UC-ENTERED-QTY in UC-FROM-UOM        (item scoped)
      *>             -> UC-STOCK-QTY, UC-FACTOR, UC-RESULT-QTY
      *>   FROMSTK   UC-STOCK-QTY -> UC-RESULT-QTY in UC-TO-UOM
      *>   FACTOR    look up the effective stock units per UC-FROM-UOM
      *>             for UC-USAGE-TYPE at UC-EFF-DATE
      *>   CONVERT   dimensional conversion UC-FROM-UOM -> UC-TO-UOM
      *>             using inv.Uom only (no item involved)
      *>   CHECKQTY  test UC-ENTERED-QTY against the item quantum and
      *>             indivisibility -> UC-ROUNDED-QTY / UC-ADJUSTED
      *>   UNITCOST  sterling unit cost of a purchase price
      *>   ITEMUOM   list the item's effective conversions (UC-LINES)
      *>
      *> UC-USAGE-TYPE : STOCK PURCHASE SALES ISSUE PRICE PACK
      *> UC-EFF-DATE   : YYYYMMDD; 0 means the COMMAREA business date.
      *> Factors are held to nine decimals, quantities to six (D-09).
      *>
      *> REASON CODES
      *>   UOM0001 item not known in this company
      *>   UOM0002 unit of measure not known
      *>   UOM0003 no effective conversion for that unit and usage
      *>   UOM0004 conversion factor is zero or negative
      *>   UOM0005 units are dimensionally incompatible
      *>   UOM0006 quantity breaks the item quantum / indivisibility
      *>   UOM0007 price-per quantity or exchange rate is zero
      *>   UOM0008 unsupported function
      *>   UOM0009 quantity out of range
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-04-22 RJH  CR0418 STOCK CONTROL UNIT CONVERSION
      *> 1999-11-30 KMP  Y2K - EFFECTIVE DATES HOLD FOUR DIGIT YEARS
      *> 2004-07-02 RJH  CR1180 PRICE-PER QUANTITY FOR IMPORTED PARTS
      *> 2026-10-05 STERP ITM-005 TO ITM-007 EFFECTIVE DATED FACTORS
      *>================================================================
      *> REQ: ITM-005, ITM-006, ITM-007
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  UC-BODY.
               10  UC-ITEM-CODE        PIC X(40).
               10  UC-FROM-UOM         PIC X(10).
               10  UC-TO-UOM           PIC X(10).
               10  UC-USAGE-TYPE       PIC X(10).
               10  UC-EFF-DATE         PIC 9(8).
               10  UC-ENTERED-QTY      PIC S9(13)V9(6).
               10  UC-STOCK-QTY        PIC S9(13)V9(6).
               10  UC-RESULT-QTY       PIC S9(13)V9(6).
               10  UC-FACTOR           PIC S9(9)V9(9).
               10  UC-INVERSE-FACTOR   PIC S9(9)V9(9).
               10  UC-STOCK-UOM        PIC X(10).
               10  UC-FROM-DIM         PIC X(10).
               10  UC-TO-DIM           PIC X(10).
               10  UC-DECIMALS         PIC 9(2).
               10  UC-QUANTUM          PIC S9(13)V9(6).
               10  UC-INDIVISIBLE      PIC X.
                   88  UC-IS-INDIVISIBLE         VALUE "Y".
               10  UC-TRACEABILITY     PIC X(8).
               10  UC-PACK-DESC        PIC X(40).
               10  UC-EFF-FROM         PIC 9(8).
               10  UC-EFF-TO           PIC 9(8).
               10  UC-SOURCE           PIC X(10).
                   88  UC-FROM-ITEM              VALUE "ITEMUOM".
                   88  UC-FROM-UOMTAB            VALUE "UOM".
                   88  UC-FROM-IDENTITY          VALUE "IDENTITY".
               10  UC-FOREIGN-PRICE    PIC S9(11)V9(6).
               10  UC-PRICE-PER-QTY    PIC S9(9)V9(6).
               10  UC-PRICE-UOM        PIC X(10).
               10  UC-FX-PER-GBP       PIC S9(9)V9(8).
               10  UC-UNIT-COST-GBP    PIC S9(11)V9(6).
               10  UC-ROUNDED-QTY      PIC S9(13)V9(6).
               10  UC-ADJUSTED         PIC X.
                   88  UC-WAS-ADJUSTED           VALUE "Y".
               10  UC-LINE-CNT         PIC 9(3).
               10  UC-LINES OCCURS 20 TIMES.
                   15  UC-L-UOM        PIC X(10).
                   15  UC-L-USAGE      PIC X(10).
                   15  UC-L-FACTOR     PIC S9(9)V9(9).
                   15  UC-L-FROM       PIC 9(8).
                   15  UC-L-TO         PIC 9(8).
                   15  UC-L-PACK       PIC X(40).
