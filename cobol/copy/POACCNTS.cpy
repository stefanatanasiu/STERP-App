      *>================================================================
      *> POACCNTS - PURCHASING NOMINAL ACCOUNT AND POLICY CONSTANTS
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> The 1998 distribution suite kept its nominal account numbers
      *> in one copybook so that a chart change was a single recompile.
      *> They agree with fin.EventAccountMap; the duplication is
      *> deliberate legacy texture and is recorded in the module's
      *> technical debt note.
      *>
      *> D-08 baseline tolerances are held here as the compiled
      *> fallback.  po.ToleranceRule and core.Parameter override them
      *> at run time; the literals only apply when neither is present.
      *>----------------------------------------------------------------
      *> 1998-04-02 RJH  CR0788 DISTRIBUTION SUITE
      *> 2021-01-01 TMW  BREXIT - POSTPONED IMPORT VAT ACCOUNTS
      *> 2026-10-06 STERP D-08 BASELINE TOLERANCES
      *>================================================================
       01  PO-ACCOUNTS.
           05  PO-ACC-STOCK            PIC X(12) VALUE "1100".
           05  PO-ACC-GRNI             PIC X(12) VALUE "2010".
           05  PO-ACC-ACCRUAL          PIC X(12) VALUE "2020".
           05  PO-ACC-CREDITOR         PIC X(12) VALUE "2000".
           05  PO-ACC-VAT-IN           PIC X(12) VALUE "1260".
           05  PO-ACC-VAT-OUT          PIC X(12) VALUE "2040".
           05  PO-ACC-PPV              PIC X(12) VALUE "5100".
           05  PO-ACC-LANDED           PIC X(12) VALUE "5200".
           05  PO-ACC-DUTY             PIC X(12) VALUE "5220".
           05  PO-ACC-SERVICE          PIC X(12) VALUE "5030".
           05  PO-ACC-SCRAP            PIC X(12) VALUE "5110".
       01  PO-EVENTS.
           05  PO-EV-RECEIPT           PIC X(30) VALUE "PO.RECEIPT".
           05  PO-EV-SERVICE           PIC X(30)
                                       VALUE "PO.SERVICEACCEPT".
           05  PO-EV-RETURN            PIC X(30) VALUE "PO.RETURN".
           05  PO-EV-LANDED-EST        PIC X(30) VALUE "PO.LANDEDEST".
           05  PO-EV-LANDED-ACT        PIC X(30) VALUE "PO.LANDEDACT".
           05  PO-EV-DUTY              PIC X(30) VALUE "PO.IMPORTDUTY".
           05  PO-EV-IMPORT-VAT        PIC X(30) VALUE "PO.IMPORTVAT".
      *>  D-08 purchasing tolerance baseline.  Over delivery is 0% by
      *>  default; an item or supplier override of up to 5% is allowed.
       01  PO-POLICY.
           05  PO-POL-OVER-PCT         PIC S9(5)V9(4) VALUE 0.
           05  PO-POL-OVER-MAX-PCT     PIC S9(5)V9(4) VALUE 5.
           05  PO-POL-PRICE-PCT        PIC S9(5)V9(4) VALUE 2.
           05  PO-POL-PRICE-ABS        PIC S9(9)V99   VALUE 50.
           05  PO-POL-CHARGE-ABS       PIC S9(9)V99   VALUE 25.
           05  PO-POL-VAT-ABS          PIC S9(9)V99   VALUE 0.05.
           05  PO-POL-ROUND-ABS        PIC S9(9)V99   VALUE 1.
           05  PO-POL-EARLY-DAYS       PIC S9(4)      VALUE 5.
           05  PO-POL-BUDGET-WARN      PIC S9(5)V9(4) VALUE 90.
           05  PO-POL-BUDGET-BLOCK     PIC S9(5)V9(4) VALUE 100.
           05  PO-POL-MAX-TERM-DAYS    PIC S9(4)      VALUE 60.
           05  PO-POL-DECL-SPEND       PIC S9(9)V99   VALUE 25000.
