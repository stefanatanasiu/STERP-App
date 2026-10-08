/*  Demonstration transactions for the sales ledger, for both Stellar Components (STC) and Caledon
    Assemblies (CAL), so that the debtors screens and the aged debt, statement, dunning and Direct
    Debit reports all have something real to show.  The set covers current and 30, 60 and 90 day
    overdue invoices, a credit note applied and a credit note left unapplied, full and partial
    receipts, an unallocated receipt, a customer deposit, a short payment cleared with a GBP 5.00
    small difference write-off (D-22), a partly disputed invoice with a promise to pay, a
    dishonoured Direct Debit collection that reinstates the debt, euro and US dollar invoices with a
    euro receipt that realises an exchange loss, a late payment interest claim and a Direct Debit
    mandate at Caledon.  Dates are relative to the demo "today" so that the ageing stays meaningful.
    Residuals are never typed in: they are recomputed from the allocations, which is how the real
    ledger behaves, and makes the script safe to re-run.
    REQ: AR-004, AR-009, AR-010, AR-011, AR-012, AR-013, AR-015, AR-016, AR-017, AR-018, AR-019,
    AR-020, AR-022, AR-024, AR-025, D-16, D-22, D-25, D-28 */
SET NOCOUNT ON;
GO
DECLARE @t date = CAST(GETDATE() AS date);
/*  Invoices and credit notes.  Net and VAT are in document currency; FxRate is foreign units per
    GBP 1 (D-14), so the sterling side is the document value divided by the rate. */
DECLARE @inv TABLE (Co varchar(10), DocType varchar(12), DocNo varchar(30), Cust varchar(20), DocOff int, DueOff int,
                    Ccy char(3), Fx decimal(18,8), Net decimal(18,2), VatCode varchar(4), VatPct decimal(9,4),
                    Terms varchar(12), Descr varchar(200), Qty decimal(19,6), OrderRef varchar(40), OrigDoc varchar(30));
INSERT @inv VALUES
 ('STC','INVOICE','SI-26-1001','C0001',-150,-120,'GBP',1,12000.00,'S',20,'N30','Alloy frame sets, assorted sizes',300,'PO-PCD-88120',NULL),
 ('STC','INVOICE','SI-26-1002','C0004',-100, -70,'GBP',1, 4250.00,'S',20,'N30','Wheel builds and spares',170,'PO-BCW-4417',NULL),
 ('STC','INVOICE','SI-26-1003','C0008', -75, -45,'GBP',1, 3000.00,'S',20,'N30','Hydraulic brake assemblies',120,'PO-SCC-2290',NULL),
 ('STC','INVOICE','SI-26-1004','C0003', -50,  -5,'GBP',1,25000.00,'S',20,'N45','Groupset shipment, quarter three',500,'PO-KVG-90011',NULL),
 ('STC','INVOICE','SI-26-1005','C0005', -40, -10,'GBP',1, 6400.00,'S',20,'D2N30','Finished wheelsets',80,'PO-HWC-3321',NULL),
 ('STC','INVOICE','SI-26-1006','C0007', -20,  40,'GBP',1,18000.00,'S',20,'N60','Framework subassemblies',240,'PO-MFL-7710',NULL),
 ('STC','INVOICE','SI-26-1007','C0002', -10,  20,'GBP',1, 5000.00,'S',20,'EOM30','Monthly dealer stock order',200,'PO-SBC-6602',NULL),
 ('STC','INVOICE','SI-26-1008','C0009', -65, -35,'GBP',1, 2500.00,'S',20,'N30','Touring pannier racks',100,'PO-LTB-1184',NULL),
 ('STC','INVOICE','SI-26-1009','C0021', -45, -15,'EUR',1.17500000,12000.00,'EXP',0,'N30','Export consignment, Nantes',150,'PO-VA-55120',NULL),
 ('STC','INVOICE','SI-26-1010','C0027', -30,  15,'USD',1.26500000,48000.00,'EXP',0,'N45','Export consignment, Portland',600,'PO-CCI-77301',NULL),
 ('STC','INVOICE','SI-26-1011','C0033', -95, -35,'GBP',1,40000.00,'S',20,'N60','Retail season programme',800,'PO-NRG-22014',NULL),
 ('STC','INVOICE','SI-26-1012','C0032', -70, -40,'GBP',1, 1800.00,'S',20,'N30','Hire fleet servicing parts',60,'PO-DAL-9901',NULL),
 ('STC','INVOICE','SI-26-1013','C0012', -20,  -6,'GBP',1,  900.00,'S',20,'N14','Workshop consumables',45,'PO-ARW-0312',NULL),
 ('STC','INVOICE','SI-26-1014','C0014', -25,   5,'GBP',1, 7500.00,'S',20,'N30','Council cycle scheme supply',150,'PO-MCC-40028',NULL),
 ('STC','INVOICE','SI-26-1015','C0006', -55, -25,'GBP',1, 2200.00,'S',20,'N30','Dealer replenishment',88,'PO-TRS-1190',NULL),
 ('STC','CREDITNOTE','CN-26-0001','C0001',-110,-110,'GBP',1,1000.00,'S',20,'N30','Credit for short delivery on SI-26-1001',25,NULL,'SI-26-1001'),
 ('STC','CREDITNOTE','CN-26-0002','C0003', -15, -15,'GBP',1, 500.00,'S',20,'N45','Goodwill credit, carriage recharge',1,NULL,'SI-26-1004'),
 ('CAL','INVOICE','CSI-26-2001','K0001', -80, -50,'GBP',1, 9000.00,'S',20,'N30','Assembled commuter bicycles',30,'PO-FFC-2201',NULL),
 ('CAL','INVOICE','CSI-26-2002','K0002', -40, -10,'GBP',1, 3600.00,'S',20,'N30','Trail bike assemblies',12,'PO-HTB-3310',NULL),
 ('CAL','INVOICE','CSI-26-2003','K0003', -15,  15,'GBP',1,15000.00,'S',20,'EOM30','Commuter supply contract',50,'PO-LCS-4420',NULL),
 ('CAL','INVOICE','CSI-26-2004','K0004',-130,-116,'GBP',1, 2000.00,'S',20,'N14','Hire fleet refurbishment',8,'PO-BCH-5502',NULL),
 ('CAL','INVOICE','CSI-26-2005','K0005', -25,   5,'GBP',1,22000.00,'S',20,'N30','Intercompany assembly supply',100,'PO-STC-7001',NULL),
 ('CAL','CREDITNOTE','CCN-26-2001','K0002',-12,-12,'GBP',1,400.00,'S',20,'N30','Credit for warranty return',2,NULL,'CSI-26-2002');

INSERT ar.SalesInvoice (CompanyCode, DocumentType, InvoiceNumber, CustomerCode, DocumentDate, TaxPointDate, PostingDate, DueDate,
        FiscalYear, PeriodNo, CurrencyCode, FxRate, FxRateDate, FxRateType, NetAmount, VatAmount, GrossAmount, NetAmountGBP,
        VatAmountGBP, GrossAmountGBP, TermsCode, TermsSnapshot, TermsSource, BillToName, CustomerVatRegNo, CompanyVatRegNo,
        VatLocation, CustomerOrderRef, OriginalInvoiceNumber, CreditReason, Status, PostedBy, PostedAt, IssuedAt, CreatedBy)
SELECT i.Co, i.DocType, i.DocNo, i.Cust, DATEADD(day, i.DocOff, @t), DATEADD(day, i.DocOff, @t), DATEADD(day, i.DocOff, @t),
       DATEADD(day, i.DueOff, @t), p.FiscalYear, p.PeriodNo, i.Ccy, i.Fx, DATEADD(day, i.DocOff, @t),
       CASE WHEN i.Ccy = 'GBP' THEN NULL ELSE 'SPOT' END, i.Net, ROUND(i.Net * i.VatPct / 100, 2),
       i.Net + ROUND(i.Net * i.VatPct / 100, 2), ROUND(i.Net / i.Fx, 2), ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2),
       ROUND((i.Net + ROUND(i.Net * i.VatPct / 100, 2)) / i.Fx, 2), i.Terms,
       'Terms ' + i.Terms + ' snapshotted at confirmation', 'CUSTOMER', c.LegalName, c.VatRegNo, co.VatRegNo, c.VatLocation,
       i.OrderRef, i.OrigDoc, CASE WHEN i.DocType = 'CREDITNOTE' THEN i.Descr END, 'POSTED', 'ARCLERK',
       CAST(DATEADD(day, i.DocOff, @t) AS datetime2(3)), CAST(DATEADD(day, i.DocOff, @t) AS datetime2(3)), 'DEMO'
  FROM @inv i
  JOIN ar.Customer c ON c.CompanyCode = i.Co AND c.CustomerCode = i.Cust
  JOIN core.Company co ON co.CompanyCode = i.Co
  OUTER APPLY (SELECT TOP (1) x.FiscalYear, x.PeriodNo FROM fin.Period x
                WHERE x.CompanyCode = i.Co AND DATEADD(day, i.DocOff, @t) BETWEEN x.StartDate AND x.EndDate) p
 WHERE NOT EXISTS (SELECT 1 FROM ar.SalesInvoice v WHERE v.CompanyCode = i.Co AND v.DocumentType = i.DocType AND v.InvoiceNumber = i.DocNo);

INSERT ar.SalesInvoiceLine (InvoiceId, LineNum, Description, Quantity, UomCode, UnitPrice, PricePerQty, PriceSource, NetAmount,
        VatCode, VatRatePct, VatAmount, NetAmountGBP, VatAmountGBP, VatLiability, DimCostCentre, RevenueAccountCode)
SELECT v.InvoiceId, 1, i.Descr, i.Qty, 'EA', ROUND(i.Net / NULLIF(i.Qty, 0), 6), 1, 'PRICELIST', i.Net, i.VatCode, i.VatPct,
       ROUND(i.Net * i.VatPct / 100, 2), ROUND(i.Net / i.Fx, 2), ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2),
       CASE WHEN i.VatPct > 0 THEN 'STANDARD' ELSE 'ZERO' END, 'SALES', '4000'
  FROM @inv i JOIN ar.SalesInvoice v ON v.CompanyCode = i.Co AND v.DocumentType = i.DocType AND v.InvoiceNumber = i.DocNo
 WHERE NOT EXISTS (SELECT 1 FROM ar.SalesInvoiceLine l WHERE l.InvoiceId = v.InvoiceId AND l.LineNum = 1);

INSERT ar.SalesInvoiceVat (InvoiceId, VatCode, RatePct, NetAmount, VatAmount, NetAmountGBP, VatAmountGBP, VatBox, NetBox)
SELECT v.InvoiceId, i.VatCode, i.VatPct, i.Net, ROUND(i.Net * i.VatPct / 100, 2), ROUND(i.Net / i.Fx, 2),
       ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2), CASE WHEN i.VatPct > 0 THEN 1 END, 6
  FROM @inv i JOIN ar.SalesInvoice v ON v.CompanyCode = i.Co AND v.DocumentType = i.DocType AND v.InvoiceNumber = i.DocNo
 WHERE NOT EXISTS (SELECT 1 FROM ar.SalesInvoiceVat x WHERE x.InvoiceId = v.InvoiceId AND x.VatCode = i.VatCode);

/*  The open item behind each document.  A credit note carries a negative amount so that it is
    never aged as overdue debt (section 8). */
INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate, FiscalYear, PeriodNo,
        CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, VatAmountGBP, TermsCode,
        DiscountDueDate, DiscountAmount, SettlementStatus, SourceInvoiceId, CreatedBy)
SELECT v.CompanyCode, v.CustomerCode, CASE WHEN v.DocumentType = 'CREDITNOTE' THEN 'CREDIT' ELSE 'INVOICE' END, v.InvoiceNumber,
       v.DocumentDate, v.DueDate, v.PostingDate, v.FiscalYear, v.PeriodNo, v.CurrencyCode, v.FxRate,
       v.GrossAmount * s.Sign, v.GrossAmountGBP * s.Sign, v.GrossAmount * s.Sign, v.GrossAmountGBP * s.Sign,
       v.VatAmountGBP * s.Sign, v.TermsCode,
       CASE WHEN v.TermsCode = 'D2N30' THEN DATEADD(day, 10, v.DocumentDate) END,
       CASE WHEN v.TermsCode = 'D2N30' THEN ROUND(v.GrossAmount * 0.02, 2) END, 'OPEN', v.InvoiceId, 'DEMO'
  FROM ar.SalesInvoice v
 CROSS APPLY (SELECT CASE WHEN v.DocumentType = 'CREDITNOTE' THEN -1 ELSE 1 END) AS s (Sign)
 WHERE v.CreatedBy = 'DEMO'
   AND NOT EXISTS (SELECT 1 FROM ar.OpenItem o WHERE o.CompanyCode = v.CompanyCode AND o.DocumentNumber = v.InvoiceNumber
                     AND o.ItemType = CASE WHEN v.DocumentType = 'CREDITNOTE' THEN 'CREDIT' ELSE 'INVOICE' END);
GO
/*  Receipts.  The euro receipt is banked at a worse rate than the invoice was raised at, so the
    allocation realises an exchange loss (AR-019); the Dalesway Direct Debit was returned unpaid by
    the paying bank and is therefore never allocated (AR-020, D-16). */
DECLARE @t date = CAST(GETDATE() AS date);
DECLARE @rc TABLE (Co varchar(10), RcNo varchar(30), Cust varchar(20), Method varchar(10), Bank varchar(20), DayOff int,
                   Ccy char(3), Fx decimal(18,8), Gross decimal(18,2), State varchar(12), ItemType varchar(12),
                   BankRef varchar(60), DishonourReason varchar(200));
INSERT @rc VALUES
 ('STC','RC-26-0001','C0005','BACS','GBP1', -5,'GBP',1.00000000, 4000.00,'CONFIRMED','RECEIPT','BACS 20261001 HWC',NULL),
 ('STC','RC-26-0002','C0001','BACS','GBP1',-20,'GBP',1.00000000, 5000.00,'CONFIRMED','RECEIPT','BACS 20260916 PCD',NULL),
 ('STC','RC-26-0003','C0014','BACS','GBP1', -3,'GBP',1.00000000, 2500.00,'CONFIRMED','RECEIPT','BACS 20261003 UNIDENTIFIED REF',NULL),
 ('STC','RC-26-0004','C0012','FPS', 'GBP1', -4,'GBP',1.00000000, 1075.50,'CONFIRMED','RECEIPT','FPS 20261002 ARW',NULL),
 ('STC','RC-26-0005','C0021','CHAPS','EUR1',-12,'EUR',1.20000000,12000.00,'CONFIRMED','RECEIPT','CHAPS 20260924 VELO ATLANTIQUE',NULL),
 ('STC','RC-26-0006','C0032','DD',  'GBP1',-45,'GBP',1.00000000, 2160.00,'DISHONOURED','RECEIPT','BACS DD 20260822 DALESWAY','ARUDD reason 0: refer to payer'),
 ('STC','RC-26-0007','C0011','CARD','GBP1',-12,'GBP',1.00000000, 1500.00,'CONFIRMED','DEPOSIT','CARD 20260924 FENLAND DEPOSIT',NULL),
 ('CAL','CRC-26-2001','K0001','BACS','GBP1',-7,'GBP',1.00000000, 6000.00,'CONFIRMED','RECEIPT','BACS 20260929 FFC',NULL),
 ('CAL','CRC-26-2002','K0003','BACS','GBP1',-2,'GBP',1.00000000, 3000.00,'CONFIRMED','RECEIPT','BACS 20261004 LCS ON ACCOUNT',NULL);

INSERT ar.Receipt (CompanyCode, ReceiptNumber, CustomerCode, PayerCustomerCode, PayerName, PaymentMethod, BankAccountCode,
        ReceiptDate, ValueDate, PostingDate, CurrencyCode, FxRate, GrossReceived, GrossReceivedGBP, AllocatedAmount,
        UnallocatedAmount, BankReference, ConfirmationState, DishonouredDate, DishonourReason, Status, CorrelationId, CreatedBy)
SELECT r.Co, r.RcNo, r.Cust, r.Cust, c.LegalName, r.Method, r.Bank, DATEADD(day, r.DayOff, @t), DATEADD(day, r.DayOff, @t),
       DATEADD(day, r.DayOff, @t), r.Ccy, r.Fx, r.Gross, ROUND(r.Gross / r.Fx, 2), 0, ROUND(r.Gross / r.Fx, 2), r.BankRef,
       r.State, CASE WHEN r.State = 'DISHONOURED' THEN DATEADD(day, r.DayOff + 2, @t) END, r.DishonourReason,
       CASE WHEN r.State = 'DISHONOURED' THEN 'REVERSED' ELSE 'POSTED' END, 'DEMO-' + r.RcNo, 'DEMO'
  FROM @rc r JOIN ar.Customer c ON c.CompanyCode = r.Co AND c.CustomerCode = r.Cust
 WHERE NOT EXISTS (SELECT 1 FROM ar.Receipt x WHERE x.CompanyCode = r.Co AND x.ReceiptNumber = r.RcNo);

INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate, FiscalYear, PeriodNo,
        CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, SettlementStatus,
        SourceReceiptId, CreatedBy)
SELECT p.CompanyCode, p.CustomerCode, r.ItemType, p.ReceiptNumber, p.ReceiptDate, p.ReceiptDate, p.PostingDate, f.FiscalYear,
       f.PeriodNo, p.CurrencyCode, p.FxRate, -p.GrossReceived, -p.GrossReceivedGBP, -p.GrossReceived, -p.GrossReceivedGBP,
       'OPEN', p.ReceiptId, 'DEMO'
  FROM @rc r
  JOIN ar.Receipt p ON p.CompanyCode = r.Co AND p.ReceiptNumber = r.RcNo
 OUTER APPLY (SELECT TOP (1) x.FiscalYear, x.PeriodNo FROM fin.Period x
               WHERE x.CompanyCode = p.CompanyCode AND p.PostingDate BETWEEN x.StartDate AND x.EndDate) f
 WHERE p.ConfirmationState <> 'DISHONOURED'
   AND NOT EXISTS (SELECT 1 FROM ar.OpenItem o WHERE o.CompanyCode = p.CompanyCode AND o.ItemType = r.ItemType
                     AND o.DocumentNumber = p.ReceiptNumber);
GO
/*  Allocations.  A credit note applied in full, two part payments, a short payment cleared with the
    D-22 small difference, and a cross-currency settlement that realises an exchange loss. */
DECLARE @t date = CAST(GETDATE() AS date);
DECLARE @al TABLE (Co varchar(10), SrcType varchar(10), SrcDoc varchar(30), SrcItemType varchar(12), TgtDoc varchar(30),
                   DayOff int, Applied decimal(18,2), AppliedGBP decimal(18,2), Disc decimal(18,2), WriteOff decimal(18,2),
                   Fx decimal(18,2), IsCross bit, WoReason varchar(200));
INSERT @al VALUES
 ('STC','CREDIT', 'CN-26-0001','CREDIT', 'SI-26-1001',-110, 1200.00, 1200.00,0,   0,    0,    0,NULL),
 ('STC','RECEIPT','RC-26-0002','RECEIPT','SI-26-1001', -20, 5000.00, 5000.00,0,   0,    0,    0,NULL),
 ('STC','RECEIPT','RC-26-0001','RECEIPT','SI-26-1005',  -5, 4000.00, 4000.00,0,   0,    0,    0,NULL),
 ('STC','RECEIPT','RC-26-0004','RECEIPT','SI-26-1013',  -4, 1075.50, 1075.50,0,4.50,    0,    0,'Short payment within the D-22 clerk tolerance of GBP 5.00'),
 ('STC','RECEIPT','RC-26-0005','RECEIPT','SI-26-1009', -12,12000.00,10212.77,0,   0,-212.77,  1,NULL),
 ('CAL','RECEIPT','CRC-26-2001','RECEIPT','CSI-26-2001',-7, 6000.00, 6000.00,0,   0,    0,    0,NULL);

INSERT ar.Allocation (CompanyCode, CustomerCode, SourceType, SourceOpenItemId, TargetOpenItemId, EffectiveDate, CurrencyCode,
        AppliedAmount, AppliedAmountGBP, DiscountAmountGBP, WriteOffAmountGBP, WriteOffReason, RealisedFxGBP, IsCrossCurrency,
        ConversionRate, Status, CreatedBy)
SELECT a.Co, t.CustomerCode, a.SrcType, s.OpenItemId, t.OpenItemId, DATEADD(day, a.DayOff, @t), t.CurrencyCode, a.Applied,
       a.AppliedGBP, a.Disc, a.WriteOff, a.WoReason, a.Fx, a.IsCross, CASE WHEN a.IsCross = 1 THEN s.FxRate END, 'ACTIVE', 'DEMO'
  FROM @al a
  JOIN ar.OpenItem s ON s.CompanyCode = a.Co AND s.DocumentNumber = a.SrcDoc AND s.ItemType = a.SrcItemType
  JOIN ar.OpenItem t ON t.CompanyCode = a.Co AND t.DocumentNumber = a.TgtDoc AND t.ItemType = 'INVOICE'
 WHERE NOT EXISTS (SELECT 1 FROM ar.Allocation x WHERE x.SourceOpenItemId = s.OpenItemId AND x.TargetOpenItemId = t.OpenItemId);
GO
/*  Residuals are derived, never edited (ar.OpenItem header comment), so recompute them from the
    allocations.  A target is cleared by cash, settlement discount and write-off together; a source
    is consumed by the cash applied and by the exchange difference realised on it. */
UPDATE o
   SET ResidualAmount = o.OriginalAmount - ISNULL(t.Doc, 0) + ISNULL(s.Doc, 0),
       ResidualAmountGBP = o.OriginalAmountGBP - ISNULL(t.Gbp, 0) + ISNULL(s.Gbp, 0),
       SettlementStatus = CASE WHEN o.OriginalAmountGBP - ISNULL(t.Gbp, 0) + ISNULL(s.Gbp, 0) = 0 THEN 'SETTLED'
                               WHEN ISNULL(t.Gbp, 0) + ISNULL(s.Gbp, 0) <> 0 THEN 'PARTIAL' ELSE 'OPEN' END,
       ModifiedBy = 'DEMO', ModifiedAt = SYSDATETIME()
  FROM ar.OpenItem o
 OUTER APPLY (SELECT SUM(x.AppliedAmount), SUM(x.AppliedAmountGBP + x.DiscountAmountGBP + x.WriteOffAmountGBP)
                FROM ar.Allocation x WHERE x.TargetOpenItemId = o.OpenItemId AND x.Status = 'ACTIVE') AS t (Doc, Gbp)
 OUTER APPLY (SELECT SUM(x.AppliedAmount), SUM(x.AppliedAmountGBP + x.RealisedFxGBP)
                FROM ar.Allocation x WHERE x.SourceOpenItemId = o.OpenItemId AND x.Status = 'ACTIVE') AS s (Doc, Gbp)
 WHERE o.CreatedBy = 'DEMO';
UPDATE p
   SET AllocatedAmount = ISNULL(a.Applied, 0), UnallocatedAmount = p.GrossReceived - ISNULL(a.Applied, 0),
       ModifiedBy = 'DEMO', ModifiedAt = SYSDATETIME()
  FROM ar.Receipt p
 OUTER APPLY (SELECT SUM(x.AppliedAmount) FROM ar.Allocation x JOIN ar.OpenItem o ON o.OpenItemId = x.SourceOpenItemId
               WHERE o.SourceReceiptId = p.ReceiptId AND x.Status = 'ACTIVE') AS a (Applied)
 WHERE p.CreatedBy = 'DEMO';
GO
/*  Dispute, collection state and the promise to pay.  A dispute is partial: collection continues
    for the undisputed 1,800.00 while the 700.00 quality claim is investigated (section 8). */
DECLARE @t date = CAST(GETDATE() AS date);
UPDATE ar.OpenItem SET IsDisputed = 1, DisputedAmountGBP = 700.00, CollectionStatus = 'DISPUTED', ModifiedBy = 'DEMO', ModifiedAt = SYSDATETIME()
 WHERE CompanyCode = 'STC' AND ItemType = 'INVOICE' AND DocumentNumber = 'SI-26-1008' AND CreatedBy = 'DEMO';
UPDATE ar.OpenItem SET CollectionStatus = 'ESCALATED', LastReminderStep = 4, LastReminderDate = DATEADD(day, -14, @t), ModifiedBy = 'DEMO', ModifiedAt = SYSDATETIME()
 WHERE CompanyCode = 'STC' AND ItemType = 'INVOICE' AND DocumentNumber = 'SI-26-1001' AND CreatedBy = 'DEMO';
UPDATE ar.OpenItem SET CollectionStatus = 'PROMISED', LastReminderStep = 2, LastReminderDate = DATEADD(day, -9, @t), ModifiedBy = 'DEMO', ModifiedAt = SYSDATETIME()
 WHERE CompanyCode = 'STC' AND ItemType = 'INVOICE' AND DocumentNumber = 'SI-26-1002' AND CreatedBy = 'DEMO';

DECLARE @cc TABLE (Co varchar(10), CaseRef varchar(30), Cust varchar(20), CaseType varchar(12), Doc varchar(30),
                   ReasonCode varchar(20), Reason varchar(300), Disputed decimal(18,2), Owner varchar(20), Step tinyint,
                   NextOff int, NextAction varchar(200), Status varchar(12), Escalated varchar(40));
INSERT @cc VALUES
 ('STC','CASE-26-0001','C0009','DISPUTE','SI-26-1008','QUALITY','Customer disputes 700.00 of pannier racks as damaged on delivery; photographs received',700.00,'CREDMGR',2,3,'Quality to confirm the credit; collect the undisputed 1,800.00','OPEN',NULL),
 ('STC','CASE-26-0002','C0004','COLLECTION','SI-26-1002','CASHFLOW','Account 70 days overdue; customer cites a delayed end customer payment',0,'ARCLERK',2,7,'Promise to pay in full on the agreed date','OPEN',NULL),
 ('STC','CASE-26-0003','C0001','COLLECTION','SI-26-1001','NONPAY','Oldest debt 120 days past due after a part payment and a credit note',0,'CREDMGR',4,2,'Letter before action issued; prepare external recovery hand-off','ESCALATED','Pre-action protocol letter issued'),
 ('CAL','CASE-26-2001','K0004','COLLECTION','CSI-26-2004','NONPAY','Hire fleet account 116 days past due with no contact since the second reminder',0,'CREDMGR',4,1,'Escalate to the credit manager for a letter before action','ESCALATED','Credit manager review');
INSERT ar.CollectionCase (CompanyCode, CustomerCode, CaseRef, CaseType, OpenItemId, DocumentNumber, ReasonCode, Reason,
        DisputedAmountGBP, OwnerUser, CollectionStep, NextActionDate, NextAction, EscalatedTo, Status, OpenedAt, CreatedBy)
SELECT k.Co, k.Cust, k.CaseRef, k.CaseType, o.OpenItemId, k.Doc, k.ReasonCode, k.Reason, k.Disputed, k.Owner, k.Step,
       DATEADD(day, k.NextOff, @t), k.NextAction, k.Escalated, k.Status, CAST(DATEADD(day, -21, @t) AS datetime2(3)), 'DEMO'
  FROM @cc k LEFT JOIN ar.OpenItem o ON o.CompanyCode = k.Co AND o.DocumentNumber = k.Doc AND o.ItemType = 'INVOICE'
 WHERE NOT EXISTS (SELECT 1 FROM ar.CollectionCase x WHERE x.CompanyCode = k.Co AND x.CaseRef = k.CaseRef);

DECLARE @ca TABLE (CaseRef varchar(30), Seq int, ActionType varchar(14), DayOff int, ByUser varchar(20), Contact varchar(80),
                   Narrative varchar(400), PromiseAmt decimal(18,2), PromiseOff int, PromiseStatus varchar(10));
INSERT @ca VALUES
 ('CASE-26-0001',1,'REMINDER1',-28,'ARCLERK','Accounts Payable','Courteous reminder issued showing residual and payment details',NULL,NULL,NULL),
 ('CASE-26-0001',2,'CALL',     -21,'CREDMGR','H Pike','Customer raised a quality claim for 700.00; undisputed balance agreed as collectable',NULL,NULL,NULL),
 ('CASE-26-0001',3,'NOTE',      -7,'CREDMGR','H Pike','Photographs received and passed to quality; collection continues on the undisputed amount',NULL,NULL,NULL),
 ('CASE-26-0002',1,'REMINDER1',-63,'ARCLERK','Accounts Payable','Courteous reminder issued',NULL,NULL,NULL),
 ('CASE-26-0002',2,'REMINDER2',-49,'ARCLERK','Accounts Payable','Firm reminder citing statutory interest and compensation',NULL,NULL,NULL),
 ('CASE-26-0002',3,'PROMISE',   -2,'ARCLERK','J Brackley','Customer promised payment in full of the outstanding 5,100.00',5100.00,7,'OPEN'),
 ('CASE-26-0003',1,'REMINDER2',-99,'ARCLERK','Accounts Payable','Firm reminder issued',NULL,NULL,NULL),
 ('CASE-26-0003',2,'STOP',     -90,'CREDMGR','Credit control','Account stopped for new release; owner notified',NULL,NULL,NULL),
 ('CASE-26-0003',3,'PROMISE',  -60,'CREDMGR','R Nuttall','Promise of 5,000.00 made and kept by BACS receipt RC-26-0002',5000.00,-40,'KEPT'),
 ('CASE-26-0003',4,'DEMAND',   -14,'CREDMGR','R Nuttall','Letter before action issued under the Pre-Action Protocol for Debt Claims',NULL,NULL,NULL),
 ('CASE-26-2001',1,'REMINDER1',-95,'ARCLERK','Accounts Payable','Courteous reminder issued',NULL,NULL,NULL),
 ('CASE-26-2001',2,'REMINDER2',-80,'ARCLERK','Accounts Payable','Firm reminder issued',NULL,NULL,NULL),
 ('CASE-26-2001',3,'PROMISE',  -45,'CREDMGR','S Borders','Promise of 2,400.00 not honoured on the promised date',2400.00,-30,'BROKEN');
INSERT ar.CollectionAction (CaseId, ActionSeq, ActionType, ActionDate, PerformedBy, ContactName, Narrative, PromiseAmountGBP,
        PromiseDate, PromiseStatus)
SELECT k.CaseId, a.Seq, a.ActionType, DATEADD(day, a.DayOff, @t), a.ByUser, a.Contact, a.Narrative, a.PromiseAmt,
       CASE WHEN a.PromiseOff IS NOT NULL THEN DATEADD(day, a.PromiseOff, @t) END, a.PromiseStatus
  FROM @ca a JOIN ar.CollectionCase k ON k.CaseRef = a.CaseRef AND k.CreatedBy = 'DEMO'
 WHERE NOT EXISTS (SELECT 1 FROM ar.CollectionAction x WHERE x.CaseId = k.CaseId AND x.ActionSeq = a.Seq);
GO
/*  Direct Debit: a planned collection under advance notice at Stellar, the Dalesway collection
    returned unpaid, and a mandate at Caledon so that both companies can demonstrate the scheme.
    REQ: AR-025, D-16 */
DECLARE @t date = CAST(GETDATE() AS date);
INSERT ar.DirectDebitMandate (CompanyCode, CustomerCode, MandateReference, ServiceUserNumber, PayerName, SortCode, AccountNumber,
        SignedDate, LodgedDate, FirstCollectionDate, AdvanceNoticeDays, AdvanceNoticeAgreedInWriting, Status, CancelledDate,
        CancelReason, GuaranteeIssuedAt, CreatedBy)
SELECT v.Co, v.Cust, v.Ref, v.Sun, c.LegalName, v.Sort, v.Acct, DATEADD(day, v.SignOff, @t), DATEADD(day, v.SignOff + 4, @t),
       DATEADD(day, v.SignOff + 20, @t), 3, 1, v.Status, CASE WHEN v.Status = 'CANCELLED' THEN DATEADD(day, -43, @t) END,
       CASE WHEN v.Status = 'CANCELLED' THEN 'Collection returned unpaid by the paying bank; account moved to Bacs credit' END,
       CAST(DATEADD(day, v.SignOff + 4, @t) AS datetime2(3)), 'DEMO'
  FROM (VALUES ('CAL','K0002','CAL-DD-K0002','901144','832217','55012288',-200,'ACTIVE'),
               ('STC','C0032','STC-DD-C0032','834512','203045','77110044',-420,'CANCELLED')) AS v (Co, Cust, Ref, Sun, Sort, Acct, SignOff, Status)
  JOIN ar.Customer c ON c.CompanyCode = v.Co AND c.CustomerCode = v.Cust
 WHERE NOT EXISTS (SELECT 1 FROM ar.DirectDebitMandate m WHERE m.CompanyCode = v.Co AND m.MandateReference = v.Ref);

INSERT ar.DirectDebitCollection (MandateId, CompanyCode, CustomerCode, CollectionRef, AdvanceNoticeSentAt, SubmissionDate,
        ProcessingDate, DueDate, AmountGBP, TransactionCode, BacsFileRef, Status, UnpaidReasonCode, UnpaidDate, ReceiptId,
        OpenItemIds, CorrelationId, CreatedBy)
SELECT m.MandateId, d.Co, d.Cust, d.Ref, DATEADD(day, d.NoticeOff, @t), DATEADD(day, d.SubOff, @t), DATEADD(day, d.SubOff + 1, @t),
       DATEADD(day, d.DueOff, @t), d.Amount, d.TxnCode, d.FileRef, d.Status, d.UnpaidCode,
       CASE WHEN d.Status = 'UNPAID' THEN DATEADD(day, d.DueOff + 2, @t) END, p.ReceiptId, d.Items, 'DEMO-' + d.Ref, 'DEMO'
  FROM (VALUES
     ('STC','C0002','DDC-26-0001', -4, -1,  3, 6000.00,'01','BACS-2026-279','PLANNED',   NULL, 'SI-26-1007', NULL),
     ('STC','C0032','DDC-26-0002',-52,-48,-45, 2160.00,'17','BACS-2026-234','UNPAID',    '0',  'SI-26-1012','RC-26-0006'),
     ('CAL','K0002','DDC-26-2001', -6, -3,  1, 4320.00,'01','BACS-2026-277','SUBMITTED', NULL, 'CSI-26-2002',NULL)
   ) AS d (Co, Cust, Ref, NoticeOff, SubOff, DueOff, Amount, TxnCode, FileRef, Status, UnpaidCode, Items, RcNo)
  JOIN ar.DirectDebitMandate m ON m.CompanyCode = d.Co AND m.CustomerCode = d.Cust AND m.Status IN ('ACTIVE', 'CANCELLED')
  LEFT JOIN ar.Receipt p ON p.CompanyCode = d.Co AND p.ReceiptNumber = d.RcNo
 WHERE NOT EXISTS (SELECT 1 FROM ar.DirectDebitCollection x WHERE x.CompanyCode = d.Co AND x.CollectionRef = d.Ref);
GO
/*  A statutory interest claim on the oldest debt.  8,200.00 outstanding for 120 days at the
    reference rate of 4.00 per cent plus the statutory 8 per cent margin gives interest of
    8,200.00 x 12% x 120 / 365 = 323.51, and fixed compensation of 70.00 because the debt is
    between 1,000.00 and 9,999.99.  Neither amount carries VAT.  REQ: AR-024, D-25, D-28 */
DECLARE @t date = CAST(GETDATE() AS date);
INSERT ar.LatePaymentClaim (CompanyCode, CustomerCode, ClaimRef, OpenItemId, InvoiceNumber, InvoiceDueDate, PrincipalGBP,
        DaysOverdue, ReferenceRatePct, MarginPct, InterestGBP, CompensationGBP, RecoveryCostsGBP, TotalClaimGBP, CalculatedTo, Status)
SELECT 'STC', 'C0001', 'LPC-26-0001', o.OpenItemId, 'SI-26-1001', o.DueDate, 8200.00, 120, 4.0000, 8.0000, 323.51, 70.00, 0,
       393.51, @t, 'CALC'
  FROM ar.OpenItem o
 WHERE o.CompanyCode = 'STC' AND o.ItemType = 'INVOICE' AND o.DocumentNumber = 'SI-26-1001' AND o.CreatedBy = 'DEMO'
   AND NOT EXISTS (SELECT 1 FROM ar.LatePaymentClaim x WHERE x.CompanyCode = 'STC' AND x.ClaimRef = 'LPC-26-0001');
GO
PRINT 'Seeded AR demonstration transactions for STC and CAL';
GO
