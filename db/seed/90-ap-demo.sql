/*  Demonstration transactions for the purchase ledger, for both Stellar Components (STC) and
    Caledon Assemblies (CAL), so that the creditors screens and the aged creditors, remittance,
    BACS and payment practices reports all have something real to show.  The set covers current
    and 30, 60 and 90 day overdue supplier invoices, order matched and non order invoices, a line
    inside the price tolerance and a line outside the quantity tolerance that blocks payment, a
    disputed invoice, an invoice held for missing VAT evidence (non recoverable input tax), euro
    and US dollar invoices, an applied and an unapplied credit note, a completed BACS payment run
    with remittances for both companies, two statutory late payment claims, two supplier
    statements (one reconciled, one with differences), a bank detail change that is verified but
    still waiting for an independent approver, and a Confirmation of Payee mismatch.
    Residuals are never typed in: they are recomputed from the allocations, which is how the real
    ledger behaves and makes the script safe to re-run.  The sterling side of a foreign document
    is the document value divided by FxRate, which is foreign units per GBP 1 (D-14).
    The subledger is backed by real general ledger postings through fin.JournalStage and
    fin.usp_PostJournal, so that account 2000 (trade creditors control) agrees with the sum of
    the open items.  Because the documents are deliberately back dated into closed periods the
    demo journals are posted with the current business date; they are skipped altogether when no
    open period covers it.
    REQ: AP-003, AP-004, AP-006, AP-007, AP-008, AP-009, AP-010, AP-011, AP-013, AP-014, AP-016,
    AP-017, AP-019, AP-021, AP-023, AP-025, AP-027, AP-028, AP-030, D-14, D-25, D-26 */
SET NOCOUNT ON;
GO
DECLARE @t date = CAST(GETDATE() AS date);

/*  Supplier invoices and credit notes.  Net and VAT are in document currency. */
DECLARE @inv TABLE (Co varchar(10), DocType varchar(12), Reg varchar(30), Supp varchar(20), DocRef varchar(40),
    DocOff int, DueOff int, Ccy char(3), Fx decimal(18,8), Net decimal(18,2), VatCode varchar(4), VatPct decimal(9,4),
    Terms varchar(12), Descr varchar(200), Qty decimal(19,6), Po varchar(30), Rcpt varchar(30), Acct varchar(12),
    CC varchar(20), Nature varchar(10), RegSt varchar(14), MatchSt varchar(14), AcctSt varchar(14), PaySt varchar(14),
    SetSt varchar(12), Blocked bit, Disputed bit, Reason varchar(200), NonPo bit, RecVat bit);
INSERT @inv VALUES
 ('STC','INVOICE','PI-26-0001','S0001','AAS/118220', -95,-65,'GBP',1,24000.00,'S',20,'N60','Aluminium billet, 6082 T6',600,'PO-STC-71001','GRN-STC-5101','1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','SETTLED',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0002','S0002','PTS-44190', -80,-35,'GBP',1, 8600.00,'S',20,'N45','Welded tube, assorted sections',430,'PO-STC-71014','GRN-STC-5118','1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','SETTLED',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0003','S0003','CSS 90221', -70,-40,'GBP',1, 5400.00,'S',20,'N30','Sheet steel, cut to size',180,'PO-STC-71022','GRN-STC-5127','1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','SETTLED',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0004','S0005','WPB/20714', -62,-32,'GBP',1, 3200.00,'S',20,'N30','Sealed bearings, 6001 2RS',1600,'PO-STC-71030','GRN-STC-5140','1100','PURCH','STOCK','READY','VARIANCE','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0005','S0006','CF-11903', -48,-18,'GBP',1, 1850.00,'S',20,'N30','Socket cap screws, M6 and M8',9250,'PO-STC-71041','GRN-STC-5152','1100','PURCH','STOCK','READY','VARIANCE','POSTED','BLOCKED','UNPAID',1,0,'Quantity invoiced exceeds the accepted receipt by 185 EA, outside the 2 percent tolerance',0,1),
 ('STC','INVOICE','PI-26-0006','S0009','TVC/6620', -40,-10,'GBP',1, 2750.00,'S',20,'N30','Degreasing fluid, 20 litre drums',55,'PO-STC-71048','GRN-STC-5160','5400','PROD2','EXPENSE','READY','MATCHED','POSTED','BLOCKED','UNPAID',1,1,'Supplier disputed: two drums leaked in transit, credit requested',0,1),
 ('STC','INVOICE','PI-26-0007','S0013','WLG-330118', -35, -5,'GBP',1, 4100.00,'S',20,'N30','Inbound haulage, week 34 to 37',1,'PO-STC-71055',NULL,'6210','WHSE','EXPENSE','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0008','S0016','QU-9910442', -20, 10,'GBP',1,  980.00,'S',20,'N30','Electricity, Reading works',1,NULL,NULL,'6110','ADMIN','EXPENSE','READY','NOTREQUIRED','POSTED','APPROVED','UNPAID',0,0,NULL,1,1),
 ('STC','INVOICE','PI-26-0009','S0019','ORB-20266', -12, 18,'GBP',1, 6600.00,'S',20,'N30','Managed desktop service, quarter 3',1,'PO-STC-71061',NULL,'6300','ADMIN','EXPENSE','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0010','S0021','JPM 441',  -8,  6,'GBP',1, 1450.00,'S',20,'N14','Jig repair and re-alignment',1,'PO-STC-71066',NULL,'5400','PROD2','EXPENSE','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0011','S0023','RR-2026-8841', -30,  0,'EUR',1.17500000, 9000.00,'RC',0,'N30','Hub shells and axles, reverse charge',450,'PO-STC-71070','GRN-STC-5171','1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0012','S0027','CEI-55120', -25,  5,'USD',1.26500000,18000.00,'Z',0,'N45','Controller boards, import',900,'PO-STC-71074','GRN-STC-5178','1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('STC','INVOICE','PI-26-0013','S0030','PTP/0042', -14, 16,'GBP',1,  760.00,'S',20,'N30','Trade exhibition hospitality',1,NULL,NULL,'6800','ADMIN','EXPENSE','READY','NOTREQUIRED','AWAITAPPROVAL','BLOCKED','UNPAID',1,0,'Non order invoice awaiting cost coding approval; VAT evidence incomplete',1,0),
 ('STC','INVOICE','PI-26-0014','S0011','NGP-71188',-120,-90,'GBP',1, 3300.00,'S',20,'N30','Corrugated cases and void fill',3300,'PO-STC-70980','GRN-STC-5044','1100','PURCH','STOCK','READY','MATCHED','POSTED','BLOCKED','UNPAID',1,1,'Short delivery under query since day 30; credit note expected',0,1),
 ('STC','CREDITNOTE','PC-26-0001','S0001','AAS/CN-2210', -60,-60,'GBP',1, 1200.00,'S',20,'N60','Credit for rejected billet on AAS/118220',30,'PO-STC-71001',NULL,'1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','SETTLED',0,0,NULL,0,1),
 ('STC','CREDITNOTE','PC-26-0002','S0011','NGP/CN-991', -45,-45,'GBP',1,  900.00,'S',20,'N30','Credit for short delivery on NGP-71188',900,'PO-STC-70980',NULL,'1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('CAL','INVOICE','CPI-26-2001','T0001','STC-INT-4410', -40,-10,'GBP',1,12000.00,'S',20,'N30','Component kits, intercompany supply',400,'PO-CAL-30110','GRN-CAL-2201','1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','SETTLED',0,0,NULL,0,1),
 ('CAL','INVOICE','CPI-26-2002','T0003','LLL-7730', -25,  5,'GBP',1, 2600.00,'S',20,'N30','Distribution, central belt',1,'PO-CAL-30118',NULL,'6210','WHSE','EXPENSE','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('CAL','INVOICE','CPI-26-2003','T0004','FVU-220114', -70,-55,'GBP',1, 1800.00,'S',20,'DOM15','Gas and electricity, Motherwell',1,NULL,NULL,'6110','ADMIN','EXPENSE','READY','NOTREQUIRED','POSTED','BLOCKED','UNPAID',1,1,'Meter reading queried with the supplier',1,1),
 ('CAL','INVOICE','CPI-26-2004','T0005','KWS-4418', -15, -1,'GBP',1,  940.00,'S',20,'N14','Workshop fitting services',1,'PO-CAL-30125',NULL,'5400','PROD2','EXPENSE','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('CAL','INVOICE','CPI-26-2005','T0002','CCB-66021',  -9, 21,'GBP',1, 1250.00,'S',20,'N30','Cartons and printed sleeves',2500,'PO-CAL-30131','GRN-CAL-2214','1100','PURCH','STOCK','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1),
 ('CAL','CREDITNOTE','CPC-26-2001','T0003','LLL/CN-118', -20,-20,'GBP',1,  300.00,'S',20,'N30','Credit for missed collection',1,'PO-CAL-30118',NULL,'6210','WHSE','EXPENSE','READY','MATCHED','POSTED','APPROVED','UNPAID',0,0,NULL,0,1);

INSERT ap.PurchaseInvoice (CompanyCode, DocumentType, RegisterNumber, SupplierCode, SupplierDocRef, SupplierDocRefNormalised,
        DateReceived, DocumentDate, TaxPointDate, PostingDate, DueDate, FiscalYear, PeriodNo, VatPeriodKey, CurrencyCode,
        FxRate, FxRateDate, StatedNet, StatedVat, StatedGross, NetAmountGBP, VatAmountGBP, GrossAmountGBP, RecoverableVatGBP,
        NonRecoverableVatGBP, VatEvidenceValid, VatEvidenceNote, TermsCode, TermsSnapshot, RegistrationState, MatchState,
        AccountingState, PaymentEligibility, SettlementState, BlockReason, OwnerUser, ApprovedBy, ApprovedAt, IsNonPo,
        NonPoReason, CreatedBy)
SELECT i.Co, i.DocType, i.Reg, i.Supp, i.DocRef, ap.fn_NormaliseInvoiceRef(i.DocRef),
       DATEADD(day, i.DocOff + 2, @t), DATEADD(day, i.DocOff, @t), DATEADD(day, i.DocOff, @t),
       DATEADD(day, i.DocOff + 2, @t), DATEADD(day, i.DueOff, @t), p.FiscalYear, p.PeriodNo,
       CONVERT(varchar(7), DATEADD(day, i.DocOff, @t), 126), i.Ccy, i.Fx, DATEADD(day, i.DocOff, @t),
       i.Net, ROUND(i.Net * i.VatPct / 100, 2), i.Net + ROUND(i.Net * i.VatPct / 100, 2), ROUND(i.Net / i.Fx, 2),
       ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2), ROUND((i.Net + ROUND(i.Net * i.VatPct / 100, 2)) / i.Fx, 2),
       CASE WHEN i.RecVat = 1 THEN ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2) ELSE 0 END,
       CASE WHEN i.RecVat = 1 THEN 0 ELSE ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2) END,
       i.RecVat, CASE WHEN i.RecVat = 0 THEN 'No valid VAT invoice held; input tax treated as irrecoverable until evidence is produced' END,
       i.Terms, 'Terms ' + i.Terms + ' taken from the supplier master at registration', i.RegSt, i.MatchSt, i.AcctSt,
       i.PaySt, i.SetSt, i.Reason, 'APCLERK', CASE WHEN i.PaySt = 'APPROVED' THEN 'APMGR' END,
       CASE WHEN i.PaySt = 'APPROVED' THEN CAST(DATEADD(day, i.DocOff + 3, @t) AS datetime2(3)) END, i.NonPo,
       CASE WHEN i.NonPo = 1 THEN 'Utility or overhead charge outside the order process' END, 'DEMO'
  FROM @inv i
  OUTER APPLY (SELECT TOP (1) x.FiscalYear, x.PeriodNo FROM fin.Period x
                WHERE x.CompanyCode = i.Co AND DATEADD(day, i.DocOff + 2, @t) BETWEEN x.StartDate AND x.EndDate) p
 WHERE EXISTS (SELECT 1 FROM ap.Supplier s WHERE s.CompanyCode = i.Co AND s.SupplierCode = i.Supp)
   AND NOT EXISTS (SELECT 1 FROM ap.PurchaseInvoice v WHERE v.CompanyCode = i.Co AND v.RegisterNumber = i.Reg);

UPDATE ap.PurchaseInvoice
   SET SupplierDocRefNormalised = ap.fn_NormaliseInvoiceRef(SupplierDocRef)
 WHERE CreatedBy = 'DEMO'
   AND SupplierDocRefNormalised <> ap.fn_NormaliseInvoiceRef(SupplierDocRef);

INSERT ap.PurchaseInvoiceLine (PurchaseInvoiceId, LineNum, Description, PurchaseOrderNumber, PurchaseOrderLine, ReceiptRef,
        ReceiptLine, Quantity, UomCode, UnitPrice, PricePerQty, NetAmount, VatCode, VatRatePct, VatAmount, NetAmountGBP,
        VatAmountGBP, IsRecoverableVat, AccountCode, DimCostCentre, CostNature)
SELECT v.PurchaseInvoiceId, 1, i.Descr, i.Po, CASE WHEN i.Po IS NULL THEN NULL ELSE 1 END, i.Rcpt,
       CASE WHEN i.Rcpt IS NULL THEN NULL ELSE 1 END, i.Qty, 'EA', ROUND(i.Net / NULLIF(i.Qty, 0), 6), 1, i.Net, i.VatCode,
       i.VatPct, ROUND(i.Net * i.VatPct / 100, 2), ROUND(i.Net / i.Fx, 2), ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2),
       i.RecVat, i.Acct, i.CC, i.Nature
  FROM @inv i JOIN ap.PurchaseInvoice v ON v.CompanyCode = i.Co AND v.RegisterNumber = i.Reg
 WHERE NOT EXISTS (SELECT 1 FROM ap.PurchaseInvoiceLine l WHERE l.PurchaseInvoiceId = v.PurchaseInvoiceId AND l.LineNum = 1);

INSERT ap.PurchaseInvoiceVat (PurchaseInvoiceId, VatCode, RatePct, NetAmount, VatAmount, NetAmountGBP, VatAmountGBP,
        RecoverableGBP, VatBox, NetBox)
SELECT v.PurchaseInvoiceId, i.VatCode, i.VatPct, i.Net, ROUND(i.Net * i.VatPct / 100, 2), ROUND(i.Net / i.Fx, 2),
       ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2),
       CASE WHEN i.RecVat = 1 THEN ROUND(ROUND(i.Net * i.VatPct / 100, 2) / i.Fx, 2) ELSE 0 END,
       CASE WHEN i.VatPct > 0 OR i.VatCode = 'RC' THEN 4 ELSE NULL END, 7
  FROM @inv i JOIN ap.PurchaseInvoice v ON v.CompanyCode = i.Co AND v.RegisterNumber = i.Reg
 WHERE NOT EXISTS (SELECT 1 FROM ap.PurchaseInvoiceVat x WHERE x.PurchaseInvoiceId = v.PurchaseInvoiceId AND x.VatCode = i.VatCode);

/*  AP-022 / FIN-025 - a reverse charge is declared on both sides, so it is reported in a VAT box
    (the recoverable input tax, box 4) as well as a net box (box 7), never in the net box alone. */
UPDATE ap.PurchaseInvoiceVat
   SET VatBox = 4
 WHERE VatCode = 'RC' AND VatBox IS NULL;
UPDATE ap.PurchaseInvoiceVat
   SET NetBox = 7
 WHERE VatCode = 'RC' AND NetBox IS NULL;

/*  Three way match evidence.  Most lines agree; PI-26-0004 is inside the price tolerance and
    PI-26-0005 is outside the quantity tolerance, which is what blocks it for payment (AP-006, AP-008). */
INSERT ap.InvoiceMatch (CompanyCode, PurchaseInvoiceId, PurchaseInvoiceLineNum, PurchaseOrderNumber, PurchaseOrderLine,
        ReceiptRef, ReceiptLine, OrderedQty, ReceivedAcceptedQty, MatchedQty, OrderUnitPrice, InvoiceUnitPrice,
        QtyVariance, PriceVariancePct, PriceVarianceGBP, GrniClearedGBP, WithinTolerance, ToleranceNote, ApprovedBy,
        Status, CreatedBy)
SELECT i.Co, v.PurchaseInvoiceId, 1, i.Po, 1, i.Rcpt, CASE WHEN i.Rcpt IS NULL THEN NULL ELSE 1 END, i.Qty,
       CASE i.Reg WHEN 'PI-26-0005' THEN i.Qty - 185 ELSE i.Qty END,
       CASE i.Reg WHEN 'PI-26-0005' THEN i.Qty - 185 ELSE i.Qty END,
       CASE i.Reg WHEN 'PI-26-0004' THEN ROUND(i.Net / NULLIF(i.Qty, 0) / 1.015, 6) ELSE ROUND(i.Net / NULLIF(i.Qty, 0), 6) END,
       ROUND(i.Net / NULLIF(i.Qty, 0), 6), CASE i.Reg WHEN 'PI-26-0005' THEN 185 ELSE 0 END,
       CASE i.Reg WHEN 'PI-26-0004' THEN 1.5000 ELSE 0 END,
       CASE i.Reg WHEN 'PI-26-0004' THEN ROUND(i.Net - (i.Net / 1.015), 2) ELSE 0 END,
       CASE WHEN i.Reg = 'PI-26-0005' THEN 0 ELSE ROUND(i.Net / i.Fx, 2) END,
       CASE WHEN i.Reg = 'PI-26-0005' THEN 0 ELSE 1 END,
       CASE i.Reg WHEN 'PI-26-0004' THEN 'Unit price 1.5 percent above order, inside the 2 percent and GBP 50 tolerance'
                  WHEN 'PI-26-0005' THEN 'Invoiced quantity 185 EA above the accepted receipt, outside tolerance' END,
       CASE WHEN i.Reg IN ('PI-26-0005', 'PI-26-0014') THEN NULL ELSE 'APMGR' END,
       CASE WHEN i.Reg IN ('PI-26-0005', 'PI-26-0014') THEN 'DISPUTED' ELSE 'MATCHED' END, 'DEMO'
  FROM @inv i JOIN ap.PurchaseInvoice v ON v.CompanyCode = i.Co AND v.RegisterNumber = i.Reg
 WHERE i.Po IS NOT NULL
   AND NOT EXISTS (SELECT 1 FROM ap.InvoiceMatch m WHERE m.PurchaseInvoiceId = v.PurchaseInvoiceId AND m.PurchaseInvoiceLineNum = 1);

/*  The open item behind every registered document.  ResidualAmount is set to the full value here
    and recomputed from the allocations at the end of the script. */
INSERT ap.OpenItem (CompanyCode, SupplierCode, ItemType, DocumentNumber, SupplierDocRef, DocumentDate, DueDate, PostingDate,
        DateReceived, FiscalYear, PeriodNo, CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount,
        ResidualAmountGBP, TermsCode, SettlementStatus, IsBlocked, BlockReason, IsDisputed, PurchaseInvoiceId, CreatedBy)
SELECT v.CompanyCode, v.SupplierCode, CASE WHEN v.DocumentType = 'INVOICE' THEN 'INVOICE' ELSE 'CREDIT' END,
       v.RegisterNumber, v.SupplierDocRef, v.DocumentDate, v.DueDate, v.PostingDate, v.DateReceived, v.FiscalYear,
       v.PeriodNo, v.CurrencyCode, v.FxRate,
       CASE WHEN v.DocumentType = 'INVOICE' THEN v.StatedGross ELSE -v.StatedGross END,
       CASE WHEN v.DocumentType = 'INVOICE' THEN v.GrossAmountGBP ELSE -v.GrossAmountGBP END,
       CASE WHEN v.DocumentType = 'INVOICE' THEN v.StatedGross ELSE -v.StatedGross END,
       CASE WHEN v.DocumentType = 'INVOICE' THEN v.GrossAmountGBP ELSE -v.GrossAmountGBP END,
       v.TermsCode, 'OPEN', i.Blocked, v.BlockReason, i.Disputed, v.PurchaseInvoiceId, 'DEMO'
  FROM ap.PurchaseInvoice v JOIN @inv i ON i.Co = v.CompanyCode AND i.Reg = v.RegisterNumber
 WHERE v.AccountingState IN ('POSTED', 'AWAITAPPROVAL')
   AND NOT EXISTS (SELECT 1 FROM ap.OpenItem o WHERE o.CompanyCode = v.CompanyCode
                     AND o.ItemType = CASE WHEN v.DocumentType = 'INVOICE' THEN 'INVOICE' ELSE 'CREDIT' END
                     AND o.DocumentNumber = v.RegisterNumber);

/*  A completed BACS payment run for each company: proposal, approval by a second pair of eyes,
    instructions, remittances and the resulting payment open items (AP-011 to AP-016). */
DECLARE @prop TABLE (Co varchar(10), Ref varchar(30), CutOff int, PayOff int, Bank varchar(20), Prep varchar(20), Appr varchar(20));
INSERT @prop VALUES
 ('STC','PP-26-0007',-33,-30,'GBP1','APCLERK','APMGR'),
 ('CAL','PP-26-2003',-13,-10,'GBP1','APCLERK','APMGR');

INSERT ap.PaymentProposal (CompanyCode, ProposalRef, Version, SelectionCutoffDate, PayDate, PaymentMethod, BankAccountCode,
        CurrencyCode, IsBacsProcessingDay, BacsInputDate, Status, PreparedBy, Approver1, ModifiedBy, ModifiedAt)
SELECT p.Co, p.Ref, 1, DATEADD(day, p.CutOff, @t), DATEADD(day, p.PayOff, @t), 'BACS', p.Bank, 'GBP', 1,
       DATEADD(day, p.PayOff - 3, @t), 'RELEASED', p.Prep, p.Appr, p.Appr, CAST(DATEADD(day, p.PayOff - 2, @t) AS datetime2(3))
  FROM @prop p
 WHERE NOT EXISTS (SELECT 1 FROM ap.PaymentProposal x WHERE x.CompanyCode = p.Co AND x.ProposalRef = p.Ref AND x.Version = 1);

/*  Proposal lines: the three settled invoices at Stellar, with the Avonmouth credit note applied
    against its own invoice, and the intercompany invoice at Caledon.  One excluded line shows why
    a blocked invoice never reaches the bank. */
DECLARE @pl TABLE (Co varchar(10), Ref varchar(30), LineNum int, Doc varchar(30), Excl bit, ExclReason varchar(200), Credit decimal(18,2));
INSERT @pl VALUES
 ('STC','PP-26-0007',1,'PI-26-0001',0,NULL,1440.00),
 ('STC','PP-26-0007',2,'PI-26-0002',0,NULL,0),
 ('STC','PP-26-0007',3,'PI-26-0003',0,NULL,0),
 ('STC','PP-26-0007',4,'PI-26-0005',1,'Payment blocked: quantity variance outside tolerance',0),
 ('CAL','PP-26-2003',1,'CPI-26-2001',0,NULL,0);

INSERT ap.PaymentProposalLine (ProposalId, LineNum, SupplierCode, OpenItemId, DocumentNumber, SupplierBankId, CurrencyCode,
        GrossAmount, GrossAmountGBP, CreditAppliedGBP, NetPayGBP, IsExcluded, ExclusionReason)
SELECT pp.ProposalId, l.LineNum, o.SupplierCode, o.OpenItemId, l.Doc, b.SupplierBankId, o.CurrencyCode, o.OriginalAmount,
       o.OriginalAmountGBP, l.Credit, o.OriginalAmountGBP - l.Credit, l.Excl, l.ExclReason
  FROM @pl l
  JOIN ap.PaymentProposal pp ON pp.CompanyCode = l.Co AND pp.ProposalRef = l.Ref AND pp.Version = 1
  JOIN ap.OpenItem o ON o.CompanyCode = l.Co AND o.ItemType = 'INVOICE' AND o.DocumentNumber = l.Doc
  OUTER APPLY (SELECT TOP (1) x.SupplierBankId FROM ap.SupplierBank x
                WHERE x.CompanyCode = o.CompanyCode AND x.SupplierCode = o.SupplierCode AND x.Status = 'APPROVED'
                ORDER BY x.SupplierBankId) b
 WHERE NOT EXISTS (SELECT 1 FROM ap.PaymentProposalLine z WHERE z.ProposalId = pp.ProposalId AND z.LineNum = l.LineNum);

UPDATE pp
   SET CashPrincipalGBP = t.Gross, CreditsAppliedGBP = t.Credit, TotalOutflowGBP = t.Net, ExcludedCount = t.Excl
  FROM ap.PaymentProposal pp
  JOIN (SELECT l.ProposalId, SUM(CASE WHEN l.IsExcluded = 0 THEN l.GrossAmountGBP ELSE 0 END) AS Gross,
               SUM(CASE WHEN l.IsExcluded = 0 THEN l.CreditAppliedGBP ELSE 0 END) AS Credit,
               SUM(CASE WHEN l.IsExcluded = 0 THEN l.NetPayGBP ELSE 0 END) AS Net,
               SUM(CASE WHEN l.IsExcluded = 1 THEN 1 ELSE 0 END) AS Excl
          FROM ap.PaymentProposalLine l GROUP BY l.ProposalId) t ON t.ProposalId = pp.ProposalId
 WHERE pp.TotalOutflowGBP <> t.Net OR pp.ExcludedCount <> t.Excl;

INSERT ap.PaymentInstruction (CompanyCode, InstructionRef, ProposalId, ProposalVersion, SupplierCode, PaymentMethod,
        BankAccountCode, CurrencyCode, Amount, AmountGBP, BeneficiaryName, BeneficiarySortCode, BeneficiaryAccountNumber,
        SupplierBankId, CopResultSnapshot, ValueDate, SubmittedAt, SchemeReference, BacsFileRef, Status, SettledAt,
        SettledAmountGBP, RemittanceRef, RemittanceIssuedAt, ModifiedBy, ModifiedAt)
SELECT pp.CompanyCode, 'PY-' + RIGHT(pp.ProposalRef, 6) + '-' + RIGHT('00' + CAST(l.LineNum AS varchar(2)), 2),
       pp.ProposalId, 1, l.SupplierCode, 'BACS', pp.BankAccountCode, 'GBP', l.NetPayGBP, l.NetPayGBP, s.LegalName,
       b.SortCode, b.AccountNumber, l.SupplierBankId, b.CopResult, pp.PayDate,
       CAST(DATEADD(day, -3, pp.PayDate) AS datetime2(3)), 'BACS/' + pp.ProposalRef,
       'BACS18-' + REPLACE(CONVERT(varchar(10), pp.PayDate, 120), '-', '') + '-' + pp.CompanyCode, 'SETTLED',
       CAST(pp.PayDate AS datetime2(3)), l.NetPayGBP,
       'RM-' + RIGHT(pp.ProposalRef, 6) + '-' + RIGHT('00' + CAST(l.LineNum AS varchar(2)), 2),
       CAST(pp.PayDate AS datetime2(3)), 'APMGR', CAST(pp.PayDate AS datetime2(3))
  FROM ap.PaymentProposalLine l
  JOIN ap.PaymentProposal pp ON pp.ProposalId = l.ProposalId
  JOIN ap.Supplier s ON s.CompanyCode = pp.CompanyCode AND s.SupplierCode = l.SupplierCode
  LEFT JOIN ap.SupplierBank b ON b.SupplierBankId = l.SupplierBankId
 WHERE l.IsExcluded = 0
   AND NOT EXISTS (SELECT 1 FROM ap.PaymentInstruction x WHERE x.CompanyCode = pp.CompanyCode
                     AND x.InstructionRef = 'PY-' + RIGHT(pp.ProposalRef, 6) + '-' + RIGHT('00' + CAST(l.LineNum AS varchar(2)), 2));

INSERT ap.OpenItem (CompanyCode, SupplierCode, ItemType, DocumentNumber, SupplierDocRef, DocumentDate, DueDate, PostingDate,
        FiscalYear, PeriodNo, CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP,
        SettlementStatus, PaymentInstructionId, CreatedBy)
SELECT pi.CompanyCode, pi.SupplierCode, 'PAYMENT', pi.InstructionRef, pi.RemittanceRef, pi.ValueDate, pi.ValueDate,
       pi.ValueDate, p.FiscalYear, p.PeriodNo, 'GBP', 1, -pi.AmountGBP, -pi.AmountGBP, -pi.AmountGBP, -pi.AmountGBP,
       'OPEN', pi.PaymentInstructionId, 'DEMO'
  FROM ap.PaymentInstruction pi
  OUTER APPLY (SELECT TOP (1) x.FiscalYear, x.PeriodNo FROM fin.Period x
                WHERE x.CompanyCode = pi.CompanyCode AND pi.ValueDate BETWEEN x.StartDate AND x.EndDate) p
 WHERE NOT EXISTS (SELECT 1 FROM ap.OpenItem o WHERE o.CompanyCode = pi.CompanyCode AND o.ItemType = 'PAYMENT'
                     AND o.DocumentNumber = pi.InstructionRef);

/*  Allocations: each payment against the invoice it settled, and the Avonmouth credit note
    against its own invoice (AP-014, AP-017). */
INSERT ap.Allocation (CompanyCode, SupplierCode, SourceType, SourceOpenItemId, TargetOpenItemId, PaymentInstructionId,
        RemittanceRef, EffectiveDate, CurrencyCode, AppliedAmount, AppliedAmountGBP, Status, CreatedBy)
SELECT pp.CompanyCode, l.SupplierCode, 'PAYMENT', src.OpenItemId, tgt.OpenItemId, pi.PaymentInstructionId, pi.RemittanceRef,
       pi.ValueDate, 'GBP', l.NetPayGBP, l.NetPayGBP, 'ACTIVE', 'DEMO'
  FROM ap.PaymentProposalLine l
  JOIN ap.PaymentProposal pp ON pp.ProposalId = l.ProposalId
  JOIN ap.PaymentInstruction pi ON pi.ProposalId = pp.ProposalId AND pi.SupplierCode = l.SupplierCode
       AND pi.AmountGBP = l.NetPayGBP
  JOIN ap.OpenItem src ON src.CompanyCode = pp.CompanyCode AND src.ItemType = 'PAYMENT' AND src.DocumentNumber = pi.InstructionRef
  JOIN ap.OpenItem tgt ON tgt.CompanyCode = pp.CompanyCode AND tgt.ItemType = 'INVOICE' AND tgt.DocumentNumber = l.DocumentNumber
 WHERE l.IsExcluded = 0
   AND NOT EXISTS (SELECT 1 FROM ap.Allocation a WHERE a.SourceOpenItemId = src.OpenItemId AND a.TargetOpenItemId = tgt.OpenItemId);

INSERT ap.Allocation (CompanyCode, SupplierCode, SourceType, SourceOpenItemId, TargetOpenItemId, EffectiveDate,
        CurrencyCode, AppliedAmount, AppliedAmountGBP, Status, CreatedBy)
SELECT 'STC', 'S0001', 'CREDIT', src.OpenItemId, tgt.OpenItemId, DATEADD(day, -30, @t), 'GBP', 1440.00, 1440.00, 'ACTIVE', 'DEMO'
  FROM ap.OpenItem src
  JOIN ap.OpenItem tgt ON tgt.CompanyCode = 'STC' AND tgt.ItemType = 'INVOICE' AND tgt.DocumentNumber = 'PI-26-0001'
 WHERE src.CompanyCode = 'STC' AND src.ItemType = 'CREDIT' AND src.DocumentNumber = 'PC-26-0001'
   AND NOT EXISTS (SELECT 1 FROM ap.Allocation a WHERE a.SourceOpenItemId = src.OpenItemId AND a.TargetOpenItemId = tgt.OpenItemId);

/*  Residuals and settlement states recomputed from the allocations. */
UPDATE o
   SET ResidualAmount = o.OriginalAmount - x.Applied, ResidualAmountGBP = o.OriginalAmountGBP - x.Applied,
       SettlementStatus = CASE WHEN ABS(o.OriginalAmountGBP - x.Applied) < 0.005 THEN 'SETTLED'
                               WHEN x.Applied <> 0 THEN 'PARTIAL' ELSE 'OPEN' END,
       ModifiedBy = 'DEMO', ModifiedAt = SYSDATETIME()
  FROM ap.OpenItem o
  CROSS APPLY (SELECT ISNULL(SUM(a.AppliedAmountGBP), 0) AS Applied FROM ap.Allocation a
                WHERE a.Status = 'ACTIVE' AND a.TargetOpenItemId = o.OpenItemId) x
 WHERE o.CreatedBy = 'DEMO' AND o.ItemType IN ('INVOICE')
   AND (o.ResidualAmountGBP <> o.OriginalAmountGBP - x.Applied);

UPDATE o
   SET ResidualAmount = o.OriginalAmount + x.Applied, ResidualAmountGBP = o.OriginalAmountGBP + x.Applied,
       SettlementStatus = CASE WHEN ABS(o.OriginalAmountGBP + x.Applied) < 0.005 THEN 'SETTLED'
                               WHEN x.Applied <> 0 THEN 'PARTIAL' ELSE 'OPEN' END,
       ModifiedBy = 'DEMO', ModifiedAt = SYSDATETIME()
  FROM ap.OpenItem o
  CROSS APPLY (SELECT ISNULL(SUM(a.AppliedAmountGBP), 0) AS Applied FROM ap.Allocation a
                WHERE a.Status = 'ACTIVE' AND a.SourceOpenItemId = o.OpenItemId) x
 WHERE o.CreatedBy = 'DEMO' AND o.ItemType IN ('CREDIT', 'PAYMENT')
   AND (o.ResidualAmountGBP <> o.OriginalAmountGBP + x.Applied);

UPDATE v
   SET SettlementState = CASE WHEN o.SettlementStatus = 'SETTLED' THEN 'SETTLED'
                              WHEN o.SettlementStatus = 'PARTIAL' THEN 'PARTPAID' ELSE 'UNPAID' END,
       ModifiedBy = 'DEMO', ModifiedAt = SYSDATETIME()
  FROM ap.PurchaseInvoice v
  JOIN ap.OpenItem o ON o.PurchaseInvoiceId = v.PurchaseInvoiceId AND o.ItemType IN ('INVOICE', 'CREDIT')
 WHERE v.CreatedBy = 'DEMO'
   AND v.SettlementState <> CASE WHEN o.SettlementStatus = 'SETTLED' THEN 'SETTLED'
                                 WHEN o.SettlementStatus = 'PARTIAL' THEN 'PARTPAID' ELSE 'UNPAID' END;

/*  Bank detail changes.  Wessex Logistics has produced new details that passed Confirmation of
    Payee and a recorded call back, so the record is VERIFIED but still has no approver - the
    second pair of eyes must not be the preparer (AP-002, AP-027, D-19).  Midland Surface
    Finishing failed Confirmation of Payee outright and stays PENDING. */
INSERT ap.SupplierBank (CompanyCode, SupplierCode, BankRef, BeneficiaryName, CountryCode, CurrencyCode, SortCode,
        AccountNumber, BankName, PaymentMethod, CopResult, CopCheckedAt, CopReference, CopDecision, CallBackVerifiedAt,
        CallBackVerifiedBy, CallBackContactSource, CallBackNote, ValidFrom, Status, PreparedBy, IsSensitive, CreatedBy)
SELECT v.Co, v.Supp, 'BANK2', s.LegalName, 'GB', 'GBP', v.Sort, v.Acct, v.BankName, 'BACS', v.Cop,
       CAST(DATEADD(day, -4, @t) AS datetime2(3)), v.CopRef, v.CopNote,
       CASE WHEN v.Cop = 'MATCH' THEN CAST(DATEADD(day, -3, @t) AS datetime2(3)) END,
       CASE WHEN v.Cop = 'MATCH' THEN 'APCLERK' END,
       CASE WHEN v.Cop = 'MATCH' THEN 'Switchboard number from the signed supply agreement, not the letterhead' END,
       v.CallNote, DATEADD(day, -4, @t), v.Status, 'APCLERK', 1, 'DEMO'
  FROM (VALUES
   ('STC','S0013','404711','81223094','Thameside Bank plc','MATCH','COP-2026-441180',
    'Name and account type agreed exactly with the Confirmation of Payee response',
    'Call back made to the number held on the signed agreement; change confirmed by the finance director','VERIFIED'),
   ('STC','S0007','560022','40118877','Northern Counties Bank plc','NOMATCH','COP-2026-441207',
    'Confirmation of Payee returned no match: the account is held in the name of a different business',
    'Change request arrived by email only; held pending a call back to a verified number','PENDING')
  ) AS v (Co, Supp, Sort, Acct, BankName, Cop, CopRef, CopNote, CallNote, Status)
  JOIN ap.Supplier s ON s.CompanyCode = v.Co AND s.SupplierCode = v.Supp
 WHERE NOT EXISTS (SELECT 1 FROM ap.SupplierBank b WHERE b.CompanyCode = v.Co AND b.SupplierCode = v.Supp AND b.BankRef = 'BANK2');

/*  Statutory late payment claims from two small suppliers (AP-030, D-25). */
INSERT ap.LatePaymentClaim (CompanyCode, ClaimRef, SupplierCode, PurchaseInvoiceId, OpenItemId, DocumentNumber,
        SupplierDocRef, ClaimDate, DueDate, PaidDate, DaysLate, DebtAmountGBP, BaseRatePct, MarginPct, StatutoryRatePct,
        InterestGBP, CompensationGBP, ClaimedTotalGBP, IsDisputed, DisputeReason, Status, FinanceCostAccount, Note, CreatedBy)
SELECT v.Co, v.Ref, o.SupplierCode, o.PurchaseInvoiceId, o.OpenItemId, o.DocumentNumber, o.SupplierDocRef,
       DATEADD(day, v.ClaimOff, @t), o.DueDate, CASE WHEN v.Paid = 1 THEN DATEADD(day, v.PaidOff, @t) END, v.Days,
       v.Debt, 4.7500, 8.0000, 12.7500, ROUND(v.Debt * 12.75 / 100 * v.Days / 365.0, 2), v.Comp,
       ROUND(v.Debt * 12.75 / 100 * v.Days / 365.0, 2) + v.Comp, v.Disp, v.DispReason, v.Status, '7620', v.Note, 'DEMO'
  FROM (VALUES
   ('STC','LPC-26-0001','S0003','PI-26-0003',-28,1,-40,10,6480.00,70.00,0,NULL,'AGREED',
    'Claim agreed in full and settled with the next BACS run'),
   ('STC','LPC-26-0002','S0021','PI-26-0010', -2,0,  0,24,1740.00,40.00,1,
    'We hold a signed variation extending the terms to 45 days; the claim is being discussed','CLAIMED',
    'Claim received by email; terms evidence requested from the supplier')
  ) AS v (Co, Ref, Supp, Doc, ClaimOff, Paid, PaidOff, Days, Debt, Comp, Disp, DispReason, Status, Note)
  JOIN ap.OpenItem o ON o.CompanyCode = v.Co AND o.ItemType = 'INVOICE' AND o.DocumentNumber = v.Doc
 WHERE NOT EXISTS (SELECT 1 FROM ap.LatePaymentClaim c WHERE c.CompanyCode = v.Co AND c.ClaimRef = v.Ref);

/*  Supplier statements for reconciliation (AP-019): one agrees, one has a missing invoice and a
    timing difference. */
INSERT ap.SupplierStatement (CompanyCode, SupplierCode, StatementRef, StatementDate, CurrencyCode, StatementBalance,
        LedgerBalance, DifferenceAmount, Status, ReconciledBy, ReconciledAt, Note, CreatedBy)
SELECT v.Co, v.Supp, v.Ref, DATEADD(day, -5, @t), 'GBP', v.Bal, x.Ledger, v.Bal - x.Ledger, v.Status,
       CASE WHEN v.Status = 'RECONCILED' THEN 'APCLERK' END,
       CASE WHEN v.Status = 'RECONCILED' THEN CAST(DATEADD(day, -4, @t) AS datetime2(3)) END, v.Note, 'DEMO'
  FROM (VALUES
   ('STC','S0013','WLG-STMT-2609',4920.00,'RECONCILED','Statement agrees with the ledger'),
   ('STC','S0006','CF-STMT-2609',4440.00,'DISPUTED','Supplier shows an invoice we have never received and a cheque in transit')
  ) AS v (Co, Supp, Ref, Bal, Status, Note)
  CROSS APPLY (SELECT ISNULL(SUM(o.ResidualAmountGBP), 0) AS Ledger FROM ap.OpenItem o
                WHERE o.CompanyCode = v.Co AND o.SupplierCode = v.Supp AND o.SettlementStatus <> 'SETTLED') x
 WHERE NOT EXISTS (SELECT 1 FROM ap.SupplierStatement s WHERE s.CompanyCode = v.Co AND s.SupplierCode = v.Supp AND s.StatementRef = v.Ref);

INSERT ap.SupplierStatementLine (StatementId, LineNum, SupplierDocRef, DocumentDate, DueDate, Amount, MatchStatus,
        MatchedOpenItemId, Note)
SELECT s.StatementId, v.LineNum, v.Ref, DATEADD(day, v.DocOff, @t), DATEADD(day, v.DueOff, @t), v.Amount, v.Status,
       o.OpenItemId, v.Note
  FROM (VALUES
   ('WLG-STMT-2609',1,'WLG-330118',-35, -5,4920.00,'MATCHED',NULL),
   ('CF-STMT-2609',1,'CF-11903',-48,-18,2220.00,'DISPUTED','Quantity variance under query'),
   ('CF-STMT-2609',2,'CF-12044',-18, 12,2220.00,'MISSINGOURS','Invoice never received; copy requested')
  ) AS v (Stmt, LineNum, Ref, DocOff, DueOff, Amount, Status, Note)
  JOIN ap.SupplierStatement s ON s.StatementRef = v.Stmt
  LEFT JOIN ap.OpenItem o ON o.CompanyCode = s.CompanyCode AND o.SupplierDocRef = v.Ref AND o.ItemType = 'INVOICE'
 WHERE NOT EXISTS (SELECT 1 FROM ap.SupplierStatementLine l WHERE l.StatementId = s.StatementId AND l.LineNum = v.LineNum);
GO
/*  General ledger side.  One journal per company for the registered invoices, one for the credit
    notes and one for the payment run, so that account 2000 agrees with the purchase ledger.  The
    correlation id is deterministic, so a second run of this script returns the original journal
    rather than posting again (COM-015). */
DECLARE @t date = CAST(GETDATE() AS date), @co varchar(10), @corr varchar(64), @n int;
DECLARE @open bit = CASE WHEN EXISTS (SELECT 1 FROM fin.Period WHERE @t BETWEEN StartDate AND EndDate AND Status = 'OPEN') THEN 1 ELSE 0 END;
DECLARE c CURSOR LOCAL FAST_FORWARD FOR SELECT DISTINCT CompanyCode FROM ap.PurchaseInvoice WHERE CreatedBy = 'DEMO';
OPEN c; FETCH NEXT FROM c INTO @co;
WHILE @@FETCH_STATUS = 0
BEGIN
    IF @open = 1 AND EXISTS (SELECT 1 FROM fin.Period WHERE CompanyCode = @co AND @t BETWEEN StartDate AND EndDate AND Status = 'OPEN')
    BEGIN
        /*  Invoices: expense and irrecoverable VAT to the profit and loss, recoverable input tax
            to 1250, the gross to the creditors control. */
        SET @corr = 'APDEMO-INV-' + @co;
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount,
                    CurrencyCode, DebitAmountGBP, CreditAmountGBP, DimCostCentre, Narrative, SubledgerType, SubledgerRef, CreatedBy)
            SELECT @corr, x.LineNum, @co, x.AccountCode, x.Dr, x.Cr, 'GBP', x.Dr, x.Cr, x.CC, x.Narrative, 'AP', @co, 'DEMO'
              FROM (SELECT 1 AS LineNum, '6800' AS AccountCode,
                           SUM(v.NetAmountGBP + v.NonRecoverableVatGBP) AS Dr, 0 AS Cr, 'ADMIN' AS CC,
                           'Demonstration purchase invoices, net cost' AS Narrative
                      FROM ap.PurchaseInvoice v WHERE v.CompanyCode = @co AND v.CreatedBy = 'DEMO' AND v.DocumentType = 'INVOICE'
                    UNION ALL
                    SELECT 2, '1250', SUM(v.RecoverableVatGBP), 0, NULL, 'Demonstration purchase invoices, recoverable input VAT'
                      FROM ap.PurchaseInvoice v WHERE v.CompanyCode = @co AND v.CreatedBy = 'DEMO' AND v.DocumentType = 'INVOICE'
                    UNION ALL
                    SELECT 3, '2000', 0, SUM(v.GrossAmountGBP), NULL, 'Demonstration purchase invoices, trade creditors'
                      FROM ap.PurchaseInvoice v WHERE v.CompanyCode = @co AND v.CreatedBy = 'DEMO' AND v.DocumentType = 'INVOICE') x
             WHERE x.Dr <> 0 OR x.Cr <> 0;
            SELECT @n = COUNT(*) FROM fin.JournalStage WHERE CorrelationId = @corr;
            IF @n > 0 EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'APMGR',
                @Description = 'Purchase ledger demonstration invoices', @PostingDate = @t, @DocumentDate = @t,
                @SourceModule = 'AP', @SourceRef = 'APDEMO', @EventCode = 'AP.INVOICE', @SeriesCode = 'JNL',
                @SuppressResult = 1;
        END
        /*  Credit notes, the mirror image of the invoice journal. */
        SET @corr = 'APDEMO-CRN-' + @co;
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount,
                    CurrencyCode, DebitAmountGBP, CreditAmountGBP, DimCostCentre, Narrative, SubledgerType, SubledgerRef, CreatedBy)
            SELECT @corr, x.LineNum, @co, x.AccountCode, x.Dr, x.Cr, 'GBP', x.Dr, x.Cr, x.CC, x.Narrative, 'AP', @co, 'DEMO'
              FROM (SELECT 1 AS LineNum, '2000' AS AccountCode, SUM(v.GrossAmountGBP) AS Dr, 0 AS Cr, NULL AS CC,
                           'Demonstration supplier credit notes, trade creditors' AS Narrative
                      FROM ap.PurchaseInvoice v WHERE v.CompanyCode = @co AND v.CreatedBy = 'DEMO' AND v.DocumentType = 'CREDITNOTE'
                    UNION ALL
                    SELECT 2, '6800', 0, SUM(v.NetAmountGBP + v.NonRecoverableVatGBP), 'ADMIN', 'Demonstration supplier credit notes, net cost'
                      FROM ap.PurchaseInvoice v WHERE v.CompanyCode = @co AND v.CreatedBy = 'DEMO' AND v.DocumentType = 'CREDITNOTE'
                    UNION ALL
                    SELECT 3, '1250', 0, SUM(v.RecoverableVatGBP), NULL, 'Demonstration supplier credit notes, input VAT reversed'
                      FROM ap.PurchaseInvoice v WHERE v.CompanyCode = @co AND v.CreatedBy = 'DEMO' AND v.DocumentType = 'CREDITNOTE') x
             WHERE x.Dr <> 0 OR x.Cr <> 0;
            SELECT @n = COUNT(*) FROM fin.JournalStage WHERE CorrelationId = @corr;
            IF @n > 0 EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'APMGR',
                @Description = 'Purchase ledger demonstration credit notes', @PostingDate = @t, @DocumentDate = @t,
                @SourceModule = 'AP', @SourceRef = 'APDEMO', @EventCode = 'AP.CREDITNOTE', @SeriesCode = 'JNL',
                @SuppressResult = 1;
        END
        /*  The settled BACS run: creditors control debited, bank credited. */
        SET @corr = 'APDEMO-PAY-' + @co;
        IF NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @corr AND Status = 'COMPLETED')
        BEGIN
            DELETE fin.JournalStage WHERE CorrelationId = @corr;
            INSERT fin.JournalStage (CorrelationId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount,
                    CurrencyCode, DebitAmountGBP, CreditAmountGBP, Narrative, SubledgerType, SubledgerRef, BankAccountCode, CreatedBy)
            SELECT @corr, x.LineNum, @co, x.AccountCode, x.Dr, x.Cr, 'GBP', x.Dr, x.Cr, x.Narrative, 'AP', @co, x.Bank, 'DEMO'
              FROM (SELECT 1 AS LineNum, '2000' AS AccountCode, SUM(p.AmountGBP) AS Dr, 0 AS Cr,
                           'Demonstration BACS payment run, trade creditors' AS Narrative, NULL AS Bank
                      FROM ap.PaymentInstruction p WHERE p.CompanyCode = @co AND p.BacsFileRef LIKE 'BACS18-%'
                    UNION ALL
                    SELECT 2, '1300', 0, SUM(p.AmountGBP), 'Demonstration BACS payment run, bank', 'GBP1'
                      FROM ap.PaymentInstruction p WHERE p.CompanyCode = @co AND p.BacsFileRef LIKE 'BACS18-%') x
             WHERE x.Dr <> 0 OR x.Cr <> 0;
            SELECT @n = COUNT(*) FROM fin.JournalStage WHERE CorrelationId = @corr;
            IF @n > 0 EXEC fin.usp_PostJournal @CompanyCode = @co, @CorrelationId = @corr, @UserCode = 'APMGR',
                @Description = 'Purchase ledger demonstration payment run', @PostingDate = @t, @DocumentDate = @t,
                @SourceModule = 'AP', @SourceRef = 'APDEMO', @EventCode = 'AP.PAYMENT', @SeriesCode = 'JNL',
                @SuppressResult = 1;
        END
    END
    FETCH NEXT FROM c INTO @co;
END
CLOSE c; DEALLOCATE c;
GO
/*  Link the journals back to the documents they came from, so the drill down works. */
UPDATE v SET JournalId = j.JournalId
  FROM ap.PurchaseInvoice v
  JOIN fin.Journal j ON j.CompanyCode = v.CompanyCode AND j.SourceRef = 'APDEMO'
       AND j.EventCode = CASE WHEN v.DocumentType = 'INVOICE' THEN 'AP.INVOICE' ELSE 'AP.CREDITNOTE' END
 WHERE v.CreatedBy = 'DEMO' AND v.JournalId IS NULL;
GO
