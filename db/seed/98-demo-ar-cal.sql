/*  STERP - Stefan's ERP - demonstration data, Caledonian Assemblies credit control  (db\seed\98-demo-ar-cal.sql)

    The sales ledger demonstration (91-ar-demo.sql) gives Stellar Components a disputed invoice,
    a live promise to pay and an escalated case, but Caledonian Assemblies only has a single
    escalated collection case.  The credit-control worklist, the dunning suppression rule
    (a disputed item is not chased) and the promise-to-pay diary therefore could not be shown
    for the second company.

    This file adds one dispute and one open promise to pay for Caledonian.  It is seeded here
    rather than in the sales ledger demonstration so that the two files can be maintained by
    different teams; the rows are guarded by NOT EXISTS and the script is re-runnable.
    REQ: AR-016, AR-017, AR-018
*/
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'ar.CollectionCase') IS NULL
    PRINT 'ar.CollectionCase is not deployed yet - Caledonian credit control demonstration skipped.';
GO
IF OBJECT_ID(N'ar.CollectionCase') IS NOT NULL
AND EXISTS (SELECT 1 FROM core.Company WHERE CompanyCode = 'CAL')
BEGIN
    DECLARE @t date = CAST(GETDATE() AS date);

    DECLARE @cc TABLE (Co varchar(10), CaseRef varchar(30), Cust varchar(20), CaseType varchar(12), Doc varchar(30),
                       ReasonCode varchar(20), Reason varchar(300), Disputed decimal(18,2), Owner varchar(20),
                       Step tinyint, NextOff int, NextAction varchar(200), Status varchar(12), Escalated varchar(40));
    INSERT @cc VALUES
     ('CAL','CASE-26-2002','K0003','DISPUTE','CSI-26-2003','QUALITY',
      'Customer disputes 1,250.00 of the commuter supply contract as short delivered; proof of delivery requested from the carrier',
      1250.00,'CREDMGR',2,4,'Carrier proof of delivery expected; chase only the undisputed balance','OPEN',NULL),
     ('CAL','CASE-26-2003','K0002','COLLECTION','CSI-26-2002','CASHFLOW',
      'Trail bike assemblies invoice 10 days past due; customer waiting on its own end-customer settlement',
      0,'ARCLERK',2,6,'Promise to pay in full on the agreed date','OPEN',NULL);

    INSERT ar.CollectionCase (CompanyCode, CustomerCode, CaseRef, CaseType, OpenItemId, DocumentNumber, ReasonCode, Reason,
            DisputedAmountGBP, OwnerUser, CollectionStep, NextActionDate, NextAction, EscalatedTo, Status, OpenedAt, CreatedBy)
    SELECT k.Co, k.Cust, k.CaseRef, k.CaseType, o.OpenItemId, k.Doc, k.ReasonCode, k.Reason, k.Disputed, k.Owner, k.Step,
           DATEADD(day, k.NextOff, @t), k.NextAction, k.Escalated, k.Status, CAST(DATEADD(day, -18, @t) AS datetime2(3)), 'DEMO'
      FROM @cc k
      LEFT JOIN ar.OpenItem o ON o.CompanyCode = k.Co AND o.DocumentNumber = k.Doc AND o.ItemType = 'INVOICE'
     WHERE EXISTS (SELECT 1 FROM ar.Customer c WHERE c.CompanyCode = k.Co AND c.CustomerCode = k.Cust)
       AND NOT EXISTS (SELECT 1 FROM ar.CollectionCase x WHERE x.CompanyCode = k.Co AND x.CaseRef = k.CaseRef);

    DECLARE @ca TABLE (CaseRef varchar(30), Seq int, ActionType varchar(14), DayOff int, ByUser varchar(20),
                       Contact varchar(80), Narrative varchar(400), PromiseAmt decimal(18,2), PromiseOff int,
                       PromiseStatus varchar(10));
    INSERT @ca VALUES
     ('CASE-26-2002',1,'REMINDER1',-16,'ARCLERK','Accounts Payable','Courteous reminder issued showing the full balance and payment details',NULL,NULL,NULL),
     ('CASE-26-2002',2,'CALL',     -12,'CREDMGR','L Mackay','Customer raised a short delivery claim of 1,250.00; the balance of the invoice is agreed as collectable',NULL,NULL,NULL),
     ('CASE-26-2002',3,'NOTE',      -4,'CREDMGR','L Mackay','Carrier investigation opened; dunning suppressed on the disputed item only',NULL,NULL,NULL),
     ('CASE-26-2003',1,'REMINDER1',-11,'ARCLERK','Accounts Payable','Courteous reminder issued',NULL,NULL,NULL),
     ('CASE-26-2003',2,'PROMISE',   -3,'ARCLERK','D Shearer','Customer promised payment in full of the outstanding 3,600.00 by bank transfer',3600.00,6,'OPEN');

    INSERT ar.CollectionAction (CaseId, ActionSeq, ActionType, ActionDate, PerformedBy, ContactName, Narrative,
            PromiseAmountGBP, PromiseDate, PromiseStatus)
    SELECT k.CaseId, a.Seq, a.ActionType, DATEADD(day, a.DayOff, @t), a.ByUser, a.Contact, a.Narrative, a.PromiseAmt,
           CASE WHEN a.PromiseOff IS NOT NULL THEN DATEADD(day, a.PromiseOff, @t) END, a.PromiseStatus
      FROM @ca a
      JOIN ar.CollectionCase k ON k.CaseRef = a.CaseRef AND k.CompanyCode = 'CAL'
     WHERE NOT EXISTS (SELECT 1 FROM ar.CollectionAction x WHERE x.CaseId = k.CaseId AND x.ActionSeq = a.Seq);
END
GO
