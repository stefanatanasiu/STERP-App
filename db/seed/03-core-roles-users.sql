/*  Roles, role grants, business users, scoped role assignments, segregation-of-duties rules,
    the D-07 approval matrix and a sample named delegation.  The Windows login of the installing
    user is mapped to SYSADM so the desktop signs in without extra setup.
    REQ: COM-002, COM-003, COM-008, COM-019, D-06, D-07 */
SET NOCOUNT ON;
GO
MERGE core.Role AS t
USING (VALUES
 ('SYSADM','System administrator',0),('DIRECTOR','Director',1),('FINCTRL','Financial controller',1),
 ('GLCLERK','General ledger clerk',0),('ARCLERK','Sales ledger clerk',0),('CREDMGR','Credit manager',1),
 ('APCLERK','Purchase ledger clerk',0),('APMGR','Accounts payable manager',1),('TREASURY','Treasury and payment authoriser',1),
 ('BUYER','Buyer',0),('PURMGR','Purchasing manager',1),('SALESREP','Sales representative',0),
 ('SALESMGR','Sales manager',1),('WHSE','Warehouse operator',0),('INVMGR','Inventory manager',1),
 ('QUALITY','Quality manager',1),('ENGINEER','Design engineer',0),('ENGMGR','Engineering manager',1),
 ('COSTACC','Cost accountant',0),('VATOWNER','VAT and tax owner',0),('AUDITOR','Auditor (read only)',0),
 ('RESTRICT','Restricted site user',0)
) AS s (RoleCode, Name, IsApprovalRole)
ON t.RoleCode = s.RoleCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.IsApprovalRole EXCEPT SELECT t.Name, t.IsApprovalRole)
    THEN UPDATE SET Name = s.Name, IsApprovalRole = s.IsApprovalRole, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (RoleCode, Name, IsApprovalRole, CreatedBy) VALUES (s.RoleCode, s.Name, s.IsApprovalRole, 'SEED');
GO
/*  Role grants.  SYSADM holds the whole catalogue, which is also why SYSADM trips the
    segregation-of-duties rules below: approvals are made by the business role, not by the
    administrator.  AUDITOR is read only. */
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT 'SYSADM', p.PermissionCode, 'SEED' FROM core.Permission p
 WHERE NOT EXISTS (SELECT 1 FROM core.RolePermission r WHERE r.RoleCode = 'SYSADM' AND r.PermissionCode = p.PermissionCode);
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT 'AUDITOR', p.PermissionCode, 'SEED' FROM core.Permission p
 WHERE (p.PermissionCode LIKE '%.VIEW' OR p.PermissionCode IN ('RPT.RUN','RPT.FINANCIAL.RUN','RPT.RECON.RUN'))
   AND p.PermissionCode NOT IN ('SYS.BANKDETAIL.VIEW','AP.BANK.VIEW')
   AND NOT EXISTS (SELECT 1 FROM core.RolePermission r WHERE r.RoleCode = 'AUDITOR' AND r.PermissionCode = p.PermissionCode);
GO
DECLARE @g TABLE (RoleCode varchar(20), Perms varchar(2000));
INSERT @g VALUES
 ('DIRECTOR','SYS.APPROVE.T4,SYS.APPROVAL.VIEW,SYS.OBLIGATION.VIEW,AP.PRACTICES.APPROVE,FIN.STATUTORY.VIEW,FIN.ENQUIRY.VIEW,AR.ENQUIRY.VIEW,AP.ENQUIRY.VIEW,SO.ENQUIRY.VIEW,PO.ENQUIRY.VIEW,CST.COST.VIEW,RPT.RUN,RPT.FINANCIAL.RUN,RPT.KPI.VIEW,RPT.RECON.RUN'),
 ('FINCTRL','FIN.APPROVE.T3,FIN.ACCOUNT.VIEW,FIN.ACCOUNT.MAINT,FIN.DIM.MAINT,FIN.JOURNAL.APPROVE,FIN.JOURNAL.POST,FIN.JOURNAL.REVERSE,FIN.PERIOD.VIEW,FIN.PERIOD.CLOSE,FIN.PERIOD.REOPEN,FIN.PERIOD.SOFTPOST,FIN.YEAREND.RUN,FIN.BUDGET.MAINT,FIN.BUDGET.APPROVE,FIN.BANK.MAINT,FIN.BANK.RECONCILE,FIN.RECURRING.MAINT,FIN.REVAL.RUN,FIN.VAT.APPROVE,FIN.STATUTORY.VIEW,FIN.ENQUIRY.VIEW,CST.STANDARD.ACTIVATE,CST.COST.VIEW,CST.WIP.VIEW,CST.VARIANCE.VIEW,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,SYS.EXCEPTION.RESOLVE,SYS.LEGALHOLD.APPLY,SYS.OBLIGATION.VIEW,SYS.BANKDETAIL.VIEW,SYS.AUDIT.VIEW,AR.ENQUIRY.VIEW,AR.CREDIT.VIEW,AP.ENQUIRY.VIEW,SO.ENQUIRY.VIEW,PO.ENQUIRY.VIEW,ITM.STOCK.VIEW,RPT.RUN,RPT.FINANCIAL.RUN,RPT.KPI.VIEW,RPT.RECON.RUN,RPT.EXPORT.DATA'),
 ('GLCLERK','FIN.JOURNAL.ENTER,FIN.JOURNAL.POST,FIN.JOURNAL.REVERSE,FIN.ACCOUNT.VIEW,FIN.PERIOD.VIEW,FIN.ENQUIRY.VIEW,FIN.RECURRING.MAINT,FIN.BANK.RECONCILE,FIN.REVAL.RUN,SYS.EXCEPTION.VIEW,SYS.APPROVAL.VIEW,RPT.RUN,RPT.FINANCIAL.RUN'),
 ('ARCLERK','AR.CUSTOMER.VIEW,AR.CREDIT.VIEW,AR.INVOICE.ENTER,AR.INVOICE.POST,AR.CREDITNOTE.ENTER,AR.RECEIPT.ENTER,AR.RECEIPT.POST,AR.ALLOCATE.RUN,AR.WRITEOFF.POST,AR.STATEMENT.RUN,AR.DD.COLLECT.RUN,AR.BADDEBT.CLAIM,AR.LATEPAY.CLAIM,AR.ENQUIRY.VIEW,FIN.ENQUIRY.VIEW,SYS.EXCEPTION.VIEW,RPT.RUN'),
 ('CREDMGR','AR.CUSTOMER.VIEW,AR.CUSTOMER.MAINT,AR.CUSTOMER.APPROVE,AR.CREDIT.VIEW,AR.CREDIT.SET,AR.CREDIT.SET.HIGH,AR.CREDIT.OVERRIDE,AR.HOLD.SET,AR.HOLD.RELEASE,AR.WRITEOFF.APPROVE,AR.DUNNING.RUN,AR.LATEPAY.APPROVE,AR.DD.MANDATE.MAINT,AR.DD.COLLECT.APPROVE,AR.ENQUIRY.VIEW,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,SYS.EXCEPTION.RESOLVE,RPT.RUN,RPT.KPI.VIEW'),
 ('APCLERK','AP.SUPPLIER.VIEW,AP.INVOICE.ENTER,AP.INVOICE.MATCH,AP.INVOICE.POST,AP.CREDITNOTE.ENTER,AP.PAYMENT.PROPOSE,AP.STATEMENT.RECONCILE,AP.PRACTICES.REPORT,AP.ENQUIRY.VIEW,PO.ENQUIRY.VIEW,FIN.ENQUIRY.VIEW,SYS.EXCEPTION.VIEW,RPT.RUN'),
 ('APMGR','AP.SUPPLIER.VIEW,AP.SUPPLIER.MAINT,AP.SUPPLIER.APPROVE,AP.DUEDILIGENCE.MAINT,AP.BANK.VIEW,AP.BANK.MAINT,AP.BANK.VERIFY,AP.INVOICE.APPROVE,AP.HOLD.SET,AP.PRACTICES.REPORT,AP.ENQUIRY.VIEW,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,SYS.EXCEPTION.RESOLVE,SYS.SANCTIONS.SCREEN,RPT.RUN'),
 ('TREASURY','AP.PAYMENT.APPROVE,AP.PAYMENT.RELEASE,AP.PAYMENT.POST,AP.BANK.APPROVE,AP.BANK.VIEW,AP.ENQUIRY.VIEW,FIN.BANK.MAINT,FIN.BANK.RECONCILE,FIN.ENQUIRY.VIEW,SYS.BANKDETAIL.VIEW,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,RPT.RUN'),
 ('BUYER','PO.REQ.ENTER,PO.ORDER.ENTER,PO.ORDER.AMEND,PO.ORDER.RELEASE,PO.ORDER.CLOSE,PO.SCHEDULE.MAINT,PO.IMPORT.ENTER,PO.SERVICE.ACCEPT,PO.ENQUIRY.VIEW,AP.SUPPLIER.VIEW,ITM.ITEM.VIEW,ITM.STOCK.VIEW,CST.COST.VIEW,SYS.EXCEPTION.VIEW,RPT.RUN'),
 ('PURMGR','PO.REQ.APPROVE,PO.ORDER.APPROVE,PO.ORDER.APPROVE.HIGH,PO.RETURN.AUTHORISE,PO.ORDER.CLOSE,PO.ENQUIRY.VIEW,AP.SUPPLIER.VIEW,ITM.ITEM.VIEW,ITM.STOCK.VIEW,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,SYS.EXCEPTION.RESOLVE,RPT.RUN,RPT.KPI.VIEW'),
 ('SALESREP','SO.QUOTE.ENTER,SO.ORDER.ENTER,SO.ORDER.AMEND,SO.ENQUIRY.VIEW,AR.CUSTOMER.VIEW,AR.CREDIT.VIEW,ITM.ITEM.VIEW,ITM.STOCK.VIEW,RPT.RUN'),
 ('SALESMGR','SO.QUOTE.APPROVE,SO.ORDER.RELEASE,SO.ORDER.CANCEL,SO.PRICE.MAINT,SO.PRICE.OVERRIDE,SO.MARGIN.OVERRIDE,SO.REBATE.MAINT,SO.REBATE.APPROVE,SO.RETURN.AUTHORISE,SO.ENQUIRY.VIEW,AR.CREDITNOTE.APPROVE,AR.CUSTOMER.VIEW,AR.CREDIT.VIEW,PO.ORDER.APPROVE,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,RPT.RUN,RPT.KPI.VIEW'),
 ('WHSE','SO.PICK.CONFIRM,SO.DESPATCH.CONFIRM,SO.RETURN.RECEIVE,PO.RECEIPT.CONFIRM,ITM.ITEM.VIEW,ITM.STOCK.VIEW,ITM.STOCK.ADJUST,ITM.TRANSFER.POST,ITM.COUNT.ENTER,ITM.SCRAP.POST,ITM.LOT.MAINT,KIT.RESERVE.RUN,KIT.ISSUE.POST,KIT.RETURN.POST,KIT.SCRAP.POST,KIT.COMPLETE.POST,KIT.ENQUIRY.VIEW,RPT.RUN'),
 ('INVMGR','ITM.LOCATION.MAINT,ITM.ADJUST.APPROVE,ITM.COUNT.APPROVE,ITM.QUALITY.RELEASE,ITM.STOCK.VIEW,ITM.ITEM.VIEW,ITM.LOT.MAINT,CST.NRV.APPROVE,CST.COST.VIEW,PO.ORDER.APPROVE,KIT.DISASSEMBLE.POST,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,SYS.EXCEPTION.RESOLVE,RPT.RUN,RPT.KPI.VIEW'),
 ('QUALITY','ITM.QUALITY.RELEASE,ITM.LOT.MAINT,ITM.STOCK.VIEW,ITM.ITEM.VIEW,SO.RETURN.AUTHORISE,SO.RETURN.RECEIVE,PO.RETURN.AUTHORISE,KIT.SUBSTITUTE.APPROVE,KIT.ENQUIRY.VIEW,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,SYS.EXCEPTION.RESOLVE,RPT.RUN'),
 ('ENGINEER','ITM.ITEM.VIEW,ITM.ITEM.MAINT,ITM.UOM.MAINT,ITM.CUSTOMS.MAINT,BOM.VIEW,BOM.MAINT,BOM.EXPLODE.RUN,BOM.WHEREUSED.VIEW,BOM.COST.ROLLUP,KIT.DEF.MAINT,KIT.TRIAL.RUN,KIT.ENQUIRY.VIEW,CST.COST.VIEW,RPT.RUN'),
 ('ENGMGR','ITM.ITEM.RELEASE,ITM.REVISION.APPROVE,BOM.VIEW,BOM.RELEASE,BOM.CHANGE.APPROVE,BOM.WHEREUSED.VIEW,KIT.DEF.RELEASE,KIT.SUBSTITUTE.APPROVE,PO.ORDER.APPROVE,SYS.APPROVAL.VIEW,SYS.EXCEPTION.VIEW,SYS.EXCEPTION.RESOLVE,RPT.RUN'),
 ('COSTACC','CST.COST.VIEW,CST.STANDARD.MAINT,CST.RATE.MAINT,CST.JOB.MAINT,CST.JOB.CLOSE,CST.WIP.VIEW,CST.VARIANCE.VIEW,CST.LANDED.MAINT,CST.NRV.POST,CST.REVALUE.RUN,BOM.COST.ROLLUP,BOM.VIEW,ITM.STOCK.VIEW,ITM.ITEM.VIEW,FIN.ENQUIRY.VIEW,FIN.ACCOUNT.VIEW,FIN.JOURNAL.ENTER,SYS.EXCEPTION.VIEW,RPT.RUN,RPT.FINANCIAL.RUN,RPT.KPI.VIEW'),
 ('VATOWNER','FIN.VAT.PREPARE,FIN.VAT.SUBMIT,FIN.VAT.CORRECT,FIN.ENQUIRY.VIEW,FIN.ACCOUNT.VIEW,FIN.STATUTORY.VIEW,AR.ENQUIRY.VIEW,AP.ENQUIRY.VIEW,SYS.OBLIGATION.VIEW,SYS.EXCEPTION.VIEW,RPT.RUN,RPT.FINANCIAL.RUN'),
 ('RESTRICT','ITM.ITEM.VIEW,ITM.STOCK.VIEW,SO.ORDER.ENTER,SO.ENQUIRY.VIEW,AR.CUSTOMER.VIEW,RPT.RUN');
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT g.RoleCode, LTRIM(v.value), 'SEED'
  FROM @g g CROSS APPLY STRING_SPLIT(g.Perms, ',') v
 WHERE EXISTS (SELECT 1 FROM core.Permission p WHERE p.PermissionCode = LTRIM(v.value))
   AND NOT EXISTS (SELECT 1 FROM core.RolePermission r WHERE r.RoleCode = g.RoleCode AND r.PermissionCode = LTRIM(v.value));
GO
MERGE core.AppUser AS t
USING (VALUES
 ('SYSADM','System Administrator','System administrator','STC','RDG',0),
 ('DIRECTOR','Helen Marchetti','Managing director','STC','RDG',0),
 ('FINCTRL','Alan Prydderch','Financial controller','STC','RDG',0),
 ('GLCLERK','Priya Nandakumar','General ledger clerk','STC','RDG',0),
 ('ARCLERK','Dawn Okafor','Sales ledger clerk','STC','RDG',0),
 ('CREDMGR','Martin Boyce','Credit manager','STC','RDG',0),
 ('APCLERK','Tomasz Zielinski','Purchase ledger clerk','STC','RDG',0),
 ('APMGR','Sian Rutherford','Accounts payable manager','STC','RDG',0),
 ('TREASURY','Gavin Lochhead','Treasury and payment authoriser','STC','RDG',0),
 ('BUYER','Nadia Farouk','Buyer','STC','BHM',0),
 ('PURMGR','Colin Ashworth','Purchasing manager','STC','BHM',0),
 ('SALESREP','Jay Ellington','Sales representative','STC','RDG',0),
 ('SALESMGR','Ruth Kavanagh','Sales manager','STC','RDG',0),
 ('WHSE','Dermot Hanley','Warehouse operator','STC','BHM',0),
 ('INVMGR','Louise Tregaskis','Inventory manager','STC','BHM',0),
 ('QUALITY','Peter Sandhu','Quality manager','STC','BHM',0),
 ('ENGINEER','Ffion Llewellyn','Design engineer','STC','RDG',0),
 ('ENGMGR','Stuart McAllan','Engineering manager','STC','RDG',0),
 ('COSTACC','Hilary Trent','Cost accountant','STC','RDG',0),
 ('VATOWNER','Imran Chaudhry','VAT and tax owner','STC','RDG',0),
 ('AUDITOR','External Audit','Auditor (read only)','STC','RDG',1),
 ('RESTRICT','Oliver Keane','Site clerk, Reading only','STC','RDG',0)
) AS s (UserCode, DisplayName, JobTitle, DefaultCompanyCode, DefaultSiteCode, IsReadOnly)
ON t.UserCode = s.UserCode
WHEN MATCHED AND EXISTS (SELECT s.DisplayName, s.JobTitle, s.DefaultCompanyCode, s.DefaultSiteCode, s.IsReadOnly
                         EXCEPT SELECT t.DisplayName, t.JobTitle, t.DefaultCompanyCode, t.DefaultSiteCode, t.IsReadOnly)
    THEN UPDATE SET DisplayName = s.DisplayName, JobTitle = s.JobTitle, DefaultCompanyCode = s.DefaultCompanyCode,
                    DefaultSiteCode = s.DefaultSiteCode, IsReadOnly = s.IsReadOnly, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN
    INSERT (UserCode, DisplayName, JobTitle, DefaultCompanyCode, DefaultSiteCode, IsReadOnly, EmailAddress, CreatedBy)
    VALUES (s.UserCode, s.DisplayName, s.JobTitle, s.DefaultCompanyCode, s.DefaultSiteCode, s.IsReadOnly,
            LOWER(s.UserCode) + '@stellarcomponents.example', 'SEED');
GO
/*  Scoped role assignments.  '*' in CompanyCode or SiteCode means every company or every site.
    RESTRICT is deliberately limited to company STC, site RDG (COM-019 test case). */
DECLARE @ur TABLE (UserCode varchar(20), RoleCode varchar(20), CompanyCode varchar(10), SiteCode varchar(10));
INSERT @ur
SELECT u.UserCode, u.UserCode, '*', '*' FROM core.AppUser u
 WHERE u.UserCode NOT IN ('RESTRICT','SALESREP','WHSE');
INSERT @ur VALUES ('RESTRICT','RESTRICT','STC','RDG'), ('SALESREP','SALESREP','STC','*'),
                  ('SALESREP','SALESREP','CAL','*'), ('WHSE','WHSE','STC','*'), ('WHSE','WHSE','CAL','*');
INSERT core.UserRole (UserCode, RoleCode, CompanyCode, SiteCode, ValidFrom, CreatedBy)
SELECT x.UserCode, x.RoleCode, x.CompanyCode, x.SiteCode, '2024-04-01', 'SEED'
  FROM @ur x
 WHERE EXISTS (SELECT 1 FROM core.Role r WHERE r.RoleCode = x.RoleCode)
   AND NOT EXISTS (SELECT 1 FROM core.UserRole ur WHERE ur.UserCode = x.UserCode AND ur.RoleCode = x.RoleCode
                     AND ur.CompanyCode = x.CompanyCode AND ur.SiteCode = x.SiteCode);
GO
/*  Map the installing Windows account to SYSADM so the WinForms client can sign in. */
DECLARE @login varchar(128) = SUSER_SNAME();
UPDATE core.AppUser SET WindowsLogin = NULL, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
 WHERE WindowsLogin = @login AND UserCode <> 'SYSADM';
UPDATE core.AppUser SET WindowsLogin = @login, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
 WHERE UserCode = 'SYSADM' AND ISNULL(WindowsLogin, '') <> @login;
GO
/*  Segregation of duties (COM-003).  A user who can originate an action may not approve it.
    Each rule is scoped to the approval type it protects so the control is precise. */
MERGE core.SoDRule AS t
USING (VALUES
 ('SOD-JNL','Journal preparer may not approve','FIN.JOURNAL.ENTER','FIN.JOURNAL.APPROVE','JOURNAL','BLOCK'),
 ('SOD-PAY','Invoice poster may not release payment','AP.INVOICE.POST','AP.PAYMENT.APPROVE','PAYMENTRUN','BLOCK'),
 ('SOD-PAY1','Invoice poster may not approve a single payment','AP.INVOICE.POST','AP.PAYMENT.APPROVE','PAYMENTSINGLE','BLOCK'),
 ('SOD-BANK','Bank detail maintainer may not approve the change','AP.BANK.MAINT','AP.BANK.APPROVE','PAYEEBANK','BLOCK'),
 ('SOD-SUPP','Supplier maintainer may not authorise payment','AP.SUPPLIER.MAINT','AP.PAYMENT.APPROVE','PAYMENTRUN','BLOCK'),
 ('SOD-WOFF','Cash poster may not approve a write off','AR.RECEIPT.POST','AR.WRITEOFF.APPROVE','ARWRITEOFF','BLOCK'),
 ('SOD-CRN','Credit note raiser may not approve it','AR.CREDITNOTE.ENTER','AR.CREDITNOTE.APPROVE','SALESCREDIT','BLOCK'),
 ('SOD-PO','Order raiser may not approve the order','PO.ORDER.ENTER','PO.ORDER.APPROVE','PURCHASEORDER','BLOCK'),
 ('SOD-REQ','Requisitioner may not approve the requisition','PO.REQ.ENTER','PO.REQ.APPROVE','REQUISITION','BLOCK'),
 ('SOD-ADJ','Adjustment enterer may not approve it','ITM.STOCK.ADJUST','ITM.ADJUST.APPROVE','STOCKADJUST','BLOCK'),
 ('SOD-CNT','Count enterer may not approve the count','ITM.COUNT.ENTER','ITM.COUNT.APPROVE','STOCKCOUNT','BLOCK'),
 ('SOD-STD','Standard cost maintainer may not activate it','CST.STANDARD.MAINT','CST.STANDARD.ACTIVATE','STDCOSTACTIVATE','BLOCK'),
 ('SOD-NRV','Write down poster may not approve it','CST.NRV.POST','CST.NRV.APPROVE','STOCKWRITEDOWN','BLOCK'),
 ('SOD-VAT','VAT preparer may not approve the return','FIN.VAT.PREPARE','FIN.VAT.APPROVE','VATRETURN','BLOCK'),
 ('SOD-VATE','VAT preparer may not approve an error disclosure','FIN.VAT.CORRECT','FIN.VAT.APPROVE','VATERROR','BLOCK'),
 ('SOD-BOM','Bill maintainer may not release the revision','BOM.MAINT','BOM.RELEASE','BOMRELEASE','BLOCK'),
 ('SOD-ITEM','Item maintainer may not release the item','ITM.ITEM.MAINT','ITM.ITEM.RELEASE','ITEMRELEASE','BLOCK'),
 ('SOD-KIT','Kit definer may not release the version','KIT.DEF.MAINT','KIT.DEF.RELEASE','KITDEFRELEASE','BLOCK'),
 ('SOD-CRED','Customer maintainer may not approve the limit','AR.CUSTOMER.MAINT','AR.CREDIT.SET','CREDITLIMIT','WARN'),
 ('SOD-PINV','Invoice enterer may not approve the invoice','AP.INVOICE.ENTER','AP.INVOICE.APPROVE','PURCHINVOICE','BLOCK')
) AS s (SoDRuleCode, Name, PermissionA, PermissionB, ApprovalType, Severity)
ON t.SoDRuleCode = s.SoDRuleCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.PermissionA, s.PermissionB, s.ApprovalType, s.Severity
                         EXCEPT SELECT t.Name, t.PermissionA, t.PermissionB, t.ApprovalType, t.Severity)
    THEN UPDATE SET Name = s.Name, PermissionA = s.PermissionA, PermissionB = s.PermissionB,
                    ApprovalType = s.ApprovalType, Severity = s.Severity, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (SoDRuleCode, Name, PermissionA, PermissionB, ApprovalType, Severity, CreatedBy)
    VALUES (s.SoDRuleCode, s.Name, s.PermissionA, s.PermissionB, s.ApprovalType, s.Severity, 'SEED');
GO
/*  Approval matrix (specification 13, section 2.1).  Bands are in sterling; the highest band
    has no maximum.  All amounts are defaults and may be changed as reference data. */
MERGE core.ApprovalRule AS t
USING (VALUES
 ('PURCHASEORDER',1,0,5000,'PO.ORDER.APPROVE','PURMGR','Budget holder'),
 ('PURCHASEORDER',2,5000.01,25000,'PO.ORDER.APPROVE.HIGH','PURMGR','Purchasing manager'),
 ('PURCHASEORDER',3,25000.01,100000,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('PURCHASEORDER',4,100000.01,NULL,'SYS.APPROVE.T4','DIRECTOR','Director'),
 ('REQUISITION',1,0,5000,'PO.REQ.APPROVE','PURMGR','Budget holder'),
 ('REQUISITION',2,5000.01,25000,'PO.ORDER.APPROVE.HIGH','PURMGR','Purchasing manager'),
 ('REQUISITION',3,25000.01,100000,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('REQUISITION',4,100000.01,NULL,'SYS.APPROVE.T4','DIRECTOR','Director'),
 ('JOURNAL',1,0,50000,'FIN.JOURNAL.APPROVE','FINCTRL','Independent approver'),
 ('JOURNAL',2,50000.01,NULL,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('PAYMENTRUN',1,0,50000,'AP.PAYMENT.APPROVE','TREASURY','Payment authoriser'),
 ('PAYMENTRUN',2,50000.01,NULL,'FIN.APPROVE.T3','FINCTRL','Financial controller or director'),
 ('PAYMENTSINGLE',1,0,100000,'AP.PAYMENT.APPROVE','TREASURY','Payment authoriser'),
 ('PAYMENTSINGLE',2,100000.01,NULL,'SYS.APPROVE.T4','DIRECTOR','Director'),
 ('PAYEEBANK',1,0,NULL,'AP.BANK.APPROVE','TREASURY','Treasury after independent verification'),
 ('SALESCREDIT',1,0,1000,'AR.CREDITNOTE.APPROVE','SALESMGR','Sales manager'),
 ('SALESCREDIT',2,1000.01,10000,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('SALESCREDIT',3,10000.01,NULL,'SYS.APPROVE.T4','DIRECTOR','Director'),
 ('ARWRITEOFF',1,0,1000,'AR.WRITEOFF.APPROVE','CREDMGR','Credit manager'),
 ('ARWRITEOFF',2,1000.01,10000,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('ARWRITEOFF',3,10000.01,NULL,'SYS.APPROVE.T4','DIRECTOR','Director'),
 ('STOCKADJUST',1,0,1000,'ITM.ADJUST.APPROVE','INVMGR','Inventory manager'),
 ('STOCKADJUST',2,1000.01,10000,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('STOCKADJUST',3,10000.01,NULL,'SYS.APPROVE.T4','DIRECTOR','Director'),
 ('STOCKCOUNT',1,0,1000,'ITM.COUNT.APPROVE','INVMGR','Inventory manager'),
 ('STOCKCOUNT',2,1000.01,NULL,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('STOCKWRITEDOWN',1,0,1000,'CST.NRV.APPROVE','INVMGR','Inventory manager'),
 ('STOCKWRITEDOWN',2,1000.01,10000,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('STOCKWRITEDOWN',3,10000.01,NULL,'SYS.APPROVE.T4','DIRECTOR','Director'),
 ('PRICEBELOWMIN',1,0,NULL,'SO.MARGIN.OVERRIDE','SALESMGR','Sales manager or sales director'),
 ('CREDITLIMIT',1,0,25000,'AR.CREDIT.SET','CREDMGR','Credit controller'),
 ('CREDITLIMIT',2,25000.01,100000,'AR.CREDIT.SET.HIGH','CREDMGR','Credit manager'),
 ('CREDITLIMIT',3,100000.01,NULL,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('CREDITOVERRIDE',1,0,50000,'AR.CREDIT.OVERRIDE','CREDMGR','Credit manager'),
 ('CREDITOVERRIDE',2,50000.01,NULL,'FIN.APPROVE.T3','FINCTRL','Financial controller'),
 ('STDCOSTACTIVATE',1,0,NULL,'CST.STANDARD.ACTIVATE','FINCTRL','Financial controller'),
 ('VATRETURN',1,0,NULL,'FIN.VAT.APPROVE','FINCTRL','Financial controller'),
 ('VATERROR',1,0,NULL,'FIN.VAT.APPROVE','FINCTRL','Financial controller'),
 ('PAYMENTPRACTICES',1,0,NULL,'AP.PRACTICES.APPROVE','DIRECTOR','Director'),
 ('STATUTORYACCOUNTS',1,0,NULL,'SYS.APPROVE.T4','DIRECTOR','Director'),
 ('PERIODREOPEN',1,0,NULL,'FIN.PERIOD.REOPEN','FINCTRL','Financial controller'),
 ('BUDGET',1,0,NULL,'FIN.BUDGET.APPROVE','FINCTRL','Financial controller'),
 ('PURCHINVOICE',1,0,NULL,'AP.INVOICE.APPROVE','APMGR','Accounts payable manager'),
 ('SUPPLIERNEW',1,0,NULL,'AP.SUPPLIER.APPROVE','APMGR','Accounts payable manager'),
 ('CUSTOMERNEW',1,0,NULL,'AR.CUSTOMER.APPROVE','CREDMGR','Credit manager'),
 ('SANCTIONSMATCH',1,0,NULL,'SYS.SANCTIONS.CLEAR','FINCTRL','Compliance owner'),
 ('QUOTE',1,0,NULL,'SO.QUOTE.APPROVE','SALESMGR','Sales manager'),
 ('REBATE',1,0,NULL,'SO.REBATE.APPROVE','SALESMGR','Sales manager'),
 ('LATEPAYCLAIM',1,0,NULL,'AR.LATEPAY.APPROVE','CREDMGR','Credit manager'),
 ('DDCOLLECTION',1,0,NULL,'AR.DD.COLLECT.APPROVE','CREDMGR','Credit manager'),
 ('BOMRELEASE',1,0,NULL,'BOM.RELEASE','ENGMGR','Engineering manager'),
 ('BOMCHANGE',1,0,NULL,'BOM.CHANGE.APPROVE','ENGMGR','Engineering manager'),
 ('ITEMRELEASE',1,0,NULL,'ITM.ITEM.RELEASE','ENGMGR','Engineering manager'),
 ('ITEMREVISION',1,0,NULL,'ITM.REVISION.APPROVE','ENGMGR','Engineering manager'),
 ('KITDEFRELEASE',1,0,NULL,'KIT.DEF.RELEASE','ENGMGR','Engineering manager'),
 ('KITSUBSTITUTE',1,0,NULL,'KIT.SUBSTITUTE.APPROVE','QUALITY','Quality manager')
) AS s (ApprovalType, SeqNo, MinAmount, MaxAmount, RequiredPermission, RequiredRole, Description)
ON t.CompanyCode = '*' AND t.ApprovalType = s.ApprovalType AND t.SeqNo = s.SeqNo
WHEN MATCHED AND EXISTS (SELECT s.MinAmount, s.MaxAmount, s.RequiredPermission, s.RequiredRole, s.Description
                         EXCEPT SELECT t.MinAmount, t.MaxAmount, t.RequiredPermission, t.RequiredRole, t.Description)
    THEN UPDATE SET MinAmount = s.MinAmount, MaxAmount = s.MaxAmount, RequiredPermission = s.RequiredPermission,
                    RequiredRole = s.RequiredRole, Description = s.Description, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN
    INSERT (CompanyCode, ApprovalType, SeqNo, MinAmount, MaxAmount, RequiredPermission, RequiredRole, ApproversRequired, DecisionRef, Description, CreatedBy)
    VALUES ('*', s.ApprovalType, s.SeqNo, s.MinAmount, s.MaxAmount, s.RequiredPermission, s.RequiredRole,
            CASE WHEN s.ApprovalType LIKE 'PAYMENT%' THEN 2 ELSE 1 END, 'D-07', s.Description, 'SEED');
GO
/*  One worked delegation: the financial controller delegates journal approval while on leave.
    Named, reasoned and inside the 30 day maximum (D-07). */
INSERT core.Delegation (CompanyCode, FromUser, ToUser, ApprovalType, MaxAmount, ValidFrom, ValidTo, Reason, Status, CreatedBy)
SELECT '*', 'FINCTRL', 'COSTACC', 'JOURNAL', 25000, CAST(SYSDATETIME() AS date), DATEADD(day, 14, CAST(SYSDATETIME() AS date)),
       'Financial controller annual leave', 'ACTIVE', 'SEED'
 WHERE NOT EXISTS (SELECT 1 FROM core.Delegation WHERE FromUser = 'FINCTRL' AND ToUser = 'COSTACC' AND ApprovalType = 'JOURNAL');
GO
