/*  Forward foreign keys - relationships whose parent table is created in a later folder.
    Kept here so each table file stays independently re-runnable.  REQ: COM-001, COM-004 */
SET NOCOUNT ON;
DECLARE @fk TABLE (Nm sysname, Sql nvarchar(400));
INSERT INTO @fk (Nm, Sql) VALUES
 ('FK_Site_Calendar',      N'ALTER TABLE core.Site ADD CONSTRAINT FK_Site_Calendar FOREIGN KEY (CalendarCode) REFERENCES core.Calendar(CalendarCode)'),
 ('FK_Company_Calendar',   N'ALTER TABLE core.Company ADD CONSTRAINT FK_Company_Calendar FOREIGN KEY (CalendarCode) REFERENCES core.Calendar(CalendarCode)'),
 ('FK_Company_Currency',   N'ALTER TABLE core.Company ADD CONSTRAINT FK_Company_Currency FOREIGN KEY (BaseCurrency) REFERENCES core.Currency(CurrencyCode)'),
 ('FK_NumberSeries_Co',    N'ALTER TABLE core.NumberSeries ADD CONSTRAINT FK_NumberSeries_Co FOREIGN KEY (CompanyCode) REFERENCES core.Company(CompanyCode)'),
 ('FK_Account_StatLine',   N'ALTER TABLE fin.Account ADD CONSTRAINT FK_Account_StatLine FOREIGN KEY (CompanyCode) REFERENCES core.Company(CompanyCode)'),
 ('FK_DimValue_Co',        N'ALTER TABLE fin.DimensionValue ADD CONSTRAINT FK_DimValue_Co FOREIGN KEY (CompanyCode) REFERENCES core.Company(CompanyCode)'),
 ('FK_AccDimRule_Acc',     N'ALTER TABLE fin.AccountDimRule ADD CONSTRAINT FK_AccDimRule_Acc FOREIGN KEY (CompanyCode, AccountCode) REFERENCES fin.Account(CompanyCode, AccountCode)'),
 ('FK_Period_FiscalYear',  N'ALTER TABLE fin.Period ADD CONSTRAINT FK_Period_FiscalYear FOREIGN KEY (CompanyCode, FiscalYear) REFERENCES fin.FiscalYear(CompanyCode, FiscalYear)'),
 ('FK_VatPeriod_Co',       N'ALTER TABLE fin.VatPeriod ADD CONSTRAINT FK_VatPeriod_Co FOREIGN KEY (CompanyCode) REFERENCES core.Company(CompanyCode)'),
 ('FK_Journal_Period',     N'ALTER TABLE fin.Journal ADD CONSTRAINT FK_Journal_Period FOREIGN KEY (CompanyCode, FiscalYear, PeriodNo) REFERENCES fin.Period(CompanyCode, FiscalYear, PeriodNo)'),
 ('FK_JournalLine_Acc',    N'ALTER TABLE fin.JournalLine ADD CONSTRAINT FK_JournalLine_Acc FOREIGN KEY (CompanyCode, AccountCode) REFERENCES fin.Account(CompanyCode, AccountCode)'),
 ('FK_EventMap_Acc',       N'ALTER TABLE fin.EventAccountMap ADD CONSTRAINT FK_EventMap_Acc FOREIGN KEY (CompanyCode, AccountCode) REFERENCES fin.Account(CompanyCode, AccountCode)'),
 ('FK_BankAccount_Acc',    N'ALTER TABLE fin.BankAccount ADD CONSTRAINT FK_BankAccount_Acc FOREIGN KEY (CompanyCode, AccountCode) REFERENCES fin.Account(CompanyCode, AccountCode)'),
 ('FK_BudgetLine_Budget',  N'ALTER TABLE fin.BudgetLine ADD CONSTRAINT FK_BudgetLine_Budget FOREIGN KEY (CompanyCode, BudgetCode, FiscalYear, Version) REFERENCES fin.Budget(CompanyCode, BudgetCode, FiscalYear, Version)'),
 ('FK_ItemUom_Item',       N'ALTER TABLE inv.ItemUom ADD CONSTRAINT FK_ItemUom_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ItemRevision_Item',  N'ALTER TABLE inv.ItemRevision ADD CONSTRAINT FK_ItemRevision_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ItemSite_Item',      N'ALTER TABLE inv.ItemSite ADD CONSTRAINT FK_ItemSite_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ItemSite_Site',      N'ALTER TABLE inv.ItemSite ADD CONSTRAINT FK_ItemSite_Site FOREIGN KEY (CompanyCode, SiteCode) REFERENCES core.Site(CompanyCode, SiteCode)'),
 ('FK_ItemVat_Item',       N'ALTER TABLE inv.ItemVatClass ADD CONSTRAINT FK_ItemVat_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ItemCustoms_Item',   N'ALTER TABLE inv.ItemCustoms ADD CONSTRAINT FK_ItemCustoms_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ItemPkg_Item',       N'ALTER TABLE inv.ItemPackaging ADD CONSTRAINT FK_ItemPkg_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ItemPartner_Item',   N'ALTER TABLE inv.ItemPartner ADD CONSTRAINT FK_ItemPartner_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_Warehouse_Site',     N'ALTER TABLE inv.Warehouse ADD CONSTRAINT FK_Warehouse_Site FOREIGN KEY (CompanyCode, SiteCode) REFERENCES core.Site(CompanyCode, SiteCode)'),
 ('FK_Location_Whs',       N'ALTER TABLE inv.Location ADD CONSTRAINT FK_Location_Whs FOREIGN KEY (CompanyCode, SiteCode, WarehouseCode) REFERENCES inv.Warehouse(CompanyCode, SiteCode, WarehouseCode)'),
 ('FK_Lot_Item',           N'ALTER TABLE inv.Lot ADD CONSTRAINT FK_Lot_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_Serial_Item',        N'ALTER TABLE inv.Serial ADD CONSTRAINT FK_Serial_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_StockBalance_Item',  N'ALTER TABLE inv.StockBalance ADD CONSTRAINT FK_StockBalance_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_StockBalance_Loc',   N'ALTER TABLE inv.StockBalance ADD CONSTRAINT FK_StockBalance_Loc FOREIGN KEY (CompanyCode, SiteCode, WarehouseCode, LocationCode) REFERENCES inv.Location(CompanyCode, SiteCode, WarehouseCode, LocationCode)'),
 ('FK_BomHeader_Item',     N'ALTER TABLE bom.BomHeader ADD CONSTRAINT FK_BomHeader_Item FOREIGN KEY (CompanyCode, ParentItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ItemCost_Item',      N'ALTER TABLE cst.ItemCost ADD CONSTRAINT FK_ItemCost_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_CostLayer_Item',     N'ALTER TABLE cst.CostLayer ADD CONSTRAINT FK_CostLayer_Item FOREIGN KEY (CompanyCode, ItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_StdCost_Set',        N'ALTER TABLE cst.StandardCost ADD CONSTRAINT FK_StdCost_Set FOREIGN KEY (CompanyCode, CostSetCode) REFERENCES cst.CostSet(CompanyCode, CostSetCode)'),
 ('FK_JobBudget_Job',      N'ALTER TABLE cst.JobBudget ADD CONSTRAINT FK_JobBudget_Job FOREIGN KEY (CompanyCode, JobRef) REFERENCES cst.Job(CompanyCode, JobRef)'),
 ('FK_JobCost_Job',        N'ALTER TABLE cst.JobCostTransaction ADD CONSTRAINT FK_JobCost_Job FOREIGN KEY (CompanyCode, JobRef) REFERENCES cst.Job(CompanyCode, JobRef)'),
 ('FK_CustCredit_Cust',    N'ALTER TABLE ar.CustomerCredit ADD CONSTRAINT FK_CustCredit_Cust FOREIGN KEY (CompanyCode, CustomerCode) REFERENCES ar.Customer(CompanyCode, CustomerCode)'),
 ('FK_CustAddr_Cust',      N'ALTER TABLE ar.CustomerAddress ADD CONSTRAINT FK_CustAddr_Cust FOREIGN KEY (CompanyCode, CustomerCode) REFERENCES ar.Customer(CompanyCode, CustomerCode)'),
 ('FK_Customer_Terms',     N'ALTER TABLE ar.Customer ADD CONSTRAINT FK_Customer_Terms FOREIGN KEY (TermsCode) REFERENCES core.PaymentTerms(TermsCode)'),
 ('FK_SalesInvoice_Cust',  N'ALTER TABLE ar.SalesInvoice ADD CONSTRAINT FK_SalesInvoice_Cust FOREIGN KEY (CompanyCode, CustomerCode) REFERENCES ar.Customer(CompanyCode, CustomerCode)'),
 ('FK_ArOpenItem_Cust',    N'ALTER TABLE ar.OpenItem ADD CONSTRAINT FK_ArOpenItem_Cust FOREIGN KEY (CompanyCode, CustomerCode) REFERENCES ar.Customer(CompanyCode, CustomerCode)'),
 ('FK_ArAlloc_Src',        N'ALTER TABLE ar.Allocation ADD CONSTRAINT FK_ArAlloc_Src FOREIGN KEY (SourceOpenItemId) REFERENCES ar.OpenItem(OpenItemId)'),
 ('FK_ArAlloc_Tgt',        N'ALTER TABLE ar.Allocation ADD CONSTRAINT FK_ArAlloc_Tgt FOREIGN KEY (TargetOpenItemId) REFERENCES ar.OpenItem(OpenItemId)'),
 ('FK_DDMandate_Cust',     N'ALTER TABLE ar.DirectDebitMandate ADD CONSTRAINT FK_DDMandate_Cust FOREIGN KEY (CompanyCode, CustomerCode) REFERENCES ar.Customer(CompanyCode, CustomerCode)'),
 ('FK_SuppAddr_Supp',      N'ALTER TABLE ap.SupplierAddress ADD CONSTRAINT FK_SuppAddr_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_SuppBank_Supp',      N'ALTER TABLE ap.SupplierBank ADD CONSTRAINT FK_SuppBank_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_SuppDd_Supp',        N'ALTER TABLE ap.SupplierDueDiligence ADD CONSTRAINT FK_SuppDd_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_Supplier_Terms',     N'ALTER TABLE ap.Supplier ADD CONSTRAINT FK_Supplier_Terms FOREIGN KEY (TermsCode) REFERENCES core.PaymentTerms(TermsCode)'),
 ('FK_PurchInv_Supp',      N'ALTER TABLE ap.PurchaseInvoice ADD CONSTRAINT FK_PurchInv_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_ApOpenItem_Supp',    N'ALTER TABLE ap.OpenItem ADD CONSTRAINT FK_ApOpenItem_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_ApAlloc_Src',        N'ALTER TABLE ap.Allocation ADD CONSTRAINT FK_ApAlloc_Src FOREIGN KEY (SourceOpenItemId) REFERENCES ap.OpenItem(OpenItemId)'),
 ('FK_ApAlloc_Tgt',        N'ALTER TABLE ap.Allocation ADD CONSTRAINT FK_ApAlloc_Tgt FOREIGN KEY (TargetOpenItemId) REFERENCES ap.OpenItem(OpenItemId)'),
 ('FK_PayInstr_Supp',      N'ALTER TABLE ap.PaymentInstruction ADD CONSTRAINT FK_PayInstr_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_SalesOrder_Cust',    N'ALTER TABLE so.SalesOrder ADD CONSTRAINT FK_SalesOrder_Cust FOREIGN KEY (CompanyCode, CustomerCode) REFERENCES ar.Customer(CompanyCode, CustomerCode)'),
 ('FK_SalesOrder_Site',    N'ALTER TABLE so.SalesOrder ADD CONSTRAINT FK_SalesOrder_Site FOREIGN KEY (CompanyCode, SiteCode) REFERENCES core.Site(CompanyCode, SiteCode)'),
 ('FK_PriceListLine_List', N'ALTER TABLE so.PriceListLine ADD CONSTRAINT FK_PriceListLine_List FOREIGN KEY (CompanyCode, PriceListCode) REFERENCES so.PriceList(CompanyCode, PriceListCode)'),
 ('FK_Despatch_Cust',      N'ALTER TABLE so.Despatch ADD CONSTRAINT FK_Despatch_Cust FOREIGN KEY (CompanyCode, CustomerCode) REFERENCES ar.Customer(CompanyCode, CustomerCode)'),
 ('FK_RebateAgr_Cust',     N'ALTER TABLE so.RebateAgreement ADD CONSTRAINT FK_RebateAgr_Cust FOREIGN KEY (CompanyCode, CustomerCode) REFERENCES ar.Customer(CompanyCode, CustomerCode)'),
 ('FK_PurchOrder_Supp',    N'ALTER TABLE po.PurchaseOrder ADD CONSTRAINT FK_PurchOrder_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_PurchOrder_Site',    N'ALTER TABLE po.PurchaseOrder ADD CONSTRAINT FK_PurchOrder_Site FOREIGN KEY (CompanyCode, SiteCode) REFERENCES core.Site(CompanyCode, SiteCode)'),
 ('FK_PoReceipt_Supp',     N'ALTER TABLE po.Receipt ADD CONSTRAINT FK_PoReceipt_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_SupplierRet_Supp',   N'ALTER TABLE po.SupplierReturn ADD CONSTRAINT FK_SupplierRet_Supp FOREIGN KEY (CompanyCode, SupplierCode) REFERENCES ap.Supplier(CompanyCode, SupplierCode)'),
 ('FK_KitDef_Item',        N'ALTER TABLE kit.KitDefinition ADD CONSTRAINT FK_KitDef_Item FOREIGN KEY (CompanyCode, KitItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ExecKit_Item',       N'ALTER TABLE kit.ExecutionKit ADD CONSTRAINT FK_ExecKit_Item FOREIGN KEY (CompanyCode, ParentItemCode) REFERENCES inv.Item(CompanyCode, ItemCode)'),
 ('FK_ExecKit_Site',       N'ALTER TABLE kit.ExecutionKit ADD CONSTRAINT FK_ExecKit_Site FOREIGN KEY (CompanyCode, SiteCode) REFERENCES core.Site(CompanyCode, SiteCode)');
DECLARE @n sysname, @s nvarchar(400);
DECLARE c CURSOR LOCAL FAST_FORWARD FOR SELECT Nm, Sql FROM @fk;
OPEN c; FETCH NEXT FROM c INTO @n, @s;
WHILE @@FETCH_STATUS = 0
BEGIN
    IF OBJECT_ID(N'dbo.' + @n) IS NULL AND NOT EXISTS (SELECT 1 FROM sys.foreign_keys WHERE name = @n) EXEC (@s);
    FETCH NEXT FROM c INTO @n, @s;
END
CLOSE c; DEALLOCATE c;
GO
