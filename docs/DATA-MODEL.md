# STERP - Stefan's ERP: data model

Database `STERP` on SQL Server 2025. Generated keys come from `sys.indexes`; purposes are
maintained by hand. Every module team reads this before adding tables, so that the shared
contract objects are used rather than duplicated.

## 1. Conventions that apply everywhere

* Tables are PascalCase singular, surrogate keys `<Table>Id` (`bigint` for movements, journal
  lines and audit), business keys are `varchar` codes unique inside `CompanyCode`.
* Every company-owned row carries `CompanyCode varchar(10)`; site-owned rows add `SiteCode`.
  `'*'` means *all companies* or *all sites* in `core.Parameter`, `core.UserRole`,
  `core.ApprovalRule` and `core.Delegation`.
* Money is `decimal(18,2)` held twice — document currency (`...Amount`) and sterling
  (`...AmountGBP`). Unit prices and costs are `decimal(19,6)`, quantities `decimal(19,6)`,
  exchange rates `decimal(18,8)` as **foreign units per £1** (D-14), percentages `decimal(9,4)`.
* Dates are `date`, timestamps `datetime2(3)`, statuses `varchar(12)` with CHECK constraints.
* Mutable tables carry `CreatedBy / CreatedAt / ModifiedBy / ModifiedAt / RowVer` (rowversion,
  optimistic concurrency, COM-016).
* Confirmed documents keep snapshots of the commercial inputs (address, terms, price, VAT code,
  unit) — never update a posted document's money; reverse and replace (COM-006, FIN-006).
* Errors are raised as `THROW 50xxx, 'STERP-E-xxxx: message', 1;`. Procedures return result
  sets only, because COBOL callers read result sets.

Totals: 12 schemas, 179 tables, 24 stored procedures and 3 functions in the shared layer
(plus 14 `usp_Ui*` procedures owned by the .NET framework), 12 audit triggers and 24 SQL
control tests. Executable shared T-SQL (procedures, functions, triggers) is about 890 lines —
the business logic lives in COBOL.

## 2. `core` — companies, people, control and reference data (35 tables)

| Table | Purpose | Key |
|---|---|---|
| `core.Company` | Legal company, statutory identifiers, framework, VAT scheme, base currency | `CompanyCode` |
| `core.CompanyProfile` | Effective-dated statutory profile (VAT numbers, EORI, stagger, registered office) | `CompanyCode, EffectiveFrom` |
| `core.Site` | Operating site with its nation and working calendar | `CompanyCode, SiteCode` |
| `core.AppUser` | Business user, optional Windows login, default company and site | `UserCode` |
| `core.Role` | Business role; `IsApprovalRole` marks authority-holding roles | `RoleCode` |
| `core.UserRole` | Role assignment scoped to company and site, effective dated | `UserCode, RoleCode, CompanyCode, SiteCode` |
| `core.Permission` | Permission catalogue (`AR.RECEIPT.POST` style), sensitivity and approval flags | `PermissionCode` |
| `core.RolePermission` | Grants of permissions to roles | `RoleCode, PermissionCode` |
| `core.SoDRule` | Segregation of duties: a user holding both permissions may not approve | `SoDRuleCode` |
| `core.ApprovalRule` | The D-07 approval matrix: value band to required permission | `CompanyCode, ApprovalType, SeqNo` |
| `core.Approval` | Approval requests and decisions, content hash, delegation used | `ApprovalId` |
| `core.Delegation` | Named, time-bounded delegation of approval authority (max 30 days) | `DelegationId` |
| `core.Parameter` | Effective-dated parameters carrying decisions D-01 to D-30 | `ParamCode, CompanyCode, EffectiveFrom` |
| `core.NumberSeries` | One continuous sequence per company and document type | `CompanyCode, SeriesCode` |
| `core.DocumentNumber` | Every number issued, with void reason and audit trail | `CompanyCode, SeriesCode, DocumentNumber` |
| `core.AuditLog` | Field-level audit of protected data, with reason and authority reference | `AuditId` |
| `core.ExceptionItem` | Work queue of holds, warnings and errors with owner, priority and due date | `ExceptionId` |
| `core.IdempotencyKey` | Correlation ids so a repeated instruction has no second effect | `CorrelationId` |
| `core.JobRun` | Batch job history: submitter, status, return code, log and spool files | `JobRunId` |
| `core.Currency` | Currencies in use with their decimal places | `CurrencyCode` |
| `core.FxRate` | Daily `SPOT`, monthly `HMRC` and `BOE` rates, foreign units per £1 | `CurrencyCode, RateDate, RateType` |
| `core.Calendar` | Working calendars `ENG`, `SCT`, `NIR`, `BACS` | `CalendarCode` |
| `core.CalendarDay` | Expanded day-by-day calendar with day type and holiday name | `CalendarCode, CalDate` |
| `core.Holiday` | Bank and public holidays 2026-2028 by calendar | `CalendarCode, HolidayDate` |
| `core.PaymentTerms` | Terms basis (`INVOICE`, `EOM`, `DAYOFNEXTMONTH`), discount and statutory default | `TermsCode` |
| `core.RetentionClass` | Retention periods, personal data flag and disposal action (D-17) | `RetentionClass` |
| `core.LegalHold` | Legal holds that suspend disposal | `LegalHoldId` |
| `core.SanctionsList` | Sanctions list entries with normalised names and aliases | `ListEntryId` |
| `core.SanctionsScreening` | Screening results and decisions per party and trigger event | `ScreeningId` |
| `core.StatutoryObligation` | Register of filings and payments with due date, owner and evidence | `ObligationId` |
| `core.MenuItem` | Desktop menu tree; `FormClass` drives the WinForms shell | `MenuCode` |
| `core.ReportDef` | Report catalogue: batch program, parameter spec, permission | `ReportCode` |
| `core.MaintDef` / `core.MaintColumn` | Generic maintenance metadata (**owned by the .NET framework**) | `MaintCode` / `MaintCode, ColumnName` |
| `core.UserPreference` | Per-user desktop preferences (**owned by the .NET framework**) | `UserCode, PrefKey` |

## 3. `fin` — general ledger, VAT and treasury (25 tables)

| Table | Purpose | Key |
|---|---|---|
| `fin.AccountClass` | Account classification, normal balance and statement (`BS` / `PL`) | `ClassCode` |
| `fin.Account` | Chart of accounts with control type, manual-journal flag and statutory line | `CompanyCode, AccountCode` |
| `fin.StatutoryLine` | FRS 102 / FRS 105 statutory line definitions | `Framework, StatutoryLineCode` |
| `fin.Dimension` | Analysis dimensions: `CC` cost centre, `PROJ` project | `DimensionCode` |
| `fin.DimensionValue` | Permitted dimension values with owner and validity | `CompanyCode, DimensionCode, DimValue` |
| `fin.AccountDimRule` | `MANDATORY` / `OPTIONAL` / `FORBIDDEN` dimension rule per account | `CompanyCode, AccountCode, DimensionCode` |
| `fin.EventAccountMap` | Business event to debit/credit legs; no mapping means the posting is blocked | `CompanyCode, EventCode, LegCode` |
| `fin.FiscalYear` | Fiscal years, April to March, with close status | `CompanyCode, FiscalYear` |
| `fin.Period` | Twelve periods per year, `FUTURE` / `OPEN` / `SOFT` / `HARD`, with reopen reason | `CompanyCode, FiscalYear, PeriodNo` |
| `fin.CloseChecklist` | Period close timetable and sign-off | `ChecklistId` |
| `fin.JournalStage` | Staging table for journal lines keyed by `CorrelationId` (input to `usp_PostJournal`) | `StageId` |
| `fin.Journal` | Posted journal header: number, type, source, event, reversal links, evidence | `JournalId` |
| `fin.JournalLine` | Journal lines in document currency and sterling with dimensions and subledger refs | `JournalLineId` |
| `fin.RecurringJournal` | Recurring journal templates and their schedule | `CompanyCode, RecurringCode` |
| `fin.Budget` / `fin.BudgetLine` | Approved budgets by cost centre and period, with versions | `CompanyCode, BudgetCode, FiscalYear, Version` / `BudgetLineId` |
| `fin.BankAccount` | Company bank accounts, BACS service user number, supported rails | `CompanyCode, BankAccountCode` |
| `fin.BankStatementLine` | Imported statement lines awaiting reconciliation | `StatementLineId` |
| `fin.BankRecon` | Reconciliation runs and their balances | `ReconId` |
| `fin.VatCode` | VAT codes with liability, mechanism and the MTD nine-box mapping | `VatCode` |
| `fin.VatRate` | Effective-dated rates with statutory reference | `VatCode, EffectiveFrom` |
| `fin.VatTransaction` | Per-document VAT analysis feeding the return | `VatTxnId` |
| `fin.VatPeriod` | VAT return periods and their status | `CompanyCode, VatPeriodKey` |
| `fin.VatReturn` | Nine-box return, preparation, approval and submission evidence | `VatReturnId` |
| `fin.VatErrorCorrection` | FIN-027 error corrections and disclosures | `ErrorId` |

## 4. `inv` — items and stock, module `itm` (20 tables)

| Table | Purpose | Key |
|---|---|---|
| `inv.Uom` | Units of measure with dimension, base unit and factor (imperial conversions) | `UomCode` |
| `inv.Item` | Item master: class, traceability, make/buy, valuation group, cost method, lifecycle, compliance flags | `CompanyCode, ItemCode` |
| `inv.ItemUom` | Per-item conversions by usage (`STOCK`, `PURCHASE`, `SALES`, `ISSUE`, `PACK`, `PRICE`) | `CompanyCode, ItemCode, UomCode, UsageType, EffectiveFrom` |
| `inv.ItemRevision` | Revision history with effectivity, interchangeability and approval | `CompanyCode, ItemCode, Revision` |
| `inv.ItemSite` | Site planning data: default location, lead time, reorder point, ABC class | `CompanyCode, SiteCode, ItemCode` |
| `inv.ItemVatClass` | Effective-dated sales and purchase VAT code per item | `CompanyCode, ItemCode, Direction, EffectiveFrom` |
| `inv.ItemCustoms` | Commodity code, origin, preference evidence, duty rate, export control | `CompanyCode, ItemCode, EffectiveFrom` |
| `inv.ItemPackaging` | Packaging material and weight for producer responsibility reporting | `CompanyCode, ItemCode, PackagingMaterial, PackagingType` |
| `inv.ItemPartner` | Supplier, customer and manufacturer part numbers, prices and breaks | `ItemPartnerId` |
| `inv.ItemSubstitute` | Approved substitutes by context, with customer agreement flag | `CompanyCode, ItemCode, SubstituteItemCode` |
| `inv.Warehouse` | Warehouses within a site | `CompanyCode, SiteCode, WarehouseCode` |
| `inv.Location` | Stock locations with type, availability and ownership flags | `CompanyCode, SiteCode, WarehouseCode, LocationCode` |
| `inv.Lot` | Lots with supplier reference, dates, expiry, quality status and certificate | `CompanyCode, ItemCode, LotCode` |
| `inv.Serial` | Serial numbers with current location, status and warranty end | `CompanyCode, ItemCode, SerialNo` |
| `inv.StockBalance` | Balance by site, location, item, revision, lot, serial, owner and quality status | `StockBalanceId` |
| `inv.StockMovement` | Every movement with direction, cost, source document and correlation id | `MovementId` |
| `inv.StockReservation` | Reservations against orders and kits | `ReservationId` |
| `inv.QualityHold` | Quality holds and their release | `HoldId` |
| `inv.StockCount` / `inv.StockCountLine` | Counts, recounts, variances and approvals | `CountId` / `CountLineId` |

## 5. `bom` — bills of materials (7 tables)

| Table | Purpose | Key |
|---|---|---|
| `bom.BomHeader` | Bill header and revision: status, effectivity window, base quantity, yield, consolidation | `BomHeaderId` |
| `bom.BomComponent` | Component occurrence: usage basis, quantity per, scrap fraction, fixed per run or batch, phantom, optional, alternate group, effectivity | `BomComponentId` |
| `bom.BomByProduct` | By-products and their recovery value basis | `BomByProductId` |
| `bom.BomChange` / `bom.BomChangeLine` | Engineering change orders, their lines and approval | `ChangeId` / `ChangeLineId` |
| `bom.BomExplosion` / `bom.BomExplosionLine` | Stored explosion runs: level, path, gross starts, net requirement | `ExplosionId` / `ExplosionLineId` |

## 6. `cst` — costing, jobs and valuation (18 tables)

| Table | Purpose | Key |
|---|---|---|
| `cst.CostElement` | Cost elements (material, labour, machine, subcontract, overhead, landed, variance) | `ElementCode` |
| `cst.CostPolicy` | Valuation method per valuation group, effective dated | `CompanyCode, ValuationGroup, EffectiveFrom` |
| `cst.CostSet` | Standard cost sets with status and activation approval | `CompanyCode, CostSetCode` |
| `cst.StandardCost` / `cst.StandardCostElement` | Standard cost per item and revision, split by element | `StandardCostId` / `StandardCostId, ElementCode` |
| `cst.ItemCost` | Carrying value, average unit cost, standard, last purchase, NRV and allowance | `ItemCostId` |
| `cst.ItemCostHistory` | Moving weighted average history, one row per recalculation | `CostHistoryId` |
| `cst.CostLayer` | FIFO layers with original and remaining quantity, unit cost and landed adjustment | `LayerId` |
| `cst.WorkCentre` | Work centres with labour, machine and overhead rates | `CompanyCode, WorkCentreCode, EffectiveFrom` |
| `cst.Job` | Jobs and sub-jobs (two levels, D-13) with status and close dates | `CompanyCode, JobRef` |
| `cst.JobBudget` / `cst.JobBudgetLine` | Job budgets by element and period | `JobBudgetId` / `JobBudgetLineId` |
| `cst.JobCostTransaction` | Every cost booked to a job, including late costs | `JobCostId` |
| `cst.WipBalance` | Work in progress balance by job and element | `WipId` |
| `cst.Variance` | Recorded variances by type, job and period | `VarianceId` |
| `cst.LandedCost` / `cst.LandedCostAllocation` | Landed cost documents and their allocation to receipt lines | `LandedCostId` / `AllocationId` |
| `cst.Impairment` | Net realisable value write-downs and their approval | `ImpairmentId` |

## 7. `kit` — kit definition and execution (11 tables)

| Table | Purpose | Key |
|---|---|---|
| `kit.KitDefinition` | Kit parent and version, commercial or execution, status and release | `KitDefId` |
| `kit.KitDefinitionLine` | Kit lines with quantity, substitution rules and standalone selling price | `KitDefLineId` |
| `kit.TrialKit` / `kit.TrialKitLine` | Trial kits (availability simulation) and their results | `TrialId` / `TrialLineId` |
| `kit.ExecutionKit` | Execution kit: parent item, quantity, status, job link, reservation state | `ExecutionKitId` |
| `kit.ExecutionKitLine` | Required, reserved, issued, returned and scrapped quantity per component | `KitLineId` |
| `kit.KitIssue` | Issues, returns and scrap movements against a kit | `KitIssueId` |
| `kit.KitCompletion` | Completions into stock, with serial or lot assignment | `CompletionId` |
| `kit.KitGenealogy` | Which lots and serials went into which completed unit | `GenealogyId` |
| `kit.Disassembly` / `kit.DisassemblyLine` | Disassembly orders and the components recovered | `DisassemblyId` / `DisassemblyLineId` |

## 8. `ar` — debtors and credit control (16 tables)

| Table | Purpose | Key |
|---|---|---|
| `ar.Customer` | Customer master: legal form, VAT location, terms, collection method, sanctions and insolvency status | `CompanyCode, CustomerCode` |
| `ar.CustomerAddress` | Registered, billing, statement and delivery addresses with contacts | `AddressId` |
| `ar.CustomerCredit` | Credit limit, risk class, insurance cover, review date, hold and pro forma flags | `CompanyCode, CustomerCode` |
| `ar.CreditOverride` | Time-limited credit overrides with approver and expiry | `OverrideId` |
| `ar.SalesInvoice` | Invoice and credit note headers with snapshotted terms, address and FX | `InvoiceId` |
| `ar.SalesInvoiceLine` | Invoice lines with snapshotted price, unit, VAT code and cost of sale | `InvoiceLineId` |
| `ar.SalesInvoiceVat` | VAT analysis per invoice and code | `InvoiceId, VatCode` |
| `ar.OpenItem` | Open item ledger: original and residual amount, due date, dispute and dunning state | `OpenItemId` |
| `ar.Receipt` | Receipts by method with unallocated balance and FX | `ReceiptId` |
| `ar.Allocation` | Allocation of receipts and credits to open items, with write-off and discount | `AllocationId` |
| `ar.DirectDebitMandate` | Mandates with service user number, advance notice and guarantee evidence | `MandateId` |
| `ar.DirectDebitCollection` | Collections submitted, returned or represented | `CollectionId` |
| `ar.CollectionCase` / `ar.CollectionAction` | Collection cases and the actions taken on them | `CaseId` / `ActionId` |
| `ar.LatePaymentClaim` | Statutory interest and compensation claims, approval and waiver reason | `ClaimId` |
| `ar.BadDebtRelief` | Bad debt relief claims and repayments | `ReliefId` |

## 9. `ap` — creditors and supplier settlement (16 tables)

| Table | Purpose | Key |
|---|---|---|
| `ap.Supplier` | Supplier master: VAT location, business size, terms, payment method, holds, sanctions status | `CompanyCode, SupplierCode` |
| `ap.SupplierAddress` | Registered, order, remittance and collection addresses | `AddressId` |
| `ap.SupplierBank` | Payee bank details with Confirmation of Payee result, call-back verification and approval | `SupplierBankId` |
| `ap.SupplierDueDiligence` | Sanctions, modern slavery, anti-bribery, insurance and quality evidence | `DueDiligenceId` |
| `ap.PurchaseInvoice` | Invoice and credit note headers with matching and approval state | `PurchaseInvoiceId` |
| `ap.PurchaseInvoiceLine` | Invoice lines with order and receipt links and price variance | `PurchaseInvoiceLineId` |
| `ap.PurchaseInvoiceVat` | VAT analysis per invoice and code, including reverse charge and import VAT | `PurchaseInvoiceId, VatCode` |
| `ap.InvoiceMatch` | Three-way match results and tolerance outcomes | `MatchId` |
| `ap.OpenItem` | Supplier open items with due date, hold and payment state | `OpenItemId` |
| `ap.PaymentProposal` / `ap.PaymentProposalLine` | Payment runs, their lines and authorisation | `ProposalId` / `ProposalLineId` |
| `ap.PaymentInstruction` | Instructions sent to the bank or bureau, with file reference | `PaymentInstructionId` |
| `ap.Allocation` | Allocation of payments and credits to supplier open items | `AllocationId` |
| `ap.SupplierStatement` / `ap.SupplierStatementLine` | Imported supplier statements and reconciliation differences | `StatementId` / `StatementLineId` |
| `ap.PaymentPracticesReport` | Half-yearly payment practices figures and director approval | `ReportId` |

## 10. `so` — sales order management (15 tables)

| Table | Purpose | Key |
|---|---|---|
| `so.PriceList` / `so.PriceListLine` | Price lists, breaks, currency and effectivity | `CompanyCode, PriceListCode` / `PriceLineId` |
| `so.Quote` / `so.QuoteLine` | Quotations, validity and conversion to order | `QuoteId` / `QuoteLineId` |
| `so.SalesOrder` | Order header with snapshotted terms, Incoterms, address and credit state | `SalesOrderId` |
| `so.SalesOrderLine` | Lines with original and revised promise dates, price snapshot and allocation state | `SalesOrderLineId` |
| `so.DeliverySchedule` | Call-off schedules against an order line | `ScheduleId` |
| `so.PickList` / `so.PickListLine` | Picking instructions and confirmed picks by lot and serial | `PickListId` / `PickLineId` |
| `so.Despatch` / `so.DespatchLine` | Despatch confirmation (the revenue event), carrier and export evidence | `DespatchId` / `DespatchLineId` |
| `so.ReturnAuthorisation` / `so.ReturnLine` | Return authorisations, inspection outcome and disposition | `ReturnId` / `ReturnLineId` |
| `so.RebateAgreement` / `so.RebateAccrual` | Rebate agreements, accruals and settlements | `RebateId` / `AccrualId` |

## 11. `po` — purchase order management (12 tables)

| Table | Purpose | Key |
|---|---|---|
| `po.Requisition` / `po.RequisitionLine` | Requisitions, budget check result and approval | `RequisitionId` / `RequisitionLineId` |
| `po.PurchaseOrder` | Order header with commitment value, approval state and Incoterms | `PurchaseOrderId` |
| `po.PurchaseOrderLine` | Lines with original and confirmed dates, price, tolerance and short-close reason | `PurchaseOrderLineId` |
| `po.PurchaseSchedule` | Call-off schedules against an order line | `ScheduleId` |
| `po.Receipt` / `po.ReceiptLine` | Goods receipts, quarantine routing, over-delivery and early-delivery flags | `ReceiptId` / `ReceiptLineId` |
| `po.ServiceAcceptance` | Service acceptance, the recognition event for services | `AcceptanceId` |
| `po.SupplierReturn` / `po.SupplierReturnLine` | Returns to supplier, RMA reference and expected credit | `SupplierReturnId` / `SupplierReturnLineId` |
| `po.ImportEntry` / `po.ImportEntryLine` | Import entries, duty, postponed VAT and C79 evidence | `ImportEntryId` / `ImportEntryLineId` |

## 12. `rpt` and `stg`

| Table | Purpose | Key |
|---|---|---|
| `rpt.MetricDefinition` | KPI definitions, formula, cohort and comparative basis (D-19) | `MetricCode` |
| `rpt.ReconciliationRun` | Cross-module reconciliation runs and their differences | `ReconRunId` |
| `stg.ImportBatch` | Import batches with source, status and row counts | `BatchId` |
| `stg.ImportRow` | Staged rows with validation outcome and error text | `ImportRowId` |

## 13. Procedure and function catalogue

All procedures begin `SET NOCOUNT ON; SET XACT_ABORT ON;`, raise `THROW 50xxx,
'STERP-E-xxxx: ...'` for business errors and return result sets only. Optional parameters are
shown with `=`.

### 13.1 `core`

| Procedure | Parameters | Result set |
|---|---|---|
| `core.usp_CheckPermission` | `@UserCode, @PermissionCode, @CompanyCode, @SiteCode = NULL` | `UserCode, PermissionCode, CompanyCode, SiteCode, Allowed, Reason` |
| `core.usp_GetUserContext` | `@WindowsLogin = NULL, @UserCode = NULL` | user row, then one row per permitted `CompanyCode, CompanyName, SiteCode, SiteName, Nation, CalendarCode, BaseCurrency` |
| `core.usp_GetParameter` | `@ParamCode, @CompanyCode = '*', @AsOf = NULL` | `ParamCode, CompanyCode, Value, DataType, DecisionRef, EffectiveFrom` |
| `core.fn_Param` | `(@ParamCode, @CompanyCode, @AsOf)` | scalar `varchar` value |
| `core.fn_HasPermission` | `(@UserCode, @PermissionCode, @CompanyCode, @SiteCode)` | scalar `bit` |
| `core.usp_NextDocumentNumber` | `@CompanyCode, @SeriesCode, @UserCode = NULL, @CorrelationId = NULL` | `CompanyCode, SeriesCode, DocumentNumber, SequenceNo` — `UPDLOCK, HOLDLOCK`, gap free |
| `core.usp_VoidDocumentNumber` | `@CompanyCode, @SeriesCode, @DocumentNumber, @UserCode, @Reason` | `CompanyCode, SeriesCode, DocumentNumber, Status, VoidReason` |
| `core.usp_WriteAudit` | `@UserCode, @CompanyCode, @EntityType, @EntityKey, @Action, @FieldName = NULL, @OldValue = NULL, @NewValue = NULL, @Reason = NULL, @AuthorityRef = NULL, @Source = 'SQL', @CorrelationId = NULL, @IsSensitive = 0` | `AuditId` |
| `core.usp_RaiseException` | `@CompanyCode, @Module, @Severity, @Category, @Message, @DocumentType = NULL, @DocumentRef = NULL, @SiteCode = NULL, @OwnerUser = NULL, @OwnerRole = NULL, @Priority = NULL, @DueAt = NULL, @AmountGBP = NULL, @Quantity = NULL, @RaisedBy = NULL, @CorrelationId = NULL` | the created `core.ExceptionItem` row |
| `core.usp_ResolveException` | `@ExceptionId, @UserCode, @ResolutionNote, @Status = 'RESOLVED'` | the updated exception row |
| `core.usp_RequestApproval` | `@CompanyCode, @DocumentType, @DocumentRef, @ApprovalType, @ContentHash, @RequestedBy, @Amount = NULL, @CurrencyCode = 'GBP', @AmountGBP = NULL, @DueAt = NULL, @CorrelationId = NULL` | `ApprovalId, ..., RequiredPermission, RequiredRole, Status` (14 columns) |
| `core.usp_DecideApproval` | `@ApprovalId, @UserCode, @Decision, @Comment = NULL` | decided approval row; rejects self-approval (`E-0006`), SoD conflict (`E-0007`), missing authority (`E-0008`) |
| `core.usp_InvalidateApprovals` | `@CompanyCode, @DocumentType, @DocumentRef, @ContentHash, @UserCode = NULL, @Reason = NULL` | `InvalidatedCount, DocumentType, DocumentRef, ContentHash` |
| `core.usp_IdempotentBegin` | `@CorrelationId, @Operation, @CompanyCode = NULL, @UserCode = NULL` | `CorrelationId, Operation, Outcome (NEW/DUPLICATE/INPROGRESS), ResultRef, ResultMessage` |
| `core.usp_IdempotentEnd` | `@CorrelationId, @Status, @ResultRef = NULL, @ResultMessage = NULL` | the completed key row |
| `core.usp_JobRunStart` | `@JobName, @CompanyCode = NULL, @SubmittedBy = NULL, @SiteCode = NULL, @Arguments = NULL` | `JobRunId, JobName, Status, StartedAt` |
| `core.usp_JobRunEnd` | `@JobRunId, @Status, @ReturnCode = 0, @RowsProcessed = NULL, @LogFile = NULL, @SpoolFile = NULL, @Message = NULL` | the completed job run row |
| `core.usp_GetFxRate` | `@CurrencyCode, @RateDate, @RateType = 'SPOT', @CompanyCode = NULL` | `CurrencyCode, RateDate, RateType, UnitsPerGBP, Source, AgeDays`; a missing or zero rate raises `E-0010` |
| `core.usp_IsBusinessDay` | `@CalendarCode, @Date` | `CalendarCode, CheckDate, IsBusinessDay, DayType, HolidayName, NextBusinessDay, PreviousBusinessDay` |
| `core.fn_AddBusinessDays` | `(@CalendarCode, @Date, @Days)` | scalar `date` |
| `core.usp_ScreenParty` | `@CompanyCode, @PartyType, @PartyCode, @PartyName, @Country = NULL, @UserCode = NULL, @TriggerEvent = 'ONBOARD'` | screening row; a match also raises a `HOLD` exception |
| `core.usp_AgeExceptions` | `@AsOf = NULL` | aged and escalated exceptions (nightly job) |
| `core.usp_CheckFxStaleness` | `@AsOf = NULL, @MaxAgeDays = NULL, @RateType = 'SPOT'` | `CurrencyCode, LatestRateDate, AgeDays, MaxAgeDays` (daily job) |

### 13.2 `fin` and `inv`

| Procedure | Parameters | Behaviour and result set |
|---|---|---|
| `fin.usp_ValidatePeriod` | `@CompanyCode, @PostingDate, @UserCode = NULL, @Action = 'POST'` | returns the period and its status; `HARD` and `FUTURE` are refused, `SOFT` needs close authority |
| `fin.usp_PostJournal` | `@CompanyCode, @CorrelationId, @UserCode, @Description, @PostingDate, @JournalType = 'MANUAL', @DocumentDate = NULL, @CurrencyCode = 'GBP', @SourceModule = NULL, @SourceRef = NULL, @EventCode = NULL, @SeriesCode = 'JNL', @FxRate = NULL, @EvidenceRef = NULL` | lines are staged in `fin.JournalStage` under the same `@CorrelationId`; validates account status, control accounts, mandatory and forbidden dimensions, event mapping, period, balance in document currency **and** sterling; returns `JournalId, JournalNumber, CompanyCode, FiscalYear, PeriodNo, TotalDebitGBP, TotalCreditGBP, Status, WasDuplicate` |
| `fin.usp_ReverseJournal` | `@CompanyCode, @JournalNumber, @UserCode, @Reason, @PostingDate = NULL, @ReversePct = 100, @CorrelationId = NULL, @SeriesCode = 'JNLR'` | creates a linked full or partial reversal; the original is never altered |
| `inv.usp_PostStockMovement` | `@CompanyCode, @SiteCode, @WarehouseCode, @LocationCode, @ItemCode, @MovementType, @Quantity, @MovementDate, @UserCode, @Revision = NULL, @LotCode = NULL, @SerialNo = NULL, @OwnerType = 'OWNED', @OwnerCode = NULL, @QualityStatus = 'ACCEPTED', @Direction = NULL, @UomCode = NULL, @EnteredQuantity = NULL, @ConversionFactor = NULL, @UnitCost = NULL, @PostingDate = NULL, @SourceModule = NULL, @SourceDocType = NULL, @SourceDocRef = NULL, @SourceDocLine = NULL, @CounterpartLocationCode = NULL, @ReasonCode = NULL, @Reason = NULL, @ApprovalRef = NULL, @CorrelationId = NULL` | writes `inv.StockMovement`, updates `inv.StockBalance`; never allows a negative balance (`E-0015`), enforces lot and serial control (`E-0027`) and the quantity increment (`E-0028`) |

Error numbers in use: `E-0002` unknown sign-on, `E-0004` numbering, `E-0005` void,
`E-0006` self-approval, `E-0007` segregation of duties, `E-0008` authority, `E-0009` period,
`E-0010` exchange rate, `E-0011` balance, `E-0012` account, `E-0013` dimension, `E-0014` event
mapping, `E-0015` stock, `E-0016`/`E-0017` idempotency and staging, `E-0021` approval rule,
`E-0025` item, `E-0026` location, `E-0027` traceability, `E-0028` quantity increment.

### 13.3 Audit triggers

`core.tr_Company_Audit`, `core.tr_AppUser_Audit`, `core.tr_UserRole_Audit`,
`core.tr_RolePermission_Audit`, `core.tr_Parameter_Audit`, `fin.tr_Account_Audit`,
`ar.tr_Customer_Audit`, `ar.tr_CustomerCredit_Audit`, `ap.tr_Supplier_Audit`,
`ap.tr_SupplierBank_Audit`, `inv.tr_Item_Audit`, `so.tr_PriceListLine_Audit` — all write
field-level rows to `core.AuditLog`.

## 14. Seeded reference identities

* Companies **STC** *Stellar Components Ltd* (sites **RDG** Reading, **BHM** Birmingham) and
  **CAL** *Caledon Assemblies Ltd* (sites **GLA** Glasgow on the Scottish calendar,
  **LIV** Livingston). Base currency GBP; EUR and USD in use.
* Fiscal years FY2025-FY2027, April to March. FY2025 hard closed; FY2026 P01-P05 hard,
  P06 (September 2026) soft, P07-P08 (October, November 2026) open, later periods future.
* Users `SYSADM, DIRECTOR, FINCTRL, GLCLERK, ARCLERK, CREDMGR, APCLERK, APMGR, TREASURY, BUYER,
  PURMGR, SALESREP, SALESMGR, WHSE, INVMGR, QUALITY, ENGINEER, ENGMGR, COSTACC, VATOWNER,
  AUDITOR` (read only) and `RESTRICT` (company STC, site RDG only, no bank details). Each user
  holds the identically named role. The installing Windows login is mapped to `SYSADM`.
* VAT codes `S` 20%, `R` 5%, `Z`, `E`, `O`, `RC`, `DRC`, `PIV`, `C79`, `EXP`, `BD`.
* Payment terms `N14, N30, N45, N60, EOM30, EOM60, DOM15, DOM31, D2N30, D1N45, STAT, PREPAY,
  PROFORMA`. Calendars `ENG`, `SCT`, `NIR`, `BACS` with real bank holidays 2026-2028.
* 116 accounts per company, cost centres `ADMIN, SALES, PURCH, ENG, PROD1, PROD2, WHSE, QUAL,
  MAINT, IT` and projects `GEN, NPD26, TOOLUP, EXPORT, LEAN, ERPMIG`.
* 70 business events with 294 mapping legs, 35 number series per company, 183 parameters.
* Demo master data: 139 items, 68 locations, 45 customers, 35 suppliers, 269 opening stock
  balances, 27 FIFO layers. No transactions are seeded.

## 15. Notes for module teams

1. **Never hard code a policy value.** Read it with `core.fn_Param` or
   `core.usp_GetParameter`; every decision D-01 to D-30 is already seeded.
2. **Use the contract procedures**, do not re-implement numbering, approvals, audit,
   idempotency, FX or period validation.
3. **Post through `fin.usp_PostJournal`**: stage your lines in `fin.JournalStage` with your own
   `CorrelationId`, then call the procedure once. Give it an `@EventCode` that exists in
   `fin.EventAccountMap`; if your module needs a new event, add it to `db\seed\10-fin-eventmap.sql`.
4. **Profit and loss accounts require a cost centre** and the work in progress and variance
   accounts also require a project; balance sheet accounts must not carry a cost centre.
5. **`SYSADM` deliberately trips the segregation of duties rules** because it holds the whole
   permission catalogue. Approve with the business role (`FINCTRL`, `APMGR`, `TREASURY`,
   `CREDMGR`, `SALESMGR`, `INVMGR`, `ENGMGR`, `QUALITY`, `DIRECTOR`).
6. **Add your own tables** under `db\tables\<NN-module>\` and cross-schema foreign keys under
   `db\tables\99-fk\`. Keep scripts re-runnable and re-deploy with
   `build\build-db.ps1 -Only <your folder>` twice.
7. **The .NET framework owns** `db\tables\00-core\90-*.sql`, `91-*`, `92-*`,
   `db\procs\core\usp_Ui*.sql`, `db\seed\05-core-menu-system.sql` and
   `db\seed\06-core-maintdefs.sql`. Register new screens by seeding `core.MenuItem` from your
   own module seed file.

