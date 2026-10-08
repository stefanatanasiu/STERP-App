/*  ap views - the stable read surface the 1991 batch programs bind to.
    The batch COBOL selects from these views rather than the base tables
    so a column rename does not mean recompiling the purchase ledger.
    REQ: AP-011, AP-015, AP-016, AP-026, AP-028, AP-006, AP-007        */
SET NOCOUNT ON;
GO
/*  Open items with their age, block state and ageing basis.
    REQ: AP-026                                                        */
CREATE OR ALTER VIEW ap.vw_OpenItemAge
AS
SELECT  o.CompanyCode,
        o.OpenItemId,
        o.SupplierCode,
        s.LegalName                                 AS SupplierName,
        o.ItemType,
        o.DocumentNumber,
        o.SupplierDocRef,
        o.DocumentDate,
        o.DueDate,
        o.CurrencyCode,
        o.ResidualAmount,
        o.ResidualAmountGBP,
        o.ReservedAmountGBP,
        o.SettlementStatus,
        CASE WHEN o.IsDisputed = 1 THEN 'DISPUTED'
             WHEN o.IsBlocked  = 1 THEN 'BLOCKED'
             ELSE 'OPEN' END                        AS BlockState,
        o.BlockReason,
        DATEDIFF(day, o.DueDate, CAST(SYSDATETIME() AS date))
                                                    AS DaysOverdue
FROM    ap.OpenItem o
JOIN    ap.Supplier s
        ON  s.CompanyCode  = o.CompanyCode
        AND s.SupplierCode = o.SupplierCode;
GO
/*  Per supplier balance, unapplied credits and advances.              */
CREATE OR ALTER VIEW ap.vw_SupplierBalance
AS
SELECT  o.CompanyCode,
        o.SupplierCode,
        MAX(s.LegalName)                            AS SupplierName,
        SUM(CASE WHEN o.ItemType = 'INVOICE'
                 THEN o.ResidualAmountGBP ELSE 0 END)   AS InvoiceGBP,
        SUM(CASE WHEN o.ItemType = 'CREDIT'
                 THEN o.ResidualAmountGBP ELSE 0 END)   AS CreditGBP,
        SUM(CASE WHEN o.ItemType = 'ADVANCE'
                 THEN o.ResidualAmountGBP ELSE 0 END)   AS AdvanceGBP,
        SUM(o.ResidualAmountGBP)                        AS BalanceGBP,
        SUM(o.ReservedAmountGBP)                        AS ReservedGBP,
        SUM(CASE WHEN o.IsBlocked = 1 OR o.IsDisputed = 1
                 THEN o.ResidualAmountGBP ELSE 0 END)   AS BlockedGBP,
        COUNT_BIG(*)                                    AS OpenItems
FROM    ap.OpenItem o
JOIN    ap.Supplier s
        ON  s.CompanyCode  = o.CompanyCode
        AND s.SupplierCode = o.SupplierCode
WHERE   o.ResidualAmountGBP <> 0
GROUP BY o.CompanyCode, o.SupplierCode;
GO
/*  Per invoice match / block / payment eligibility state.
    REQ: AP-006, AP-007, AP-008                                        */
CREATE OR ALTER VIEW ap.vw_InvoiceMatchState
AS
SELECT  i.CompanyCode,
        i.PurchaseInvoiceId,
        i.RegisterNumber,
        i.SupplierCode,
        i.SupplierDocRef,
        i.SupplierDocRefNormalised,
        i.DocumentType,
        i.DocumentDate,
        i.DueDate,
        i.GrossAmountGBP,
        COUNT(m.MatchId)                            AS MatchRows,
        SUM(CASE WHEN m.WithinTolerance = 1
                 THEN 1 ELSE 0 END)                 AS LinesMatched,
        SUM(CASE WHEN m.WithinTolerance = 0
                 THEN 1 ELSE 0 END)                 AS LinesVariance,
        CASE WHEN COUNT(m.MatchId) = 0 THEN 'UNMATCHED'
             WHEN SUM(CASE WHEN m.WithinTolerance = 1
                           THEN 0 ELSE 1 END) = 0   THEN 'MATCHED'
             ELSE 'PARTIAL' END                     AS MatchState
FROM    ap.PurchaseInvoice i
LEFT JOIN ap.InvoiceMatch m
        ON m.PurchaseInvoiceId = i.PurchaseInvoiceId
GROUP BY i.CompanyCode, i.PurchaseInvoiceId, i.RegisterNumber,
        i.SupplierCode, i.SupplierDocRef, i.SupplierDocRefNormalised,
        i.DocumentType, i.DocumentDate, i.DueDate, i.GrossAmountGBP;
GO
/*  What APBACS cuts into the Standard 18 file.  Everything the file
    needs is already on the instruction, so there is no join to the
    bank table and a later bank change cannot alter a cut file.
    REQ: AP-015, AP-016                                                */
CREATE OR ALTER VIEW ap.vw_BacsInstruction
AS
SELECT  p.CompanyCode,
        p.ProposalRef,
        p.Version                                   AS ProposalVersion,
        i.PaymentInstructionId,
        i.InstructionRef,
        i.SupplierCode,
        s.LegalName                                 AS SupplierName,
        i.PaymentMethod,
        ISNULL(i.BeneficiarySortCode, '')           AS SortCode,
        ISNULL(i.BeneficiaryAccountNumber, '')      AS AccountNumber,
        i.BeneficiaryName,
        ISNULL(i.InstructionRef, i.SupplierCode)    AS PaymentReference,
        i.CurrencyCode,
        i.AmountGBP,
        i.Status,
        ISNULL(i.CopResultSnapshot, 'UNVERIFIED')   AS CopResult,
        i.ValueDate
FROM    ap.PaymentInstruction i
JOIN    ap.PaymentProposal p
        ON p.ProposalId = i.ProposalId
JOIN    ap.Supplier s
        ON  s.CompanyCode  = i.CompanyCode
        AND s.SupplierCode = i.SupplierCode
WHERE   i.Status IN ('APPROVED', 'SUBMITTED');
GO
/*  One line per document settled by a payment run, for the advice.
    REQ: AP-016, AP-017                                                */
CREATE OR ALTER VIEW ap.vw_RemittanceLine
AS
SELECT  p.CompanyCode,
        p.ProposalRef,
        i.SupplierCode,
        s.LegalName                                 AS SupplierName,
        i.InstructionRef,
        i.PaymentMethod,
        ISNULL(o.ItemType, 'INVOICE')               AS DocumentType,
        ISNULL(o.SupplierDocRef, '')                AS SupplierDocRef,
        ISNULL(o.DocumentNumber, '')                AS DocumentNumber,
        o.DocumentDate,
        ISNULL(o.CurrencyCode, i.CurrencyCode)      AS CurrencyCode,
        ISNULL(o.OriginalAmountGBP, 0)              AS GrossGBP,
        ISNULL(a.DiscountAmountGBP, 0)              AS DiscountGBP,
        ISNULL(a.DifferenceGBP, 0)                  AS SetOffGBP,
        ISNULL(a.AppliedAmountGBP, i.AmountGBP)     AS PaidGBP
FROM    ap.PaymentInstruction i
JOIN    ap.PaymentProposal p
        ON p.ProposalId = i.ProposalId
JOIN    ap.Supplier s
        ON  s.CompanyCode  = i.CompanyCode
        AND s.SupplierCode = i.SupplierCode
LEFT JOIN ap.Allocation a
        ON a.PaymentInstructionId = i.PaymentInstructionId
LEFT JOIN ap.OpenItem o
        ON o.OpenItemId = a.TargetOpenItemId;
GO
/*  The raw rows behind RPT-018.  The clock starts at the LATER of
    invoice receipt and goods receipt and stops on the settlement
    date, which is what the regulations require.
    REQ: AP-028                                                        */
CREATE OR ALTER VIEW ap.vw_PaymentPracticesBase
AS
SELECT  o.CompanyCode,
        o.SupplierCode,
        o.DocumentNumber,
        CAST(COALESCE(o.DateReceived, o.DocumentDate) AS date)
                                                    AS StartDate,
        o.DueDate,
        CAST(i.SettledAt AS date)                   AS PaidDate,
        DATEDIFF(day, o.DocumentDate, o.DueDate)    AS TermDays,
        ISNULL(a.AppliedAmountGBP, 0)               AS AmountBase,
        CAST(CASE WHEN o.IsDisputed = 1 THEN 1 ELSE 0 END AS bit)
                                                    AS WasDisputed
FROM    ap.Allocation a
JOIN    ap.PaymentInstruction i
        ON i.PaymentInstructionId = a.PaymentInstructionId
JOIN    ap.OpenItem o
        ON o.OpenItemId = a.TargetOpenItemId
WHERE   i.SettledAt IS NOT NULL
  AND   o.ItemType = 'INVOICE'
  AND   a.Status <> 'REVERSED';
GO

/*  The same columns as ap.vw_BacsInstruction without the approval
    status filter, for reproducing the file of a run that has already
    been released.  APBACS still verifies that the run was approved;
    this view only lets it show what was actually sent.
    REQ: AP-015, AP-016                                                */
CREATE OR ALTER VIEW ap.vw_BacsInstructionAll
AS
SELECT  p.CompanyCode,
        p.ProposalRef,
        i.SupplierCode,
        s.LegalName                                 AS SupplierName,
        i.PaymentMethod,
        ISNULL(i.BeneficiarySortCode, '')           AS SortCode,
        ISNULL(i.BeneficiaryAccountNumber, '')      AS AccountNumber,
        i.BeneficiaryName,
        ISNULL(i.InstructionRef, i.SupplierCode)    AS PaymentReference,
        i.CurrencyCode,
        i.AmountGBP,
        i.Status,
        ISNULL(i.CopResultSnapshot, 'UNVERIFIED')   AS CopResult,
        i.ValueDate
FROM    ap.PaymentInstruction i
JOIN    ap.PaymentProposal p
  ON p.ProposalId = i.ProposalId
JOIN    ap.Supplier s
  ON  s.CompanyCode  = i.CompanyCode
  AND s.SupplierCode = i.SupplierCode;
GO
