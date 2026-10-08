/*  Purchase ledger enquiry procedures for the client screens.  They are read only: every
    decision - validation, matching, approval, release - belongs to the AP COBOL programs.
    The screens show what the engine has already decided.
    REQ: AP-001, AP-002, AP-003, AP-006, AP-011, AP-012, AP-019, AP-023, AP-027 */
SET NOCOUNT ON;
GO
/*  One supplier with its ledger balance, hold flags and verification state (AP-001, AP-024). */
CREATE OR ALTER PROCEDURE ap.usp_SupplierEnquiry @CompanyCode varchar(10), @SupplierCode varchar(20) = NULL,
    @Mode varchar(10) = 'ALL'
AS
BEGIN
    SET NOCOUNT ON;
    SELECT  s.CompanyCode, s.SupplierCode, s.LegalName, s.Status, s.BusinessSize, s.CurrencyCode, s.TermsCode,
            s.PaymentMethod, s.VatLocation, s.VatRegNo, s.SanctionsStatus, s.PurchaseHold, s.PaymentHold, s.HoldReason,
            ISNULL(b.BalanceGBP, 0)   AS BalanceGBP,
            ISNULL(b.BlockedGBP, 0)   AS BlockedGBP,
            ISNULL(b.ReservedGBP, 0)  AS ReservedGBP,
            ISNULL(k.BankCount, 0)    AS BankCount,
            ISNULL(k.UnapprovedBanks, 0) AS UnapprovedBanks,
            ISNULL(k.CopFailures, 0)  AS CopFailures
      FROM  ap.Supplier s
      LEFT JOIN ap.vw_SupplierBalance b ON b.CompanyCode = s.CompanyCode AND b.SupplierCode = s.SupplierCode
      OUTER APPLY (SELECT COUNT(*) AS BankCount,
                          SUM(CASE WHEN x.Status <> 'APPROVED' AND x.ValidTo IS NULL THEN 1 ELSE 0 END) AS UnapprovedBanks,
                          SUM(CASE WHEN x.CopResult IN ('NOMATCH', 'CLOSEMATCH') THEN 1 ELSE 0 END) AS CopFailures
                     FROM ap.SupplierBank x
                    WHERE x.CompanyCode = s.CompanyCode AND x.SupplierCode = s.SupplierCode) k
     WHERE  s.CompanyCode = @CompanyCode
       AND (@SupplierCode IS NULL OR @SupplierCode = '' OR s.SupplierCode = @SupplierCode
            OR s.LegalName LIKE '%' + @SupplierCode + '%')
       AND (@Mode <> 'HOLD' OR s.PurchaseHold = 1 OR s.PaymentHold = 1)
       AND (@Mode <> 'UNVERIFIED' OR ISNULL(k.UnapprovedBanks, 0) > 0 OR ISNULL(k.CopFailures, 0) > 0)
     ORDER BY s.SupplierCode;
END
GO
/*  The bank records held for a supplier, newest first, so that a change waiting for an
    independent approver and a Confirmation of Payee mismatch are both visible (AP-002, AP-027). */
CREATE OR ALTER PROCEDURE ap.usp_ListSupplierBanks @CompanyCode varchar(10), @SupplierCode varchar(20)
AS
BEGIN
    SET NOCOUNT ON;
    SELECT  b.SupplierBankId, b.BankRef, b.BeneficiaryName, b.CurrencyCode, b.SortCode, b.AccountNumber, b.Iban,
            b.PaymentMethod, b.Status, b.CopResult, b.CopCheckedAt, b.CallBackVerifiedBy, b.PreparedBy, b.ApprovedBy,
            b.ApprovedAt, b.ValidFrom, b.ValidTo,
            CASE WHEN b.Status = 'APPROVED' AND b.ValidTo IS NULL THEN 'USABLE'
                 WHEN b.Status IN ('PENDING', 'VERIFIED') THEN 'AWAITING APPROVAL'
                 WHEN b.Status = 'REJECTED' THEN 'REJECTED' ELSE 'SUPERSEDED' END AS UsableState
      FROM  ap.SupplierBank b
     WHERE  b.CompanyCode = @CompanyCode AND b.SupplierCode = @SupplierCode
     ORDER BY b.SupplierBankId DESC;
END
GO
/*  The invoice register with its registration, matching, accounting and payment states, so the
    clerk can see at a glance what is blocked and why (AP-003, AP-006, AP-008, AP-023). */
CREATE OR ALTER PROCEDURE ap.usp_ListInvoiceRegister @CompanyCode varchar(10), @SupplierCode varchar(20) = NULL,
    @Mode varchar(12) = 'ALL'
AS
BEGIN
    SET NOCOUNT ON;
    SELECT  i.PurchaseInvoiceId, i.RegisterNumber, i.SupplierCode, s.LegalName AS SupplierName, i.SupplierDocRef,
            i.DocumentType, i.DocumentDate, i.DueDate, i.CurrencyCode, i.StatedGross, i.GrossAmountGBP,
            i.RecoverableVatGBP, i.NonRecoverableVatGBP, i.RegistrationState, i.MatchState, i.AccountingState,
            i.PaymentEligibility, i.SettlementState, i.IsNonPo, i.BlockReason,
            DATEDIFF(day, i.DueDate, CAST(GETDATE() AS date)) AS DaysOverdue,
            ISNULL(m.MatchRows, 0)    AS MatchRows,
            ISNULL(m.LinesMatched, 0) AS LinesMatched
      FROM  ap.PurchaseInvoice i
      JOIN  ap.Supplier s ON s.CompanyCode = i.CompanyCode AND s.SupplierCode = i.SupplierCode
      LEFT JOIN ap.vw_InvoiceMatchState m ON m.PurchaseInvoiceId = i.PurchaseInvoiceId
     WHERE  i.CompanyCode = @CompanyCode
       AND (@SupplierCode IS NULL OR @SupplierCode = '' OR i.SupplierCode = @SupplierCode
            OR i.SupplierDocRef LIKE '%' + @SupplierCode + '%' OR i.RegisterNumber LIKE '%' + @SupplierCode + '%')
       AND (@Mode = 'ALL'
            OR (@Mode = 'BLOCKED'  AND i.PaymentEligibility = 'BLOCKED')
            OR (@Mode = 'UNMATCHED' AND i.MatchState IN ('UNMATCHED', 'PARTIAL', 'VARIANCE'))
            OR (@Mode = 'UNPAID'   AND i.SettlementState IN ('UNPAID', 'PARTPAID'))
            OR (@Mode = 'OVERDUE'  AND i.SettlementState <> 'SETTLED' AND i.DueDate < CAST(GETDATE() AS date)))
     ORDER BY i.DueDate, i.RegisterNumber;
END
GO
/*  Open items for one supplier or for the whole ledger, already aged (AP-016, AP-019). */
CREATE OR ALTER PROCEDURE ap.usp_ListApOpenItems @CompanyCode varchar(10), @SupplierCode varchar(20) = NULL,
    @Mode varchar(10) = 'OPEN'
AS
BEGIN
    SET NOCOUNT ON;
    SELECT  o.OpenItemId, o.SupplierCode, o.SupplierName, o.ItemType, o.DocumentNumber, o.SupplierDocRef,
            o.DocumentDate, o.DueDate, o.CurrencyCode, o.ResidualAmount, o.ResidualAmountGBP, o.ReservedAmountGBP,
            o.SettlementStatus, o.BlockState, o.BlockReason, o.DaysOverdue,
            CASE WHEN o.DaysOverdue <= 0 THEN 'CURRENT' WHEN o.DaysOverdue <= 30 THEN '1-30'
                 WHEN o.DaysOverdue <= 60 THEN '31-60' WHEN o.DaysOverdue <= 90 THEN '61-90'
                 ELSE '90 PLUS' END AS AgeBucket
      FROM  ap.vw_OpenItemAge o
     WHERE  o.CompanyCode = @CompanyCode
       AND (@SupplierCode IS NULL OR @SupplierCode = '' OR o.SupplierCode = @SupplierCode)
       AND (@Mode <> 'OPEN' OR o.SettlementStatus <> 'SETTLED')
       AND (@Mode <> 'BLOCKED' OR o.BlockState <> 'NONE')
     ORDER BY o.SupplierCode, o.DueDate;
END
GO
/*  A payment proposal with its lines and the instructions it produced (AP-011 to AP-016). */
CREATE OR ALTER PROCEDURE ap.usp_ListProposals @CompanyCode varchar(10), @ProposalRef varchar(30) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF @ProposalRef IS NULL OR @ProposalRef = ''
        SELECT  p.ProposalId, p.ProposalRef, p.Version, p.SelectionCutoffDate, p.PayDate, p.PaymentMethod,
                p.BankAccountCode, p.CurrencyCode, p.CashPrincipalGBP, p.CreditsAppliedGBP, p.TotalOutflowGBP,
                p.ExcludedCount, p.Status, p.PreparedBy, p.Approver1,
                (SELECT COUNT(*) FROM ap.PaymentProposalLine l WHERE l.ProposalId = p.ProposalId) AS LineCount
          FROM  ap.PaymentProposal p
         WHERE  p.CompanyCode = @CompanyCode
         ORDER BY p.ProposalId DESC;
    ELSE
        SELECT  l.ProposalLineId, l.LineNum, l.SupplierCode, s.LegalName AS SupplierName, l.DocumentNumber,
                l.CurrencyCode, l.GrossAmount, l.GrossAmountGBP, l.DiscountGBP, l.CreditAppliedGBP, l.NetPayGBP,
                l.IsExcluded, l.ExclusionReason, l.IsReservedElsewhere,
                i.InstructionRef, i.Status AS InstructionStatus, i.RemittanceRef, i.BacsFileRef, i.ValueDate
          FROM  ap.PaymentProposalLine l
          JOIN  ap.PaymentProposal p ON p.ProposalId = l.ProposalId
          JOIN  ap.Supplier s ON s.CompanyCode = p.CompanyCode AND s.SupplierCode = l.SupplierCode
          LEFT JOIN ap.PaymentInstruction i ON i.ProposalId = p.ProposalId AND i.SupplierCode = l.SupplierCode
               AND i.AmountGBP = l.NetPayGBP
         WHERE  p.CompanyCode = @CompanyCode AND p.ProposalRef = @ProposalRef
         ORDER BY l.LineNum;
END
GO
