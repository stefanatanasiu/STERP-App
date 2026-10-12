/*  ap.usp_ReserveProposal - payment proposal selection and reservation.  Selection and
    reservation are one serialised operation: the open items are taken with UPDLOCK, HOLDLOCK so
    that two proposals prepared at the same time cannot both claim the same residual (AP-011,
    COM-005).  Items that are blocked, disputed, on a supplier payment hold, without an approved
    payee destination or already reserved elsewhere are listed as exclusions with their reason
    rather than quietly dropped.
    REQ: AP-011, AP-014, AP-017, AP-020, COM-005, D-16 */
CREATE OR ALTER PROCEDURE ap.usp_ReserveProposal
    @CompanyCode varchar(10), @ProposalId int, @UserCode varchar(20), @Mode varchar(10) = 'RESERVE'
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    DECLARE @cutoff date, @payDate date, @ccy char(3), @from varchar(20), @to varchar(20), @status varchar(12), @own bit = 0;
    SELECT @cutoff = SelectionCutoffDate, @payDate = PayDate, @ccy = CurrencyCode, @status = Status
      FROM ap.PaymentProposal WHERE ProposalId = @ProposalId AND CompanyCode = @CompanyCode;
    IF @cutoff IS NULL THROW 50031, 'STERP-E-0031: That payment proposal does not exist.', 1;
    IF @status IN ('RELEASED', 'CANCELLED', 'SUPERSEDED')
        THROW 50031, 'STERP-E-0031: A released or cancelled proposal cannot be reselected.', 1;
    IF @@TRANCOUNT = 0 BEGIN BEGIN TRANSACTION; SET @own = 1; END
    BEGIN TRY
        /*  Release whatever this version previously held before reselecting. */
        UPDATE oi SET ReservedAmountGBP = CASE WHEN oi.ReservedAmountGBP - l.ReservedAmountGBP < 0 THEN 0
                                               ELSE oi.ReservedAmountGBP - l.ReservedAmountGBP END
          FROM ap.OpenItem oi JOIN ap.PaymentProposalLine l ON l.OpenItemId = oi.OpenItemId
         WHERE l.ProposalId = @ProposalId AND l.ReservedAmountGBP > 0;
        DELETE ap.PaymentProposalLine WHERE ProposalId = @ProposalId;
        IF @Mode = 'RELEASE'
        BEGIN
            UPDATE ap.PaymentProposal SET CashPrincipalGBP = 0, CreditsAppliedGBP = 0, DiscountsTakenGBP = 0,
                   TotalOutflowGBP = 0, ReservedTotalGBP = 0, ExcludedCount = 0, ModifiedBy = @UserCode,
                   ModifiedAt = SYSDATETIME() WHERE ProposalId = @ProposalId;
            IF @own = 1 COMMIT TRANSACTION;
            SELECT CAST(0 AS varchar(24)) AS SelectedCount, CAST(0 AS varchar(24)) AS ExcludedCount,
                   CAST(0 AS varchar(24)) AS CashPrincipal, CAST(0 AS varchar(24)) AS CreditsApplied,
                   CAST(0 AS varchar(24)) AS Discounts, CAST(0 AS varchar(24)) AS Reserved, CAST(0 AS varchar(24)) AS Outflow;
            RETURN;
        END
        DECLARE @cand TABLE (OpenItemId bigint, SupplierCode varchar(20), DocumentNumber varchar(30), ItemType varchar(12),
                             DueDate date, CurrencyCode char(3), Residual decimal(18,2), ResidualGBP decimal(18,2),
                             ReservedGBP decimal(18,2), DiscountGBP decimal(18,2), SupplierBankId int,
                             Excluded bit, Reason varchar(200));
        INSERT @cand
        SELECT oi.OpenItemId, oi.SupplierCode, oi.DocumentNumber, oi.ItemType, oi.DueDate, oi.CurrencyCode,
               oi.ResidualAmount, oi.ResidualAmountGBP, oi.ReservedAmountGBP,
               CASE WHEN oi.DiscountDueDate >= @payDate AND oi.ItemType = 'INVOICE' THEN ISNULL(oi.DiscountAmount, 0) ELSE 0 END,
               b.SupplierBankId,
               CASE WHEN oi.IsBlocked = 1 OR oi.IsDisputed = 1 OR s.PaymentHold = 1 OR s.Status = 'PENDING'
                      OR s.SanctionsStatus IN ('MATCH', 'BLOCKED') OR b.SupplierBankId IS NULL
                      OR (oi.ItemType = 'INVOICE' AND oi.ResidualAmountGBP - oi.ReservedAmountGBP <= 0)
                     THEN 1 ELSE 0 END,
               CASE WHEN oi.IsBlocked = 1 THEN 'Payment blocked: ' + ISNULL(oi.BlockReason, 'no reason recorded')
                    WHEN oi.IsDisputed = 1 THEN 'Invoice is in dispute'
                    WHEN s.PaymentHold = 1 THEN 'Supplier payment hold: ' + ISNULL(s.HoldReason, 'no reason recorded')
                    WHEN s.Status = 'PENDING' THEN 'Supplier account is pending approval'
                    WHEN s.SanctionsStatus IN ('MATCH', 'BLOCKED') THEN 'Sanctions screening blocks this supplier'
                    WHEN b.SupplierBankId IS NULL THEN 'No verified and approved payment destination'
                    WHEN oi.ItemType = 'INVOICE' AND oi.ResidualAmountGBP - oi.ReservedAmountGBP <= 0
                         THEN 'Already reserved by another active payment instruction'
                    ELSE NULL END
          FROM ap.OpenItem oi WITH (UPDLOCK, HOLDLOCK)
          JOIN ap.Supplier s ON s.CompanyCode = oi.CompanyCode AND s.SupplierCode = oi.SupplierCode
          OUTER APPLY (SELECT TOP (1) sb.SupplierBankId FROM ap.SupplierBank sb
                        WHERE sb.CompanyCode = oi.CompanyCode AND sb.SupplierCode = oi.SupplierCode
                          AND sb.Status = 'APPROVED' AND (sb.ValidTo IS NULL OR sb.ValidTo >= @payDate)
                        ORDER BY sb.SupplierBankId DESC) b
         WHERE oi.CompanyCode = @CompanyCode AND oi.SettlementStatus IN ('OPEN', 'PARTIAL')
           AND oi.CurrencyCode = @ccy AND oi.ItemType IN ('INVOICE', 'CREDIT', 'ADVANCE')
           AND (oi.ItemType <> 'INVOICE' OR oi.DueDate <= @cutoff)
           AND EXISTS (SELECT 1 FROM ap.PaymentProposal p WHERE p.ProposalId = @ProposalId);

        /* A credit can offset only its own supplier's eligible invoices, never another payee. */
        UPDATE c SET Excluded = 1, Reason = 'No eligible invoice for this supplier to apply the credit against'
          FROM @cand c
         WHERE c.Excluded = 0 AND c.ItemType = 'CREDIT'
           AND NOT EXISTS (SELECT 1 FROM @cand i WHERE i.SupplierCode = c.SupplierCode
                            AND i.ItemType = 'INVOICE' AND i.Excluded = 0
                            AND i.ResidualGBP - i.ReservedGBP - i.DiscountGBP > 0);

        INSERT ap.PaymentProposalLine (ProposalId, LineNum, SupplierCode, OpenItemId, DocumentNumber, SupplierBankId,
                                       CurrencyCode, GrossAmount, GrossAmountGBP, DiscountGBP, CreditAppliedGBP, NetPayGBP,
                                       IsExcluded, ExclusionReason, IsReservedElsewhere, ReservedAmountGBP)
        SELECT @ProposalId, ROW_NUMBER() OVER (ORDER BY c.SupplierCode, c.DueDate, c.OpenItemId), c.SupplierCode,
               c.OpenItemId, c.DocumentNumber, c.SupplierBankId, c.CurrencyCode, c.Residual, c.ResidualGBP, c.DiscountGBP,
               CASE WHEN c.ItemType = 'CREDIT' THEN -c.ResidualGBP ELSE 0 END,
               CASE WHEN c.Excluded = 1 THEN 0
                    WHEN c.ItemType = 'INVOICE' THEN c.ResidualGBP - c.ReservedGBP - c.DiscountGBP
                    ELSE c.ResidualGBP END,
               c.Excluded, c.Reason, CASE WHEN c.ReservedGBP > 0 THEN 1 ELSE 0 END,
               CASE WHEN c.Excluded = 1 OR @Mode <> 'RESERVE' THEN 0
                    WHEN c.ItemType = 'INVOICE' THEN c.ResidualGBP - c.ReservedGBP ELSE 0 END
          FROM @cand c;

        IF @Mode = 'RESERVE'
            UPDATE oi SET ReservedAmountGBP = oi.ReservedAmountGBP + l.ReservedAmountGBP, ModifiedBy = @UserCode,
                          ModifiedAt = SYSDATETIME()
              FROM ap.OpenItem oi JOIN ap.PaymentProposalLine l ON l.OpenItemId = oi.OpenItemId
             WHERE l.ProposalId = @ProposalId AND l.ReservedAmountGBP > 0;

        DECLARE @cash decimal(18,2), @cr decimal(18,2), @disc decimal(18,2), @res decimal(18,2), @sel int, @exc int;
        SELECT @cash = ISNULL(SUM(CASE WHEN IsExcluded = 0 AND NetPayGBP > 0 THEN NetPayGBP ELSE 0 END), 0),
               @cr = ISNULL(SUM(CASE WHEN IsExcluded = 0 THEN CreditAppliedGBP ELSE 0 END), 0),
               @disc = ISNULL(SUM(CASE WHEN IsExcluded = 0 THEN DiscountGBP ELSE 0 END), 0),
               @res = ISNULL(SUM(ReservedAmountGBP), 0),
               @sel = SUM(CASE WHEN IsExcluded = 0 THEN 1 ELSE 0 END), @exc = SUM(CASE WHEN IsExcluded = 1 THEN 1 ELSE 0 END)
          FROM ap.PaymentProposalLine WHERE ProposalId = @ProposalId;
        UPDATE ap.PaymentProposal
           SET CashPrincipalGBP = @cash, CreditsAppliedGBP = @cr, DiscountsTakenGBP = @disc,
               TotalOutflowGBP = CASE WHEN @cash - @cr < 0 THEN 0 ELSE @cash - @cr END, ReservedTotalGBP = @res,
               ExcludedCount = @exc, Status = CASE WHEN @Mode = 'RESERVE' THEN 'PROPOSED' ELSE 'DRAFT' END,
               ContentHash = NULL, ApprovalId = NULL, ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME()
         WHERE ProposalId = @ProposalId;
        IF @own = 1 COMMIT TRANSACTION;
    END TRY
    BEGIN CATCH
        IF @own = 1 AND XACT_STATE() <> 0 ROLLBACK TRANSACTION;
        THROW;
    END CATCH
    SELECT CAST(ISNULL(@sel, 0) AS varchar(24)) AS SelectedCount, CAST(ISNULL(@exc, 0) AS varchar(24)) AS ExcludedCount,
           CAST(ISNULL(@cash, 0) AS varchar(24)) AS CashPrincipal, CAST(ISNULL(@cr, 0) AS varchar(24)) AS CreditsApplied,
           CAST(ISNULL(@disc, 0) AS varchar(24)) AS Discounts, CAST(ISNULL(@res, 0) AS varchar(24)) AS Reserved,
           CAST(ISNULL(CASE WHEN @cash - @cr < 0 THEN 0 ELSE @cash - @cr END, 0) AS varchar(24)) AS Outflow;
END;
GO
