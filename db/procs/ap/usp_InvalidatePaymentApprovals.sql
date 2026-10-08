/*  ap.usp_InvalidatePaymentApprovals - an approved change of payee bank details withdraws every
    payment approval that still relies on the old beneficiary.  Approved-but-unsubmitted
    instructions are cancelled and their reservations released; already submitted instructions
    keep their own approved snapshot and are only corrected by an authorised cancellation and
    replacement, never by an in-place redirect.
    REQ: AP-002, AP-012, AP-013, AP-016, COM-003 */
CREATE OR ALTER PROCEDURE ap.usp_InvalidatePaymentApprovals
    @CompanyCode varchar(10), @SupplierCode varchar(20), @UserCode varchar(20), @Reason varchar(200) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    DECLARE @invalidated int = 0, @unsubmitted int = 0, @own bit = 0;
    SET @Reason = ISNULL(@Reason, 'Payee bank details changed');
    DECLARE @props TABLE (ProposalId int, ProposalRef varchar(30));
    IF @@TRANCOUNT = 0 BEGIN BEGIN TRANSACTION; SET @own = 1; END
    BEGIN TRY
        INSERT @props (ProposalId, ProposalRef)
        SELECT p.ProposalId, p.ProposalRef
          FROM ap.PaymentProposal p WITH (UPDLOCK, HOLDLOCK)
         WHERE p.CompanyCode = @CompanyCode AND p.Status IN ('PROPOSED', 'APPROVED')
           AND EXISTS (SELECT 1 FROM ap.PaymentProposalLine l
                        WHERE l.ProposalId = p.ProposalId AND l.SupplierCode = @SupplierCode AND l.IsExcluded = 0);

        UPDATE a SET Status = 'INVALIDATED', Comment = LEFT(@Reason, 400), DecidedAt = SYSDATETIME()
          FROM core.Approval a JOIN @props p ON p.ProposalRef = a.DocumentRef
         WHERE a.CompanyCode = @CompanyCode AND a.DocumentType = 'PAYMENTPROPOSAL' AND a.Status IN ('PENDING', 'APPROVED');
        SET @invalidated = @@ROWCOUNT;

        UPDATE ap.PaymentProposal
           SET Status = 'DRAFT', ApprovalId = NULL, Approver1 = NULL, Approver1At = NULL, Approver2 = NULL,
               Approver2At = NULL, ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME()
         WHERE ProposalId IN (SELECT ProposalId FROM @props) AND Status = 'APPROVED';

        /*  An instruction that has not left the building may be cancelled and its reservation
            released.  One that has been submitted may not: its outcome is not yet known. */
        UPDATE ap.PaymentInstruction
           SET Status = 'CANCELLED', OutcomeNote = LEFT('Cancelled: ' + @Reason, 300),
               ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME()
         WHERE CompanyCode = @CompanyCode AND SupplierCode = @SupplierCode AND Status = 'APPROVED';
        SET @unsubmitted = @@ROWCOUNT;

        UPDATE oi SET ReservedAmountGBP = CASE WHEN oi.ReservedAmountGBP - l.ReservedAmountGBP < 0 THEN 0
                                               ELSE oi.ReservedAmountGBP - l.ReservedAmountGBP END,
                      ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME()
          FROM ap.OpenItem oi JOIN ap.PaymentProposalLine l ON l.OpenItemId = oi.OpenItemId
          JOIN @props p ON p.ProposalId = l.ProposalId
         WHERE l.ReservedAmountGBP > 0 AND oi.SupplierCode = @SupplierCode;

        UPDATE l SET ReservedAmountGBP = 0, IsReservedElsewhere = 0, ReservedByInstructionId = NULL
          FROM ap.PaymentProposalLine l JOIN @props p ON p.ProposalId = l.ProposalId
         WHERE l.SupplierCode = @SupplierCode;

        INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Reason, Source)
        VALUES (@UserCode, @CompanyCode, 'ap.PaymentProposal', @SupplierCode, 'INVALIDATE', 'APPROVED', 'DRAFT', @Reason, 'SQL');
        IF @own = 1 COMMIT TRANSACTION;
    END TRY
    BEGIN CATCH
        IF @own = 1 AND XACT_STATE() <> 0 ROLLBACK TRANSACTION;
        THROW;
    END CATCH
    SELECT CAST(@invalidated AS varchar(12)) AS ApprovalsInvalidated,
           CAST(@unsubmitted AS varchar(12)) AS InstructionsCancelled;
END;
GO
