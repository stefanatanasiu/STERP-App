/*  Approval control tests.  Each assertion runs in its own transaction and rolls back, so the
    database is left exactly as it was found.
    REQ: COM-003, COM-008, COM-022, D-07 */
SET NOCOUNT ON;
GO
PRINT '# core-approvals';
DECLARE @a TABLE (ApprovalId bigint, CompanyCode varchar(10), DocumentType varchar(20), DocumentRef varchar(40),
                  ApprovalType varchar(30), Amount decimal(18,2), AmountGBP decimal(18,2), ContentHash varchar(64),
                  RequiredPermission varchar(40), RequiredRole varchar(20), RequestedBy varchar(20),
                  RequestedAt datetime2(3), DueAt datetime2(3), Status varchar(14));
DECLARE @id bigint, @msg varchar(400), @st varchar(14);

/*  1 - the preparer may never approve their own item. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @a;
    INSERT @a EXEC core.usp_RequestApproval @CompanyCode = 'STC', @DocumentType = 'JOURNAL', @DocumentRef = 'TST-APPR-1',
        @ApprovalType = 'JOURNAL', @ContentHash = 'HASH-A1', @RequestedBy = 'FINCTRL', @Amount = 1500.00;
    SELECT TOP (1) @id = ApprovalId FROM @a;
    EXEC core.usp_DecideApproval @ApprovalId = @id, @UserCode = 'FINCTRL', @Decision = 'APPROVED';
    PRINT 'not ok 1 - self approval was accepted -- REQ: COM-003';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0006%' PRINT 'ok 1 - self approval is rejected -- REQ: COM-003, D-07';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - a segregation of duties conflict blocks the decision. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @a;
    INSERT @a EXEC core.usp_RequestApproval @CompanyCode = 'STC', @DocumentType = 'JOURNAL', @DocumentRef = 'TST-APPR-2',
        @ApprovalType = 'JOURNAL', @ContentHash = 'HASH-A2', @RequestedBy = 'GLCLERK', @Amount = 2500.00;
    SELECT TOP (1) @id = ApprovalId FROM @a;
    EXEC core.usp_DecideApproval @ApprovalId = @id, @UserCode = 'SYSADM', @Decision = 'APPROVED';
    PRINT 'not ok 2 - a user who can originate the journal was allowed to approve it -- REQ: COM-003';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0007%' PRINT 'ok 2 - segregation of duties blocks the approver -- REQ: COM-003, D-07';
    ELSE PRINT 'not ok 2 - unexpected error: ' + @msg;
END CATCH

/*  3 - an independent approver with the right authority is accepted. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @a;
    INSERT @a EXEC core.usp_RequestApproval @CompanyCode = 'STC', @DocumentType = 'JOURNAL', @DocumentRef = 'TST-APPR-3',
        @ApprovalType = 'JOURNAL', @ContentHash = 'HASH-A3', @RequestedBy = 'GLCLERK', @Amount = 2500.00;
    SELECT TOP (1) @id = ApprovalId FROM @a;
    EXEC core.usp_DecideApproval @ApprovalId = @id, @UserCode = 'FINCTRL', @Decision = 'APPROVED';
    SELECT @st = Status FROM core.Approval WHERE ApprovalId = @id;
    IF @st = 'APPROVED' PRINT 'ok 3 - an independent approver may approve -- REQ: COM-003, D-07';
    ELSE PRINT 'not ok 3 - approval status was ' + ISNULL(@st, 'NULL');
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 3 - unexpected error: ' + @msg;
END CATCH

/*  4 - a material change to the content invalidates the approval. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @a;
    INSERT @a EXEC core.usp_RequestApproval @CompanyCode = 'STC', @DocumentType = 'JOURNAL', @DocumentRef = 'TST-APPR-4',
        @ApprovalType = 'JOURNAL', @ContentHash = 'HASH-B1', @RequestedBy = 'GLCLERK', @Amount = 3000.00;
    SELECT TOP (1) @id = ApprovalId FROM @a;
    EXEC core.usp_DecideApproval @ApprovalId = @id, @UserCode = 'FINCTRL', @Decision = 'APPROVED';
    EXEC core.usp_InvalidateApprovals @CompanyCode = 'STC', @DocumentType = 'JOURNAL', @DocumentRef = 'TST-APPR-4',
        @ContentHash = 'HASH-B2', @UserCode = 'GLCLERK', @Reason = 'Amount changed after approval';
    SELECT @st = Status FROM core.Approval WHERE ApprovalId = @id;
    IF @st = 'INVALIDATED' PRINT 'ok 4 - a material change invalidates the approval -- REQ: COM-003, COM-022';
    ELSE PRINT 'not ok 4 - approval status after the change was ' + ISNULL(@st, 'NULL');
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 4 - unexpected error: ' + @msg;
END CATCH

/*  5 - a named, time bounded delegation carries the authority. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @a;
    INSERT @a EXEC core.usp_RequestApproval @CompanyCode = 'STC', @DocumentType = 'JOURNAL', @DocumentRef = 'TST-APPR-5',
        @ApprovalType = 'JOURNAL', @ContentHash = 'HASH-C1', @RequestedBy = 'GLCLERK', @Amount = 4000.00;
    SELECT TOP (1) @id = ApprovalId FROM @a;
    EXEC core.usp_DecideApproval @ApprovalId = @id, @UserCode = 'COSTACC', @Decision = 'APPROVED',
        @Comment = 'Approved under the financial controller delegation';
    SELECT @st = Status FROM core.Approval WHERE ApprovalId = @id;
    IF @st = 'APPROVED' AND EXISTS (SELECT 1 FROM core.Approval WHERE ApprovalId = @id AND DelegationId IS NOT NULL)
        PRINT 'ok 5 - a named delegation is accepted and recorded -- REQ: COM-008, D-07';
    ELSE PRINT 'not ok 5 - delegated approval status was ' + ISNULL(@st, 'NULL');
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 5 - unexpected error: ' + @msg;
END CATCH

/*  6 - a user without the authority and without a delegation is refused. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @a;
    INSERT @a EXEC core.usp_RequestApproval @CompanyCode = 'STC', @DocumentType = 'PAYRUN', @DocumentRef = 'TST-APPR-6',
        @ApprovalType = 'PAYMENTRUN', @ContentHash = 'HASH-D1', @RequestedBy = 'APCLERK', @Amount = 12000.00;
    SELECT TOP (1) @id = ApprovalId FROM @a;
    EXEC core.usp_DecideApproval @ApprovalId = @id, @UserCode = 'WHSE', @Decision = 'APPROVED';
    PRINT 'not ok 6 - a user without payment authority approved a payment run -- REQ: COM-003';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0008%' PRINT 'ok 6 - approval authority is required -- REQ: COM-003, D-07';
    ELSE PRINT 'not ok 6 - unexpected error: ' + @msg;
END CATCH
GO
