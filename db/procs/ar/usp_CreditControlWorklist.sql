/*  ar.usp_CreditControlWorklist - the credit controller's daily list.  Items are ordered by the
    dunning stage they have reached and then by value, suppressed items are held back unless the
    caller asks for them, and the account summary beside the list shows who is over limit, on hold
    or holding unapplied cash that would clear the debt if it were allocated.
    REQ: AR-015, AR-016, AR-008, AR-024, D-06, D-28 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE ar.usp_CreditControlWorklist @CompanyCode varchar(10), @UserCode varchar(20) = NULL,
    @OwnerUser varchar(20) = NULL, @MinAmountGBP decimal(18,2) = 0, @StageCode varchar(12) = NULL, @IncludeSuppressed bit = 0
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'AR.ENQUIRY', @CompanyCode, NULL) = 0
        THROW 50301, 'STERP-E-0301: You are not authorised to enquire on debtor accounts in this company.', 1;
    IF @StageCode IS NOT NULL AND @StageCode NOT IN ('STATEMENT', 'REMINDER1', 'REMINDER2', 'ACCOUNTSTOP', 'FINALDEMAND')
        THROW 50304, 'STERP-E-0304: Unknown collection stage. Use STATEMENT, REMINDER1, REMINDER2, ACCOUNTSTOP or FINALDEMAND.', 1;
    /*  1 - the overdue items to work, worst stage and largest value first. */
    SELECT w.CompanyCode, w.CustomerCode, w.CustomerName, w.OpenItemId, w.DocumentNumber, w.DocumentDate, w.DueDate,
           w.CurrencyCode, w.ResidualAmount, w.ResidualAmountGBP, w.UndisputedGBP, w.DaysOverdue, w.BucketNo, w.BucketName,
           w.StageNo, w.StageCode, w.StageAction, w.OwnerUser, w.IsDisputed, w.DisputedAmountGBP, w.CaseRef, w.CaseType,
           w.CaseStatus, w.NextActionDate, w.NextAction, w.PromiseAmountGBP, w.PromiseDate, w.PromiseStatus, w.IsSuppressed,
           w.SuppressReason, w.LastReminderStep, w.LastReminderDate, w.CollectionMethod, w.HoldFlag, w.Priority
      FROM ar.vw_CollectionWorklist w
     WHERE w.CompanyCode = @CompanyCode
       AND (@OwnerUser IS NULL OR w.OwnerUser = @OwnerUser)
       AND (@StageCode IS NULL OR w.StageCode = @StageCode)
       AND w.ResidualAmountGBP >= ISNULL(@MinAmountGBP, 0)
       AND (@IncludeSuppressed = 1 OR w.IsSuppressed = 0)
     ORDER BY w.StageNo DESC, w.ResidualAmountGBP DESC, w.DueDate;
    /*  2 - the accounts behind the list, with the cash that is sitting unapplied against them. */
    SELECT b.CompanyCode, b.CustomerCode, b.LegalName AS CustomerName, b.CustomerStatus, b.ControllerUser, b.AccountOwner,
           b.CreditLimitGBP, b.OverrideGBP, b.BalanceGBP, b.ExposureGBP, b.CreditAvailableGBP, b.UtilisationPct, b.CreditState,
           b.HoldFlag, b.HoldReason, b.OverdueGBP, b.UndisputedOverdueGBP, b.DisputedGBP, b.UnappliedCashGBP, b.CreditNoteGBP,
           b.DepositGBP, b.MaxDaysOverdue, b.OldestDueDate, b.RiskClass, b.ReviewDate, b.ContactRestriction,
           x.WorkItems, x.WorstStage, x.NextPromiseDate
      FROM ar.vw_CustomerBalance b
     CROSS APPLY (SELECT ISNULL(COUNT_BIG(w.OpenItemId), 0), ISNULL(MAX(w.StageNo), 0), MIN(w.PromiseDate)
            FROM ar.vw_CollectionWorklist w WHERE w.CompanyCode = b.CompanyCode AND w.CustomerCode = b.CustomerCode
             AND (@OwnerUser IS NULL OR w.OwnerUser = @OwnerUser) AND (@IncludeSuppressed = 1 OR w.IsSuppressed = 0))
          AS x (WorkItems, WorstStage, NextPromiseDate)
     WHERE b.CompanyCode = @CompanyCode AND (x.WorkItems > 0 OR b.CreditState IN ('HOLD', 'OVERLIMIT'))
     ORDER BY x.WorstStage DESC, b.UndisputedOverdueGBP DESC, b.CustomerCode;
END
GO
