/*  cst list procedures - the two worklists the costing screens open on.  They are enquiries
    over cst.CostSet and cst.vw_JobSummary; no rule lives here, because a screen that needs a
    rule asks the COBOL engine for it.
    REQ: CST-007, CST-012, CST-014, CST-016, CST-018 */
SET NOCOUNT ON;
GO
/*  cst.usp_CostSetList - every cost set with the state it is in, who prepared it and who
    approved it.  Preparer and approver are listed side by side because the separation of the
    two is the control (CST-007); a set whose approver equals its preparer is visible rather
    than hidden, and STDCOST refuses it with CST0405 when activation is attempted. */
CREATE OR ALTER PROCEDURE cst.usp_CostSetList
    @CompanyCode varchar(10),
    @Status      varchar(10) = NULL,
    @Purpose     varchar(12) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SELECT CostSet      = s.CostSetCode,
           Name         = s.Name,
           Purpose      = s.Purpose,
           Status       = s.Status,
           Currency     = s.CurrencyCode,
           FiscalYear   = s.FiscalYear,
           EffectiveFrom = s.EffectiveFrom,
           ComparedTo   = s.ComparedToSetCode,
           PreparedBy   = s.PreparedBy,
           ApprovedBy   = s.ApprovedBy,
           ApprovedAt   = s.ApprovedAt,
           ActivatedAt  = s.ActivatedAt,
           Items        = (SELECT COUNT(*) FROM cst.StandardCost c
                            WHERE c.CompanyCode = s.CompanyCode AND c.CostSetCode = s.CostSetCode),
           SelfApproved = CASE WHEN s.ApprovedBy IS NOT NULL
                                AND s.ApprovedBy = s.PreparedBy THEN 'YES' ELSE '' END
      FROM cst.CostSet s
     WHERE s.CompanyCode = @CompanyCode
       AND (@Status IS NULL OR s.Status = @Status)
       AND (@Purpose IS NULL OR s.Purpose = @Purpose)
     ORDER BY CASE s.Status WHEN 'ACTIVE' THEN 1 WHEN 'APPROVED' THEN 2
                            WHEN 'REVIEW' THEN 3 WHEN 'DRAFT' THEN 4 ELSE 5 END,
              s.EffectiveFrom DESC, s.CostSetCode;
END
GO
/*  cst.usp_JobList - the job worklist behind the job cost enquiry.  Sub-jobs are returned
    under their parent and their level is stated, so the reader can see which rows roll into
    which and nothing is counted twice (D-13, CST-014). */
CREATE OR ALTER PROCEDURE cst.usp_JobList
    @CompanyCode varchar(10),
    @JobRef      varchar(20) = NULL,
    @Status      varchar(14) = NULL,
    @Customer    varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SELECT Job         = j.JobRef,
           Parent      = j.ParentJobRef,
           Level       = j.JobLevel,
           Description = j.Description,
           Customer    = j.CustomerCode,
           Operational = j.OperationalStatus,
           Financial   = j.FinancialStatus,
           Budget      = j.BudgetGBP,
           Actual      = j.ActualGBP,
           Committed   = j.CommittedGBP,
           Variance    = j.VarianceGBP,
           Wip         = ISNULL((SELECT TOP (1) w.ClosingValueGBP FROM cst.WipBalance w
                                  WHERE w.CompanyCode = j.CompanyCode AND w.JobRef = j.JobRef
                                  ORDER BY w.FiscalYear DESC, w.PeriodNo DESC), 0)
      FROM cst.vw_JobSummary j
     WHERE j.CompanyCode = @CompanyCode
       AND (@JobRef IS NULL OR j.JobRef = @JobRef OR j.ParentJobRef = @JobRef)
       AND (@Status IS NULL OR j.OperationalStatus = @Status)
       AND (@Customer IS NULL OR j.CustomerCode = @Customer)
     ORDER BY ISNULL(j.ParentJobRef, j.JobRef), j.JobLevel, j.JobRef;
END
GO
