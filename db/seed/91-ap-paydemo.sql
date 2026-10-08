/*  91-ap-paydemo.sql - the people a demonstration payment run needs.

    D-07 wants TWO independent authorisers on a payment run (core parameter
    APR.PAYRUN.AUTHORISERS = 2) and only the TREASURY role carries
    AP.PAYMENT.APPROVE, so the delivered dataset had exactly one business user who
    could sign and a run could never be authorised end to end without SYSADM.  The
    purchase ledger therefore seeds a deputy treasury authoriser.  APDEMO uses
    APCLERK as preparer, TREASURY as first authoriser, TREASURY2 as second and
    TREASURY as releaser: four duties, four signatures, no system account.

    Idempotent: re-running changes nothing.
    REQ: AP-012, AP-013, COM-003, D-07                                          */

INSERT core.AppUser (UserCode, DisplayName, JobTitle, DefaultCompanyCode, DefaultSiteCode,
                     IsReadOnly, EmailAddress, CreatedBy)
SELECT 'TREASURY2', 'Bethan Carrick', 'Deputy treasury authoriser', 'STC', 'RDG', 0,
       'treasury2@stellarcomponents.example', 'SEED'
 WHERE NOT EXISTS (SELECT 1 FROM core.AppUser u WHERE u.UserCode = 'TREASURY2');
GO

INSERT core.UserRole (UserCode, RoleCode, CompanyCode, SiteCode, ValidFrom, CreatedBy)
SELECT 'TREASURY2', 'TREASURY', '*', '*', '2024-04-01', 'SEED'
 WHERE EXISTS (SELECT 1 FROM core.Role r WHERE r.RoleCode = 'TREASURY')
   AND NOT EXISTS (SELECT 1 FROM core.UserRole ur WHERE ur.UserCode = 'TREASURY2'
                     AND ur.RoleCode = 'TREASURY' AND ur.CompanyCode = '*' AND ur.SiteCode = '*');
GO
