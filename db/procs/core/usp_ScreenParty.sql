/*  core.usp_ScreenParty - UK financial sanctions screening of customers, suppliers and payees at
    onboarding, on change and on each list update.  A potential match blocks trading until an
    authorised reviewer records a decision.  REQ: COM-029, AR-028, PO-026, D-29 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_ScreenParty @CompanyCode varchar(10), @PartyType varchar(12), @PartyCode varchar(20), @PartyName varchar(120),
    @Country varchar(40) = NULL, @UserCode varchar(20) = NULL, @TriggerEvent varchar(20) = 'ONBOARD'
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @PartyName IS NULL OR LEN(LTRIM(@PartyName)) = 0 THROW 50019, 'STERP-E-0019: A party name is required before screening.', 1;
    DECLARE @norm varchar(120) = UPPER(LTRIM(RTRIM(@PartyName))), @status varchar(12) = 'CLEAR', @entry int, @matched varchar(120), @score decimal(9,4) = 0, @id bigint;
    SET @norm = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(@norm, '.', ''), ',', ''), '''', ''), ' LIMITED', ' LTD'), '  ', ' ');
    SELECT TOP (1) @entry = ListEntryId, @matched = Name, @score = 100 FROM core.SanctionsList WHERE IsActive = 1 AND NameNormalised = @norm;
    IF @entry IS NULL
        SELECT TOP (1) @entry = ListEntryId, @matched = Name, @score = 70 FROM core.SanctionsList
         WHERE IsActive = 1 AND (NameNormalised LIKE '%' + @norm + '%' OR @norm LIKE '%' + NameNormalised + '%' OR ISNULL(Aliases, '') LIKE '%' + @norm + '%')
         ORDER BY LEN(NameNormalised);
    IF @entry IS NOT NULL SET @status = CASE WHEN @score >= 100 THEN 'MATCH' ELSE 'POTENTIAL' END;
    INSERT core.SanctionsScreening (CompanyCode, PartyType, PartyCode, PartyName, Country, ScreenedBy, TriggerEvent, MatchStatus, MatchedEntryId, MatchScore, MatchedName,
                                    Decision, Note)
    VALUES (@CompanyCode, @PartyType, @PartyCode, @PartyName, @Country, @UserCode, @TriggerEvent, @status, @entry, @score, @matched,
            CASE WHEN @status = 'CLEAR' THEN 'CLEARED' ELSE 'PENDING' END,
            CASE WHEN @status = 'CLEAR' THEN 'No match on the UK sanctions list.' ELSE 'Potential match: order release, despatch, receipt of funds and payment are blocked until an authorised reviewer decides.' END);
    SET @id = CAST(SCOPE_IDENTITY() AS bigint);
    IF @status <> 'CLEAR'
        INSERT core.ExceptionItem (CompanyCode, Module, Severity, Category, DocumentType, DocumentRef, Message, OwnerRole, Priority, RaisedBy)
        VALUES (@CompanyCode, 'XMD', 'HOLD', 'SANCTIONS-MATCH', @PartyType, @PartyCode,
                'Sanctions screening returned ' + @status + ' for ' + @PartyName + ' against ' + ISNULL(@matched, '') + '. Trading and payment are blocked pending an authorised decision.', 'FINCTRL', 1, @UserCode);
    SELECT ScreeningId, CompanyCode, PartyType, PartyCode, PartyName, MatchStatus, MatchedEntryId, MatchedName, MatchScore, Decision, ScreenedAt, Note
      FROM core.SanctionsScreening WHERE ScreeningId = @id;
END
GO
