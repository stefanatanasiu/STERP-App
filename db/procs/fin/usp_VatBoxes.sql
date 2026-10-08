/*  fin.usp_VatBoxes - the nine box VAT return built from posted VAT records.  Nothing is ever
    typed into a box: every figure is an aggregation of fin.VatTransaction rows (FIN-025,
    FIN-026).  Box 3 = Box 1 + Box 2 and Box 5 = Box 3 - Box 4; boxes 6 to 9 are net of VAT and
    reported in whole pounds.  Under the cash accounting scheme (FIN-029) the invoice-basis
    rows are replaced by the settled proportions held in fin.VatCashLink, except for postponed
    import VAT and reverse charges, which always stay on the invoice basis.
    Transactions posted late into a finalised period are carried into the next open return and
    flagged as late entries (FIN-018).
    REQ: FIN-018, FIN-025, FIN-026, FIN-028, FIN-029 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE fin.usp_VatBoxes @CompanyCode varchar(10), @VatPeriodKey varchar(10), @Basis varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF @Basis IS NULL SELECT @Basis = ISNULL(VatScheme, 'INVOICE') FROM core.Company WHERE CompanyCode = @CompanyCode;
    DECLARE @b TABLE (BoxNo tinyint, BoxValue decimal(18,2), TransactionCount int);
    /*  Invoice basis, and the mechanisms that stay on the invoice basis under cash accounting. */
    INSERT @b (BoxNo, BoxValue, TransactionCount)
    SELECT d.BoxNo, SUM(d.Contribution), COUNT(DISTINCT d.VatTxnId)
      FROM fin.vw_VatBoxDetail d
     WHERE d.CompanyCode = @CompanyCode AND d.VatPeriodKey = @VatPeriodKey
       AND (@Basis <> 'CASH' OR d.Mechanism IN ('PVA', 'RCSERVICE', 'DRC', 'C79'))
     GROUP BY d.BoxNo;
    /*  Cash basis: VAT and net follow the money, in proportion to the amount settled. */
    IF @Basis = 'CASH'
    BEGIN
        INSERT @b (BoxNo, BoxValue, TransactionCount)
        SELECT x.BoxNo, SUM(x.Contribution), COUNT(DISTINCT x.VatTxnId)
          FROM (SELECT k.VatTxnId, CASE WHEN k.Direction = 'OUTPUT' THEN t.VatBox ELSE t.VatBox END AS BoxNo, k.SettledVatGBP AS Contribution
                  FROM fin.VatCashLink k JOIN fin.VatTransaction t ON t.VatTxnId = k.VatTxnId
                 WHERE k.CompanyCode = @CompanyCode AND k.VatPeriodKey = @VatPeriodKey AND t.Mechanism NOT IN ('PVA', 'RCSERVICE', 'DRC', 'C79') AND t.VatBox IS NOT NULL
                UNION ALL
                SELECT k.VatTxnId, t.NetBox, k.SettledNetGBP
                  FROM fin.VatCashLink k JOIN fin.VatTransaction t ON t.VatTxnId = k.VatTxnId
                 WHERE k.CompanyCode = @CompanyCode AND k.VatPeriodKey = @VatPeriodKey AND t.Mechanism NOT IN ('PVA', 'RCSERVICE', 'DRC', 'C79') AND t.NetBox IS NOT NULL) x
         GROUP BY x.BoxNo;
    END
    /*  Error corrections approved for this return adjust box 1 or box 4 (FIN-027). */
    INSERT @b (BoxNo, BoxValue, TransactionCount)
    SELECT CASE WHEN e.NetErrorAmount >= 0 THEN 1 ELSE 4 END, SUM(ABS(e.NetErrorAmount)), COUNT(*)
      FROM fin.VatErrorCorrection e
     WHERE e.CompanyCode = @CompanyCode AND e.CorrectionVatPeriodKey = @VatPeriodKey AND e.Method = 'NEXTRETURN' AND e.Status IN ('APPROVED', 'ADJUSTED')
     GROUP BY CASE WHEN e.NetErrorAmount >= 0 THEN 1 ELSE 4 END;
    DECLARE @b1 decimal(18,2) = ISNULL((SELECT SUM(BoxValue) FROM @b WHERE BoxNo = 1), 0);
    DECLARE @b2 decimal(18,2) = ISNULL((SELECT SUM(BoxValue) FROM @b WHERE BoxNo = 2), 0);
    DECLARE @b4 decimal(18,2) = ISNULL((SELECT SUM(BoxValue) FROM @b WHERE BoxNo = 4), 0);
    DECLARE @b6 decimal(18,2) = ISNULL((SELECT SUM(BoxValue) FROM @b WHERE BoxNo = 6), 0);
    DECLARE @b7 decimal(18,2) = ISNULL((SELECT SUM(BoxValue) FROM @b WHERE BoxNo = 7), 0);
    SELECT @CompanyCode AS CompanyCode, @VatPeriodKey AS VatPeriodKey, @Basis AS Basis,
           @b1 AS Box1, @b2 AS Box2, @b1 + @b2 AS Box3, @b4 AS Box4, (@b1 + @b2) - @b4 AS Box5,
           FLOOR(@b6) AS Box6, FLOOR(@b7) AS Box7, CAST(0 AS decimal(18,0)) AS Box8, CAST(0 AS decimal(18,0)) AS Box9,
           ISNULL((SELECT SUM(TransactionCount) FROM @b), 0) AS RecordCount,
           ISNULL((SELECT COUNT(*) FROM fin.VatTransaction WHERE CompanyCode = @CompanyCode AND VatPeriodKey = @VatPeriodKey AND IsLateEntry = 1), 0) AS LateEntryCount;
END
GO
/*  fin.usp_VatBoxDrill - the source records behind one box.  A finalised return drills into the
    frozen detail so that a later edit of a source transaction cannot change what was reported;
    an open return drills into the live VAT records.  REQ: FIN-018, FIN-025, FIN-026 */
CREATE OR ALTER PROCEDURE fin.usp_VatBoxDrill @CompanyCode varchar(10), @VatPeriodKey varchar(10), @BoxNo tinyint, @Version smallint = NULL
AS
BEGIN
    SET NOCOUNT ON;
    DECLARE @rid int;
    SELECT TOP (1) @rid = VatReturnId FROM fin.VatReturn
     WHERE CompanyCode = @CompanyCode AND VatPeriodKey = @VatPeriodKey AND (@Version IS NULL OR Version = @Version) AND Status IN ('FINALISED', 'SUBMITTED', 'AMENDED')
     ORDER BY Version DESC;
    IF @rid IS NOT NULL
        SELECT d.BoxNo, t.DocumentType, t.DocumentRef, t.PartyCode, t.TaxPointDate, t.PostingDate, t.VatCode, t.Mechanism,
               t.Direction, t.NetAmountGBP, t.VatAmountGBP, d.ContributionGBP, d.IsLateEntry, t.JournalId, 'FINALISED' AS SourceKind
          FROM fin.VatReturnDetail d JOIN fin.VatTransaction t ON t.VatTxnId = d.VatTxnId
         WHERE d.VatReturnId = @rid AND d.BoxNo = @BoxNo
         ORDER BY t.TaxPointDate, t.DocumentRef;
    ELSE
        SELECT d.BoxNo, d.DocumentType, d.DocumentRef, d.PartyCode, d.TaxPointDate, d.PostingDate, d.VatCode, d.Mechanism,
               d.Direction, d.NetAmountGBP, d.VatAmountGBP, d.Contribution AS ContributionGBP, d.IsLateEntry, NULL AS JournalId, 'OPEN' AS SourceKind
          FROM fin.vw_VatBoxDetail d
         WHERE d.CompanyCode = @CompanyCode AND d.VatPeriodKey = @VatPeriodKey AND d.BoxNo = @BoxNo
         ORDER BY d.TaxPointDate, d.DocumentRef;
END
GO
