/*  STERP - Stefan's ERP - 00-create-database.sql
    Creates the STERP database on the local default instance if it does not exist.
    Run by build\build-db.ps1 (connected to master).  -Full drops it first. */
SET NOCOUNT ON;
IF DB_ID(N'STERP') IS NULL
BEGIN
    CREATE DATABASE STERP COLLATE Latin1_General_CI_AS;
END
GO
ALTER DATABASE STERP SET RECOVERY SIMPLE;
ALTER DATABASE STERP SET READ_COMMITTED_SNAPSHOT ON WITH ROLLBACK IMMEDIATE;
ALTER DATABASE STERP SET ALLOW_SNAPSHOT_ISOLATION ON;
GO
