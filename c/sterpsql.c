/*
 * STERPSQL.C - SQLIO low level ODBC interface for STERP COBOL programs
 *
 * Originally written 1999 to replace the ISAM file handler when the
 * ledgers moved to SQL Server 7.0.  Called ONLY from the COBOL module
 * SQLIO (cobol/online/common/SQLIO.cob) - application programs must
 * not call these entry points directly.
 *
 * Change log
 *   1999-03-11  RJH  Initial version (SQL Server 7.0, DSN based)
 *   2003-07-02  KMP  Multiple cursors (8 slots) for nested fetch loops
 *   2009-11-19  KMP  Parameter binding - stop building SQL with STRING
 *   2017-05-30  ANW  DSN-less connection, MARS, ODBC Driver 13
 *   2026-10-04  STERP  ODBC Driver 18, TrustServerCertificate
 *
 * Conventions: every entry point returns an int.  0 = OK, 100 = no
 * (more) data, -1 = error (text available through STSQLERR).
 * All string arguments from COBOL are space padded and NOT null
 * terminated unless documented otherwise; lengths are passed by
 * reference as COMP-5 (int *).
 */
#include <windows.h>
#include <sql.h>
#include <sqlext.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define MAXCUR   8
#define MAXPARM  40
#define PARMLEN  4000
#define COLBUF   8192

static SQLHENV  henv = SQL_NULL_HENV;
static SQLHDBC  hdbc = SQL_NULL_HDBC;
static int      connected = 0;
static SQLHSTMT hstmt[MAXCUR + 1];
static char     parmval[MAXCUR + 1][MAXPARM][PARMLEN + 1];
static SQLLEN   parmind[MAXCUR + 1][MAXPARM];
static char     lasterr[1024] = "";
static char     laststate[6] = "00000";
static char     colbuf[COLBUF];

static const char *DEFAULT_CONN =
    "Driver={ODBC Driver 18 for SQL Server};Server=.;Database=STERP;"
    "Trusted_Connection=yes;TrustServerCertificate=yes;MARS_Connection=yes;"
    "APP=STERP-COBOL";

static void diag(SQLSMALLINT type, SQLHANDLE h)
{
    SQLCHAR state[6], msg[900];
    SQLINTEGER native;
    SQLSMALLINT len;
    lasterr[0] = 0;
    strcpy(laststate, "HY000");
    if (SQL_SUCCEEDED(SQLGetDiagRec(type, h, 1, state, &native, msg, sizeof msg, &len))) {
        memcpy(laststate, state, 5);
        laststate[5] = 0;
        _snprintf(lasterr, sizeof lasterr - 1, "%s", msg);
    }
}

/* copy a space padded COBOL field into a C string, trimming trailing spaces */
static void cobstr(char *dst, const char *src, int len, int max)
{
    int n = len;
    if (n > max) n = max;
    while (n > 0 && (src[n - 1] == ' ' || src[n - 1] == 0)) n--;
    memcpy(dst, src, n);
    dst[n] = 0;
}

static int ensure_conn(void)
{
    const char *cs;
    SQLRETURN r = SQL_ERROR;
    int attempt;
    if (connected) return 0;
    cs = getenv("STERP_ODBC");
    if (cs == NULL || *cs == 0) cs = DEFAULT_CONN;
    SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &henv);
    SQLSetEnvAttr(henv, SQL_ATTR_ODBC_VERSION, (void *)SQL_OV_ODBC3, 0);
    /* 2026-10-06 retry transient login failures (server busy / login delay) */
    for (attempt = 1; attempt <= 4; attempt++) {
        SQLAllocHandle(SQL_HANDLE_DBC, henv, &hdbc);
        SQLSetConnectAttr(hdbc, SQL_ATTR_LOGIN_TIMEOUT, (SQLPOINTER)30, 0);
        r = SQLDriverConnect(hdbc, NULL, (SQLCHAR *)cs, SQL_NTS, NULL, 0, NULL, SQL_DRIVER_NOPROMPT);
        if (SQL_SUCCEEDED(r)) break;
        diag(SQL_HANDLE_DBC, hdbc);
        SQLFreeHandle(SQL_HANDLE_DBC, hdbc);
        hdbc = SQL_NULL_HDBC;
        if (strcmp(laststate, "28000") == 0) break;   /* genuine login refusal - no retry */
        Sleep(2000 * attempt);
    }
    if (!SQL_SUCCEEDED(r)) {
        SQLFreeHandle(SQL_HANDLE_ENV, henv);
        henv = SQL_NULL_HENV;
        return -1;
    }
    connected = 1;
    return 0;
}

static int curok(int *cur)
{
    if (cur == NULL || *cur < 1 || *cur > MAXCUR) {
        strcpy(laststate, "HY024");
        strcpy(lasterr, "SQLIO: cursor number must be 1-8");
        return 0;
    }
    return 1;
}

/* CONNECT using connection text (len) - blank text = environment/default */
int STSQLCON(char *conn, int *len)
{
    char cs[1024];
    if (connected) return 0;
    cobstr(cs, conn, *len, sizeof cs - 1);
    if (cs[0] != 0) {
        char envset[1100];
        _snprintf(envset, sizeof envset - 1, "STERP_ODBC=%s", cs);
        envset[sizeof envset - 1] = 0;
        _putenv(envset);
    }
    return ensure_conn();
}

int STSQLDIS(void)
{
    int i;
    for (i = 1; i <= MAXCUR; i++) {
        if (hstmt[i]) { SQLFreeHandle(SQL_HANDLE_STMT, hstmt[i]); hstmt[i] = NULL; }
    }
    if (connected) {
        SQLDisconnect(hdbc);
        SQLFreeHandle(SQL_HANDLE_DBC, hdbc);
        SQLFreeHandle(SQL_HANDLE_ENV, henv);
        connected = 0;
    }
    return 0;
}

/* PREPARE statement text on cursor; clears previous bindings */
int STSQLPRP(int *cur, char *sql, int *len)
{
    char *txt;
    SQLRETURN r;
    if (!curok(cur)) return -1;
    if (ensure_conn() != 0) return -1;
    if (hstmt[*cur]) { SQLFreeHandle(SQL_HANDLE_STMT, hstmt[*cur]); hstmt[*cur] = NULL; }
    SQLAllocHandle(SQL_HANDLE_STMT, hdbc, &hstmt[*cur]);
    txt = (char *)malloc(*len + 1);
    cobstr(txt, sql, *len, *len);
    r = SQLPrepare(hstmt[*cur], (SQLCHAR *)txt, SQL_NTS);
    free(txt);
    if (!SQL_SUCCEEDED(r)) { diag(SQL_HANDLE_STMT, hstmt[*cur]); return -1; }
    return 0;
}

/* bind parameter idx (1 based) as character data, isnull = 1 for NULL */
int STSQLPRM(int *cur, int *idx, char *val, int *len, int *isnull)
{
    int i;
    SQLRETURN r;
    SQLULEN colsz;
    if (!curok(cur)) return -1;
    i = *idx - 1;
    if (i < 0 || i >= MAXPARM) { strcpy(lasterr, "SQLIO: parameter index out of range"); return -1; }
    cobstr(parmval[*cur][i], val, *len, PARMLEN);
    parmind[*cur][i] = (*isnull) ? SQL_NULL_DATA : SQL_NTS;
    colsz = strlen(parmval[*cur][i]);
    if (colsz == 0) colsz = 1;
    r = SQLBindParameter(hstmt[*cur], (SQLUSMALLINT)(*idx), SQL_PARAM_INPUT, SQL_C_CHAR,
                         SQL_VARCHAR, colsz, 0, parmval[*cur][i], PARMLEN + 1, &parmind[*cur][i]);
    if (!SQL_SUCCEEDED(r)) { diag(SQL_HANDLE_STMT, hstmt[*cur]); return -1; }
    return 0;
}

/* EXECUTE prepared statement; 0 ok, 100 = executed but no rows affected/no result */
int STSQLEXE(int *cur)
{
    SQLRETURN r;
    if (!curok(cur)) return -1;
    r = SQLExecute(hstmt[*cur]);
    if (r == SQL_NO_DATA) return 100;
    if (!SQL_SUCCEEDED(r)) { diag(SQL_HANDLE_STMT, hstmt[*cur]); return -1; }
    return 0;
}

/* FETCH next row; each column copied as text, left justified, space padded
 * into consecutive slots of widths[0..ncols-1].  Columns beyond *ncols ignored. */
int STSQLFET(int *cur, char *buf, int *widths, int *ncols)
{
    SQLSMALLINT nc, c;
    SQLLEN ind;
    SQLRETURN r;
    char *p = buf;
    if (!curok(cur)) return -1;
    /* 2026-10-06 skip row-count-only results (INSERT/UPDATE without SET NOCOUNT ON
       ahead of a SELECT) so the caller lands on the first real result set */
    nc = 0;
    if (hstmt[*cur]) SQLNumResultCols(hstmt[*cur], &nc);
    while (nc == 0) {
        r = SQLMoreResults(hstmt[*cur]);
        if (r == SQL_NO_DATA) return 100;
        if (!SQL_SUCCEEDED(r)) { diag(SQL_HANDLE_STMT, hstmt[*cur]); return -1; }
        SQLNumResultCols(hstmt[*cur], &nc);
    }
    r = SQLFetch(hstmt[*cur]);
    if (r == SQL_NO_DATA) return 100;
    if (!SQL_SUCCEEDED(r)) { diag(SQL_HANDLE_STMT, hstmt[*cur]); return -1; }
    SQLNumResultCols(hstmt[*cur], &nc);
    for (c = 1; c <= nc && c <= *ncols; c++) {
        int w = widths[c - 1];
        memset(p, ' ', w);
        colbuf[0] = 0;
        r = SQLGetData(hstmt[*cur], c, SQL_C_CHAR, colbuf, sizeof colbuf, &ind);
        if (SQL_SUCCEEDED(r) && ind != SQL_NULL_DATA) {
            size_t L = strlen(colbuf);
            if ((int)L > w) L = w;
            memcpy(p, colbuf, L);
        }
        p += w;
    }
    return 0;
}

/* move to the next result set of a batch / stored procedure */
int STSQLMOR(int *cur)
{
    SQLRETURN r;
    if (!curok(cur)) return -1;
    r = SQLMoreResults(hstmt[*cur]);
    if (r == SQL_NO_DATA) return 100;
    if (!SQL_SUCCEEDED(r)) { diag(SQL_HANDLE_STMT, hstmt[*cur]); return -1; }
    return 0;
}

int STSQLROW(int *cur, int *rows)
{
    SQLLEN n = 0;
    if (!curok(cur)) return -1;
    SQLRowCount(hstmt[*cur], &n);
    *rows = (int)n;
    return 0;
}

int STSQLCLS(int *cur)
{
    if (!curok(cur)) return -1;
    if (hstmt[*cur]) SQLFreeStmt(hstmt[*cur], SQL_CLOSE);
    return 0;
}

/* transaction control: 1 = begin (autocommit off), 2 = commit, 3 = rollback */
int STSQLTXN(int *mode)
{
    SQLRETURN r = SQL_SUCCESS;
    if (ensure_conn() != 0) return -1;
    switch (*mode) {
    case 1:
        r = SQLSetConnectAttr(hdbc, SQL_ATTR_AUTOCOMMIT, (SQLPOINTER)SQL_AUTOCOMMIT_OFF, 0);
        break;
    case 2:
        r = SQLEndTran(SQL_HANDLE_DBC, hdbc, SQL_COMMIT);
        SQLSetConnectAttr(hdbc, SQL_ATTR_AUTOCOMMIT, (SQLPOINTER)SQL_AUTOCOMMIT_ON, 0);
        break;
    case 3:
        r = SQLEndTran(SQL_HANDLE_DBC, hdbc, SQL_ROLLBACK);
        SQLSetConnectAttr(hdbc, SQL_ATTR_AUTOCOMMIT, (SQLPOINTER)SQL_AUTOCOMMIT_ON, 0);
        break;
    default:
        strcpy(lasterr, "SQLIO: invalid transaction mode");
        return -1;
    }
    if (!SQL_SUCCEEDED(r)) { diag(SQL_HANDLE_DBC, hdbc); return -1; }
    return 0;
}

/* last error: state (5 bytes) and message (space padded to *len) */
int STSQLERR(char *state, char *msg, int *len)
{
    size_t L = strlen(lasterr);
    memcpy(state, laststate, 5);
    memset(msg, ' ', *len);
    if ((int)L > *len) L = *len;
    memcpy(msg, lasterr, L);
    return 0;
}
