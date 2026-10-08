using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using STERP.Common;

namespace STERP.Data
{
    /// <summary>
    /// Stored-procedure data access in the style of the old Microsoft Data Access
    /// Application Block: static Execute* helpers, no connection management in the
    /// callers.  Every overload accepts either a SqlParameter array or loose
    /// name/value pairs - SqlHelper.ExecuteDataTable("core.usp_X", "@UserCode", u).
    /// </summary>
    public static class SqlHelper
    {
        public static string ConnectionString = SterpConfig.SqlConnectionString;
        public static int CommandTimeout = SterpConfig.GetInt("STERP.CommandTimeout", 120);

        /// <summary>Builds parameters from "@Name", value, "@Name2", value2 ... A null value becomes DBNull.</summary>
        public static SqlParameter[] Params(params object[] nameValuePairs)
        {
            List<SqlParameter> list = new List<SqlParameter>();
            if (nameValuePairs != null)
            {
                for (int i = 0; i + 1 < nameValuePairs.Length; i += 2)
                {
                    string n = Convert.ToString(nameValuePairs[i]);
                    if (n.Length == 0) continue;
                    if (n[0] != '@') n = "@" + n;
                    object v = nameValuePairs[i + 1];
                    if (v == null) v = DBNull.Value;
                    else if (v is string && ((string)v).Length == 0) v = DBNull.Value;
                    list.Add(new SqlParameter(n, v));
                }
            }
            return list.ToArray();
        }

        public static SqlConnection OpenConnection()
        {
            SqlConnection cn = new SqlConnection(ConnectionString);
            cn.Open();
            return cn;
        }

        private static SqlCommand Build(SqlConnection cn, string procedure, SqlParameter[] parms)
        {
            SqlCommand cm = new SqlCommand(procedure, cn);
            // a name is a procedure, anything containing a space is ad-hoc SQL text
            cm.CommandType = procedure.Trim().IndexOf(' ') > 0 ? CommandType.Text : CommandType.StoredProcedure;
            cm.CommandTimeout = CommandTimeout;
            if (parms != null)
                foreach (SqlParameter p in parms)
                    if (p != null) cm.Parameters.Add(p);
            return cm;
        }

        private static Exception Translate(SqlException ex, string procedure)
        {
            BusinessRuleException bre = BusinessRuleException.TryTranslate(ex);
            SterpLog.Error("SQL " + procedure + " -> " + ex.Message);
            return (Exception)bre ?? ex;
        }

        // ---------------------------------------------------------------- DataSet
        public static DataSet ExecuteDataSet(string procedure, SqlParameter[] parms)
        {
            using (SqlConnection cn = OpenConnection())
            using (SqlCommand cm = Build(cn, procedure, parms))
            using (SqlDataAdapter da = new SqlDataAdapter(cm))
            {
                DataSet ds = new DataSet();
                ds.Locale = Fmt.UK;
                try { da.Fill(ds); }
                catch (SqlException ex) { throw Translate(ex, procedure); }
                return ds;
            }
        }

        public static DataSet ExecuteDataSet(string procedure, params object[] nameValuePairs)
        {
            return ExecuteDataSet(procedure, Params(nameValuePairs));
        }

        // -------------------------------------------------------------- DataTable
        public static DataTable ExecuteDataTable(string procedure, SqlParameter[] parms)
        {
            DataSet ds = ExecuteDataSet(procedure, parms);
            return ds.Tables.Count > 0 ? ds.Tables[0] : new DataTable();
        }

        public static DataTable ExecuteDataTable(string procedure, params object[] nameValuePairs)
        {
            return ExecuteDataTable(procedure, Params(nameValuePairs));
        }

        /// <summary>First row of the first result set, or null.</summary>
        public static DataRow ExecuteRow(string procedure, params object[] nameValuePairs)
        {
            DataTable t = ExecuteDataTable(procedure, Params(nameValuePairs));
            return t.Rows.Count > 0 ? t.Rows[0] : null;
        }

        // ------------------------------------------------------------- NonQuery
        public static int ExecuteNonQuery(string procedure, SqlParameter[] parms)
        {
            using (SqlConnection cn = OpenConnection())
            using (SqlCommand cm = Build(cn, procedure, parms))
            {
                try { return cm.ExecuteNonQuery(); }
                catch (SqlException ex) { throw Translate(ex, procedure); }
            }
        }

        public static int ExecuteNonQuery(string procedure, params object[] nameValuePairs)
        {
            return ExecuteNonQuery(procedure, Params(nameValuePairs));
        }

        // --------------------------------------------------------------- Scalar
        public static object ExecuteScalar(string procedure, SqlParameter[] parms)
        {
            using (SqlConnection cn = OpenConnection())
            using (SqlCommand cm = Build(cn, procedure, parms))
            {
                try { return cm.ExecuteScalar(); }
                catch (SqlException ex) { throw Translate(ex, procedure); }
            }
        }

        public static object ExecuteScalar(string procedure, params object[] nameValuePairs)
        {
            return ExecuteScalar(procedure, Params(nameValuePairs));
        }

        // --------------------------------------------------------------- Reader
        /// <summary>Caller must Dispose the reader; the connection closes with it.</summary>
        public static SqlDataReader ExecuteReader(string procedure, params object[] nameValuePairs)
        {
            SqlConnection cn = OpenConnection();
            SqlCommand cm = Build(cn, procedure, Params(nameValuePairs));
            try { return cm.ExecuteReader(CommandBehavior.CloseConnection); }
            catch (SqlException ex) { cn.Close(); throw Translate(ex, procedure); }
        }

        /// <summary>True when the object (table, view, procedure) exists - used by the diagnostics form.</summary>
        public static bool ObjectExists(string qualifiedName)
        {
            try
            {
                object o = ExecuteScalar("SELECT OBJECT_ID(@n)", "@n", qualifiedName);
                return o != null && o != DBNull.Value;
            }
            catch (Exception ex) { SterpLog.Warn("ObjectExists " + qualifiedName + ": " + ex.Message); return false; }
        }
    }
}
