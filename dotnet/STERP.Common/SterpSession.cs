using System;
using System.Collections.Generic;

namespace STERP.Common
{
    /// <summary>One (company, site) pair the signed-on user may work in.</summary>
    public class CompanySite
    {
        public string CompanyCode;
        public string CompanyName;
        public string SiteCode;
        public string SiteName;
        public string BaseCurrency;
        public string Nation;
        public override string ToString()
        {
            return (CompanyCode + "/" + SiteCode).PadRight(16) + " " + CompanyName +
                   (string.IsNullOrEmpty(SiteName) ? "" : " - " + SiteName);
        }
    }

    /// <summary>
    /// Process-wide sign-on context.  Deliberately a shared mutable singleton -
    /// the original VB6 client had a global UserRec and every form read it.
    /// </summary>
    public sealed class SterpSession
    {
        public static SterpSession Current = new SterpSession();

        public string UserCode = "";
        public string DisplayName = "";
        public string WindowsLogin = "";
        public bool IsSysAdm;
        /// <summary>Set when SYSADM is demonstrating the system as somebody else.</summary>
        public string RealUserCode = "";
        public bool IsImpersonating { get { return RealUserCode.Length > 0 && RealUserCode != UserCode; } }

        public string CompanyCode = "";
        public string CompanyName = "";
        public string SiteCode = "";
        public string SiteName = "";
        public string BaseCurrency = "GBP";
        public string Nation = "ENG";

        public DateTime BusinessDate = DateTime.Today;
        public int FiscalYear;
        public int PeriodNo;
        public string PeriodStatus = "";

        public readonly List<CompanySite> Access = new List<CompanySite>();
        /// <summary>Permission answers already obtained from core.usp_CheckPermission.</summary>
        public readonly Dictionary<string, bool> PermissionCache =
            new Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase);

        public bool IsSignedOn { get { return UserCode.Length > 0; } }
        public string PeriodText { get { return Fmt.Period(FiscalYear, PeriodNo); } }

        public string Title
        {
            get
            {
                string t = UserCode;
                if (IsImpersonating) t += " (as " + RealUserCode + ")";
                return t;
            }
        }

        public void SelectCompanySite(CompanySite cs)
        {
            if (cs == null) return;
            CompanyCode = cs.CompanyCode; CompanyName = cs.CompanyName;
            SiteCode = cs.SiteCode; SiteName = cs.SiteName;
            BaseCurrency = string.IsNullOrEmpty(cs.BaseCurrency) ? "GBP" : cs.BaseCurrency;
            Nation = string.IsNullOrEmpty(cs.Nation) ? "ENG" : cs.Nation;
            PermissionCache.Clear();
        }

        public void SignOff()
        {
            Current = new SterpSession();
        }

        /// <summary>Idempotency key for a repeatable instruction (COM-015).</summary>
        public string NewCorrelationId() { return Guid.NewGuid().ToString(); }
    }
}
