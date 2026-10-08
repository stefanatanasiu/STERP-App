using System;

namespace STERP.CobolInterop
{
    /// <summary>
    /// Outcome of a LINK, taken from the STCAHDR header.  Return codes follow the old
    /// CICS/batch convention: 00 OK, 04 warning, 06 hold, 08 error, 12 severe, 16 not found.
    /// </summary>
    public class CobolResult
    {
        public string Program = "";
        public int ReturnCode;
        public string ReasonCode = "";
        public string Message = "";
        public int MessageCount;
        public string CorrelationId = "";
        public CommArea Area;

        public bool IsOk { get { return ReturnCode == 0; } }
        public bool IsWarning { get { return ReturnCode == 4; } }
        public bool IsHold { get { return ReturnCode == 6; } }
        public bool IsNotFound { get { return ReturnCode == 16; } }
        public bool IsError { get { return ReturnCode >= 8; } }
        public bool IsSevere { get { return ReturnCode == 12; } }
        /// <summary>True when the engine accepted the request (00 or 04).</summary>
        public bool Succeeded { get { return ReturnCode <= 4; } }

        public string Severity
        {
            get
            {
                switch (ReturnCode)
                {
                    case 0: return "OK";
                    case 4: return "WARNING";
                    case 6: return "HOLD";
                    case 8: return "ERROR";
                    case 12: return "SEVERE";
                    case 16: return "NOT FOUND";
                    default: return "RC" + ReturnCode.ToString("00");
                }
            }
        }

        /// <summary>Message as shown to the user, with the reason code in brackets (COM-017).</summary>
        public string Display
        {
            get
            {
                string m = (Message ?? "").Trim();
                if (m.Length == 0) m = Severity;
                return ReasonCode.Trim().Length > 0 ? m + "  (" + ReasonCode.Trim() + ")" : m;
            }
        }

        public override string ToString()
        {
            return Program + " RC=" + ReturnCode.ToString("00") + " " + ReasonCode + " " + Message;
        }
    }
}
