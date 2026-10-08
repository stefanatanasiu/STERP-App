Imports STERP.Common

''' <summary>core.AuditLog enquiry and the client side of core.usp_WriteAudit (COM-013).</summary>
Public Class AuditService
    Inherits ServiceBase

    Public Function Enquire(ByVal entityType As String, ByVal entityKey As String,
                            ByVal fromDate As Date, ByVal toDate As Date,
                            ByVal userCode As String) As DataTable
        Return Query("core.usp_UiAuditEnquiry", "@CompanyCode", Company, "@EntityType", entityType,
                     "@EntityKey", entityKey, "@FromDate", fromDate, "@ToDate", toDate, "@UserCode", userCode)
    End Function

    ''' <summary>Writes a client side audit note; never throws (COM-013 is best effort from the UI).</summary>
    Public Shared Sub Note(ByVal action As String, ByVal entityKey As String, ByVal reason As String)
        Try
            STERP.Data.SqlHelper.ExecuteNonQuery("core.usp_WriteAudit",
                "@UserCode", SterpSession.Current.UserCode,
                "@CompanyCode", SterpSession.Current.CompanyCode,
                "@EntityType", "SESSION", "@EntityKey", entityKey,
                "@Action", action, "@Reason", reason, "@Source", "STERP.exe")
        Catch ex As Exception
            SterpLog.Warn("WriteAudit " & action & ": " & ex.Message)
        End Try
    End Sub

End Class

''' <summary>Approvals inbox - decisions are taken by core.usp_DecideApproval (COM-003).</summary>
Public Class ApprovalService
    Inherits ServiceBase

    Public Function Inbox(ByVal status As String) As DataTable
        Return Query("core.usp_UiApprovalInbox", "@CompanyCode", Company, "@UserCode", User, "@Status", status)
    End Function

    ''' <summary>decision is APPROVED or REJECTED; self-approval is refused by the procedure.</summary>
    Public Function Decide(ByVal approvalId As Integer, ByVal decision As String, ByVal comment As String) As String
        Try
            Exec("core.usp_DecideApproval", "@ApprovalId", approvalId, "@UserCode", User,
                 "@Decision", decision, "@Comment", comment)
            Return ""
        Catch ex As STERP.Data.BusinessRuleException
            Return ex.Message & "  (" & ex.Code & ")"
        End Try
    End Function

End Class

''' <summary>Exception worklist (holds, warnings and errors raised by the engine).</summary>
Public Class ExceptionService
    Inherits ServiceBase

    Public Function Worklist(ByVal status As String, ByVal severity As String, ByVal ownerUser As String) As DataTable
        Return Query("core.usp_UiExceptionList", "@CompanyCode", Company, "@Status", status,
                     "@Severity", severity, "@OwnerUser", ownerUser)
    End Function

    Public Function Resolve(ByVal exceptionId As Integer, ByVal note As String) As String
        Try
            Exec("core.usp_ResolveException", "@ExceptionId", exceptionId, "@UserCode", User, "@ResolutionNote", note)
            Return ""
        Catch ex As STERP.Data.BusinessRuleException
            Return ex.Message & "  (" & ex.Code & ")"
        End Try
    End Function

End Class

''' <summary>Batch job monitor over core.JobRun.</summary>
Public Class JobService
    Inherits ServiceBase

    Public Function Runs(ByVal fromDate As Date, ByVal status As String) As DataTable
        Return Query("core.usp_UiJobList", "@CompanyCode", Company, "@FromDate", fromDate, "@Status", status)
    End Function

End Class
