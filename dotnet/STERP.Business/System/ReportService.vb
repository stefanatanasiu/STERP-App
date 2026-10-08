Imports System.Diagnostics
Imports System.IO
Imports STERP.Common

''' <summary>Outcome of a batch report run.</summary>
Public Class ReportRun
    Public ReportCode As String = ""
    Public ProgramName As String = ""
    Public ReturnCode As Integer
    Public SpoolFile As String = ""
    Public LogFile As String = ""
    Public Output As String = ""
    Public JobRunId As Integer
    Public ReadOnly Property Succeeded() As Boolean
        Get
            Return ReturnCode <= 4
        End Get
    End Property
End Class

''' <summary>
''' Report launcher: reads core.ReportDef, runs the COBOL batch EXE from bin\x86 with
''' KEY=VALUE arguments, captures the console log, picks up the "STERP-I-SPOOL path"
''' line and registers the run in core.JobRun (CONVENTIONS 3.6).
''' </summary>
Public Class ReportService
    Inherits ServiceBase

    Public Function Definitions(ByVal moduleCode As String) As DataTable
        Dim filter As String = If(moduleCode, "").Trim()
        Dim all As DataTable = Query("core.usp_UiReportList", "@UserCode", User, "@CompanyCode", Company, "@Module", DBNull.Value)
        If filter.Length = 0 Then Return all
        ' a menu row may name one report (FormArgument = ReportCode, e.g. CSTRP010)
        ' instead of a module - the catalogue is then narrowed to that single report
        Dim hit As DataTable = all.Clone()
        For Each r As DataRow In all.Rows
            If String.Equals(Fmt.Str(r("ReportCode")), filter, StringComparison.OrdinalIgnoreCase) Then
                hit.ImportRow(r)
            End If
        Next
        If hit.Rows.Count > 0 Then Return hit
        Dim byModule As DataTable = Query("core.usp_UiReportList", "@UserCode", User, "@CompanyCode", Company, "@Module", filter)
        If byModule IsNot Nothing AndAlso byModule.Rows.Count > 0 Then Return byModule
        Return all
    End Function

    ''' <summary>
    ''' Builds the batch command line: keys upper cased, KEY=VALUE separated by a single
    ''' space, values holding a space wrapped in double quotes, empty values dropped.
    ''' </summary>
    Public Shared Function BuildArguments(ByVal args As Dictionary(Of String, String)) As String
        Dim cmd As New Text.StringBuilder()
        If args Is Nothing Then Return ""
        For Each k As String In args.Keys
            If k Is Nothing OrElse k.Trim().Length = 0 Then Continue For
            Dim v As String = If(args(k), "").Trim()
            If v.Length = 0 Then Continue For
            If cmd.Length > 0 Then cmd.Append(" ")
            cmd.Append(k.Trim().ToUpperInvariant()).Append("=")
            If v.IndexOf(" "c) >= 0 Then cmd.Append(""""c).Append(v).Append(""""c) Else cmd.Append(v)
        Next
        Return cmd.ToString()
    End Function

    ''' <summary>Picks up the last "STERP-I-SPOOL &lt;full path&gt;" line of a batch console log.</summary>
    Public Shared Function ParseSpool(ByVal consoleOutput As String) As String
        Dim found As String = ""
        If consoleOutput Is Nothing Then Return found
        For Each line As String In consoleOutput.Split(ChrW(10))
            Dim t As String = line.Trim()
            If t.Length > 13 AndAlso t.StartsWith("STERP-I-SPOOL", StringComparison.OrdinalIgnoreCase) AndAlso
               Char.IsWhiteSpace(t(13)) Then
                Dim path As String = t.Substring(13).Trim()
                If path.Length > 0 Then found = path
            End If
        Next
        Return found
    End Function

    ''' <summary>
    ''' The JCL style job streams in jobs\*.cmd are catalogued with the reserved
    ''' ReportCode prefix "JOB." (seed 96-xmd-jobs.sql).  They are submitted with
    ''' cmd.exe from the application root, exactly as the operator console does
    ''' in STERPOPS 3350-BUILD-COMMAND.
    ''' </summary>
    Public Shared Function IsJobStream(ByVal reportCode As String) As Boolean
        Return If(reportCode, "").Trim().StartsWith("JOB.", StringComparison.OrdinalIgnoreCase)
    End Function

    Public Shared Function HomeDir() As String
        Return Path.GetFullPath(Path.Combine(SterpConfig.BinDir, "..\.."))
    End Function

    Public Shared Function JobStream(ByVal programName As String) As String
        Return Path.Combine(HomeDir(), "jobs", If(programName, "").Trim() & ".cmd")
    End Function

    Public Function Run(ByVal reportCode As String, ByVal programName As String,
                        ByVal args As Dictionary(Of String, String)) As ReportRun
        Dim rr As New ReportRun()
        rr.ReportCode = reportCode
        rr.ProgramName = programName.Trim().ToUpperInvariant()

        Dim job As Boolean = IsJobStream(reportCode)
        Dim exe As String = If(job, JobStream(rr.ProgramName),
                                    Path.Combine(SterpConfig.BinDir, rr.ProgramName & ".exe"))
        Dim cmd As String = BuildArguments(args)

        rr.JobRunId = JobStart(rr.ProgramName)
        If Not File.Exists(exe) Then
            rr.ReturnCode = 16
            rr.Output = "STERP-E-0016 " & If(job, "Job stream " & rr.ProgramName & ".CMD is not installed in " & Path.Combine(HomeDir(), "jobs"),
                                                  "Batch program " & rr.ProgramName & ".EXE is not installed in " & SterpConfig.BinDir)
            JobEnd(rr, JobStatus(rr.ReturnCode))
            Return rr
        End If

        Dim psi As ProcessStartInfo
        If job Then
            psi = New ProcessStartInfo("cmd.exe", "/c """"" & exe & """ " & cmd & """")
            psi.WorkingDirectory = HomeDir()
        Else
            psi = New ProcessStartInfo(exe, cmd)
            psi.WorkingDirectory = SterpConfig.BinDir
        End If
        psi.UseShellExecute = False
        psi.RedirectStandardOutput = True
        psi.RedirectStandardError = True
        psi.CreateNoWindow = True
        psi.EnvironmentVariables("STERP_RUN") = SterpConfig.RunDir
        psi.EnvironmentVariables("STERP_ODBC") = SterpConfig.OdbcConnectionString

        Dim text As New Text.StringBuilder()
        SterpLog.Info("RUN " & rr.ProgramName & " " & cmd)
        Try
            Using p As Process = Process.Start(psi)
                ' both pipes must be drained while the program runs: reading one to the end
                ' first deadlocks as soon as the other fills its 4K buffer
                AddHandler p.OutputDataReceived,
                    Sub(src As Object, ev As DataReceivedEventArgs)
                        If ev.Data IsNot Nothing Then SyncLock text : text.AppendLine(ev.Data) : End SyncLock
                    End Sub
                AddHandler p.ErrorDataReceived,
                    Sub(src As Object, ev As DataReceivedEventArgs)
                        If ev.Data IsNot Nothing Then SyncLock text : text.AppendLine(ev.Data) : End SyncLock
                    End Sub
                p.BeginOutputReadLine()
                p.BeginErrorReadLine()
                p.WaitForExit()
                rr.ReturnCode = p.ExitCode
            End Using
        Catch ex As Exception
            rr.ReturnCode = 12
            text.AppendLine("STERP-E-0012 " & ex.Message)
        End Try
        rr.Output = text.ToString()
        rr.SpoolFile = ParseSpool(rr.Output)

        Try
            Dim dir As String = Path.Combine(SterpConfig.RunDir, "logs")
            Directory.CreateDirectory(dir)
            rr.LogFile = Path.Combine(dir, rr.ProgramName & "-" & DateTime.Now.ToString("yyyyMMddHHmmss") & ".log")
            File.WriteAllText(rr.LogFile, rr.Output)
        Catch ex As Exception
            SterpLog.Warn("Report log: " & ex.Message)
        End Try

        JobEnd(rr, JobStatus(rr.ReturnCode))
        Return rr
    End Function

    ''' <summary>
    ''' Maps a batch return code to a core.JobRun status.  The column only accepts
    ''' OK, WARNING, FAILED, ABENDED and RUNNING (CK_JobRun_St).
    ''' </summary>
    Public Shared Function JobStatus(ByVal returnCode As Integer) As String
        If returnCode <= 0 Then Return "OK"
        If returnCode <= 4 Then Return "WARNING"
        If returnCode < 16 Then Return "FAILED"
        Return "ABENDED"
    End Function

    Private Function JobStart(ByVal jobName As String) As Integer
        Try
            Dim r As DataRow = QueryRow("core.usp_JobRunStart", "@JobName", jobName,
                                        "@CompanyCode", Company, "@SubmittedBy", User)
            Return Fmt.Int(Col(r, "JobRunId", 0))
        Catch ex As Exception
            SterpLog.Warn("JobRunStart: " & ex.Message)
            Return 0
        End Try
    End Function

    Private Sub JobEnd(ByVal rr As ReportRun, ByVal status As String)
        If rr.JobRunId = 0 Then Return
        Try
            Exec("core.usp_JobRunEnd", "@JobRunId", rr.JobRunId, "@Status", status,
                 "@ReturnCode", rr.ReturnCode, "@LogFile", rr.LogFile, "@SpoolFile", rr.SpoolFile)
        Catch ex As Exception
            SterpLog.Warn("JobRunEnd: " & ex.Message)
        End Try
    End Sub

End Class
