Imports System.Threading
Imports STERP.Business
Imports STERP.Common
Imports STERP.CobolInterop

''' <summary>
''' Entry point of STERP.EXE.  Application.EnableVisualStyles is deliberately NOT
''' called: the client must look like the Windows Classic application it has always
''' been (COM-024).
''' </summary>
Public NotInheritable Class Program

    <STAThread()>
    Public Shared Sub Main(ByVal args As String())
        Application.SetCompatibleTextRenderingDefault(False)
        ToolStripManager.VisualStylesEnabled = False
        ToolStripManager.Renderer = New ClassicRenderer()
        AddHandler Application.ThreadException, AddressOf OnThreadException
        AddHandler AppDomain.CurrentDomain.UnhandledException, AddressOf OnFatal
        SterpLog.Info("STERP client starting, " & SterpConfig.BinDir)

        ' undocumented QA sweep used by build\ui-smoke.ps1 - never reaches an operator
        If args IsNot Nothing AndAlso args.Length > 0 AndAlso
           String.Equals(args(0).TrimStart("-"c, "/"c), "qa-screens", StringComparison.OrdinalIgnoreCase) Then
            Try
                CobolGateway.Initialise()
            Catch ex As Exception
                SterpLog.Warn("QA sweep without the engine: " & ex.Message)
            End Try
            Environment.ExitCode = QaScreens.Run(If(args.Length > 1, args(1), IO.Path.Combine(SterpConfig.BinDir, "qa-screens")),
                                                 If(args.Length > 2, args(2), ""))
            Return
        End If

        Dim splash As New frmSplash()
        splash.Show()
        splash.ShowStep("Starting transaction engine...")
        Try
            CobolGateway.Initialise()
            splash.ShowStep("Engine directory: " & CobolGateway.ProgramCount() & " programs")
        Catch ex As Exception
            SterpLog.Error("Engine start-up", ex)
            splash.ShowStep("Engine unavailable - running without the engine")
        End Try
        splash.ShowStep("Connecting to the STERP database...")
        Thread.Sleep(400)
        splash.Close()

        Using login As New frmLogin()
            If login.ShowDialog() <> DialogResult.OK Then
                SterpLog.Info("Sign-on cancelled")
                Return
            End If
        End Using
        Application.Run(New frmMain())
        SterpLog.Info("STERP client closed")
    End Sub

    Private Shared Sub OnThreadException(ByVal sender As Object, ByVal e As ThreadExceptionEventArgs)
        SterpLog.Error("Unhandled", e.Exception)
        MessageBox.Show("STERP-E-9999: An unexpected error has occurred." & vbCrLf & vbCrLf &
                        e.Exception.Message & vbCrLf & vbCrLf &
                        "The event has been written to " & SterpLog.LogFile,
                        "STERP - Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
    End Sub

    Private Shared Sub OnFatal(ByVal sender As Object, ByVal e As UnhandledExceptionEventArgs)
        SterpLog.Error("Fatal", TryCast(e.ExceptionObject, Exception))
    End Sub

End Class
