Imports STERP.Common
Imports STERP.CobolInterop

''' <summary>Engine health check - LINK to PING and read the directory (COM-011).</summary>
Public Class EngineService
    Inherits ServiceBase

    Public EngineVersion As String = ""
    Public DatabaseName As String = ""
    Public ServerTime As String = ""
    Public DatabaseVersion As String = ""

    ''' <summary>Round trips a COMMAREA through PING; returns the result for display.</summary>
    Public Function Ping() As CobolResult
        Dim ca As CommArea = NewArea("PINGCA")
        ca.SetDecimal("PG-ECHO-AMOUNT", 1234.56D)
        ca.SetDecimal("PG-ECHO-QTY(1)", 1.5D)
        ca.SetDecimal("PG-ECHO-QTY(2)", -2.25D)
        ca.SetDecimal("PG-ECHO-QTY(3)", 10D)
        ca.SetString("PG-ECHO-CODE(1)", "EA")
        ca.SetString("PG-ECHO-CODE(2)", "KG")
        ca.SetString("PG-ECHO-CODE(3)", "BOX")
        Dim r As CobolResult = Link("PING", ca, "HEALTH")
        EngineVersion = ca.GetString("PG-ENGINE-VERSION")
        DatabaseName = ca.GetString("PG-DB-NAME")
        ServerTime = ca.GetString("PG-DB-SERVER-TIME")
        DatabaseVersion = ca.GetString("PG-DB-VERSION")
        Return r
    End Function

    ''' <summary>Short status for the status bar: ONLINE / DEGRADED / DOWN.</summary>
    Public Function Status() As String
        Try
            Dim r As CobolResult = Ping()
            If r.IsOk AndAlso DatabaseName.Length > 0 Then Return "ONLINE"
            If r.Succeeded Then Return "DEGRADED"
            Return "DOWN"
        Catch ex As Exception
            SterpLog.Warn("Engine ping: " & ex.Message)
            Return "DOWN"
        End Try
    End Function

    Public Function ProgramNames() As String()
        Return CobolGateway.ProgramNames()
    End Function

    Public Function ProgramCount() As Integer
        Return CobolGateway.ProgramCount()
    End Function

    ''' <summary>COMMAREA mapping test used by the diagnostics form.</summary>
    Public Function EchoTest(ByVal amount As Decimal, ByRef dump As String) As CobolResult
        Dim ca As CommArea = NewArea("PINGCA")
        ca.SetDecimal("PG-ECHO-AMOUNT", amount)
        ca.SetDecimal("PG-ECHO-QTY(1)", amount)
        ca.SetDecimal("PG-ECHO-QTY(2)", -amount)
        ca.SetDecimal("PG-ECHO-QTY(3)", 0D)
        Dim r As CobolResult = Link("PING", ca, "ECHO")
        dump = ca.HexDump()
        Return r
    End Function

    Public Function Layout(ByVal copybook As String) As String
        Return CopybookParser.Load(copybook).Map()
    End Function

End Class
