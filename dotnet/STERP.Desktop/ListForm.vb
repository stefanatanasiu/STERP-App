Imports STERP.Common

''' <summary>
''' Enquiry window pattern used by the worklists: a toolbar, a row of filters and a
''' classic grid.  A derived form adds its filters in BuildFilters() and fills the
''' grid in LoadData().
''' </summary>
Public Class ListForm
    Inherits BaseForm

    Protected WithEvents Grid As DataGridView
    Protected Tools As ToolStrip
    Protected Filters As Panel
    Private _x As Integer = 6
    Private _y As Integer = 4
    Private Const FilterPitch As Integer = 26

    Public Sub New()
        Me.ClientSize = New Size(760, 420)
        Grid = New DataGridView() With {.Dock = DockStyle.Fill}
        GridHelper.Style(Grid)
        Filters = New Panel() With {.Dock = DockStyle.Top, .Height = 28}
        Tools = New ToolStrip() With {.Dock = DockStyle.Top, .RenderMode = ToolStripRenderMode.System,
                                      .GripStyle = ToolStripGripStyle.Hidden}
        Me.Controls.Add(Grid)
        Me.Controls.Add(Filters)
        Me.Controls.Add(Tools)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        BuildFilters()
        If Filters.Controls.Count = 0 Then Filters.Visible = False
        AddTool("Refresh (F5)", "REFRESH", AddressOf RefreshClicked)
        BuildTools()
        AddTool("Close (F3)", "CLOSE", AddressOf CloseClicked)
        LoadData()
    End Sub

    ''' <summary>Derived forms add their filter controls here.</summary>
    Protected Overridable Sub BuildFilters()
    End Sub

    ''' <summary>Derived forms add their action buttons here (between Refresh and Close).</summary>
    Protected Overridable Sub BuildTools()
    End Sub

    Protected Overridable Sub LoadData()
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadData()
    End Sub

    ' ------------------------------------------------------------ filter factory
    ''' <summary>
    ''' Starts a new filter line when the next pair would hang off the right hand edge.
    ''' Classic screens wrapped their selection criteria; they never pushed a field out
    ''' of sight (COM-024).
    ''' </summary>
    Private Sub Reserve(ByVal needed As Integer)
        Dim room As Integer = Filters.ClientSize.Width - 8
        If room < 120 Then room = Me.ClientSize.Width - 8
        If _x > 6 AndAlso _x + needed > room Then
            _x = 6
            _y += FilterPitch
            Filters.Height = _y + 24
        End If
    End Sub

    Protected Function FilterEdit(ByVal caption As String, ByVal width As Integer) As TextBox
        Dim capW As Integer = caption.Length * 6 + 6
        Reserve(capW + 8 + width)
        UiStyle.Caption(Filters, caption, _x, _y, capW)
        _x += capW + 8
        Dim t As TextBox = UiStyle.Edit(Filters, _x, _y - 1, width)
        _x += width + 12
        Return t
    End Function

    Protected Function FilterList(ByVal caption As String, ByVal width As Integer,
                                  ByVal ParamArray items() As String) As ComboBox
        Dim capW As Integer = caption.Length * 6 + 6
        Reserve(capW + 8 + width)
        UiStyle.Caption(Filters, caption, _x, _y, capW)
        _x += capW + 8
        Dim c As ComboBox = UiStyle.Picker(Filters, _x, _y - 2, width)
        c.Items.AddRange(items)
        If c.Items.Count > 0 Then c.SelectedIndex = 0
        _x += width + 12
        Return c
    End Function

    Protected Function AddTool(ByVal text As String, ByVal glyph As String, ByVal handler As EventHandler) As ToolStripButton
        Return UiStyle.Tool(Tools, text, glyph, handler)
    End Function

    Protected Sub AddSeparator()
        Tools.Items.Add(New ToolStripSeparator())
    End Sub

    Private Sub RefreshClicked(ByVal sender As Object, ByVal e As EventArgs)
        LoadData()
    End Sub

    Private Sub CloseClicked(ByVal sender As Object, ByVal e As EventArgs)
        CloseForm()
    End Sub

    ''' <summary>Binds the grid and reports the row count on the status line.</summary>
    Protected Sub Populate(ByVal data As DataTable)
        GridHelper.Bind(Grid, data)
        GridHelper.FillWidth(Grid)
        SetStatus(If(data Is Nothing, 0, data.Rows.Count) & " record(s)", "")
    End Sub

End Class
