Imports STERP.Common

''' <summary>
''' Classic DataGridView styling and binding helpers - flat grey headers, 17 pixel
''' rows, no row selector bitmaps, UK formatting by column data type (COM-024).
''' </summary>
Public NotInheritable Class GridHelper

    Public Shared Sub Style(ByVal g As DataGridView)
        g.BackgroundColor = UiStyle.Face
        g.BorderStyle = BorderStyle.Fixed3D
        g.CellBorderStyle = DataGridViewCellBorderStyle.SingleHorizontal
        g.ColumnHeadersBorderStyle = DataGridViewHeaderBorderStyle.Raised
        g.EnableHeadersVisualStyles = False
        g.ColumnHeadersDefaultCellStyle.BackColor = UiStyle.Face
        g.ColumnHeadersDefaultCellStyle.ForeColor = UiStyle.FaceText
        g.ColumnHeadersDefaultCellStyle.SelectionBackColor = UiStyle.Face
        g.ColumnHeadersDefaultCellStyle.SelectionForeColor = UiStyle.FaceText
        g.ColumnHeadersDefaultCellStyle.Font = UiStyle.BodyFont
        g.ColumnHeadersHeight = 19
        g.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.DisableResizing
        g.RowTemplate.Height = UiStyle.RowHeight
        g.RowHeadersWidth = 18
        g.RowHeadersVisible = True
        g.RowHeadersDefaultCellStyle.BackColor = UiStyle.Face
        g.RowHeadersDefaultCellStyle.SelectionBackColor = UiStyle.Face
        g.AllowUserToAddRows = False
        g.AllowUserToDeleteRows = False
        g.AllowUserToResizeRows = False
        g.ReadOnly = True
        g.MultiSelect = False
        g.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        g.DefaultCellStyle.Font = UiStyle.BodyFont
        g.DefaultCellStyle.BackColor = UiStyle.WindowFace
        g.DefaultCellStyle.ForeColor = UiStyle.FaceText
        g.DefaultCellStyle.SelectionBackColor = UiStyle.Chosen
        g.DefaultCellStyle.SelectionForeColor = UiStyle.ChosenText
        g.AlternatingRowsDefaultCellStyle.BackColor = UiStyle.WindowFace
        g.GridColor = UiStyle.Shadow
        g.StandardTab = True
        AddHandler g.Paint, AddressOf PaintEmptyNotice
    End Sub

    ''' <summary>
    ''' A classic list always says so when it holds nothing - an empty grey rectangle
    ''' gives the operator no cue at all, and COM-024 forbids cues that rely on colour.
    ''' Set the grid's Tag to override the wording.
    ''' </summary>
    Private Shared Sub PaintEmptyNotice(ByVal sender As Object, ByVal e As PaintEventArgs)
        Dim g As DataGridView = DirectCast(sender, DataGridView)
        If g.RowCount > 0 Then Return
        Dim text As String = Convert.ToString(g.Tag)
        If text Is Nothing OrElse text.Trim().Length = 0 OrElse text.StartsWith("$") Then
            text = "(no records - press F5 to refresh)"
        End If
        Dim top As Integer = If(g.ColumnHeadersVisible AndAlso g.Columns.Count > 0, g.ColumnHeadersHeight, 0)
        Dim area As New Rectangle(0, top, g.ClientSize.Width, Math.Max(g.ClientSize.Height - top, 16))
        Using back As New SolidBrush(UiStyle.WindowFace)
            e.Graphics.FillRectangle(back, area)
        End Using
        ControlPaint.DrawBorder3D(e.Graphics, area, Border3DStyle.Flat, Border3DSide.Top)
        Using fore As New SolidBrush(UiStyle.Shadow)
            Using fmt As New StringFormat()
                fmt.Alignment = StringAlignment.Center
                fmt.LineAlignment = StringAlignment.Center
                e.Graphics.DrawString(text, UiStyle.BodyFont, fore, area, fmt)
            End Using
        End Using
    End Sub

    ''' <summary>Binds a table and applies captions, widths and UK formats.</summary>
    Public Shared Sub Bind(ByVal g As DataGridView, ByVal data As DataTable)
        g.DataSource = Nothing
        g.Columns.Clear()
        g.AutoGenerateColumns = True
        g.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.DisplayedCells
        g.DataSource = data
        If data Is Nothing Then Return
        Dim last As DataGridViewColumn = Nothing
        For Each c As DataGridViewColumn In g.Columns
            c.SortMode = DataGridViewColumnSortMode.Automatic
            c.HeaderText = Humanise(c.DataPropertyName)
            FormatColumn(c, GuessType(data.Columns(c.DataPropertyName)))
            last = c
        Next
        If last IsNot Nothing Then last.AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
    End Sub

    ''' <summary>CustomerCode -> "Customer Code", AmountGBP -> "Amount GBP".</summary>
    Public Shared Function Humanise(ByVal columnName As String) As String
        If String.IsNullOrEmpty(columnName) Then Return ""
        Dim sb As New Text.StringBuilder()
        For i As Integer = 0 To columnName.Length - 1
            Dim c As Char = columnName(i)
            If i > 0 AndAlso Char.IsUpper(c) AndAlso
               (Not Char.IsUpper(columnName(i - 1)) OrElse
                (i + 1 < columnName.Length AndAlso Char.IsLower(columnName(i + 1)))) Then sb.Append(" "c)
            sb.Append(c)
        Next
        Return sb.ToString()
    End Function

    ''' <summary>Stretches the last visible column so the list always fills its window.</summary>
    Public Shared Sub FillWidth(ByVal g As DataGridView)
        Dim last As DataGridViewColumn = Nothing
        For Each c As DataGridViewColumn In g.Columns
            If c.Visible Then last = c
        Next
        If last IsNot Nothing Then last.AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
    End Sub

    ''' <summary>Applies the caption, width and format registered in core.MaintColumn.</summary>
    Public Shared Sub Decorate(ByVal g As DataGridView, ByVal columnName As String,
                               ByVal caption As String, ByVal dataType As String, ByVal width As Integer)
        If Not g.Columns.Contains(columnName) Then Return
        Dim c As DataGridViewColumn = g.Columns(columnName)
        c.HeaderText = caption
        If width > 0 Then
            c.AutoSizeMode = DataGridViewAutoSizeColumnMode.None
            c.Width = width
        End If
        FormatColumn(c, dataType)
    End Sub

    Public Shared Sub FormatColumn(ByVal c As DataGridViewColumn, ByVal dataType As String)
        Select Case UCase(dataType)
            Case "MONEY"
                c.DefaultCellStyle.Format = "#,##0.00"
                c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            Case "QTY"
                c.DefaultCellStyle.Format = "#,##0.000000"
                c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            Case "RATE"
                c.DefaultCellStyle.Format = "#,##0.00000000"
                c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            Case "PCT"
                c.DefaultCellStyle.Format = "#,##0.00##"
                c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            Case "INT"
                c.DefaultCellStyle.Format = "#,##0"
                c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            Case "DATE"
                c.DefaultCellStyle.Format = "dd/MM/yyyy"
                c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
            Case "STAMP"
                c.DefaultCellStyle.Format = "dd/MM/yyyy HH:mm:ss"
            Case "BOOL"
                c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
        End Select
        c.DefaultCellStyle.FormatProvider = Fmt.UK
    End Sub

    Private Shared Function GuessType(ByVal c As DataColumn) As String
        If c Is Nothing Then Return "TEXT"
        If c.DataType Is GetType(Date) Then
            Return If(c.ColumnName.EndsWith("At") OrElse c.ColumnName.EndsWith("Time"), "STAMP", "DATE")
        End If
        If c.DataType Is GetType(Decimal) Then
            If c.ColumnName.EndsWith("Qty") OrElse c.ColumnName.StartsWith("Qty") Then Return "QTY"
            Return "MONEY"
        End If
        If c.DataType Is GetType(Integer) OrElse c.DataType Is GetType(Long) Then Return "INT"
        If c.DataType Is GetType(Boolean) Then Return "BOOL"
        Return "TEXT"
    End Function

    ''' <summary>Value of a column in the selected row (Nothing when no row is current).</summary>
    Public Shared Function Selected(ByVal g As DataGridView, ByVal columnName As String) As Object
        If g.CurrentRow Is Nothing Then Return Nothing
        Dim v As DataRowView = TryCast(g.CurrentRow.DataBoundItem, DataRowView)
        If v Is Nothing OrElse Not v.Row.Table.Columns.Contains(columnName) Then Return Nothing
        If IsDBNull(v.Row(columnName)) Then Return Nothing
        Return v.Row(columnName)
    End Function

    Public Shared Function SelectedRow(ByVal g As DataGridView) As DataRow
        If g.CurrentRow Is Nothing Then Return Nothing
        Dim v As DataRowView = TryCast(g.CurrentRow.DataBoundItem, DataRowView)
        Return If(v Is Nothing, Nothing, v.Row)
    End Function

End Class
