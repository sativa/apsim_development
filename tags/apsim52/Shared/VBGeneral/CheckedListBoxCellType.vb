Imports System.Windows.Forms
Imports System.Drawing

Public Class CheckedListBoxCellType
    Inherits FarPoint.Win.Spread.CellType.GeneralCellType

    Dim list As New CheckedListBox

    Public Sub New()
        list.DrawMode = DrawMode.OwnerDrawFixed
        list.ScrollAlwaysVisible = True
        list.ItemHeight = 20
        AddHandler list.DrawItem, AddressOf Me.listDrawitem
    End Sub
    Private Sub listDrawitem(ByVal sender As Object, ByVal e As DrawItemEventArgs)
        DrawListBoxItem(CType(sender, ListBox), e)
    End Sub
    Public Overrides Function GetEditorControl(ByVal appearance As FarPoint.Win.Spread.Appearance, ByVal zoomFactor As Single) As System.Windows.Forms.Control
        Return list
    End Function
    Public Overrides Sub PaintCell(ByVal g As System.Drawing.Graphics, ByVal r As System.Drawing.Rectangle, ByVal appearance As FarPoint.Win.Spread.Appearance, ByVal value As Object, ByVal isSelected As Boolean, ByVal isLocked As Boolean, ByVal zoomFactor As Single)
        Dim brushBackground As New SolidBrush(appearance.BackColor)
        g.FillRectangle(brushBackground, r)
        Dim brushText As New SolidBrush(appearance.ForeColor)
        If Not IsNothing(value) Then
            g.DrawString(value.ToString(), appearance.Font, brushText, r.X, r.Y)
        End If
    End Sub
    Public Sub DrawListBoxItem(ByVal list As ListBox, ByVal e As DrawItemEventArgs)
        Dim brshBackgroundBrush As Brush
        Dim brshTextBrush As Brush
        Dim fntTextFont As Font
        Dim strText As String
        Dim objStringFormat As StringFormat
        e.DrawBackground()
        strText = list.Items(e.Index).ToString()
        If (e.State And DrawItemState.Selected) = DrawItemState.Selected Then
            brshBackgroundBrush = New SolidBrush(System.Drawing.SystemColors.Highlight)
            brshTextBrush = New SolidBrush(Color.White)
        Else
            brshBackgroundBrush = New SolidBrush(Color.White)
            brshTextBrush = New SolidBrush(Color.Black)
        End If
        fntTextFont = list.Font
        e.Graphics.FillRectangle(brshBackgroundBrush, e.Bounds)
        objStringFormat = New StringFormat
        objStringFormat.Alignment = StringAlignment.Near
        objStringFormat.LineAlignment = StringAlignment.Center
        e.Graphics.DrawString(strText, fntTextFont, brshTextBrush, New RectangleF(e.Bounds.X, e.Bounds.Y, e.Graphics.MeasureString(strText, fntTextFont).Width, e.Bounds.Height), objStringFormat)
        e.DrawFocusRectangle()
    End Sub

    Public Property Items() As String()
        Get
            Dim ReturnItems(list.Items.Count - 1) As String
            For i As Integer = 0 To list.Items.Count - 1
                ReturnItems(i) = list.Items(i).ToString
            Next
            Return ReturnItems
        End Get
        Set(ByVal values As String())
            list.Items.Clear()
            For Each value As String In values
                list.Items.Add(value)
            Next
        End Set
    End Property
    Public Overrides Sub SetEditorValue(ByVal value As Object)
        Dim SelectedStrings() As String = value.ToString().Split(vbCrLf.ToCharArray())
        For i As Integer = 0 To list.Items.Count - 1
            Dim IsSelected As Boolean = Array.IndexOf(SelectedStrings, list.Items(i)) <> -1
            list.SetItemChecked(i, IsSelected)
        Next
    End Sub
    Public Overrides Function GetEditorValue() As Object
        Dim St As String = ""
        For Each CheckedItem As Object In list.CheckedItems
            If St <> "" Then
                St += vbCrLf
            End If
            St += CheckedItem.ToString
        Next
        Return St
    End Function
End Class
