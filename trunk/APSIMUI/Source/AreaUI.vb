Imports System.Collections
Imports System.Collections.Specialized
Imports System.IO
Imports System.Convert
Imports CSGeneral
Imports VBGeneral

Public Class areaui
    Inherits BaseUI

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        HelpLabel.Text = "This screen shows all components in the paddock. A background bitmap can be added by right clicking above. Components can be double clicked to edit them."
    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents ListView As System.Windows.Forms.ListView
    Friend WithEvents ListViewContextMenu As System.Windows.Forms.ContextMenu
    Friend WithEvents MenuItem1 As System.Windows.Forms.MenuItem
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.ListView = New System.Windows.Forms.ListView
        Me.ColumnHeader1 = New System.Windows.Forms.ColumnHeader
        Me.ListViewContextMenu = New System.Windows.Forms.ContextMenu
        Me.MenuItem1 = New System.Windows.Forms.MenuItem
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.SuspendLayout()
        '
        'ListView
        '
        Me.ListView.Alignment = System.Windows.Forms.ListViewAlignment.Default
        Me.ListView.AllowDrop = True
        Me.ListView.AutoArrange = False
        Me.ListView.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.ListView.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1})
        Me.ListView.ContextMenu = Me.ListViewContextMenu
        Me.ListView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ListView.Location = New System.Drawing.Point(0, 23)
        Me.ListView.MultiSelect = False
        Me.ListView.Name = "ListView"
        Me.ListView.Size = New System.Drawing.Size(940, 522)
        Me.ListView.TabIndex = 0
        '
        'ColumnHeader1
        '
        Me.ColumnHeader1.Width = -2
        '
        'ListViewContextMenu
        '
        Me.ListViewContextMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuItem1})
        '
        'MenuItem1
        '
        Me.MenuItem1.Index = 0
        Me.MenuItem1.Text = "Load picture"
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.DefaultExt = "jpg"
        Me.OpenFileDialog.Filter = """JPG files|*.jpg|BMP files|*.bmp|All files|*.*"
        Me.OpenFileDialog.Title = "Select a picture to load"
        '
        'areaui
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(6, 15)
        Me.ClientSize = New System.Drawing.Size(940, 585)
        Me.Controls.Add(Me.ListView)
        Me.Name = "areaui"
        Me.Controls.SetChildIndex(Me.ListView, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' ----------------------------------
    ' Refresh the listview
    ' ----------------------------------
    Overrides Sub refresh()
        MyBase.Refresh()
        ListView.Clear()
        ListView.LargeImageList = UIManager.LargeImageList

        ' Add an item for all children of this system.
        Dim ChildList As StringCollection = UIManager.GetUserVisibleComponents(MyData)
        Dim ChildName As String
        For Each ChildName In ChildList
            'create new item
            Dim item As New ListViewItem(ChildName, 0)
            item.ImageIndex = UIManager.LargeImageIndex(MyData.Child(ChildName).Type)
            ListView.Items.Add(item)

            ' try and position this new item.
            Dim child As APSIMData = MyData.Child(ChildName)
            Dim x As String = child.Attribute("x")
            Dim y As String = child.Attribute("y")
            If x <> "" And y <> "" Then
                CSGeneral.ListViewAPI.SetItemPosition(ListView, item.Index, Convert.ToInt32(x), Convert.ToInt32(y))
            End If

        Next

        ' Put up a background bitmap on listview.
        Dim BitmapNode As APSIMData = MyData.Child("bitmap")
        If Not IsNothing(BitmapNode) Then
            Dim TempFileName As String = Path.GetTempPath() + "\\apsimui.jpg"
            Dim b As Bitmap = CSUtility.DecodeStringToBitmap(BitmapNode.Value)
            b.Save(TempFileName)
            CSGeneral.ListViewAPI.SetListViewImage(ListView, TempFileName, ImagePosition.TopLeft)
        End If

    End Sub


    ' ---------------------------------------------------------
    ' User has double clicked an item - show user interface
    ' for that item.
    ' ---------------------------------------------------------
    Private Sub ListView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles ListView.DoubleClick
        UIManager.ShowUI(MyData.Child(ListView.SelectedItems.Item(0).Text))
    End Sub


    ' ------------------------------------------------
    ' User has selected an item on the 
    ' context menu.
    ' ------------------------------------------------
    Private Sub MenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuItem1.Click
        If OpenFileDialog.ShowDialog = DialogResult.OK Then
            Dim FileName As String = OpenFileDialog.FileName
            CSGeneral.ListViewAPI.SetListViewImage(ListView, FileName, ImagePosition.TopLeft)

            Dim BitmapNode As APSIMData = MyData.Child("bitmap")
            If IsNothing(BitmapNode) Then
                BitmapNode = New APSIMData("bitmap", "bitmap")
            End If

            Dim b As New Bitmap(FileName)
            BitmapNode.Value = CSUtility.EncodeBitmapToString(b)
            MyData.Add(BitmapNode)

        End If
    End Sub


    ' --------------------------------------------------------
    ' User is trying to initiate a drag - allow drag operation
    ' --------------------------------------------------------
    Private Sub ListView_ItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles ListView.ItemDrag
        ListView.DoDragDrop(ListView.SelectedItems, DragDropEffects.Move)
    End Sub


    ' --------------------------------------------------
    ' User is dragging an item
    ' --------------------------------------------------
    Private Sub ListView_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles ListView.DragEnter
        Dim i As Integer
        For i = 0 To e.Data.GetFormats().Length - 1
            If e.Data.GetFormats()(i).Equals("System.Windows.Forms.ListView+SelectedListViewItemCollection") Then
                'The data from the drag source is moved to the target.
                e.Effect = DragDropEffects.Move
            End If
        Next
    End Sub


    ' -------------------------------------------------
    ' User has dropped selected items.
    ' -------------------------------------------------
    Private Sub ListView_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles ListView.DragDrop
        'Convert the mouse coordinates to client coordinates.
        Dim p As Point = ListView.PointToClient(New Point(e.X, e.Y))

        For Each item As ListViewItem In ListView.SelectedItems
            CSGeneral.ListViewAPI.SetItemPosition(ListView, ListView.SelectedItems.Item(0).Index, p.X, p.Y)
            Dim child As APSIMData = MyData.Child(item.Text)
            child.SetAttribute("x", p.X.ToString)
            child.SetAttribute("y", p.Y.ToString)
        Next
    End Sub
End Class
