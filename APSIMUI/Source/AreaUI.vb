Imports System.Collections
Imports System.Collections.Specialized
Public Class areaui
    Inherits BaseUI
    '    Private ListView As New ListView
#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

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
    Friend WithEvents LargeImageList As System.Windows.Forms.ImageList
    Friend WithEvents ListView As System.Windows.Forms.ListView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(areaui))
        Me.LargeImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.ListView = New System.Windows.Forms.ListView
        Me.SuspendLayout()
        '
        'LargeImageList
        '
        Me.LargeImageList.ImageSize = New System.Drawing.Size(32, 32)
        Me.LargeImageList.ImageStream = CType(resources.GetObject("LargeImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.LargeImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'ListView
        '
        Me.ListView.AllowDrop = True
        Me.ListView.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ListView.LargeImageList = Me.LargeImageList
        Me.ListView.Location = New System.Drawing.Point(8, 8)
        Me.ListView.Name = "ListView"
        Me.ListView.Size = New System.Drawing.Size(868, 608)
        Me.ListView.TabIndex = 0
        '
        'areaui
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(883, 625)
        Me.Controls.Add(Me.ListView)
        Me.Name = "areaui"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub areaui_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        '        Me.Controls.Add(ListView)
        '       With ListView
        '      .Left = 10
        '     .Top = 10
        '    .Width = ListView.Parent.Width - 20
        '   .Height = ListView.Parent.Height - 20
        '  .Anchor = AnchorStyles.Bottom Or AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
        ' .View = View.LargeIcon
        '.LargeImageList = LargeImageList
        '.AutoArrange = True
        '.Visible = True
        ' .Enabled = True
        'End With
    End Sub
    Overrides Sub refresh()
        MyBase.Refresh()

        ListView.Clear()
        '        Dim AreaList As New StringCollection
        '        APSIMFile.GetChildListByType(DataPath, "area", AreaList)
        '        Dim AreaName As String
        '        For Each AreaName In AreaList
        '        Dim item As New ListViewItem(AreaName, 0)
        '        item.ImageIndex = 0
        '        ListView.Items.Add(item)
        '        Next

        Dim ChildList As New StringCollection
        ChildList = APSIMData.ChildList

        Dim ChildName As String
        For Each ChildName In ChildList
            Dim item As New ListViewItem(ChildName, 0)
            item.ImageIndex = 0
            ListView.Items.Add(item)
        Next

    End Sub


    Private Sub ListView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles ListView.DoubleClick
        Try
            Dim item As ListViewItem
            For Each item In ListView.SelectedItems
                
                'UIManager.ShowUI(APSIMData.Child(item.Text))
            Next
        Catch ex As Exception
            MsgBox("Error loading user interface", MsgBoxStyle.Critical, "Error")
        End Try
    End Sub


    Private Sub ListView_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles ListView.DragDrop
        '        Dim datastring As String = e.Data.GetData(DataFormats.Text)
        '        Dim APSIMdata As New APSIMData
        '        APSIMdata.Data = datastring
        '        Dim caption As String = data.RootPath
        '        Dim item As New ListViewItem(caption, 0)
        '        ListView.Items.Add(item)
        '        UIManager.AddComponent(DataPath, datastring)
    End Sub

    Private Sub ListView_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles ListView.DragEnter
        If (e.Data.GetDataPresent(DataFormats.Text)) Then
            e.Effect = DragDropEffects.Copy

        Else
            e.Effect = DragDropEffects.None
        End If
    End Sub

End Class
