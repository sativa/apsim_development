Imports General
Imports System.Xml
Imports System.Collections.Specialized
Imports System.Windows.Forms
Imports System.Drawing

Public Class DataTree
    Inherits System.Windows.Forms.UserControl
    Protected TreeData As APSIMData
    Delegate Sub DataSelectedEventHandler(ByVal sender As Object, ByVal e As APSIMData)
    Public Event DataSelectedEvent As DataSelectedEventHandler

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'UserControl1 overrides dispose to clean up the component list.
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
    Friend WithEvents TreeView As System.Windows.Forms.TreeView
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(DataTree))
        Me.TreeView = New System.Windows.Forms.TreeView
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.SuspendLayout()
        '
        'TreeView
        '
        Me.TreeView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TreeView.HideSelection = False
        Me.TreeView.HotTracking = True
        Me.TreeView.ImageList = Me.ImageList
        Me.TreeView.Location = New System.Drawing.Point(0, 0)
        Me.TreeView.Name = "TreeView"
        Me.TreeView.PathSeparator = "|"
        Me.TreeView.Size = New System.Drawing.Size(344, 488)
        Me.TreeView.TabIndex = 0
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'DataTree
        '
        Me.Controls.Add(Me.TreeView)
        Me.Name = "DataTree"
        Me.Size = New System.Drawing.Size(344, 488)
        Me.ResumeLayout(False)

    End Sub

#End Region
    WriteOnly Property Data() As APSIMData
        Set(ByVal Value As APSIMData)
            Try
                TreeData = Value
                FillTree()
            Catch e As Exception
                MsgBox(e.Message, MsgBoxStyle.Critical, "Error Setting Tree Data")
            End Try
        End Set
    End Property
    Private Sub FillTree()
        Try
            TreeView.Nodes.Clear()

            'Dim newnode As TreeNode = TreeView.Nodes.Add(TreeData.Name)

            BuildTree(TreeData, Nothing)
            TreeView.ExpandAll()

        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building data tree")
        End Try
    End Sub
    Private Sub BuildTree(ByVal Data As APSIMData, ByRef ParentNode As TreeNode)
        Try

            ' NOTE - Path must be fully qualified name 
            Dim ChildList As New StringCollection
            ChildList = Data.ChildList

            Dim item As String
            For Each item In ChildList
                Dim child As APSIMData = Data.Child(item)

                Dim type As String = child.Type
                Dim addthis As Boolean = False
                Dim openindex As Integer
                Dim closedindex As Integer
                Select Case LCase(type)
                    Case "simulation", "simulations"
                        addthis = True
                        openindex = 0
                        closedindex = 1
                    Case "tracker", "sheep", "stock"
                        addthis = True
                        openindex = 2
                        closedindex = 3
                    Case "metfile"
                        addthis = True
                        openindex = 4
                        closedindex = 4
                    Case "summaryfile"
                        addthis = True
                        openindex = 5
                        closedindex = 5
                    Case "outputfile"
                        addthis = True
                        openindex = 6
                        closedindex = 6
                    Case "logic"
                        addthis = True
                        openindex = 7
                        closedindex = 7
                    Case "component"
                        addthis = True
                        openindex = 8
                        closedindex = 8
                    Case "variable"
                        addthis = True
                        openindex = 9
                        closedindex = 9
                    Case "area"
                        addthis = True
                        openindex = 10
                        closedindex = 10
                    Case "crop"
                        addthis = True
                        openindex = 11
                        closedindex = 11
                    Case "soil"
                        addthis = True
                        openindex = 12
                        closedindex = 12

                    Case Else
                        addthis = False
                End Select
                If addthis Then
                    Dim childnode As TreeNode
                    If ParentNode Is Nothing Then
                        childnode = TreeView.Nodes.Add(item)
                    Else
                        childnode = ParentNode.Nodes.Add(item)
                    End If

                    childnode.ImageIndex = closedindex
                    childnode.SelectedImageIndex = openindex

                    BuildTree(child, childnode)

                End If
            Next

        Catch e As Exception
            MsgBox("Error building tree for : " + Data.Name + vbCrLf + vbCrLf + e.Message, MsgBoxStyle.Critical, "Error building simulation tree")
        End Try
    End Sub
    Public Overrides Property AllowDrop() As Boolean
        Get
            Return TreeView.AllowDrop
        End Get
        Set(ByVal Value As Boolean)
            TreeView.AllowDrop = Value
        End Set
    End Property

    Private Sub TreeView_ItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles TreeView.ItemDrag
        Dim datastring As String = TreeData.Child(e.Item.fullpath).XML
        TreeView.DoDragDrop(datastring, DragDropEffects.Copy)

    End Sub

    Private Sub TreeView_AfterSelect(ByVal sender As System.Object, ByVal e As System.Windows.Forms.TreeViewEventArgs) Handles TreeView.AfterSelect
        RaiseEvent DataSelectedEvent(Me, TreeData.FindChild(e.Node.FullPath))
    End Sub
    Public Sub Expand()
        TreeView.ExpandAll()
    End Sub

    Private Sub TreeView_AfterLabelEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles TreeView.AfterLabelEdit
        Dim oldname As String = TreeView.SelectedNode.Text
        Dim newname As String = e.Label

        Dim path As String = TreeView.SelectedNode.FullPath
        If InStr(path, "/") > 0 Then
            path = Mid$(path, InStr(path, "/"))
        End If

        If newname Is Nothing Or Len(newname) = 0 Or e.CancelEdit Then
            e.CancelEdit = True
        Else
            'APSIMFile.RenameComponent(datapath, newname)
            TreeData.FindChild(path).SetAttribute("name", newname)
            'If MainUImanager.RenameComponent(path, newname) = False Then
            'e.CancelEdit = True
            'End If
        End If

    End Sub
    Property LabelEdit() As Boolean
        Get
            Return TreeView.LabelEdit
        End Get
        Set(ByVal Value As Boolean)
            TreeView.LabelEdit = Value
        End Set
    End Property

    Private Sub TreeView_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragEnter
        If AllowDrop = True Then
            e.Effect = DragDropEffects.Copy
        Else
            e.Effect = DragDropEffects.None
        End If
    End Sub

    Private Sub TreeView_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragDrop
        Try
            Dim pt As Point
            Dim DestinationNode As TreeNode
            pt = CType(sender, TreeView).PointToClient(New Point(e.X, e.Y))
            DestinationNode = CType(sender, TreeView).GetNodeAt(pt)
            '        'MsgBox(DestinationNode.Text)
            '        'MsgBox(DestinationNode.FullPath)
            '        'MsgBox(e.Data.GetData(DataFormats.Text))
            Dim NewDataString As String = e.Data.GetData(DataFormats.Text)
            Dim NewData As New APSIMData(NewDataString)
            TreeData.FindChild(DestinationNode.FullPath).Add(NewData)
            FillTree()
            'DestinationNode.Nodes.Add(NewData.n)


            '        UpdateMainForm()

        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error")
        End Try

    End Sub
End Class
