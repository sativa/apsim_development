Imports VBGeneral
Imports System.Xml
Imports System.Collections.Specialized
Imports System.Windows.Forms
Imports System.Drawing

' ------------------------------------------
' Data control for displaying a tree based
' on an APSIMData instance.
' ------------------------------------------
Public Class DataTree
    Inherits BaseDataControl

    Delegate Sub DataSelectedEventHandler(ByVal e As APSIMData)
    Delegate Sub BeforeDataSelectedEventHandler()
    Delegate Sub DoubleClickHandler()
    Public Event DataSelectedEvent As DataSelectedEventHandler
    Public Event BeforeDataSelectedEvent As BeforeDataSelectedEventHandler
    Public Event DoubleClickEvent As DoubleClickHandler
    Private MaxNumLevels As Integer = 100
    Private ShowAllComponents As Boolean = False
    Private ExpandAllNodes As Boolean = True


#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

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
    Friend WithEvents AddFolderMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents DeleteItemMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents ContextMenu1 As System.Windows.Forms.ContextMenu
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.TreeView = New System.Windows.Forms.TreeView
        Me.ContextMenu1 = New System.Windows.Forms.ContextMenu
        Me.AddFolderMenuItem = New System.Windows.Forms.MenuItem
        Me.DeleteItemMenuItem = New System.Windows.Forms.MenuItem
        Me.SuspendLayout()
        '
        'TreeView
        '
        Me.TreeView.AllowDrop = True
        Me.TreeView.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.TreeView.ContextMenu = Me.ContextMenu1
        Me.TreeView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TreeView.HideSelection = False
        Me.TreeView.HotTracking = True
        Me.TreeView.ImageIndex = -1
        Me.TreeView.Location = New System.Drawing.Point(0, 20)
        Me.TreeView.Name = "TreeView"
        Me.TreeView.PathSeparator = "|"
        Me.TreeView.SelectedImageIndex = -1
        Me.TreeView.Size = New System.Drawing.Size(336, 468)
        Me.TreeView.TabIndex = 0
        '
        'ContextMenu1
        '
        Me.ContextMenu1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.AddFolderMenuItem, Me.DeleteItemMenuItem})
        '
        'AddFolderMenuItem
        '
        Me.AddFolderMenuItem.Index = 0
        Me.AddFolderMenuItem.Text = "&Add Folder"
        '
        'DeleteItemMenuItem
        '
        Me.DeleteItemMenuItem.Index = 1
        Me.DeleteItemMenuItem.Text = "Delete Item"
        '
        'DataTree
        '
        Me.AllowDrop = True
        Me.Controls.Add(Me.TreeView)
        Me.Name = "DataTree"
        Me.Size = New System.Drawing.Size(336, 488)
        Me.Controls.SetChildIndex(Me.TreeView, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' ------------------------------------------------------
    ' Set the visible components context string property
    ' ------------------------------------------------------
    WriteOnly Property MaximumNumLevels() As Integer
        Set(ByVal Value As Integer)
            MaxNumLevels = Value
        End Set
    End Property


    ' ------------------------------------------------------
    ' Set the showall property
    ' ------------------------------------------------------
    WriteOnly Property ShowAll() As Boolean
        Set(ByVal Value As Boolean)
            ShowAllComponents = Value
        End Set
    End Property


    ' ------------------------------------------------------
    ' Set the showall property
    ' ------------------------------------------------------
    WriteOnly Property ExpandAll() As Boolean
        Set(ByVal Value As Boolean)
            ExpandAllNodes = Value
        End Set
    End Property


    ' ----------------------------------------------
    ' Override the base fill method and populate
    ' ourselves.
    ' ----------------------------------------------
    Overrides Sub Refresh()
        If Not IsNothing(ApplicationSettings) And Not IsNothing(Data) Then
            CaptionLabel.Text = Data.Name
            TreeView.Nodes.Clear()
            AddNode(Data, Nothing)
            Dim RootNode As TreeNode = TreeView.Nodes(0)
            PopulateTree(Data, RootNode)
            If ExpandAllNodes Then
                TreeView.ExpandAll()
            End If
            RootNode.Expand()
        End If
    End Sub


    ' -----------------------------------------------
    ' Get an APSIM data for the specified full path
    ' which is delimited by '|' characters.
    ' -----------------------------------------------
    Public Function GetDataForFullPath(ByVal FullPath As String) As APSIMData
        Dim PosDelimiter As Integer = FullPath.IndexOf("|")
        If PosDelimiter = -1 Then
            If FullPath = Data.Name Then
                Return Data
            Else
                Throw New System.Exception("Cannot find parent name in GetDataForFullPath. Invalid FullPath: " + FullPath)
            End If
        Else
            Return Data.FindChild(FullPath.Substring(PosDelimiter + 1))
        End If
    End Function


    ' ---------------------------------------
    ' Return data of currently selected node
    ' ---------------------------------------
    Public Function SelectedNode() As APSIMData
        If IsNothing(TreeView.SelectedNode) Then
            Return Nothing
        Else
            Return GetDataForFullPath(TreeView.SelectedNode.FullPath)
        End If
    End Function


    ' ----------------------------------------------
    ' Populate the tree using the specified data.
    ' ParentNode can be nothing
    ' ----------------------------------------------
    Private Sub PopulateTree(ByVal Data As APSIMData, ByRef ParentNode As TreeNode)
        TreeView.ImageList = ApplicationSettings.SmallImageList

        ' Display a wait cursor while the TreeNodes are being created.
        Cursor.Current = Cursors.WaitCursor

        ' Suppress repainting the TreeView until all the objects have been created.
        TreeView.BeginUpdate()

        DisplayNode(Data, ParentNode, 0)

        ' Begin repainting the TreeView.
        TreeView.EndUpdate()

        ' Display a wait cursor while the TreeNodes are being created.
        Cursor.Current = Cursors.Default

    End Sub

    ' ---------------------------------------
    ' Recursively display a node and its
    ' child nodes in the tree.
    ' ---------------------------------------
    Private Sub DisplayNode(ByVal Data As APSIMData, ByRef ParentNode As TreeNode, ByRef NumLevels As Integer)
        Try
            For Each child As APSIMData In Data.Children
                If NumLevels < MaxNumLevels And (ShowAllComponents Or ApplicationSettings.IsComponentVisible(child.Type)) Then
                    Dim childnode As TreeNode = AddNode(child, ParentNode)
                    DisplayNode(child, childnode, NumLevels + 1)
                End If
            Next


        Catch e As System.Exception
            MsgBox("Error building tree for : " + Data.Name + vbCrLf + vbCrLf + e.Message, MsgBoxStyle.Critical, "Error building simulation tree")
        End Try

    End Sub

    ' -----------------------------------------------
    ' Add the specified node to the specified parent.
    ' -----------------------------------------------
    Private Function AddNode(ByVal NodeData As APSIMData, ByVal ParentNode As TreeNode) As TreeNode
        TreeView.ImageList = ApplicationSettings.SmallImageList
        Dim type As String = NodeData.Type
        Dim shortcut As String = NodeData.Attribute("shortcut")
        Dim ImageIndex As Integer = ApplicationSettings.SmallImageIndex(NodeData.Type)
        Dim childnode As TreeNode
        If ParentNode Is Nothing Then
            childnode = TreeView.Nodes.Add(NodeData.Name)
        Else
            childnode = ParentNode.Nodes.Add(NodeData.Name)
        End If

        childnode.ImageIndex = ImageIndex
        childnode.SelectedImageIndex = ImageIndex

        If shortcut <> "" Then
            childnode.ForeColor = System.Drawing.Color.Blue
        End If
        Return childnode
    End Function

    ' -------------------------
    ' Allow drop property
    ' -------------------------
    Public Overrides Property AllowDrop() As Boolean
        Get
            Return TreeView.AllowDrop
        End Get
        Set(ByVal Value As Boolean)
            TreeView.AllowDrop = Value
        End Set
    End Property


    ' -------------------------
    ' Sorted property
    ' -------------------------
    Public Property Sorted() As Boolean
        Get
            Return TreeView.Sorted
        End Get
        Set(ByVal Value As Boolean)
            TreeView.Sorted = Value
        End Set
    End Property


    ' ---------------------------------
    ' Select a node in the data tree.
    ' ---------------------------------
    Public ReadOnly Property Nodes() As TreeNodeCollection
        Get
            Return TreeView.Nodes
        End Get
    End Property


    ' ---------------------------------
    ' Select a node in the data tree.
    ' ---------------------------------
    Public Sub SelectNode(ByVal Node As TreeNode)
        TreeView.SelectedNode = Node
        TreeView_AfterSelect(Nothing, New System.Windows.Forms.TreeViewEventArgs(Node, TreeViewAction.ByKeyboard))
    End Sub


    ' -----------------------------------------
    ' User is just about to select a new node.
    ' throw an event if necessary.
    ' -----------------------------------------
    Private Sub TreeView_BeforeSelect(ByVal sender As Object, ByVal e As System.Windows.Forms.TreeViewCancelEventArgs) Handles TreeView.BeforeSelect
        RaiseEvent BeforeDataSelectedEvent()
        Dim DestinationNodeType As String = GetDataForFullPath(e.Node.FullPath).Type
        If DestinationNodeType = "folder" Or DestinationNodeType = "soils" Then
            AddFolderMenuItem.Enabled = True
        Else
            AddFolderMenuItem.Enabled = False
        End If

    End Sub


    ' -----------------------------------------
    ' User has just finished selecting a node
    ' -----------------------------------------
    Private Sub TreeView_AfterSelect(ByVal sender As System.Object, ByVal e As System.Windows.Forms.TreeViewEventArgs) Handles TreeView.AfterSelect
        If e.Action <> TreeViewAction.Unknown Then
            RaiseEvent DataSelectedEvent(GetDataForFullPath(e.Node.FullPath))
        End If
    End Sub


    ' ---------------------------------------------------
    ' User has just finished editing the label of a node.
    ' ---------------------------------------------------
    Private Sub TreeView_AfterLabelEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles TreeView.AfterLabelEdit
        Dim oldname As String = TreeView.SelectedNode.Text
        Dim newname As String = e.Label

        Dim path As String = TreeView.SelectedNode.FullPath
        If InStr(path, "/") > 0 Then
            path = Mid$(path, InStr(path, "/"))
        End If

        If newname Is Nothing Or Len(newname) = 0 Or e.CancelEdit Or LCase(e.Node.Text) = "shared" Then
            e.CancelEdit = True
        Else
            'APSIMFile.RenameComponent(datapath, newname)
            GetDataForFullPath(path).SetAttribute("name", newname)
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


    ' -----------------------------------------------------------------
    ' User has initiated a drag on a node - the full xml of the node
    ' is stored as the data associated with the drag event args.
    ' -----------------------------------------------------------------
    Private Sub TreeView_ItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles TreeView.ItemDrag
        Dim Data As APSIMData = GetDataForFullPath(e.Item.fullpath)
        Dim DataString As String
        Dim PosShared As Integer = e.Item.fullpath.IndexOf("|shared|")
        If PosShared <> -1 Then
            DataString = "<" + Data.Type + " name=""" + Data.Name + """ shortcut=""" + Data.Name + """/>"
        Else
            DataString = Data.XML
        End If
        TreeView.DoDragDrop(DataString, DragDropEffects.Copy)
    End Sub


    ' -------------------------
    ' User is dragging a node
    ' -------------------------
    Private Sub TreeView_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragEnter
        If AllowDrop = True Then
            e.Effect = DragDropEffects.Copy
        Else
            e.Effect = DragDropEffects.None
        End If
    End Sub


    ' --------------------------------------------------
    ' User has dragged a node over us - allow drop?
    ' --------------------------------------------------
    Private Sub TreeView_DragOver(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragOver
        e.Effect = DragDropEffects.None
        If AllowDrop = True Or e.Data.GetDataPresent(GetType(System.String)) Then
            Dim pt As Point = TreeView.PointToClient(New Point(e.X, e.Y))
            Dim DestinationNode As TreeNode = TreeView.GetNodeAt(pt)
            If DestinationNode Is Nothing Then
                ' do nothing
            Else
                Dim DestinationNodeType As String = GetDataForFullPath(DestinationNode.FullPath).Type

                Dim SourceDataString As String = e.Data.GetData(DataFormats.Text)
                Dim SourceData As New APSIMData(SourceDataString)
                If ApplicationSettings.AllowComponentAdd(SourceData.Type, DestinationNodeType) Then
                    e.Effect = DragDropEffects.Copy
                End If
            End If
        End If
    End Sub


    ' --------------------------------------------------
    ' User has released mouse button during a drag.
    ' Accept the dragged node.
    ' --------------------------------------------------
    Private Sub TreeView_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragDrop
        Try
            Dim pt As Point
            Dim DestinationNode As TreeNode
            pt = CType(sender, TreeView).PointToClient(New Point(e.X, e.Y))
            DestinationNode = CType(sender, TreeView).GetNodeAt(pt)
            Dim NewDataString As String = e.Data.GetData(DataFormats.Text)
            Dim NewData As New APSIMData(NewDataString)
            Dim fullpath As String = DestinationNode.FullPath
            GetDataForFullPath(fullpath).Add(NewData)
            NewData = GetDataForFullPath(fullpath + "|" + NewData.Name)
            'fill()
            'DestinationNode.Nodes.Clear()
            Dim NewNode As TreeNode = AddNode(NewData, DestinationNode)
            PopulateTree(NewData, NewNode)
            DestinationNode.Expand()

        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error")
        End Try

    End Sub

    Private Sub TreeView_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TreeView.KeyDown
        If e.KeyCode = Keys.Delete Then
            Dim ParentNode As APSIMData = GetDataForFullPath(TreeView.SelectedNode.FullPath).Parent
            ParentNode.Delete(TreeView.SelectedNode.Text)
            TreeView.SelectedNode.Remove()
            TreeView_AfterSelect(Nothing, New TreeViewEventArgs(TreeView.SelectedNode, TreeViewAction.ByKeyboard))
        End If
    End Sub

    Private Sub AddFolderMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddFolderMenuItem.Click
        Dim SelectedNodeData As APSIMData = GetDataForFullPath(TreeView.SelectedNode.FullPath)
        'SelectedNodeData.Add(NewData)

        Dim NewData As APSIMData = New APSIMData("Folder", "New folder")
        Dim fullpath As String = TreeView.SelectedNode.FullPath
        GetDataForFullPath(fullpath).Add(NewData)
        Dim NewNode As TreeNode = AddNode(NewData, TreeView.SelectedNode)
        PopulateTree(NewData, NewNode)
        TreeView.SelectedNode.Expand()


    End Sub

    Private Sub DeleteItemMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DeleteItemMenuItem.Click
        Dim ParentNode As APSIMData = GetDataForFullPath(TreeView.SelectedNode.FullPath).Parent
        ParentNode.Delete(TreeView.SelectedNode.Text)
        TreeView.SelectedNode.Remove()
    End Sub

    Private Sub TreeView_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TreeView.MouseDown
        Dim pt As Point
        Dim DestinationNode As TreeNode
        pt = New Point(e.X, e.Y)
        DestinationNode = CType(sender, TreeView).GetNodeAt(pt)
        If Not IsNothing(DestinationNode) Then
            If e.Button = MouseButtons.Right Then
                TreeView.SelectedNode = DestinationNode
            End If
        End If
    End Sub

    Private Sub TreeView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles TreeView.DoubleClick
        RaiseEvent DoubleClickEvent()
    End Sub

End Class
