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
    Inherits BaseView
    Private MaxNumLevels As Integer = 100
    Private ShowAllComponents As Boolean = False
    Private ExpandAllNodes As Boolean = True
    Private IsSorted As Boolean = False
    Private LastNode As TreeNode
    Private FirstNode As TreeNode
    Private UserChange As Boolean = True
    Delegate Sub NotifyEventHandler()
    Public Event DoubleClickEvent As NotifyEventHandler



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
    Friend WithEvents MenuItem1 As System.Windows.Forms.MenuItem
    Friend WithEvents CutMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents CopyMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents PasteMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem2 As System.Windows.Forms.MenuItem
    Friend WithEvents MoveUpMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents MoveDownMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents RenameMenuItem As System.Windows.Forms.MenuItem
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.TreeView = New System.Windows.Forms.TreeView
        Me.ContextMenu1 = New System.Windows.Forms.ContextMenu
        Me.AddFolderMenuItem = New System.Windows.Forms.MenuItem
        Me.DeleteItemMenuItem = New System.Windows.Forms.MenuItem
        Me.MenuItem1 = New System.Windows.Forms.MenuItem
        Me.CutMenuItem = New System.Windows.Forms.MenuItem
        Me.CopyMenuItem = New System.Windows.Forms.MenuItem
        Me.PasteMenuItem = New System.Windows.Forms.MenuItem
        Me.MenuItem2 = New System.Windows.Forms.MenuItem
        Me.MoveUpMenuItem = New System.Windows.Forms.MenuItem
        Me.MoveDownMenuItem = New System.Windows.Forms.MenuItem
        Me.RenameMenuItem = New System.Windows.Forms.MenuItem
        Me.SuspendLayout()
        '
        'TreeView
        '
        Me.TreeView.AllowDrop = True
        Me.TreeView.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.TreeView.ContextMenu = Me.ContextMenu1
        Me.TreeView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TreeView.HideSelection = False
        Me.TreeView.ImageIndex = -1
        Me.TreeView.LabelEdit = True
        Me.TreeView.Location = New System.Drawing.Point(0, 40)
        Me.TreeView.Name = "TreeView"
        Me.TreeView.PathSeparator = "|"
        Me.TreeView.SelectedImageIndex = -1
        Me.TreeView.Size = New System.Drawing.Size(705, 713)
        Me.TreeView.TabIndex = 0
        '
        'ContextMenu1
        '
        Me.ContextMenu1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.AddFolderMenuItem, Me.DeleteItemMenuItem, Me.RenameMenuItem, Me.MenuItem1, Me.CutMenuItem, Me.CopyMenuItem, Me.PasteMenuItem, Me.MenuItem2, Me.MoveUpMenuItem, Me.MoveDownMenuItem})
        '
        'AddFolderMenuItem
        '
        Me.AddFolderMenuItem.Index = 0
        Me.AddFolderMenuItem.Text = "&Add Folder"
        '
        'DeleteItemMenuItem
        '
        Me.DeleteItemMenuItem.Index = 1
        Me.DeleteItemMenuItem.Shortcut = System.Windows.Forms.Shortcut.Del
        Me.DeleteItemMenuItem.Text = "&Delete"
        '
        'MenuItem1
        '
        Me.MenuItem1.Index = 3
        Me.MenuItem1.Text = "-"
        '
        'CutMenuItem
        '
        Me.CutMenuItem.Index = 4
        Me.CutMenuItem.Shortcut = System.Windows.Forms.Shortcut.CtrlX
        Me.CutMenuItem.Text = "Cu&t"
        '
        'CopyMenuItem
        '
        Me.CopyMenuItem.Index = 5
        Me.CopyMenuItem.Shortcut = System.Windows.Forms.Shortcut.CtrlC
        Me.CopyMenuItem.Text = "&Copy"
        '
        'PasteMenuItem
        '
        Me.PasteMenuItem.Index = 6
        Me.PasteMenuItem.Shortcut = System.Windows.Forms.Shortcut.CtrlV
        Me.PasteMenuItem.Text = "&Paste"
        '
        'MenuItem2
        '
        Me.MenuItem2.Index = 7
        Me.MenuItem2.Text = "-"
        '
        'MoveUpMenuItem
        '
        Me.MoveUpMenuItem.Index = 8
        Me.MoveUpMenuItem.ShowShortcut = False
        Me.MoveUpMenuItem.Text = "Move &up        Ctrl+Up"
        '
        'MoveDownMenuItem
        '
        Me.MoveDownMenuItem.Index = 9
        Me.MoveDownMenuItem.ShowShortcut = False
        Me.MoveDownMenuItem.Text = "Move do&wn    Ctrl+Down"
        '
        'RenameMenuItem
        '
        Me.RenameMenuItem.Index = 2
        Me.RenameMenuItem.Text = "&Rename"
        '
        'DataTree
        '
        Me.AllowDrop = True
        Me.Controls.Add(Me.TreeView)
        Me.Name = "DataTree"
        Me.Size = New System.Drawing.Size(705, 753)
        Me.Controls.SetChildIndex(Me.TreeView, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region



    ' ------------------------------------------------
    ' Called to setup the global application object
    ' ------------------------------------------------
    Overrides Property Controller() As BaseController
        Get
            Return MyBase.Controller
        End Get
        Set(ByVal Value As BaseController)
            MyBase.Controller = Value
            If Not IsNothing(Value) Then
                AddHandler Controller.AddEvent, AddressOf Refresh
                AddHandler Controller.DeleteEvent, AddressOf Refresh
                AddHandler Controller.RenameEvent, AddressOf Refresh
                AddHandler Controller.SelectionChangingEvent, AddressOf OnSelectionChanging
                AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged
            End If
        End Set
    End Property


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


    ' ------------------------------------------------------
    ' Set the sortall property
    ' ------------------------------------------------------
    WriteOnly Property SortAll() As Boolean
        Set(ByVal Value As Boolean)
            IsSorted = Value
            TreeView.Sorted = IsSorted
            Refresh()
        End Set
    End Property


    ' ----------------------------------------------
    ' Override the base fill method and populate
    ' ourselves.
    ' ----------------------------------------------
    Overrides Sub Refresh()
        If Not IsNothing(Controller) AndAlso Not Controller.AllData Is Nothing AndAlso UserChange Then
            HelpText = ""
            TreeView.BeginUpdate()
            TreeView.Nodes.Clear()
            AddNode(Controller.AllData, Nothing)
            Dim RootNode As TreeNode = TreeView.Nodes(0)
            PopulateTree(Controller.AllData, RootNode)
            TreeView.Sorted = IsSorted
            If ExpandAllNodes Then
                TreeView.ExpandAll()
            End If
            RootNode.Expand()
            TreeView.EndUpdate()
        End If
    End Sub

    ' ----------------------------------------------
    ' Populate the tree using the specified data.
    ' ParentNode can be nothing
    ' ----------------------------------------------
    Private Sub PopulateTree(ByVal Data As APSIMData, ByRef ParentNode As TreeNode)
        TreeView.ImageList = Controller.SmallImageList

        ' Display a wait cursor while the TreeNodes are being created.
        Cursor.Current = Cursors.WaitCursor

        ' Suppress repainting the TreeView until all the objects have been created.
        TreeView.BeginUpdate()

        DisplayNode(Data, ParentNode, 0)
        TreeView.Sorted = IsSorted

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
                If NumLevels < MaxNumLevels And (ShowAllComponents Or Controller.IsComponentVisible(child.Type)) Then
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
        TreeView.ImageList = Controller.SmallImageList
        Dim type As String = NodeData.Type
        Dim shortcut As String = NodeData.Attribute("shortcut")
        Dim ImageIndex As Integer = Controller.SmallImageIndex(NodeData.Type)
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


    ' ---------------------------------------------
    ' Code from TreeViewMS component.
    ' We have specified manual painting.
    ' ---------------------------------------------
    Protected Overrides Sub OnPaint(ByVal pe As PaintEventArgs)
        MyBase.OnPaint(pe)
    End Sub


    ' ---------------------------------------------
    ' Code from TreeViewMS component.
    ' Does multiple selection of nodes.
    ' ---------------------------------------------
    Private Sub OnBeforeSelect(ByVal sender As Object, ByVal e As TreeViewCancelEventArgs) Handles TreeView.BeforeSelect
        If UserChange Then
            Dim Control As Boolean = (ModifierKeys = Keys.Control)
            Dim Shift As Boolean = (ModifierKeys = Keys.Shift)

            ' selecting the node twice while pressing CTRL ?
            Dim SelectedPaths As StringCollection = Controller.SelectedPaths()
            If Control And SelectedPaths.Contains(e.Node.FullPath) Then
                ' unselect it (let framework know we don't want selection this time)
                e.Cancel = True

                ' update nodes
                RemovePaintFromNodes()
                SelectedPaths.Remove(e.Node.FullPath)
                Controller.SelectedPaths = SelectedPaths
                PaintSelectedNodes()
                Return
            End If

            LastNode = e.Node
            If Not Shift Then
                FirstNode = e.Node ' store begin of shift sequence
            End If
        End If
    End Sub


    ' ---------------------------------------------
    ' Code from TreeViewMS component.
    ' Does multiple selection of nodes.
    ' ---------------------------------------------
    Private Sub OnAfterSelect(ByVal sender As Object, ByVal e As TreeViewEventArgs) Handles TreeView.AfterSelect
        If UserChange Then
            UserChange = False
            Dim Control As Boolean = (ModifierKeys = Keys.Control)
            Dim Shift As Boolean = (ModifierKeys = Keys.Shift)

            Dim SelectedPaths As StringCollection = Controller.SelectedPaths()
            If Control Then
                If Not SelectedPaths.Contains(e.Node.FullPath) Then ' new node ?
                    SelectedPaths.Add(e.Node.FullPath)
                Else  ' not new, remove it from the collection
                    RemovePaintFromNodes()
                    SelectedPaths.Remove(e.Node.FullPath)
                End If
                PaintSelectedNodes()
            Else
                ' SHIFT is pressed
                If Shift Then
                    Dim MyQueue As New Queue

                    Dim UpperNode As TreeNode = FirstNode
                    Dim BottomNode As TreeNode = e.Node
                    ' case 1 : begin and end nodes are parent
                    Dim Parent As Boolean = IsParent(FirstNode, e.Node) ' is m_firstNode parent (direct or not) of e.Node
                    If Not Parent Then
                        Parent = IsParent(BottomNode, UpperNode)
                        If Parent Then ' swap nodes

                            Dim t As TreeNode = UpperNode
                            UpperNode = BottomNode
                            BottomNode = t
                        End If
                    End If
                    If Parent Then
                        Dim n As TreeNode = BottomNode
                        While Not n Is UpperNode.Parent
                            If Not SelectedPaths.Contains(n.FullPath) Then ' new node ?
                                MyQueue.Enqueue(n)
                            End If
                            n = n.Parent
                        End While
                    Else
                        ' case 2 : nor the begin nor the end node are descendant one another

                        If (UpperNode.Parent Is Nothing And BottomNode.Parent Is Nothing) Or _
                           (Not IsNothing(UpperNode.Parent) And UpperNode.Parent.Nodes.Contains(BottomNode)) Then   ' are they siblings ?
                            Dim IndexUpper As Integer = UpperNode.Index
                            Dim IndexBottom As Integer = BottomNode.Index
                            If IndexBottom < IndexUpper Then ' reversed?

                                Dim t As TreeNode = UpperNode
                                UpperNode = BottomNode
                                BottomNode = t
                                IndexUpper = UpperNode.Index
                                IndexBottom = BottomNode.Index
                            End If

                            Dim n As TreeNode = UpperNode
                            While IndexUpper <= IndexBottom

                                If Not SelectedPaths.Contains(n.FullPath) Then  ' new node ?
                                    MyQueue.Enqueue(n)
                                End If

                                n = n.NextNode

                                IndexUpper = IndexUpper + 1
                            End While
                        Else

                            If Not SelectedPaths.Contains(UpperNode.FullPath) Then
                                MyQueue.Enqueue(UpperNode)
                            End If
                            If Not SelectedPaths.Contains(BottomNode.FullPath) Then
                                MyQueue.Enqueue(BottomNode)
                            End If
                        End If
                    End If
                    For Each Node As TreeNode In MyQueue
                        SelectedPaths.Add(Node.FullPath)
                    Next

                    FirstNode = e.Node ' let us chain several SHIFTs if we like it
                Else
                    ' in the case of a simple click, just add this item
                    If SelectedPaths.Count > 0 Then
                        RemovePaintFromNodes()
                        SelectedPaths.Clear()
                    End If
                    SelectedPaths.Add(e.Node.FullPath)
                End If
            End If
            Controller.SelectedPaths = SelectedPaths
            PaintSelectedNodes()
            UserChange = True
        End If
    End Sub


    ' --------------------------------------------------
    ' Returns a tree node given a fullly delimited path.
    ' --------------------------------------------------
    Private Function GetNodeFromPath(ByVal ChildPath As String) As TreeNode
        Dim name As String
        Dim Path As String = ChildPath
        Dim CurrentNode As TreeNode = Nothing
        Do Until Path = ""
            Dim PosDelimiter As Integer = Path.IndexOf("|")
            If PosDelimiter <> -1 Then
                name = Path.Substring(0, PosDelimiter)
                Path = Path.Substring(PosDelimiter + 1)
            Else
                name = Path
                Path = ""
            End If

            Dim ChildNode As TreeNode = Nothing
            If CurrentNode Is Nothing Then
                ChildNode = TreeView.Nodes(0)
            Else
                For Each ChildNode In CurrentNode.Nodes
                    If ChildNode.Text = name Then
                        Exit For
                    End If
                Next
            End If
            CurrentNode = ChildNode
            If Not IsNothing(CurrentNode) Then
                If CurrentNode.Text <> name Then
                    CurrentNode = Nothing
                End If
            End If
            If IsNothing(CurrentNode) Then
                Exit Do
            End If
        Loop

        If IsNothing(CurrentNode) Then
            Throw New System.Exception("Cannot find tree node for path: " + ChildPath)
        Else
            Return CurrentNode
        End If
    End Function


    ' ---------------------------------------------
    ' Code from TreeViewMS component.
    ' Paint all selected nodes in highlight colour.
    ' ---------------------------------------------
    Private Sub PaintSelectedNodes()
        Dim SelectedPaths As StringCollection = Controller.SelectedPaths()
        For Each NodePath As String In SelectedPaths
            Dim n As TreeNode = GetNodeFromPath(NodePath)
            n.BackColor = SystemColors.Highlight
            n.ForeColor = SystemColors.HighlightText
        Next
    End Sub


    ' ---------------------------------------------
    ' Code from TreeViewMS component.
    ' Returns true if the specified parent node is
    ' a parent for the specified child node.
    ' ---------------------------------------------
    Private Function IsParent(ByVal ParentNode As TreeNode, ByVal ChildNode As TreeNode) As Boolean
        If ParentNode Is ChildNode Then
            Return True
        End If

        Dim n As TreeNode = ChildNode
        Dim Found As Boolean = False
        While Not Found And Not n Is Nothing
            n = n.Parent
            Found = (n Is ParentNode)
        End While
        Return Found
    End Function


    ' ---------------------------------------------
    ' Code from TreeViewMS component.
    ' Removes the highlighting from all selected nodes.
    ' ---------------------------------------------
    Private Sub RemovePaintFromNodes()
        Dim SelectedPaths As StringCollection = Controller.SelectedPaths()

        If SelectedPaths.Count = 0 Then
            Return
        End If

        Dim n0 As TreeNode = GetNodeFromPath(SelectedPaths(0))
        Dim back As Color = n0.TreeView.BackColor
        Dim fore As Color = n0.TreeView.ForeColor
        For Each NodePath As String In SelectedPaths
            Dim n As TreeNode = GetNodeFromPath(NodePath)
            n.BackColor = back
            n.ForeColor = fore
        Next
    End Sub

    ' ---------------------------------------
    ' Do we allow the rename of the node?
    ' ---------------------------------------
    Private Sub TreeView_BeforeLabelEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles TreeView.BeforeLabelEdit

        e.CancelEdit = Not Controller.AllowRenameSelected()
        If Not e.CancelEdit Then
            TreeView.ContextMenu = Nothing
        End If
    End Sub


    ' ---------------------------------------------------
    ' User has just finished editing the label of a node.
    ' ---------------------------------------------------
    Private Sub TreeView_AfterLabelEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles TreeView.AfterLabelEdit

        'catch the event where user starts a label edit but does not change anything
        'Appears to be a bug in TreeView control
        If IsNothing(e.Label) Then
            e.CancelEdit = True
            Exit Sub

        End If

        ' A tree view node label cannot be an empty string.  If it is then
        ' cancel the edit.
        If (Not e.Label.Equals("")) Then
            UserChange = False
            Controller.RenameSelected(e.Label)
            UserChange = True

        Else
            e.CancelEdit = True
        End If

        TreeView.ContextMenu = Me.ContextMenu1
    End Sub



    ' -----------------------------------------------------------------
    ' User has initiated a drag on a node - the full xml of the node
    ' is stored as the data associated with the drag event args.
    ' -----------------------------------------------------------------
    Private Sub TreeView_ItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles TreeView.ItemDrag
        If Controller.SelectedPaths.IndexOf(e.Item.FullPath) = -1 Then
            TreeView.SelectedNode = e.Item
        End If

        If Controller.SelectedData.Count > 0 Then

            Dim FullXML As String
            For Each Data As APSIMData In Controller.SelectedData
                FullXML = FullXML + Data.XML
            Next
            Dim AllowedEffects As DragDropEffects
            If Controller.AllowChanges() Then
                AllowedEffects = DragDropEffects.Copy Or DragDropEffects.Move
            Else
                AllowedEffects = DragDropEffects.Copy
            End If
            Dim ItemsToPotentiallyDelete As StringCollection = Controller.SelectedPaths
            If TreeView.DoDragDrop(FullXML, AllowedEffects) = DragDropEffects.Move Then
                Controller.Delete(ItemsToPotentiallyDelete)
            End If
        End If
    End Sub


    '-------------------------
    'User is dragging a node
    '-------------------------
    'Private Sub TreeView_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragEnter
    '    Dim Control As Boolean = (ModifierKeys = Keys.Control)
    '    Dim Shift As Boolean = (ModifierKeys = Keys.Shift)
    '    'If Control Then
    '    'e.Effect = DragDropEffects.Copy
    '    'Else
    '    e.Effect = DragDropEffects.Move
    '    'End If
    'End Sub


    ' --------------------------------------------------
    ' User has dragged a node over us - allow drop?
    ' --------------------------------------------------
    Private Sub TreeView_DragOver(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragOver
        'e.Effect = DragDropEffects.None
        If e.Data.GetDataPresent(GetType(System.String)) Then
            Dim pt As Point = TreeView.PointToClient(New Point(e.X, e.Y))
            Dim DestinationNode As TreeNode = TreeView.GetNodeAt(pt)
            If DestinationNode Is Nothing Then
                ' do nothing
            Else
                Dim FullXML As String = e.Data.GetData(DataFormats.Text)
                If Controller.AllowAddXMLToData(FullXML, DestinationNode.FullPath) Then
                    Dim Control As Boolean = (ModifierKeys = Keys.Control)
                    Dim Shift As Boolean = (ModifierKeys = Keys.Shift)
                    If Shift Then
                        e.Effect = DragDropEffects.Move
                    Else
                        e.Effect = DragDropEffects.Copy
                    End If
                Else
                    e.Effect = DragDropEffects.None
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
            Dim pt As Point = CType(sender, TreeView).PointToClient(New Point(e.X, e.Y))
            Dim DestinationNode As TreeNode = CType(sender, TreeView).GetNodeAt(pt)
            Dim FullXML As String = e.Data.GetData(DataFormats.Text)
            TreeView.SelectedNode = DestinationNode
            Controller.AddXMLToSelected(FullXML)
        Catch ex As System.Exception
            MessageBox.Show(ex.Message, "Cannot drop the nodes", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub


    ' --------------------------------------------------
    ' User has pressed a key.
    ' --------------------------------------------------
    Private Sub TreeView_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TreeView.KeyDown
        If e.KeyCode = Keys.Delete Then
            Controller.Delete(Controller.SelectedPaths)
        ElseIf e.Control And e.KeyCode = Keys.X Then
            Controller.Cut()
        ElseIf e.Control And e.KeyCode = Keys.C Then
            Controller.Copy()
        ElseIf e.Control And e.KeyCode = Keys.V Then
            Controller.Paste()
        ElseIf e.Control And e.KeyCode = Keys.Up Then
            Controller.MoveSelectedUp()
        ElseIf e.Control And e.KeyCode = Keys.Down Then
            Controller.MoveSelectedDown()
        End If
    End Sub


    ' ----------------------------------------
    ' Trap the right mouse button and perform
    ' a node select.
    ' ----------------------------------------
    Private Sub TreeView_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TreeView.MouseDown
        Dim pt As Point
        Dim DestinationNode As TreeNode
        pt = New Point(e.X, e.Y)
        DestinationNode = CType(sender, TreeView).GetNodeAt(pt)
        If e.Button = MouseButtons.Right And Not IsNothing(DestinationNode) Then
            If Controller.SelectedPaths.IndexOf(DestinationNode.FullPath) = -1 Then
                TreeView.SelectedNode = DestinationNode
            End If
        End If
    End Sub

    ' --------------------------------------------------------------------
    ' Selection is about to change - remove paint from all selected nodes.
    ' --------------------------------------------------------------------
    Private Sub OnSelectionChanging()
        If UserChange Then
            UserChange = False
            RemovePaintFromNodes()
            UserChange = True
        End If
    End Sub


    ' ----------------------------------------
    ' Selection has changed - update tree.
    ' ----------------------------------------
    Private Sub OnSelectionChanged()
        If UserChange Then
            UserChange = False
            Dim Selections As StringCollection = Controller.SelectedPaths()
            If Selections.Count > 0 Then
                TreeView.SelectedNode = GetNodeFromPath(Selections(0))
                PaintSelectedNodes()
            End If
            UserChange = True
        End If

    End Sub


    ' --------------------------------------
    ' Context menu is about to popup 
    ' Set functionality for it.
    ' --------------------------------------
    Private Sub ContextMenu1_Popup(ByVal sender As Object, ByVal e As System.EventArgs) Handles ContextMenu1.Popup
        AddFolderMenuItem.Enabled = Controller.AllowAddFolderToSelected
        DeleteItemMenuItem.Enabled = Controller.AllowDeleteSelected
        RenameMenuItem.Enabled = Controller.AllowRenameSelected
        CutMenuItem.Enabled = Controller.AllowCut
        CopyMenuItem.Enabled = Controller.AllowCopy
        PasteMenuItem.Enabled = Controller.AllowPaste
        MoveUpMenuItem.Enabled = Controller.AllowMoveSelectedUp And Not TreeView.Sorted
        MoveDownMenuItem.Enabled = Controller.AllowMoveSelectedDown And Not TreeView.Sorted
    End Sub

    ' ---------------------------
    ' User wants to add a folder.
    ' ---------------------------
    Private Sub AddFolderMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddFolderMenuItem.Click
        UserChange = False
        Controller.AddXMLToSelected("<folder name=""New folder""/>")
        Dim NewNode As TreeNode = TreeView.SelectedNode.Nodes.Add("New folder")
        Dim ImageIndex As Integer = Controller.SmallImageIndex("folder")
        NewNode.ImageIndex = ImageIndex
        NewNode.SelectedImageIndex = ImageIndex
        NewNode.BeginEdit()
        UserChange = True
    End Sub


    ' -------------------------------------------
    ' User wants to delete the current selection
    ' -------------------------------------------
    Private Sub DeleteItemMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DeleteItemMenuItem.Click
        Controller.Delete(Controller.SelectedPaths)
    End Sub


    ' -------------------------------------------
    ' User wants to cut the current selection.
    ' -------------------------------------------
    Private Sub CutMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles CutMenuItem.Click
        Controller.Cut()
    End Sub


    ' -------------------------------------------
    ' User wants to copy the current selection.
    ' -------------------------------------------
    Private Sub CopyMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles CopyMenuItem.Click
        Controller.Copy()
    End Sub


    ' -------------------------------------------
    ' User wants to paste the current selection.
    ' -------------------------------------------
    Private Sub PasteMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles PasteMenuItem.Click
        Controller.Paste()
    End Sub


    Private Sub MoveUpMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles MoveUpMenuItem.Click
        Controller.MoveSelectedUp()
    End Sub

    Private Sub MoveDownMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles MoveDownMenuItem.Click
        Controller.MoveSelectedDown()
    End Sub

    Private Sub RenameMenuItemClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles RenameMenuItem.Click
        If Not TreeView.SelectedNode Is Nothing Then
            TreeView.SelectedNode.BeginEdit()
        End If
    End Sub

    Private Sub TreeView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles TreeView.DoubleClick
        RaiseEvent DoubleClickEvent()
    End Sub
End Class
