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
    Delegate Sub OnDataTreeKeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)

    Public Event DoubleClickEvent As NotifyEventHandler
    Public Event DataTreeKeyPress As OnDataTreeKeyPress


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
        Me.RenameMenuItem = New System.Windows.Forms.MenuItem
        Me.MenuItem1 = New System.Windows.Forms.MenuItem
        Me.CutMenuItem = New System.Windows.Forms.MenuItem
        Me.CopyMenuItem = New System.Windows.Forms.MenuItem
        Me.PasteMenuItem = New System.Windows.Forms.MenuItem
        Me.MenuItem2 = New System.Windows.Forms.MenuItem
        Me.MoveUpMenuItem = New System.Windows.Forms.MenuItem
        Me.MoveDownMenuItem = New System.Windows.Forms.MenuItem
        Me.SuspendLayout()
        '
        'TreeView
        '
        Me.TreeView.AllowDrop = True
        Me.TreeView.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.TreeView.ContextMenu = Me.ContextMenu1
        Me.TreeView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TreeView.HideSelection = False
        Me.TreeView.LabelEdit = True
        Me.TreeView.Location = New System.Drawing.Point(0, 40)
        Me.TreeView.Name = "TreeView"
        Me.TreeView.PathSeparator = "|"
        Me.TreeView.ShowNodeToolTips = True
        Me.TreeView.Size = New System.Drawing.Size(843, 549)
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
        'RenameMenuItem
        '
        Me.RenameMenuItem.Index = 2
        Me.RenameMenuItem.Text = "&Rename"
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
        'DataTree
        '
        Me.AllowDrop = True
        Me.Controls.Add(Me.TreeView)
        Me.Name = "DataTree"
        Me.Size = New System.Drawing.Size(843, 589)
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
                AddHandler Controller.NodeChangedEvent, AddressOf OnNodeChanged
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
            Refresh()
            TreeView.Sorted = IsSorted
        End Set
    End Property


    Overrides Sub Refresh()
        ' ----------------------------------------------
        ' Override the base refresh method and populate
        ' ourselves.
        ' ----------------------------------------------
        If Not IsNothing(Controller) AndAlso Not Controller.AllData Is Nothing AndAlso UserChange Then
            HelpText = ""
            TreeView.ImageList = Controller.SmallImageList
            If TreeView.Nodes.Count = 0 Then
                TreeView.Nodes.Add(Controller.AllData.Name)
            End If
            RefreshNodeAndChildren(TreeView.Nodes(0), Controller.AllData)
            TreeView.Sorted = IsSorted
            If ExpandAllNodes Then
                TreeView.ExpandAll()
            Else
                TreeView.Nodes(0).Expand()
            End If
        End If
    End Sub

    Private Sub RefreshNodeAndChildren(ByVal Node As TreeNode, ByVal Data As APSIMData)
        ' ------------------------------------------------------------
        ' Refresh the specified TreeNode using the specified APSIMData
        ' plus all the child nodes.
        ' ------------------------------------------------------------
        TreeView.ImageList = Controller.SmallImageList
        Windows.Forms.Cursor.Current = Cursors.WaitCursor
        TreeView.BeginUpdate()

        RecursivelyRefreshNodeAndChildren(Node, Data, 0)

        If Controller.SelectedPaths.Count >= 1 Then
            UserChange = False
            TreeView.SelectedNode = GetNodeFromPath(Controller.SelectedPaths(0))
            UserChange = True
        End If

        TreeView.EndUpdate()
        Windows.Forms.Cursor.Current = Cursors.Default
    End Sub

    Private Sub RecursivelyRefreshNodeAndChildren(ByVal Node As TreeNode, ByVal Data As APSIMData, ByVal Level As Integer)
        ' ---------------------------------------
        ' Recursively refresh a node and its
        ' child nodes in the tree.
        ' ---------------------------------------

        RefreshNode(Node, Data)

        ' Go refresh all children.
        If Level < MaxNumLevels Then
            Dim ChildIndex As Integer = 0
            For Each Child As APSIMData In Data.Children
                If ShowAllComponents OrElse Controller.IsComponentVisible(Child.Type) Then
                    Dim ChildTreeNode As TreeNode
                    If ChildIndex < Node.Nodes.Count Then
                        ChildTreeNode = Node.Nodes(ChildIndex)
                    Else
                        ChildTreeNode = Node.Nodes.Add(Child.Name)
                    End If
                    RecursivelyRefreshNodeAndChildren(ChildTreeNode, Child, Level + 1)
                    ChildIndex = ChildIndex + 1
                End If
            Next
            While Node.Nodes.Count > ChildIndex
                Node.Nodes.Remove(Node.Nodes(Node.Nodes.Count - 1))
            End While
        End If
    End Sub

    Private Sub RefreshNode(ByVal Node As TreeNode, ByVal Data As APSIMData)
        ' -----------------------------------------------
        ' Set all the properties of the specified node.
        ' -----------------------------------------------
        Node.Text = Data.Name
        Dim ImageIndex As Integer = Controller.SmallImageIndex(Data.Type)
        Node.ImageIndex = ImageIndex
        Node.SelectedImageIndex = ImageIndex
        If Data.Attribute("description") <> "" Then
            Node.ToolTipText = Data.Attribute("description")
        End If

        Dim Selected As Boolean = Controller.SelectedPaths.IndexOf(BaseController.GetFullPathForData(Data)) >= 0
        PaintNode(Node, Selected)
    End Sub

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
                SelectedPaths.Remove(e.Node.FullPath)
                Controller.SelectedPaths = SelectedPaths
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
                    SelectedPaths.Remove(e.Node.FullPath)
                End If

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
                        SelectedPaths.Clear()
                    End If
                    SelectedPaths.Add(e.Node.FullPath)
                End If
            End If
            Controller.SelectedPaths = SelectedPaths

            UserChange = True
        End If

    End Sub


    Private Sub OnNodeChanged(ByVal DataFullPath As String, ByVal Data As APSIMData)
        RefreshNodeAndChildren(GetNodeFromPath(DataFullPath), Data)
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


    Private Sub PaintNodes(ByVal NodePaths As StringCollection, ByVal Selected As Boolean)
        ' ------------------------------------------------------------------
        ' Paint all specified nodes in highlight colour if "Selected" = true
        ' or as normal nodes otherwise.
        ' ------------------------------------------------------------------
        If Selected And NodePaths.Count >= 1 Then
            UserChange = False
            TreeView.SelectedNode = GetNodeFromPath(NodePaths(0))
            UserChange = True
        End If

        For Each NodePath As String In NodePaths
            PaintNode(GetNodeFromPath(NodePath), Selected)
        Next
    End Sub

    Private Sub PaintNode(ByVal n As TreeNode, ByVal Selected As Boolean)
        n.ForeColor = TreeView.ForeColor
        n.BackColor = TreeView.BackColor
        If Selected Then
            n.BackColor = SystemColors.Highlight
            n.ForeColor = SystemColors.HighlightText
        End If

        ' Get the data for this node.
        Dim NodeData As APSIMData = Controller.GetDataForFullPath(n.FullPath)
        If NodeData.Attribute("shortcut") <> "" Then
            n.ForeColor = Color.Blue
        End If
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
            Controller.RenameSelected(e.Label)
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

            Dim FullXML As String = ""
            For Each Data As APSIMData In Controller.SelectedData
                ' If Data is in shared then only drag a shortcut. Otherwise drag the full
                ' XML.
                Dim P As APSIMData = Data.Parent
                Dim IsShared As Boolean = False
                While Not IsNothing(P)
                    If P.Name.ToLower = "shared" Then
                        IsShared = True
                        Exit While
                    Else
                        P = P.Parent
                    End If
                End While
                If IsShared Then
                    FullXML = FullXML + "<" + Data.Type + " shortcut=""" + Data.Name + """/>"
                Else
                    FullXML = FullXML + Data.XML
                End If
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
        If e.Button = Windows.Forms.MouseButtons.Right And Not IsNothing(DestinationNode) Then
            If Controller.SelectedPaths.IndexOf(DestinationNode.FullPath) = -1 Then
                TreeView.SelectedNode = DestinationNode
            End If
        End If
    End Sub

    Private Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        ' ----------------------------------------
        ' Selection has changed - update tree.
        ' ----------------------------------------

        PaintNodes(OldSelections, False)
        PaintNodes(NewSelections, True)
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

    Private Sub TreeView_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TreeView.KeyPress
        RaiseEvent DataTreeKeyPress(sender, e)
    End Sub
End Class
