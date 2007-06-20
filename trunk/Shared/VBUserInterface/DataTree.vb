Imports System.Xml
Imports System.Collections.Specialized
Imports System.Windows.Forms
Imports System.Drawing
Imports VBGeneral

' ------------------------------------------
' Data control for displaying a tree based
' on an APSIMData instance.
' ------------------------------------------
Public Class DataTree
    Inherits BaseView
    Private MaxNumLevels As Integer = 100
    Private ShowAllComponents As Boolean = False
    Private IsSorted As Boolean = False
    Private DisablePainting = False
    Private LastNode As TreeNode
    Private FirstNode As TreeNode
    Friend WithEvents PopupMenu As System.Windows.Forms.ContextMenuStrip
    Private Controller As BaseController

    Public Event DoubleClickEvent As EventHandler(Of EventArgs)

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
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.TreeView = New System.Windows.Forms.TreeView
        Me.PopupMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.SuspendLayout()
        '
        'TreeView
        '
        Me.TreeView.AllowDrop = True
        Me.TreeView.BackColor = System.Drawing.SystemColors.Window
        Me.TreeView.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.TreeView.CausesValidation = False
        Me.TreeView.ContextMenuStrip = Me.PopupMenu
        Me.TreeView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TreeView.DrawMode = System.Windows.Forms.TreeViewDrawMode.OwnerDrawText
        Me.TreeView.HideSelection = False
        Me.TreeView.LabelEdit = True
        Me.TreeView.Location = New System.Drawing.Point(0, 40)
        Me.TreeView.Name = "TreeView"
        Me.TreeView.ShowNodeToolTips = True
        Me.TreeView.Size = New System.Drawing.Size(1020, 705)
        Me.TreeView.TabIndex = 0
        '
        'PopupMenu
        '
        Me.PopupMenu.Name = "PopupMenu"
        Me.PopupMenu.Size = New System.Drawing.Size(153, 26)
        '
        'DataTree
        '
        Me.AllowDrop = True
        Me.Controls.Add(Me.TreeView)
        Me.Name = "DataTree"
        Me.Size = New System.Drawing.Size(1020, 745)
        Me.Controls.SetChildIndex(Me.TreeView, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    WriteOnly Property MaximumNumLevels() As Integer
        ' ------------------------------------------------------
        ' Set the visible components context string property
        ' ------------------------------------------------------
        Set(ByVal Value As Integer)
            MaxNumLevels = Value
        End Set
    End Property
    WriteOnly Property ShowAll() As Boolean
        ' ------------------------------------------------------
        ' Set the showall property
        ' ------------------------------------------------------
        Set(ByVal Value As Boolean)
            ShowAllComponents = Value
        End Set
    End Property
    Public Sub ExpandAll()
        TreeView.ExpandAll()
    End Sub
    Public Sub CollapseAll()
        TreeView.CollapseAll()
    End Sub
    WriteOnly Property SortAll() As Boolean
        ' ------------------------------------------------------
        ' Set the sortall property
        ' ------------------------------------------------------
        Set(ByVal Value As Boolean)
            IsSorted = Value
            If Not IsNothing(Controller) Then
                RefreshView("\")
            End If
            TreeView.Sorted = IsSorted
        End Set
    End Property
    Public Property Sorted() As Boolean
        ' -------------------------
        ' Sorted property
        ' -------------------------
        Get
            Return TreeView.Sorted
        End Get
        Set(ByVal Value As Boolean)
            TreeView.Sorted = Value
        End Set
    End Property
    Private Function GetNodeFromPath(ByVal ChildPath As String) As TreeNode
        ' --------------------------------------------------
        ' Returns a tree node given a fullly delimited path.
        ' --------------------------------------------------
        Dim name As String
        Dim Path As String = ChildPath
        Dim CurrentNode As TreeNode = Nothing
        Do Until Path = ""
            Dim PosDelimiter As Integer = Path.IndexOf(TreeView.PathSeparator)
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

        Return CurrentNode
    End Function
    Public Overrides Sub OnLoad(ByVal Controller As BaseController)
        HelpText = ""
        TreeView.ImageList = Controller.SmallImageList
        Me.Controller = Controller
        Controller.ProvideToolStrip(PopupMenu, "ContextMenu")
        AddHandler Controller.ApsimData.DataStructureChangedEvent, AddressOf RefreshView
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged
        AddHandler TreeView.BeforeSelect, AddressOf OnBeforeSelect
    End Sub
    Public Sub ExpandAllFolders()
        TreeView.CollapseAll()
        ExpandAllFolders(TreeView.Nodes(0))
        TreeView.Nodes(0).Expand()
    End Sub
    Public Sub ExpandOneLevel()
        TreeView.CollapseAll()
        TreeView.Nodes(0).Expand()
    End Sub


#Region "Refresh methods"
    Public Overrides Sub RefreshView(ByVal NodePath As String)
        ' ----------------------------------------------
        ' Override the base refresh method and populate
        ' ourselves.
        ' ----------------------------------------------
        Windows.Forms.Cursor.Current = Cursors.WaitCursor
        TreeView.BeginUpdate()
        DisablePainting = True

        If TreeView.Nodes.Count = 0 Then
            Dim RootNode As TreeNode = TreeView.Nodes.Add(Controller.ApsimData.AllData.Name)
            RefreshNode(RootNode, Controller.ApsimData.AllData)
        End If

        RecursivelyRefreshNodeAndChildren(TreeView.Nodes(0), Controller.ApsimData.AllData, 0)
        TreeView.Sorted = IsSorted

        DisablePainting = False
        TreeView.EndUpdate()
        Windows.Forms.Cursor.Current = Cursors.Default
    End Sub
    Private Sub RecursivelyRefreshNodeAndChildren(ByVal Node As TreeNode, ByVal Data As APSIMData, ByVal Level As Integer)
        ' ---------------------------------------
        ' Recursively refresh a node and its
        ' child nodes in the tree.
        ' ---------------------------------------

        If Node.Text <> Data.Name Then
            RefreshNode(Node, Data)
        End If

        ' Go refresh all children.
        If Level < MaxNumLevels Then
            Dim ChildIndex As Integer = 0
            For Each Child As APSIMData In Data.Children
                If ShowAllComponents OrElse Controller.IsComponentVisible(Child) Then
                    Dim ChildTreeNode As TreeNode
                    If ChildIndex < Node.Nodes.Count Then
                        ChildTreeNode = Node.Nodes(ChildIndex)
                    Else
                        ChildTreeNode = Node.Nodes.Add(Child.Name)
                        RefreshNode(ChildTreeNode, Child)
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
        Node.Tag = Data.Type.ToLower = "folder"   ' keep track if this is a folder node.
        If Data.Attribute("description") <> "" Then
            Node.ToolTipText = Data.Attribute("description")
        End If
    End Sub
    Private Sub ExpandAllFolders(ByVal Node As TreeNode)
        If Node.Tag = True Then
            Dim ThereAreSubFolders As Boolean = False
            For Each Child As TreeNode In Node.Nodes
                If Child.Tag = True Then
                    ThereAreSubFolders = True
                    Exit For
                End If
            Next
            If ThereAreSubFolders Then
                Node.Expand()
                For Each Child As TreeNode In Node.Nodes
                    ExpandAllFolders(Child)
                Next
            End If
        End If
    End Sub
#End Region

#Region "Selection methods"
    Private PreviousNode As TreeNode
    Private Sub OnBeforeSelect(ByVal sender As Object, ByVal e As TreeViewCancelEventArgs)
        ' ---------------------------------------------
        ' Code from TreeViewMS component.
        ' Does multiple selection of nodes.
        ' ---------------------------------------------
        Dim Control As Boolean = (ModifierKeys = Keys.Control)
        Dim Shift As Boolean = (ModifierKeys = Keys.Shift)

        Dim SelectedPaths As StringCollection = Controller.SelectedPaths
        If Control Then
            SelectedPaths.Add(e.Node.FullPath)
            PreviousNode = e.Node
        ElseIf Shift Then
            If Not IsNothing(PreviousNode) AndAlso PreviousNode.Parent.Equals(e.Node.Parent) Then
                Dim FirstIndex As Integer = PreviousNode.Index
                Dim LastIndex As Integer = e.Node.Index
                If FirstIndex > LastIndex Then
                    Dim TempIndex As Integer = LastIndex
                    LastIndex = FirstIndex
                    FirstIndex = TempIndex
                End If
                SelectedPaths.Clear()
                For i As Integer = FirstIndex To LastIndex
                    SelectedPaths.Add(PreviousNode.Parent.Nodes(i).FullPath)
                Next
                PreviousNode = e.Node
            End If
        Else
            If SelectedPaths.Count = 1 AndAlso SelectedPaths(0) = e.Node.FullPath Then
                e.Node.BeginEdit()
            Else
                SelectedPaths.Clear()
                SelectedPaths.Add(e.Node.FullPath)
                PreviousNode = e.Node
            End If
        End If
        Controller.SelectedPaths = SelectedPaths
    End Sub
    Private Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        ' -----------------------------------------------------------------
        ' Selection has changed - update tree.
        ' -----------------------------------------------------------------
        For Each NodePath As String In OldSelections
            Dim Node As TreeNode = GetNodeFromPath(NodePath)
            If Not IsNothing(Node) Then
                Node.BackColor = TreeView.BackColor ' this will invalidate node.
            End If
        Next
        For Each NodePath As String In NewSelections
            Dim Node As TreeNode = GetNodeFromPath(NodePath)
            If Not IsNothing(Node) Then
                Node.BackColor = SystemColors.Highlight ' this will invalidate node.
            End If
            Node.EnsureVisible()
        Next
    End Sub
#End Region

#Region "Rename methods"
    Private Sub TreeView_BeforeLabelEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles TreeView.BeforeLabelEdit
        ' ---------------------------------------
        ' Do we allow the rename of the node?
        ' ---------------------------------------
        e.CancelEdit = e.Node.Level = 0
        If Not e.CancelEdit Then
            TreeView.ContextMenu = Nothing
        End If
    End Sub
    Private Sub TreeView_AfterLabelEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles TreeView.AfterLabelEdit
        ' ---------------------------------------------------
        ' User has just finished editing the label of a node.
        ' ---------------------------------------------------

        'catch the event where user starts a label edit but does not change anything
        'Appears to be a bug in TreeView control
        If Not IsNothing(e.Label) AndAlso Not e.Label.Equals("") Then
            ' Firstly empty the current selections.
            Controller.SelectedPath = ""

            ' Change the data
            Controller.ApsimData.Rename(e.Node.FullPath, e.Label)

            ' Now tell the base controller about the new selectionws.
            Controller.SelectedPath = e.Node.Parent.FullPath + "\" + e.Label
        End If
        e.CancelEdit = True

        'dph TreeView.ContextMenu = Me.ContextMenu1
        TreeView.ContextMenuStrip = PopupMenu
    End Sub
#End Region

#Region "Drag / Drop methods"
    Private Sub TreeView_ItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles TreeView.ItemDrag
        ' -----------------------------------------------------------------
        ' User has initiated a drag on a node - the full xml of the node
        ' is stored as the data associated with the drag event args.
        ' -----------------------------------------------------------------
        If Controller.SelectedPaths.IndexOf(e.Item.FullPath) = -1 Then
            TreeView.SelectedNode = e.Item
        End If

        If Controller.SelectedPaths.Count > 0 Then

            Dim FullXML As String = ""
            For Each SelectedPath As String In Controller.SelectedPaths
                ' If Data is in shared then only drag a shortcut. Otherwise drag the full
                ' XML.
                Dim Data As APSIMData = Controller.ApsimData.AllData.Find(SelectedPath)
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
                    FullXML = FullXML + "<" + Data.Type + " name=""" + Data.Name + """ shortcut=""" + Data.Name + """/>"
                Else
                    FullXML = FullXML + Data.XML
                End If
            Next
            Dim AllowedEffects As DragDropEffects
            If Not Controller.ApsimData.IsReadOnly Then
                AllowedEffects = DragDropEffects.Copy Or DragDropEffects.Move
            Else
                AllowedEffects = DragDropEffects.Copy
            End If
            If TreeView.DoDragDrop(FullXML, AllowedEffects) = DragDropEffects.Move Then
                Controller.ApsimData.Delete(Controller.SelectedPaths)
            End If
        End If
    End Sub
    Private Sub TreeView_DragOver(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragOver
        ' --------------------------------------------------
        ' User has dragged a node over us - allow drop?
        ' --------------------------------------------------
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
    Private Sub TreeView_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TreeView.DragDrop
        ' --------------------------------------------------
        ' User has released mouse button during a drag.
        ' Accept the dragged node.
        ' --------------------------------------------------
        Try
            Dim pt As Point = CType(sender, TreeView).PointToClient(New Point(e.X, e.Y))
            Dim DestinationNode As TreeNode = CType(sender, TreeView).GetNodeAt(pt)
            Dim FullXML As String = e.Data.GetData(DataFormats.Text)
            TreeView.SelectedNode = DestinationNode
            Controller.ApsimData.Add(Controller.SelectedPath, FullXML)
        Catch ex As System.Exception
            MessageBox.Show(ex.Message, "Cannot drop the nodes", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub
#End Region

#Region "Mouse events"
    Private Sub TreeView_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TreeView.MouseDown
        ' ---------------------------------------------------------------
        ' If the user right clicks on a node and that node isn't already
        ' selected, then go select it.
        ' ---------------------------------------------------------------
        Dim pt As Point
        Dim DestinationNode As TreeNode
        pt = New Point(e.X, e.Y)
        DestinationNode = CType(sender, TreeView).GetNodeAt(pt)
        If e.Button = Windows.Forms.MouseButtons.Right And Not IsNothing(DestinationNode) Then
            If Controller.SelectedPaths.IndexOf(DestinationNode.FullPath) = -1 Then
                Controller.SelectedPath = DestinationNode.FullPath
            End If
        End If
    End Sub
    Private Sub TreeView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles TreeView.DoubleClick
        ' -----------------------------------------------------
        ' User has double clicked an item. Simply fire an
        ' event. The NewDocumentForm requires this event.
        ' -----------------------------------------------------
        RaiseEvent DoubleClickEvent(Nothing, Nothing)
    End Sub
#End Region

#Region "Paint methods"
    Private NormalForeBrush As Brush = New SolidBrush(SystemColors.WindowText)
    Private NormalBackBrush As Brush = New SolidBrush(SystemColors.Window)
    Private HighlightForeBrush As Brush = New SolidBrush(SystemColors.HighlightText)
    Private HighlightBackBrush As Brush = New SolidBrush(SystemColors.Highlight)
    Private Sub TreeView_DrawNode(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DrawTreeNodeEventArgs) Handles TreeView.DrawNode
        ' Draw the background and node text for a selected node.
        'If (e.State And TreeNodeStates.Selected) <> 0 Then


        ' Draw the background of the selected node. The NodeBounds
        ' method makes the highlight rectangle large enough to
        ' include the text of a node tag, if one is present.

        ' Retrieve the node font. If the node font has not been set,
        ' use the TreeView font.
        If Not DisablePainting Then
            Dim nodeFont As Font = e.Node.NodeFont
            If nodeFont Is Nothing Then
                nodeFont = CType(sender, TreeView).Font
            End If

            Dim ForeBrush As Brush = NormalForeBrush
            Dim BackBrush As Brush = NormalBackBrush
            Dim Selected As Boolean = Controller.SelectedPaths.IndexOf(e.Node.FullPath) <> -1
            If Selected Then
                ForeBrush = HighlightForeBrush
                BackBrush = HighlightBackBrush
            End If

            e.Graphics.FillRectangle(BackBrush, e.Bounds)
            e.Graphics.DrawString(e.Node.Text, nodeFont, ForeBrush, e.Bounds.Left, e.Bounds.Top)
        End If
        e.DrawDefault = False
    End Sub

#End Region

End Class
