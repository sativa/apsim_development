Imports System.Xml
Imports System.Collections.Specialized
Imports System.Windows.Forms
Imports System.Drawing
Imports VBGeneral
Imports CSGeneral


Public Class DataTree
    Inherits TreeView       'DataTree inherits from TreeView NOT from BaseView, but it still uses BaseController as its go between to the Model.
    'ApsimUI only has 2 Views, BaseView and TreeView. TreeView is the parent of the DataTree, and BaseView is the parent of every other UI in ApsimUI.

    ' ---------------------------------------------------
    ' Tree control for visualising an ApsimFile
    ' ---------------------------------------------------
    Private PopupMenu As New System.Windows.Forms.ContextMenuStrip
    Private Controller As BaseController
    Private FirstTimeRename As Boolean = False
    Private EnableNodeSelection As Boolean = True

    Public Event DoubleClickEvent As EventHandler(Of EventArgs)

    Public Sub OnLoad(ByVal Controller As BaseController)
        ' ---------------------------------------------------
        ' Set ourselves up.
        ' ---------------------------------------------------
        Me.Controller = Controller
        ContextMenuStrip = PopupMenu
        PathSeparator = "/"
        ImageList = Controller.Configuration.ImageList("SmallIcon")
        Controller.ProvideToolStrip(PopupMenu, "ContextMenu")
        ShowNodeToolTips = True
        AddHandler Controller.ApsimData.ComponentChangedEvent, AddressOf OnRefresh
        AddHandler Controller.ApsimData.FileNameChanged, AddressOf OnFileNameChanged
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged
    End Sub
    Private Function GetNodeFromPath(ByVal ChildPath As String) As TreeNode
        ' --------------------------------------------------
        ' Returns a tree node given a fullly delimited path.
        ' --------------------------------------------------
        Dim name As String
        Dim Path As String = ChildPath.Substring(1)
        Dim CurrentNode As TreeNode = Nothing
        Do Until Path = ""
            Dim PosDelimiter As Integer = Path.IndexOf(PathSeparator)
            If PosDelimiter <> -1 Then
                name = Path.Substring(0, PosDelimiter)
                Path = Path.Substring(PosDelimiter + 1)
            Else
                name = Path
                Path = ""
            End If

            Dim ChildNode As TreeNode = Nothing
            If CurrentNode Is Nothing Then
                If Nodes.Count = 0 Then
                    Return Nothing
                Else
                    ChildNode = Nodes(0)
                End If
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
    Private Function GetPathFromNode(ByVal Node As TreeNode)
        Return PathSeparator + Node.FullPath    'just put an extra "/" in front of the node path, so "root/child" becomes "/root/child" (this is needed because our "Selected Path" root starts with a /, whereas the inbuilt node full path property does not start with a / at the root)
    End Function

    Private Sub OnFileNameChanged(ByVal FileName As String)
        ' ---------------------------------------------------------
        ' The file name has changed so expand all folder nodes.
        ' ---------------------------------------------------------
        If Nodes.Count = 1 Then
            Dim RootNode As TreeNode = Nodes(0)
            CollapseAll()
            RootNode.Expand()
            If RootNode.Nodes.Count = 1 AndAlso _
               (RootNode.Nodes(0).Tag.ToString.ToLower = "simulation") Then
                ExpandAll()
            ElseIf RootNode.Nodes.Count = 1 AndAlso _
                     RootNode.Nodes(0).Text.ToString.ToLower = "australia" Then
                RootNode.Nodes(0).Expand()
            End If
            Controller.SelectedPath = Controller.ApsimData.RootComponent.FullPath
        End If
    End Sub
    Private Sub ExpandAllFolders(ByVal Node As TreeNode)
        If Node.Tag.ToString.ToLower = "folder" Then
            Dim ThereAreSubFolders As Boolean = False
            For Each Child As TreeNode In Node.Nodes
                If Child.Tag.ToString.ToLower = "folder" Then
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

#Region "Refresh methods"
    Private Overloads Sub OnRefresh(ByVal Comp As ApsimFile.Component)
        ' ----------------------------------------------
        ' Do a refresh from the specified Comp down
        ' ----------------------------------------------
        Windows.Forms.Cursor.Current = Cursors.WaitCursor       'set the cursor object (usually an arrow) to the wait cursor (usually an hourglass)
        BeginUpdate()                                           'inbuilt tree function, it disables redrawing of the tree

        Try

            'If (the tree has no nodes) OR (Comp [the component parameter this sub was passed] is Null)
            If (Nodes.Count = 0) Or (Comp Is Nothing) Then
                Nodes.Clear()                                   'get rid of all the nodes in the tree 
                Dim RootNode As TreeNode = Nodes.Add(Controller.ApsimData.RootComponent.Name)   'create the root node from the root component and add it to the tree.
                RefreshNodeAndChildren(RootNode, Controller.ApsimData.RootComponent)            'refresh the tree from the root node down.

                'Get the node you want to refresh 
            Else
                Dim NodeToRefresh As TreeNode = GetNodeFromPath(Comp.FullPath)                  'get the corresponding node for the component this sub was passed
                If IsNothing(NodeToRefresh) Then
                    NodeToRefresh = Nodes(0)
                End If
                RefreshNodeAndChildren(NodeToRefresh, Comp)                                     'refresh the tree from this node down.
            End If
        Catch ex As Exception
            EndUpdate()                                         'inbuilt tree function, reinables redrawing of the tree
            Windows.Forms.Cursor.Current = Cursors.Default      'set the cursor object back to the default windows cursor (usually an arrow)
            Throw
        End Try

        'If multiple nodes are selected set the tree's selected node to the first one.

        'NB. Windows inbuilt Tree control does not allow you to select more then one node.
        '   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see ColourNode sub in the 'Selection methods' Region) 
        '   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.

        If (Controller.SelectedPaths.Count > 0) Then
            EnableNodeSelection = False                         'don't let the user click any other nodes while this code executes
            SelectedNode = GetNodeFromPath(Controller.SelectedPaths(0))     'set tree's selected node property to the first item in the SelectedPaths. The tree control complains if you don't have something set as the SelectedNode, but we don't use it.
            EnableNodeSelection = True                          'let the user click on other nodes again
        End If


        EndUpdate()                                             'inbuilt tree function, reinables redrawing of the tree
        Windows.Forms.Cursor.Current = Cursors.Default          'set the cursor object back to the default windows cursor (usually an arrow)
    End Sub
    Private Sub RefreshNodeAndChildren(ByVal Node As TreeNode, ByVal Comp As ApsimFile.Component)
        ' --------------------------------------------------
        ' Recursively refresh the specified treenode and its
        ' child nodes in the tree.
        ' --------------------------------------------------

        ' Refresh the specified node first.
        Node.Text = Comp.Name
        Node.ImageIndex = Controller.Configuration.ImageIndex(Comp.Type, "SmallIcon")
        Node.SelectedImageIndex = Node.ImageIndex
        Node.Tag = Comp.Type
        Node.ToolTipText = Comp.Description
        If Not IsNothing(Comp.ShortCutTo) Then
            Node.ToolTipText = "Linked to " + Comp.ShortCutTo.FullPath
            If Not Comp.Enabled Then
                Node.ToolTipText = "Disabled: " + Node.ToolTipText
            End If
        End If
        If Not Comp.Enabled Then
            Node.ToolTipText = "Disabled" + Node.ToolTipText
        End If
        ColourNode(Node)
        ' Go refresh all children.
        Dim ChildIndex As Integer = 0
        For Each Child As ApsimFile.Component In Comp.ChildNodes
            Dim ChildTreeNode As TreeNode
            If ChildIndex < Node.Nodes.Count Then
                ChildTreeNode = Node.Nodes(ChildIndex)
            Else
                ChildTreeNode = Node.Nodes.Add(Child.Name)
            End If
            RefreshNodeAndChildren(ChildTreeNode, Child)
            ChildIndex = ChildIndex + 1
        Next
        While Node.Nodes.Count > ChildIndex
            Node.Nodes.Remove(Node.Nodes(Node.Nodes.Count - 1))
        End While
    End Sub
#End Region

#Region "Selection methods"
    Private PreviousNode As TreeNode
    Private Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        ' -----------------------------------------------------------------
        ' Selection has changed - update tree.
        ' -----------------------------------------------------------------

        'NB. Windows inbuilt Tree control does not allow you to select more then one node.
        '   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see the ColourNode sub below) 
        '   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.


        EnableNodeSelection = False     'don't let the user click any other nodes while this code executes

        'Change the colour of all the old selected nodes to the "unselected" colours
        For Each NodePath As String In OldSelections
            Dim Node As TreeNode = GetNodeFromPath(NodePath)    'get the node that the old selected path points to
            If Not IsNothing(Node) Then
                ColourNode(Node)                                'change the colour of the unselected node to the unselected colours.
            End If

        Next

        'Change the colour of all the new selected nodes to the "selected" colours
        For Each NodePath As String In NewSelections
            Dim Node As TreeNode = GetNodeFromPath(NodePath)    'get the node that the new selected path points to.
            SelectedNode = Node         'set the Tree's selected node to the node specified in the new selected path (just used to trigger the AfterSelect event, which is handled by OnTreeSelectionChanged() subroutine below this subroutine) (nb. we REDO this for EVERY node in NewSelections. We have to do this one node at a time because the Tree does not allow you to select more then one node) 
            ColourNode(Node)            'change the colour of the new selected node to the selected colours.
            Node.EnsureVisible()        'use inbuilt tree node function that expands the tree to make sure the node specified is visible in the tree.  
        Next

        EnableNodeSelection = True      'let the user click on other nodes again
    End Sub
    Private Sub OnTreeSelectionChanged(ByVal Sender As Object, ByVal e As TreeViewEventArgs) Handles Me.AfterSelect
        If EnableNodeSelection Then
            Controller.SelectedPath = GetPathFromNode(e.Node)
        End If
    End Sub

#End Region

    Private LinkFont As Font = New System.Drawing.Font(Me.Font.FontFamily, Me.Font.Size, FontStyle.Underline)
    Private UnLinkFont As Font = New System.Drawing.Font(Me.Font.FontFamily, Me.Font.Size, FontStyle.Regular)
    Private Sub ColourNode(ByVal Node As TreeNode)

        'NB. Windows inbuilt Tree control does not allow you to select more then one node.
        '   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see the code below) 
        '   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.

        'If the node is linked to another node  
        If Node.ToolTipText.IndexOf("Linked to") = 0 Then   'nb. ToolTipText is the text that appears when you hover the mouse over the node. IndexOf just returns the index of the first occurance of one string in another.
            Node.ForeColor = Color.Blue                     'colour to blue
            Node.NodeFont = LinkFont                        'font to underlined (see LinkFont variable declared just above this ColourNode function)
            Node.BackColor = BackColor                      'back colour to default back colour for the tree

            'If the node is disabled 
        ElseIf Node.ToolTipText.IndexOf("Disabled") = 0 Then
            Node.ForeColor = SystemColors.InactiveCaptionText   'colour to the default colour for a windows system disabled element 
            Node.BackColor = SystemColors.InactiveCaption       'back colour to the default back colour for a disabled item in windows 

            'If it's just a normal node
        Else
            Node.ForeColor = Color.Black    'colour to black
            Node.BackColor = BackColor      'back colour to default back colour for the tree
            Node.NodeFont = UnLinkFont      'font to regular (see UnLinkFont variable declared just above this ColourNode function)

        End If

        'If the node is a selected node
        If Controller.SelectedPaths.IndexOf(GetPathFromNode(Node)) <> -1 Then       'this IndexOf is for a string collection NOT a string. So it it checks every string in the collection for an exact match with the search string. If it finds one it returns that strings index in the collection.
            Node.ForeColor = SystemColors.HighlightText             'colour to the default colour for a selected item in windows
            Node.BackColor = SystemColors.Highlight                 'back colour to the default back colour for a selected item in windows
        End If
    End Sub


#Region "Rename methods"    '(rename done by doing 2 seperate clicks (See "Left Click Only" code in TreeView_MouseDown). NOT by a right mouse click then selecting rename, this is handled by Rename() sub in BaseAction.vb)   

    'event handlers for a Node.BeginEdit()

    Private Sub OnBeforeEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles Me.BeforeLabelEdit
        ' ---------------------------------------------------
        ' User is about to start editing a tree node.
        ' We must disable the popup menu because if the user
        ' hits DELETE while editing the node, the ACTION
        ' will trigger, deleting the whole node rather than
        ' the bit of text on the node caption.
        ' ---------------------------------------------------
        PopupMenu.Enabled = False
    End Sub

    Private Sub OnAfterEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles Me.AfterLabelEdit
        ' ---------------------------------------------------
        ' User has just finished editing the label of a node.
        ' ---------------------------------------------------

        If Not FirstTimeRename Then
            If Not IsNothing(e.Label) Then
                ' Firstly empty the current selections.
                Controller.SelectedPath = ""

                ' Change the data
                Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(GetPathFromNode(e.Node))
                Comp.Name = e.Label

                ' Now tell the base controller about the new selections.
                Controller.SelectedPath = GetPathFromNode(e.Node.Parent) + "/" + e.Label
            End If
            LabelEdit = False
        End If
        FirstTimeRename = False
        PopupMenu.Enabled = True
    End Sub
#End Region

#Region "Drag / Drop methods"
    Private PathsBeingDragged As StringCollection
    Private Sub TreeView_ItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles Me.ItemDrag
        ' -----------------------------------------------------------------
        ' User has initiated a drag on a node - the full xml of the node
        ' is stored as the data associated with the drag event args.
        ' -----------------------------------------------------------------
        If Controller.SelectedPaths.IndexOf(GetPathFromNode(e.Item)) = -1 Then
            SelectedNode = e.Item
        End If

        Dim FullXML As String = ""

        For Each SelectedPath As String In Controller.SelectedPaths
            Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(SelectedPath)
            FullXML = FullXML + Comp.FullXML
        Next
        PathsBeingDragged = Controller.SelectedPaths
        DoDragDrop(FullXML, DragDropEffects.Copy Or DragDropEffects.Link Or DragDropEffects.Move)
    End Sub
    Private Sub TreeView_DragOver(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragOver
        ' --------------------------------------------------
        ' User has dragged a node over us - allow drop?
        ' --------------------------------------------------
        If e.Data.GetDataPresent(GetType(System.String)) Then
            Dim pt As Point = PointToClient(New Point(e.X, e.Y))
            Dim DestinationNode As TreeNode = GetNodeAt(pt)
            If Not IsNothing(DestinationNode) Then
                Dim FullXML As String = e.Data.GetData(DataFormats.Text)
                Dim DropComp As ApsimFile.Component = Controller.ApsimData.Find(GetPathFromNode(DestinationNode))
                If DropComp.AllowAdd(FullXML) Then
                    If Not IsNothing(PathsBeingDragged) AndAlso PathsBeingDragged.Count > 0 AndAlso (Control.ModifierKeys And Keys.Shift) = Keys.Shift Then
                        e.Effect = DragDropEffects.Move
                    ElseIf Not IsNothing(PathsBeingDragged) AndAlso PathsBeingDragged.Count > 0 And (Control.ModifierKeys And Keys.Alt) = Keys.Alt Then
                        e.Effect = DragDropEffects.Link
                    Else
                        e.Effect = DragDropEffects.Copy
                    End If
                Else
                    e.Effect = DragDropEffects.None
                End If
            End If
        End If
    End Sub
    Private Sub TreeView_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragDrop
        ' --------------------------------------------------
        ' User has released mouse button during a drag.
        ' Accept the dragged node.
        ' --------------------------------------------------
        Dim pt As Point = CType(sender, TreeView).PointToClient(New Point(e.X, e.Y))
        Dim DestinationNode As TreeNode = CType(sender, TreeView).GetNodeAt(pt)
        Dim FullXML As String = e.Data.GetData(DataFormats.Text)
        Controller.SelectedPath = GetPathFromNode(DestinationNode)
        If e.Effect = DragDropEffects.Copy Then
            Controller.Selection.Add(FullXML)
        ElseIf e.Effect = DragDropEffects.Link Then
            For Each DraggedPath As String In PathsBeingDragged
                Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(DraggedPath)
                If Not IsNothing(Comp) Then
                    Controller.Selection.AddShortCut(Comp)
                End If
            Next
        Else
            Controller.Selection.Add(FullXML)
            For Each DraggedPath As String In PathsBeingDragged
                Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(DraggedPath)
                If Not IsNothing(Comp) Then
                    Comp.Parent.Delete(Comp)
                End If
            Next
        End If
        If Not IsNothing(PathsBeingDragged) Then
            PathsBeingDragged.Clear()
        End If
    End Sub
#End Region

#Region "Mouse events"
    Private Sub TreeView_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown
        ' ---------------------------------------------------------------
        ' If the user right clicks on a node and that node isn't already
        ' selected, then go select it.
        ' ---------------------------------------------------------------

        'Initialise variables

        EnableNodeSelection = False                         'don't let the user click any other nodes while this code executes

        Dim ClickedNode As TreeNode = GetNodeAt(e.Location) 'get the node in the datatree that was clicked on.
        If IsNothing(ClickedNode) Then                      'if the button was pressed on a non node area of the datatree
            Return                                          'do nothing.
        End If
        If (e.X < ClickedNode.Bounds.Left - 20) Then        '20 pixels allowing for an image to the left of the text of a node
            Return
        End If

        Dim RightClick As Boolean = (e.Button = Windows.Forms.MouseButtons.Right)   'is it a right mouse button click
        Dim Control As Boolean = (ModifierKeys = Keys.Control)                      'is control button on keyboard pressed 
        Dim Shift As Boolean = (ModifierKeys = Keys.Shift)                          'is shift button on keyboard pressed

        Dim SelectedPaths As StringCollection = Controller.SelectedPaths    'get the selected paths from the controller. (stores more then one nodes path because the user can hold down the control key and select more then one node)




        'Left Click with a Control button OR Right Click                   'TODO: this may execute for CTRL + Centre Mouse Click too, you may want to specifically exclude this later.

        If Control Then
            If SelectedPaths.IndexOf(GetPathFromNode(ClickedNode)) = -1 Then    'check to see if the clicked node has been clicked on already (this IndexOf is for a string collection NOT a string. So it it checks every string in the collection for an exact match with the search string. If it finds one it returns that strings index in the collection)
                SelectedPaths.Add(GetPathFromNode(ClickedNode))                 'if not then add it's path to the list of selected paths
                PreviousNode = ClickedNode                                      'store clicked node as the previously clicked on node (used for Shift button)
            End If


            'Left Click with a Shift button

        ElseIf Shift Then
            If Not IsNothing(PreviousNode) AndAlso PreviousNode.Parent.Equals(ClickedNode.Parent) Then
                Dim FirstIndex As Integer = PreviousNode.Index      'set to the index of the previously clicked on node
                Dim LastIndex As Integer = ClickedNode.Index        'set to the index of the clicked node
                If FirstIndex > LastIndex Then
                    Dim TempIndex As Integer = LastIndex
                    LastIndex = FirstIndex
                    FirstIndex = TempIndex
                End If
                SelectedPaths.Clear()                               'get rid of old selected paths
                For i As Integer = FirstIndex To LastIndex          'add the paths for all the node's between the first index and the last index, to the list of selected paths 
                    SelectedPaths.Add(GetPathFromNode(PreviousNode.Parent.Nodes(i)))
                Next
                PreviousNode = ClickedNode                          'store clicked node as the previously clicked on node (incase Shift button is used on the next click as well)
            End If


            'Right Click Only

        ElseIf RightClick Then
            SelectedPaths.Clear()                               'get rid of existing selected paths
            SelectedPaths.Add(GetPathFromNode(ClickedNode))     'add new path to selected paths
            PreviousNode = ClickedNode                          'store clicked node as the previously clicked on node (used for Shift button)


            'Left Click Only

        ElseIf Not IsNothing(ClickedNode) Then
            'click on same thing that was already selected.
            If Not Controller.ApsimData.IsReadOnly _
                    AndAlso SelectedPaths.Count = 1 _
                    AndAlso SelectedPaths(0) = GetPathFromNode(ClickedNode) _
                    AndAlso ClickedNode.Level > 0 Then
                'if not readonly, and user has clicked once before, and what they clicked this time is the same as what they clicked last time, and they have clicked on a node that is lower down then the root node. 
                LabelEdit = True        'set the tree's label edit property  to true, allowing all the nodes on the tree to have their labels edited. (needs to be set to true for Node.BeginEdit() to work)  
                FirstTimeRename = True
                ClickedNode.BeginEdit() 'call the inbuilt tree node function that allows the user to edit the nodes label. (see OnBeforeEdit and OnAfterEdit sub for what happens before and after the user edits the label)
                Exit Sub

                'click on something different to what was already selected.
            Else
                SelectedPaths.Clear()                               'get rid of existing selected paths
                SelectedPaths.Add(GetPathFromNode(ClickedNode))     'add new path to selected paths
                PreviousNode = ClickedNode                          'store clicked node as the previously clicked on node (used for Shift button)
            End If
        End If


        'Finish off

        Controller.SelectedPaths = SelectedPaths        'update the selected paths "in the controller"

        EnableNodeSelection = True                      'let the user click on other nodes again
    End Sub
    Private Sub TreeView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.DoubleClick
        ' -----------------------------------------------------
        ' User has double clicked an item. Simply fire an
        ' event. The NewDocumentForm requires this event.
        ' -----------------------------------------------------
        RaiseEvent DoubleClickEvent(Nothing, Nothing)
    End Sub
#End Region

    Private Sub InitializeComponent()
        Me.SuspendLayout()
        '
        'DataTree
        '
        Me.ForeColor = System.Drawing.Color.Goldenrod
        Me.HideSelection = False
        Me.ResumeLayout(False)

    End Sub

End Class
