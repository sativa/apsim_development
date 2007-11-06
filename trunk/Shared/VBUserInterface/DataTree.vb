Imports System.Xml
Imports System.Collections.Specialized
Imports System.Windows.Forms
Imports System.Drawing
Imports VBGeneral
Imports CSGeneral


Public Class DataTree
    Inherits TreeView

    ' ---------------------------------------------------
    ' Tree control for visualising an ApsimFile
    ' ---------------------------------------------------
    Private DisablePainting = False
    Private LastNode As TreeNode
    Private FirstNode As TreeNode
    Private PopupMenu As New System.Windows.Forms.ContextMenuStrip
    Private Controller As BaseController
    Private FirstTimeRename As Boolean = False

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
        Return PathSeparator + Node.FullPath
    End Function

    Private Sub OnFileNameChanged(ByVal FileName As String)
        ' ---------------------------------------------------------
        ' The file name has changed so expand all folder nodes.
        ' ---------------------------------------------------------
        CollapseAll()
        If Nodes.Count = 1 AndAlso _
           Nodes(0).Nodes.Count > 0 AndAlso _
           Nodes(0).Nodes(0).Tag.ToString.ToLower = "simulation" Then
            ExpandAll()
        Else
            ExpandAllFolders(Nodes(0))
        End If
        Controller.SelectedPath = Controller.ApsimData.RootComponent.FullPath
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
        Windows.Forms.Cursor.Current = Cursors.WaitCursor
        BeginUpdate()
        DisablePainting = True

        Try
            Dim NodeToRefresh As TreeNode = GetNodeFromPath(Comp.FullPath)
            If Nodes.Count = 0 Or NodeToRefresh Is Nothing Then
                Nodes.Clear()
                Dim RootNode As TreeNode = Nodes.Add(Controller.ApsimData.RootComponent.Name)
                RefreshNodeAndChildren(RootNode, Controller.ApsimData.RootComponent)
            Else
                RefreshNodeAndChildren(NodeToRefresh, Comp)
            End If
        Catch ex As Exception
            DisablePainting = False
            EndUpdate()
            Windows.Forms.Cursor.Current = Cursors.Default
            Throw
        End Try

        DisablePainting = False
        EndUpdate()
        Windows.Forms.Cursor.Current = Cursors.Default
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

        For Each NodePath As String In OldSelections
            Dim Node As TreeNode = GetNodeFromPath(NodePath)
            If Not IsNothing(Node) Then
                ColourNode(Node)
            End If

        Next
        For Each NodePath As String In NewSelections
            Dim Node As TreeNode = GetNodeFromPath(NodePath)
            ColourNode(Node)
            Node.EnsureVisible()
        Next
    End Sub
#End Region

    Private Sub ColourNode(ByVal Node As TreeNode)
        If Node.ToolTipText.IndexOf("Linked to") = 0 Then
            Node.ForeColor = SystemColors.HotTrack
            Node.BackColor = BackColor
        ElseIf Controller.SelectedPaths.IndexOf(GetPathFromNode(Node)) = -1 Then
            Node.ForeColor = ForeColor
            Node.BackColor = BackColor
        Else
            Node.ForeColor = SystemColors.HighlightText
            Node.BackColor = SystemColors.Highlight
            SelectedNode = Node
        End If
    End Sub


#Region "Rename methods"
    Private Sub TreeView_AfterLabelEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles Me.AfterLabelEdit
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
            ' If Data is in shared then only drag a shortcut. Otherwise drag the full
            ' XML.
            Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(SelectedPath)
            'Dim P As XmlNode = Data.Parent
            FullXML = FullXML + Comp.Contents
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
        PathsBeingDragged.Clear()
    End Sub
#End Region

#Region "Mouse events"
    Private Sub TreeView_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown
        ' ---------------------------------------------------------------
        ' If the user right clicks on a node and that node isn't already
        ' selected, then go select it.
        ' ---------------------------------------------------------------
        Dim pt As Point
        pt = New Point(e.X, e.Y)
        Dim ClickedNode As TreeNode = CType(sender, TreeView).GetNodeAt(pt)

        Dim Control As Boolean = (ModifierKeys = Keys.Control Or e.Button = Windows.Forms.MouseButtons.Right)
        Dim Shift As Boolean = (ModifierKeys = Keys.Shift)

        Dim SelectedPaths As StringCollection = Controller.SelectedPaths
        If Control Then
            If SelectedPaths.IndexOf(GetPathFromNode(ClickedNode)) = -1 Then
                SelectedPaths.Add(GetPathFromNode(ClickedNode))
                PreviousNode = ClickedNode
            End If
        ElseIf Shift Then
            If Not IsNothing(PreviousNode) AndAlso PreviousNode.Parent.Equals(ClickedNode.Parent) Then
                Dim FirstIndex As Integer = PreviousNode.Index
                Dim LastIndex As Integer = ClickedNode.Index
                If FirstIndex > LastIndex Then
                    Dim TempIndex As Integer = LastIndex
                    LastIndex = FirstIndex
                    FirstIndex = TempIndex
                End If
                SelectedPaths.Clear()
                For i As Integer = FirstIndex To LastIndex
                    SelectedPaths.Add(GetPathFromNode(PreviousNode.Parent.Nodes(i)))
                Next
                PreviousNode = ClickedNode
            End If
        Else
            If SelectedPaths.Count = 1 AndAlso SelectedPaths(0) = GetPathFromNode(ClickedNode) And _
                ClickedNode.Level > 0 Then
                LabelEdit = True
                FirstTimeRename = True
                ClickedNode.BeginEdit()
                Exit Sub
            Else
                SelectedPaths.Clear()
                SelectedPaths.Add(GetPathFromNode(ClickedNode))
                PreviousNode = ClickedNode
            End If
        End If

        Controller.SelectedPaths = SelectedPaths
    End Sub
    Private Sub TreeView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.DoubleClick
        ' -----------------------------------------------------
        ' User has double clicked an item. Simply fire an
        ' event. The NewDocumentForm requires this event.
        ' -----------------------------------------------------
        RaiseEvent DoubleClickEvent(Nothing, Nothing)
    End Sub
#End Region

End Class
