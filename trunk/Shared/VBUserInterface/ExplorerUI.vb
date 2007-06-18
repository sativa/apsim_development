Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.IO
Imports VBGeneral

Public Class ExplorerUI
    Inherits BaseView
    Private UIs As New ArrayList
    Private UITypes As New StringCollection
    Private CurrentUIIndex As Integer = -1
    Private Controller As BaseController

#Region " Windows Form Designer generated code "

    Public Sub New(ByVal controller As BaseController)
        MyBase.New()
        InitializeComponent()
    End Sub

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
    Friend WithEvents Splitter As System.Windows.Forms.Splitter
    Friend WithEvents UIPanel As System.Windows.Forms.Panel
    Friend WithEvents DataTree As DataTree
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.DataTree = New DataTree
        Me.Splitter = New System.Windows.Forms.Splitter
        Me.UIPanel = New System.Windows.Forms.Panel
        Me.SuspendLayout()
        '
        'DataTree
        '
        Me.DataTree.AllowDrop = True
        Me.DataTree.AutoScroll = True
        Me.DataTree.BackColor = System.Drawing.SystemColors.Control
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Left
        Me.DataTree.HelpText = ""
        Me.DataTree.Location = New System.Drawing.Point(0, 0)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(256, 828)
        Me.DataTree.Sorted = False
        Me.DataTree.TabIndex = 3
        '
        'Splitter
        '
        Me.Splitter.Location = New System.Drawing.Point(256, 0)
        Me.Splitter.Name = "Splitter"
        Me.Splitter.Size = New System.Drawing.Size(5, 828)
        Me.Splitter.TabIndex = 4
        Me.Splitter.TabStop = False
        '
        'UIPanel
        '
        Me.UIPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.UIPanel.Location = New System.Drawing.Point(261, 0)
        Me.UIPanel.Name = "UIPanel"
        Me.UIPanel.Size = New System.Drawing.Size(759, 828)
        Me.UIPanel.TabIndex = 5
        '
        'ExplorerUI
        '
        Me.Controls.Add(Me.UIPanel)
        Me.Controls.Add(Me.Splitter)
        Me.Controls.Add(Me.DataTree)
        Me.Name = "ExplorerUI"
        Me.Size = New System.Drawing.Size(1020, 828)
        Me.Controls.SetChildIndex(Me.DataTree, 0)
        Me.Controls.SetChildIndex(Me.Splitter, 0)
        Me.Controls.SetChildIndex(Me.UIPanel, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overrides Sub OnLoad(ByVal Controller As BaseController)
        Me.Controller = Controller
        DataTree.OnLoad(Controller)
        AddHandler Controller.ApsimData.DataStructureChangedEvent, AddressOf RefreshView
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged
        AddHandler Controller.BeforeSaveEvent, AddressOf OnBeforeSave
    End Sub
    Public Overrides Sub RefreshView(ByVal NodePath As String)
        ' -------------------------------------------------------
        ' Called by parent to refresh ourselves. 
        ' -------------------------------------------------------
        Visible = True
        MyHelpLabel.Visible = False
    End Sub

    WriteOnly Property SortAll() As Boolean
        Set(ByVal Value As Boolean)
            ' -------------------------------------------------------
            ' Set the DataTree to display nodes in alphabetical order
            ' -------------------------------------------------------
            DataTree.SortAll = Value
        End Set
    End Property

    Public Sub ExpandAllFolders()
        DataTree.ExpandAllFolders()
    End Sub

    Public Sub ExpandAll()
        DataTree.ExpandAll()
    End Sub
    Public Sub CollapseAll()
        DataTree.CollapseAll()
    End Sub

    Private Sub ShowUI()
        ' -------------------------------------------------
        ' Create and show a specific UI depending on the
        ' currently selected data
        ' -------------------------------------------------
        If CurrentUIIndex = -1 OrElse UITypes(CurrentUIIndex) <> Controller.Data.Type Then
            CloseUI()
            CurrentUIIndex = UITypes.IndexOf(Controller.Data.Type)
            If CurrentUIIndex = -1 Then
                Dim View As BaseView = Controller.CreateUI(Controller.Data.Type)
                If Not IsNothing(View) Then
                    UIs.Add(View)
                    UITypes.Add(Controller.Data.Type)
                    CurrentUIIndex = UIs.Count - 1
                End If
            End If
        End If
        If CurrentUIIndex <> -1 Then
            Dim View As BaseView = UIs(CurrentUIIndex)
            View.OnLoad(Controller)
            View.Parent = UIPanel
            View.Dock = DockStyle.Fill
            View.Show()
            View.RefreshView(Controller.Data.FullPath)
        End If
    End Sub

    Private Sub CloseUI()
        ' -------------------------------------------------
        ' Close the current UI
        ' -------------------------------------------------
        If CurrentUIIndex <> -1 Then
            Dim View As BaseView = UIs(CurrentUIIndex)
            SaveCurrentView()
            View.OnClose()
            UIPanel.Controls.Remove(View)
            CurrentUIIndex = -1
        End If
    End Sub

    Public Sub SaveCurrentView()
        ' -----------------------------------------------------
        ' Tell current view to save.
        ' -----------------------------------------------------
        If CurrentUIIndex <> -1 Then
            Dim View As BaseView = UIs(CurrentUIIndex)
            View.Save()
        End If
    End Sub

    Private Sub OnBeforeSave()
        ' -----------------------------------------------------
        ' User is about to do a save.
        ' -----------------------------------------------------
        If Controller.SelectedPaths.Count = 1 AndAlso Not IsNothing(Controller.Data) Then
            SaveCurrentView()
        End If
    End Sub


    Private Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        ' -----------------------------------------------------
        ' User has selected a node - update user interface
        ' -----------------------------------------------------
        Visible = True
        If Controller.SelectedPaths.Count = 1 Then
            ShowUI()
        Else
            CloseUI()
        End If
    End Sub


End Class
