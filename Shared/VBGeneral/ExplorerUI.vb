Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.IO
Public Class ExplorerUI
    Inherits BaseView
    Private MyParentForm As Form
    Private MyApplicationName As String
    Private UIs As New ArrayList
    Private UITypes As New StringCollection
    Private CurrentUIIndex As Integer = -1

#Region " Windows Form Designer generated code "

    Public Sub New(ByVal ParentForm As Form)
        MyBase.New()
        InitializeComponent()
        MyParentForm = ParentForm
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
        Me.DataTree = New VBGeneral.DataTree
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

    Public Overrides Sub RefreshView(ByVal Controller As BaseController)
        ' -------------------------------------------------------
        ' Called by parent to refresh ourselves. 
        ' -------------------------------------------------------
        MyBase.RefreshView(Controller)

        If Not IsNothing(MyParentForm) And MyApplicationName = "" Then
            MyApplicationName = MyParentForm.Text
        End If
        RemoveHandler Controller.AllData.DataChanged, AddressOf OnDataChanged
        RemoveHandler Controller.SelectionChangingEvent, AddressOf OnSelectionChanging
        RemoveHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged
        RemoveHandler Controller.BeforeSaveEvent, AddressOf OnBeforeSave
        RemoveHandler Controller.AfterSaveEvent, AddressOf OnAfterSave
        RemoveHandler Controller.RefreshRequiredEvent, AddressOf RefreshView

        AddHandler Controller.AllData.DataChanged, AddressOf OnDataChanged
        AddHandler Controller.SelectionChangingEvent, AddressOf OnSelectionChanging
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged
        AddHandler Controller.BeforeSaveEvent, AddressOf OnBeforeSave
        AddHandler Controller.AfterSaveEvent, AddressOf OnAfterSave
        AddHandler Controller.RefreshRequiredEvent, AddressOf RefreshView
        MyHelpLabel.Visible = False
        UpdateCaption()
        DataTree.RefreshView(Controller)
    End Sub

    WriteOnly Property ExpandAll() As Boolean
        Set(ByVal Value As Boolean)
            ' ------------------------------------------------------
            ' Tell the DataTree to expand all nodes.
            ' ------------------------------------------------------
            DataTree.ExpandAll = Value
        End Set
    End Property

    WriteOnly Property SortAll() As Boolean
        Set(ByVal Value As Boolean)
            ' -------------------------------------------------------
            ' Set the DataTree to display nodes in alphabetical order
            ' -------------------------------------------------------
            DataTree.SortAll = Value
        End Set
    End Property

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
            View.Parent = UIPanel
            View.Dock = DockStyle.Fill
            View.Show()
            View.RefreshView(Controller)
        End If
    End Sub

    Private Sub CloseUI()
        ' -------------------------------------------------
        ' Close the current UI
        ' -------------------------------------------------
        If CurrentUIIndex <> -1 Then
            UIPanel.Controls.Remove(UIs(CurrentUIIndex))
            CurrentUIIndex = -1
        End If
    End Sub

    Private Sub SaveCurrentView()
        ' -----------------------------------------------------
        ' Tell current view to save.
        ' -----------------------------------------------------
        If CurrentUIIndex <> -1 Then
            Dim View As BaseView = UIs(CurrentUIIndex)
            View.Save()
        End If
    End Sub

    Private Sub UpdateCaption()
        ' ----------------------------------------
        ' Called to update the main form's caption
        ' ----------------------------------------
        If Not IsNothing(MyParentForm) Then
            If Controller.DirtyData Then
                MyParentForm.Text = MyApplicationName + " - " + Controller.FileName + "*"
            ElseIf Not Controller.AllowDataChanges Then
                MyParentForm.Text = MyApplicationName + " - " + Controller.FileName + " [readonly]"
            Else
                MyParentForm.Text = MyApplicationName + " - " + Controller.FileName
            End If
        End If
    End Sub

    Private Sub OnSelectionChanging()
        ' -----------------------------------------------------
        ' User is selecting a new node - save current UI
        ' -----------------------------------------------------
        SaveCurrentView()
    End Sub

    Private Sub OnBeforeSave()
        ' -----------------------------------------------------
        ' User is about to do a save.
        ' -----------------------------------------------------
        SaveCurrentView()
    End Sub

    Private Sub OnAfterSave()
        ' -----------------------------------------------------
        ' User has saved the data
        ' -----------------------------------------------------
        UpdateCaption()
    End Sub

    Private Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        ' -----------------------------------------------------
        ' User has selected a node - update user interface
        ' -----------------------------------------------------
        If Controller.SelectedPaths.Count = 1 Then
            ShowUI()
        Else
            CloseUI()
        End If
    End Sub

    Public Sub OnDataChanged(ByVal ChangedData As APSIMData)
        ' --------------------------
        ' User has changed some data
        ' --------------------------
        UpdateCaption()
    End Sub

End Class
