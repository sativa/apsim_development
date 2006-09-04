Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.IO
Public Class ExplorerUI
    Inherits BaseView
    Private MyCurrentUI As BaseView
    Private MyParentForm As Form
    Private MyApplicationName As String
    Private CurrentUIType As String = ""


#Region " Windows Form Designer generated code "

    Public Sub New(ByVal ParentForm As Form, _
                    ByVal App As BaseController)
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Get application name
        MyParentForm = ParentForm
        MyBase.Controller = App
        DataTree.Controller = Controller
        If Not IsNothing(MyParentForm) Then
            MyApplicationName = MyParentForm.Text
        End If
        AddHandler Controller.DataChangedEvent, AddressOf UpdateCaption
        AddHandler Controller.SelectionChangingEvent, AddressOf OnSelectionChanging
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged
        AddHandler Controller.NewDataEvent, AddressOf OnNewDataEvent
        AddHandler Controller.BeforeSaveEvent, AddressOf OnBeforeSave
        MyHelpLabel.Visible = False
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
        Me.DataTree.Controller = Nothing
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Left
        Me.DataTree.HelpText = ""
        Me.DataTree.Location = New System.Drawing.Point(0, 40)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(256, 760)
        Me.DataTree.Sorted = False
        Me.DataTree.TabIndex = 3
        '
        'Splitter
        '
        Me.Splitter.Location = New System.Drawing.Point(256, 40)
        Me.Splitter.Name = "Splitter"
        Me.Splitter.Size = New System.Drawing.Size(5, 760)
        Me.Splitter.TabIndex = 4
        Me.Splitter.TabStop = False
        '
        'UIPanel
        '
        Me.UIPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.UIPanel.Location = New System.Drawing.Point(261, 40)
        Me.UIPanel.Name = "UIPanel"
        Me.UIPanel.Size = New System.Drawing.Size(469, 760)
        Me.UIPanel.TabIndex = 5
        '
        'ExplorerUI
        '
        Me.Controls.Add(Me.UIPanel)
        Me.Controls.Add(Me.Splitter)
        Me.Controls.Add(Me.DataTree)
        Me.Name = "ExplorerUI"
        Me.Size = New System.Drawing.Size(730, 800)
        Me.Controls.SetChildIndex(Me.DataTree, 0)
        Me.Controls.SetChildIndex(Me.Splitter, 0)
        Me.Controls.SetChildIndex(Me.UIPanel, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    ' ------------------------------------------------------
    ' Set the expandAll property
    ' ------------------------------------------------------
    WriteOnly Property ExpandAll() As Boolean
        Set(ByVal Value As Boolean)
            DataTree.ExpandAll = Value
        End Set
    End Property


    ' ------------------------------------------------------
    ' Set the SortAll property
    ' ------------------------------------------------------
    WriteOnly Property SortAll() As Boolean
        Set(ByVal Value As Boolean)
            DataTree.SortAll = Value
        End Set
    End Property


    ' -------------------------------------------------
    ' Create and show a specific UI depending on the
    ' specified user interface type.
    ' -------------------------------------------------
    Public Sub ShowUI(ByVal Data As APSIMData)
        If IsNothing(MyCurrentUI) OrElse CurrentUIType <> Data.Type Then
            CloseUI()
            MyCurrentUI = Controller.CreateUI(Data.Type)
        End If
        If Not IsNothing(MyCurrentUI) Then
            If MyCurrentUI.Controller Is Nothing Then
                MyCurrentUI.Controller = Controller
            ElseIf Not MyCurrentUI.Controller Is Controller Then
                MyCurrentUI.Controller.AllData = Data
                MyCurrentUI.Refresh()
            Else
                MyCurrentUI.Refresh()
            End If
            MyCurrentUI.Parent = UIPanel
            MyCurrentUI.Dock = DockStyle.Fill
            MyCurrentUI.Show()
            Me.Visible = True
            CurrentUIType = Data.Type
        Else
            MyCurrentUI = Nothing
        End If
        DataTree.Visible = Data.Type.ToLower() <> "startup"
        Splitter.Visible = DataTree.Visible
    End Sub


    ' -------------------------------------------------
    ' Close the current UI
    ' -------------------------------------------------
    Private Sub CloseUI()
        If MyCurrentUI Is Nothing Then
            'Dont need to do anthing here
        Else
            UIPanel.Controls.Remove(MyCurrentUI)
            MyCurrentUI.Dispose()
            MyCurrentUI = Nothing
            UIPanel.Visible = True
        End If
    End Sub


    ' ----------------------------------------
    ' Called to update the main form's caption
    ' ----------------------------------------
    Private Sub UpdateCaption()
        If Not IsNothing(MyParentForm) Then
            If Controller.DirtyData Then
                MyParentForm.Text = MyApplicationName + " - " + Controller.FileName + "*"
            ElseIf Not Controller.AllowChanges Then
                MyParentForm.Text = MyApplicationName + " - " + Controller.FileName + " [readonly]"
            Else
                MyParentForm.Text = MyApplicationName + " - " + Controller.FileName
            End If
        End If
    End Sub

    ' -----------------------------------------------------
    ' User is selecting a new node - save current UI
    ' -----------------------------------------------------
    Public Sub OnSelectionChanging()
        If Not MyCurrentUI Is Nothing Then
            MyCurrentUI.Save()
        End If
    End Sub


    ' -----------------------------------------------------
    ' User is about to do a save.
    ' -----------------------------------------------------
    Public Sub OnBeforeSave()
        If Not MyCurrentUI Is Nothing Then
            DataTree.Focus()
            MyCurrentUI.Save()
        End If
    End Sub


    ' -----------------------------------------------------
    ' User has selected a node - update user interface
    ' -----------------------------------------------------
    Public Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        If Controller.SelectedPaths.Count = 1 Then
            ShowUI(Controller.Data)
        Else
            CloseUI()
        End If
    End Sub


    ' -----------------------------------------------------
    ' User has selected new data - refresh ourselves.
    ' -----------------------------------------------------
    Public Sub OnNewDataEvent()
        UpdateCaption()
    End Sub


    ' -----------------------------------------------
    ' The data structure has changed - refresh the ui
    ' -----------------------------------------------
    Public Sub OnRename(ByVal OldNodeName As String, ByVal NewNodeName As String)
        If Not IsNothing(MyCurrentUI) Then
            If Not MyCurrentUI.Controller Is Controller Then
                MyCurrentUI.Controller.AllData = Controller.Data
                MyCurrentUI.Refresh()
            End If
        End If
    End Sub
End Class
