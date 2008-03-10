Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.IO
Imports System.xml
Imports VBGeneral
Imports CSGeneral

Public Class ExplorerUI
    Inherits UserControl
    Private UIs As New ArrayList
    Private UITypes As New StringCollection
    Private CurrentUIIndex As Integer = -1
    Private Controller As BaseController

#Region " Windows Form Designer generated code "

    Public Sub New()
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
        Me.DataTree = New VBUserInterface.DataTree
        Me.Splitter = New System.Windows.Forms.Splitter
        Me.UIPanel = New System.Windows.Forms.Panel
        Me.SuspendLayout()
        '
        'DataTree
        '
        Me.DataTree.AllowDrop = True
        Me.DataTree.BackColor = System.Drawing.SystemColors.Window
        Me.DataTree.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Left
        Me.DataTree.Location = New System.Drawing.Point(0, 0)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(256, 828)
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
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overloads Sub OnLoad(ByVal Controller As BaseController)
        Me.Controller = Controller
        DataTree.OnLoad(Controller)
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged
        AddHandler Controller.ApsimData.BeforeSave, AddressOf OnBeforeSave
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
        Dim SelectedData As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPath)
        If CurrentUIIndex = -1 OrElse UITypes(CurrentUIIndex) <> SelectedData.Type Then
            CloseUI()
            CurrentUIIndex = UITypes.IndexOf(SelectedData.Type)
            If CurrentUIIndex = -1 Then
                Dim View As BaseView = Controller.CreateUI(SelectedData.Type)
                If Not IsNothing(View) Then
                    UIs.Add(View)
                    UITypes.Add(SelectedData.Type)
                    CurrentUIIndex = UIs.Count - 1
                End If
            End If
        Else
            SaveCurrentView()
        End If
        If CurrentUIIndex <> -1 Then
            Try
                Dim View As BaseView = UIs(CurrentUIIndex)
                View.OnLoad(Controller, Controller.SelectedPath, Controller.Selection.Contents)
                View.Parent = UIPanel
                View.Dock = DockStyle.Fill
                View.Show()
                View.OnRefresh()
            Catch ex As Exception
                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub
    Public Sub CloseUI()
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
            Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(View.NodePath)
            If Not IsNothing(Comp) Then
                View.OnSave()
                Comp.Contents = View.GetData()
            End If
        End If
    End Sub
    Public Sub RefreshCurrentView()
        If CurrentUIIndex <> -1 Then
            Dim View As BaseView = UIs(CurrentUIIndex)
            View.OnLoad(Controller, Controller.SelectedPath, Controller.Selection.Contents)
            View.OnRefresh()
        End If
    End Sub
    Public ReadOnly Property CurrentView() As BaseView
        Get
            If CurrentUIIndex <> -1 Then
                Return UIs(CurrentUIIndex)
            Else
                Return Nothing
            End If
        End Get
    End Property
    Private Sub OnBeforeSave()
        ' -----------------------------------------------------
        ' User is about to do a save.
        ' -----------------------------------------------------
        If Controller.SelectedPaths.Count = 1 Then
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
