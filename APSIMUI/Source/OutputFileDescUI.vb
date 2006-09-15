Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports VBGeneral
Imports CSGeneral

Public Class OutputFileDescUI
    Inherits BaseView


#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

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
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents DataTree As VBGeneral.DataTree
    Friend WithEvents RightHandPanel As System.Windows.Forms.Panel
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents EventsListView As APSIMUI.EventsListView
    Friend WithEvents Splitter2 As System.Windows.Forms.Splitter
    Friend WithEvents VariablesListView As APSIMUI.ReportVariablesListView

    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.DataTree = New VBGeneral.DataTree
        Me.RightHandPanel = New System.Windows.Forms.Panel
        Me.VariablesListView = New APSIMUI.ReportVariablesListView
        Me.Splitter2 = New System.Windows.Forms.Splitter
        Me.EventsListView = New APSIMUI.EventsListView
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.RightHandPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.CheckFileExists = False
        Me.OpenFileDialog.DefaultExt = "out"
        Me.OpenFileDialog.Filter = "APSIM output files(*.out)|*.out|All Files (*.*)|*.*"
        Me.OpenFileDialog.Title = "Enter output file name"
        '
        'DataTree
        '
        Me.DataTree.AllowDrop = True
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Left
        Me.DataTree.Location = New System.Drawing.Point(0, 17)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(213, 725)
        Me.DataTree.Sorted = False
        Me.DataTree.TabIndex = 8
        '
        'RightHandPanel
        '
        Me.RightHandPanel.Controls.Add(Me.VariablesListView)
        Me.RightHandPanel.Controls.Add(Me.Splitter2)
        Me.RightHandPanel.Controls.Add(Me.EventsListView)
        Me.RightHandPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RightHandPanel.Location = New System.Drawing.Point(213, 17)
        Me.RightHandPanel.Name = "RightHandPanel"
        Me.RightHandPanel.Size = New System.Drawing.Size(540, 725)
        Me.RightHandPanel.TabIndex = 11
        '
        'VariablesListView
        '
        Me.VariablesListView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.VariablesListView.DockPadding.Left = 5
        Me.VariablesListView.Location = New System.Drawing.Point(0, 0)
        Me.VariablesListView.Name = "VariablesListView"
        Me.VariablesListView.Size = New System.Drawing.Size(540, 604)
        Me.VariablesListView.TabIndex = 14
        '
        'Splitter2
        '
        Me.Splitter2.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Splitter2.Location = New System.Drawing.Point(0, 604)
        Me.Splitter2.Name = "Splitter2"
        Me.Splitter2.Size = New System.Drawing.Size(540, 3)
        Me.Splitter2.TabIndex = 13
        Me.Splitter2.TabStop = False
        '
        'EventsListView
        '
        Me.EventsListView.AllowDrop = True
        Me.EventsListView.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.EventsListView.Location = New System.Drawing.Point(0, 607)
        Me.EventsListView.Name = "EventsListView"
        Me.EventsListView.Size = New System.Drawing.Size(540, 118)
        Me.EventsListView.TabIndex = 12
        '
        'Splitter1
        '
        Me.Splitter1.Location = New System.Drawing.Point(213, 17)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(3, 725)
        Me.Splitter1.TabIndex = 12
        Me.Splitter1.TabStop = False
        '
        'OutputFileDescUI
        '
        Me.ClientSize = New System.Drawing.Size(753, 773)
        Me.Controls.Add(Me.Splitter1)
        Me.Controls.Add(Me.RightHandPanel)
        Me.Controls.Add(Me.DataTree)
        Me.Name = "OutputFileDescUI"
        Me.Controls.SetChildIndex(Me.DataTree, 0)
        Me.Controls.SetChildIndex(Me.RightHandPanel, 0)
        Me.Controls.SetChildIndex(Me.Splitter1, 0)
        Me.RightHandPanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' ----------------------------------
    ' Refresh the variable tree.
    ' ----------------------------------
    Overrides Sub RefreshView(ByVal Controller As BaseController)
        MyBase.RefreshView(Controller)
        HelpText = "Use the variables tab to specify APSIM variables that should be written to the output file. Use the file contents tab to view the contents of the output file."

        DataTree.Sorted = True
        DataTree.ExpandAll = False
        DataTree.HelpText = "Variable and events"
        Dim VariablesController As New ApsimUIController("", "", "")
        VariablesController.AllData = BuildDataTree()
        DataTree.RefreshView(VariablesController)
        DataTree.ShowAll = True
        VariablesListView.RefreshView(Controller)
        EventsListView.RefreshView(Controller)
    End Sub


    ' ------------------------------------
    ' Create an APSIMData for the 
    ' tree control
    ' ------------------------------------
    Private Function BuildDataTree() As APSIMData
        Dim VariablesXML As String = "<components/>"
        Dim UIManager As ApsimUIController = Controller

        If Controller.Data.Parent.Type = "outputfile" Then
            Return New APSIMData(UIManager.GetOutputFileDescriptions(Controller.Data.Parent.Parent))
        Else
            Return New APSIMData("<components/>")
        End If
    End Function

    'Private Sub DataTree_DataSelectedEvent(ByVal e As VBGeneral.APSIMData) Handles DataTree.DataSelectedEvent
    '    HelpText = e.Attribute("description")
    'End Sub

    Public Overrides Sub Save()
        VariablesListView.Save()
    End Sub
End Class
