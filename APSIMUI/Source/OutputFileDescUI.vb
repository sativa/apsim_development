Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports VBGeneral
Imports CSGeneral

Public Class OutputFileDescUI
    Inherits BaseUI


#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        Xceed.Grid.Licenser.LicenseKey = "GRD22-KTL57-34ZF5-W4JA"

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
        Me.Splitter2 = New System.Windows.Forms.Splitter
        Me.EventsListView = New APSIMUI.EventsListView
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.VariablesListView = New APSIMUI.ReportVariablesListView
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
        Me.DataTree.LabelEdit = False
        Me.DataTree.Location = New System.Drawing.Point(0, 23)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(256, 595)
        Me.DataTree.Sorted = False
        Me.DataTree.TabIndex = 8
        '
        'RightHandPanel
        '
        Me.RightHandPanel.Controls.Add(Me.VariablesListView)
        Me.RightHandPanel.Controls.Add(Me.Splitter2)
        Me.RightHandPanel.Controls.Add(Me.EventsListView)
        Me.RightHandPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RightHandPanel.Location = New System.Drawing.Point(256, 23)
        Me.RightHandPanel.Name = "RightHandPanel"
        Me.RightHandPanel.Size = New System.Drawing.Size(797, 595)
        Me.RightHandPanel.TabIndex = 11
        '
        'Splitter2
        '
        Me.Splitter2.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Splitter2.Location = New System.Drawing.Point(0, 456)
        Me.Splitter2.Name = "Splitter2"
        Me.Splitter2.Size = New System.Drawing.Size(797, 3)
        Me.Splitter2.TabIndex = 13
        Me.Splitter2.TabStop = False
        '
        'EventsListView
        '
        Me.EventsListView.AllowDrop = True
        Me.EventsListView.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.EventsListView.Location = New System.Drawing.Point(0, 459)
        Me.EventsListView.Name = "EventsListView"
        Me.EventsListView.Size = New System.Drawing.Size(797, 136)
        Me.EventsListView.TabIndex = 12
        '
        'Splitter1
        '
        Me.Splitter1.Location = New System.Drawing.Point(256, 23)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(3, 595)
        Me.Splitter1.TabIndex = 12
        Me.Splitter1.TabStop = False
        '
        'VariablesListView
        '
        Me.VariablesListView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.VariablesListView.Location = New System.Drawing.Point(0, 0)
        Me.VariablesListView.Name = "VariablesListView"
        Me.VariablesListView.Size = New System.Drawing.Size(797, 456)
        Me.VariablesListView.TabIndex = 14
        '
        'OutputFileDescUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(6, 15)
        Me.ClientSize = New System.Drawing.Size(1053, 658)
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
    Overrides Sub refresh()
        MyBase.Refresh()
        HelpLabel.Text = "Use the variables tab to specify APSIM variables that should be written to the output file. Use the file contents tab to view the contents of the output file."

        DataTree.Sorted = True
        DataTree.CaptionLabel.Text = "Variable and events"
        'DataTree.ShowAll = True
        DataTree.Data = BuildDataTree()

        VariablesListView.Data = Data
        EventsListView.Data = Data
    End Sub


    ' ------------------------------------
    ' Create an APSIMData for the 
    ' tree control
    ' ------------------------------------
    Private Function BuildDataTree() As APSIMData
        Dim VariablesXML As String = "<components/>"
        Dim UIManager As UIManager = Explorer.ApplicationSettings

        If Data.Parent.Type = "outputfile" Then
            Return New APSIMData(UIManager.GetOutputFileDescriptions(Data.Parent.Parent))
        Else
            Return New APSIMData("<components/>")
        End If
    End Function

    Private Sub DataTree_DataSelectedEvent(ByVal e As VBGeneral.APSIMData) Handles DataTree.DataSelectedEvent
        HelpLabel.Text = e.Attribute("description")
    End Sub
End Class
