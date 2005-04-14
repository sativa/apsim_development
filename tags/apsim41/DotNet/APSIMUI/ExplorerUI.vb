Imports DataTreeControl
Imports General
Public Class explorerui
    Inherits BaseUI
    Private _UImanager As New UIManager
#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        _UImanager.SimulationExplorer = SimulationExplorer
        _UImanager.UIPanel = Panel

        'Event Handlers
        AddHandler SimulationExplorer.DataSelectedEvent, AddressOf OnDataSelected


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
    Friend WithEvents SimulationExplorer As DataTree
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents Panel As System.Windows.Forms.Panel
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.SimulationExplorer = New DataTreeControl.DataTree
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.Panel = New System.Windows.Forms.Panel
        Me.SuspendLayout()
        '
        'SimulationExplorer
        '
        Me.SimulationExplorer.Dock = System.Windows.Forms.DockStyle.Left
        Me.SimulationExplorer.LabelEdit = False
        Me.SimulationExplorer.Location = New System.Drawing.Point(0, 0)
        Me.SimulationExplorer.Name = "SimulationExplorer"
        Me.SimulationExplorer.Size = New System.Drawing.Size(256, 704)
        Me.SimulationExplorer.TabIndex = 3
        '
        'Splitter1
        '
        Me.Splitter1.Location = New System.Drawing.Point(256, 0)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(5, 704)
        Me.Splitter1.TabIndex = 4
        Me.Splitter1.TabStop = False
        '
        'Panel
        '
        Me.Panel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Panel.Location = New System.Drawing.Point(261, 0)
        Me.Panel.Name = "Panel"
        Me.Panel.Size = New System.Drawing.Size(809, 704)
        Me.Panel.TabIndex = 5
        '
        'explorerui
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1070, 704)
        Me.Controls.Add(Me.Panel)
        Me.Controls.Add(Me.Splitter1)
        Me.Controls.Add(Me.SimulationExplorer)
        Me.Name = "explorerui"
        Me.Text = "explorerui"
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub refresh()
        MyBase.Refresh()
        SimulationExplorer.Data = Data
        '_UImanager.FillSimulationExplorer(Data)
    End Sub

    Sub OnDataSelected(ByVal sender As Object, ByVal e As APSIMData)

        _UImanager.ShowUI(e)

    End Sub

    Private Sub TreeView_DoubleClick(ByVal sender As System.Object, ByVal e As System.EventArgs)


    End Sub


    Private Sub TreeView_MouseMove(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        Dim LeftDown As Boolean = (e.Button And MouseButtons.Left) > 0

        'If LeftDown Then
        'Dim toolname As String = ToolListBar.SelectedGroup.SelectedItem.Caption()
        'Dim datapath As String = ToolFile.RootPath + "/" + toolname
        'Dim data As String = ToolFile.GetXml(datapath)
        'Dim datastring As String = "hello"
        'TreeView.DoDragDrop(datastring, DragDropEffects.Copy)
        'Else

        'End If

    End Sub

    'Private Sub TreeView_ItemDrag(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ItemDragEventArgs)
    '    Dim datastring As String = APSIMData.FindChild(e.Item.fullpath).XML
    '    'TreeView.DoDragDrop(e.Item, DragDropEffects.Copy)
    '    TreeView.DoDragDrop(datastring, DragDropEffects.Copy)
    '    'MsgBox(e.Item.fullpath)
    '    'MsgBox(APSIMData.Child(e.Item.fullpath).XML)

    'End Sub

    'Private Sub TreeView_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
    'Dim path As String = SimulationExplorer.SelectedNode.FullPath
    '   _UImanager.ShowUI(APSIMData.Child(path))

    'End Sub
End Class
