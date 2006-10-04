Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports VBGeneral
Imports CSGeneral

Public Class OutputFileDescUI
    Inherits BaseView
    Private UserChange As Boolean = True

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
    Friend WithEvents RightHandPanel As System.Windows.Forms.Panel
    Friend WithEvents EventsListView As APSIMUI.EventsListView
    Friend WithEvents FpSpread1 As FarPoint.Win.Spread.FpSpread
    Friend WithEvents Grid As FarPoint.Win.Spread.SheetView
    Friend WithEvents Spread As FarPoint.Win.Spread.FpSpread
    Friend WithEvents VariableListView As System.Windows.Forms.ListView
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader2 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader3 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader4 As System.Windows.Forms.ColumnHeader
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents Splitter2 As System.Windows.Forms.Splitter

    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim TipAppearance1 As FarPoint.Win.Spread.TipAppearance = New FarPoint.Win.Spread.TipAppearance
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.RightHandPanel = New System.Windows.Forms.Panel
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.VariableListView = New System.Windows.Forms.ListView
        Me.ColumnHeader1 = New System.Windows.Forms.ColumnHeader
        Me.ColumnHeader2 = New System.Windows.Forms.ColumnHeader
        Me.ColumnHeader4 = New System.Windows.Forms.ColumnHeader
        Me.ColumnHeader3 = New System.Windows.Forms.ColumnHeader
        Me.Spread = New FarPoint.Win.Spread.FpSpread
        Me.Grid = New FarPoint.Win.Spread.SheetView
        Me.Splitter2 = New System.Windows.Forms.Splitter
        Me.EventsListView = New APSIMUI.EventsListView
        Me.RightHandPanel.SuspendLayout()
        CType(Me.Spread, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.CheckFileExists = False
        Me.OpenFileDialog.DefaultExt = "out"
        Me.OpenFileDialog.Filter = "APSIM output files(*.out)|*.out|All Files (*.*)|*.*"
        Me.OpenFileDialog.Title = "Enter output file name"
        '
        'RightHandPanel
        '
        Me.RightHandPanel.Controls.Add(Me.Splitter1)
        Me.RightHandPanel.Controls.Add(Me.VariableListView)
        Me.RightHandPanel.Controls.Add(Me.Spread)
        Me.RightHandPanel.Controls.Add(Me.Splitter2)
        Me.RightHandPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RightHandPanel.Location = New System.Drawing.Point(0, 40)
        Me.RightHandPanel.Name = "RightHandPanel"
        Me.RightHandPanel.Size = New System.Drawing.Size(753, 733)
        Me.RightHandPanel.TabIndex = 11
        '
        'Splitter1
        '
        Me.Splitter1.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Splitter1.Location = New System.Drawing.Point(0, 411)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(753, 3)
        Me.Splitter1.TabIndex = 16
        Me.Splitter1.TabStop = False
        '
        'VariableListView
        '
        Me.VariableListView.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1, Me.ColumnHeader2, Me.ColumnHeader4, Me.ColumnHeader3})
        Me.VariableListView.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.VariableListView.FullRowSelect = True
        Me.VariableListView.Location = New System.Drawing.Point(0, 414)
        Me.VariableListView.Name = "VariableListView"
        Me.VariableListView.Size = New System.Drawing.Size(753, 316)
        Me.VariableListView.Sorting = System.Windows.Forms.SortOrder.Ascending
        Me.VariableListView.TabIndex = 15
        Me.VariableListView.UseCompatibleStateImageBehavior = False
        Me.VariableListView.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader1
        '
        Me.ColumnHeader1.Text = "Variable name"
        Me.ColumnHeader1.Width = 120
        '
        'ColumnHeader2
        '
        Me.ColumnHeader2.Text = "Component name"
        Me.ColumnHeader2.Width = 120
        '
        'ColumnHeader4
        '
        Me.ColumnHeader4.Text = "Array?"
        Me.ColumnHeader4.Width = 45
        '
        'ColumnHeader3
        '
        Me.ColumnHeader3.Text = "Description"
        Me.ColumnHeader3.Width = 410
        '
        'Spread
        '
        Me.Spread.AccessibleDescription = "Spread, Sheet1, Row 0, Column 0, "
        Me.Spread.AllowDrop = True
        Me.Spread.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Spread.EditModeReplace = True
        Me.Spread.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.Spread.Location = New System.Drawing.Point(0, 0)
        Me.Spread.Name = "Spread"
        Me.Spread.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.Grid})
        Me.Spread.Size = New System.Drawing.Size(753, 730)
        Me.Spread.TabIndex = 14
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.Spread.TextTipAppearance = TipAppearance1
        Me.Spread.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        '
        'Grid
        '
        Me.Grid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.Grid.ColumnCount = 3
        Me.Grid.AutoUpdateNotes = True
        Me.Grid.ColumnHeader.Cells.Get(0, 0).Value = "Variable name"
        Me.Grid.ColumnHeader.Cells.Get(0, 1).Value = "Array?"
        Me.Grid.ColumnHeader.Cells.Get(0, 2).Value = "Description"
        Me.Grid.Columns.Get(0).BackColor = System.Drawing.Color.WhiteSmoke
        Me.Grid.Columns.Get(0).Label = "Variable name"
        Me.Grid.Columns.Get(0).Width = 145.0!
        Me.Grid.Columns.Get(1).Label = "Array?"
        Me.Grid.Columns.Get(1).Locked = True
        Me.Grid.Columns.Get(1).Width = 52.0!
        Me.Grid.Columns.Get(2).Label = "Description"
        Me.Grid.Columns.Get(2).Locked = True
        Me.Grid.Columns.Get(2).Width = 338.0!
        Me.Grid.RowHeader.Columns.Default.Resizable = False
        Me.Grid.RowHeader.Visible = False
        Me.Grid.SheetName = "Sheet1"
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'Splitter2
        '
        Me.Splitter2.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Splitter2.Location = New System.Drawing.Point(0, 730)
        Me.Splitter2.Name = "Splitter2"
        Me.Splitter2.Size = New System.Drawing.Size(753, 3)
        Me.Splitter2.TabIndex = 13
        Me.Splitter2.TabStop = False
        '
        'EventsListView
        '
        Me.EventsListView.AllowDrop = True
        Me.EventsListView.AutoScroll = True
        Me.EventsListView.BackColor = System.Drawing.SystemColors.Control
        Me.EventsListView.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.EventsListView.HelpText = ""
        Me.EventsListView.Location = New System.Drawing.Point(0, 615)
        Me.EventsListView.Name = "EventsListView"
        Me.EventsListView.Size = New System.Drawing.Size(753, 118)
        Me.EventsListView.TabIndex = 12
        '
        'OutputFileDescUI
        '
        Me.Controls.Add(Me.RightHandPanel)
        Me.Name = "OutputFileDescUI"
        Me.Size = New System.Drawing.Size(753, 773)
        Me.Controls.SetChildIndex(Me.RightHandPanel, 0)
        Me.RightHandPanel.ResumeLayout(False)
        CType(Me.Spread, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub RefreshView(ByVal Controller As BaseController)
        ' ----------------------------------
        ' Refresh the variable grid
        ' ----------------------------------
        MyBase.RefreshView(Controller)
        If Controller.Data.Type.ToLower = "variables" Then
            HelpText = "Drag variables from the list at the bottom to the grid at the top." + vbCrLf + _
                       "Advanced examples of variable naming." + vbCrLf + _
                       "       wheat.yield            -  A variable name can be prefixed with a ComponentName." + vbCrLf + _
                       "       sw()                   -  Array variables can be summed over the profile with ()." + vbCrLf + _
                       "       sw(2)                  -  Specific elements of array variables can be reported." + vbCrLf + _
                       "       sw(2-4)                -  Ranges of elements of arrays can be reported." + vbCrLf + _
                       "       wheat.yield as whtyld  -  Variable names can also be renamed in the output file - aliased"
        Else
            HelpText = "Drag one or more frequencies from the list at the bottom to the grid at the top."
        End If

        UserChange = False
        PopulateVariableGrid()
        PopulateVariableListView()
        UserChange = True

        Dim InputMap As FarPoint.Win.Spread.InputMap = Spread.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused)
        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None), FarPoint.Win.Spread.SpreadActions.MoveToNextRow)
    End Sub

    Private Sub PopulateVariableGrid()
        ' -----------------------------------
        ' Populate the variable grid
        ' -----------------------------------
        Dim Row As Integer = 0
        For Each Variable As APSIMData In Controller.Data.Children
            Grid.Cells(Row, 0).Value = Variable.Name
            Grid.Cells(Row, 1).Value = Variable.Attribute("array")
            Grid.Cells(Row, 2).Value = Variable.Attribute("description")
            Row += 1
        Next
    End Sub
    Private Sub PopulateVariableListView()
        ' ----------------------------------------------
        ' Populate the variable list view box
        ' ----------------------------------------------
        VariableListView.BeginUpdate()
        VariableListView.Groups.Clear()
        VariableListView.Items.Clear()
        Dim ApsimUI As ApsimUIController = Controller
        Dim VariableData As APSIMData = ApsimUI.GetVariableDescriptions(Controller.Data.Parent.Parent, Controller.Data.Type)

        For Each VariableGroup As APSIMData In VariableData.Children
            Dim NewGroup As New ListViewGroup(VariableGroup.Name)
            VariableListView.Groups.Add(NewGroup)

            For Each Variable As APSIMData In VariableGroup.Children
                Dim ListItem As New ListViewItem(Variable.Name)
                ListItem.Group = NewGroup
                ListItem.SubItems.Add(VariableGroup.Attribute("module"))
                If Variable.Attribute("array") = "T" Then
                    ListItem.SubItems.Add("Yes")
                Else
                    ListItem.SubItems.Add("No")
                End If
                ListItem.SubItems.Add(Variable.Attribute("description"))
                VariableListView.Items.Add(ListItem)
            Next
        Next
        VariableListView.EndUpdate()
    End Sub
    Private Sub SaveVariableGrid()
        ' --------------------------------------------------
        ' Save the variable grid back to the selected data.
        ' --------------------------------------------------

        ' Work out the property type from the currently selected data type by removing the last character.
        ' e.g. if current data type is 'variables' then property type is 'variable'
        Dim PropertyType As String = Controller.Data.Type
        PropertyType = PropertyType.Remove(PropertyType.Length - 1)

        Dim NumVariables As Integer = GridUtils.FindFirstBlankCell(Grid, 0)
        Controller.Data.EnsureNumberOfChildren(PropertyType, "", NumVariables)

        Dim Row As Integer = 0
        For Each Variable As APSIMData In Controller.Data.Children(PropertyType)
            Variable.Name = Grid.Cells(Row, 0).Text
            Variable.SetAttribute("array", Grid.Cells(Row, 1).Text)
            Variable.SetAttribute("description", Grid.Cells(Row, 2).Text)
            Row += 1
        Next
    End Sub

    Private Sub GridKeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Spread.KeyDown
        ' --------------------------------------------------
        ' If user has hit delete then delete the entire row.
        ' --------------------------------------------------
        If e.KeyCode = Keys.Delete Then
            If Grid.SelectionCount > 0 Then
                Dim Range As FarPoint.Win.Spread.Model.CellRange = Grid.GetSelection(0)
                If Range.ColumnCount = 5 Then
                    ' delete the entire rows.
                    Grid.Rows(Range.Row, Range.Row + Range.RowCount - 1).Remove()
                Else
                    ' just clear the cell contents.
                    Grid.Cells(Range.Row, Range.Column, Range.Row + Range.RowCount - 1, Range.Column + Range.ColumnCount - 1).Value = ""
                End If
            End If
        End If
    End Sub
    Public Overrides Sub Save()
        SaveVariableGrid()
    End Sub
    Private Sub Grid_CellChanged(ByVal sender As System.Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles Grid.CellChanged
        ' -------------------------------------------------------
        ' User has entered there own variable. See if we can find
        ' it in the variable list. If so, then add description
        ' and isArray to grid.
        ' -------------------------------------------------------
        If UserChange Then
            UserChange = False
            Dim LowerName As String = Grid.Cells(e.Row, e.Column).Text.ToLower
            For Each Item As ListViewItem In VariableListView.Items
                If Item.Text.ToLower = LowerName Then
                    Grid.Cells(e.Row, 0).Text = Item.Text
                    Grid.Cells(e.Row, 1).Text = Item.SubItems(2).Text
                    Grid.Cells(e.Row, 2).Text = Item.SubItems(3).Text
                End If
            Next
            UserChange = True
        End If
    End Sub

#Region "Drag / Drop methods"
    Private Sub ListViewItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles VariableListView.ItemDrag
        ' --------------------------------------------------------
        ' User is trying to initiate a drag - allow drag operation
        ' --------------------------------------------------------
        VariableListView.DoDragDrop("xx", DragDropEffects.All)
    End Sub
    Private Sub VariablesGridDragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Spread.DragEnter
        e.Effect = DragDropEffects.Copy
    End Sub
    Private Sub VariablesGridDragOver(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Spread.DragOver
        e.Effect = DragDropEffects.Copy
    End Sub
    Private Sub VariablesGridDragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Spread.DragDrop
        ' --------------------------------------------------
        ' User has dropped a variable onto the variable grid
        ' --------------------------------------------------
        UserChange = False
        Dim Row As Integer = GridUtils.FindFirstBlankCell(Grid, 0)
        For Each SelectedItem As ListViewItem In VariableListView.SelectedItems
            Grid.Cells(Row, 0).Text = SelectedItem.Text
            Grid.Cells(Row, 1).Text = SelectedItem.SubItems(2).Text
            Grid.Cells(Row, 2).Text = SelectedItem.SubItems(3).Text
            Row += 1
        Next
        UserChange = True
        SaveVariableGrid()
    End Sub


#End Region


End Class
