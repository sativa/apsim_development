Imports VBGeneral
Imports Xceed.Grid.Editors
Imports System.Collections.Specialized
Public Class ReportVariablesListView
    Inherits BaseDataControl
    Private InFill As Boolean

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
    Friend WithEvents GroupByRow1 As Xceed.Grid.GroupByRow
    Friend WithEvents ColumnManagerRow1 As Xceed.Grid.ColumnManagerRow
    Friend WithEvents dataRowTemplate1 As Xceed.Grid.DataRow
    Friend WithEvents VariablesList As Xceed.Grid.GridControl
    Friend WithEvents Column1 As Xceed.Grid.Column
    Friend WithEvents cellColumnManagerRow1Column1 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents celldataRowTemplate1Column1 As Xceed.Grid.DataCell
    Friend WithEvents Column2 As Xceed.Grid.Column
    Friend WithEvents cellColumnManagerRow1Column2 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents celldataRowTemplate1Column2 As Xceed.Grid.DataCell
    Friend WithEvents Column3 As Xceed.Grid.Column
    Friend WithEvents cellColumnManagerRow1Column3 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents celldataRowTemplate1Column3 As Xceed.Grid.DataCell
    Friend WithEvents Column4 As Xceed.Grid.Column
    Friend WithEvents cellColumnManagerRow1Column4 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents celldataRowTemplate1Column4 As Xceed.Grid.DataCell
    Friend WithEvents Column5 As Xceed.Grid.Column
    Friend WithEvents cellColumnManagerRow1Column5 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents celldataRowTemplate1Column5 As Xceed.Grid.DataCell
    Friend WithEvents Tooltip As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.VariablesList = New Xceed.Grid.GridControl
        Me.Column1 = New Xceed.Grid.Column
        Me.Column2 = New Xceed.Grid.Column
        Me.Column3 = New Xceed.Grid.Column
        Me.Column4 = New Xceed.Grid.Column
        Me.Column5 = New Xceed.Grid.Column
        Me.dataRowTemplate1 = New Xceed.Grid.DataRow
        Me.celldataRowTemplate1Column1 = New Xceed.Grid.DataCell
        Me.celldataRowTemplate1Column2 = New Xceed.Grid.DataCell
        Me.celldataRowTemplate1Column3 = New Xceed.Grid.DataCell
        Me.celldataRowTemplate1Column4 = New Xceed.Grid.DataCell
        Me.celldataRowTemplate1Column5 = New Xceed.Grid.DataCell
        Me.GroupByRow1 = New Xceed.Grid.GroupByRow
        Me.ColumnManagerRow1 = New Xceed.Grid.ColumnManagerRow
        Me.cellColumnManagerRow1Column1 = New Xceed.Grid.ColumnManagerCell
        Me.cellColumnManagerRow1Column2 = New Xceed.Grid.ColumnManagerCell
        Me.cellColumnManagerRow1Column3 = New Xceed.Grid.ColumnManagerCell
        Me.cellColumnManagerRow1Column4 = New Xceed.Grid.ColumnManagerCell
        Me.cellColumnManagerRow1Column5 = New Xceed.Grid.ColumnManagerCell
        Me.Tooltip = New System.Windows.Forms.Label
        CType(Me.VariablesList, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.dataRowTemplate1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.ColumnManagerRow1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'VariablesList
        '
        Me.VariablesList.AllowDrop = True
        Me.VariablesList.CausesValidation = False
        Me.VariablesList.Columns.Add(Me.Column1)
        Me.VariablesList.Columns.Add(Me.Column2)
        Me.VariablesList.Columns.Add(Me.Column3)
        Me.VariablesList.Columns.Add(Me.Column4)
        Me.VariablesList.Columns.Add(Me.Column5)
        Me.VariablesList.DataRowTemplate = Me.dataRowTemplate1
        Me.VariablesList.Dock = System.Windows.Forms.DockStyle.Fill
        Me.VariablesList.FixedHeaderRows.Add(Me.GroupByRow1)
        Me.VariablesList.FixedHeaderRows.Add(Me.ColumnManagerRow1)
        Me.VariablesList.Location = New System.Drawing.Point(0, 23)
        Me.VariablesList.Name = "VariablesList"
        '
        'VariablesList.RowSelectorPane
        '
        Me.VariablesList.RowSelectorPane.Visible = False
        Me.VariablesList.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
        Me.VariablesList.SingleClickEdit = False
        Me.VariablesList.Size = New System.Drawing.Size(648, 290)
        Me.VariablesList.TabIndex = 1
        Me.VariablesList.UIStyle = Xceed.Grid.UIStyle.UIStyle.System
        '
        'Column1
        '
        Me.Column1.SortDirection = Xceed.Grid.SortDirection.None
        Me.Column1.Title = "Name"
        Me.Column1.VisibleIndex = 0
        Me.Column1.Initialize("Column1", GetType(System.String))
        '
        'Column2
        '
        Me.Column2.SortDirection = Xceed.Grid.SortDirection.None
        Me.Column2.Title = "Module"
        Me.Column2.VisibleIndex = 1
        Me.Column2.Initialize("Column2", GetType(System.String))
        '
        'Column3
        '
        Me.Column3.SortDirection = Xceed.Grid.SortDirection.None
        Me.Column3.Title = "Alias"
        Me.Column3.VisibleIndex = 2
        Me.Column3.Initialize("Column3", GetType(System.String))
        '
        'Column4
        '
        Me.Column4.SortDirection = Xceed.Grid.SortDirection.None
        Me.Column4.Title = "Arrayspec"
        Me.Column4.VisibleIndex = 3
        Me.Column4.Initialize("Column4", GetType(System.String))
        '
        'Column5
        '
        Me.Column5.SortDirection = Xceed.Grid.SortDirection.None
        Me.Column5.Title = "Description"
        Me.Column5.VisibleIndex = 4
        Me.Column5.Initialize("Column5", GetType(System.String))
        '
        'dataRowTemplate1
        '
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column1)
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column2)
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column3)
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column4)
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column5)
        '
        'celldataRowTemplate1Column1
        '
        Me.celldataRowTemplate1Column1.BackColor = System.Drawing.Color.White
        Me.celldataRowTemplate1Column1.Font = New System.Drawing.Font("Tahoma", 8.25!)
        Me.celldataRowTemplate1Column1.Initialize("Column1")
        '
        'celldataRowTemplate1Column2
        '
        Me.celldataRowTemplate1Column2.BackColor = System.Drawing.Color.White
        Me.celldataRowTemplate1Column2.ForeColor = System.Drawing.Color.Black
        Me.celldataRowTemplate1Column2.Initialize("Column2")
        '
        'celldataRowTemplate1Column3
        '
        Me.celldataRowTemplate1Column3.BackColor = System.Drawing.Color.White
        Me.celldataRowTemplate1Column3.ForeColor = System.Drawing.Color.Black
        Me.celldataRowTemplate1Column3.Initialize("Column3")
        '
        'celldataRowTemplate1Column4
        '
        Me.celldataRowTemplate1Column4.BackColor = System.Drawing.Color.White
        Me.celldataRowTemplate1Column4.ForeColor = System.Drawing.Color.Black
        Me.celldataRowTemplate1Column4.Initialize("Column4")
        '
        'celldataRowTemplate1Column5
        '
        Me.celldataRowTemplate1Column5.BackColor = System.Drawing.Color.White
        Me.celldataRowTemplate1Column5.ForeColor = System.Drawing.Color.Black
        Me.celldataRowTemplate1Column5.Initialize("Column5")
        '
        'GroupByRow1
        '
        Me.GroupByRow1.Visible = False
        '
        'ColumnManagerRow1
        '
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column1)
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column2)
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column3)
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column4)
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column5)
        Me.cellColumnManagerRow1Column1.Initialize("Column1")
        Me.cellColumnManagerRow1Column2.Initialize("Column2")
        Me.cellColumnManagerRow1Column3.Initialize("Column3")
        Me.cellColumnManagerRow1Column4.Initialize("Column4")
        Me.cellColumnManagerRow1Column5.Initialize("Column5")
        '
        'Tooltip
        '
        Me.Tooltip.BackColor = System.Drawing.SystemColors.Info
        Me.Tooltip.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Tooltip.Font = New System.Drawing.Font("Tahoma", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Tooltip.ForeColor = System.Drawing.SystemColors.InfoText
        Me.Tooltip.Location = New System.Drawing.Point(0, 313)
        Me.Tooltip.Name = "Tooltip"
        Me.Tooltip.Size = New System.Drawing.Size(648, 23)
        Me.Tooltip.TabIndex = 2
        '
        'ReportVariablesListView
        '
        Me.Controls.Add(Me.VariablesList)
        Me.Controls.Add(Me.Tooltip)
        Me.Name = "ReportVariablesListView"
        Me.Size = New System.Drawing.Size(648, 336)
        Me.Controls.SetChildIndex(Me.Tooltip, 0)
        Me.Controls.SetChildIndex(Me.VariablesList, 0)
        CType(Me.VariablesList, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.dataRowTemplate1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.ColumnManagerRow1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    ' ------------------------
    ' Fill in grid
    ' ------------------------
    Overrides Sub fill()
        InFill = True
        CaptionLabel.Text = "Output variables"

        VariablesList.DataRows.Clear()
        Dim VariablesNode As APSIMData = MyData.Child("variables")
        Dim row As Xceed.Grid.DataRow
        For Each child As APSIMData In VariablesNode.Children("variable")
            row = VariablesList.DataRows.AddNew()
            row.Cells(0).Value = child.Attribute("variablename")
            row.Cells(1).Value = child.Attribute("module")
            row.Cells(2).Value = child.Attribute("name")
            row.Cells(3).Value = child.Attribute("arrayspec")
            row.Cells(4).Value = child.Attribute("description")

            ' Only make the arrayspec field visible for array variables.
            row.Cells(3).Visible = child.Attribute("array") = "T"

            AddHandler row.Cells(0).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(1).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(2).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(3).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(4).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(0).EnteringEdit, AddressOf Me.CellEnteringEdit
            AddHandler row.Cells(1).EnteringEdit, AddressOf Me.CellEnteringEdit
            AddHandler row.Cells(2).EnteringEdit, AddressOf Me.CellEnteringEdit
            AddHandler row.Cells(3).EnteringEdit, AddressOf Me.CellEnteringEdit
            AddHandler row.Cells(4).EnteringEdit, AddressOf Me.CellEnteringEdit


        Next
        AddBlankRow()
        row = VariablesList.DataRows.AddNew()   ' Not sure why this extra row is needed.
        InFill = False
    End Sub


    ' ---------------------------------
    ' Add a blank row to grid.
    ' ---------------------------------
    Sub AddBlankRow()
        Dim row As Xceed.Grid.DataRow
        row = VariablesList.DataRows(VariablesList.DataRows.Count - 1)
        If row.Cells(0).Value <> "" Or row.Cells(1).Value <> "" Or row.Cells(2).Value <> "" _
            Or row.Cells(3).Value <> "" Or row.Cells(4).Value <> "" Then
            row = VariablesList.DataRows.AddNew()
            AddHandler row.Cells(0).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(1).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(2).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(3).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(4).EditLeft, AddressOf Me.CellLeavingEdit
            AddHandler row.Cells(0).EnteringEdit, AddressOf Me.CellEnteringEdit
            AddHandler row.Cells(1).EnteringEdit, AddressOf Me.CellEnteringEdit
            AddHandler row.Cells(2).EnteringEdit, AddressOf Me.CellEnteringEdit
            AddHandler row.Cells(3).EnteringEdit, AddressOf Me.CellEnteringEdit
            AddHandler row.Cells(4).EnteringEdit, AddressOf Me.CellEnteringEdit
        End If
    End Sub


    ' -------------------------------------------------
    ' User has hit a key - see if it is a delete.
    ' -------------------------------------------------
    Private Sub VariablesList_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles VariablesList.KeyDown
        If e.KeyValue = Keys.Delete Then
            Dim Row As Xceed.Grid.DataRow = VariablesList.CurrentCell.ParentRow
            Dim name As String = Row.Cells(2).Value
            MyData.Child("Variables").Delete(name)
            VariablesList.DataRows.Remove(Row)
        End If
    End Sub


    ' -------------------------------------------------
    ' User is about to modify a cell - provide help text.
    ' -------------------------------------------------
    Private Sub CellEnteringEdit(ByVal sender As Object, ByVal e As Xceed.Grid.EnteringEditEventArgs)
        Select Case VariablesList.CurrentCell.ParentColumn.Index
            Case 0
                Tooltip.Text = "Enter the name of the APSIM variable"
            Case 1
                Tooltip.Text = "Enter the APSIM module name that owns the variable"
            Case 2
                Tooltip.Text = "Enter a column heading name for the output file"
            Case 3
                Tooltip.Text = "For array variables, enter an array specifier e.g. () for sum, (1-2) for first 2 elements or leave blank to output all elements"
            Case 4
                Tooltip.Text = "Enter an option description for the variable"
        End Select
    End Sub


    ' -------------------------------------------------
    ' User has modified a cell - save row.
    ' -------------------------------------------------
    Private Sub CellLeavingEdit(ByVal sender As Object, ByVal e As Xceed.Grid.EditLeftEventArgs)
        If Not InFill Then
            Dim Row As Xceed.Grid.DataRow = VariablesList.CurrentCell.ParentRow
            Dim VariablesNode As APSIMData = MyData.Child("variables")

            If Row.Cells(2).Value = "" Then
                Row.Cells(2).Value = Row.Cells(0).Value
            End If

            Dim Child As APSIMData = VariablesNode.Child(Row.Cells(2).Value)
            If IsNothing(Child) Then
                Child = New APSIMData("variable", "NewReportingVariable")
                VariablesNode.Add(Child)
                Child = VariablesNode.Child("NewReportingVariable")
            End If
            Child.SetAttribute("variablename", Row.Cells(0).Value)
            Child.SetAttribute("module", Row.Cells(1).Value)
            Child.SetAttribute("name", Row.Cells(2).Value)
            Child.SetAttribute("arrayspec", Row.Cells(3).Value)
            Child.SetAttribute("description", Row.Cells(4).Value)
            AddBlankRow()
        End If
    End Sub


    ' -------------------------------------------------
    ' A drag operation has entered the variables list.
    ' -------------------------------------------------
    Private Sub VariablesList_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles VariablesList.DragEnter
        If AllowDrop = True Then
            e.Effect = DragDropEffects.Copy
        Else
            e.Effect = DragDropEffects.None
        End If
    End Sub


    ' --------------------------------
    ' User has dragged over us.
    ' --------------------------------
    Private Sub VariablesList_DragOver(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles VariablesList.DragOver
        e.Effect = DragDropEffects.Copy
    End Sub


    ' -------------------------------------------------
    ' The user has dropped an item on us
    ' -------------------------------------------------
    Private Sub VariablesList_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles VariablesList.DragDrop
        Dim NewDataString As String = e.Data.GetData(DataFormats.Text)
        Dim NewData As New APSIMData(NewDataString)
        If NewData.Type = "variable" Then
            NewData.SetAttribute("variablename", NewData.Attribute("name"))
            NewData.SetAttribute("arrayspec", "")
            MyData.Child("Variables").Add(NewData)
            fill()
        Else
            MsgBox("You can only add variables to the output variables list.", MsgBoxStyle.Critical, "Error")
        End If
    End Sub


    Private Sub VariablesList_AddingDataRow(ByVal sender As System.Object, ByVal e As Xceed.Grid.AddingDataRowEventArgs) Handles VariablesList.AddingDataRow

    End Sub
End Class
