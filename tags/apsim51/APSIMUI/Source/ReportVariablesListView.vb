Imports VBGeneral
Imports System.Collections.Specialized
Public Class ReportVariablesListView
    Inherits BaseView
    Private ComponentNames As New StringCollection
    Private ComponentTypes As New StringCollection
    Private ApsimUI As ApsimUIController
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
    Friend WithEvents NotifyIcon1 As System.Windows.Forms.NotifyIcon
    Friend WithEvents GridPopupMenu As System.Windows.Forms.ContextMenu
    Friend WithEvents DeleteRowMenu As System.Windows.Forms.MenuItem
    Friend WithEvents DeleteAllMenu As System.Windows.Forms.MenuItem
    Friend WithEvents FpSpread As FarPoint.Win.Spread.FpSpread
    Friend WithEvents VariablesList As FarPoint.Win.Spread.SheetView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim TipAppearance1 As FarPoint.Win.Spread.TipAppearance = New FarPoint.Win.Spread.TipAppearance
        Dim ComboBoxCellType1 As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
        Me.GridPopupMenu = New System.Windows.Forms.ContextMenu
        Me.DeleteRowMenu = New System.Windows.Forms.MenuItem
        Me.DeleteAllMenu = New System.Windows.Forms.MenuItem
        Me.NotifyIcon1 = New System.Windows.Forms.NotifyIcon(Me.components)
        Me.FpSpread = New FarPoint.Win.Spread.FpSpread
        Me.VariablesList = New FarPoint.Win.Spread.SheetView
        CType(Me.FpSpread, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.VariablesList, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GridPopupMenu
        '
        Me.GridPopupMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.DeleteRowMenu, Me.DeleteAllMenu})
        '
        'DeleteRowMenu
        '
        Me.DeleteRowMenu.Index = 0
        Me.DeleteRowMenu.Text = "&Delete variable"
        '
        'DeleteAllMenu
        '
        Me.DeleteAllMenu.Index = 1
        Me.DeleteAllMenu.Text = "Delete &all variables"
        '
        'NotifyIcon1
        '
        Me.NotifyIcon1.Text = "NotifyIcon1"
        Me.NotifyIcon1.Visible = True
        '
        'FpSpread
        '
        Me.FpSpread.AccessibleDescription = "FpSpread, Sheet1, Row 0, Column 0, "
        Me.FpSpread.AllowDrop = True
        Me.FpSpread.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FpSpread.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.FpSpread.Location = New System.Drawing.Point(0, 40)
        Me.FpSpread.Name = "FpSpread"
        Me.FpSpread.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.VariablesList})
        Me.FpSpread.Size = New System.Drawing.Size(734, 677)
        Me.FpSpread.TabIndex = 3
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.FpSpread.TextTipAppearance = TipAppearance1
        Me.FpSpread.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        '
        'VariablesList
        '
        Me.VariablesList.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.VariablesList.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.VariablesList.ColumnCount = 5
        Me.VariablesList.AutoUpdateNotes = True
        Me.VariablesList.ColumnHeader.Cells.Get(0, 0).Value = "Name"
        Me.VariablesList.ColumnHeader.Cells.Get(0, 1).Value = "Module"
        Me.VariablesList.ColumnHeader.Cells.Get(0, 2).Value = "Alias"
        Me.VariablesList.ColumnHeader.Cells.Get(0, 3).Value = "Array Operation"
        Me.VariablesList.ColumnHeader.Cells.Get(0, 4).Value = "Description"
        Me.VariablesList.Columns.Get(0).Label = "Name"
        Me.VariablesList.Columns.Get(0).Width = 101.0!
        Me.VariablesList.Columns.Get(1).Label = "Module"
        Me.VariablesList.Columns.Get(1).Width = 109.0!
        Me.VariablesList.Columns.Get(2).Label = "Alias"
        Me.VariablesList.Columns.Get(2).Width = 102.0!
        ComboBoxCellType1.Editable = True
        ComboBoxCellType1.Items = New String() {"all layers", "sum", "(1)", "(1-2)"}
        Me.VariablesList.Columns.Get(3).CellType = ComboBoxCellType1
        Me.VariablesList.Columns.Get(3).Label = "Array Operation"
        Me.VariablesList.Columns.Get(3).Width = 90.0!
        Me.VariablesList.Columns.Get(4).Label = "Description"
        Me.VariablesList.Columns.Get(4).Width = 256.0!
        Me.VariablesList.RowHeader.Columns.Default.Resizable = False
        Me.VariablesList.RowHeader.Visible = False
        Me.VariablesList.SheetName = "Sheet1"
        Me.VariablesList.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'ReportVariablesListView
        '
        Me.Controls.Add(Me.FpSpread)
        Me.Name = "ReportVariablesListView"
        Me.Size = New System.Drawing.Size(734, 717)
        Me.Controls.SetChildIndex(Me.FpSpread, 0)
        CType(Me.FpSpread, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.VariablesList, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub Refresh()
        UserChange = False
        HelpText = "Output variables"
        VariablesList.RowCount = 0
        ApsimUI = Controller

        If Not IsNothing(Controller.Data) Then
            Dim VariablesNode As APSIMData = Controller.Data.Child("variables")
            ApsimUI.GetSiblingComponents(VariablesNode.Parent.Parent, ComponentNames, ComponentTypes)

            For Each child As APSIMData In VariablesNode.Children("variable")
                AddModuleType(child)
                VariablesList.RowCount = VariablesList.RowCount + 1
                Dim Row As Integer = VariablesList.RowCount - 1
                PopulateRowFromData(Row, child)
            Next
            AddBlankRow()

        End If
        UserChange = True
    End Sub
    Public Overrides Sub Save()
        Dim Variables As APSIMData = Controller.Data.Child("variables")
        If Variables Is Nothing Then
            Variables = Controller.Data.Add(New APSIMData("variables", ""))
        End If
        Variables.Clear()
        For Row As Integer = 0 To VariablesList.RowCount - 2
            Dim VariableName As String = VariablesList.Cells(Row, 2).Value
            If VariableName = "" Then
                VariableName = VariablesList.Cells(Row, 0).Value
            End If
            If VariableName = "" Then
                Throw New System.Exception("Invalid variable name found on row " + Row.ToString)
            End If
            Dim Variable As APSIMData = Variables.Add(New APSIMData("variable", VariableName))
            Variable.SetAttribute("variablename", VariablesList.Cells(Row, 0).Value)
            Variable.SetAttribute("module", VariablesList.Cells(Row, 1).Value)

            Dim ArraySpec As String = VariablesList.Cells(Row, 3).Value
            If ArraySpec = "" Or ArraySpec = "all layers" Then
                Variable.SetAttribute("arrayspec", " ")
            ElseIf ArraySpec = "sum" Then
                Variable.SetAttribute("arrayspec", "()")
            Else
                Variable.SetAttribute("arrayspec", ArraySpec)
            End If
            Variable.SetAttribute("description", VariablesList.Cells(Row, 4).Value)
            If Not VariablesList.Cells(Row, 3).Locked Then
                Variable.SetAttribute("array", "T")
            End If
            AddModuleType(Variable)
        Next
    End Sub
    Private Sub AddBlankRow()
        ' Ensure there is a blank row at the bottom of the grid.
        Dim DoAddNewRow As Boolean = True
        If VariablesList.RowCount > 1 Then
            Dim Row As Integer = VariablesList.RowCount - 1
            DoAddNewRow = (VariablesList.Cells(Row, 0).Value <> "" Or _
                            VariablesList.Cells(Row, 1).Value <> "" Or _
                            VariablesList.Cells(Row, 2).Value <> "" Or _
                            VariablesList.Cells(Row, 3).Value <> "" Or _
                            VariablesList.Cells(Row, 4).Value <> "")
        End If
        If DoAddNewRow Then
            VariablesList.RowCount = VariablesList.RowCount + 1
        End If
    End Sub
    Private Sub VariablesList_CellChanged(ByVal sender As Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles VariablesList.CellChanged
        If UserChange Then
            UserChange = False
            ApsimUI.DirtyData = True
            FixAliasForRow(e.Row)
            AddBlankRow()
            UserChange = True
        End If
    End Sub

    Private Sub PopulateRowFromData(ByVal Row As Integer, ByVal Variable As APSIMData)
        VariablesList.Cells(Row, 0).Value = Variable.Attribute("variablename")
        VariablesList.Cells(Row, 1).Value = Variable.Attribute("module")
        If Variable.Attribute("name") <> Variable.Attribute("variablename") Then
            VariablesList.Cells(Row, 2).Value = Variable.Attribute("name")
        End If
        Dim ArraySpec As String = Variable.Attribute("arrayspec")
        If ArraySpec = "()" Then
            ArraySpec = "sum"
        ElseIf ArraySpec = " " And Variable.Attribute("array") = "T" Then
            ArraySpec = "all layers"
        End If
        VariablesList.Cells(Row, 3).Value = ArraySpec
        VariablesList.Cells(Row, 4).Value = Variable.Attribute("description")
        If Variable.Attribute("VariableType") <> "" Then
            VariablesList.Cells(Row, 1).Value = VariablesList.Cells(Row, 1).Value + " " + Variable.Attribute("VariableType")
        End If
        ' Only make the arrayspec field visible for array variables.
        VariablesList.Cells(Row, 3).Locked = Variable.Attribute("array") <> "T"
    End Sub


    Private Sub AddModuleType(ByVal Variable As APSIMData)
        ' Add a module type attribute to the specified variable.
        For Index As Integer = 0 To ComponentNames.Count - 1
            If ComponentNames(Index).ToLower() = Variable.Attribute("module").ToLower() Then
                Variable.SetAttribute("ModuleType", ComponentTypes(Index))
            End If
        Next
    End Sub
    Private Function FixAliasForRow(ByVal Row As Integer) As Boolean
        ' The aim of this method is to ensure the specified row has
        ' a unique alias when compared with all the other variables
        ' in the grid.

        Dim BaseAlias As String = VariablesList.Cells(Row, 2).Value
        If BaseAlias = "" Then
            BaseAlias = VariablesList.Cells(Row, 0).Value
        End If

        Dim Index As Integer = 1
        While RowClashes(Row)
            VariablesList.Cells(Row, 2).Value = BaseAlias + "{" + Index.ToString + "}"
            Index = Index + 1
        End While
    End Function
    Private Function RowClashes(ByVal Row As Integer) As Boolean
        ' Loop through all rows seeing if there is another row in variableslist
        ' that 'clashes' with the specified 'Row'
        For r As Integer = 0 To VariablesList.RowCount - 1
            If r <> Row And _
                VariablesList.Cells(r, 0).Value = VariablesList.Cells(Row, 0).Value And _
                VariablesList.Cells(r, 2).Value = VariablesList.Cells(Row, 2).Value Then
                Return True
            End If
        Next
        Return False
    End Function


#Region "Drag / Drop methods"
    Private Sub VariablesList_DragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles FpSpread.DragEnter
        If AllowDrop = True Then
            e.Effect = DragDropEffects.Copy
        Else
            e.Effect = DragDropEffects.None
        End If
    End Sub
    Private Sub VariablesList_DragOver(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles FpSpread.DragOver
        e.Effect = DragDropEffects.Copy
    End Sub
    Private Sub VariablesList_DragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles FpSpread.DragDrop
        Dim Row As Integer = VariablesList.RowCount - 1

        Dim NewDataString As String = "<dummy>" + e.Data.GetData(DataFormats.Text) + "</dummy>"
        Dim NewVariables As New APSIMData(NewDataString)
        For Each NewVariable As APSIMData In NewVariables.Children
            If NewVariable.Type = "variable" Then
                If Row >= VariablesList.RowCount Then
                    VariablesList.RowCount = VariablesList.RowCount + 1
                End If
                NewVariable.SetAttribute("variablename", NewVariable.Name)
                PopulateRowFromData(Row, NewVariable)
                FixAliasForRow(Row)
                Row = Row + 1
            Else
                MsgBox("You can only add variables to the output variables list.", MsgBoxStyle.Critical, "Error")
            End If
        Next
        ApsimUI.DirtyData = True
    End Sub


#End Region



    Private Sub FpSpread_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FpSpread.KeyDown
        If e.KeyCode = Keys.Delete Then
            If VariablesList.SelectionCount > 0 Then
                Dim Range As FarPoint.Win.Spread.Model.CellRange = VariablesList.GetSelection(0)
                If Range.ColumnCount = 5 Then
                    ' delete the entire rows.
                    VariablesList.Rows(Range.Row, Range.Row + Range.RowCount - 1).Remove()
                Else
                    ' just clear the cell contents.
                    VariablesList.Cells(Range.Row, Range.Column, Range.Row + Range.RowCount - 1, Range.Column + Range.ColumnCount - 1).Value = ""
                End If
                AddBlankRow()
            End If
            ApsimUI.DirtyData = True
        End If
    End Sub

End Class
