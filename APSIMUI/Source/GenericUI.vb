Imports VBGeneral
Imports Xceed.Grid.Editors
Imports System.Collections
Imports System.Collections.Specialized

Public Class GenericUI
    Inherits APSIMUI.BaseUI
    Dim InRefresh As Boolean

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If Not Grid.CurrentCell Is Nothing Then
            If Grid.CurrentCell.IsBeingEdited Then
                Grid.CurrentCell.LeaveEdit(False)
            End If
        End If
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
    Friend WithEvents Grid As Xceed.Grid.GridControl
    Friend WithEvents NameColumn As Xceed.Grid.Column
    Friend WithEvents ValueColumn As Xceed.Grid.Column
    Friend WithEvents GroupByRow1 As Xceed.Grid.GroupByRow
    Friend WithEvents ColumnManagerRow1 As Xceed.Grid.ColumnManagerRow
    Friend WithEvents cellColumnManagerRow1Column1 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents dataRowTemplate1 As Xceed.Grid.DataRow
    Friend WithEvents celldataRowTemplate1Column2 As Xceed.Grid.DataCell
    Friend WithEvents cellColumnManagerRow1Column2 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents celldataRowTemplate1Column1 As Xceed.Grid.DataCell
    Friend WithEvents PictureBox As System.Windows.Forms.PictureBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.Grid = New Xceed.Grid.GridControl
        Me.NameColumn = New Xceed.Grid.Column
        Me.ValueColumn = New Xceed.Grid.Column
        Me.dataRowTemplate1 = New Xceed.Grid.DataRow
        Me.celldataRowTemplate1Column1 = New Xceed.Grid.DataCell
        Me.celldataRowTemplate1Column2 = New Xceed.Grid.DataCell
        Me.GroupByRow1 = New Xceed.Grid.GroupByRow
        Me.ColumnManagerRow1 = New Xceed.Grid.ColumnManagerRow
        Me.cellColumnManagerRow1Column1 = New Xceed.Grid.ColumnManagerCell
        Me.cellColumnManagerRow1Column2 = New Xceed.Grid.ColumnManagerCell
        Me.PictureBox = New System.Windows.Forms.PictureBox
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.dataRowTemplate1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.ColumnManagerRow1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Grid
        '
        Me.Grid.CausesValidation = False
        Me.Grid.Columns.Add(Me.NameColumn)
        Me.Grid.Columns.Add(Me.ValueColumn)
        Me.Grid.DataRowTemplate = Me.dataRowTemplate1
        Me.Grid.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Grid.FixedHeaderRows.Add(Me.GroupByRow1)
        Me.Grid.FixedHeaderRows.Add(Me.ColumnManagerRow1)
        Me.Grid.Location = New System.Drawing.Point(88, 23)
        Me.Grid.Name = "Grid"
        '
        'Grid.RowSelectorPane
        '
        Me.Grid.RowSelectorPane.Visible = False
        Me.Grid.SingleClickEdit = True
        Me.Grid.Size = New System.Drawing.Size(933, 546)
        Me.Grid.TabIndex = 2
        Me.Grid.UIStyle = Xceed.Grid.UIStyle.UIStyle.System
        '
        'NameColumn
        '
        Me.NameColumn.Title = "Name"
        Me.NameColumn.VisibleIndex = 0
        Me.NameColumn.Width = 187
        Me.NameColumn.Initialize("Name", GetType(System.String))
        '
        'ValueColumn
        '
        Me.ValueColumn.Title = "Value"
        Me.ValueColumn.VisibleIndex = 1
        Me.ValueColumn.Width = 267
        Me.ValueColumn.Initialize("Column2", GetType(System.String))
        '
        'dataRowTemplate1
        '
        Me.dataRowTemplate1.AccessibleName = "Data row 1 in data row template"
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column1)
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column2)
        Me.dataRowTemplate1.Height = 22
        '
        'celldataRowTemplate1Column1
        '
        Me.celldataRowTemplate1Column1.BackColor = System.Drawing.SystemColors.Window
        Me.celldataRowTemplate1Column1.ForeColor = System.Drawing.SystemColors.WindowText
        Me.celldataRowTemplate1Column1.Initialize("Name")
        '
        'celldataRowTemplate1Column2
        '
        Me.celldataRowTemplate1Column2.BackColor = System.Drawing.SystemColors.Window
        Me.celldataRowTemplate1Column2.ForeColor = System.Drawing.SystemColors.WindowText
        Me.celldataRowTemplate1Column2.Initialize("Column2")
        '
        'GroupByRow1
        '
        Me.GroupByRow1.AccessibleName = "Group by row 1 in fixed header"
        Me.GroupByRow1.Indented = False
        Me.GroupByRow1.Visible = False
        '
        'ColumnManagerRow1
        '
        Me.ColumnManagerRow1.AccessibleName = "Column manager row 2 in fixed header"
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column1)
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column2)
        Me.ColumnManagerRow1.Height = 19
        Me.cellColumnManagerRow1Column1.Initialize("Name")
        Me.cellColumnManagerRow1Column2.Initialize("Column2")
        '
        'PictureBox
        '
        Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox.Location = New System.Drawing.Point(0, 23)
        Me.PictureBox.Name = "PictureBox"
        Me.PictureBox.Size = New System.Drawing.Size(88, 546)
        Me.PictureBox.TabIndex = 3
        Me.PictureBox.TabStop = False
        '
        'GenericUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1021, 609)
        Me.Controls.Add(Me.Grid)
        Me.Controls.Add(Me.PictureBox)
        Me.Name = "GenericUI"
        Me.Controls.SetChildIndex(Me.PictureBox, 0)
        Me.Controls.SetChildIndex(Me.Grid, 0)
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.dataRowTemplate1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.ColumnManagerRow1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        MyBase.Refresh()

        InRefresh = True

        Dim inifile As New APSIMSettings
        Dim imagefile As String = UIManager.Image(mydata.Type)
        PictureBox.Image = Image.FromFile(imagefile)

        For Each Prop As APSIMData In MyData.Children()
            If Prop.Children.Count = 0 Then
                Dim row As Xceed.Grid.DataRow = Grid.DataRows.AddNew()
                row.BeginEdit()
                row.Cells(0).Value = Prop.Name
                row.Cells(1).Value = Prop.Value
                Dim Editor As CustomEditor = CreateCustomEditorForColumn(Prop)
                If Not IsNothing(Editor) Then
                    row.Cells(1).CellEditor = Editor
                End If
                AddHandler row.Cells(1).LeavingEdit, AddressOf Me.CellLeavingEdit

                row.EndEdit()
            End If
        Next

        InRefresh = False

    End Sub


    Private Sub Grid_CurrentCellChanged1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Me.Visible Then
            MyData.DataTable = Grid.DataSource
        End If
    End Sub


    ' --------------------------------
    ' Set the type of a grid column
    ' --------------------------------
    Shared Function CreateCustomEditorForColumn(ByVal Prop As APSIMData) As CustomEditor
        If Prop.Attribute("type") = "yesno" Then
            Dim CheckCombo As New ComboBox
            CheckCombo.Items.Add("yes")
            CheckCombo.Items.Add("no")
            Return New CustomEditor(CheckCombo, "Text", True)
        ElseIf Prop.Attribute("type") = "date" Then
            Dim DateEditor As New DateTimePicker
            DateEditor.Format = DateTimePickerFormat.Short

            Return New CustomEditor(DateEditor, "Text", True)
        ElseIf Prop.Attribute("type") = "list" Then
            Dim CheckCombo As New ComboBox
            Dim Values() As String = Prop.Attribute("listvalues").Split(",")
            For Each Value As String In Values
                Value = Value.Trim
                CheckCombo.Items.Add(Value)
            Next
            Return New CustomEditor(CheckCombo, "Text", True)
        ElseIf Prop.Attribute("type") = "modulename" Then
            Dim CheckCombo As New ComboBox
            Dim Values As StringCollection = GetMatchingModuleNames(Prop)
            For Each Value As String In Values
                Value = Value.Trim
                CheckCombo.Items.Add(Value)
            Next
            Return New CustomEditor(CheckCombo, "Text", True)
        ElseIf Prop.Attribute("type") = "cultivars" Then
            Dim CultivarCombo As New ComboBox
            CultivarCombo.Name = Prop.Name
            Return New CustomEditor(CultivarCombo, "Text", True)
        Else
            Return Nothing
        End If
    End Function


    ' ------------------------------------------------------------------
    ' Return a list of instance names for the specified module name
    ' ------------------------------------------------------------------
    Shared Function GetMatchingModuleNames(ByVal Prop As APSIMData) As StringCollection
        Dim Values As New StringCollection
        Dim System As APSIMData = Prop.Parent
        While System.Type <> "simulation" And Not IsNothing(System.Parent)
            System = System.Parent
        End While

        For Each ApsimModule As APSIMData In System.Children
            If Prop.Attribute("moduletype") = "" Or ApsimModule.Type = Prop.Attribute("moduletype") Then
                Values.Add(ApsimModule.Name())
            End If
        Next
        Return Values
    End Function


    ' -------------------------------------------------------------------
    ' The current cell has changed - update description if necessary.
    ' -------------------------------------------------------------------
    Private Sub Grid_CurrentCellChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles Grid.CurrentCellChanged
        Dim Description As String = Mydata.Child(Grid.CurrentCell.ParentRow.Cells(0).Value).Attribute("description")
        If Description <> "" Then
            HelpLabel.Text = Description
        Else
            HelpLabel.Text = ""
        End If
    End Sub


    ' -----------------------------------------------------------------------------
    ' A cell has changed values - if user has made the change then update the
    ' value in MyData
    ' -----------------------------------------------------------------------------
    Private Sub CellLeavingEdit(ByVal sender As Object, ByVal e As Xceed.Grid.LeavingEditEventArgs)
        If Not InRefresh Then
            Dim Row As Xceed.Grid.DataRow = Grid.CurrentCell.ParentRow
            Dim Prop As APSIMData = MyData.Child(Row.Cells(0).Value)
            Prop.Value = e.NewValue
        End If
    End Sub

    ' -----------------------------------------------------------------------------
    ' User is doing a run - make sure grid isn't in edit mode.
    ' -----------------------------------------------------------------------------
    Overrides Sub SaveToAPSIMFile()
        If Not Grid.CurrentCell Is Nothing Then
            If Grid.CurrentCell.IsBeingEdited Then
                Grid.CurrentCell.LeaveEdit(True)
            End If
        End If
    End Sub
End Class
