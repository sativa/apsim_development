Imports VBGeneral
Imports Xceed.Grid.Editors
Imports System.Collections
Imports System.Collections.Specialized
Imports System.IO

Public Class RuleUI
    Inherits APSIMUI.BaseUI
    Dim ScriptHasChanged As Boolean
    Dim InRefresh As Boolean
    Dim Cultivars As APSIMData


#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        Xceed.Grid.Licenser.LicenseKey = "GRD22-WRGNK-2AHF5-44JA"

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
    Friend WithEvents celldataRowTemplate1Column1 As Xceed.Grid.DataCell
    Friend WithEvents celldataRowTemplate1Column2 As Xceed.Grid.DataCell
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents PropertyGrid As Xceed.Grid.GridControl
    Friend WithEvents Description As Xceed.Grid.Column
    Friend WithEvents Value As Xceed.Grid.Column
    Friend WithEvents Category As Xceed.Grid.Column
    Friend WithEvents dataRowTemplate1 As Xceed.Grid.DataRow
    Friend WithEvents celldataRowTemplate1Column11 As Xceed.Grid.DataCell
    Friend WithEvents celldataRowTemplate1Column21 As Xceed.Grid.DataCell
    Friend WithEvents celldataRowTemplate1Column3 As Xceed.Grid.DataCell
    Friend WithEvents GroupByRow1 As Xceed.Grid.GroupByRow
    Friend WithEvents ColumnManagerRow1 As Xceed.Grid.ColumnManagerRow
    Friend WithEvents cellColumnManagerRow1Column1 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents cellColumnManagerRow1Column2 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents cellColumnManagerRow1Column3 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents Group1 As Xceed.Grid.Group
    Friend WithEvents GroupManagerRow1 As Xceed.Grid.GroupManagerRow
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents ScriptBox As System.Windows.Forms.RichTextBox
    Friend WithEvents Name As Xceed.Grid.Column
    Friend WithEvents cellColumnManagerRow1Column4 As Xceed.Grid.ColumnManagerCell
    Friend WithEvents celldataRowTemplate1Column4 As Xceed.Grid.DataCell
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.celldataRowTemplate1Column1 = New Xceed.Grid.DataCell
        Me.celldataRowTemplate1Column2 = New Xceed.Grid.DataCell
        Me.TabControl1 = New System.Windows.Forms.TabControl
        Me.TabPage1 = New System.Windows.Forms.TabPage
        Me.PropertyGrid = New Xceed.Grid.GridControl
        Me.Description = New Xceed.Grid.Column
        Me.Value = New Xceed.Grid.Column
        Me.Category = New Xceed.Grid.Column
        Me.dataRowTemplate1 = New Xceed.Grid.DataRow
        Me.celldataRowTemplate1Column11 = New Xceed.Grid.DataCell
        Me.celldataRowTemplate1Column21 = New Xceed.Grid.DataCell
        Me.celldataRowTemplate1Column3 = New Xceed.Grid.DataCell
        Me.GroupByRow1 = New Xceed.Grid.GroupByRow
        Me.ColumnManagerRow1 = New Xceed.Grid.ColumnManagerRow
        Me.cellColumnManagerRow1Column1 = New Xceed.Grid.ColumnManagerCell
        Me.cellColumnManagerRow1Column2 = New Xceed.Grid.ColumnManagerCell
        Me.cellColumnManagerRow1Column3 = New Xceed.Grid.ColumnManagerCell
        Me.Group1 = New Xceed.Grid.Group
        Me.GroupManagerRow1 = New Xceed.Grid.GroupManagerRow
        Me.TabPage2 = New System.Windows.Forms.TabPage
        Me.ScriptBox = New System.Windows.Forms.RichTextBox
        Me.Name = New Xceed.Grid.Column
        Me.cellColumnManagerRow1Column4 = New Xceed.Grid.ColumnManagerCell
        Me.celldataRowTemplate1Column4 = New Xceed.Grid.DataCell
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.PropertyGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.dataRowTemplate1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.ColumnManagerRow1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage2.SuspendLayout()
        Me.SuspendLayout()
        '
        'celldataRowTemplate1Column1
        '
        Me.celldataRowTemplate1Column1.ReadOnly = True
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(0, 23)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(937, 611)
        Me.TabControl1.TabIndex = 3
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.PropertyGrid)
        Me.TabPage1.Location = New System.Drawing.Point(4, 25)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Size = New System.Drawing.Size(929, 582)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Properties"
        '
        'PropertyGrid
        '
        Me.PropertyGrid.BackgroundImageStyle = Xceed.Grid.ImageStyle.Stretch
        Me.PropertyGrid.Columns.Add(Me.Description)
        Me.PropertyGrid.Columns.Add(Me.Value)
        Me.PropertyGrid.Columns.Add(Me.Category)
        Me.PropertyGrid.Columns.Add(Me.Name)
        Me.PropertyGrid.DataRowTemplate = Me.dataRowTemplate1
        Me.PropertyGrid.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PropertyGrid.FixedHeaderRows.Add(Me.GroupByRow1)
        Me.PropertyGrid.FixedHeaderRows.Add(Me.ColumnManagerRow1)
        Me.PropertyGrid.GroupTemplates.Add(Me.Group1)
        Me.PropertyGrid.Location = New System.Drawing.Point(0, 0)
        Me.PropertyGrid.Name = "PropertyGrid"
        '
        'PropertyGrid.RowSelectorPane
        '
        Me.PropertyGrid.RowSelectorPane.Visible = False
        Me.PropertyGrid.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
        Me.PropertyGrid.SingleClickEdit = True
        Me.PropertyGrid.Size = New System.Drawing.Size(929, 582)
        Me.PropertyGrid.TabIndex = 3
        Me.PropertyGrid.UIStyle = Xceed.Grid.UIStyle.UIStyle.WindowsXP
        '
        'Description
        '
        Me.Description.BackColor = System.Drawing.SystemColors.Window
        Me.Description.ForeColor = System.Drawing.SystemColors.WindowText
        Me.Description.ReadOnly = True
        Me.Description.SortDirection = Xceed.Grid.SortDirection.None
        Me.Description.Title = "Description"
        Me.Description.VisibleIndex = 0
        Me.Description.Width = 441
        Me.Description.Initialize("Column1", GetType(System.String))
        '
        'Value
        '
        Me.Value.BackColor = System.Drawing.SystemColors.Window
        Me.Value.ForeColor = System.Drawing.SystemColors.WindowText
        Me.Value.SortDirection = Xceed.Grid.SortDirection.None
        Me.Value.Title = "Value"
        Me.Value.VisibleIndex = 1
        Me.Value.Width = 158
        Me.Value.Initialize("Column2", GetType(System.String))
        '
        'Category
        '
        Me.Category.SortDirection = Xceed.Grid.SortDirection.None
        Me.Category.Title = "Category"
        Me.Category.Visible = False
        Me.Category.VisibleIndex = 2
        Me.Category.Initialize("Column3", GetType(System.String))
        '
        'dataRowTemplate1
        '
        Me.dataRowTemplate1.AccessibleName = "Data row 1 in data row template"
        Me.dataRowTemplate1.BackColor = System.Drawing.SystemColors.Window
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column11)
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column21)
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column3)
        Me.dataRowTemplate1.Cells.Add(Me.celldataRowTemplate1Column4)
        Me.dataRowTemplate1.ForeColor = System.Drawing.SystemColors.WindowText
        Me.dataRowTemplate1.Height = 18
        Me.celldataRowTemplate1Column11.Initialize("Column1")
        Me.celldataRowTemplate1Column21.Initialize("Column2")
        Me.celldataRowTemplate1Column3.Initialize("Column3")
        '
        'GroupByRow1
        '
        Me.GroupByRow1.AccessibleName = "Group by row 1 in fixed header"
        Me.GroupByRow1.CellForeColor = System.Drawing.SystemColors.WindowText
        Me.GroupByRow1.ForeColor = System.Drawing.SystemColors.WindowText
        Me.GroupByRow1.Indented = False
        Me.GroupByRow1.Visible = False
        '
        'ColumnManagerRow1
        '
        Me.ColumnManagerRow1.AccessibleName = "Column manager row 2 in fixed header"
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column1)
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column2)
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column3)
        Me.ColumnManagerRow1.Cells.Add(Me.cellColumnManagerRow1Column4)
        Me.ColumnManagerRow1.ForeColor = System.Drawing.SystemColors.WindowText
        Me.ColumnManagerRow1.Height = 19
        Me.ColumnManagerRow1.Visible = False
        '
        'cellColumnManagerRow1Column1
        '
        Me.cellColumnManagerRow1Column1.ReadOnly = True
        Me.cellColumnManagerRow1Column1.Initialize("Column1")
        Me.cellColumnManagerRow1Column2.Initialize("Column2")
        Me.cellColumnManagerRow1Column3.Initialize("Column3")
        '
        'Group1
        '
        Me.Group1.GroupBy = "Column3"
        Me.Group1.HeaderRows.Add(Me.GroupManagerRow1)
        '
        'GroupManagerRow1
        '
        Me.GroupManagerRow1.TitleFormat = "%GroupTitle%"
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.ScriptBox)
        Me.TabPage2.Location = New System.Drawing.Point(4, 25)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Size = New System.Drawing.Size(929, 582)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Script"
        '
        'ScriptBox
        '
        Me.ScriptBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ScriptBox.Font = New System.Drawing.Font("Courier New", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ScriptBox.Location = New System.Drawing.Point(0, 0)
        Me.ScriptBox.Name = "ScriptBox"
        Me.ScriptBox.Size = New System.Drawing.Size(929, 582)
        Me.ScriptBox.TabIndex = 0
        Me.ScriptBox.Text = ""
        '
        'Name
        '
        Me.Name.Title = "Name"
        Me.Name.Visible = False
        Me.Name.VisibleIndex = 3
        Me.Name.Initialize("Column4", GetType(System.String))
        Me.cellColumnManagerRow1Column4.Initialize("Column4")
        Me.celldataRowTemplate1Column4.Initialize("Column4")
        '
        'RuleUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(6, 15)
        Me.ClientSize = New System.Drawing.Size(937, 674)
        Me.Controls.Add(Me.TabControl1)
        Me.Controls.SetChildIndex(Me.TabControl1, 0)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        CType(Me.PropertyGrid, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.dataRowTemplate1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.ColumnManagerRow1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage2.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    ' -----------------------------------
    ' Refresh the UI
    ' -----------------------------------
    Overrides Sub refresh()
        MyBase.Refresh()

        ' Get the cultivar file
        Dim Settings As New APSIMSettings
        Dim CultivarFilename As String = Settings.GetSetting("ApsimUI", "CultivarFile")
        If Not File.Exists(CultivarFilename) Then
            Throw New Exception("Cannot find cultivar file: " + CultivarFilename)
        Else
            Dim file As New APSIMFile
            file.Open(CultivarFilename)
            Cultivars = file.data
        End If

        InRefresh = True

        ' Fill the property grid.
        For Each Category As APSIMData In MyData.Children("category")
            AddPropertiesToGrid(Category)
        Next
        UpdateAllCultivarDropDowns()

        ' Fill the script box.
        ScriptBox.Text = MyData.Child("data").Value

        InRefresh = False
    End Sub


    ' --------------------------------------------------------------------
    ' Add a group of properties to grid for the specified data category
    ' --------------------------------------------------------------------
    Sub AddPropertiesToGrid(ByVal Data As APSIMData)
        For Each Prop As APSIMData In Data.Children("property")

            Dim row As Xceed.Grid.DataRow = PropertyGrid.DataRows.AddNew()
            row.Cells(0).Value = Prop.Attribute("description")
            row.Cells(1).Value = Prop.Attribute("value")
            row.Cells(2).Value = Data.Attribute("name")
            row.Cells(3).Value = Prop.Name
            If Prop.Attribute("type") = "yesno" Then
                Dim CheckCombo As New ComboBox
                CheckCombo.Items.Add("yes")
                CheckCombo.Items.Add("no")
                row.Cells(1).CellEditor = New CustomEditor(CheckCombo, "Text", True)
            ElseIf Prop.Attribute("type") = "list" Then
                Dim CheckCombo As New ComboBox
                Dim Values() As String = Prop.Attribute("listvalues").Split(",")
                For Each Value As String In Values
                    Value = Value.Trim
                    CheckCombo.Items.Add(Value)
                Next
                row.Cells(1).CellEditor = New CustomEditor(CheckCombo, "Text", True)
            ElseIf Prop.Attribute("type") = "modulename" Then
                Dim CheckCombo As New ComboBox
                Dim Values As StringCollection = GetMatchingModuleNames(Prop.Attribute("moduletype"))
                For Each Value As String In Values
                    Value = Value.Trim
                    CheckCombo.Items.Add(Value)
                Next
                row.Cells(1).CellEditor = New CustomEditor(CheckCombo, "Text", True)
            ElseIf Prop.Attribute("type") = "cultivars" Then
                Dim CultivarCombo As New ComboBox
                CultivarCombo.Name = Prop.Name
                row.Cells(1).CellEditor = New CustomEditor(CultivarCombo, "Text", True)
            End If
            AddHandler row.Cells(1).LeavingEdit, AddressOf Me.CellLeavingEdit
            row.EndEdit()
        Next

    End Sub


    ' -------------------------------------------------------------------
    ' Keep track of when the user changes the text in the script box.
    ' -------------------------------------------------------------------
    Private Sub ScriptBox_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ScriptBox.TextChanged
        ScriptHasChanged = True
    End Sub


    ' --------------------------------------
    ' Save the script box if it has changd.
    ' --------------------------------------
    Overrides Sub SaveToAPSIMFile()
        If ScriptHasChanged Then
            MyData.Child("data").Value = ScriptBox.Text
        End If
    End Sub


    ' -----------------------------------------------------------------------------
    ' A cell has changed values - if user has made the change then update the
    ' value in MyData
    ' -----------------------------------------------------------------------------
    Private Sub CellLeavingEdit(ByVal sender As Object, ByVal e As Xceed.Grid.LeavingEditEventArgs)
        If Not InRefresh Then
            Dim Row As Xceed.Grid.DataRow = PropertyGrid.CurrentCell.ParentRow
            Dim Category As APSIMData = MyData.Child(Row.Cells(2).Value)
            Dim Prop As APSIMData = Category.Child(Row.Cells(3).Value)
            Prop.SetAttribute("value", e.NewValue)
            If Row.Cells(3).Value = "crop" Then
                UpdateAllCultivarDropDowns()
            End If
        End If
    End Sub


    ' ------------------------------------------------------------------
    ' Return a list of instance names for the specified module name
    ' ------------------------------------------------------------------
    Function GetMatchingModuleNames(ByVal ModuleNameToMatch As String) As StringCollection
        Dim Values As New StringCollection
        Dim System As APSIMData = MyData.Parent.Parent
        If Not IsNothing(System) Then
            For Each ApsimModule As APSIMData In System.Children
                If ModuleNameToMatch = "" Or ApsimModule.Type = ModuleNameToMatch Then
                    Values.Add(ApsimModule.Name())
                End If
            Next
        End If
        Return Values
    End Function


    ' ----------------------------------
    ' Update any cultivar drop downs.
    ' ----------------------------------
    Sub UpdateAllCultivarDropDowns()
        Dim RowIndex As Integer = 0
        For Each Category As APSIMData In MyData.Children("category")
            For Each Prop As APSIMData In Category.Children("property")
                If Prop.Attribute("type") = "cultivars" Then
                    PopulateCultivarDropDown(Prop, PropertyGrid.DataRows(RowIndex))
                End If
                RowIndex = RowIndex + 1
            Next
        Next
    End Sub


    ' ------------------------------------------------------------------
    ' Populate a cultivar combo box
    ' ------------------------------------------------------------------
    Sub PopulateCultivarDropDown(ByVal CultivarProp As APSIMData, ByVal Row As Xceed.Grid.DataRow)
        Dim Values As New StringCollection
        Dim CropPropertyName As String = CultivarProp.Attribute("croppropertyname")

        ' Locate the crop property name to get the instance name of the crop.
        Dim InstanceName As String
        For Each Category As APSIMData In MyData.Children("category")
            For Each Prop As APSIMData In Category.Children("property")
                If Prop.Attribute("name").ToLower = CropPropertyName Then
                    InstanceName = Prop.Attribute("value")
                End If
            Next
        Next

        ' If we now have an crop instance name then we can populate the cultivar box.
        If InstanceName <> "" Then
            Dim Crop As APSIMData = Cultivars.Child(InstanceName)
            If Not IsNothing(Crop) Then
                Values = Crop.ChildList
            End If
        End If

        ' populate the dropdown
        Dim C As Control = Row.Cells(1).CellEditor.Control
        Dim DropDown As ComboBox = C
        For Each Item As String In Values
            DropDown.Items.Add(Item)
        Next
    End Sub

End Class
