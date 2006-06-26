Imports VBGeneral
Imports System.Collections
Imports System.Collections.Specialized
Imports System.IO
Imports FarPoint.Win.Spread

Public Class RuleUI
    Inherits VBGeneral.BaseView
    Dim InRefresh As Boolean
    Dim Cultivars As APSIMData


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
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents FpSpread1 As FarPoint.Win.Spread.FpSpread
    Friend WithEvents PropertyGrid As FarPoint.Win.Spread.SheetView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim TipAppearance1 As FarPoint.Win.Spread.TipAppearance = New FarPoint.Win.Spread.TipAppearance
        Me.TabControl1 = New System.Windows.Forms.TabControl
        Me.TabPage1 = New System.Windows.Forms.TabPage
        Me.FpSpread1 = New FarPoint.Win.Spread.FpSpread
        Me.PropertyGrid = New FarPoint.Win.Spread.SheetView
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PropertyGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(0, 40)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(1020, 469)
        Me.TabControl1.TabIndex = 3
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.FpSpread1)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Size = New System.Drawing.Size(1012, 443)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Properties"
        '
        'FpSpread1
        '
        Me.FpSpread1.AccessibleDescription = "FpSpread1, Sheet1, Row 0, Column 0, "
        Me.FpSpread1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FpSpread1.EditModeReplace = True
        Me.FpSpread1.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.FpSpread1.Location = New System.Drawing.Point(0, 0)
        Me.FpSpread1.Name = "FpSpread1"
        Me.FpSpread1.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.PropertyGrid})
        Me.FpSpread1.Size = New System.Drawing.Size(1012, 443)
        Me.FpSpread1.TabIndex = 0
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.FpSpread1.TextTipAppearance = TipAppearance1
        Me.FpSpread1.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        '
        'PropertyGrid
        '
        Me.PropertyGrid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.PropertyGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.PropertyGrid.ColumnCount = 4
        Me.PropertyGrid.AutoUpdateNotes = True
        Me.PropertyGrid.ColumnHeader.Visible = False
        Me.PropertyGrid.Columns.Get(0).Width = 298.0!
        Me.PropertyGrid.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Left
        Me.PropertyGrid.Columns.Get(1).Width = 136.0!
        Me.PropertyGrid.Columns.Get(2).Visible = False
        Me.PropertyGrid.Columns.Get(3).Visible = False
        Me.PropertyGrid.RowHeader.Columns.Default.Resizable = False
        Me.PropertyGrid.RowHeader.Visible = False
        Me.PropertyGrid.SheetName = "Sheet1"
        Me.PropertyGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'RuleUI
        '
        Me.Controls.Add(Me.TabControl1)
        Me.Name = "RuleUI"
        Me.Size = New System.Drawing.Size(1020, 509)
        Me.Controls.SetChildIndex(Me.TabControl1, 0)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PropertyGrid, System.ComponentModel.ISupportInitialize).EndInit()
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
        Dim CultivarFilename As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "ApsimUI", "CultivarFile")
        If Not File.Exists(CultivarFilename) Then
            Throw New System.Exception("Cannot find cultivar file: " + CultivarFilename)
        Else
            Dim FileData As New APSIMData
            FileData.LoadFromFile(CultivarFilename)
            Cultivars = FileData
        End If

        InRefresh = True

        ' Fill the property grid.
        PropertyGrid.RowCount = 0
        PropertyGrid.RowCount = 100
        Dim Row As Integer = 0
        For Each Category As APSIMData In Controller.Data.Children("category")
            AddPropertiesToGrid(Category, Row)
        Next
        PropertyGrid.RowCount = Row
        UpdateAllCropDropDowns()

        ' Create a tab for each condition.
        While TabControl1.TabPages.Count > 1
            TabControl1.TabPages.RemoveAt(1)
        End While
        For Each Condition As APSIMData In Controller.Data.Children("condition")
            Dim page As New TabPage(Condition.Name)
            Dim ScriptBox As New RichTextBox
            ScriptBox.Text = Condition.Value
            ScriptBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            page.Controls.Add(ScriptBox)
            ScriptBox.Dock = DockStyle.Fill
            TabControl1.TabPages.Add(page)
        Next

        InRefresh = False
    End Sub


    ' --------------------------------------------------------------------
    ' Add a group of properties to grid for the specified data category
    ' --------------------------------------------------------------------
    Sub AddPropertiesToGrid(ByVal Data As APSIMData, ByRef Row As Integer)
        PropertyGrid.Cells(Row, 0).Value = Data.Name
        PropertyGrid.Rows(Row).BackColor = Color.LightSteelBlue
        PropertyGrid.Rows(Row).Locked = True
        Row = Row + 1

        Dim ApsimUI As ApsimUIController = Controller

        For Each Prop As APSIMData In Data.Children("property")
            PropertyGrid.Cells(Row, 0).Value = Prop.Attribute("description")
            PropertyGrid.Cells(Row, 1).Value = Prop.Attribute("value")
            PropertyGrid.Cells(Row, 2).Value = Data.Attribute("name")
            PropertyGrid.Cells(Row, 3).Value = Prop.Name
            ApsimUI.SetCellType(PropertyGrid, Row, 1, Prop)
            If TypeOf (PropertyGrid.Cells(Row, 1).CellType) Is FarPoint.Win.Spread.CellType.ComboBoxCellType Then
                Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = PropertyGrid.Cells(Row, 1).CellType

                If Trim(PropertyGrid.Cells(Row, 1).Value) = "" And Combo.Items.Length > 0 Then
                    ' This is so that the cellchanged event is acted upon in the handler below.
                    InRefresh = False
                    PropertyGrid.Cells(Row, 1).Value = Combo.Items(0)
                    InRefresh = True
                End If
            End If
            Row = Row + 1
        Next
    End Sub

    ' --------------------------------------
    ' Save the script box if it has changd.
    ' --------------------------------------
    Overrides Sub Save()

        Dim index As Integer = 1

        'stop editing the current cell before the save is called.  NullReferenceException is thrown when 
        'user moves from node in the tree to another without editing.
        If Not IsNothing(Me.FpSpread1.ActiveSheet.ActiveCell) AndAlso Not IsNothing(Me.FpSpread1.ActiveSheet.ActiveCell.Editor) Then
            Me.FpSpread1.ActiveSheet.ActiveCell.Editor.StopEditing()

        End If

        For Each Condition As APSIMData In Controller.Data.Children("condition")
            Dim page As TabPage = TabControl1.TabPages.Item(index)
            Dim ScriptBox As RichTextBox = page.Controls.Item(0)
            Condition.Value = ScriptBox.Text
            index = index + 1

        Next
    End Sub


    ' -----------------------------------------------------------------------------
    ' A cell has changed values - if user has made the change then update the
    ' value in MyData
    ' -----------------------------------------------------------------------------
    Private Sub PropertyGrid_CellChanged(ByVal sender As Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles PropertyGrid.CellChanged

        If Not InRefresh Then
            Dim Category As APSIMData = Controller.Data.Child(PropertyGrid.Cells(e.Row, 2).Value)
            Dim Prop As APSIMData = Category.Child(PropertyGrid.Cells(e.Row, 3).Value)

            ' Cells with a type of 'ddmmmdate' use the value in the text property to set the 'value' attribute
            ' Prop object.  Otherwise use the 'value' property
            If Prop.Attribute("type").ToLower().Equals("ddmmmdate") Then

                If IsValidDate(Me.FpSpread1.ActiveSheet.Cells(e.Row, e.Column).Value) Then
                    Prop.SetAttribute("value", PropertyGrid.Cells(e.Row, 1).Text)

                Else
                    Me.FpSpread1.ActiveSheet.SetActiveCell(e.Row, e.Column)

                End If



            Else
                Prop.SetAttribute("value", PropertyGrid.Cells(e.Row, 1).Value)
                If PropertyGrid.Cells(e.Row, 3).Value = "crop" Then UpdateAllCropDropDowns()
            End If

        End If

    End Sub


    Private Function IsValidDate(ByVal aDate As String) As Boolean
        'Checks if the given date is valid against an Australian Culture

        Dim systemDateInfo As System.Globalization.DateTimeFormatInfo = New System.Globalization.CultureInfo("en-AU").DateTimeFormat()
        Dim blnIsValid As Boolean = True

        Try
            Date.Parse(aDate, systemDateInfo)

        Catch fe As System.FormatException
            MessageBox.Show(fe.Message, "Invalid Date", MessageBoxButtons.OK, MessageBoxIcon.Error)
            blnIsValid = False
        End Try

        Return blnIsValid

    End Function
    ' ----------------------------------
    ' Update any cultivar drop downs.
    ' ----------------------------------
    Sub UpdateAllCropDropDowns()
        Dim Row As Integer = 0
        For Each Category As APSIMData In Controller.Data.Children("category")
            If PropertyGrid.Rows(Row).Locked Then
                Row = Row + 1
            End If
            For Each Prop As APSIMData In Category.Children("property")
                If Prop.Attribute("type") = "cultivars" Then
                    PopulateDropDown("cultivar", Prop, PropertyGrid.Cells(Row, 1).CellType)
                ElseIf Prop.Attribute("type") = "classes" Then
                    PopulateDropDown("class", Prop, PropertyGrid.Cells(Row, 1).CellType)
                End If
                Row = Row + 1
            Next
        Next
    End Sub


    ' ------------------------------------------------------------------
    ' Populate a cultivar combo box
    ' ------------------------------------------------------------------
    Sub PopulateDropDown(ByVal PropType As String, ByVal CropProp As APSIMData, ByVal Combo As CellType.ComboBoxCellType)
        Dim Values As New StringCollection
        Dim CropPropertyName As String = CropProp.Attribute("croppropertyname")

        ' Locate the crop property name to get the instance name of the crop.
        Dim InstanceName As String = ""
        For Each Category As APSIMData In Controller.Data.Children("category")
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
                Values = Crop.ChildList(PropType)
            End If
        End If

        ' populate the dropdown
        Dim Items(Values.Count - 1) As String
        Values.CopyTo(Items, 0)
        Combo.Items = Items
    End Sub

End Class
