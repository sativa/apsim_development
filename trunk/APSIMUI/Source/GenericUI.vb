Imports VBGeneral
Imports Xceed.Grid.Editors
Imports System.Collections
Imports System.Collections.Specialized
Imports FarPoint.Win.Spread

Public Class GenericUI
    Inherits VBGeneral.BaseView
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
    Friend WithEvents PictureBox As System.Windows.Forms.PictureBox
    Friend WithEvents FpSpread1 As FarPoint.Win.Spread.FpSpread
    Friend WithEvents Grid As FarPoint.Win.Spread.SheetView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.PictureBox = New System.Windows.Forms.PictureBox
        Me.FpSpread1 = New FarPoint.Win.Spread.FpSpread
        Me.Grid = New FarPoint.Win.Spread.SheetView
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureBox
        '
        Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox.Location = New System.Drawing.Point(0, 40)
        Me.PictureBox.Name = "PictureBox"
        Me.PictureBox.Size = New System.Drawing.Size(88, 594)
        Me.PictureBox.TabIndex = 3
        Me.PictureBox.TabStop = False
        '
        'FpSpread1
        '
        Me.FpSpread1.AllowDragDrop = True
        Me.FpSpread1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FpSpread1.EditModeReplace = True
        Me.FpSpread1.Location = New System.Drawing.Point(88, 40)
        Me.FpSpread1.Name = "FpSpread1"
        Me.FpSpread1.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.Grid})
        Me.FpSpread1.Size = New System.Drawing.Size(570, 594)
        Me.FpSpread1.TabIndex = 4
        Me.FpSpread1.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Never
        '
        'Grid
        '
        Me.Grid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.Grid.ColumnCount = 2
        Me.Grid.ColumnHeader.Cells.Get(0, 0).Text = "Name"
        Me.Grid.ColumnHeader.Cells.Get(0, 1).Text = "Value"
        Me.Grid.Columns.Get(0).Label = "Name"
        Me.Grid.Columns.Get(0).Width = 206.0!
        Me.Grid.Columns.Get(1).Label = "Value"
        Me.Grid.Columns.Get(1).Width = 206.0!
        Me.Grid.RowHeader.Columns.Default.Resizable = False
        Me.Grid.RowHeader.Visible = False
        Me.Grid.SheetName = "Sheet1"
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'GenericUI
        '
        Me.ClientSize = New System.Drawing.Size(658, 634)
        Me.Controls.Add(Me.FpSpread1)
        Me.Controls.Add(Me.PictureBox)
        Me.Name = "GenericUI"
        Me.Controls.SetChildIndex(Me.PictureBox, 0)
        Me.Controls.SetChildIndex(Me.FpSpread1, 0)
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub Refresh()
        MyBase.Refresh()

        InRefresh = True

        Dim inifile As New APSIMSettings
        Dim UIManager As ApsimUIController = Controller
        Dim imagefile As String = UIManager.ImageFileForType(Controller.Data.Type)
        PictureBox.Image = Image.FromFile(imagefile)
        Grid.RowCount = Controller.Data.ChildList.Count
        Dim row As Integer = 0
        For Each Prop As APSIMData In Controller.Data.Children()
            If Prop.Children.Count = 0 Then
                Dim Desc As String = Prop.Attribute("description")
                If Desc = "" Then
                    Desc = Prop.Name
                End If
                Grid.Cells(row, 0).Value = Desc
                Grid.Cells(row, 1).Value = Prop.Value
                Dim Editor As CustomEditor
                Dim DefaultText As String
                SetCellType(Grid, row, 1, Prop)
            End If
            row = row + 1
        Next

        InRefresh = False

    End Sub


    Private Sub Grid_CurrentCellChanged1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Me.Visible Then
            Controller.Data.DataTable = Grid.DataSource
        End If
    End Sub


    ' --------------------------------
    ' Set the type of a grid column
    ' --------------------------------
    Shared Sub SetCellType(ByVal Grid As SheetView, _
                            ByVal row As Integer, _
                            ByVal col As Integer, _
                            ByVal Prop As APSIMData)
        If Prop.Attribute("type") = "yesno" Then
            Dim Combo As CellType.ComboBoxCellType = New CellType.ComboBoxCellType
            Combo.Items = New String() {"yes", "no"}
            Grid.Cells(row, col).CellType = Combo
        ElseIf Prop.Attribute("type") = "date" Then
            Dim DateEditor As CellType.DateTimeCellType = New CellType.DateTimeCellType
            DateEditor.DateDefault = Prop.Value
            DateEditor.DropDownButton = True
            Grid.Cells(row, col).CellType = DateEditor
        ElseIf Prop.Attribute("type") = "list" Then
            Dim Combo As CellType.ComboBoxCellType = New CellType.ComboBoxCellType
            Combo.Items = Prop.Attribute("listvalues").Split(",")
            Grid.Cells(row, col).CellType = Combo
        ElseIf Prop.Attribute("type") = "modulename" Then
            Dim Combo As CellType.ComboBoxCellType = New CellType.ComboBoxCellType
            Combo.Items = GetMatchingModuleNames(Prop)
            Grid.Cells(row, col).CellType = Combo
        ElseIf Prop.Attribute("type") = "cultivars" Then
            Dim Combo As CellType.ComboBoxCellType = New CellType.ComboBoxCellType
            Combo.Items = GetMatchingModuleNames(Prop)
            Grid.Cells(row, col).CellType = Combo
        End If
    End Sub


    ' ------------------------------------------------------------------
    ' Return a list of instance names for the specified module name
    ' ------------------------------------------------------------------
    Shared Function GetMatchingModuleNames(ByVal Prop As APSIMData) As String()
        Dim Values As New StringCollection
        Dim System As APSIMData = Prop.Parent
        While System.Type <> "simulation" And System.Type <> "area" And Not IsNothing(System.Parent)
            System = System.Parent
        End While

        For Each ApsimModule As APSIMData In System.Children
            If Prop.Attribute("moduletype") = "" Or ApsimModule.Type = Prop.Attribute("moduletype") Then
                Values.Add(ApsimModule.Name())
            End If
        Next
        Dim ReturnValues(Values.Count - 1) As String
        Values.CopyTo(ReturnValues, 0)
        Return ReturnValues
    End Function


    Private Sub Grid_CellChanged(ByVal sender As Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles Grid.CellChanged
        If Not InRefresh Then
            Dim Prop As APSIMData = Controller.Data.Children()(e.Row)
            Prop.Value = Grid.Cells(e.Row, e.Column).Value
        End If
    End Sub
End Class
