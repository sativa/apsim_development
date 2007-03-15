Imports VBGeneral
Imports System.Collections
Imports System.Collections.Specialized
Imports FarPoint.Win.Spread
Imports System.IO

Public Class GenericUI
    Inherits BaseView
    Dim InRefresh As Boolean
    Dim PropertyData As New ArrayList
    Delegate Sub NotifyEventHandler()
    Public Event PropertiesChangedEvent As NotifyEventHandler


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
        Dim TipAppearance1 As FarPoint.Win.Spread.TipAppearance = New FarPoint.Win.Spread.TipAppearance
        Me.PictureBox = New System.Windows.Forms.PictureBox
        Me.FpSpread1 = New FarPoint.Win.Spread.FpSpread
        Me.Grid = New FarPoint.Win.Spread.SheetView
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureBox
        '
        Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox.Location = New System.Drawing.Point(0, 40)
        Me.PictureBox.Name = "PictureBox"
        Me.PictureBox.Size = New System.Drawing.Size(88, 677)
        Me.PictureBox.TabIndex = 3
        Me.PictureBox.TabStop = False
        '
        'FpSpread1
        '
        Me.FpSpread1.AccessibleDescription = "FpSpread1, Sheet1, Row 0, Column 0, "
        Me.FpSpread1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FpSpread1.EditModeReplace = True
        Me.FpSpread1.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.FpSpread1.Location = New System.Drawing.Point(88, 40)
        Me.FpSpread1.Name = "FpSpread1"
        Me.FpSpread1.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.Grid})
        Me.FpSpread1.Size = New System.Drawing.Size(934, 677)
        Me.FpSpread1.TabIndex = 4
        Me.FpSpread1.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Never
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.FpSpread1.TextTipAppearance = TipAppearance1
        Me.FpSpread1.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.FpSpread1.TextTipPolicy = FarPoint.Win.Spread.TextTipPolicy.Floating
        Me.FpSpread1.TextTipDelay = 500
        '
        'Grid
        '
        Me.Grid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.Grid.ColumnCount = 3
        Me.Grid.AutoUpdateNotes = True
        Me.Grid.ColumnHeader.Cells.Get(0, 0).Value = "Name"
        Me.Grid.ColumnHeader.Cells.Get(0, 1).Value = "Value"
        Me.Grid.ColumnHeader.Cells.Get(0, 2).Value = " "
        Me.Grid.Columns.Get(0).Label = "Name"
        Me.Grid.Columns.Get(0).Locked = True
        Me.Grid.Columns.Get(0).Width = 272.0!
        Me.Grid.Columns.Get(1).Label = "Value"
        Me.Grid.Columns.Get(1).Width = 206.0!
        Me.Grid.Columns.Get(2).Label = " "
        Me.Grid.Columns.Get(2).Visible = False
        Me.Grid.Columns.Get(2).Width = 21.0!
        Me.Grid.RowHeader.Columns.Default.Resizable = False
        Me.Grid.RowHeader.Visible = False
        Me.Grid.SheetName = "Sheet1"
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'GenericUI
        '
        Me.Controls.Add(Me.FpSpread1)
        Me.Controls.Add(Me.PictureBox)
        Me.Name = "GenericUI"
        Me.Size = New System.Drawing.Size(1022, 717)
        Me.Controls.SetChildIndex(Me.PictureBox, 0)
        Me.Controls.SetChildIndex(Me.FpSpread1, 0)
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub RefreshView(ByVal Controller As BaseController)
        ' --------------------------------------------------------------------
        ' Refresh this user interface
        ' --------------------------------------------------------------------
        MyBase.RefreshView(Controller)

        InRefresh = True
        Grid.Columns(2).Locked = True

        If Not IsNothing(Controller.Data) Then
            ' set the banner image correctly.
            Dim inifile As New APSIMSettings
            Dim imagefile As String = Controller.ImageFileForType(Controller.Data.Type)
            If imagefile <> "" And File.Exists(imagefile) Then
                PictureBox.BackgroundImage = Drawing.Image.FromFile(imagefile)
            End If

            Grid.RowCount = 0
            Grid.RowCount = 1000
            PropertyData.Clear()
            Dim Row As Integer = 0
            If Controller.Data.Children("category").Length > 0 Then
                For Each Category As APSIMData In Controller.Data.Children("category")
                    PropertyData.Add(Nothing)
                    Grid.Cells(Row, 0).Value = Category.Name
                    Grid.Rows(Row).BackColor = System.Drawing.Color.LightSteelBlue
                    Grid.Rows(Row).Locked = True
                    Row = Row + 1
                    AddPropertiesToGrid(Category, Row)
                Next
            Else
                AddPropertiesToGrid(Controller.Data, Row)
            End If
            Grid.RowCount = Row
        End If

        InRefresh = False

    End Sub

    Sub AddPropertiesToGrid(ByVal Data As APSIMData, ByRef Row As Integer)
        ' --------------------------------------------------------------------
        ' Add a group of properties to grid for the specified data. Row is 
        ' updated and returned to caller.
        ' --------------------------------------------------------------------
        For Each Prop As APSIMData In Data.Children
            If Prop.ChildNames.Length = 0 Then
                Dim ExistingIndex As Integer = FindExistingProp(Prop)
                If ExistingIndex = -1 Then
                    PropertyData.Add(Prop)
                    Grid.Cells(Row, 0).Value = Prop.Attribute("description")
                    Grid.Cells(Row, 0).Tag = Prop.Name
                    If Grid.Cells(Row, 0).Value = "" Then
                        Grid.Cells(Row, 0).Value = Prop.Name
                    End If
                    Controller.CreateCellEditorForRow(Prop, Grid, Row)
                    Controller.PopulateCellEditor(Prop, Grid.Cells(Row, 1).CellType)
                    Grid.Cells(Row, 1).Value = Prop.Value
                    ' If the cell type is a combo box and the cell value is blank then set the cell value
                    ' to the first item in the combo box.
                    If TypeOf (Grid.Cells(Row, 1).CellType) Is FarPoint.Win.Spread.CellType.ComboBoxCellType Then
                        Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = Grid.Cells(Row, 1).CellType
                        If Trim(Grid.Cells(Row, 1).Value) = "" And Combo.Items.Length > 0 Then
                            Grid.Cells(Row, 1).Value = Combo.Items(0)
                            UpdateEditors()
                        End If
                    End If
                    Row = Row + 1
                Else
                    Grid.Cells(ExistingIndex, 1).Text += vbCrLf + Prop.Value
                    If PropertyData(ExistingIndex).Attribute("type").ToLower() = "filenames" Then
                        Grid.Rows(ExistingIndex).Height = Math.Min(Grid.Rows(ExistingIndex).GetPreferredHeight, 300)
                    End If
                End If
            End If
        Next
        If Controller.MsgBoxString <> "" Then
            MsgBox(Controller.MsgBoxString, MsgBoxStyle.Information)
            Controller.MsgBoxString = ""
        End If
    End Sub

    Private Function FindExistingProp(ByVal DataToFind As APSIMData)
        For i As Integer = 0 To PropertyData.Count - 1
            Dim Data As APSIMData = PropertyData(i)
            If Not IsNothing(Data) AndAlso DataToFind.Name = Data.Name Then
                Return i
            End If
        Next
        Return -1
    End Function

    Private Sub Grid_CellChanged(ByVal sender As Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles Grid.CellChanged
        ' --------------------------------------------------------------------
        ' User has changed something - go save change to Data
        ' --------------------------------------------------------------------
        If e.Column = 1 And Not InRefresh Then
            Dim Prop As APSIMData = PropertyData(e.Row)
            Dim Values() As String
            If Grid.Cells(e.Row, 1).Text = "" Then
                Dim SingleValue(0) As String
                SingleValue(0) = ""
                Values = SingleValue
            Else
                Values = Grid.Cells(e.Row, 1).Text.Split(vbCrLf.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
            End If

            If Values.Length > 0 Then
                Dim Name As String = Prop.Name
                Dim Type As String = Prop.Type
                Dim CellType As String = Prop.Attribute("type")
                Dim Description As String = Prop.Attribute("description")
                Dim Parent As APSIMData = Prop.Parent
                Parent.BeginUpdate()

                Parent.EnsureNumberOfChildren(Type, Name, Values.Length)

                Dim Children() As APSIMData = Parent.Children(Type)

                ' Create new properties
                For i As Integer = 0 To Values.Length - 1
                    If Description <> "" Then
                        Children(i).SetAttribute("description", Description)
                    End If
                    If CellType <> "" Then
                        Children(i).SetAttribute("type", CellType)
                    End If
                    Children(i).Value = Values(i)
                Next
                Parent.EndUpdate()
                RaiseEvent PropertiesChangedEvent()
            End If
            UpdateEditors()
        End If
    End Sub

    Private Sub UpdateEditors()
        ' Update all cell editors now that we've changed a cell. 
        ' e.g. a cultivars drop down may need updating if we just changed a crop.
        For Row As Integer = 0 To PropertyData.Count - 1
            If Not IsNothing(PropertyData(Row)) Then
                Dim RowProp As APSIMData = PropertyData(Row)
                If RowProp.Attribute("type").ToLower() = "filenames" Then
                    Grid.Rows(Row).Height = Grid.Rows(Row).GetPreferredHeight
                End If

                If Row < Grid.RowCount AndAlso Not Grid.Rows(Row).Locked Then
                    Controller.PopulateCellEditor(PropertyData(Row), Grid.Cells(Row, 1).CellType)
                End If
            End If
        Next
    End Sub
    Overrides Sub Save()
        ' --------------------------------------------------------------
        ' User has clicked elsewhere make sure we drop the cell focus
        ' so that the CellChanged event is fired.
        ' --------------------------------------------------------------
        If Not IsNothing(Me.FpSpread1.ActiveSheet.ActiveCell) AndAlso Not IsNothing(Me.FpSpread1.ActiveSheet.ActiveCell.Editor) Then
            Me.FpSpread1.ActiveSheet.ActiveCell.Editor.StopEditing()
        End If
    End Sub



    'Private Sub FpSpread1_Validating(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles FpSpread1.Validating
    '    Me.FpSpread1.ActiveSheet.RaiseCellChanged(Me.Grid.ActiveCell.Row.Index, Me.Grid.ActiveCell.Column.Index)
    'End Sub

    Private Sub FpSpread1_ButtonClicked(ByVal sender As System.Object, ByVal e As FarPoint.Win.Spread.EditorNotifyEventArgs) Handles FpSpread1.ButtonClicked
        ' --------------------------------------------------------------
        ' User has clicked a button in a cell somewhere on our grid.
        ' Pass event to BaseController so that it can act on it.
        ' --------------------------------------------------------------
        Controller.OnButtonClick(sender, e, PropertyData(e.Row))
    End Sub

    Private Sub FpSpread1_TextTipFetch(ByVal sender As System.Object, ByVal e As FarPoint.Win.Spread.TextTipFetchEventArgs) Handles FpSpread1.TextTipFetch
        e.ShowTip = True
        If Not IsNothing(Grid.Cells(e.Row, e.Column).Tag) Then
            e.TipText = "The APSIM variable for this field is [" + Grid.Cells(e.Row, e.Column).Tag.ToString() + "]"
        End If

    End Sub
End Class
