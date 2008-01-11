Imports VBGeneral
Imports CSGeneral
Imports System.Collections
Imports System.Collections.Generic
Imports System.Collections.Specialized
Imports FarPoint.Win.Spread
Imports System.IO
Imports System.Xml

Public Class GenericUI
    Inherits BaseView
    Private Empty As FarPoint.Win.Spread.CellType.EmptyCellType = New FarPoint.Win.Spread.CellType.EmptyCellType

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        Dim InputMap As FarPoint.Win.Spread.InputMap = FpSpread1.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused)
        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None), _
                    FarPoint.Win.Spread.SpreadActions.ClipboardCut)
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
    Friend WithEvents PopupMenu As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents EditModeItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents Grid As FarPoint.Win.Spread.SheetView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim TipAppearance1 As FarPoint.Win.Spread.TipAppearance = New FarPoint.Win.Spread.TipAppearance
        Dim ComboBoxCellType1 As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
        Me.PictureBox = New System.Windows.Forms.PictureBox
        Me.FpSpread1 = New FarPoint.Win.Spread.FpSpread
        Me.PopupMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.EditModeItem = New System.Windows.Forms.ToolStripMenuItem
        Me.Grid = New FarPoint.Win.Spread.SheetView
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.PopupMenu.SuspendLayout()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureBox
        '
        Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox.Location = New System.Drawing.Point(0, 18)
        Me.PictureBox.Name = "PictureBox"
        Me.PictureBox.Size = New System.Drawing.Size(88, 699)
        Me.PictureBox.TabIndex = 3
        Me.PictureBox.TabStop = False
        '
        'FpSpread1
        '
        Me.FpSpread1.AccessibleDescription = "FpSpread1, Sheet1, Row 0, Column 0, "
        Me.FpSpread1.ClipboardOptions = FarPoint.Win.Spread.ClipboardOptions.NoHeaders
        Me.FpSpread1.ContextMenuStrip = Me.PopupMenu
        Me.FpSpread1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FpSpread1.EditModeReplace = True
        Me.FpSpread1.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.FpSpread1.Location = New System.Drawing.Point(88, 18)
        Me.FpSpread1.Name = "FpSpread1"
        Me.FpSpread1.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.Grid})
        Me.FpSpread1.Size = New System.Drawing.Size(934, 699)
        Me.FpSpread1.TabIndex = 4
        Me.FpSpread1.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Never
        Me.FpSpread1.TabStripRatio = 0.284684684684685
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.FpSpread1.TextTipAppearance = TipAppearance1
        Me.FpSpread1.TextTipPolicy = FarPoint.Win.Spread.TextTipPolicy.Floating
        Me.FpSpread1.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.FpSpread1.SetViewportPreferredWidth(0, 532)
        '
        'PopupMenu
        '
        Me.PopupMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.EditModeItem})
        Me.PopupMenu.Name = "PopupMenu"
        Me.PopupMenu.Size = New System.Drawing.Size(147, 26)
        '
        'EditModeItem
        '
        Me.EditModeItem.Name = "EditModeItem"
        Me.EditModeItem.Size = New System.Drawing.Size(146, 22)
        Me.EditModeItem.Text = "Edit mode"
        '
        'Grid
        '
        Me.Grid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.Grid.ColumnCount = 6
        Me.Grid.AutoUpdateNotes = True
        Me.Grid.ColumnHeader.Cells.Get(0, 0).Value = "Name"
        Me.Grid.ColumnHeader.Cells.Get(0, 1).Value = "Type"
        Me.Grid.ColumnHeader.Cells.Get(0, 2).Value = "List items (csv)"
        Me.Grid.ColumnHeader.Cells.Get(0, 3).Value = "Description"
        Me.Grid.ColumnHeader.Cells.Get(0, 4).Value = "Value"
        Me.Grid.ColumnHeader.Cells.Get(0, 5).Value = " "
        Me.Grid.Columns.Get(0).BackColor = System.Drawing.Color.LavenderBlush
        Me.Grid.Columns.Get(0).Label = "Name"
        Me.Grid.Columns.Get(0).Width = 75.0!
        Me.Grid.Columns.Get(1).BackColor = System.Drawing.Color.LavenderBlush
        ComboBoxCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right
        ComboBoxCellType1.Editable = True
        ComboBoxCellType1.Items = New String() {"text", "date", "ddmmmdate", "yesno", "crop", "cultivars", "classes", "modulename", "list", "multilist", "category", "filename", "multiedit"}
        ComboBoxCellType1.MaxDrop = 12
        Me.Grid.Columns.Get(1).CellType = ComboBoxCellType1
        Me.Grid.Columns.Get(1).Label = "Type"
        Me.Grid.Columns.Get(1).Width = 92.0!
        Me.Grid.Columns.Get(2).BackColor = System.Drawing.Color.LavenderBlush
        Me.Grid.Columns.Get(2).Label = "List items (csv)"
        Me.Grid.Columns.Get(2).Width = 91.0!
        Me.Grid.Columns.Get(3).Label = "Description"
        Me.Grid.Columns.Get(3).Locked = True
        Me.Grid.Columns.Get(3).Width = 272.0!
        Me.Grid.Columns.Get(4).Label = "Value"
        Me.Grid.Columns.Get(4).Width = 206.0!
        Me.Grid.Columns.Get(5).Label = " "
        Me.Grid.Columns.Get(5).Visible = False
        Me.Grid.Columns.Get(5).Width = 21.0!
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
        Me.PopupMenu.ResumeLayout(False)
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Protected Overrides Sub OnLoad()
        MyBase.OnLoad()
        Dim InputMap As FarPoint.Win.Spread.InputMap = FpSpread1.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused)

        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None), _
                    FarPoint.Win.Spread.SpreadActions.ClipboardCut)
        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None), _
                    FarPoint.Win.Spread.SpreadActions.MoveToNextRow)
    End Sub

    Public Overrides Sub OnRefresh()
        ' --------------------------------------------------------------------
        ' Refresh this user interface
        ' --------------------------------------------------------------------
        'InRefresh = True

        Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(NodePath)

        ' set the banner image correctly.
        Dim inifile As New APSIMSettings
        Dim imagefile As String = Controller.Configuration.Info(Comp.Type, "image")
        If imagefile <> "" And File.Exists(imagefile) Then
            PictureBox.BackgroundImage = Drawing.Image.FromFile(imagefile)
        End If

        Grid.Columns(0).Visible = False
        Grid.Columns(1).Visible = False
        Grid.Columns(2).Visible = False
        EditModeItem.Checked = False

        ' Populate the grid.
        Grid.RowCount = 0
        Grid.RowCount = 1000
        Dim Row As Integer = 0
        PopulateGrid(Data, Row)
        Grid.RowCount = Math.Max(Row, 1)

        'InRefresh = False
    End Sub
    Private Sub PopulateGrid(ByVal Data As XmlNode, ByRef Row As Integer)
        ' --------------------------------------------------------------------
        ' Add a group of properties to grid for the specified data. Row is 
        ' updated and returned to caller.
        ' --------------------------------------------------------------------
        For Each Prop As XmlNode In XmlHelper.ChildNodes(Data, "")
            If Prop.Name = "category" Then
                Grid.Cells(Row, 1).Text = "category"
                Grid.Cells(Row, 3).Text = XmlHelper.Attribute(Prop, "description")
                If Grid.Cells(Row, 3).Text = "" Then
                    Grid.Cells(Row, 3).Text = XmlHelper.Name(Prop)
                End If
            ElseIf Prop.Name <> "condition" Then
                Grid.Cells(Row, 0).Text = Prop.Name
                Grid.Cells(Row, 1).Text = XmlHelper.Attribute(Prop, "type")
                If Grid.Cells(Row, 1).Text = "" Then
                    Grid.Cells(Row, 1).Text = "text"
                End If
                Grid.Cells(Row, 2).Text = XmlHelper.Attribute(Prop, "listvalues")
                Grid.Cells(Row, 3).Text = XmlHelper.Attribute(Prop, "description")
                If Grid.Cells(Row, 3).Text = "" Then
                    Grid.Cells(Row, 3).Text = XmlHelper.Name(Prop)
                End If
                Grid.Cells(Row, 4).Text = Prop.InnerText
            End If
            Row = Row + 1
        Next
    End Sub

    Private InCellChanged As Boolean = False
    Private Sub Grid_CellChanged(ByVal sender As Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles Grid.CellChanged
        ' --------------------------------------------------------------------
        ' User has changed something - see if we need to create editors or
        ' setup other columns.
        ' --------------------------------------------------------------------
        If Not InCellChanged Then
            InCellChanged = True
            If e.Column = 1 Or e.Column = 2 Then
                CreateCellEditorForRow(e.Row)

            ElseIf e.Column = 2 And TypeOf Grid.Cells(e.Row, 4).CellType Is FarPoint.Win.Spread.CellType.ComboBoxCellType Then
                Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = Grid.Cells(e.Row, 4).CellType
                If Not IsNothing(Combo) Then
                    Combo.Items = Grid.Cells(e.Row, 2).Text.Split(",")
                    If Grid.Cells(e.Row, 4).Text = "" And Combo.Items.Length > 0 Then
                        Grid.Cells(e.Row, 4).Text = Combo.Items(0)
                    End If
                End If
            End If
            InCellChanged = False
        End If
    End Sub

    Public Overrides Sub OnSave()
        ' --------------------------------------------------------------
        ' Save all our changes back to Data
        ' --------------------------------------------------------------
        If Not IsNothing(Me.FpSpread1.ActiveSheet.ActiveCell) AndAlso Not IsNothing(Me.FpSpread1.ActiveSheet.ActiveCell.Editor) Then
            Me.FpSpread1.ActiveSheet.ActiveCell.Editor.StopEditing()
        End If
        Data.RemoveAll()
        For Row As Integer = 0 To VBUserInterface.GridUtils.FindFirstBlankCell(Grid, 1) - 1
            Dim DataType As String = Grid.Cells(Row, 1).Text
            If DataType <> "" Then
                Dim Type As String = Grid.Cells(Row, 0).Text
                If Type = "" Then
                    Dim Category As XmlNode = Data.AppendChild(Data.OwnerDocument.CreateElement("category"))
                    XmlHelper.SetAttribute(Category, "description", Grid.Cells(Row, 3).Text)
                Else
                    Dim NewNode As XmlNode = Data.AppendChild(Data.OwnerDocument.CreateElement(Type))
                    XmlHelper.SetAttribute(NewNode, "type", DataType)
                    If Grid.Cells(Row, 2).Text <> "" Then
                        XmlHelper.SetAttribute(NewNode, "listvalues", Grid.Cells(Row, 2).Text)
                    End If
                    XmlHelper.SetAttribute(NewNode, "description", Grid.Cells(Row, 3).Text)
                    NewNode.InnerText = Grid.Cells(Row, 4).Text
                End If
            End If
        Next
    End Sub

    Private Sub FpSpread1_ButtonClicked(ByVal sender As System.Object, ByVal e As FarPoint.Win.Spread.EditorNotifyEventArgs) Handles FpSpread1.ButtonClicked
        ' --------------------------------------------------------------
        ' User has clicked a button in a cell somewhere on our grid.
        ' Pass event to BaseController so that it can act on it.
        ' --------------------------------------------------------------
        Dim Dialog As New OpenFileDialog
        Dialog.AddExtension = True
        If Dialog.ShowDialog = DialogResult.OK Then
            Dim Text As String = ""
            For Each FileName As String In Dialog.FileNames
                Text += FileName + vbCrLf
            Next
            Grid.Cells(e.Row, 1).Value = Text
        End If
    End Sub

    Public Sub CreateCellEditorForRow(ByVal Row As Integer)
        ' --------------------------------------------------------------------
        ' Create and return a cell editor based on the property based in.
        ' --------------------------------------------------------------------
        Dim Type As String = Grid.Cells(Row, 1).Text
        Grid.Cells(Row, 4).CellType = Nothing
        Grid.Rows(Row).BackColor = System.Drawing.Color.White

        If Type = "yesno" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Items = New String() {"yes", "no"}
            Grid.Cells(Row, 4).CellType = Combo

        ElseIf Type = "date" Then
            Dim DateEditor As FarPoint.Win.Spread.CellType.DateTimeCellType = New FarPoint.Win.Spread.CellType.DateTimeCellType
            DateEditor.DateTimeFormat = FarPoint.Win.Spread.CellType.DateTimeFormat.ShortDate
            If Grid.Cells(Row, 4).Text <> "" Then
                DateEditor.DateDefault = Grid.Cells(Row, 4).Text
            End If
            DateEditor.DropDownButton = True
            Grid.Cells(Row, 4).CellType = DateEditor

        ElseIf Type = "list" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Items = Grid.Cells(Row, 2).Text.Split(",")
            Combo.Editable = True
            Grid.Cells(Row, 4).CellType = Combo

        ElseIf Type = "multilist" Then
            Dim Combo As CheckedListBoxCellType = New CheckedListBoxCellType
            Combo.Items = Grid.Cells(Row, 2).Text.Split(",")
            Grid.Cells(Row, 4).CellType = Combo
            Grid.Rows(Row).Height = 80

        ElseIf Type = "filename" Then
            Grid.Columns(5).Visible = True
            Dim Button As FarPoint.Win.Spread.CellType.ButtonCellType = New FarPoint.Win.Spread.CellType.ButtonCellType
            Button.Picture = My.Resources.folder
            Grid.Cells(Row, 5).CellType = Button

        ElseIf Type = "multiedit" Then
            Dim Text As FarPoint.Win.Spread.CellType.TextCellType = New FarPoint.Win.Spread.CellType.TextCellType
            Text.Multiline = True
            Text.MaxLength = 5000
            Grid.Cells(Row, 4).CellType = Text
            Grid.Rows(Row).Height = 80

        ElseIf Type = "category" Then
            Empty.ReadOnly = True
            Grid.Rows(Row).BackColor = System.Drawing.Color.LightSteelBlue
            Grid.Cells(Row, 2).CellType = Empty
            Grid.Cells(Row, 4).CellType = Empty

        ElseIf Type = "modulename" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Dim Paddock As ApsimFile.Component = Controller.ApsimData.Find(NodePath).FindContainingPaddock()
            If Not IsNothing(Paddock) Then
                Combo.Items = Paddock.ChildNames
            End If

        ElseIf Type = "crop" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Editable = True
            Dim Paddock As ApsimFile.Component = Controller.ApsimData.Find(NodePath).FindContainingPaddock()
            If Not IsNothing(Paddock) Then
                Dim Crops As New List(Of String)
                For Each ModuleName As String In Paddock.ChildNames
                    If Controller.Configuration.Info(ModuleName, "IsCrop").ToLower = "yes" Then
                        Crops.Add(ModuleName)
                    End If
                Next
                Dim CropNames(Crops.Count - 1) As String
                Crops.CopyTo(CropNames)
                Combo.Items = CropNames
            End If

        ElseIf Type = "cultivars" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Editable = True

            ' Try and locate a row with crop as the name.
            Dim CropRow As Integer
            For CropRow = 0 To VBUserInterface.GridUtils.FindFirstBlankCell(Grid, 1) - 1
                If Grid.Cells(CropRow, 0).Text.ToLower = "crop" Then
                    Exit For
                End If
            Next

            ' If we found a crop row then go and get all cultivars for that crop.
            If CropRow < VBUserInterface.GridUtils.FindFirstBlankCell(Grid, 1) - 1 Then
                Combo.Items = Controller.Configuration.GetCultivarsForCrop(Grid.Cells(CropRow, 4).Text)
            End If
        End If

        If Type <> "list" And Type <> "multilist" Then
            Grid.Columns(2).CellType = Empty
        End If
    End Sub

    Private Sub OnEditModeClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles PopupMenu.ItemClicked
        EditModeItem.Checked = Not EditModeItem.Checked
        Grid.Columns(0).Visible = EditModeItem.Checked
        Grid.Columns(1).Visible = EditModeItem.Checked
        Grid.Columns(2).Visible = EditModeItem.Checked
        Grid.Columns(3).Locked = Not EditModeItem.Checked
        If EditModeItem.Checked Then
            Grid.RowCount = 500
        Else
            Grid.RowCount = VBUserInterface.GridUtils.FindFirstBlankCell(Grid, 1)
        End If
    End Sub
End Class
