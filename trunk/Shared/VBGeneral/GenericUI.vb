Imports VBGeneral
Imports System.Collections
Imports System.Collections.Specialized
Imports FarPoint.Win.Spread
Imports System.IO

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
        Me.PictureBox.Size = New System.Drawing.Size(88, 551)
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
        Me.FpSpread1.Size = New System.Drawing.Size(811, 551)
        Me.FpSpread1.TabIndex = 4
        Me.FpSpread1.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Never
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.FpSpread1.TextTipAppearance = TipAppearance1
        Me.FpSpread1.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        '
        'Grid
        '
        Me.Grid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.Grid.ColumnCount = 2
        Me.Grid.AutoUpdateNotes = True
        Me.Grid.ColumnHeader.Cells.Get(0, 0).Value = "Name"
        Me.Grid.ColumnHeader.Cells.Get(0, 1).Value = "Value"
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
        Me.Controls.Add(Me.FpSpread1)
        Me.Controls.Add(Me.PictureBox)
        Me.Name = "GenericUI"
        Me.Size = New System.Drawing.Size(899, 591)
        Me.Controls.SetChildIndex(Me.PictureBox, 0)
        Me.Controls.SetChildIndex(Me.FpSpread1, 0)
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub Refresh()
        MyBase.Refresh()

        InRefresh = True

        If Not IsNothing(Controller.Data) Then
            Dim inifile As New APSIMSettings
            Dim imagefile As String = Controller.ImageFileForType(Controller.Data.Type)
            If imagefile <> "" And File.Exists(imagefile) Then
                PictureBox.BackgroundImage = Drawing.Image.FromFile(imagefile)
            End If
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
                    Controller.SetCellType(Grid, row, 1, Prop)
                End If
                row = row + 1
            Next
        End If

        InRefresh = False

    End Sub

    Private Sub Grid_CellChanged(ByVal sender As Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles Grid.CellChanged
        If Not InRefresh Then
            Dim Prop As APSIMData = Controller.Data.Children()(e.Row)
            Prop.Value = Grid.Cells(e.Row, e.Column).Value
            Dim UIManager As BaseController = Controller
            Dim msg As String = UIManager.CheckParameterBound(Controller.Data.Type, Prop.Name, Prop.Value)
            If msg <> "" Then
                MsgBox(msg, MsgBoxStyle.Exclamation, "User Error")
            End If
        End If
    End Sub

    Private Sub FpSpread1_Validating(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles FpSpread1.Validating
        Me.FpSpread1.ActiveSheet.RaiseCellChanged(Me.Grid.ActiveCell.Row.Index, Me.Grid.ActiveCell.Column.Index)

    End Sub

End Class
