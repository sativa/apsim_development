Imports VBGeneral

Imports System.Math
Imports System.IO
Imports CSGeneral
Imports VBMet
Imports Xceed.Chart.Core
Imports Xceed.Chart.Standard
Imports Xceed.Chart.GraphicsCore
Imports System.Drawing

Public Class MetGraphControl
    Inherits BaseView

    Protected Metfile As New APSIMInputFile
    Private StartDate As DateTime
    Private EndDate As DateTime
    Private GraphType As String
    Private FileName As String
    Friend WithEvents ToolStripContainer1 As System.Windows.Forms.ToolStripContainer
    Friend WithEvents ToolStrip As System.Windows.Forms.ToolStrip
    Friend WithEvents RawDataButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents RainfallButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents MonthlyRainfallButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents RadiationButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents TemperatureButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator3 As System.Windows.Forms.ToolStripSeparator
    Private CurrentShortCut As Shortcut

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()
        InitializeComponent()
    End Sub

    'UserControl overrides dispose to clean up the component list.
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
    Friend WithEvents ChartHelper As VBGeneral.ChartHelper
    Friend WithEvents ChartBox As Xceed.Chart.ChartControl
    Friend WithEvents YearBox As System.Windows.Forms.NumericUpDown
    Friend WithEvents ChartPanel As System.Windows.Forms.Panel
    Friend WithEvents ContentsBox As System.Windows.Forms.RichTextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MetGraphControl))
        Me.ChartPanel = New System.Windows.Forms.Panel
        Me.YearBox = New System.Windows.Forms.NumericUpDown
        Me.ChartBox = New Xceed.Chart.ChartControl
        Me.ChartHelper = New VBGeneral.ChartHelper
        Me.ContentsBox = New System.Windows.Forms.RichTextBox
        Me.ToolStripContainer1 = New System.Windows.Forms.ToolStripContainer
        Me.ToolStrip = New System.Windows.Forms.ToolStrip
        Me.RawDataButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator
        Me.RainfallButton = New System.Windows.Forms.ToolStripButton
        Me.MonthlyRainfallButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator
        Me.TemperatureButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator
        Me.RadiationButton = New System.Windows.Forms.ToolStripButton
        Me.ChartPanel.SuspendLayout()
        CType(Me.YearBox, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStripContainer1.ContentPanel.SuspendLayout()
        Me.ToolStripContainer1.LeftToolStripPanel.SuspendLayout()
        Me.ToolStripContainer1.SuspendLayout()
        Me.ToolStrip.SuspendLayout()
        Me.SuspendLayout()
        '
        'ChartPanel
        '
        Me.ChartPanel.Controls.Add(Me.ChartBox)
        Me.ChartPanel.Controls.Add(Me.YearBox)
        Me.ChartPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ChartPanel.Location = New System.Drawing.Point(0, 0)
        Me.ChartPanel.Name = "ChartPanel"
        Me.ChartPanel.Size = New System.Drawing.Size(601, 677)
        Me.ChartPanel.TabIndex = 5
        Me.ChartPanel.Visible = False
        '
        'YearBox
        '
        Me.YearBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.YearBox.CausesValidation = False
        Me.YearBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.YearBox.Location = New System.Drawing.Point(16, 18)
        Me.YearBox.Maximum = New Decimal(New Integer() {2000, 0, 0, 0})
        Me.YearBox.Minimum = New Decimal(New Integer() {1900, 0, 0, 0})
        Me.YearBox.Name = "YearBox"
        Me.YearBox.Size = New System.Drawing.Size(64, 21)
        Me.YearBox.TabIndex = 11
        Me.YearBox.Value = New Decimal(New Integer() {1900, 0, 0, 0})
        '
        'ChartBox
        '
        Me.ChartBox.BackColor = System.Drawing.SystemColors.Control
        Me.ChartBox.Background = CType(resources.GetObject("ChartBox.Background"), Xceed.Chart.Standard.Background)
        Me.ChartBox.Charts = CType(resources.GetObject("ChartBox.Charts"), Xceed.Chart.Core.ChartCollection)
        Me.ChartBox.InteractivityOperations = CType(resources.GetObject("ChartBox.InteractivityOperations"), Xceed.Chart.Core.InteractivityOperationsCollection)
        Me.ChartBox.Labels = CType(resources.GetObject("ChartBox.Labels"), Xceed.Chart.Standard.ChartLabelCollection)
        Me.ChartBox.Legends = CType(resources.GetObject("ChartBox.Legends"), Xceed.Chart.Core.LegendCollection)
        Me.ChartBox.Location = New System.Drawing.Point(16, 258)
        Me.ChartBox.Name = "ChartBox"
        Me.ChartBox.Settings = CType(resources.GetObject("ChartBox.Settings"), Xceed.Chart.Core.Settings)
        Me.ChartBox.Size = New System.Drawing.Size(541, 393)
        Me.ChartBox.TabIndex = 12
        Me.ChartBox.Watermarks = CType(resources.GetObject("ChartBox.Watermarks"), Xceed.Chart.Standard.WatermarkCollection)
        '
        'ChartHelper
        '
        Me.ChartHelper.Chart = Me.ChartBox
        Me.ChartHelper.DataTable = Nothing
        '
        'ContentsBox
        '
        Me.ContentsBox.Font = New System.Drawing.Font("Courier New", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ContentsBox.Location = New System.Drawing.Point(0, 0)
        Me.ContentsBox.Name = "ContentsBox"
        Me.ContentsBox.Size = New System.Drawing.Size(557, 240)
        Me.ContentsBox.TabIndex = 8
        Me.ContentsBox.Text = ""
        '
        'ToolStripContainer1
        '
        Me.ToolStripContainer1.BottomToolStripPanelVisible = False
        '
        'ToolStripContainer1.ContentPanel
        '
        Me.ToolStripContainer1.ContentPanel.Controls.Add(Me.ContentsBox)
        Me.ToolStripContainer1.ContentPanel.Controls.Add(Me.ChartPanel)
        Me.ToolStripContainer1.ContentPanel.Size = New System.Drawing.Size(601, 677)
        Me.ToolStripContainer1.Dock = System.Windows.Forms.DockStyle.Fill
        '
        'ToolStripContainer1.LeftToolStripPanel
        '
        Me.ToolStripContainer1.LeftToolStripPanel.Controls.Add(Me.ToolStrip)
        Me.ToolStripContainer1.Location = New System.Drawing.Point(0, 40)
        Me.ToolStripContainer1.Name = "ToolStripContainer1"
        Me.ToolStripContainer1.RightToolStripPanelVisible = False
        Me.ToolStripContainer1.Size = New System.Drawing.Size(734, 677)
        Me.ToolStripContainer1.TabIndex = 9
        Me.ToolStripContainer1.Text = "ToolStripContainer1"
        Me.ToolStripContainer1.TopToolStripPanelVisible = False
        '
        'ToolStrip
        '
        Me.ToolStrip.Dock = System.Windows.Forms.DockStyle.None
        Me.ToolStrip.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.RawDataButton, Me.ToolStripSeparator1, Me.RainfallButton, Me.MonthlyRainfallButton, Me.ToolStripSeparator2, Me.TemperatureButton, Me.ToolStripSeparator3, Me.RadiationButton})
        Me.ToolStrip.LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle.VerticalStackWithOverflow
        Me.ToolStrip.Location = New System.Drawing.Point(0, 3)
        Me.ToolStrip.Name = "ToolStrip"
        Me.ToolStrip.Size = New System.Drawing.Size(133, 264)
        Me.ToolStrip.TabIndex = 0
        Me.ToolStrip.Text = "ToolStrip1"
        '
        'RawDataButton
        '
        Me.RawDataButton.Checked = True
        Me.RawDataButton.CheckOnClick = True
        Me.RawDataButton.CheckState = System.Windows.Forms.CheckState.Checked
        Me.RawDataButton.Image = Global.APSIMUI.My.Resources.Resources.text
        Me.RawDataButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.RawDataButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.RawDataButton.Name = "RawDataButton"
        Me.RawDataButton.Size = New System.Drawing.Size(131, 44)
        Me.RawDataButton.Text = "Raw data"
        Me.RawDataButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(131, 6)
        '
        'RainfallButton
        '
        Me.RainfallButton.CheckOnClick = True
        Me.RainfallButton.Image = Global.APSIMUI.My.Resources.Resources.drink_blue
        Me.RainfallButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.RainfallButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.RainfallButton.Name = "RainfallButton"
        Me.RainfallButton.Size = New System.Drawing.Size(131, 44)
        Me.RainfallButton.Text = "Rainfall chart"
        Me.RainfallButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'MonthlyRainfallButton
        '
        Me.MonthlyRainfallButton.CheckOnClick = True
        Me.MonthlyRainfallButton.Image = Global.APSIMUI.My.Resources.Resources.drink_blue
        Me.MonthlyRainfallButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.MonthlyRainfallButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.MonthlyRainfallButton.Name = "MonthlyRainfallButton"
        Me.MonthlyRainfallButton.Size = New System.Drawing.Size(131, 44)
        Me.MonthlyRainfallButton.Text = "Monthly rainfall chart"
        Me.MonthlyRainfallButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(131, 6)
        '
        'TemperatureButton
        '
        Me.TemperatureButton.CheckOnClick = True
        Me.TemperatureButton.Image = Global.APSIMUI.My.Resources.Resources.thermometer
        Me.TemperatureButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.TemperatureButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.TemperatureButton.Name = "TemperatureButton"
        Me.TemperatureButton.Size = New System.Drawing.Size(131, 44)
        Me.TemperatureButton.Text = "Temperature chart"
        Me.TemperatureButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'ToolStripSeparator3
        '
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        Me.ToolStripSeparator3.Size = New System.Drawing.Size(131, 6)
        '
        'RadiationButton
        '
        Me.RadiationButton.CheckOnClick = True
        Me.RadiationButton.Image = Global.APSIMUI.My.Resources.Resources.sunglasses
        Me.RadiationButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.RadiationButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.RadiationButton.Name = "RadiationButton"
        Me.RadiationButton.Size = New System.Drawing.Size(131, 44)
        Me.RadiationButton.Text = "Radiation chart"
        Me.RadiationButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'MetGraphControl
        '
        Me.Controls.Add(Me.ToolStripContainer1)
        Me.Name = "MetGraphControl"
        Me.Size = New System.Drawing.Size(734, 717)
        Me.Controls.SetChildIndex(Me.ToolStripContainer1, 0)
        Me.ChartPanel.ResumeLayout(False)
        CType(Me.YearBox, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStripContainer1.ContentPanel.ResumeLayout(False)
        Me.ToolStripContainer1.LeftToolStripPanel.ResumeLayout(False)
        Me.ToolStripContainer1.LeftToolStripPanel.PerformLayout()
        Me.ToolStripContainer1.ResumeLayout(False)
        Me.ToolStripContainer1.PerformLayout()
        Me.ToolStrip.ResumeLayout(False)
        Me.ToolStrip.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

#End Region
    Public Sub SetFileName(ByVal FileName As String)
        Me.FileName = FileName
        If File.Exists(FileName) Then
            Dim sr As StreamReader = New StreamReader(FileName)
            ContentsBox.Text = sr.ReadToEnd
            sr.Close()
            Dim StartDate As DateTime
            Dim EndDate As DateTime
            Metfile.GetStartEndDate(FileName, StartDate, EndDate)
            YearBox.Minimum = StartDate.Year
            YearBox.Maximum = EndDate.Year
        End If
        Controller.Data.ChildValue("filename") = FileName
        Refresh()
    End Sub
    Public Overrides Sub Refresh()
        If File.Exists(FileName) Then
            ContentsBox.Visible = RawDataButton.Checked
            ChartPanel.Visible = Not ContentsBox.Visible

            If ChartPanel.Visible Then
                ContentsBox.Dock = DockStyle.None
                ChartPanel.Dock = DockStyle.Fill
                Metfile.ReadFromFile(FileName, New Date(YearBox.Value, 1, 1), New Date(YearBox.Value, 12, 31))
                Dim FirstRow As DataRow = Metfile.Data.Rows(0)
                Dim LastRow As DataRow = Metfile.Data.Rows(Metfile.Data.Rows.Count - 1)
                Dim DateColumn As DataColumn = Metfile.Data.Columns("Date")

                If RainfallButton.Checked Then
                    DoRainfallChart()
                ElseIf RadiationButton.Checked Then
                    DoRadiationChart()
                ElseIf TemperatureButton.Checked Then
                    DoTemperatureChart()
                ElseIf MonthlyRainfallButton.Checked Then
                    DoMonthlyRainfallChart()
                End If
            Else
                ChartPanel.Dock = DockStyle.None
                ContentsBox.Dock = DockStyle.Fill
            End If
        End If
    End Sub

    Private Sub RawDataButton_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RawDataButton.CheckedChanged, RainfallButton.CheckedChanged, MonthlyRainfallButton.CheckedChanged, TemperatureButton.CheckedChanged, RadiationButton.CheckedChanged
        Dim ButtonThatWasClicked As ToolStripButton = sender
        If ButtonThatWasClicked.Checked Then
            For Each Item As ToolStripItem In ToolStrip.Items
                If Item.GetType().ToString = "System.Windows.Forms.ToolStripButton" Then
                    Dim Button As ToolStripButton = Item
                    If Not Button Is ButtonThatWasClicked Then
                        Button.Checked = False
                    End If
                End If
            Next
            Refresh()
        End If
    End Sub

    Private Sub YearBox_ValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles YearBox.ValueChanged
        Refresh()
    End Sub

#Region "Chart drawing methods"
    Private Sub DoRainfallChart()
        ChartHelper.Clear()
        ChartHelper.DataTable = ReadAnnualDataTable()
        ChartHelper.CreateChartSeriesFromDataTable("Rainfall", "date", "rain", False, _
            Drawing.Color.Blue, 1, LinePattern.Solid, _
            Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY)
        ChartBox.Refresh()
    End Sub
    Private Sub DoMonthlyRainfallChart()
        Dim r(11) As Double
        Dim e(11) As Double

        Dim DailyData As New DataTable
        DailyData = ReadAnnualDataTable()
        Dim DateColumn As New DataColumn
        DateColumn = DailyData.Columns("date")
        Dim RainColumn As New DataColumn
        RainColumn = DailyData.Columns("rain")
        Dim EvapColumn As New DataColumn
        EvapColumn = DailyData.Columns("evap")

        For Each row As DataRow In DailyData.Rows
            Dim ThisDay As Date = row(DateColumn)
            If Not IsNothing(RainColumn) Then
                r(ThisDay.Month - 1) = r(ThisDay.Month - 1) + row(RainColumn)
            End If
            If Not IsNothing(EvapColumn) Then
                e(ThisDay.Month - 1) = e(ThisDay.Month - 1) + row(EvapColumn)
            End If
        Next
        Dim month(11) As Double
        For i As Integer = 1 To 12
            month(i - 1) = i
        Next

        ChartHelper.Clear()
        ChartHelper.CreateChartSeriesFromArray("Rainfall", month, r, False, _
                Color.Blue, 1, LinePattern.Solid, _
                StandardAxis.PrimaryX, StandardAxis.PrimaryY)
        ChartHelper.CreateChartSeriesFromArray("Evaporation", month, e, False, _
                Color.Red, 1, LinePattern.Solid, _
                StandardAxis.PrimaryX, StandardAxis.PrimaryY)
        ChartBox.Refresh()

    End Sub
    Private Sub DoTemperatureChart()
        ChartHelper.Clear()
        ChartHelper.DataTable = ReadAnnualDataTable()
        ChartHelper.CreateChartSeriesFromDataTable("Maximum Temperature", "date", "maxt", False, _
                Color.Red, 1, LinePattern.Solid, _
                StandardAxis.PrimaryX, StandardAxis.PrimaryY)
        ChartHelper.CreateChartSeriesFromDataTable("Minimum Temperature", "date", "mint", False, _
                Color.Blue, 1, LinePattern.Solid, _
                StandardAxis.PrimaryX, StandardAxis.PrimaryY)
        ChartBox.Refresh()

    End Sub
    Private Sub FrostRiskChart()
        'With Graph
        '    .Clear()
        '    Dim count(366) As Single
        '    Dim num(366) As Single
        '    Dim Risk(366) As Single

        '    Dim Data As DataTable
        '    Metfile.ReadFromFile(MyData.Child("filename").Value)
        '    Data = Metfile.Data

        '    Dim DateColumn As New DataColumn
        '    DateColumn = Data.Columns("date")
        '    Dim MintColumn As New DataColumn
        '    MintColumn = Data.Columns("mint")

        '    For Each row As DataRow In Data.Rows
        '        Today = row(DateColumn)
        '        num(Today.DayOfYear) = num(Today.DayOfYear) + 1
        '        If Not IsNothing(MintColumn) Then
        '            If row(MintColumn) < 0 Then
        '                count(Today.DayOfYear) = count(Today.DayOfYear) + 1
        '            End If
        '        End If
        '    Next
        '    For i As Integer = 1 To 366
        '        If num(i) > 0 Then
        '            Risk(i) = count(i) / num(i) * 100.0
        '        Else
        '            Risk(i) = 0
        '        End If
        '    Next

        '    Dim RiskPlot As New HistogramPlot(New ArrayAdapter(Risk))
        '    RiskPlot.Color = Color.Blue
        '    .Add(RiskPlot)

        '    .Title = "Percent Frost Risk Across All Years"
        '    .PlotBackColor = Color.Beige
        '    .YAxis1.WorldMin = 0
        '    .XAxis1.WorldMin = 0
        '    ' Force a re-draw the control. 
        '    .Refresh()

        'End With
    End Sub
    Private Sub DoRadiationChart()
        'Dim Rain() As Single = ReadAnnualData("rain")
        'Dim Radn() As Single = ReadAnnualData("radn")
        'Dim Vp() As Single = ReadAnnualData("vp")
        'Dim MinT() As Single = ReadAnnualData("mint")


        'ChartBox.Charts(0).Series.Clear()
        'ChartBox.Charts(0).Axis(Xceed.Chart.Core.StandardAxis.SecondaryY).Visible = True
        'Dim RainfallSeries As Xceed.Chart.Core.BarSeries = ChartBox.Charts(0).Series.Add(Xceed.Chart.Core.SeriesType.Bar)
        'Dim RadnSeries As Xceed.Chart.Core.LineSeries = ChartBox.Charts(0).Series.Add(Xceed.Chart.Core.SeriesType.Line)
        'Dim VPSeries As Xceed.Chart.Core.LineSeries = ChartBox.Charts(0).Series.Add(Xceed.Chart.Core.SeriesType.Line)
        'Dim QmaxSeries As Xceed.Chart.Core.LineSeries = ChartBox.Charts(0).Series.Add(Xceed.Chart.Core.SeriesType.Line)
        'Dim QmaxDrySeries As Xceed.Chart.Core.LineSeries = ChartBox.Charts(0).Series.Add(Xceed.Chart.Core.SeriesType.Line)

        'For doy As Integer = 0 To 365
        '    If Vp(doy) = 0 Then
        '        Vp(doy) = MinT(doy) / 10.0
        '    End If
        '    RainfallSeries.Values.Add(Rain(doy))
        '    RadnSeries.Values.Add(Radn(doy))
        '    VPSeries.Values.Add(Vp(doy))
        '    QmaxSeries.Values.Add(QMax(doy + 1, -27.0, VBMet.Taz, VBMet.Alpha, Vp(doy)))
        '    QmaxDrySeries.Values.Add(QMax(doy + 1, -27.0, VBMet.Taz, VBMet.Alpha, 0.0))
        'Next
        Dim D As DataTable = ReadAnnualDataTable()
        If (IsNothing(D.Columns("Qmax"))) Then
            D.Columns.Add("Qmax")
        End If
        Dim doy As Integer = 0
        For Each row As DataRow In D.Rows
            doy = doy + 1
            Dim vpcolumn As DataColumn = D.Columns("vp")
            Dim latitude As Single = Metfile.Constant("latitude").Value
            If Not IsNothing(vpcolumn) Then
                row("Qmax") = QMax(doy + 1, latitude, VBMet.Taz, VBMet.Alpha, row("vp"))
            Else
                row("Qmax") = QMax(doy + 1, latitude, VBMet.Taz, VBMet.Alpha, svp(row("mint")))
            End If
        Next

        ChartHelper.Clear()
        ChartHelper.DataTable = D
        ChartHelper.CreateChartSeriesFromDataTable("Rainfall", "date", "rain", False, _
                Color.Blue, 1, LinePattern.Solid, _
                StandardAxis.PrimaryX, StandardAxis.SecondaryY)
        ChartHelper.CreateChartSeriesFromDataTable("Radiation", "date", "radn", False, _
                Color.Orange, 1, LinePattern.Solid, _
                StandardAxis.PrimaryX, StandardAxis.PrimaryY)
        ChartHelper.CreateChartSeriesFromDataTable("Max. Radiation", "date", "Qmax", False, _
                Color.Red, 1, LinePattern.Solid, _
                StandardAxis.PrimaryX, StandardAxis.PrimaryY)
        ChartBox.Refresh()
    End Sub

#End Region

    Private Function ReadAnnualData(ByVal ColumnName As String) As Single()
        Dim temp(366) As Single

        Metfile.ReadFromFile(FileName, New Date(YearBox.Value, 1, 1), New Date(YearBox.Value, 12, 31))

        Dim MetData As New DataTable
        MetData = Metfile.Data
        Dim TempColumn As DataColumn
        TempColumn = MetData.Columns(ColumnName)

        If Not IsNothing(TempColumn) Then
            For i As Integer = 1 To MetData.Rows.Count
                temp(i) = MetData.Rows(i - 1)(TempColumn)
            Next
        End If

        Return temp
    End Function
    Private Function ReadAnnualDataTable() As DataTable

        Metfile.ReadFromFile(FileName, New Date(YearBox.Value, 1, 1), New Date(YearBox.Value, 12, 31))
        Return Metfile.Data

    End Function

End Class
