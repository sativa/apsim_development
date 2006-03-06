Imports VBGeneral

Imports System.Math
Imports System.IO
Imports CSGeneral
Imports VBMet
Imports Xceed.Chart.Core
Imports Xceed.Chart.Standard
Imports Xceed.SmartUI.Controls.OutlookShortcutBar
Imports System.Drawing

Public Class MetGraphControl
    Inherits BaseView

    Protected Metfile As New APSIMInputFile
    Private StartDate As DateTime
    Private EndDate As DateTime
    Private GraphType As String
    Private FileName As String
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
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents PageSetupDialog1 As System.Windows.Forms.PageSetupDialog
    Friend WithEvents RainfallGroup As Xceed.SmartUI.Controls.OutlookShortcutBar.Group
    Friend WithEvents DailyRain As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents OutlookBar As Xceed.SmartUI.Controls.OutlookShortcutBar.SmartOutlookShortcutBar
    Friend WithEvents TemperatureGroup As Xceed.SmartUI.Controls.OutlookShortcutBar.Group
    Friend WithEvents DailyTemperature As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents RadiationGroup As Xceed.SmartUI.Controls.OutlookShortcutBar.Group
    Friend WithEvents HumidityGroup As Xceed.SmartUI.Controls.OutlookShortcutBar.Group
    Friend WithEvents DailyVP As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents DailyVPD As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents DailyRadiation As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents MonthlyRainfall As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents FrostRisk As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents ChartHelper As VBGeneral.ChartHelper
    Friend WithEvents Group1 As Xceed.SmartUI.Controls.OutlookShortcutBar.Group
    Friend WithEvents Contents As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents ChartBox As Xceed.Chart.ChartControl
    Friend WithEvents YearBox As System.Windows.Forms.NumericUpDown
    Friend WithEvents ChartPanel As System.Windows.Forms.Panel
    Friend WithEvents ContentsBox As System.Windows.Forms.RichTextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MetGraphControl))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.PageSetupDialog1 = New System.Windows.Forms.PageSetupDialog
        Me.OutlookBar = New Xceed.SmartUI.Controls.OutlookShortcutBar.SmartOutlookShortcutBar(Me.components)
        Me.Group1 = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("MetData")
        Me.Contents = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("RawData")
        Me.RainfallGroup = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("Rainfall charts")
        Me.DailyRain = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("Rainfall Chart", 2)
        Me.MonthlyRainfall = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("Monthly Rainfall Chart", 2)
        Me.TemperatureGroup = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("Temperature charts")
        Me.DailyTemperature = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("Temperature Chart", 1)
        Me.FrostRisk = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("Frost Risk", 1)
        Me.RadiationGroup = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("Radiation charts")
        Me.DailyRadiation = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("DailyRadiation", 3)
        Me.HumidityGroup = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("Humidity charts")
        Me.DailyVP = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("DailyVP", 0)
        Me.DailyVPD = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("DailyVPD", 0)
        Me.ChartPanel = New System.Windows.Forms.Panel
        Me.YearBox = New System.Windows.Forms.NumericUpDown
        Me.ChartBox = New Xceed.Chart.ChartControl
        Me.ChartHelper = New VBGeneral.ChartHelper
        Me.ContentsBox = New System.Windows.Forms.RichTextBox
        Me.ChartPanel.SuspendLayout()
        CType(Me.YearBox, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(32, 32)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'OutlookBar
        '
        Me.OutlookBar.Items.AddRange(New Object() {Me.Group1, Me.RainfallGroup, Me.TemperatureGroup, Me.RadiationGroup, Me.HumidityGroup})
        Me.OutlookBar.Location = New System.Drawing.Point(0, 40)
        Me.OutlookBar.Name = "OutlookBar"
        Me.OutlookBar.Size = New System.Drawing.Size(128, 456)
        Me.OutlookBar.TabIndex = 1
        Me.OutlookBar.Text = "OutlookBar"
        '
        'Group1
        '
        Me.Group1.Expanded = True
        Me.Group1.Items.AddRange(New Object() {Me.Contents})
        Me.Group1.Text = "MetData"
        Me.Group1.TextPosition = Xceed.SmartUI.Controls.OutlookShortcutBar.TextPosition.Bottom
        '
        'Contents
        '
        Me.Contents.Image = CType(resources.GetObject("Contents.Image"), System.Drawing.Image)
        Me.Contents.Text = "RawData"
        '
        'RainfallGroup
        '
        Me.RainfallGroup.Items.AddRange(New Object() {Me.DailyRain, Me.MonthlyRainfall})
        Me.RainfallGroup.ItemsImageList = Me.ImageList
        Me.RainfallGroup.Text = "Rainfall charts"
        Me.RainfallGroup.TextPosition = Xceed.SmartUI.Controls.OutlookShortcutBar.TextPosition.Bottom
        '
        'DailyRain
        '
        Me.DailyRain.ImageIndex = 2
        Me.DailyRain.Text = "Rainfall Chart"
        '
        'MonthlyRainfall
        '
        Me.MonthlyRainfall.ImageIndex = 2
        Me.MonthlyRainfall.Text = "Monthly Rainfall Chart"
        '
        'TemperatureGroup
        '
        Me.TemperatureGroup.Items.AddRange(New Object() {Me.DailyTemperature, Me.FrostRisk})
        Me.TemperatureGroup.ItemsImageList = Me.ImageList
        Me.TemperatureGroup.Text = "Temperature charts"
        Me.TemperatureGroup.TextPosition = Xceed.SmartUI.Controls.OutlookShortcutBar.TextPosition.Bottom
        '
        'DailyTemperature
        '
        Me.DailyTemperature.ImageIndex = 1
        Me.DailyTemperature.Text = "Temperature Chart"
        '
        'FrostRisk
        '
        Me.FrostRisk.Enabled = False
        Me.FrostRisk.ImageIndex = 1
        Me.FrostRisk.Text = "Frost Risk"
        '
        'RadiationGroup
        '
        Me.RadiationGroup.Items.AddRange(New Object() {Me.DailyRadiation})
        Me.RadiationGroup.ItemsImageList = Me.ImageList
        Me.RadiationGroup.Text = "Radiation charts"
        Me.RadiationGroup.TextPosition = Xceed.SmartUI.Controls.OutlookShortcutBar.TextPosition.Bottom
        '
        'DailyRadiation
        '
        Me.DailyRadiation.ImageIndex = 3
        Me.DailyRadiation.Text = "DailyRadiation"
        '
        'HumidityGroup
        '
        Me.HumidityGroup.Items.AddRange(New Object() {Me.DailyVP, Me.DailyVPD})
        Me.HumidityGroup.ItemsImageList = Me.ImageList
        Me.HumidityGroup.Text = "Humidity charts"
        Me.HumidityGroup.TextPosition = Xceed.SmartUI.Controls.OutlookShortcutBar.TextPosition.Bottom
        '
        'DailyVP
        '
        Me.DailyVP.Enabled = False
        Me.DailyVP.ImageIndex = 0
        Me.DailyVP.Text = "DailyVP"
        '
        'DailyVPD
        '
        Me.DailyVPD.Enabled = False
        Me.DailyVPD.ImageIndex = 0
        Me.DailyVPD.Text = "DailyVPD"
        '
        'ChartPanel
        '
        Me.ChartPanel.Controls.Add(Me.YearBox)
        Me.ChartPanel.Controls.Add(Me.ChartBox)
        Me.ChartPanel.Location = New System.Drawing.Point(160, 304)
        Me.ChartPanel.Name = "ChartPanel"
        Me.ChartPanel.Size = New System.Drawing.Size(464, 192)
        Me.ChartPanel.TabIndex = 5
        Me.ChartPanel.Visible = False
        '
        'YearBox
        '
        Me.YearBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.YearBox.CausesValidation = False
        Me.YearBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.YearBox.Location = New System.Drawing.Point(8, 120)
        Me.YearBox.Maximum = New Decimal(New Integer() {2000, 0, 0, 0})
        Me.YearBox.Minimum = New Decimal(New Integer() {1900, 0, 0, 0})
        Me.YearBox.Name = "YearBox"
        Me.YearBox.Size = New System.Drawing.Size(64, 21)
        Me.YearBox.TabIndex = 11
        Me.YearBox.Value = New Decimal(New Integer() {1900, 0, 0, 0})
        '
        'ChartBox
        '
        Me.ChartBox.AutoScrollMargin = New System.Drawing.Size(0, 0)
        Me.ChartBox.AutoScrollMinSize = New System.Drawing.Size(0, 0)
        Me.ChartBox.BackColor = System.Drawing.SystemColors.ActiveBorder
        Me.ChartBox.Background = CType(resources.GetObject("ChartBox.Background"), Xceed.Chart.Standard.Background)
        Me.ChartBox.Charts = CType(resources.GetObject("ChartBox.Charts"), Xceed.Chart.Core.ChartCollection)
        Me.ChartBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ChartBox.InteractivityOperations = CType(resources.GetObject("ChartBox.InteractivityOperations"), Xceed.Chart.Standard.InteractivityOperationsCollection)
        Me.ChartBox.Labels = CType(resources.GetObject("ChartBox.Labels"), Xceed.Chart.Standard.ChartLabelCollection)
        Me.ChartBox.Legends = CType(resources.GetObject("ChartBox.Legends"), Xceed.Chart.Core.LegendCollection)
        Me.ChartBox.Location = New System.Drawing.Point(0, 0)
        Me.ChartBox.Name = "ChartBox"
        Me.ChartBox.Settings = CType(resources.GetObject("ChartBox.Settings"), Xceed.Chart.Core.Settings)
        Me.ChartBox.Size = New System.Drawing.Size(464, 192)
        Me.ChartBox.TabIndex = 10
        Me.ChartBox.Watermarks = CType(resources.GetObject("ChartBox.Watermarks"), Xceed.Chart.Standard.WatermarkCollection)
        '
        'ChartHelper
        '
        Me.ChartHelper.Chart = Me.ChartBox
        Me.ChartHelper.DataTable = Nothing
        'Me.ChartHelper.Grid = Nothing
        '
        'ContentsBox
        '
        Me.ContentsBox.Font = New System.Drawing.Font("Courier New", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ContentsBox.Location = New System.Drawing.Point(152, 40)
        Me.ContentsBox.Name = "ContentsBox"
        Me.ContentsBox.Size = New System.Drawing.Size(472, 240)
        Me.ContentsBox.TabIndex = 8
        Me.ContentsBox.Text = ""
        Me.ContentsBox.Visible = False
        '
        'MetGraphControl
        '
        Me.Controls.Add(Me.ChartPanel)
        Me.Controls.Add(Me.ContentsBox)
        Me.Controls.Add(Me.OutlookBar)
        Me.Name = "MetGraphControl"
        Me.Size = New System.Drawing.Size(665, 482)
        Me.Controls.SetChildIndex(Me.OutlookBar, 0)
        Me.Controls.SetChildIndex(Me.ContentsBox, 0)
        Me.Controls.SetChildIndex(Me.ChartPanel, 0)
        Me.ChartPanel.ResumeLayout(False)
        CType(Me.YearBox, System.ComponentModel.ISupportInitialize).EndInit()
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
        CurrentShortCut = Contents
        PopulateGraph()
    End Sub
    Public Sub PopulateGraph()
        If File.Exists(FileName) Then
            ContentsBox.Visible = CurrentShortCut Is Contents
            ChartPanel.Visible = Not ContentsBox.Visible

            If ChartPanel.Visible Then
                ContentsBox.Dock = DockStyle.None
                ChartPanel.Dock = DockStyle.Fill
                Metfile.ReadFromFile(FileName, New Date(YearBox.Value, 1, 1), New Date(YearBox.Value, 12, 31))
                Dim FirstRow As DataRow = Metfile.Data.Rows(0)
                Dim LastRow As DataRow = Metfile.Data.Rows(Metfile.Data.Rows.Count - 1)
                Dim DateColumn As DataColumn = Metfile.Data.Columns("Date")

                If CurrentShortCut Is DailyRain Then
                    DoRainfallChart()
                ElseIf CurrentShortCut Is DailyRadiation Then
                    DoRadiationChart()
                ElseIf CurrentShortCut Is DailyTemperature Then
                    DoTemperatureChart()
                ElseIf CurrentShortCut Is MonthlyRainfall Then
                    DoMonthlyRainfallChart()
                End If
            Else
                ChartPanel.Dock = DockStyle.None
                ContentsBox.Dock = DockStyle.Fill
            End If
        End If
    End Sub
    Private Sub ChartButtonClick(ByVal sender As System.Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles DailyRain.Click, DailyRadiation.Click, DailyTemperature.Click, DailyVP.Click, DailyVPD.Click, MonthlyRainfall.Click, Contents.Click
        CurrentShortCut = e.Item
        PopulateGraph()
    End Sub

    Private Sub YearBox_ValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles YearBox.ValueChanged
        PopulateGraph()
    End Sub

#Region "Chart drawing methods"
    Private Sub DoRainfallChart()
        ChartHelper.Clear()
        ChartHelper.DataTable = ReadAnnualDataTable()
        ChartHelper.CreateChartSeriesFromDataTable("Rainfall", "date", "rain", False, _
            Drawing.Color.Blue, 1, Xceed.Chart.Standard.LinePattern.Solid, _
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
