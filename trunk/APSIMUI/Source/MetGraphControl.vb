Imports VBGeneral

Imports System.Math
Imports CSGeneral
Imports VBMet
Public Class MetGraphControl
    Inherits BaseDataControl

    Protected Metfile As New APSIMInputFile
    Private StartDate As DateTime
    Private EndDate As DateTime
    Private GraphType As String
    Private Const RainfallChart = "Rainfall Chart"
    Private Const MonthlyRainfallChart = "Monthly Rainfall Chart"
    Private Const TemperatureChart = "Temperature Chart"
    Private Const RadiationChart = "Radiation Chart"

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        
        GraphType = RainfallChart
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
    Friend WithEvents Splitter As System.Windows.Forms.Splitter
    Friend WithEvents ChartBox As Xceed.Chart.ChartControl
    Friend WithEvents Panel As System.Windows.Forms.Panel
    Friend WithEvents TrackBar As System.Windows.Forms.TrackBar
    Friend WithEvents YearBox As System.Windows.Forms.TextBox
    Friend WithEvents MonthlyRainfall As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents FrostRisk As Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut
    Friend WithEvents ChartHelper As VBGeneral.ChartHelper
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MetGraphControl))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.PageSetupDialog1 = New System.Windows.Forms.PageSetupDialog
        Me.OutlookBar = New Xceed.SmartUI.Controls.OutlookShortcutBar.SmartOutlookShortcutBar(Me.components)
        Me.RainfallGroup = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("Rainfall")
        Me.DailyRain = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("Rainfall Chart", 2)
        Me.MonthlyRainfall = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("Monthly Rainfall Chart", 2)
        Me.TemperatureGroup = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("Temperature")
        Me.DailyTemperature = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("Temperature Chart", 1)
        Me.FrostRisk = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("Frost Risk", 1)
        Me.RadiationGroup = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("Radiation")
        Me.DailyRadiation = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("DailyRadiation", 3)
        Me.HumidityGroup = New Xceed.SmartUI.Controls.OutlookShortcutBar.Group("Humidity")
        Me.DailyVP = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("DailyVP", 0)
        Me.DailyVPD = New Xceed.SmartUI.Controls.OutlookShortcutBar.Shortcut("DailyVPD", 0)
        Me.Splitter = New System.Windows.Forms.Splitter
        Me.ChartBox = New Xceed.Chart.ChartControl
        Me.Panel = New System.Windows.Forms.Panel
        Me.TrackBar = New System.Windows.Forms.TrackBar
        Me.YearBox = New System.Windows.Forms.TextBox
        Me.ChartHelper = New VBGeneral.ChartHelper
        Me.Panel.SuspendLayout()
        CType(Me.TrackBar, System.ComponentModel.ISupportInitialize).BeginInit()
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
        Me.OutlookBar.Items.AddRange(New Object() {Me.RainfallGroup, Me.TemperatureGroup, Me.RadiationGroup, Me.HumidityGroup})
        Me.OutlookBar.Location = New System.Drawing.Point(0, 23)
        Me.OutlookBar.Name = "OutlookBar"
        Me.OutlookBar.Size = New System.Drawing.Size(120, 513)
        Me.OutlookBar.TabIndex = 1
        Me.OutlookBar.Text = "OutlookBar"
        '
        'RainfallGroup
        '
        Me.RainfallGroup.Expanded = True
        Me.RainfallGroup.Items.AddRange(New Object() {Me.DailyRain, Me.MonthlyRainfall})
        Me.RainfallGroup.ItemsImageList = Me.ImageList
        Me.RainfallGroup.Text = "Rainfall"
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
        Me.TemperatureGroup.Text = "Temperature"
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
        Me.RadiationGroup.Text = "Radiation"
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
        Me.HumidityGroup.Text = "Humidity"
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
        'Splitter
        '
        Me.Splitter.Location = New System.Drawing.Point(120, 23)
        Me.Splitter.Name = "Splitter"
        Me.Splitter.Size = New System.Drawing.Size(5, 513)
        Me.Splitter.TabIndex = 2
        Me.Splitter.TabStop = False
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
        Me.ChartBox.Location = New System.Drawing.Point(125, 23)
        Me.ChartBox.Name = "ChartBox"
        Me.ChartBox.Settings = CType(resources.GetObject("ChartBox.Settings"), Xceed.Chart.Core.Settings)
        Me.ChartBox.Size = New System.Drawing.Size(763, 463)
        Me.ChartBox.TabIndex = 3
        Me.ChartBox.Watermarks = CType(resources.GetObject("ChartBox.Watermarks"), Xceed.Chart.Standard.WatermarkCollection)
        '
        'Panel
        '
        Me.Panel.Controls.Add(Me.TrackBar)
        Me.Panel.Controls.Add(Me.YearBox)
        Me.Panel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Panel.Location = New System.Drawing.Point(125, 486)
        Me.Panel.Name = "Panel"
        Me.Panel.Size = New System.Drawing.Size(763, 50)
        Me.Panel.TabIndex = 5
        '
        'TrackBar
        '
        Me.TrackBar.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.TrackBar.LargeChange = 1
        Me.TrackBar.Location = New System.Drawing.Point(100, 5)
        Me.TrackBar.Name = "TrackBar"
        Me.TrackBar.Size = New System.Drawing.Size(663, 45)
        Me.TrackBar.TabIndex = 8
        Me.TrackBar.TickStyle = System.Windows.Forms.TickStyle.Both
        '
        'YearBox
        '
        Me.YearBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.YearBox.Location = New System.Drawing.Point(0, 0)
        Me.YearBox.Name = "YearBox"
        Me.YearBox.TabIndex = 9
        Me.YearBox.Text = "TextBox1"
        '
        'ChartHelper
        '
        Me.ChartHelper.Chart = Me.ChartBox
        Me.ChartHelper.DataTable = Nothing
        Me.ChartHelper.Grid = Nothing
        '
        'MetGraphControl
        '
        Me.Controls.Add(Me.ChartBox)
        Me.Controls.Add(Me.Panel)
        Me.Controls.Add(Me.Splitter)
        Me.Controls.Add(Me.OutlookBar)
        Me.Name = "MetGraphControl"
        Me.Size = New System.Drawing.Size(888, 536)
        Me.Controls.SetChildIndex(Me.OutlookBar, 0)
        Me.Controls.SetChildIndex(Me.Splitter, 0)
        Me.Controls.SetChildIndex(Me.Panel, 0)
        Me.Controls.SetChildIndex(Me.ChartBox, 0)
        Me.Panel.ResumeLayout(False)
        CType(Me.TrackBar, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overrides Sub fill()
        Try
            Metfile.ReadFromFile(MyData.Child("filename").Value)
            Dim FirstRow As DataRow = Metfile.Data.Rows(0)
            Dim LastRow As DataRow = Metfile.Data.Rows(Metfile.Data.Rows.Count - 1)
            Dim DateColumn As DataColumn = Metfile.Data.Columns("Date")
            StartDate = FirstRow(DateColumn)
            EndDate = LastRow(DateColumn)
            YearBox.Text = StartDate.Year
            With TrackBar
                .Minimum = StartDate.Year
                .Maximum = EndDate.Year
                .SmallChange = 1
                .LargeChange = 1
            End With
        RedrawGraph()

        Catch e As System.Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Unable to build met file interface")
        End Try
    End Sub

    Private Sub RedrawGraph()

        Select Case GraphType
            Case RainfallChart
                DoRainfallChart()
            Case MonthlyRainfallChart
                DoMonthlyRainfallChart()
            Case TemperatureChart
                DoTemperatureChart()
            Case RadiationChart
                DoRadiationChart()
            Case "Frost Risk Chart"
                FrostRiskChart()
        End Select

    End Sub

    Private Sub DoRainfallChart()
        ChartHelper.Clear()
        ChartHelper.DataTable = ReadAnnualDataTable()
        ChartHelper.CreateChartSeriesFromDataTable("Rainfall", "date", "rain", False, Drawing.Color.Blue, Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY)
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
        ChartHelper.CreateChartSeriesFromArray("Rainfall", month, r, False, Drawing.Color.Blue, Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY)
        ChartHelper.CreateChartSeriesFromArray("Evaporation", month, e, False, Drawing.Color.Red, Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY)
        ChartBox.Refresh()

    End Sub
    Private Sub DoTemperatureChart()
        ChartHelper.Clear()
        ChartHelper.DataTable = ReadAnnualDataTable()
        ChartHelper.CreateChartSeriesFromDataTable("Maximum Temperature", "date", "maxt", False, Drawing.Color.Red, Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY)
        ChartHelper.CreateChartSeriesFromDataTable("Minimum Temperature", "date", "mint", False, Drawing.Color.Blue, Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY)
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
        ChartHelper.CreateChartSeriesFromDataTable("Rainfall", "date", "rain", False, Drawing.Color.Blue, Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.SecondaryY)
        ChartHelper.CreateChartSeriesFromDataTable("Radiation", "date", "radn", False, Drawing.Color.Orange, Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY)
        ChartHelper.CreateChartSeriesFromDataTable("Max. Radiation", "date", "Qmax", False, Drawing.Color.Red, Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY)
        ChartBox.Refresh()
    End Sub
    Private Function ReadAnnualData(ByVal ColumnName As String) As Single()
        Dim temp(366) As Single

        Metfile.ReadFromFile(MyData.Child("filename").Value, New Date(TrackBar.Value, 1, 1), New Date(TrackBar.Value, 12, 31))

        Dim data As New DataTable
        data = Metfile.Data
        Dim TempColumn As DataColumn
        TempColumn = data.Columns(ColumnName)

        If Not IsNothing(TempColumn) Then
            For i As Integer = 1 To data.Rows.Count
                temp(i) = data.Rows(i - 1)(TempColumn)
            Next
        End If

        Return temp
    End Function
    Private Function ReadAnnualDataTable() As DataTable
        Dim Data As New DataTable

        Metfile.ReadFromFile(MyData.Child("filename").Value, New Date(TrackBar.Value, 1, 1), New Date(TrackBar.Value, 12, 31))
        Return Metfile.Data

    End Function



    Private Sub DailyRain_Click(ByVal sender As System.Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles DailyRain.Click
        GraphType = RainfallChart
        RedrawGraph()
    End Sub


    Private Sub DailyTemperature_Click(ByVal sender As System.Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles DailyTemperature.Click
        GraphType = TemperatureChart
        RedrawGraph()
    End Sub


    Private Sub YearBox_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles YearBox.TextChanged
        Try
            If (Val(YearBox.Text) >= TrackBar.Minimum And Val(YearBox.Text) <= TrackBar.Maximum) Then
                TrackBar.Value = YearBox.Text
            End If
        Catch Ex As System.Exception

        End Try
    End Sub

    Private Sub TrackBar_ValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TrackBar.ValueChanged
        YearBox.Text = TrackBar.Value
        RedrawGraph()

    End Sub


    Private Sub MonthlyRainfall_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles MonthlyRainfall.Click
        GraphType = MonthlyRainfallChart
        RedrawGraph()

    End Sub

    Private Sub DailyRadiation_Click(ByVal sender As System.Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles DailyRadiation.Click
        GraphType = RadiationChart
        RedrawGraph()
    End Sub
End Class
