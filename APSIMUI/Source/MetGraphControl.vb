Imports VBGeneral
Imports scpl
Imports System.Math
Imports CSGeneral
Public Class MetGraphControl
    Inherits BaseDataControl

    Protected Metfile As New APSIMInputFile
    Private StartDate As DateTime
    Private EndDate As DateTime

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        FillListBar()
        'fill()
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
    Friend WithEvents Description As System.Windows.Forms.Label
    Friend WithEvents ListBar As vbAccelerator.Components.ListBarControl.ListBar
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents ScrollBar As System.Windows.Forms.HScrollBar
    Friend WithEvents Graph As scpl.Windows.PlotSurface2D
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MetGraphControl))
        Me.Description = New System.Windows.Forms.Label
        Me.ListBar = New vbAccelerator.Components.ListBarControl.ListBar
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.ScrollBar = New System.Windows.Forms.HScrollBar
        Me.Graph = New scpl.Windows.PlotSurface2D
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.SuspendLayout()
        '
        'Description
        '
        Me.Description.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Description.Location = New System.Drawing.Point(0, 456)
        Me.Description.Name = "Description"
        Me.Description.Size = New System.Drawing.Size(888, 80)
        Me.Description.TabIndex = 0
        Me.Description.Text = "Description of the graph type"
        '
        'ListBar
        '
        Me.ListBar.AllowDragGroups = False
        Me.ListBar.AllowDragItems = False
        Me.ListBar.Dock = System.Windows.Forms.DockStyle.Left
        Me.ListBar.DrawStyle = vbAccelerator.Components.ListBarControl.ListBarDrawStyle.ListBarDrawStyleOfficeXP
        Me.ListBar.LargeImageList = Me.ImageList
        Me.ListBar.Location = New System.Drawing.Point(0, 0)
        Me.ListBar.Name = "ListBar"
        Me.ListBar.SelectOnMouseDown = False
        Me.ListBar.Size = New System.Drawing.Size(136, 456)
        Me.ListBar.SmallImageList = Me.ImageList
        Me.ListBar.TabIndex = 3
        Me.ListBar.ToolTip = Nothing
        '
        'Splitter1
        '
        Me.Splitter1.Location = New System.Drawing.Point(136, 0)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(5, 456)
        Me.Splitter1.TabIndex = 4
        Me.Splitter1.TabStop = False
        '
        'ScrollBar
        '
        Me.ScrollBar.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ScrollBar.Location = New System.Drawing.Point(141, 436)
        Me.ScrollBar.Name = "ScrollBar"
        Me.ScrollBar.Size = New System.Drawing.Size(747, 20)
        Me.ScrollBar.TabIndex = 5
        '
        'Graph
        '
        Me.Graph.AllowSelection = False
        Me.Graph.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.Graph.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Graph.HorizontalEdgeLegendPlacement = scpl.Legend.Placement.Inside
        Me.Graph.LegendBorderStyle = scpl.Legend.BorderType.Shadow
        Me.Graph.LegendXOffset = 10.0!
        Me.Graph.LegendYOffset = 1.0!
        Me.Graph.Location = New System.Drawing.Point(141, 0)
        Me.Graph.Name = "Graph"
        Me.Graph.Padding = 10
        Me.Graph.PlotBackColor = System.Drawing.Color.White
        Me.Graph.ShowLegend = False
        Me.Graph.Size = New System.Drawing.Size(747, 436)
        Me.Graph.TabIndex = 6
        Me.Graph.Title = ""
        Me.Graph.TitleFont = New System.Drawing.Font("Arial", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Pixel)
        Me.Graph.VerticalEdgeLegendPlacement = scpl.Legend.Placement.Outside
        Me.Graph.XAxis1 = Nothing
        Me.Graph.XAxis2 = Nothing
        Me.Graph.YAxis1 = Nothing
        Me.Graph.YAxis2 = Nothing
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(32, 32)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'MetGraphControl
        '
        Me.Controls.Add(Me.Graph)
        Me.Controls.Add(Me.ScrollBar)
        Me.Controls.Add(Me.Splitter1)
        Me.Controls.Add(Me.ListBar)
        Me.Controls.Add(Me.Description)
        Me.Name = "MetGraphControl"
        Me.Size = New System.Drawing.Size(888, 536)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overrides Sub fill()
        Metfile.ReadFromFile(MyData.Child("filename").Value)
        Dim FirstRow As DataRow = Metfile.Data.Rows(0)
        Dim LastRow As DataRow = Metfile.Data.Rows(Metfile.Data.Rows.Count - 1)
        Dim DateColumn As DataColumn = Metfile.Data.Columns("Date")
        StartDate = FirstRow(DateColumn)
        EndDate = LastRow(DateColumn)

        With ScrollBar
            .Minimum = StartDate.Year
            .Maximum = EndDate.Year
            .SmallChange = 1
            .LargeChange = 1
        End With



        With Graph
            .Clear()
            'Create a new line plot from array data via the ArrayAdapter class.
            Dim lp As New LinePlot(New ArrayAdapter(makeDaub(256)))
            lp.Color = Color.Green

            ' Add it to the plot Surface
            .Add(lp)
            .Title = "Daubechies Wavelet"
            ' Ok, the above will produce a decent default plot, but we would like to change
            ' some of the Y Axis details. First, we'd like lots of small ticks (10) between 
            ' large tick values. Secondly, we'd like to draw a grid for the Y values. To do 
            ' this, we create a new LinearAxis (we could also use Label, Log etc). Rather than
            ' starting from scratch, we use the constructor that takes an existing axis and
            ' clones it (values in the superclass Axis only are cloned). PlotSurface2D
            ' automatically determines a suitable axis when we add plots to it (merging
            ' current requirements with old requirements), and we use this as our starting
            ' point. Because we didn't specify which Y Axis we are using when we added the 
            ' above line plot (there is one on the left - YAxis1 and one on the right - YAxis2)
            ' PlotSurface2D.Add assumed we were using YAxis1. So, we create a new axis based on
            ' YAxis1, update the details we want, then set the YAxis1 to be our updated one.
            Dim lax As New LinearAxis(.YAxis1)
            lax.NumberSmallTicks = 10
            lax.GridDetail = Axis.GridType.Fine
            .YAxis1 = lax

            ' We would also like to modify the way in which the X Axis is printed. This time,
            ' we'll just modify the relevant PlotSurface2D Axis directly. 
            .XAxis1.GridDetail = Axis.GridType.Coarse
            .XAxis1.WorldMax = 100.0F

            .PlotBackColor = Color.Beige

            ' Force a re-draw the control. 
            .Refresh()

        End With

    End Sub
    Public Function makeDaub(ByVal len As Integer) As Single()

        Dim daub4_h() As Single = {0.4829629F, 0.8365163F, 0.224143863F, -0.129409522F}
        Dim daub4_g() As Single = {-0.129409522F, -0.224143863F, 0.8365163F, -0.4829629F}

        Dim a(len) As Single
        a(8) = 1
        Dim t() As Single

        Dim ns As Integer = 4

        While (ns < len / 2)
            t = a.Clone()

            ns = ns * 2
            Dim i As Integer
            For i = 0 To ns * 2 - 1
                a(i) = 0.0F
            Next

            ' wavelet contribution
            For i = 0 To ns - 1
                Dim j As Integer
                For j = 0 To 3
                    a((2 * i + j) Mod (2 * ns)) = a((2 * i + j) Mod (2 * ns)) + daub4_g(j) * t(i + ns)
                Next j
            Next i

            ' smooth contribution
            For i = 0 To ns - 1
                Dim j As Integer
                For j = 0 To 3
                    a((2 * i + j) Mod (2 * ns)) = a((2 * i + j) Mod (2 * ns)) + daub4_h(j) * t(i)
                Next j
            Next i
        End While
        Return a
    End Function
    Private Sub FillListBar()
        ListBar.Groups.Clear()

        ListBar.Groups.Add("Rainfall")
        ListBar.Groups(0).Items.Add("Rainfall Chart", 0)
        ListBar.Groups(0).Items.Add("Monthly Rainfall Chart", 0)

        ListBar.Groups.Add("Temperature")
        ListBar.Groups(1).Items.Add("Temperature Chart", 0)
        ListBar.Groups(1).Items.Add("Frost Risk Chart", 0)

        ListBar.Groups.Add("Radiation")
        ListBar.Groups(2).Items.Add("Radiation Chart", 0)

    End Sub


    Private Sub ListBar_ItemClicked(ByVal sender As Object, ByVal e As vbAccelerator.Components.ListBarControl.ItemClickedEventArgs) Handles ListBar.ItemClicked
        RedrawGraph()
    End Sub
    Private Sub RedrawGraph()
        Select Case ListBar.SelectedGroup.SelectedItem.Caption
            Case "Rainfall Chart"
                RainfallChart()
            Case "Monthly Rainfall Chart"
                MonthlyRainfallChart()
            Case "Temperature Chart"
                TemperatureChart()
            Case "Radiation Chart"
                RadiationChart()
        End Select

        If ListBar.SelectedGroup.SelectedItem.Caption = "Rainfall Chart" Then

        End If
    End Sub

    Private Sub RainfallChart()
        With Graph
            .Clear()
            'Create a new line plot from array data via the ArrayAdapter class.
            Dim lp As New HistogramPlot(New ArrayAdapter(ReadAnnualData("rain")))
            lp.Color = Color.Blue

            .Add(lp)
            .Title = "Daily Rainfall (Year = " + Str(ScrollBar.Value) + ")"
            .PlotBackColor = Color.Beige
            ' Force a re-draw the control. 
            .Refresh()

        End With
    End Sub
    Private Sub MonthlyRainfallChart()
        With Graph
            .Clear()
            Dim r(12) As Single
            Dim e(12) As Single

            Dim DailyData As New DataTable
            DailyData = ReadAnnualDataTable()
            Dim DateColumn As New DataColumn
            DateColumn = DailyData.Columns("date")
            Dim RainColumn As New DataColumn
            RainColumn = DailyData.Columns("rain")
            Dim EvapColumn As New DataColumn
            EvapColumn = DailyData.Columns("evap")

            For Each row As DataRow In DailyData.Rows
                Today = row(DateColumn)
                If Not IsNothing(RainColumn) Then
                    r(Today.Month) = r(Today.Month) + row(RainColumn)
                End If
                If Not IsNothing(EvapColumn) Then
                    e(Today.Month) = e(Today.Month) + row(EvapColumn)
                End If
            Next

            Dim rain As New HistogramPlot(New ArrayAdapter(r))
            rain.Color = Color.Blue
            .Add(rain)
            Dim evap As New LinePlot(New ArrayAdapter(e))
            evap.Color = Color.Red
            .Add(evap)

            .Title = "Monthly Rainfall (Year = " + Str(ScrollBar.Value) + ")"
            .PlotBackColor = Color.Beige
            .YAxis1.WorldMin = 0
            .XAxis1.WorldMin = 1
            ' Force a re-draw the control. 
            .Refresh()

        End With
    End Sub
    Private Sub TemperatureChart()
        With Graph
            .Clear()
            'Create a new line plot from array data via the ArrayAdapter class.
            Dim maxt As New LinePlot(New ArrayAdapter(ReadAnnualData("maxt")))
            maxt.Color = Color.Red
            .Add(maxt)

            Dim mint As New LinePlot(New ArrayAdapter(ReadAnnualData("mint")))
            mint.Color = Color.Blue
            .Add(mint)

            .Title = "Daily Temperature(Year = " + Str(ScrollBar.Value) + ")"
            .PlotBackColor = Color.Beige
            ' Force a re-draw the control. 
            .Refresh()

        End With
    End Sub
    Private Sub RadiationChart()
        With Graph
            .Clear()
            'Create a new line plot from array data via the ArrayAdapter class.
            Dim Radn As New LinePlot(New ArrayAdapter(ReadAnnualData("radn")))
            Radn.Color = Color.Red
            Radn.Label = "Radn"
            .Add(Radn)

            Dim RadnMax As New LinePlot(New ArrayAdapter(ReadAnnualData("radn")))
            RadnMax.Color = Color.Blue
            RadnMax.Label = "Max Radn"
            .Add(RadnMax)

            Dim Vp As New LinePlot(New ArrayAdapter(ReadAnnualData("radn")))
            Vp.Color = Color.Green
            Vp.Label = "VP"
            .Add(Vp)

            .Title = "Daily Radiation(Year = " + Str(ScrollBar.Value) + ")"
            .PlotBackColor = Color.Beige
            .LegendAttachTo(XAxisPosition.Top, YAxisPosition.Right)
            .ShowLegend = True
            .LegendBorderStyle = Legend.BorderType.Line

            ' Force a re-draw the control. 
            .Refresh()

        End With
    End Sub
    Private Function ReadAnnualData(ByVal ColumnName As String) As Single()
        Dim temp(366) As Single

        Metfile.ReadFromFile(MyData.Child("filename").Value, New Date(ScrollBar.Value, 1, 1), New Date(ScrollBar.Value, 12, 31))

        Dim data As New DataTable
        data = Metfile.Data
        Dim TempColumn As DataColumn
        TempColumn = data.Columns(ColumnName)

        For i As Integer = 1 To data.Rows.Count
            temp(i) = data.Rows(i - 1)(TempColumn)
        Next

        Return temp
    End Function
    Private Function ReadAnnualDataTable() As DataTable
        Dim Data As New DataTable

        Metfile.ReadFromFile(MyData.Child("filename").Value, New Date(ScrollBar.Value, 1, 1), New Date(ScrollBar.Value, 12, 31))
        Return Metfile.Data

    End Function
    Private Function makeRain() As Single()
        Dim Rain(366) As Single
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Rain(Int(Rnd() * 366)) = Rnd() * 100
        Return Rain
    End Function

    Private Sub ScrollBar_ValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ScrollBar.ValueChanged
        RedrawGraph()
    End Sub
End Class
