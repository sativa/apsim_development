Imports System.Drawing
Imports Xceed.Grid
Imports Xceed.Chart
Imports Xceed.Chart.Core

' -----------------------------------------------
' This component provides a simpler interface
' to the XCeed.Chart control.
' -----------------------------------------------
Public Class ChartHelper
    Inherits System.ComponentModel.Component
    Private _Chart As ChartControl
    Private _Grid As GridControl


    ' ---------------
    ' ChartProperty
    ' ---------------
    Public Property Chart() As ChartControl
        Get
            Return _Chart
        End Get
        Set(ByVal Value As ChartControl)
            _Chart = Value
            Clear()
        End Set
    End Property


    ' ---------------
    ' GridProperty
    ' ---------------
    Public Property Grid() As GridControl
        Get
            Return _Grid
        End Get
        Set(ByVal Value As GridControl)
            _Grid = Value
            Clear()
        End Set
    End Property


    ' ------------------------------------
    ' Setup chart properties
    ' ------------------------------------
    Public Sub Clear()
        If Not IsNothing(_Chart) And Not IsNothing(_Grid) Then
            ' format chart
            _Chart.Charts(0).Axis(StandardAxis.PrimaryX).ScaleMode = AxisScaleMode.Numeric
            _Chart.Charts(0).Axis(StandardAxis.SecondaryX).ScaleMode = AxisScaleMode.Numeric

            ' clear all chart series.
            _Chart.Charts(0).Series.Clear()
        End If
    End Sub


    ' ----------------------------------------------
    ' Create a chart series on the specified chart
    ' ----------------------------------------------
    Public Sub CreateChartSeries(ByVal SeriesName As String, ByVal X() As Double, ByVal Y() As Double, _
                                                ByVal Markers As Boolean, ByVal SeriesColour As Color, _
                                                ByVal LinkedXAxis As StandardAxis, ByVal LinkedYAxis As StandardAxis)
        If Not IsNothing(_Chart) And Not IsNothing(_Grid) Then
            If X.Length <> Y.Length Then
                Throw New System.Exception("The number of x and y values doesn't match in routine: CreateChartSeriesFromGrid")
            End If

            ' create the series.
            Dim NewSeries As LineSeries = _Chart.Charts(0).Series.Add(SeriesType.Line)
            NewSeries.DataLabels.Mode = DataLabelsMode.None
            NewSeries.LineBorder.Color = SeriesColour
            NewSeries.LineFillEffect.Color = SeriesColour
            NewSeries.Name = SeriesName
            NewSeries.DisplayOnAxis(LinkedXAxis, True)
            NewSeries.DisplayOnAxis(LinkedYAxis, True)
            NewSeries.Markers.Visible = Markers
            NewSeries.Markers.FillEffect.Color = SeriesColour
            NewSeries.Markers.Border.Width = 1
            NewSeries.Markers.Border.Color = Color.Empty

            ' Feed x and y values to new series.
            For Index As Integer = 0 To X.Length - 1
                NewSeries.AddXY(Y(Index), X(Index))
            Next
            NewSeries.UseXValues = True
        End If
    End Sub

    ' ----------------------------------------------
    ' Create a chart series on the specified chart
    ' ----------------------------------------------
    Public Sub CreateChartSeriesFromGrid(ByVal SeriesName As String, ByVal XColumn As Integer, ByVal YColumn As Integer, _
                                                                ByVal Markers As Boolean, ByVal SeriesColour As Color, _
                                                                ByVal LinkedXAxis As StandardAxis, ByVal LinkedYAxis As StandardAxis)

        If Not IsNothing(_Chart) And Not IsNothing(_Grid) Then
            ' Get x and y values from grid.
            Dim X() As Double = GridUtilities.DePopulateColumn(_Grid, XColumn)
            Dim Y() As Double = GridUtilities.DePopulateColumn(_Grid, YColumn)
            If X.Length <> Y.Length Then
                Throw New System.Exception("The number of x and y values doesn't match in routine: CreateChartSeriesFromGrid")
            End If

            CreateChartSeries(SeriesName, X, Y, Markers, SeriesColour, LinkedXAxis, LinkedYAxis)
        End If
    End Sub


End Class
