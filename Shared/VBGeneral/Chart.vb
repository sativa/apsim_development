
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
    Private _DataTable As DataTable

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
    ' ---------------
    ' DataTable Property
    ' ---------------
    Public Property DataTable() As DataTable
        Get
            Return _DataTable
        End Get
        Set(ByVal Value As DataTable)
            _DataTable = Value
            Clear()
        End Set
    End Property


    ' ------------------------------------
    ' Setup chart properties
    ' ------------------------------------
    Public Sub Clear()
        If Not IsNothing(_Chart) Then
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
    Public Function CreateChartSeries(ByVal SeriesName As String, _
                                                ByVal Markers As Boolean, ByVal SeriesColour As Color, _
                                                ByVal LinkedXAxis As StandardAxis, ByVal LinkedYAxis As StandardAxis) As LineSeries
        If Not IsNothing(_Chart) Then
            'If (System.Type.GetTypeCode(X(0)) = TypeCode.Double) Then

            'Else

            'End If


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

            Return NewSeries
        End If
    End Function

    Private Sub PopulateChartSeries(ByVal Series As LineSeries, ByVal X() As Double, ByVal y() As Double)
        If X.Length <> y.Length Then
            Throw New System.Exception("The number of x and y values doesn't match in routine: CreateChartSeriesFromGrid")
        End If
        ' Feed x and y values to new series.
        For Index As Integer = 0 To X.Length - 1
            Series.AddXY(y(Index), X(Index))
        Next
        Series.UseXValues = True
        _Chart.Charts(0).Axis(Xceed.Chart.Core.StandardAxis.PrimaryX).ValueFormat.Format = Xceed.Chart.Utilities.ValueFormat.Default
    End Sub
    Private Sub PopulateChartSeries(ByVal Series As LineSeries, ByVal X() As Date, ByVal y() As Double)
        If X.Length <> y.Length Then
            Throw New System.Exception("The number of x and y values doesn't match in routine: CreateChartSeriesFromGrid")
        End If
        ' Feed x and y values to new series.
        For Index As Integer = 0 To X.Length - 1
            Series.AddXY(y(Index), X(Index).ToOADate)
        Next
        Series.UseXValues = True

        _Chart.Charts(0).Axis(Xceed.Chart.Core.StandardAxis.PrimaryX).ValueFormat.Format = Xceed.Chart.Utilities.ValueFormat.Date
        With _Chart.Charts(0).Axis(Xceed.Chart.Core.StandardAxis.PrimaryX).DateTimeScale
            .MajorTickMode = Xceed.Chart.Core.MajorTickModeDateTime.Months
            .AutoMin = True
            .AutoMax = True
        End With

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

            Dim newseries As LineSeries = CreateChartSeries(SeriesName, Markers, SeriesColour, LinkedXAxis, LinkedYAxis)
            PopulateChartSeries(newseries, X, Y)
        End If
    End Sub

    ' ----------------------------------------------
    ' Create a chart series on the specified chart
    ' ----------------------------------------------
    Public Sub CreateChartSeriesFromDataTable(ByVal SeriesName As String, ByVal XColumn As String, ByVal YColumn As String, _
                                                                ByVal Markers As Boolean, ByVal SeriesColour As Color, _
                                                                ByVal LinkedXAxis As StandardAxis, ByVal LinkedYAxis As StandardAxis)

        If Not IsNothing(_Chart) And Not IsNothing(_DataTable) Then

            'Dim t As TypeCode = System.Type.GetTypeCode(_DataTable.Rows(0)(XColumn))
            Dim X(_DataTable.Rows.Count - 1) As Date
            Dim Y(_DataTable.Rows.Count - 1) As Double

            Dim i As Integer = 0
            For Each row As System.Data.DataRow In _DataTable.Rows
                X(i) = row(XColumn)
                Y(i) = row(YColumn)
                i = i + 1
            Next


            If X.Length <> Y.Length Then
                Throw New System.Exception("The number of x and y values doesn't match in routine: CreateChartSeriesFromGrid")
            End If

            Dim NewSeries As LineSeries = CreateChartSeries(SeriesName, Markers, SeriesColour, LinkedXAxis, LinkedYAxis)
            PopulateChartSeries(NewSeries, X, Y)

        End If
    End Sub

    ' ----------------------------------------------
    ' Create a chart series on the specified chart
    ' ----------------------------------------------
    Public Sub CreateChartSeriesFromArray(ByVal SeriesName As String, ByVal X() As Double, ByVal Y() As Double, _
                                                                ByVal Markers As Boolean, ByVal SeriesColour As Color, _
                                                                ByVal LinkedXAxis As StandardAxis, ByVal LinkedYAxis As StandardAxis)


        If X.Length <> Y.Length Then
            Throw New System.Exception("The number of x and y values doesn't match in routine: CreateChartSeriesFromGrid")
        End If

        Dim NewSeries As LineSeries = CreateChartSeries(SeriesName, Markers, SeriesColour, LinkedXAxis, LinkedYAxis)
        PopulateChartSeries(NewSeries, X, Y)


    End Sub

End Class
