Imports FarPoint.Win.Spread

' ------------------------------------------
' This class contains a few Grid functions
' ------------------------------------------
Public Class GridUtils

    ' ------------------------------------------------------
    ' Populate the specified grid column with double values.
    ' ------------------------------------------------------
    Shared Sub SetColumnAsDoubles(ByVal Grid As SheetView, _
                                  ByVal ColumnIndex As Integer, _
                                  ByVal Values() As Double)
        ' Make sure the grid has enough rows.
        If (Grid.RowCount < Values.Length) Then
            Grid.RowCount = Values.Length
        End If

        ' Add values to column
        Dim RowIndex As Integer = 0
        For Each Value As Double In Values
            If Value <> 999999.0 Then
                Grid.Cells(RowIndex, ColumnIndex).Value = Value
            End If
            RowIndex = RowIndex + 1
        Next
    End Sub


    ' --------------------------------------
    ' Set the cell of a grid as a double.
    ' --------------------------------------
    Shared Sub SetCellAsDouble(ByVal Grid As SheetView, _
                                   ByVal ColumnIndex As Integer, _
                                   ByVal RowIndex As Integer, _
                                   ByVal Value As Double)
        If (Value <> 999999.0) Then
            Grid.Cells(RowIndex, ColumnIndex).Value = Value
        End If
    End Sub


    ' ------------------------------------------------------
    ' Populate the specified grid column with string values.
    ' ------------------------------------------------------
    Shared Sub SetColumnAsStrings(ByVal Grid As SheetView, _
                                  ByVal ColumnIndex As Integer, _
                                  ByVal Values() As String)

        ' Make sure the grid has enough rows.
        If (Grid.RowCount < Values.Length) Then
            Grid.RowCount = Values.Length
        End If

        ' Add values to column
        Dim RowIndex As Integer = 0
        For Each Value As String In Values
            Grid.Cells(RowIndex, ColumnIndex).Value = Value
            RowIndex = RowIndex + 1
        Next
    End Sub


    ' ----------------------------------------------------------
    ' Get a column of double values from the specified grid column
    ' ----------------------------------------------------------
    Shared Function GetColumnAsDoubles(ByVal Grid As SheetView, _
                                       ByVal ColumnIndex As Integer, ByVal NumValues As Integer) As Double()
        Dim NumValuesToReturn = Math.Min(Grid.RowCount, NumValues)
        Dim Values(NumValuesToReturn - 1) As Double

        Dim Index As Integer = 0
        For RowIndex As Integer = 0 To NumValuesToReturn - 1
            If Grid.Cells(RowIndex, ColumnIndex).Text <> "" Then
                Values(Index) = Grid.Cells(RowIndex, ColumnIndex).Value
            Else
                Values(Index) = 999999.0
            End If

            Index = Index + 1
        Next
        Return Values
    End Function


    ' ----------------------------------------------------------
    ' Get a column of string values from the specified grid column
    ' ----------------------------------------------------------
    Shared Function GetColumnAsStrings(ByVal Grid As SheetView, _
                                       ByVal ColumnIndex As Integer, ByVal NumValues As Integer) As String()

        Dim NumValuesToReturn = Math.Min(Grid.RowCount, NumValues)
        Dim Values(NumValuesToReturn - 1) As String

        Dim Index As Integer = 0
        For RowIndex As Integer = 0 To NumValuesToReturn - 1
            Values(Index) = Grid.Cells(RowIndex, ColumnIndex).Value
            Index = Index + 1
        Next
        Return Values
    End Function


    ' --------------------------------------
    ' Get the contents of a cell of a grid.
    ' --------------------------------------
    Shared Function GetCellAsDouble(ByVal Grid As SheetView, _
                                   ByVal ColumnIndex As Integer, _
                                   ByVal RowIndex As Integer) As Double
        If (Not Grid.Cells(RowIndex, ColumnIndex).Value Is Nothing) Then
            Return Grid.Cells(RowIndex, ColumnIndex).Value
        Else
            Return 999999.0
        End If
    End Function


    ' --------------------------------------
    ' Get the contents of a cell of a grid.
    ' --------------------------------------
    Shared Function GetCellAsString(ByVal Grid As SheetView, _
                                   ByVal ColumnIndex As Integer, _
                                   ByVal RowIndex As Integer) As String
        If (Not IsNothing(Grid.Cells(RowIndex, ColumnIndex).Value)) Then
            Return Grid.Cells(RowIndex, ColumnIndex).Value
        Else
            Return ""
        End If
    End Function

    ' ---------------------------------------------------------------------
    ' Get number of non blank values in column of the specified data table
    ' ---------------------------------------------------------------------
    Public Shared Function FindFirstBlankCell(ByVal Grid As SheetView, _
                                                    ByVal ColumnIndex As Integer) As Integer
        For RowIndex As Integer = 0 To Grid.RowCount - 1
            If Grid.Cells(RowIndex, ColumnIndex).Text = "" Then
                Return RowIndex
            End If
        Next
        Return Grid.RowCount
    End Function
    ' ---------------------------------------------------------------------
    ' Get number of non blank values in column of the specified data table
    ' ---------------------------------------------------------------------
    Public Shared Function FindBlankCells(ByVal Grid As SheetView, _
                                                    ByVal ColumnIndex As Integer, ByVal RowCount As Integer) As Integer()
        Dim count As Integer = 0
        Dim RowIndexs(RowCount - 1) As Integer
        For RowIndex As Integer = 0 To RowCount - 1
            If Grid.Cells(RowIndex, ColumnIndex).Text = "" Then
                RowIndexs(count) = RowIndex
                count += 1
            End If
        Next
        ReDim Preserve RowIndexs(count - 1)
        Return RowIndexs
    End Function
    ' -----------------------
    ' How many rows in sheet have data
    ' -----------------------
    Public Shared Function FindRowsInSheet(ByVal Grid As SheetView) As Integer
        Dim i As Integer = 0
        For RowIndex As Integer = Grid.RowCount - 1 To 0 Step -1
            If Grid.Cells(RowIndex, 0).Text <> "" Then
                i += 1
            End If
        Next
        Return i
    End Function
    ' --------------------------------------------------------------
    ' Return the last row in the specified grid that has data in it.
    ' --------------------------------------------------------------
    Public Shared Function FindLastBlankCell(ByVal Grid As SheetView) As Integer
        For RowIndex As Integer = Grid.RowCount - 1 To 0 Step -1
            If Grid.Cells(RowIndex, 0).Text <> "" Then
                Return RowIndex
            End If
        Next
        Return 0
    End Function
End Class
