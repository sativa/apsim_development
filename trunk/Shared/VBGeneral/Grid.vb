Imports Xceed.Grid

' ------------------------------------------
' This class contains a few Grid functions
' ------------------------------------------
Public Class GridUtils

    ' ------------------------------------------------------
    ' Populate the specified grid column with double values.
    ' ------------------------------------------------------
    Shared Sub SetColumnAsDoubles(ByRef Grid As Xceed.Grid.GridControl, _
                                  ByVal ColumnIndex As Integer, ByVal StartRow As Integer, _
                                  ByVal Values() As Double, _
                                  ByVal Format As String)
        Grid.BeginInit()

        ' Make sure the grid has enough rows.
        While Grid.DataRows.Count <> (Values.Length + StartRow)
            Grid.DataRows.AddNew().EndEdit()
        End While

        ' Add values to column
        Dim RowIndex As Integer = StartRow
        For Each Value As Double In Values
            Grid.DataRows.Item(RowIndex).Cells(ColumnIndex).Value = Value.ToString(Format)
            RowIndex = RowIndex + 1
        Next

        Grid.EndInit()
    End Sub


    ' ------------------------------------------------------
    ' Populate the specified grid column with string values.
    ' ------------------------------------------------------
    Shared Sub SetColumnAsStrings(ByRef Grid As Xceed.Grid.GridControl, _
                                  ByVal ColumnIndex As Integer, ByVal StartRow As Integer, _
                                  ByVal Values() As String)
        Grid.BeginInit()

        ' Make sure the grid has enough rows.
        While Grid.DataRows.Count <> (Values.Length + StartRow)
            Grid.DataRows.AddNew().EndEdit()
        End While

        ' Add values to column
        Dim RowIndex As Integer = StartRow
        For Each Value As String In Values
            Grid.DataRows.Item(RowIndex).Cells(ColumnIndex).Value = Value
            RowIndex = RowIndex + 1
        Next
        Grid.EndInit()
    End Sub


    ' ----------------------------------------------------------
    ' Get a column of double values from the specified grid column
    ' ----------------------------------------------------------
    Shared Function GetColumnAsDoubles(ByRef Grid As Xceed.Grid.GridControl, _
                                       ByVal ColumnIndex As Integer, ByVal StartRow As Integer) As Double()

        Dim Values(Grid.DataRows.Count - StartRow) As Double

        Dim Index As Integer = 0
        For RowIndex As Integer = StartRow To Grid.DataRows.Count - 1
            Values(Index) = Grid.DataRows.Item(RowIndex).Cells(ColumnIndex).Value
            Index = Index + 1
        Next
        Return Values
    End Function


    ' ----------------------------------------------------------
    ' Get a column of string values from the specified grid column
    ' ----------------------------------------------------------
    Shared Function GetColumnAsStrings(ByRef Grid As Xceed.Grid.GridControl, _
                                       ByVal ColumnIndex As Integer, ByVal StartRow As Integer) As String()

        Dim Values(Grid.DataRows.Count - StartRow) As String

        Dim Index As Integer = 0
        For RowIndex As Integer = StartRow To Grid.DataRows.Count - 1
            Values(Index) = Grid.DataRows.Item(RowIndex).Cells(ColumnIndex).Value
            Index = Index + 1
        Next
        Return Values
    End Function



End Class
