Imports Xceed.Grid

' ------------------------------------------
' This class contains a few Grid functions
' ------------------------------------------
Public Class GridUtilities

    ' --------------------------------------------------
    ' Populate the specified grid column with values.
    ' --------------------------------------------------
    Shared Sub PopulateColumn(ByRef Grid As Xceed.Grid.GridControl, ByVal ColumnIndex As Integer, ByVal Values() As Double, _
                                                ByVal Format As String)
        Grid.BeginInit()
        ' Make sure the grid has enough rows.
        While Grid.DataRows.Count < Values.Length
            Grid.DataRows.AddNew().EndEdit()
        End While

        ' Add values to column
        Dim RowIndex As Integer = 0
        For Each Value As Double In Values
            Grid.DataRows.Item(RowIndex).Cells(ColumnIndex).Value = Value.ToString(Format)
            RowIndex = RowIndex + 1
        Next
        Grid.EndInit()
    End Sub


    ' ----------------------------------------------------------
    ' Get a column of values from the specified grid column
    ' ----------------------------------------------------------
    Shared Function DePopulateColumn(ByRef Grid As Xceed.Grid.GridControl, ByVal ColumnIndex As Integer) As Double()

        Dim Values(Grid.DataRows.Count-1) As Double

        Dim Index As Integer = 0
        For RowIndex As Integer = 0 To Grid.DataRows.Count - 1
            Values(Index) = Grid.DataRows.Item(RowIndex).Cells(ColumnIndex).Value
            Index = Index + 1
        Next
        Return Values
    End Function

End Class
