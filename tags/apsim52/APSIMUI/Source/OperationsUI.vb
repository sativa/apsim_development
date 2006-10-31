Imports VBGeneral

Public Class OperationsUI


    Overrides Sub RefreshView(ByVal Controller As BaseController)
        MyBase.RefreshView(Controller)

        Dim StartGridRow As Integer = 0
        Dim EndGridRow As Integer = 0
        StartDayGrid.ClearRange(0, 0, StartDayGrid.RowCount, StartDayGrid.ColumnCount, False)
        For Each child As APSIMData In Controller.Data.Children("operation")
            If child.Attribute("condition") = "start_of_day" Then
                StartDayGrid.Cells(StartGridRow, 0).Text = child.ChildValue("date")
                StartDayGrid.Cells(StartGridRow, 1).Text = child.ChildValue("action")
                StartGridRow = StartGridRow + 1
            Else
                EndDayGrid.Cells(EndGridRow, 0).Text = child.ChildValue("date")
                EndDayGrid.Cells(EndGridRow, 1).Text = child.ChildValue("action")
                EndGridRow = EndGridRow + 1
            End If
        Next

        Dim InputMap As FarPoint.Win.Spread.InputMap = Spread.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused)
        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None), FarPoint.Win.Spread.SpreadActions.MoveToNextRow)
    End Sub

    Public Overrides Sub Save()
        Controller.Data.Clear()
        Dim Row As Integer = 0
        While Row < StartDayGrid.RowCount - 1 And StartDayGrid.Cells(Row, 0).Text <> ""
            Dim NewNode As New APSIMData("operation", "")
            NewNode.SetAttribute("condition", "start_of_day")
            NewNode.ChildValue("date") = StartDayGrid.Cells(Row, 0).Text
            NewNode.ChildValue("action") = StartDayGrid.Cells(Row, 1).Text
            Controller.Data.Add(NewNode)
            Row = Row + 1
        End While
        Row = 0
        While Row < EndDayGrid.RowCount - 1 And EndDayGrid.Cells(Row, 0).Text <> ""
            Dim NewNode As New APSIMData("operation", "")
            NewNode.SetAttribute("condition", "end_of_day")
            NewNode.ChildValue("date") = EndDayGrid.Cells(Row, 0).Text
            NewNode.ChildValue("action") = EndDayGrid.Cells(Row, 1).Text
            Controller.Data.Add(NewNode)
            Row = Row + 1
        End While
    End Sub

    Private Sub GridKeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Spread.KeyDown
        ' --------------------------------------------------
        ' If user has hit delete then delete the entire row.
        ' --------------------------------------------------
        If e.KeyCode = Keys.Delete Then
            If Spread.ActiveSheet.SelectionCount > 0 Then
                Dim Range As FarPoint.Win.Spread.Model.CellRange = Spread.ActiveSheet.GetSelection(0)
                If Range.ColumnCount = 5 Then
                    ' delete the entire rows.
                    Spread.ActiveSheet.Rows(Range.Row, Range.Row + Range.RowCount - 1).Remove()
                Else
                    ' just clear the cell contents.
                    Spread.ActiveSheet.Cells(Range.Row, Range.Column, Range.Row + Range.RowCount - 1, Range.Column + Range.ColumnCount - 1).Value = ""
                End If
            End If
        End If
    End Sub

End Class
