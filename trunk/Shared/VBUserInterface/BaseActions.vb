Imports VBGeneral
Public Class BaseActions
    Public Shared Sub FileOpen(ByVal Controller As BaseController)
        If Controller.FileSaveAfterPrompt() Then
            Dim dialog As New OpenFileDialog
            dialog.Filter = Controller.OpenDialogFilter
            If dialog.ShowDialog = DialogResult.OK Then
                Controller.FileOpen(dialog.FileName)
            End If
        End If
    End Sub
    Public Shared Sub FileSave(ByVal Controller As BaseController)
        Controller.ApsimData.Save(Controller.FileName)
    End Sub
    Public Shared Sub FileSaveAs(ByVal Controller As BaseController)
        Dim Dialog As New SaveFileDialog
        Dialog.Filter = Controller.OpenDialogFilter
        Dialog.AddExtension = True
        Dialog.OverwritePrompt = True
        If Dialog.ShowDialog = DialogResult.OK Then
            Controller.FileSave(Dialog.FileName)
        End If
    End Sub



    Public Shared Sub AddFolder(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Add a folder
        ' --------------------------------------------------------
        Controller.ApsimData.Add(Controller.SelectedPath, "<folder name=""New folder""/>")
    End Sub
    Public Shared Sub Delete(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Delete selected nodes
        ' --------------------------------------------------------
        For Each SelectedPath As String In Controller.SelectedPaths
            Controller.ApsimData.Delete(SelectedPath)
        Next
    End Sub
    Public Shared Sub Rename(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Rename selected nodes
        ' --------------------------------------------------------
        For Each SelectedPath As String In Controller.SelectedPaths
            Dim Node As APSIMData = Controller.ApsimData.Find(SelectedPath)
            Dim NewName = InputDialog.InputBox("Enter new name for node:", "Rename the selected node", Node.Name, False)
            Controller.ApsimData.Rename(SelectedPath, NewName)
        Next
    End Sub

    Public Shared Sub Cut(ByVal Controller As BaseController, ByVal NodePath As String)
        ' --------------------------------------------------------
        ' Perform a clipboard cut operation
        ' --------------------------------------------------------
        Copy(Controller)
        Delete(Controller)
    End Sub
    Public Shared Sub Copy(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Perform a clipboard copy operation
        ' --------------------------------------------------------
        Dim Contents As String = ""
        For Each SelectedPath As String In Controller.SelectedPaths
            Dim Node As APSIMData = Controller.ApsimData.Find(SelectedPath)
            Contents = Contents + Node.XML + vbCrLf
        Next
        Clipboard.SetDataObject(Contents, True)
    End Sub
    Public Shared Sub Paste(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Perform a clipboard paste operation
        ' --------------------------------------------------------
        Dim iData As IDataObject = Clipboard.GetDataObject()
        Dim xml As String = CType(iData.GetData(DataFormats.Text), String)
        Controller.ApsimData.Add(Controller.SelectedPath, xml)
    End Sub
    Public Shared Sub MoveUp(ByVal Controller As BaseController)
        ' --------------------------------------------------------        
        ' Move all selected items up
        ' --------------------------------------------------------
        Controller.ApsimData.MoveUp(Controller.SelectedPaths)
    End Sub
    Public Shared Sub MoveDown(ByVal Controller As BaseController)
        ' --------------------------------------------------------        
        ' Move all selected items down
        ' --------------------------------------------------------
        Controller.ApsimData.MoveDown(Controller.SelectedPaths)
    End Sub

    Public Shared Sub ExpandAll(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Expand all nodes in tree.
        ' --------------------------------------------------------
        Controller.Explorer.ExpandAll()
    End Sub
    Public Shared Sub CollapseAll(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Collapse all nodes in tree.
        ' --------------------------------------------------------
        Controller.Explorer.CollapseAll()
    End Sub
End Class

