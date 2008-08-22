Imports VBGeneral
Imports CSGeneral
Imports System.Xml
Imports System.IO
Imports System.Collections.Specialized

Public Class BaseActions
    Public Shared Sub FileOpen(ByVal Controller As BaseController)
        If Controller.FileSaveAfterPrompt() Then
            Dim dialog As New OpenFileDialog
            dialog.Filter = Controller.Configuration.Setting("DialogFilter")
            dialog.DefaultExt = Controller.Configuration.Setting("DefaultExtension")
            If dialog.ShowDialog = DialogResult.OK Then
                Controller.ApsimData.OpenFile(dialog.FileName)
                Controller.RefreshToolStrips()
            End If
        End If
    End Sub
    Public Shared Sub FileSave(ByVal Controller As BaseController)
        If Controller.ApsimData.FileName = "Untitled" Then
            BaseActions.FileSaveAs(Controller)
        Else
            Controller.ApsimData.Save()
        End If
    End Sub
    Public Shared Sub FileSaveAs(ByVal Controller As BaseController)
        Dim Dialog As New SaveFileDialog
        Dialog.Filter = Controller.Configuration.Setting("DialogFilter")
        Dialog.DefaultExt = Controller.Configuration.Setting("DefaultExtension")
        Dialog.AddExtension = True
        Dialog.OverwritePrompt = True
        If Dialog.ShowDialog = DialogResult.OK Then
            Controller.Explorer.SaveCurrentView()
            Controller.ApsimData.SaveAs(Dialog.FileName)
            Controller.RefreshToolStrips()
        End If
    End Sub
    Public Shared Sub HelpAbout(ByVal Controller As BaseController)
        If Controller.Configuration.Setting("SplashScreen") <> "" Then
            Dim SplashForm As Form = BaseController.CreateClass(Controller.Configuration.Setting("SplashScreen"))
            SplashForm.ShowDialog()
        End If
    End Sub
    Public Shared Sub AddFolder(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Add a folder
        ' --------------------------------------------------------
        Controller.Selection.Add("<folder/>")
    End Sub
    Public Shared Sub Delete(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Delete selected nodes
        ' --------------------------------------------------------
        Dim PathsToDelete As Specialized.StringCollection = Controller.SelectedPaths
        Dim Selection As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPaths(0))
        Controller.SelectedPath = Selection.Parent.FullPath
        For Each SelectedPath As String In PathsToDelete
            Dim CompToDelete As ApsimFile.Component = Controller.ApsimData.Find(SelectedPath)
            CompToDelete.Parent.Delete(CompToDelete)
        Next
    End Sub
    Public Shared Sub Rename(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Rename selected nodes
        ' --------------------------------------------------------
        For Each SelectedPath As String In Controller.SelectedPaths

            'get the new name the user entered
            Dim NewName = InputDialog.InputBox("Enter new name for node:", "Rename the selected node", Controller.Selection.Name, False)

            'set rename the selected node
            Controller.Selection.Name = NewName

        Next
    End Sub

    Public Shared Sub Cut(ByVal Controller As BaseController)
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
        Controller.ApsimData.CopyToClipboard(Controller.SelectedPaths)
    End Sub
    Public Shared Sub Paste(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Perform a clipboard paste operation
        ' --------------------------------------------------------
        Dim iData As IDataObject = Clipboard.GetDataObject()
        Dim xml As String = CType(iData.GetData(DataFormats.Text), String)
        Controller.Selection.Add(xml)
    End Sub
    Public Shared Sub MoveUp(ByVal Controller As BaseController)
        ' --------------------------------------------------------        
        ' Move all selected items up
        ' --------------------------------------------------------
        Dim PathsToMove As New List(Of String)
        For Each SelectedPath As String In Controller.SelectedPaths
            PathsToMove.Add(SelectedPath.Substring(SelectedPath.LastIndexOf(XmlHelper.Delimiter) + 1))
        Next
        Dim Parent As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPaths(0)).Parent
        Parent.MoveUp(PathsToMove)
    End Sub
    Public Shared Sub MoveDown(ByVal Controller As BaseController)
        ' --------------------------------------------------------        
        ' Move all selected items down
        ' --------------------------------------------------------
        Dim PathsToMove As New List(Of String)
        For Each SelectedPath As String In Controller.SelectedPaths
            PathsToMove.Add(SelectedPath.Substring(SelectedPath.LastIndexOf(XmlHelper.Delimiter) + 1))
        Next
        Dim Parent As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPaths(0)).Parent
        Parent.MoveDown(PathsToMove)
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


    Public Shared Sub MakeConcrete(ByVal Controller As BaseController)
        Controller.Selection.MakeConcrete()
    End Sub

    Public Shared Function CalcFileName(ByVal Data As ApsimFile.Component) As String
        ' -----------------------------------
        ' Get an autogenerated output/summary
        ' file name for specified node.
        ' NB The returned filename won't have
        ' a path.
        ' -----------------------------------

        Dim SimulationName As String = Nothing
        Dim PaddockName As String = Nothing
        Dim D As ApsimFile.Component = Data     'temp component variable (set it to the parameter passed in [starting component])

        While Not IsNothing(D.Parent)           'loop back up the component datastructure until you get to the root component.
            D = D.Parent
            If D.Type.ToLower() = "area" Then
                PaddockName = D.Name            'store the paddock name for later
            ElseIf D.Type.ToLower() = "simulation" Then
                SimulationName = D.Name         'store the simulation name for later
            End If
        End While

        Dim FileName As String = SimulationName     'start out with the filename just being the simulation name
        If Not IsNothing(PaddockName) AndAlso PaddockName.ToLower() <> "paddock" Then           'if there was a paddock component and it had been renamed by the user.
            FileName = FileName + " " + PaddockName 'add the paddock name to the simulation name
        End If
        If Data.Name.ToLower() <> "outputfile" And Data.Name.ToLower() <> "summaryfile" Then    'if the starting component (either an outputfile or summary file) has been renamed by the user 
            FileName = FileName + " " + Data.Name   'add the starting component name to the simulation name
        End If
        If Data.Type = "summaryfile" Then           'add the correct file suffix
            FileName = FileName + ".sum"
        Else
            FileName = FileName + ".out"
        End If

        Return FileName
    End Function

    Public Shared Function GetCSVListOfOutputFiles(ByVal Controller As BaseController) As String
        ' ---------------------------------------------------------------
        ' Return to caller a list of all output files that the user has 
        ' selected to export. Returns "" if user hits cancel.
        ' ---------------------------------------------------------------
        Dim OutputFiles As New StringCollection
        For Each SelectedNodePath As String In Controller.SelectedPaths
            Dim SelectedData As ApsimFile.Component = Controller.ApsimData.Find(SelectedNodePath)
            While SelectedData.Type <> "simulation" AndAlso SelectedData.Type <> "folder" AndAlso SelectedData.Type <> "simulations"
                SelectedData = SelectedData.Parent
            End While
            BaseActions.GetOutputFiles(Controller, SelectedData, OutputFiles)
        Next
        Dim ReturnString As String = ""
        For Each St As String In OutputFiles
            If ReturnString <> "" Then
                ReturnString += ","
            End If
            ReturnString += St
        Next
        Return ReturnString
    End Function

    Public Shared Sub GetOutputFiles(ByVal Controller As BaseController, ByVal Data As ApsimFile.Component, ByVal OutputFiles As StringCollection)
        ' ------------------------------------------------------------
        'return an array of output filenames under the specified data.
        ' ------------------------------------------------------------
        For Each Child As ApsimFile.Component In Data.ChildNodes
            ' If child node is an "area", "simulation" or "simulations" then node is not a leaf
            ' and a recursive call is made
            If Child.Type.ToLower() = "area" Or Child.Type.ToLower() = "simulation" _
               Or Child.Type.ToLower() = "simulations" Or Child.Type.ToLower() = "folder" Then
                GetOutputFiles(Controller, Child, OutputFiles)

            ElseIf Child.Type.ToLower() = "outputfile" Then
                Dim FullFileName As String = BaseActions.CalcFileName(Child)
                If Controller.ApsimData.FileName <> "" Then
                    FullFileName = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), FullFileName)
                End If
                If File.Exists(FullFileName) Then
                    OutputFiles.Add(FullFileName)
                End If
            End If
        Next
    End Sub
End Class

