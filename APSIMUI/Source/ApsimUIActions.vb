Imports VBUserInterface
Imports VBGeneral
Imports CSGeneral
Imports System.IO
Imports System.Collections.Specialized
Imports System.Collections.Generic
Imports System.Xml
Imports ApsimRun

Public Class ApsimUIActions
    Public Shared Sub FileNew(ByVal Controller As BaseController)
        If Controller.FileSaveAfterPrompt() Then
            Dim NewDocForm As New NewDocumentForm
            If NewDocForm.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                Controller.Explorer.CloseUI()
                Dim Doc As New XmlDocument()
                Dim Folder As XmlNode = Doc.AppendChild(Doc.CreateElement("folder"))
                XmlHelper.SetName(Folder, "Simulations")
                Dim Data As XmlNode = Folder.AppendChild(Doc.ImportNode(NewDocForm.Selection, True))
                XmlHelper.SetAttribute(Folder, "version", XmlHelper.Attribute(Data, "version"))
                XmlHelper.DeleteAttribute(Data, "version")
                Controller.ApsimData.[New](Folder.OuterXml)
            End If
            NewDocForm.Close()
        End If
    End Sub

    Public Shared Sub HelpDocumentation(ByVal Controller As BaseController)
        Dim HelpURL As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "docfile")
        Process.Start(HelpURL)
    End Sub


#Region "Simulation methods"
    Public Shared Sub Run(ByVal Controller As BaseController)
        ' ------------------------------------------------
        ' Go looking for simulations to run. Look at the
        ' currently selected nodes first and progressively
        ' their parents until some simulations are found.
        ' ------------------------------------------------
        Dim SimulationsToRun As New List(Of String)

        For Each NodePath As String In Controller.SelectedPaths
            SimulationsToRun.Add(NodePath)
        Next
        Dim F As New ApsimRun.SimulationRunnerForm(Nothing)
        F.Show(Controller.MainForm)
        F.AddFromGUI(Controller.ApsimData, SimulationsToRun)
    End Sub


    Public Shared Sub Enable(ByVal Controller As BaseController)
        For Each NodePath As String In Controller.SelectedPaths
            Controller.ApsimData.Find(NodePath).Enabled = True
        Next
    End Sub
    Public Shared Sub Disable(ByVal Controller As BaseController)
        For Each NodePath As String In Controller.SelectedPaths
            Controller.ApsimData.Find(NodePath).Enabled = False
        Next
    End Sub

#End Region

#Region "Output file methods"
    Private Declare Ansi Sub excelFiles Lib "ApsimContextMenu.dll" _
        Alias "excelFiles" (ByVal outFileList As String)
    Private Declare Ansi Sub apsvisFiles Lib "ApsimContextMenu.dll" _
            Alias "apsvisFiles" (ByVal outFileList As String)
    Private Declare Ansi Sub apsimoutlookFiles Lib "ApsimContextMenu.dll" _
            Alias "apsimoutlookFiles" (ByVal outFileList As String)

    Public Shared Sub Graph(ByVal Controller As BaseController)
        Dim FileNames As String = GetCSVListOfOutputFiles(Controller)
        If FileNames <> "" Then
            apsvisFiles(FileNames)
        End If
    End Sub
    Public Shared Sub ApsimOutlook(ByVal Controller As BaseController)
        Dim FileNames As String = GetCSVListOfOutputFiles(Controller)
        If FileNames <> "" Then
            apsimoutlookFiles(FileNames)
        End If
    End Sub
    Public Shared Sub Excel(ByVal Controller As BaseController)
        Dim FileNames As String = GetCSVListOfOutputFiles(Controller)
        If FileNames <> "" Then
            excelFiles(FileNames)
        End If
    End Sub
    Private Shared Function GetCSVListOfOutputFiles(ByVal Controller As BaseController) As String
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
            GetOutputFiles(Controller, SelectedData, OutputFiles)
        Next
        Dim ReturnString As String = ""
        For Each St As String In OutputFiles
            If ReturnString <> "" Then
                ReturnString += ","
            End If
            ReturnString += St
        Next
        If ReturnString = "" Then
            MessageBox.Show("No output files found")
        End If
        Return ReturnString
    End Function

    Private Shared Sub GetOutputFiles(ByVal Controller As BaseController, ByVal Data As ApsimFile.Component, ByVal OutputFiles As StringCollection)
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
#End Region

End Class
