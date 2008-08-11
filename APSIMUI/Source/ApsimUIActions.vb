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
        BaseActions.FileSave(Controller)
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
        Dim FileNames As String = BaseActions.GetCSVListOfOutputFiles(Controller)
        If FileNames = "" Then
            MessageBox.Show("No output files found")
        Else
            apsvisFiles(FileNames)
        End If
    End Sub
    Public Shared Sub ApsimOutlook(ByVal Controller As BaseController)
        Dim FileNames As String = BaseActions.GetCSVListOfOutputFiles(Controller)
        If FileNames = "" Then
            MessageBox.Show("No output files found")
        Else
            apsimoutlookFiles(FileNames)
        End If
    End Sub
    Public Shared Sub Excel(ByVal Controller As BaseController)
        Dim FileNames As String = BaseActions.GetCSVListOfOutputFiles(Controller)
        If FileNames = "" Then
            MessageBox.Show("No output files found")
        Else
            excelFiles(FileNames)
        End If
    End Sub
#End Region

End Class
