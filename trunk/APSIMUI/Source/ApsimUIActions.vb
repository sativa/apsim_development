Imports VBUserInterface
Imports VBGeneral
Imports CSGeneral
Imports System.IO
Imports System.Collections.Specialized
Imports System.Xml

Public Class ApsimUIActions
    Public Shared Sub FileNew(ByVal Controller As BaseController)
        If Controller.FileSaveAfterPrompt() Then
            Dim NewDocForm As New NewDocumentForm
            If NewDocForm.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
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
    Public Shared Sub Run(ByVal Cntroller As BaseController)
        ' ------------------------------------------------
        ' Go looking for simulations to run. Look at the
        ' currently selected nodes first and progressively
        ' their parents until some simulations are found.
        ' ------------------------------------------------
        Controller = Cntroller
        SimulationsToRun.Clear()
        For Each NodePath As String In Controller.SelectedPaths
            Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(NodePath)
            While Comp.Type <> "simulation" AndAlso Comp.Type <> "folder" AndAlso Comp.Type <> "simulations"
                Comp = Comp.Parent
            End While
            If Comp.Type = "simulation" And Comp.Enabled Then
                SimulationsToRun.Add(Comp.FullPath)
            ElseIf Comp.Type = "folder" Then
                For Each Child As ApsimFile.Component In Comp.ChildNodes
                    If Child.Type = "simulation" And Child.Enabled Then
                        SimulationsToRun.Add(Child.FullPath)
                    End If
                Next
                If SimulationsToRun.Count = 0 Then
                    ' Current selection must be in a folder inside a simulation step up to parent 
                    ' looking for the parent simulation
                    While Not IsNothing(Comp) AndAlso Comp.Type <> "simulation"
                        Comp = Comp.Parent
                    End While
                    If Not IsNothing(Comp) AndAlso Comp.Type = "simulation" AndAlso Comp.Enabled Then
                        SimulationsToRun.Add(Comp.FullPath)
                    End If
                End If
            End If
        Next
        RunSimulations()
    End Sub

    Public Shared Sub StopAll(ByVal Controller As BaseController)
        ' ----------------------------------------------
        ' User wants to stop the currently running APSIM
        ' ----------------------------------------------
        WriteToListBox("All stopped", True)
        CurrentRunningSimulationIndex = SimulationsToRun.Count

        ' kill old apsim processes
        Dim AllProcesses As Process() = Process.GetProcesses()
        For Each proc As Process In AllProcesses
            RemoveHandler proc.Exited, AddressOf OnApsimExited
            If Path.GetFileName(proc.ProcessName) = "apsim" Then
                proc.Kill()
            End If
        Next
        Controller.RefreshToolStrips()
    End Sub
    Public Shared Function SimulationsAreRunning(ByVal Controller As BaseController) As Boolean
        Return CurrentRunningSimulationIndex <> SimulationsToRun.Count
    End Function

    Public Shared Function NoSimulationsAreRunning(ByVal Controller As BaseController) As Boolean
        Return CurrentRunningSimulationIndex = SimulationsToRun.Count
    End Function

    Private Shared Controller As BaseController
    Private Shared MainForm As MainUI
    Private Shared CurrentErrors As New StringCollection
    Private Shared CurrentRunningSimulationIndex As Integer
    Private Shared SimulationsToRun As New StringCollection
    Private Shared CurrentSummaryFile As StreamWriter
    Private Shared CurrentStartDate As Date
    Private Shared CurrentEndDate As Date

    Private Delegate Sub UpdateItemInRunBoxCallBack(ByVal St As String, ByVal IsError As Boolean)
    Private Delegate Sub RefreshToolStripCallback()

    Private Shared Sub RunSimulations()
        ' ---------------------------------------------------------------
        ' Save the simulation and then run one or more simulations in
        ' the background.
        ' ---------------------------------------------------------------
        BaseActions.FileSave(Controller)

        ' kill old apsim processes
        Dim AllProcesses As Process() = Process.GetProcesses()
        For Each proc As Process In AllProcesses
            If Path.GetFileName(proc.ProcessName) = "apsim" Then
                proc.Kill()
            End If
        Next

        ' reset ourselves and the UI bits on MainForm.
        MainForm = Controller.MainForm
        MainForm.CurrentProgressBar.Value = 0
        MainForm.OverallProgressBar.Value = 0
        CurrentErrors.Clear()
        MainForm.RunPanelListBox.Text = ""
        MainForm.RunPanel.Visible = True
        MainForm.RunPanelSplitter.Visible = True
        CurrentRunningSimulationIndex = -1

        RunNextSimulation()
        Controller.RefreshToolStrips()
    End Sub

    Private Shared Sub RunNextSimulation()
        ' --------------------------------------------------
        ' Run the next simulation in the run window.
        ' --------------------------------------------------
        Windows.Forms.Cursor.Current = Cursors.WaitCursor

        CurrentRunningSimulationIndex += 1

        If CurrentRunningSimulationIndex < SimulationsToRun.Count Then
            CurrentErrors.Add("")
            MainForm.CurrentProgressBar.Value = MainForm.CurrentProgressBar.Minimum

            Dim Simulation As ApsimFile.Component = Controller.ApsimData.Find(SimulationsToRun(CurrentRunningSimulationIndex))
            WriteToListBox("Running " + Simulation.Name + ": ", False)

            Dim SimFileName As String = Path.GetDirectoryName(Controller.ApsimData.FileName) + "\" + Simulation.Name + ".sim"
            If File.Exists(SimFileName) Then
                File.Delete(SimFileName)
            End If
            Dim SumFileName As String = SimFileName.Replace(".sim", ".sum")
            If File.Exists(SumFileName) Then
                File.Delete(SumFileName)
            End If


            Try
                Dim Doc As New XmlDocument
                Simulation.WriteSim(Doc, Controller.Configuration)
                Dim Contents As String = XmlHelper.FormattedXML(Doc.DocumentElement.OuterXml)

                Dim Writer As New StreamWriter(SimFileName)
                Writer.Write(Contents)
                Writer.Close()
                CurrentSummaryFile = New StreamWriter(SumFileName)

                Dim ApsimProcess As New ProcessCaller(MainForm)
                ApsimProcess.FileName = Path.GetDirectoryName(Application.ExecutablePath) + "\apsim.exe"
                ApsimProcess.Arguments = """" + SimFileName + """"
                ApsimProcess.WorkingDirectory = Path.GetDirectoryName(Controller.ApsimData.FileName)
                AddHandler ApsimProcess.Completed, AddressOf OnApsimExited
                AddHandler ApsimProcess.Cancelled, AddressOf OnApsimExited
                AddHandler ApsimProcess.StdOutReceived, AddressOf OnStdOut
                AddHandler ApsimProcess.StdErrReceived, AddressOf OnStdError
                ApsimProcess.Start()

            Catch ex As Exception
                WriteToListBox(vbCrLf + ex.Message, True)
                RunNextSimulation()
            End Try

        Else
            ' All simulations are done
            Dim WavFileName As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "ApsimFinishedWAVFileName")
            If File.Exists(WavFileName) Then
                My.Computer.Audio.Play(WavFileName)
            End If
            MainForm.OverallProgressBar.Value = MainForm.OverallProgressBar.Maximum

            MainForm.Invoke(New RefreshToolStripCallback(AddressOf Controller.RefreshToolStrips))

        End If
    End Sub

    Private Shared Sub OnStdOut(ByVal sender As Object, ByVal e As DataReceivedEventArgs)
        ' Write the summary file.
        Dim Line As String = e.Text
        CurrentSummaryFile.WriteLine(Line)
        If (Line.IndexOf("Simulation start date") <> -1) Then
            Dim PosEquals As Integer = Line.IndexOf("=")
            If (PosEquals <> -1) Then
                Dim DateString As String = Line.Substring(PosEquals + 1).Replace(" ", "")
                Date.TryParseExact(DateString, "d/MM/yyyy", Nothing, Globalization.DateTimeStyles.AllowWhiteSpaces, CurrentStartDate)
            End If

        ElseIf (Line.IndexOf("Simulation end date") <> -1) Then
            Dim PosEquals As Integer = Line.IndexOf("=")
            If (PosEquals <> -1) Then
                Dim DateString As String = Line.Substring(PosEquals + 1)
                Date.TryParseExact(DateString, "d/MM/yyyy", Nothing, Globalization.DateTimeStyles.AllowWhiteSpaces, CurrentEndDate)
            End If
        ElseIf (Line.IndexOf("(Day of year=") <> -1) Then
            Dim PosBracket As Integer = Line.IndexOf("(")
            If (PosBracket <> -1) Then
                Dim DateString As String = Line.Substring(0, PosBracket)
                Dim CurrentDate As Date
                If (Date.TryParseExact(DateString, "d MMMM yyyy", Nothing, Globalization.DateTimeStyles.AllowWhiteSpaces, CurrentDate) _
                    And CurrentStartDate < CurrentEndDate) Then
                    Dim DaysIntoSimulation As TimeSpan = (CurrentDate - CurrentStartDate)
                    Dim TotalDaysOfSimulation As TimeSpan = (CurrentEndDate - CurrentStartDate)
                    Dim SliderPosition As Integer = DaysIntoSimulation.Days / TotalDaysOfSimulation.Days * 100
                    MainForm.CurrentProgressBar.Value = SliderPosition

                    MainForm.OverallProgressBar.Value = (SliderPosition / SimulationsToRun.Count) + _
                                               (CurrentRunningSimulationIndex / SimulationsToRun.Count) * 100
                End If

            End If
        End If
    End Sub

    Private Shared Sub OnApsimExited(ByVal Sender As Object, ByVal e As System.EventArgs)
        ' --------------------------------------------------
        ' APSIM has finished running - close the summary file
        ' and update the run window
        ' and go run the next simulation
        ' --------------------------------------------------
        MainForm.CurrentProgressBar.Value = MainForm.CurrentProgressBar.Maximum

        If Not IsNothing(CurrentSummaryFile) Then
            CurrentSummaryFile.Close()
            CurrentSummaryFile = Nothing
        End If

        If CurrentRunningSimulationIndex < SimulationsToRun.Count Then
            CurrentErrors.Add("")
            If (CurrentErrors(CurrentRunningSimulationIndex) = "") Then
                WriteToListBox("Done" + vbCrLf, False)
            End If
            RunNextSimulation()
        Else
            ' The process has been killed by the user.
        End If
    End Sub


    Private Shared Sub OnStdError(ByVal sender As Object, ByVal e As DataReceivedEventArgs)
        Dim Line As String = e.Text
        CurrentSummaryFile.WriteLine(Line)
        WriteToListBox(vbCrLf + Line, True)
    End Sub

    Private Shared Sub WriteToListBox(ByVal Lines As String, ByVal IsError As Boolean)
        MainForm.Invoke(New UpdateItemInRunBoxCallBack(AddressOf UpdateItemInRunBox), New Object() {Lines, IsError})
        Application.DoEvents()
    End Sub

    Private Shared Sub UpdateItemInRunBox(ByVal St As String, ByVal IsError As Boolean)
        ' --------------------------------------------
        ' A simple method to update the run list box.
        ' This method is necessary because a list box
        ' cannot be directly updated from a background
        ' thread.
        ' --------------------------------------------
        If IsError Then
            MainForm.RunPanelListBox.SelectionColor = Color.Red
        Else
            MainForm.RunPanelListBox.SelectionColor = Color.Blue
        End If
        MainForm.RunPanelListBox.AppendText(St)

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
