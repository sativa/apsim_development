Imports VBUserInterface
Imports VBGeneral
Imports CSGeneral
Imports System.IO
Imports System.Collections.Specialized


Public Class ApsimUIActions
    Public Shared Sub FileNew(ByVal Controller As BaseController)
        If Controller.FileSaveAfterPrompt() Then
            Dim NewDocForm As New NewDocumentForm
            If NewDocForm.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                Dim Dialog As New SaveFileDialog
                Dialog.Filter = Controller.OpenDialogFilter
                Dialog.AddExtension = True
                Dialog.OverwritePrompt = True
                If Dialog.ShowDialog = DialogResult.OK Then
                    Dim newsim As New APSIMData("folder", "Simulations")
                    Dim Data As APSIMData = newsim.Add(NewDocForm.Selection)
                    newsim.SetAttribute("version", Data.Attribute("version"))
                    Data.DeleteAttribute("version")
                    Controller.ApsimData.[New](newsim.XML)
                    Controller.FileSave(Dialog.FileName)
                End If
            End If
            NewDocForm.Close()
        End If
    End Sub

    Public Shared Sub HelpDocumentation(ByVal Controller As BaseController)
        Dim HelpURL As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "docfile")
        Process.Start(HelpURL)
    End Sub
    Public Shared Sub HelpAbout(ByVal Controller As BaseController)
        Dim about As New AboutBox
        about.Show()
    End Sub

#Region "Simulation methods"
    Public Shared Sub Run(ByVal Cntroller As BaseController)
        Controller = Cntroller
        SimulationsToRun.Clear()
        For Each NodePath As String In Controller.SelectedPaths
            Dim Data As APSIMData = Controller.ApsimData.Find(NodePath)
            While Data.Type <> "simulation" AndAlso Data.Type <> "folder" AndAlso Data.Type <> "simulations"
                Data = Data.Parent
            End While
            If Data.Type = "simulation" Then
                SimulationsToRun.Add(Data.FullPath)
            ElseIf Data.Type = "folder" Then
                For Each Simulation As APSIMData In Data.Children("simulation")
                    SimulationsToRun.Add(Simulation.FullPath)
                Next
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
        Controller.ApsimData.Save(Controller.FileName)

        ' kill old apsim processes
        Dim AllProcesses As Process() = Process.GetProcesses()
        For Each proc As Process In AllProcesses
            If Path.GetFileName(proc.ProcessName) = "apsrun" Or Path.GetFileName(proc.ProcessName) = "apsim" Then
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

            Dim Simulation As APSIMData = Controller.ApsimData.Find(SimulationsToRun(CurrentRunningSimulationIndex))
            WriteToListBox("Running " + Simulation.Name + ": ", False)

            Dim SimFileName As String = Path.GetDirectoryName(Controller.FileName) + "\" + Simulation.Name + ".sim"
            If File.Exists(SimFileName) Then
                File.Delete(SimFileName)
            End If

            Dim ApsimToSimInfo As New ProcessStartInfo()
            ApsimToSimInfo.FileName = Path.GetDirectoryName(Application.ExecutablePath) + "\apsimtosim.exe"
            ApsimToSimInfo.Arguments = """" + Controller.FileName + """ """ + Simulation.Name + """"
            ApsimToSimInfo.WorkingDirectory = Path.GetDirectoryName(Controller.FileName)
            ApsimToSimInfo.RedirectStandardOutput = True
            ApsimToSimInfo.UseShellExecute = False
            ApsimToSimInfo.WindowStyle = ProcessWindowStyle.Hidden
            ApsimToSimInfo.CreateNoWindow = True

            Dim ApsimToSimProcess As Process = Process.Start(ApsimToSimInfo)
            ApsimToSimProcess.StartInfo.WindowStyle = ProcessWindowStyle.Hidden

            ApsimToSimProcess.WaitForExit()
            If ApsimToSimProcess.ExitCode <> 0 Then
                Dim Output As String = ApsimToSimProcess.StandardOutput.ReadToEnd()
                WriteToListBox(vbCrLf + Output, True)
                RunNextSimulation()
            Else
                CurrentSummaryFile = New StreamWriter(SimFileName.Replace(".sim", ".sum"))

                Dim ApsimProcess As New ProcessCaller(MainForm)
                ApsimProcess.FileName = Path.GetDirectoryName(Application.ExecutablePath) + "\apsim.exe"
                ApsimProcess.Arguments = """" + SimFileName + """"
                ApsimProcess.WorkingDirectory = Path.GetDirectoryName(Controller.FileName)
                AddHandler ApsimProcess.Completed, AddressOf OnApsimExited
                AddHandler ApsimProcess.Cancelled, AddressOf OnApsimExited
                AddHandler ApsimProcess.StdOutReceived, AddressOf OnStdOut
                AddHandler ApsimProcess.StdErrReceived, AddressOf OnStdError
                ApsimProcess.Start()
            End If

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
            Dim SelectedData As APSIMData = Controller.ApsimData.Find(SelectedNodePath)
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

    Private Shared Sub GetOutputFiles(ByVal Controller As BaseController, ByVal Data As APSIMData, ByVal OutputFiles As StringCollection)
        ' ------------------------------------------------------------
        'return an array of output filenames under the specified data.
        ' ------------------------------------------------------------
        For Each Child As APSIMData In Data.Children
            ' If child node is an "area", "simulation" or "simulations" then node is not a leaf
            ' and a recursive call is made
            If Child.Type.ToLower() = "area" Or Child.Type.ToLower() = "simulation" _
               Or Child.Type.ToLower() = "simulations" Or Child.Type.ToLower() = "folder" Then
                GetOutputFiles(Controller, Child, OutputFiles)

            ElseIf Child.Type.ToLower() = "outputfile" Then
                Dim FullFileName As String = CalcFileName(Child)
                If Controller.FileName <> "" Then
                    FullFileName = Path.Combine(Path.GetDirectoryName(Controller.FileName), FullFileName)
                End If
                If File.Exists(FullFileName) Then
                    OutputFiles.Add(FullFileName)
                End If
            End If
        Next
    End Sub

    Public Shared Function CalcFileName(ByVal Data As APSIMData) As String
        ' -----------------------------------
        ' Get an autogenerated output/summary
        ' file name for specified node.
        ' NB The returned filename won't have
        ' a path.
        ' -----------------------------------
        Dim SimulationName As String = Nothing
        Dim PaddockName As String = Nothing
        Dim D As APSIMData = Data
        While Not IsNothing(D.Parent)
            D = D.Parent
            If D.Type.ToLower() = "area" Then
                PaddockName = D.Name
            ElseIf D.Type.ToLower() = "simulation" Then
                SimulationName = D.Name
            End If
        End While

        Dim FileName As String = SimulationName
        If Not IsNothing(PaddockName) AndAlso PaddockName.ToLower() <> "paddock" Then
            FileName = FileName + " " + PaddockName
        End If
        If Data.Name.ToLower() <> "outputfile" And Data.Name.ToLower() <> "summaryfile" Then
            FileName = FileName + " " + Data.Name
        End If
        If Data.Type = "summaryfile" Then
            FileName = FileName + ".sum"
        Else
            FileName = FileName + ".out"
        End If

        Return FileName
    End Function

#End Region
End Class
