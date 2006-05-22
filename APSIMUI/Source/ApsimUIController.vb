Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports VBGeneral
Imports CSGeneral

Public Class ApsimUIController
    Inherits BaseController
    Private _MainForm As MainUI
    Private _ComponentInfos As New Collection
    Private TypesData As New APSIMData
    Private TypesFileName As String
    Private _LargeImageList As New ImageList
    Private _SmallImageList As New ImageList


    ' ---------------------
    ' Constructor
    ' ---------------------
    Sub New(ByVal DefaultExtension As String, _
            ByVal DialogFilter As String, _
            ByVal FrequentListSection As String)
        MyBase.New(DefaultExtension, DialogFilter, FrequentListSection)

        _LargeImageList.ImageSize = New Size(32, 32)
        _SmallImageList.ImageSize = New Size(16, 16)
        Dim inifile As New APSIMSettings
        TypesFileName = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "typesfile")
        TypesData.LoadFromFile(TypesFileName)
        PopulateImageLists()
    End Sub


    ' ----------------------------------------------
    ' Property that returns a small icon imagelist
    ' ----------------------------------------------
    Overrides ReadOnly Property SmallImageList() As ImageList
        Get
            Return _SmallImageList
        End Get
    End Property


    ' ------------------------------------------
    ' Populate the small icon imagelist from
    ' the .ini file.
    ' ------------------------------------------
    Public Overrides Function SmallImageIndex(ByVal ComponentType As String) As Integer
        Dim TypeInfo As APSIMData = TypesData.Child(ComponentType)
        If Not IsNothing(TypeInfo) Then
            Dim ImageIndexChild As APSIMData = TypeInfo.Child("SmallImageIndex")
            If Not IsNothing(ImageIndexChild) Then
                Return Val(ImageIndexChild.Value)
            End If
        End If
        Return -1
    End Function


    ' -------------------------------------------------
    ' Return true if the specified component is visible
    ' to the user.
    ' -------------------------------------------------
    Public Overrides Function IsComponentVisible(ByVal ComponentType As String) As Boolean
        Dim VisibleChildren As New StringCollection
        If Not IsNothing(TypesData.Child(ComponentType)) Then
            If TypesData.Child(ComponentType).Child("ShowInMainTree").Value = "Yes" Then
                Return True
            End If
        End If
        Return False
    End Function


    ' -------------------------------------------------
    ' Return true if the specified component type can
    ' be added as a child to the specified parent type.
    ' -------------------------------------------------
    Public Overrides Function AllowComponentAdd(ByVal ChildComponentType As String, ByVal ParentComponentType As String) As Boolean
        ' Look in the componentinfo's drop targets.
        For Each Drop As APSIMData In TypesData.Child(ChildComponentType).Child("drops").Children
            If Drop.Name = ParentComponentType Then
                Return True
            End If
        Next
        ' if we get here we haven't found what we're after
        Return False
    End Function


    ' -------------------------------------
    ' Create a User interface form for the
    ' specified type.
    ' -------------------------------------
    Overrides Function CreateUI(ByVal ComponentType As String) As BaseView
        Dim ComponentInfo As APSIMData = TypesData.Child(ComponentType)
        If Not IsNothing(ComponentInfo) Then
            Dim UIType As String = ComponentInfo.Child("UItype").Value()
            Select Case UIType.ToLower
                Case "area"
                    Return New areaui

                Case "met"
                    Return New MetUI

                Case "tracker"
                    Return New TrackerUI

                Case "soil"
                    Dim SoilView As New SoilUI
                    Dim SoilController As New ApsoilController(".soil", "", "", Nothing)
                    SoilController.FileName = Me.FileName
                    SoilController.AllData = Data
                    SoilView.Controller = SoilController
                    Return SoilView

                Case "area"
                    Return New areaui

                Case "toolbox"
                    Return New BaseView
                Case "file"
                    Return New FileUI

                Case "logic"
                    Return New LogicUI

                Case "empty"
                    Return New EmptyUI

                Case "swimsoil"
                    Return New SwimSoilUI

                Case "outputfiledescription"
                    Return New OutputFileDescUI

                Case "vinelogic"
                    Return New VineLogicUI

                Case "rule"
                    Return New RuleUI

                Case "initwater"
                    Return New InitWaterUI

                Case "initnitrogen"
                    Return New InitNitrogenUI

                Case "startup"

                    Return New StartupUI

                Case "memo"
                    Return New APSIMUI.MemoUI

                Case "tracker"
                    Return New APSIMUI.TrackerUI


                Case Else
                    Return New GenericUI

            End Select
        End If
        Return Nothing
    End Function




    ' ----------------------------------------------
    ' Property that returns a large icon imagelist
    ' ----------------------------------------------
    ReadOnly Property LargeImageList() As ImageList
        Get
            Return _LargeImageList
        End Get
    End Property


    ' ------------------------------------------
    ' Populate the large icon imagelist from
    ' the .ini file.
    ' ------------------------------------------
    Public Function LargeImageIndex(ByVal type As String) As Integer
        Dim TypeInfo As APSIMData = TypesData.Child(type)
        If Not IsNothing(TypeInfo) Then
            Dim ImageIndexChild As APSIMData = TypeInfo.Child("LargeImageIndex")
            If Not IsNothing(ImageIndexChild) Then
                Return Val(ImageIndexChild.Value)
            End If
        End If
        Return -1
    End Function


    ' -----------------------------------------------------------------
    ' Return a large icon file name for the specified type.
    ' -----------------------------------------------------------------
    Private Function LargeIconFileName(ByVal GivenType As String) As String
        Dim TypeInfo As APSIMData = TypesData.Child(GivenType)
        If Not IsNothing(TypeInfo) Then
            Dim IconChild As APSIMData = TypeInfo.Child("LargeIcon")
            If Not IsNothing(IconChild) Then
                Return Path.GetDirectoryName(TypesFileName) + "\" + IconChild.Value
            End If
        End If
        Return ""
    End Function


    ' -----------------------------------------------------------------
    ' Return a small icon file name for the specified type.
    ' -----------------------------------------------------------------
    Private Function SmallIconFileName(ByVal GivenType As String) As String
        Dim TypeInfo As APSIMData = TypesData.Child(GivenType)
        If Not IsNothing(TypeInfo) Then
            Dim IconChild As APSIMData = TypeInfo.Child("SmallIcon")
            If Not IsNothing(IconChild) Then
                Return Path.GetDirectoryName(TypesFileName) + "\" + IconChild.Value
            End If
        End If
        Return ""
    End Function


    ' -----------------------------------------
    ' Read component image settings from .ini file.
    ' -----------------------------------------
    Private Sub PopulateImageLists()
        For Each APSimType As APSIMData In TypesData.Children
            ' load large icon
            Dim FileName As String = LargeIconFileName(APSimType.Type)
            If FileName <> "" And File.Exists(FileName) Then
                Dim LargeBitmap As New Bitmap(LargeIconFileName(APSimType.Type))
                _LargeImageList.Images.Add(LargeBitmap)
                If IsNothing(APSimType.Child("LargeImageIndex")) Then
                    APSimType.Add(New APSIMData("LargeImageIndex", "LargeImageIndex"))
                End If
                APSimType.Child("LargeImageIndex").Value = Str(_LargeImageList.Images.Count - 1)
            End If

            ' load small icon
            FileName = SmallIconFileName(APSimType.Type)
            If FileName <> "" And File.Exists(SmallIconFileName(APSimType.Type)) Then
                Dim SmallBitmap As New Bitmap(SmallIconFileName(APSimType.Type))
                _SmallImageList.Images.Add(SmallBitmap)
                If IsNothing(APSimType.Child("SmallImageIndex")) Then
                    APSimType.Add(New APSIMData("SmallImageIndex", "SmallImageIndex"))
                End If
                APSimType.Child("SmallImageIndex").Value = Str(_SmallImageList.Images.Count - 1)
            End If
        Next
    End Sub


    ' -----------------------------------------------------------------
    ' Return type info for all child components for the specified APSIMData
    ' -----------------------------------------------------------------
    Public Function GetOutputFileDescription(ByRef Data As APSIMData, ByVal ComponentType As String, ByVal InstanceName As String) As String
        Dim ReturnString As String = ""


        For Each VariablesInfo As APSIMData In Data.Children("variables")
            Dim ComponentName As String = InstanceName
            If VariablesInfo.Name = "Water variables" Then
                ComponentName = ComponentName + " Water"
            ElseIf VariablesInfo.Name = "Nitrogen variables" Then
                ComponentName = ComponentName + " Nitrogen"
            End If

            If VariablesInfo.Attribute("link") <> "" Then
                Dim ExternalFileData As New APSIMData
                Dim Filename As String = APSIMSettings.ApsimDirectory + "\ApsimUI\" + VariablesInfo.Attribute("link")
                ExternalFileData.LoadFromFile(Filename)
                Return GetOutputFileDescription(ExternalFileData, ComponentType, InstanceName)
            Else
                ReturnString = ReturnString + VariablesInfo.XML.Replace("/>", " module=""" + ComponentName + """/>")
            End If
        Next

        Dim EventsInfo As APSIMData = Data.Child("events")
        If Not IsNothing(EventsInfo) Then
            ReturnString = ReturnString + EventsInfo.XML.Replace("/>", " module=""" + InstanceName + """/>")
        End If

        Return ReturnString
    End Function


    ' -----------------------------------------------------------------
    ' Return type info for all child components for the specified APSIMData
    ' -----------------------------------------------------------------
    Public Function GetOutputFileDescriptions(ByRef Data As APSIMData) As String
        Dim ReturnString As String = "<folder name=""Variables and Events"">"

        For Each Child As APSIMData In Data.Children
            Dim TypeInfo As APSIMData = TypesData.Child(Child.Type)
            If Not IsNothing(TypeInfo) Then
                Dim ComponentXML As String = GetOutputFileDescription(TypeInfo, Child.Type, Child.Name)
                If ComponentXML <> "" Then
                    ReturnString = ReturnString + "<" + Child.Type + " name=""" + Child.Name + """>" + ComponentXML + "</" + Child.Type + ">"
                End If
            End If
        Next
        ReturnString = ReturnString + "</folder>"
        Return ReturnString
    End Function


    ' --------------------------------------------------
    ' Return a list of sibling component names and types
    ' for the specified data component
    ' --------------------------------------------------
    Public Sub GetSiblingComponents(ByVal Component As APSIMData, _
                                    ByRef ComponentNames As StringCollection, ByRef ComponentTypes As StringCollection)
        ComponentNames.Clear()
        ComponentTypes.Clear()
        For Each Sibling As APSIMData In Component.Parent.Children
            ComponentNames.Add(Sibling.Name)
            ComponentTypes.Add(Sibling.Type)
        Next
    End Sub


    ' ---------------------------------------------------------
    ' Check all children of specified node to make sure their
    ' module name is ok.
    ' ---------------------------------------------------------
    Private Sub CheckModulesOfChildren(ByVal ParentData As APSIMData, ByVal ChildType As String, _
                                        ByVal ComponentNames As StringCollection, ByVal ComponentTypes As StringCollection)
        For Each child As APSIMData In ParentData.Children(ChildType)
            Dim ComponentName As String = child.Attribute("module")
            Dim ComponentType As String = child.Attribute("ModuleType")
            Dim ComponentIndex As Integer = ComponentNames.IndexOf(ComponentName)

            ' Try and fix up the component type if possible.
            If ComponentType = "" Then
                If ComponentIndex <> -1 Then
                    child.SetAttribute("ModuleType", ComponentTypes(ComponentIndex))
                Else
                    If ComponentName.IndexOf(" Water") <> -1 Then
                        child.SetAttribute("ModuleType", "soil")
                        child.SetAttribute("VariableType", "Water")
                    ElseIf ComponentName.IndexOf(" Nitrogen") <> -1 Then
                        child.SetAttribute("ModuleType", "soil")
                        child.SetAttribute("VariableType", "Nitrogen")
                    End If
                End If
            End If

            ' Now try and fix up the component name if possible.
            ComponentType = child.Attribute("ModuleType")
            If ComponentType = "" Then
                ParentData.Delete(child.Name)
            ElseIf ComponentIndex = -1 Then
                ComponentIndex = ComponentTypes.IndexOf(ComponentType)
                If ComponentIndex = -1 Then
                    ParentData.Delete(child.Name)
                Else
                    child.SetAttribute("module", ComponentNames(ComponentIndex))
                End If
            End If
        Next
    End Sub

    ' -----------------------------------------------------------------
    ' Return an image filename for the specified type.
    ' -----------------------------------------------------------------
    Public Function ImageFileForType(ByVal GivenType As String) As String
        Return TypesData.Child(GivenType).Child("Image").Value
    End Function


    ' -----------------------------------------------------------------
    ' Return documentation url for the specified type.
    ' -----------------------------------------------------------------
    Public Function DocumentationForType(ByVal GivenType As String) As String
        Return TypesData.Child(GivenType).Child("Documentation").Value
    End Function


    ' -----------------------------------------------------------------
    ' Return description for the specified type.
    ' -----------------------------------------------------------------
    Public Function DescriptionForType(ByVal GivenType As String) As String
        Return TypesData.Child(GivenType).Child("Description").Value
    End Function


    ' ---------------------------------------
    ' Show some help file as specified by url
    ' ---------------------------------------
    Public Sub ShowHelp(ByVal url As String)
        Process.Start(url)
    End Sub


    '' ---------------------------------------
    '' Show the help window
    '' ---------------------------------------
    'Public Sub ShowHelpBrowser(ByVal Visible As Boolean)
    '    '_MainForm.ViewHelpWindow.Checked = Visible
    '    '_MainForm.HelpBrowserPanel.Visible = Visible
    '    '_MainForm.HelpBrowsertoolBar.Visible = Visible
    '    'Dim inifile As New APSIMSettings
    '    '_MainForm.HelpBrowserPanel.Height = Val(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "helpheight"))
    '    '_MainForm.HorizontalSplitter.Enabled = Visible
    'End Sub


    ' --------------------------------
    ' Display the NewDocument form and
    ' return the selected data.
    ' --------------------------------
    Public Function LetUserSelectNewDocument() As APSIMData
        Try
            Dim NewDocForm As New NewDocumentForm
            If NewDocForm.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                Dim newsim As New APSIMData("simulations", "untitled")
                newsim.Add(NewDocForm.Selection)
                Dim Data As APSIMData = NewDocForm.Selection
                NewDocForm.Close()
                Return newsim
            End If
        Catch Err As System.Exception
            MsgBox(Err.Message, MsgBoxStyle.Critical, "Error openinig document template")
        End Try
        Return Nothing
    End Function


    ' --------------------------------------------------------
    ' Recursive routine to check and fix missing module names
    ' from output variables.
    ' --------------------------------------------------------
    Public Sub CheckAllComponents(ByVal Data As APSIMData)
        For Each Child As APSIMData In Data.Children
            If Child.Type.ToLower() = "area" Or Child.Type.ToLower() = "simulation" Then
                CheckAllComponents(Child)  ' recursion
            ElseIf Child.Type.ToLower() = "outputfile" Then
                CheckOutputFile(Child)
            ElseIf Child.Type.ToLower() = "summaryfile" Then
                SetOutputSummaryFileName(Child)
            End If
        Next
    End Sub


    ' ------------------------------------------------------
    ' Check and modify if necessary the specified OutputFile
    ' component
    ' ------------------------------------------------------
    Private Sub CheckOutputFile(ByVal OutputFile As APSIMData)
        SetOutputSummaryFileName(OutputFile)

        Dim OutputFileDescription As APSIMData = Nothing
        For Each Child As APSIMData In OutputFile.Children
            If Child.Type.ToLower() = "outputfiledescription" Then
                OutputFileDescription = Child
            End If
        Next
        If Not IsNothing(OutputFileDescription) Then
            ' Get a list of all valid components.
            Dim ComponentNames As New StringCollection
            Dim ComponentTypes As New StringCollection
            GetSiblingComponents(OutputFile, ComponentNames, ComponentTypes)

            Dim Variables As APSIMData = OutputFileDescription.Child("variables")
            If Not IsNothing(Variables) Then
                CheckModulesOfChildren(Variables, "variable", ComponentNames, ComponentTypes)
            End If

            Dim Events As APSIMData = OutputFileDescription.Child("events")
            If Not IsNothing(Events) Then
                CheckModulesOfChildren(Events, "event", ComponentNames, ComponentTypes)
            End If

        End If
    End Sub


    ' -----------------------------------
    ' Get an autogenerated output/summary
    ' file name for specified node.
    ' NB The returned path will be relative
    ' to the .apsim file directory.
    ' -----------------------------------
    Sub SetOutputSummaryFileName(ByVal Data As APSIMData)
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
        If Data.Type.ToLower() = "outputfile" Or Data.Type.ToLower() = "summaryfile" Then
            If Not IsNothing(PaddockName) Then
                If PaddockName.ToLower() <> "paddock" Then
                    FileName = FileName + " " + PaddockName
                End If
            End If
            If Data.Name.ToLower() <> "outputfile" And Data.Name.ToLower() <> "summaryfile" Then
                FileName = FileName + " " + Data.Name
            End If
            If Data.Type = "summaryfile" Then
                FileName = FileName + ".sum"
            Else
                FileName = FileName + ".out"
            End If
        Else
            FileName = Data.ChildValue("filename")
        End If
        Data.ChildValue("filename") = FileName
    End Sub


    ' --------------------------------
    ' Set the type of a grid column
    ' --------------------------------
    Sub SetCellType(ByVal Grid As FarPoint.Win.Spread.SheetView, _
                            ByVal row As Integer, _
                            ByVal col As Integer, _
                            ByVal Prop As APSIMData)

        If Prop.Attribute("type") = "yesno" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Items = New String() {"yes", "no"}
            Grid.Cells(row, col).CellType = Combo

        ElseIf Prop.Attribute("type") = "ddmmmdate" Then
            Grid.Cells(row, col).Note = "This cell accepts only 'dd/MMM' (10-Jan) or 'dd/mm/yyyy' (10/01/2005) style dates."
            Grid.Cells(row, col).NoteStyle = FarPoint.Win.Spread.NoteStyle.PopupNote

        ElseIf Prop.Attribute("type") = "date" Then
            Dim DateEditor As FarPoint.Win.Spread.CellType.DateTimeCellType = New FarPoint.Win.Spread.CellType.DateTimeCellType
            DateEditor.DateDefault = Prop.Value
            DateEditor.DropDownButton = True
            Grid.Cells(row, col).CellType = DateEditor

        ElseIf Prop.Attribute("type") = "list" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Items = Prop.Attribute("listvalues").Split(",")
            Combo.Editable = True
            Grid.Cells(row, col).CellType = Combo

        ElseIf Prop.Attribute("type") = "modulename" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Items = GetMatchingModuleNames(Prop)
            Combo.Editable = True
            Grid.Cells(row, col).CellType = Combo

        ElseIf Prop.Attribute("type") = "crop" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Items = Me.GetCropNames(Prop)
            Combo.Editable = True
            Grid.Cells(row, col).CellType = Combo

        ElseIf Prop.Attribute("type") = "cultivars" Then
            Dim CultivarCombo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            CultivarCombo.Items = GetMatchingModuleNames(Prop)
            CultivarCombo.Editable = True
            Grid.Cells(row, col).CellType = CultivarCombo

        End If
    End Sub


    ' ------------------------------------------------------------------
    ' Return a list of instance names for the specified module name
    ' ------------------------------------------------------------------
    Shared Function GetMatchingModuleNames(ByVal Prop As APSIMData) As String()
        Dim Values As New StringCollection
        Dim System As APSIMData = Prop.Parent
        While System.Type <> "simulation" And System.Type <> "area" And Not IsNothing(System.Parent)
            System = System.Parent
        End While

        For Each ApsimModule As APSIMData In System.Children
            If Prop.Attribute("moduletype") = "" Or ApsimModule.Type = Prop.Attribute("moduletype") Then
                Values.Add(ApsimModule.Name())
            End If
        Next
        Dim ReturnValues(Values.Count - 1) As String
        Values.CopyTo(ReturnValues, 0)
        Return ReturnValues
    End Function

    ' ------------------------------------------------------------------
    ' Return a list of crop names
    ' ------------------------------------------------------------------
    Function GetCropNames(ByVal Prop As APSIMData) As String()
        Dim Values As New StringCollection
        Dim System As APSIMData = Prop.Parent
        While System.Type <> "simulation" And System.Type <> "area" And Not IsNothing(System.Parent)
            System = System.Parent
        End While

        For Each ApsimModule As APSIMData In System.Children
            If TypesData.ChildValue(ApsimModule.Type + "|IsCrop").ToLower = "yes" Then
                Values.Add(ApsimModule.Name())
            End If
        Next
        Dim ReturnValues(Values.Count - 1) As String
        Values.CopyTo(ReturnValues, 0)
        Return ReturnValues
    End Function

    Public Function GetOutputFilesUnder(ByVal Data As APSIMData) As StringCollection
        ' ------------------------------------------------------------
        'return an array of output filenames under the specified data.
        ' ------------------------------------------------------------
        Dim OutputFiles As New StringCollection

        For Each Child As APSIMData In Data.Children
            ' If child node is an "area", "simulation" or "simulations" then node is not a leaf
            ' and a recursive call is made
            If Child.Type.ToLower() = "area" Or Child.Type.ToLower() = "simulation" Or Child.Type.ToLower() = "simulations" Then
                For Each OutputFile As String In GetOutputFilesUnder(Child)
                    OutputFiles.Add(OutputFile)
                Next

            ElseIf Child.Type.ToLower() = "outputfile" Then
                Dim FullFileName As String = Child.ChildValue("filename")
                If FileName <> "" Then
                    FullFileName = Path.Combine(Path.GetDirectoryName(FileName), FullFileName)
                End If
                OutputFiles.Add(FullFileName)
            End If
        Next

        Return OutputFiles
    End Function
End Class
