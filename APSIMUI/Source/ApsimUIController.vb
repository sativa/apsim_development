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
    Private ComponentDescriptionData As APSIMData = Nothing

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

        ' Load types.xml
        TypesFileName = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "typesfile")
        TypesData.LoadFromFile(TypesFileName)

        PopulateImageLists()
    End Sub

    Protected Overrides Function IsDataReadOnly() As Boolean
        Dim FileNameNoPath As String = Path.GetFileName(FileName).ToLower()
        Return MyBase.IsDataReadOnly OrElse FileNameNoPath = "apsru-australia-soils.soils" _
                                     OrElse FileNameNoPath = "standard.xml" _
                                     OrElse FileNameNoPath = "new simulations.xml"
    End Function

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
                    Return New SoilUI

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

                Case "outputfiledesc"
                    Return New OutputFileDescUI

                Case "vinelogic"
                    Return New VineLogicUI

                Case "rule"
                    Return New RuleUI

                Case "initwater"
                    Return New InitWaterUI

                Case "initnitrogen"
                    Return New InitNitrogenUI

                Case "memo"
                    Return New APSIMUI.MemoUI

                Case "tracker"
                    Return New APSIMUI.TrackerUI

                Case "operations"
                    Return New APSIMUI.OperationsUI

                    'Case "tclui"
                    '    Return New APSIMUI.TclUI

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


    Public Sub GetVariablesForComponent(ByVal ComponentType As String, ByVal InstanceName As String, _
                                        ByVal PropertyGroup As String, ByVal ReturnVariables As APSIMData)
        ' -----------------------------------------------------------------
        ' Add variable info for the specified type and instance name to the
        ' "VariableData" argument.
        ' -----------------------------------------------------------------
        Dim TypeInfo As APSIMData = TypesData.Child(ComponentType)
        If Not IsNothing(TypeInfo) Then
            For Each TypeVariables As APSIMData In TypeInfo.Children(PropertyGroup)
                Dim Variables As APSIMData = TypeVariables

                If TypeVariables.Attribute("link") <> "" Then
                    ' Load components description file if necessary
                    If ComponentDescriptionData Is Nothing Then
                        ComponentDescriptionData = New APSIMData
                        ComponentDescriptionData.LoadFromFile(APSIMSettings.ApsimDirectory + "\\ApsimUI\\ComponentDescription.xml")
                    End If

                    Dim Component As APSIMData = ComponentDescriptionData.Child(TypeVariables.Attribute("link"))
                    If Not IsNothing(Component) Then
                        Variables = ReturnVariables.Add(Component.ChildByType(PropertyGroup))
                        Variables.Name = TypeVariables.Name
                        Variables.SetAttribute("module", TypeVariables.Attribute("module"))
                    End If
                ElseIf Not IsNothing(Variables) Then
                    Variables = ReturnVariables.Add(Variables)
                End If

                If Not IsNothing(Variables) Then
                    If Variables.Name = Variables.Type Then
                        Variables.Name = InstanceName
                    End If
                    If Not Variables.AttributeExists("module") Then
                        Variables.SetAttribute("module", InstanceName)
                    Else
                        Variables.SetAttribute("module", Variables.Attribute("module").Replace("[name]", InstanceName))
                    End If
                End If
            Next
        End If
    End Sub


    Public Function GetVariableDescriptions(ByRef Data As APSIMData, ByVal PropertyGroup As String) As APSIMData
        ' ------------------------------------------------------------------------------
        ' Return variable descriptions for all child components for the specified "data"
        ' ------------------------------------------------------------------------------
        Dim ReturnVariables As New APSIMData(PropertyGroup, "")

        For Each Child As APSIMData In Data.Children
            Try
                GetVariablesForComponent(Child.Type, Child.Name, PropertyGroup, ReturnVariables)
            Catch e As Exception

            End Try

        Next
        Return ReturnVariables
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
            ComponentTypes.Add(Sibling.Type.ToLower())
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
            ComponentType = child.Attribute("ModuleType").ToLower()
            If ComponentType <> "" Then
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
    Public Overrides Function ImageFileForType(ByVal GivenType As String) As String
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

    Public Function GetPropertiesForType(ByVal GivenType As String, ByVal PropType As String) As String()
        ' ------------------------------------------------------------------
        ' Return from 'Types' database a list of property names
        ' ------------------------------------------------------------------
        Return TypesData.Child(GivenType).ChildNames(PropType)
    End Function

    ' ---------------------------------------
    ' Show some help file as specified by url
    ' ---------------------------------------
    Public Sub ShowHelp(ByVal url As String)
        Process.Start(url)
    End Sub

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


#Region "Generic UI functions"
    Public Overrides Sub CreateCellEditorForRow(ByVal Prop As APSIMData, _
                                                     ByVal Grid As FarPoint.Win.Spread.SheetView, _
                                                     ByVal Row As Integer)

        ' --------------------------------------------------------------------
        ' Create and return a cell editor based on the property based in.
        ' --------------------------------------------------------------------
        MyBase.CreateCellEditorForRow(Prop, Grid, Row)
        If Prop.Attribute("type") = "modulename" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Editable = True
            Grid.Cells(Row, 1).CellType = Combo

        ElseIf Prop.Attribute("type") = "crop" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Editable = True
            Grid.Cells(Row, 1).CellType = Combo

        ElseIf Prop.Attribute("type") = "cultivars" Then
            Dim CultivarCombo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            CultivarCombo.Editable = True
            Grid.Cells(Row, 1).CellType = CultivarCombo

        ElseIf Prop.Attribute("type") = "classes" Then
            Dim CultivarCombo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            CultivarCombo.Items = GetMatchingModuleNames(Prop)
            CultivarCombo.Editable = True
            Grid.Cells(Row, 1).CellType = CultivarCombo
        End If
    End Sub

    Public Overrides Sub PopulateCellEditor(ByVal Prop As APSIMData, ByVal Editor As FarPoint.Win.Spread.CellType.BaseCellType)
        ' --------------------------------------------------------------------
        ' Create and return a cell editor based on the property based in.
        ' --------------------------------------------------------------------
        MyBase.PopulateCellEditor(Prop, Editor)

        If Not IsNothing(Editor) AndAlso Editor.GetType().ToString = "FarPoint.Win.Spread.CellType.ComboBoxCellType" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = Editor
            If Prop.Attribute("type") = "modulename" Then
                Combo.Items = GetMatchingModuleNames(Prop)

            ElseIf Prop.Attribute("type") = "crop" Then
                Combo.Items = GetCropNames(Prop)

            ElseIf Prop.Attribute("type") = "cultivars" Then
                Dim CropPropertyName As String = Prop.Attribute("croppropertyname")
                Dim CropName As String = Prop.Parent.Child(CropPropertyName).Value
                Combo.Items = GetPropertiesForType(CropName, "cultivar")

            ElseIf Prop.Attribute("type") = "classes" Then
                Dim CropPropertyName As String = Prop.Attribute("croppropertyname")
                Dim CropName As String = Prop.Parent.Child(CropPropertyName).Value
                Combo.Items = GetPropertiesForType(CropName, "class")
            End If
        End If
    End Sub

    Public Function GetMatchingModuleNames(ByVal Prop As APSIMData) As String()
        ' ------------------------------------------------------------------
        ' Return a list of instance names for the specified module name
        ' ------------------------------------------------------------------
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

    Function GetCropNames(ByVal Prop As APSIMData) As String()
        ' ------------------------------------------------------------------
        ' Return a list of crop names
        ' ------------------------------------------------------------------
        Dim Values As New StringCollection
        Dim System As APSIMData = Prop.Parent
        While System.Type <> "simulation" And System.Type <> "area" And Not IsNothing(System.Parent)
            System = System.Parent
        End While

        For Each ApsimModule As APSIMData In System.Children
            Dim Child As APSIMData = TypesData.Child(ApsimModule.Type).Child("IsCrop")
            If Not IsNothing(Child) AndAlso Child.Value.ToLower = "yes" Then
                Values.Add(ApsimModule.Name())
            End If
        Next
        Dim ReturnValues(Values.Count - 1) As String
        Values.CopyTo(ReturnValues, 0)
        Return ReturnValues
    End Function



#End Region

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
