Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.io
Imports VBGeneral
Imports System.Reflection

Public Class BaseController
    Implements IDisposable

    ' Simple base class for a user interface manager

    Private MyData As APSIMData
    Private MyFileName As String
    Private MyDefaultExtension As String
    Private MyDialogFilter As String
    Private MyFrequentListSection As String
    Private MySelectedData As New StringCollection
    Private Updating As Boolean
    Protected disposed As Boolean = False
    Private MyMsgBoxString As String
    Private TypesFile As New APSIMData
    Private ActionFile As New APSIMData
    Private MyExplorer As ExplorerUI
    Private MyMainForm As Form
    Private LargeIcons As New ImageList
    Private MediumIcons As New ImageList
    Private SmallIcons As New ImageList
    Private TypesFileName As String

    Delegate Sub NotifyEventHandler()
    Delegate Sub NodeChangedEventHandler(ByVal OldNodeName As String, ByVal Node As APSIMData)
    Delegate Sub SelectionChangedHandler(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
    Delegate Sub NotifyRenameHandler(ByVal OldNodePath As String, ByVal NewNodePath As String)
    Delegate Sub RefreshRequiredHandler(ByVal Controller As BaseController)

    Public Event BeforeSaveEvent As NotifyEventHandler              ' Fired immediately before data is saved.
    Public Event AfterSaveEvent As NotifyEventHandler               ' Fired immediately after data is saved.
    Public Event SelectionChangingEvent As NotifyEventHandler       ' Fired when the current selection is about to change.
    Public Event SelectionChangedEvent As SelectionChangedHandler   ' Fired when the current selection has changed.
    Public Event RefreshRequiredEvent As RefreshRequiredHandler     ' Fired whenever a refresh of the ui is required.

#Region "Constructor / destructor"
    Sub New(ByVal MainForm As Form, ByVal SectionName As String)
        ' -----------------------
        ' constructor
        ' -----------------------
        MyMainForm = MainForm
        MyDefaultExtension = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), SectionName, "DefaultExtension")
        MyDialogFilter = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), SectionName, "DialogFilter")
        If Not IsNothing(MainForm) Then
            MyFrequentListSection = SectionName
        End If
        MyData = Nothing

        LargeIcons.ImageSize = New Size(32, 32)
        MediumIcons.ImageSize = New Size(24, 24)
        SmallIcons.ImageSize = New Size(16, 16)
        LargeIcons.Tag = "LargeIcon"
        MediumIcons.Tag = "MediumIcon"
        SmallIcons.Tag = "SmallIcon"

        ' Setup the types file and load all images specified by it.
        TypesFileName = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), SectionName, "TypesFile")
        If TypesFileName <> "" Then
            TypesFile.LoadFromFile(TypesFileName)
            For Each Child As APSIMData In TypesFile.Children
                LoadIcon(Child, "LargeIcon", LargeIcons)
                LoadIcon(Child, "MediumIcon", MediumIcons)
                LoadIcon(Child, "SmallIcon", SmallIcons)
            Next
        End If

        ' Setup the actions file and load all images specified by it.
        Dim ActionFileName As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), SectionName, "ActionFile")
        If ActionFileName <> "" Then
            ActionFile.LoadFromFile(ActionFileName)
            For Each Child As APSIMData In ActionFile.Child("Actions").Children
                LoadIcon(Child, "LargeIcon", LargeIcons)
                LoadIcon(Child, "MediumIcon", MediumIcons)
                LoadIcon(Child, "SmallIcon", SmallIcons)
            Next
        End If
        AddHandler ApsimData.DataDirtyChangedEvent, AddressOf OnDataDirtyChanged
    End Sub
    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
        If Not Me.disposed Then
            If disposing Then
                ' Insert code to free unmanaged resources.
            End If
            ' Insert code to free shared resources.
        End If
        Me.disposed = True
    End Sub
    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(True)
        GC.SuppressFinalize(Me)
    End Sub
    Protected Overrides Sub Finalize()
        Dispose(True)   ' Help says this should be false!!??!
        MyBase.Finalize()
    End Sub
#End Region

#Region "Overridable methods"
    Public Function IsComponentVisible(ByVal Component As APSIMData) As Boolean
        If Component.Attribute("invisible") = "yes" Then
            Return False
        End If
        Dim ComponentType As String = Component.Type()
        If Not IsNothing(TypesFile.Child(ComponentType)) Then
            If TypesFile.Child(ComponentType).Child("ShowInMainTree").Value = "Yes" Then
                Return True
            End If
        End If
        Return False
    End Function
    Public Function AllowComponentAdd(ByVal ChildComponentType As String, ByVal ParentComponentType As String) As Boolean
        ' -------------------------------------------------
        ' Return true if the specified component type can
        ' be added as a child to the specified parent type.
        ' -------------------------------------------------
        ' Look in the componentinfo's drop targets.
        For Each Drop As APSIMData In TypesFile.Child(ChildComponentType).Child("drops").Children
            If Drop.Name.ToLower = ParentComponentType.ToLower Then
                Return True
            End If
        Next
        ' if we get here we haven't found what we're after
        Return False
    End Function
    Public Function CreateUI(ByVal ComponentType As String) As BaseView
        ' -------------------------------------
        ' Create a User interface form for the
        ' specified type.
        ' -------------------------------------
        Dim ComponentInfo As APSIMData = TypesFile.Child(ComponentType)
        If Not IsNothing(ComponentInfo) Then
            Dim UIType As String = ComponentInfo.ChildValue("UItype")
            If UIType <> "" Then
                Return CreateClass(UIType)
            End If
        End If
        Return Nothing
    End Function
    Public Function ImageFileForType(ByVal GivenType As String) As String
        Return TypesFile.Child(GivenType).Child("Image").Value
    End Function
    Public Function GetComponentTypeInfo(ByVal ComponentType As String)
        Return TypesFile.Child(ComponentType)
    End Function
#End Region

#Region "Icon methods"
    Private Shared Sub LoadIcon(ByVal Data As APSIMData, ByVal Specifier As String, ByRef Icons As ImageList)
        ' -----------------------------------------------------------------
        ' Load an icon for the 'Data' type using the specifier. The icon
        ' is stored in the specified imagelist and an index node createdef
        ' to store the position of the icon in the imagelist.
        ' -----------------------------------------------------------------
        Dim IconChild As APSIMData = Data.Child(Specifier)
        If Not IsNothing(IconChild) Then
            Dim FileName As String = IconChild.Value
            If File.Exists(FileName) Then
                Dim Icon As New Bitmap(FileName)
                Icons.Images.Add(Icon)
                Data.ChildValue(Specifier + "Index") = Str(Icons.Images.Count - 1)
            End If
        End If
    End Sub
    Function IconImageList(ByVal IconType As String) As ImageList
        If IconType = "SmallIcon" Then
            Return SmallIcons
        ElseIf IconType = "MediumIcon" Then
            Return MediumIcons
        Else
            Return LargeIcons
        End If
    End Function
    Public Function IconImageIndexForType(ByVal ComponentType As String, ByVal IconType As String) As Integer
        Dim TypeInfo As APSIMData = TypesFile.Child(ComponentType)
        If Not IsNothing(TypeInfo) Then
            Dim ImageIndexChild As APSIMData = TypeInfo.Child(IconType + "Index")
            If Not IsNothing(ImageIndexChild) Then
                Return Val(ImageIndexChild.Value)
            End If
        End If
        Return -1
    End Function
    Public Function IconImageIndexForAction(ByVal Action As APSIMData, ByVal IconType As String) As Integer
        If Not IsNothing(Action) Then
            Dim ImageIndexChild As APSIMData = Action.Child(IconType + "Index")
            If Not IsNothing(ImageIndexChild) Then
                Return Convert.ToInt32(ImageIndexChild.Value)
            End If
        End If
        Return -1
    End Function
#End Region

#Region "Data methods"
    Public ReadOnly Property Data() As APSIMData
        ' -------------------------------------------------------
        ' Provides readonly access to the currently selected data.
        ' If more than one node is selected then this property
        ' will throw.
        ' -------------------------------------------------------
        Get
            If MySelectedData.Count > 1 Then
                Throw New System.Exception("Multiple items selected. Cannot return data")
            ElseIf MySelectedData.Count = 0 Then
                Return Nothing
            Else
                Return ApsimData.Find(MySelectedData(0))
            End If
        End Get
    End Property
    Public ApsimData As New ApsimFile.ApsimFile
#End Region

    Public ReadOnly Property MainForm() As Form
        Get
            Return MyMainForm
        End Get
    End Property
    Public Property Explorer() As ExplorerUI
        Get
            Return (MyExplorer)
        End Get
        Set(ByVal value As ExplorerUI)
            MyExplorer = value
        End Set
    End Property
    Public ReadOnly Property FileName() As String
        ' --------------------------------------------------------
        ' Provides readonly access to the current filename
        ' --------------------------------------------------------
        Get
            Return MyFileName
        End Get
    End Property
    Public ReadOnly Property OpenDialogFilter() As String
        Get
            Return MyDialogFilter
        End Get
    End Property
    Private Sub OnDataDirtyChanged(ByVal IsDirty As Boolean)
        ' ----------------------------------------
        ' Called to update the main form's caption
        ' ----------------------------------------
        If Not IsNothing(MainForm) Then
            If ApsimData.IsReadOnly Then
                MainForm.Text = MyFrequentListSection + " - " + FileName + " [readonly]"
            ElseIf IsDirty Then
                MainForm.Text = MyFrequentListSection + " - " + FileName + " * "
            Else
                MainForm.Text = MyFrequentListSection + " - " + FileName
            End If
        End If
    End Sub
    Public Function AllowAddXMLToData(ByVal XML As String, ByVal ParentPath As String) As Boolean
        ' --------------------------------------------------------------
        ' Do we allow the specified XML to be added to the specified
        ' parent node?
        ' --------------------------------------------------------------
        If ApsimData.IsReadOnly Then
            Return False
        Else
            ' Do we allow the specified xml to be added to the selected node?
            Dim ParentData As APSIMData = ApsimData.AllData.Find(ParentPath)
            Dim NewData As New APSIMData("<dummy>" + XML + "</dummy>")
            Dim ok As Boolean = True
            For Each Child As APSIMData In NewData.Children
                ok = ok And Me.AllowComponentAdd(Child.Type, ParentData.Type)
            Next
            Return ok And NewData.Children.Length > 0
        End If
    End Function

#Region "File handling methods"
    Public Function FileSaveAfterPrompt() As Boolean
        ' --------------------------------------------------------
        ' Often called at program exit to optionally prompt the
        ' user to save the current data if something has changed.
        ' --------------------------------------------------------
        If ApsimData.IsDirty Then
            Dim DoSave As Integer = MessageBox.Show("The current file has changed. Do you want to save it before proceeding?", _
                                                    "Save?", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)
            Select Case DoSave
                Case DialogResult.Yes
                    ' Save the file
                    ApsimData.Save(MyFileName)

                Case DialogResult.No
                    ' Do not save

                Case DialogResult.Cancel
                    ' Cancel pressed.
                    Return False
            End Select
        End If
        Return True
    End Function
    Public Sub LoadPreviousFile()
        For Each PreviousFileName As String In GetFrequentList()
            If File.Exists(PreviousFileName) Then
                FileOpen(PreviousFileName)
                Exit For
            End If
        Next
    End Sub
    Public Sub FileOpen(ByVal FileName As String)
        If File.Exists(FileName) Then
            MyFileName = FileName
            AddFileToFrequentList(MyFileName)

            ' Need to work out if the file should be readonly.
            Dim ReadOnlyFile As Boolean = (File.GetAttributes(FileName) And FileAttributes.ReadOnly) = FileAttributes.ReadOnly
            If Not ReadOnlyFile Then
                Dim FileNameNoPath As String = Path.GetFileName(FileName).ToLower()
                ReadOnlyFile = FileNameNoPath = "apsru-australia-soils.soils" _
                               OrElse FileNameNoPath = "standard.xml" _
                               OrElse FileNameNoPath = "new simulations.xml" _
                               OrElse FileNameNoPath = "graph.xml"
            End If
            If Not ReadOnlyFile Then
                Dim FileNameLower As String = Path.GetFileName(FileName).ToLower()
                If FileNameLower.Contains("apsru-australia-soils-") Then
                    Dim Password As String = InputDialog.InputBox("Enter password:", "This file is password protected", "", True)
                    If Password = "soilinfo" Then
                        ReadOnlyFile = False
                    Else
                        MessageBox.Show("Password incorrect. File will open as readonly", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                        ReadOnlyFile = True
                    End If
                End If
            End If

            Dim FileData As New APSIMData
            If FileData.LoadFromFile(FileName) Then
                SelectedPath = ""
                ApsimData.Open(FileData.XML, ReadOnlyFile)
                SelectedPath = ApsimData.Find("").Name
            End If
        End If
    End Sub
    Public Sub FileSave(ByVal FileName As String)
        MyFileName = FileName
        ApsimData.SaveAs(FileName)
        AddFileToFrequentList(FileName)
        OnDataDirtyChanged(False)
    End Sub

    Private Const MAX_NUM_FREQUENT_SIMS As Integer = 10
    Private Sub AddFileToFrequentList(ByVal filename As String)
        If MyFrequentListSection <> "" Then

            Dim FileNames() As String = GetFrequentList()

            ' Put this filename into the top of the NewFileList and
            ' then copy all existing filenames to NewFileList
            Dim NewFileList(MAX_NUM_FREQUENT_SIMS) As String
            NewFileList(0) = filename
            Dim NewFileListIndex As Integer = 1
            For i As Integer = 0 To FileNames.Length - 1
                If FileNames(i) <> filename And FileNames(i) <> "" And NewFileListIndex < MAX_NUM_FREQUENT_SIMS Then
                    NewFileList(NewFileListIndex) = FileNames(i)
                    NewFileListIndex = NewFileListIndex + 1
                End If
            Next

            ' Write NewFileList back to .ini file.
            APSIMSettings.INIWriteMultiple(APSIMSettings.ApsimIniFile(), MyFrequentListSection, "RecentFile", NewFileList)
        End If
    End Sub
    Private Function GetFrequentList() As String()
        Dim FileNames As StringCollection = APSIMSettings.INIReadMultiple(APSIMSettings.ApsimIniFile(), MyFrequentListSection, "RecentFile")
        Dim GoodFileNames As New StringCollection
        For Each FileName As String In FileNames
            If File.Exists(FileName) Then
                GoodFileNames.Add(FileName)
            End If
        Next
        Dim ReturnArray(GoodFileNames.Count - 1) As String
        GoodFileNames.CopyTo(ReturnArray, 0)
        Return ReturnArray
    End Function
#End Region

#Region "Node selection methods. NB: Paths are delimited with '\' characters"
    Public Property SelectedPaths() As StringCollection
        Get
            ' --------------------------------------------------------
            ' Provide readwrite access to the current selections.
            ' The strings returned contain data paths to all
            ' selected nodes.
            ' --------------------------------------------------------
            Dim ReturnValues As New StringCollection
            For Each FullPath As String In MySelectedData
                ReturnValues.Add(FullPath)
            Next
            Return ReturnValues
        End Get
        Set(ByVal Paths As StringCollection)
            Dim OldSelections As New StringCollection
            For Each Selection As String In MySelectedData
                OldSelections.Add(Selection)
            Next
            RaiseEvent SelectionChangingEvent()
            MySelectedData = Paths
            RaiseEvent SelectionChangedEvent(OldSelections, MySelectedData)
            RefreshToolStrips()
        End Set
    End Property
    Public Property SelectedPath() As String
        Get
            If (MySelectedData.Count = 1) Then
                Return MySelectedData(0)
            Else
                Throw New Exception("Too many nodes selected - expected 1")
            End If
        End Get
        Set(ByVal value As String)
            Dim NewSelections As New StringCollection
            If value <> "" Then
                NewSelections.Add(value)
            End If
            SelectedPaths = NewSelections
        End Set
    End Property
#End Region

#Region "Generic UI functions"
    Public Overridable Sub CreateCellEditorForRow(ByVal Prop As APSIMData, _
                                                  ByVal Grid As FarPoint.Win.Spread.SheetView, _
                                                  ByVal Row As Integer)
        ' --------------------------------------------------------------------
        ' Create and return a cell editor based on the property based in.
        ' --------------------------------------------------------------------
        If Prop.Attribute("type") = "yesno" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Items = New String() {"yes", "no"}
            Grid.Cells(Row, 1).CellType = Combo

        ElseIf Prop.Attribute("type") = "date" Then
            Dim DateEditor As FarPoint.Win.Spread.CellType.DateTimeCellType = New FarPoint.Win.Spread.CellType.DateTimeCellType
            DateEditor.DateTimeFormat = FarPoint.Win.Spread.CellType.DateTimeFormat.ShortDate
            DateEditor.DateDefault = Prop.Value
            Dim ubound As Date
            Dim lbound As Date
            Dim dtValue As Date = Prop.Value
            If (Prop.Attribute("ubound")) <> "" Then
                ubound = Prop.Attribute("ubound")
                If dtValue > ubound Then
                    Prop.Value = Prop.Attribute("ubound")
                    DateEditor.DateDefault = Prop.Value
                    MessageBox.Show("The " + Prop.Type + " selected is after the dates in the Met file." & vbCrLf & _
                                "Automatically adjusting simulation end date to match the Met file end date")
                    DateEditor.MaximumDate = ubound
                End If
            End If
            If (Prop.Attribute("lbound")) <> "" Then
                lbound = Prop.Attribute("lbound")
                If dtValue < lbound Then
                    Prop.Value = Prop.Attribute("lbound")
                    DateEditor.DateDefault = Prop.Value
                    MessageBox.Show("The " + Prop.Type + " selected is before the dates in the Met file." & vbCrLf & _
                                "Automatically adjusting simulation start date to match the Met file start date")
                    DateEditor.MinimumDate = lbound
                End If

            End If

            DateEditor.DropDownButton = True
            Grid.Cells(Row, 1).CellType = DateEditor

        ElseIf Prop.Attribute("type") = "list" Then
            Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
            Combo.Items = Prop.Attribute("listvalues").Split(",")
            Combo.Editable = True
            Grid.Cells(Row, 1).CellType = Combo

        ElseIf Prop.Attribute("type") = "multilist" Then
            Dim Combo As CheckedListBoxCellType = New CheckedListBoxCellType
            Combo.Items = Prop.Attribute("listvalues").Split(",")
            Grid.Cells(Row, 1).CellType = Combo
            Grid.Rows(Row).Height = 80

        ElseIf Prop.Attribute("type") = "filenames" Or Prop.Attribute("type") = "filename" Then
            If Prop.Attribute("type") = "filenames" Then
                Dim Text As FarPoint.Win.Spread.CellType.TextCellType = New FarPoint.Win.Spread.CellType.TextCellType
                Text.Multiline = True
                Text.MaxLength = 65536
                Text.ScrollBars = ScrollBars.Both
                Grid.Cells(Row, 1).CellType = Text
                Grid.Rows(Row).Height = 80
            End If

            Grid.Columns(2).Visible = True
            Grid.Cells(Row, 2).Locked = False
            Dim Button As FarPoint.Win.Spread.CellType.ButtonCellType = New FarPoint.Win.Spread.CellType.ButtonCellType
            Button.Picture = My.Resources.folder
            Grid.Cells(Row, 2).CellType = Button

        ElseIf Prop.Attribute("type") = "multiedit" Then
            Dim Text As FarPoint.Win.Spread.CellType.TextCellType = New FarPoint.Win.Spread.CellType.TextCellType
            Text.Multiline = True
            Text.MaxLength = 5000
            Grid.Cells(Row, 1).CellType = Text
            Grid.Rows(Row).Height = 80
        End If

    End Sub

    Public Overridable Sub PopulateCellEditor(ByVal Prop As APSIMData, ByVal Editor As FarPoint.Win.Spread.CellType.BaseCellType)
    End Sub

    Public Overridable Sub OnButtonClick(ByVal sender As System.Object, ByVal e As FarPoint.Win.Spread.EditorNotifyEventArgs, ByVal Prop As APSIMData)
        Dim Spread As FarPoint.Win.Spread.FpSpread = sender
        Dim Grid As FarPoint.Win.Spread.SheetView = Spread.ActiveSheet

        Dim Dialog As New OpenFileDialog
        Dialog.AddExtension = True
        Dialog.Multiselect = Prop.Attribute("type").ToLower() = "filenames"
        If Dialog.ShowDialog = DialogResult.OK Then
            Dim Text As String = ""
            For Each FileName As String In Dialog.FileNames
                Text += FileName + vbCrLf
            Next
            Grid.Cells(e.Row, 1).Value = Text
        End If
    End Sub
#End Region

#Region "Type info methods"
    Public Function DescriptionForType(ByVal GivenType As String) As String
        ' -----------------------------------------------------------------
        ' Return description for the specified type.
        ' -----------------------------------------------------------------
        Return TypesFile.Child(GivenType).Child("Description").Value
    End Function

    Private ComponentDescriptionData As APSIMData = Nothing
    Public Sub GetVariablesForComponent(ByVal ComponentType As String, ByVal InstanceName As String, _
                                        ByVal PropertyGroup As String, ByVal ReturnVariables As APSIMData)
        ' -----------------------------------------------------------------
        ' Add variable info for the specified type and instance name to the
        ' "VariableData" argument.
        ' -----------------------------------------------------------------
        Dim TypeInfo As APSIMData = TypesFile.Child(ComponentType)
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

#End Region


#Region "Action methods"
    ' ------------------------------------------------------------------------
    ' These action methods populate and implement all context menu's and 
    ' ToolStrips in the application. All actions are defined in an actions.xml
    ' file. Each type in types.xml has zero or more actions in it. When the
    ' user clicks on a node in the tree, the context menu will contain those
    ' actions referred to in the types.xml under the type the user clicked on.
    ' ------------------------------------------------------------------------


    Private ToolStrips As New List(Of ToolStrip)

    Public Sub RefreshToolStrips()
        For Each ToolStrip As ToolStrip In ToolStrips
            EnableActions(ToolStrip)
        Next
    End Sub
    Public Sub ProvideToolStrip(ByVal Strip As ToolStrip, ByVal ToolStripName As String)
        ' --------------------------------------------------------------
        ' This method populates the specified context menu for the 
        ' currently selected type. 
        ' --------------------------------------------------------------
        Strip.Items.Clear()
        AddHandler Strip.ItemClicked, AddressOf ActionOnClick
        Dim ToolStripDescriptor As APSIMData = ActionFile.Child(ToolStripName)
        If Not IsNothing(ToolStripDescriptor) Then
            Dim ImageAboveText As Boolean = False
            ImageAboveText = (ToolStripDescriptor.Attribute("ImageAboveText") = "yes")
            PopulateToolStrip(Strip, ToolStripDescriptor, ImageAboveText)
        End If
        ToolStrips.Add(Strip)
    End Sub
    Public Sub RemoveToolStrip(ByVal Strip As ToolStrip)
        ToolStrips.Remove(Strip)
    End Sub

    Private Sub PopulateToolStrip(ByVal Strip As ToolStrip, ByVal ToolStripDescriptor As APSIMData, ByVal ImageAboveText As Boolean)
        ' --------------------------------------------------------------
        ' This method populates the specified context menu given the
        ' specified descriptor data. 
        ' --------------------------------------------------------------

        For Each ToolDescriptor As APSIMData In ToolStripDescriptor.Children
            If ToolDescriptor.Type = "ImageSize" Then
                Strip.ImageList = IconImageList(ToolDescriptor.Value)

            ElseIf ToolDescriptor.Type = "Item" Then
                Dim Item As ToolStripItem = CreateToolStripItem(Strip, ToolDescriptor.Value)
                If ImageAboveText Then
                    Item.TextImageRelation = TextImageRelation.ImageAboveText
                End If

            ElseIf ToolDescriptor.Type = "DropDownItem" Then
                Dim DropDownActionName As String = ToolDescriptor.Attribute("action")
                Dim Action As APSIMData = ActionFile.Find("Folder\Actions\" + DropDownActionName)
                Dim DropDownButton As ToolStripDropDownItem
                Dim DropDownStrip As ToolStripDropDown
                If Strip.GetType().ToString = "System.Windows.Forms.ToolStrip" Then
                    DropDownButton = New ToolStripDropDownButton
                    DropDownStrip = New ToolStripDropDownMenu
                Else
                    DropDownButton = New ToolStripMenuItem
                    DropDownStrip = New ToolStripDropDownMenu
                End If
                DropDownButton.Text = Action.ChildValue("text")
                DropDownButton.ImageIndex = IconImageIndexForAction(Action, Strip.ImageList.Tag.ToString)
                DropDownButton.ImageScaling = ToolStripItemImageScaling.None
                DropDownButton.AutoToolTip = False
                DropDownButton.Tag = DropDownActionName
                DropDownButton.Enabled = IsActionAllowed(Action)

                If ImageAboveText Then
                    DropDownButton.TextImageRelation = TextImageRelation.ImageAboveText
                End If

                DropDownStrip.ImageList = SmallIcons
                AddHandler DropDownStrip.ItemClicked, AddressOf ActionOnClick
                PopulateToolStrip(DropDownStrip, ToolDescriptor, False)  ' recursion

                DropDownButton.DropDown = DropDownStrip
                Strip.Items.Add(DropDownButton)

            ElseIf ToolDescriptor.Type = "Separator" Then
                Strip.Items.Add(New ToolStripSeparator)

            ElseIf ToolDescriptor.Type = "RecentFileList" Then
                For Each FileName As String In GetFrequentList()
                    Dim Item As ToolStripItem = Strip.Items.Add(FileName)
                    Item.ImageIndex = -1
                    Item.Tag = ""
                    Item.ToolTipText = "Open this file"
                    If ImageAboveText Then
                        Item.TextImageRelation = TextImageRelation.ImageAboveText
                    End If
                Next
            End If
        Next
    End Sub
    Private Function CreateToolStripItem(ByVal Strip As ToolStrip, ByVal ActionName As String) As ToolStripItem
        Dim Action As APSIMData = ActionFile.Find("Folder\Actions\" + ActionName)
        If Not IsNothing(Action) Then
            Dim Item As ToolStripItem = Strip.Items.Add(Action.ChildValue("text"))
            Item.ImageIndex = IconImageIndexForAction(Action, Strip.ImageList.Tag.ToString)
            Item.ToolTipText = Action.ChildValue("description")
            Item.Tag = ActionName
            Item.ImageScaling = ToolStripItemImageScaling.None
            Dim ShortCut As String = Action.ChildValue("shortcut")
            If TypeOf Item Is ToolStripMenuItem AndAlso ShortCut <> "" Then
                CType(Item, ToolStripMenuItem).ShortcutKeys = Keys.Parse(GetType(Keys), ShortCut)
            End If
            Item.Enabled = IsActionAllowed(Action)
            Return Item
        Else
            Return Nothing
        End If
    End Function
    Private Sub EnableActions(ByVal Toolbox As ToolStrip)
        For Each ToolItem As ToolStripItem In Toolbox.Items
            If Not IsNothing(ToolItem.Tag) Then
                ToolItem.Enabled = IsActionAllowed(ActionFile.Find("Folder\Actions\" + ToolItem.Tag.ToString))
            End If
            If TypeOf ToolItem Is ToolStripDropDownItem Then
                Dim DropDownItem As ToolStripDropDownItem = ToolItem
                ToolItem.Enabled = IsActionAllowed(ActionFile.Find("Folder\Actions\" + ToolItem.Tag.ToString))
                EnableActions(DropDownItem.DropDown)
            End If
        Next
    End Sub
    Private Function IsActionAllowed(ByVal Action As APSIMData) As Boolean
        Dim Allowed As Boolean = True
        If Not IsNothing(Action) Then
            For Each DisabledWhen As APSIMData In Action.Children("DisabledWhen")
                Dim DisabledWhenFlag As String = DisabledWhen.Value
                If DisabledWhenFlag = "ReadOnly" AndAlso ApsimData.IsReadOnly Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "RootNode" AndAlso MySelectedData.Count > 0 AndAlso MySelectedData(0).IndexOf("\") = -1 Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "MultipleNodesSelected" AndAlso MySelectedData.Count > 1 Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "NothingLoaded" AndAlso FileName = "" Then
                    Allowed = False
                End If
                If Not IsNothing(DisabledWhen.Child("call")) Then
                    Dim ClassToCall As String = DisabledWhen.ChildValue("call\class")
                    Dim MethodToCall As String = DisabledWhen.ChildValue("call\method")
                    Allowed = Not CallMethodOfClass(ClassToCall, MethodToCall)
                End If
            Next

            If Allowed And MySelectedData.Count = 1 And Action.Children("AppliesTo").Length > 0 Then
                Allowed = False
                For Each AppliesTo As APSIMData In Action.Children("AppliesTo")
                    If AppliesTo.Value.ToLower = Data.Type.ToLower Then
                        Allowed = True
                    End If
                Next
            End If
        End If
        Return Allowed
    End Function

    Private Sub ActionOnClick(ByVal Sender As Object, ByVal E As ToolStripItemClickedEventArgs)
        ' --------------------------------------------------------------
        ' When the user clicks on an action, this method is called.
        ' Go perform whatever action is necessary.
        ' --------------------------------------------------------------
        If Not IsNothing(E.ClickedItem.Tag) Then
            If E.ClickedItem.Tag = "" AndAlso FileSaveAfterPrompt() Then
                FileOpen(E.ClickedItem.Text)
            Else
                InvokeAction(Sender, E.ClickedItem.Tag)
            End If
        End If
    End Sub
    Private Sub InvokeAction(ByVal Sender As Object, ByVal ActionName As String)
        Dim Action As APSIMData = ActionFile.Find("Folder\Actions\" + ActionName)
        Dim ClassToCall As String = Action.ChildValue("OnInvoke\call\class")
        Dim MethodToCall As String = Action.ChildValue("OnInvoke\call\method")
        If ClassToCall <> "" And MethodToCall <> "" Then
            ' For some reason, if we don't explicitly close the menus they remain open,
            ' and on top of other windows.
            If TypeOf Sender Is ContextMenuStrip Then
                CType(Sender, ContextMenuStrip).Close()
            ElseIf TypeOf Sender Is ToolStripDropDownMenu Then
                Dim Menu As ToolStripDropDownMenu = CType(Sender, ToolStripDropDownMenu)
                If Not IsNothing(Menu.OwnerItem) AndAlso Not IsNothing(Menu.OwnerItem.Owner) _
                       AndAlso TypeOf Menu.OwnerItem.Owner Is ContextMenuStrip Then
                    CType(Menu.OwnerItem.Owner, ContextMenuStrip).Close()
                End If
                Menu.Close()
            End If

            ' Now go and do our action.
            CallMethodOfClass(ClassToCall, MethodToCall)
        End If
    End Sub
    Private Function CallMethodOfClass(ByVal ClassToCall As String, ByVal MethodToCall As String) As Object
        ' --------------------------------------------------------------
        ' Call a static/shared method of a class to perform the 
        ' necessary action.
        ' --------------------------------------------------------------
        Try
            Dim PosPeriod As Integer = ClassToCall.IndexOf(".")
            If PosPeriod = -1 Then
                Throw New Exception("No namespace specified in action: " + ClassToCall)
            End If
            Dim NameSpaceName As String = ClassToCall.Substring(0, PosPeriod)
            Dim t As Type = Nothing
            For Each Assemb As Assembly In AppDomain.CurrentDomain.GetAssemblies
                If NameSpaceName = Assemb.GetName().Name Then
                    t = Assemb.GetType(ClassToCall, True, True)
                End If
            Next
            If IsNothing(t) Then
                Throw New Exception("Cannot find type: " + ClassToCall)
            End If

            Dim Method As MethodInfo = t.GetMethod(MethodToCall)
            If IsNothing(Method) Then
                Throw New Exception("Cannot find method '" + MethodToCall + "' in class '" + ClassToCall + "'")
            End If

            Dim Params(0) As Object
            Params(0) = Me
            Return Method.Invoke(Nothing, Params)
        Catch ex As Exception
            MessageBox.Show(ex.GetBaseException().Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End Try
        Return Nothing
    End Function
    Private Shared Function CreateClass(ByVal ClassToCall As String) As Object
        ' --------------------------------------------------------------
        ' Call a static/shared method of a class to perform the 
        ' necessary action.
        ' --------------------------------------------------------------
        Try
            Dim PosPeriod As Integer = ClassToCall.IndexOf(".")
            If PosPeriod = -1 Then
                Throw New Exception("No namespace specified in action: " + ClassToCall)
            End If
            Dim NameSpaceName As String = ClassToCall.Substring(0, PosPeriod)
            Dim t As Type = Nothing
            For Each Assemb As Assembly In AppDomain.CurrentDomain.GetAssemblies
                If NameSpaceName = Assemb.GetName().Name Then
                    t = Assemb.GetType(ClassToCall, True, True)
                End If
            Next
            If IsNothing(t) Then
                Throw New Exception("Cannot find type: " + ClassToCall)
            End If
            Return Activator.CreateInstance(t)

        Catch ex As Exception
            MessageBox.Show(ex.GetBaseException().Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End Try
        Return Nothing
    End Function

#End Region

End Class
