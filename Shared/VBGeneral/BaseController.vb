Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.io


Public MustInherit Class BaseController
    ' Simple base class for a user interface manager

    Private MyData As APSIMData
    Private MyFileName As String
    Private MyDefaultExtension As String
    Private MyDirtyData As Boolean
    Private MyDialogFilter As String
    Private MyFrequentListSection As String
    Private MySelectedData As New StringCollection
    Private Updating As Boolean
    Private IsReadOnly As Boolean

    Delegate Sub NotifyEventHandler()
    Delegate Sub NodeChangedEventHandler(ByVal OldNodeName As String, ByVal Node As APSIMData)
    Delegate Sub SelectionChangedHandler(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
    Delegate Sub NotifyRenameHandler(ByVal OldNodePath As String, ByVal NewNodePath As String)
    Public Event NewDataEvent As NotifyEventHandler
    Public Event DataChangedEvent As NotifyEventHandler
    Public Event NodeChangedEvent As NodeChangedEventHandler
    Public Event BeforeSaveEvent As NotifyEventHandler
    Public Event SelectionChangingEvent As NotifyEventHandler
    Public Event SelectionChangedEvent As SelectionChangedHandler
    Public Event ReadonlyEvent As NotifyEventHandler

    Sub New(ByVal DefaultExtension As String, _
            ByVal DialogFilter As String, _
            ByVal FrequentListSection As String)
        MyDefaultExtension = DefaultExtension
        MyDialogFilter = DialogFilter
        MyFrequentListSection = FrequentListSection
        MyData = Nothing
        MyDirtyData = False
    End Sub

#Region "Overridable methods"
    MustOverride ReadOnly Property SmallImageList() As ImageList

    MustOverride Function SmallImageIndex(ByVal ComponentType As String) As Integer

    MustOverride Function IsComponentVisible(ByVal ComponentName As String) As Boolean

    MustOverride Function AllowComponentAdd(ByVal ChildComponentType As String, ByVal ParentComponentType As String) As Boolean

    MustOverride Function CreateUI(ByVal UIType As String) As BaseView
#End Region

#Region "Data methods"
    Public ReadOnly Property Data() As APSIMData
        Get
            If MySelectedData.Count > 1 Then
                Throw New System.Exception("Multiple items selected. Cannot return data")
            ElseIf MySelectedData.Count = 0 Then
                Return Nothing
            Else
                Return GetDataForFullPath(MySelectedData(0))
            End If
        End Get
    End Property

    Public Property AllData() As APSIMData
        Get
            Return MyData
        End Get
        Set(ByVal Value As APSIMData)
            MyData = Value

            RaiseEvent NewDataEvent()
            AddHandler MyData.DataChanged, AddressOf OnDataChanged

            Dim NewSelections As New StringCollection
            NewSelections.Add(MyData.Name)
            SelectedPaths = NewSelections
        End Set
    End Property
    Private Sub OnDataChanged()
        DirtyData = True
    End Sub
    Property DirtyData() As Boolean
        Get
            Return MyDirtyData
        End Get
        Set(ByVal Value As Boolean)
            If MyDirtyData <> Value Then
                MyDirtyData = Value
                RaiseEvent DataChangedEvent()
            End If
        End Set
    End Property
#End Region

#Region "File handling methods"
    Public Property FileName() As String
        Get
            Return MyFileName
        End Get
        Set(ByVal newFileName As String)
            Me.MyFileName = newFileName

        End Set
    End Property
    ReadOnly Property AllowFileSave() As Boolean
        Get
            Return DirtyData And AllowChanges
        End Get
    End Property
    ReadOnly Property AllowFileSaveAs() As Boolean
        Get
            Return DirtyData
        End Get
    End Property
    Public Sub FileNew(ByVal DataToUse As APSIMData)
        If FileSaveAfterPrompt() Then
            MyFileName = "Untitled" + MyDefaultExtension
            DirtyData = True
            AllData = DataToUse
            IsReadOnly = False
        End If
    End Sub
    Public Sub FileNew(ByVal FileName As String)
        Dim FileData As New APSIMData
        If FileData.LoadFromFile(FileName) Then
            MyFileName = "Untitled" + MyDefaultExtension
            DirtyData = True
            AllData = FileData
            IsReadOnly = False
        End If
    End Sub
    Public Overridable Function AllowFileOpenWrite(ByVal FileName As String) As Boolean
        Return True
    End Function
    Public Sub FileOpen()
        If FileSaveAfterPrompt() Then
            Dim dialog As New OpenFileDialog
            With dialog
                .Filter = MyDialogFilter
                .AddExtension = True
            End With
            Dim choice As DialogResult = dialog.ShowDialog
            If choice = DialogResult.OK Then
                IsReadOnly = Not AllowFileOpenWrite(dialog.FileName)
                IsReadOnly = IsReadOnly Or ((File.GetAttributes(dialog.FileName) And FileAttributes.ReadOnly) = FileAttributes.ReadOnly)
                If IsReadOnly Then
                    MessageBox.Show("The file: " + dialog.FileName + " is readonly. All editing capability is disabled.", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning)
                End If
                FileOpen(dialog.FileName)
            End If
        End If
    End Sub
    Public Function FileOpen(ByVal FileName As String) As Boolean
        Dim FileData As New APSIMData
        If FileData.LoadFromFile(FileName) Then
            MyFileName = FileName
            DirtyData = False
            AllData = FileData
            AddFileToFrequentList(MyFileName)
            Return True
        Else
            Return False
        End If
    End Function
    Public Function FileOpenReadOnly(ByVal FileName As String) As Boolean
        IsReadOnly = True
        FileOpen(FileName)
    End Function
    Function FileSave() As Boolean
        RaiseEvent BeforeSaveEvent()

        If MyFileName.IndexOf("Untitled.") <> -1 Then
            Return FileSaveAs()
        Else
            AllData.SaveToFile(MyFileName)
            AddFileToFrequentList(MyFileName)
            DirtyData = False
            Return True
        End If
    End Function
    Function FileSaveAs() As Boolean
        Dim dialog As New SaveFileDialog
        With dialog
            .Filter = MyDialogFilter
            .AddExtension = True
            .OverwritePrompt = True
        End With
        Dim choice As DialogResult = dialog.ShowDialog
        If choice = DialogResult.OK Then
            MyFileName = dialog.FileName
            Return FileSave()
        Else
            ' User has cancelled - do nothing
            Return False
        End If
    End Function
    Public Function FileSaveAfterPrompt() As Boolean
        If DirtyData Then
            Dim DoSave As Integer = MessageBox.Show("The current file has changed. Do you want to save it before proceeding?", _
                                                    "Save?", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)
            Select Case DoSave
                Case DialogResult.Yes
                    ' Save the file
                    FileSave()

                Case DialogResult.No
                    ' Do not save

                Case DialogResult.Cancel
                    ' Cancel pressed.
                    Return False
            End Select
        End If
        Return True
    End Function
#End Region

#Region "Frequent file list methods"
    Private Const MAX_NUM_FREQUENT_SIMS As Integer = 10
    Public Sub AddFileToFrequentList(ByVal filename As String)
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
    Public Function GetFrequentList() As String()
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

#Region "Node selection methods. NB: Paths are delimited with '|' characters"
    Public Property SelectedPaths() As StringCollection
        Get
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
        End Set
    End Property
    Public ReadOnly Property SelectedData() As ArrayList
        Get
            If MySelectedData.Count = 0 Then
                Return New ArrayList(0)
            Else
                Dim ReturnData As New ArrayList(MySelectedData.Count - 1)
                For Each FullPath As String In MySelectedData
                    ReturnData.Add(GetDataForFullPath(FullPath))
                Next
                Return ReturnData
            End If
        End Get
    End Property
    Public ReadOnly Property SomethingIsSelected() As Boolean
        Get
            Return MySelectedData.Count > 0
        End Get
    End Property
    Private Function GetDataForFullPath(ByVal FullPath As String) As APSIMData
        Dim PosDelimiter As Integer = FullPath.IndexOf("|")
        If PosDelimiter = -1 Then
            If FullPath = AllData.Name Then
                Return AllData
            Else
                Throw New System.Exception("Cannot find parent name in GetDataForFullPath. Invalid FullPath: " + FullPath)
            End If
        Else
            Return AllData.FindChild(FullPath.Substring(PosDelimiter + 1))
        End If
    End Function
    Public Shared Function GetFullPathForData(ByVal Data As APSIMData) As String
        Dim LocalData As APSIMData = Data
        Dim FullPath As String = LocalData.Name
        LocalData = LocalData.Parent
        While Not IsNothing(LocalData)
            FullPath = LocalData.Name + "|" + FullPath
            LocalData = LocalData.Parent
        End While
        Return FullPath
    End Function
#End Region

#Region "Clipboard methods"
    ReadOnly Property AllowCut() As Boolean
        Get
            Return AllowDeleteSelected()
        End Get
    End Property
    ReadOnly Property AllowCopy() As Boolean
        Get
            Return MySelectedData.Count > 0
        End Get
    End Property
    ReadOnly Property AllowPaste() As Boolean
        Get
            If MySelectedData.Count = 1 Then
                Dim iData As IDataObject = Clipboard.GetDataObject()

                ' Determines whether the data is in a format we can use.
                If iData.GetDataPresent(DataFormats.Text) And MySelectedData.Count = 1 Then
                    Dim xml As String = CType(iData.GetData(DataFormats.Text), String)
                    Try
                        Return AllowAddXMLToData(xml, MySelectedData(0))
                    Catch ex As System.Exception
                    End Try
                End If
            End If
            Return False
        End Get
    End Property

    Public Sub Cut()
        If AllowCut() Then
            Copy()
            Delete(SelectedPaths)
        End If
    End Sub
    Public Sub Copy()
        If AllowCopy Then
            Dim Contents As String = ""
            For Each DataPath As String In MySelectedData
                Contents = Contents + GetDataForFullPath(DataPath).XML + "\r\n"
            Next
            Clipboard.SetDataObject(Contents, True)
        End If
    End Sub
    Public Sub Paste()
        If AllowPaste() Then
            Dim iData As IDataObject = Clipboard.GetDataObject()
            Dim xml As String = CType(iData.GetData(DataFormats.Text), String)
            AddXMLToSelected(xml)
        End If
    End Sub
#End Region

#Region "Data manipulation"
    Public ReadOnly Property AllowChanges() As Boolean
        Get
            Return Not Me.IsReadOnly
        End Get
    End Property

    Public Function AllowRenameSelected() As Boolean
        ' Do we allow the node, as specified by FullPath, to be renamed?
        Return AllowChanges() AndAlso MySelectedData.Count = 1 AndAlso Data.Name.ToLower() <> "shared" _
               AndAlso MySelectedData(0).IndexOf("Shared:") = -1
    End Function
    Public ReadOnly Property AllowDeleteSelected() As Boolean
        Get
            Return AllowChanges() And Not IsTopLevelNodeSelected()
        End Get
    End Property
    Public Function AllowAddXMLToData(ByVal XML As String, ByVal ParentPath As String) As Boolean
        If Not AllowChanges Then
            Return False
        Else
            ' Do we allow the specified xml to be added to the selected node?
            Dim ParentData As APSIMData = GetDataForFullPath(ParentPath)
            Dim NewData As New APSIMData("<dummy>" + XML + "</dummy>")
            Dim ok As Boolean = True
            For Each Child As APSIMData In NewData.Children
                ok = ok And Me.AllowComponentAdd(Child.Type, ParentData.Type)
            Next
            Return ok And NewData.Children.Count > 0
        End If
    End Function
    Public ReadOnly Property AllowAddFolderToSelected() As Boolean
        Get
            If MySelectedData.Count = 1 Then
                Dim SelectedType As String = GetDataForFullPath(MySelectedData(0)).Type.ToLower
                Return AllowChanges() And (SelectedType = "soils" Or SelectedType = "folder")
            Else
                Return False
            End If
        End Get
    End Property
    Public ReadOnly Property AllowMoveSelectedUp() As Boolean
        Get
            If AllowChanges AndAlso MySelectedData.Count > 0 Then
                Dim FirstSelectedData As APSIMData = GetDataForFullPath(MySelectedData(0))
                If Not FirstSelectedData.Parent Is Nothing Then
                    Dim ChildNames As StringCollection = FirstSelectedData.Parent.ChildList
                    Return FirstSelectedData.Name <> ChildNames(0) And AllSelectedNodesAreSiblings
                End If
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property AllowMoveSelectedDown() As Boolean
        Get
            If AllowChanges And MySelectedData.Count > 0 Then
                Dim LastSelectedData As APSIMData = GetDataForFullPath(MySelectedData(MySelectedData.Count - 1))
                If Not LastSelectedData.Parent Is Nothing Then
                    Dim ChildNames As StringCollection = LastSelectedData.Parent.ChildList
                    Return LastSelectedData.Name <> ChildNames(ChildNames.Count - 1) And AllSelectedNodesAreSiblings
                End If
            End If
            Return False
        End Get
    End Property

    Public Sub RenameSelected(ByVal NewName As String)
        ' Rename the node, as specified by FullPath, to NewName

        If AllowRenameSelected() Then
            Dim OldNodePath As String = MySelectedData(0)
            Dim SelectedData As APSIMData = Data
            MySelectedData.Clear()
            SelectedData.Name = NewName
            Dim NewNodePath As String = OldNodePath
            Dim PosDelimiter As String = NewNodePath.LastIndexOf("|")
            If PosDelimiter = -1 Then
                NewNodePath = NewName
            Else
                NewNodePath = NewNodePath.Substring(0, PosDelimiter + 1) + NewName
            End If
            MySelectedData.Add(NewNodePath)
            RaiseEvent NodeChangedEvent(OldNodePath, Data)
        End If
    End Sub
    Public Sub AddXMLToSelected(ByVal XML As String)
        ' Add the specified XML, as children, to the node as specified by FullPath
        If MySelectedData.Count = 1 Then
            Dim ParentData As APSIMData = Data
            Dim NewData As New APSIMData("<dummy>" + XML + "</dummy>")
            Dim NewSelections As New StringCollection
            For Each Child As APSIMData In NewData.Children
                Dim NewChildData As APSIMData = ParentData.Add(Child)
                NewSelections.Add(GetFullPathForData(NewChildData))
            Next
            RaiseEvent NodeChangedEvent(GetFullPathForData(ParentData), ParentData)
            SelectedPaths = NewSelections
        End If
    End Sub
    Public Sub Delete(ByVal FullPaths As StringCollection)
        If Me.AllowDeleteSelected Then
            Dim NewSelections As New StringCollection
            For Each Selection As String In MySelectedData
                If FullPaths.IndexOf(Selection) = -1 Then
                    NewSelections.Add(Selection)
                End If
            Next
            SelectedPaths = NewSelections

            ' Delete all nodes as specified by FullPaths
            Dim ParentData As APSIMData = Nothing
            For Each FullPath As String In FullPaths
                Dim DataToDelete As APSIMData = GetDataForFullPath(FullPath)
                ParentData = DataToDelete.Parent
                DataToDelete.Parent.Delete(DataToDelete.Name)
            Next
            RaiseEvent NodeChangedEvent(GetFullPathForData(ParentData), ParentData)
        End If
    End Sub
    Public Sub MoveSelectedUp()
        If AllowMoveSelectedUp Then
            Dim FirstSelectedData As APSIMData = GetDataForFullPath(MySelectedData(0))
            Dim ParentData As APSIMData = FirstSelectedData.Parent

            ' Move all nodes up.
            For i As Integer = 0 To MySelectedData.Count - 1
                Dim PosDelimiter As Integer = MySelectedData(i).LastIndexOf("|")
                ParentData.MoveUp(MySelectedData(i).Substring(PosDelimiter + 1), "")
            Next
            RaiseEvent NodeChangedEvent(GetFullPathForData(ParentData), ParentData)
        End If
    End Sub
    Public Sub MoveSelectedDown()
        If AllowMoveSelectedDown Then
            Dim LastSelectedData As APSIMData = GetDataForFullPath(MySelectedData(MySelectedData.Count - 1))
            Dim ParentData As APSIMData = LastSelectedData.Parent

            ' Move all nodes down
            For i As Integer = MySelectedData.Count - 1 To 0 Step -1
                Dim PosDelimiter As Integer = MySelectedData(i).LastIndexOf("|")
                ParentData.MoveDown(MySelectedData(i).Substring(PosDelimiter + 1), "")
            Next
            RaiseEvent NodeChangedEvent(GetFullPathForData(ParentData), ParentData)
        End If
    End Sub

    Private Function IsTopLevelNodeSelected() As Boolean
        Dim FoundTopLevelNode As Boolean = False
        For Each FullPath As String In MySelectedData
            FoundTopLevelNode = FoundTopLevelNode Or FullPath.IndexOf("|") = -1
        Next
        Return FoundTopLevelNode
    End Function
    Private ReadOnly Property AllSelectedNodesAreSiblings() As Boolean
        Get
            If MySelectedData.Count > 0 Then
                Dim Path As String = ""
                For Each FullPath As String In MySelectedData
                    Dim PosDelimiter As Integer = FullPath.LastIndexOf("|")
                    If PosDelimiter <> -1 Then
                        If Path = "" Then
                            Path = FullPath.Substring(0, PosDelimiter)
                        ElseIf Path = FullPath.Substring(0, PosDelimiter) Then
                            ' all ok so far - continue
                        Else
                            Return False
                        End If
                    End If
                Next
                Return True
            Else
                Return False
            End If
        End Get
    End Property

    Public Sub DataHasBeenAdded(ByVal OldDataPath As String, ByVal Node As APSIMData)
        RaiseEvent NodeChangedEvent(OldDataPath, Node)
    End Sub

    'Public Sub DataHasBeenDeleted()
    '    RaiseEvent DeleteEvent()
    'End Sub

#End Region

#Region "Keyboard hander"
    Private Function LookForShortCutOnControl(ByVal Parent As Control, ByVal KeyValue As Integer) As ToolBarButton
        For Each c As Control In Parent.Controls
            If c.GetType().ToString = "System.Windows.Forms.Panel" Then
                Dim panel As Panel = c
                Dim Button As ToolBarButton = LookForShortCutOnControl(panel, KeyValue)
                If Not Button Is Nothing Then
                    Return button
                End If
            ElseIf c.GetType().ToString = "System.Windows.Forms.ToolBar" Then
                Dim toolbar As ToolBar = c
                If Not toolbar Is Nothing And toolbar.Visible Then
                    For Each Button As ToolBarButton In toolbar.Buttons
                        Dim PosShortCut As Integer = Button.Text.IndexOf("&")
                        If PosShortCut <> -1 Then
                            Dim ShortCutKey As Integer = AscW(Button.Text.Chars(PosShortCut + 1))
                            If ShortCutKey > AscW("a") Then
                                ShortCutKey = ShortCutKey - AscW(" ")
                            End If
                            If ShortCutKey = KeyValue And Button.Enabled Then
                                Return Button
                            End If
                        End If
                    Next
                End If
            End If
        Next
        Return Nothing
    End Function

    Public Function ProcessDialogKey(ByVal Parent As Control, ByVal KeyData As Keys) As ToolBarButton
        If (KeyData And Keys.Alt) = Keys.Alt Then
            Dim Key As Integer = KeyData And Not Keys.Alt
            Return LookForShortCutOnControl(Parent, Key)
        End If
        Return Nothing
    End Function

#End Region
End Class
