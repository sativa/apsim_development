Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.io


Public MustInherit Class BaseController
    Implements IDisposable

    ' Simple base class for a user interface manager

    Private MyData As APSIMData
    Private MyFileName As String
    Private MyDefaultExtension As String
    Private MyDirtyData As Boolean
    Private MyDialogFilter As String
    Private MyFrequentListSection As String
    Private MySelectedData As New StringCollection
    Private Updating As Boolean
    Private MyIsReadOnly As Boolean
    Protected disposed As Boolean = False
    Private MyMsgBoxString As String


    Delegate Sub NotifyEventHandler()
    Delegate Sub NodeChangedEventHandler(ByVal OldNodeName As String, ByVal Node As APSIMData)
    Delegate Sub SelectionChangedHandler(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
    Delegate Sub NotifyRenameHandler(ByVal OldNodePath As String, ByVal NewNodePath As String)
    Delegate Sub RefreshRequiredHandler(ByVal Controller As BaseController)

    Public Event NewDataEvent As NotifyEventHandler                 ' Fired whenever completely new data arrives (e.g. file open)
    Public Event BeforeSaveEvent As NotifyEventHandler              ' Fired immediately before data is saved.
    Public Event AfterSaveEvent As NotifyEventHandler               ' Fired immediately after data is saved.
    Public Event SelectionChangingEvent As NotifyEventHandler       ' Fired when the current selection is about to change.
    Public Event SelectionChangedEvent As SelectionChangedHandler   ' Fired when the current selection has changed.
    Public Event RefreshRequiredEvent As RefreshRequiredHandler     ' Fired whenever a refresh of the ui is required.

#Region "Constructor / destructor"
    Sub New(ByVal DefaultExtension As String, _
            ByVal DialogFilter As String, _
            ByVal FrequentListSection As String)
        ' -----------------------
        ' constructor
        ' -----------------------
        MyDefaultExtension = DefaultExtension
        MyDialogFilter = DialogFilter
        MyFrequentListSection = FrequentListSection
        MyData = Nothing
        MyDirtyData = False
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
    MustOverride ReadOnly Property SmallImageList() As ImageList
    MustOverride Function SmallImageIndex(ByVal ComponentType As String) As Integer
    MustOverride Function ImageFileForType(ByVal ComponentType As String) As String
    MustOverride Function IsComponentVisible(ByVal Component As APSIMData) As Boolean
    MustOverride Function AllowComponentAdd(ByVal ChildComponentType As String, ByVal ParentComponentType As String) As Boolean
    MustOverride Function CreateUI(ByVal UIType As String) As BaseView
    Protected Overridable Function IsDataReadOnly() As Boolean
        Return (File.GetAttributes(FileName) And FileAttributes.ReadOnly) = FileAttributes.ReadOnly
    End Function
#End Region

#Region "Data methods"
    Public Property MsgBoxString() As String
        Get
            Return MyMsgBoxString
        End Get
        Set(ByVal value As String)
            MyMsgBoxString = value
        End Set
    End Property
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
                Return AllData.Find(MySelectedData(0))
            End If
        End Get
    End Property
    Public Property AllData() As APSIMData
        ' --------------------------------------------------------
        ' Provides readwrite access to the entire data, usually
        ' the contents of a file. This will clear the current
        ' selections.
        ' --------------------------------------------------------
        Get
            Return MyData
        End Get
        Set(ByVal Value As APSIMData)
            SelectedPaths = New StringCollection
            MyData = Value
            AddHandler MyData.DataChanged, AddressOf OnDataChanged
            RaiseEvent NewDataEvent()
        End Set
    End Property
    Private Sub OnDataChanged(ByVal DataChanged As APSIMData)
        ' --------------------------------------------------------
        ' Data has changed somewhere - flag dirty data.
        ' --------------------------------------------------------
        MyDirtyData = True
    End Sub
    Public Property DirtyData() As Boolean

        ' --------------------------------------------------------
        ' Returns true if the data needs saving i.e. it is dirty.
        ' --------------------------------------------------------
        Get
            Return MyDirtyData
        End Get
        Set(ByVal value As Boolean)
            MyDirtyData = value
        End Set
    End Property
    Public ReadOnly Property AllowDataChanges() As Boolean
        Get
            Return Not MyIsReadOnly
        End Get
    End Property
#End Region

#Region "File handling methods"
    Public ReadOnly Property FileName() As String
        ' --------------------------------------------------------
        ' Provides readonly access to the current filename
        ' --------------------------------------------------------
        Get
            Return MyFileName
        End Get
    End Property
    Public Sub FileNew(ByVal DataToUse As APSIMData)
        ' --------------------------------------------------------
        ' Create a new 'alldata' using the specified template
        ' Sets the filename to a default one.
        ' --------------------------------------------------------
        If FileSaveAfterPrompt() Then
            MyFileName = "Untitled" + MyDefaultExtension
            MyDirtyData = True
            AllData = DataToUse
            MyIsReadOnly = False
        End If
    End Sub
    Public Sub FileNew(ByVal FileName As String)
        ' --------------------------------------------------------
        ' Create a new 'alldata' using the specified filename
        ' as a template.
        ' --------------------------------------------------------
        Dim FileData As New APSIMData
        If FileData.LoadFromFile(FileName) Then
            FileNew(FileData)
        End If
    End Sub
    Public Sub FileOpen()
        ' --------------------------------------------------------
        ' Show the user a file open dialog box and if Ok is
        ' clicked - load the file.
        ' --------------------------------------------------------
        If FileSaveAfterPrompt() Then
            Dim dialog As New OpenFileDialog
            dialog.Filter = MyDialogFilter
            If dialog.ShowDialog = DialogResult.OK Then
                FileOpen(dialog.FileName)
            End If
        End If
    End Sub
    Public Function FileOpen(ByVal FileName As String) As Boolean
        ' --------------------------------------------------------
        ' Open the specified file.
        ' --------------------------------------------------------
        Dim FileData As New APSIMData
        If FileData.LoadFromFile(FileName) Then
            MyFileName = FileName
            MyIsReadOnly = IsDataReadOnly()
            If Not MyIsReadOnly Then
                Directory.SetCurrentDirectory(Path.GetDirectoryName(FileName))
            End If
            AddFileToFrequentList(MyFileName)
            MyDirtyData = False
            AllData = FileData
            Return True
        Else
            Return False
        End If
    End Function
    Function FileSave() As Boolean
        ' --------------------------------------------------------
        ' Save the current data to the current filename
        ' --------------------------------------------------------
        RaiseEvent BeforeSaveEvent()

        If Not IsNothing(MyFileName) Then
            If MyFileName.IndexOf("Untitled.") <> -1 Then
                Return FileSaveAs()
            Else
                AllData.SaveToFile(MyFileName)
                AddFileToFrequentList(MyFileName)
                MyDirtyData = False
                MyIsReadOnly = False
                RaiseEvent AfterSaveEvent()
                Return True
            End If
        End If
    End Function
    Function FileSaveAs() As Boolean
        ' --------------------------------------------------------
        ' Show the user a FileSave dialog and if Ok is pressed
        ' save the current data to the selected file.
        ' --------------------------------------------------------
        Dim dialog As New SaveFileDialog
        With dialog
            .Filter = MyDialogFilter
            .AddExtension = True
            .OverwritePrompt = True
        End With
        If dialog.ShowDialog = DialogResult.OK Then
            MyFileName = dialog.FileName
            If FileSave() Then
                MyIsReadOnly = False
                Return True
            End If
        Else
            ' User has cancelled - do nothing
            Return False
        End If
    End Function
    Public Function FileSaveAfterPrompt() As Boolean
        ' --------------------------------------------------------
        ' Often called at program exit to optionally prompt the
        ' user to save the current data if something has changed.
        ' --------------------------------------------------------
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
        End Set
    End Property
#End Region

#Region "Clipboard methods"
    ReadOnly Property AllowCut() As Boolean
        Get
            ' --------------------------------------------------------
            ' Allow a clipboard cut operation?
            ' --------------------------------------------------------
            Return AllowDeleteSelected()
        End Get
    End Property
    ReadOnly Property AllowCopy() As Boolean
        Get
            ' --------------------------------------------------------
            ' Allow a clipboard copy operation?
            ' --------------------------------------------------------
            Return MySelectedData.Count > 0
        End Get
    End Property
    ReadOnly Property AllowPaste() As Boolean
        Get
            ' --------------------------------------------------------
            ' Allow a clipboard paste operation?
            ' --------------------------------------------------------
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
        ' --------------------------------------------------------
        ' Perform a clipboard cut operation
        ' --------------------------------------------------------

        'Firstly check if any of the selections are the root node and if so
        ' do nothing
        Dim Selections As StringCollection = MySelectedData
        SelectedPaths = New StringCollection()
        Dim DeletingRoot As Boolean = False
        For Each FullPath As String In Selections
            Dim NodeToDelete As APSIMData = AllData.Find(FullPath)
            ' Make sure we do not delete the root node
            If Not IsNothing(NodeToDelete.Parent) Then
                DeletingRoot = True
                Exit For
            End If
        Next

        If AllowCut() And Not DeletingRoot Then
            Copy()
            DeleteSelected()
        End If
    End Sub
    Public Sub Copy()
        ' --------------------------------------------------------
        ' Perform a clipboard copy operation
        ' --------------------------------------------------------
        If AllowCopy Then
            Dim Contents As String = ""
            For Each FullPath As String In MySelectedData
                Contents = Contents + AllData.Find(FullPath).XML + "\r\n"
            Next
            Clipboard.SetDataObject(Contents, True)
        End If
    End Sub
    Public Sub Paste()
        ' --------------------------------------------------------
        ' Perform a clipboard paste operation
        ' --------------------------------------------------------
        If AllowPaste() Then
            Dim iData As IDataObject = Clipboard.GetDataObject()
            Dim xml As String = CType(iData.GetData(DataFormats.Text), String)
            AddXMLToSelected(xml)
        End If
    End Sub
#End Region

#Region "High level data manipulation methods"
    Public Function AllowRenameSelected() As Boolean
        ' --------------------------------------------------------------
        ' Do we allow the selected node to be renamed?
        ' --------------------------------------------------------------
        Return AllowDataChanges() AndAlso MySelectedData.Count = 1 AndAlso Data.Name.ToLower() <> "shared" = -1
    End Function
    Public ReadOnly Property AllowDeleteSelected() As Boolean
        Get
            ' --------------------------------------------------------------
            ' Do we allow the selected node to be deleted?
            ' --------------------------------------------------------------
            Return AllowDataChanges() AndAlso MySelectedData.Count >= 1 AndAlso MySelectedData(0).IndexOf("\") <> -1
        End Get
    End Property
    Public Function AllowAddXMLToData(ByVal XML As String, ByVal ParentPath As String) As Boolean
        ' --------------------------------------------------------------
        ' Do we allow the specified XML to be added to the specified
        ' parent node?
        ' --------------------------------------------------------------
        If Not AllowDataChanges Then
            Return False
        Else
            ' Do we allow the specified xml to be added to the selected node?
            Dim ParentData As APSIMData = AllData.Find(ParentPath)
            Dim NewData As New APSIMData("<dummy>" + XML + "</dummy>")
            Dim ok As Boolean = True
            For Each Child As APSIMData In NewData.Children
                ok = ok And Me.AllowComponentAdd(Child.Type, ParentData.Type)
            Next
            Return ok And NewData.Children.Length > 0
        End If
    End Function
    Public ReadOnly Property AllowMoveSelectedUp() As Boolean
        Get
            ' --------------------------------------------------------------
            ' Do we allow user to move selected items up?
            ' --------------------------------------------------------------
            If AllowDataChanges AndAlso MySelectedData.Count > 0 Then
                Dim FirstSelectedData As APSIMData = AllData.Find(MySelectedData(0))
                If Not FirstSelectedData.Parent Is Nothing Then
                    Dim ChildNames() As String = FirstSelectedData.Parent.ChildNames
                    Return FirstSelectedData.Name <> ChildNames(0) And AllSelectedNodesAreSiblings
                End If
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property AllowMoveSelectedDown() As Boolean
        Get
            ' --------------------------------------------------------------
            ' Do we allow user to move selected items down?
            ' --------------------------------------------------------------
            If AllowDataChanges And MySelectedData.Count > 0 Then
                Dim LastSelectedData As APSIMData = AllData.Find(MySelectedData(MySelectedData.Count - 1))
                If Not LastSelectedData.Parent Is Nothing Then
                    Dim ChildNames() As String = LastSelectedData.Parent.ChildNames
                    Return LastSelectedData.Name <> ChildNames(ChildNames.Length - 1) And AllSelectedNodesAreSiblings
                End If
            End If
            Return False
        End Get
    End Property

    Public Sub AddXMLToSelected(ByVal XML As String)
        ' --------------------------------------------------------------
        ' Add the specified XML, as children, to the selected node.
        ' --------------------------------------------------------------
        If MySelectedData.Count = 1 Then
            Dim ParentData As APSIMData = Data
            ParentData.BeginUpdate()
            Dim NewData As New APSIMData("<dummy>" + XML + "</dummy>")
            For Each Child As APSIMData In NewData.Children
                ParentData.Add(Child).EnsureNameIsUnique()
            Next
            ParentData.EndUpdate()
            RefreshView()
        End If
    End Sub
    Public Sub DeleteSelected()
        ' ---------------------------------------------
        ' Delete all selected nodes.
        ' ---------------------------------------------
        Dim Selections As StringCollection = MySelectedData
        SelectedPaths = New StringCollection()
        For Each FullPath As String In Selections
            Dim NodeToDelete As APSIMData = AllData.Find(FullPath)
            ' Make sure we do not delete the root node
            If Not IsNothing(NodeToDelete.Parent) Then
                NodeToDelete.Parent.Delete(NodeToDelete.Name)
            End If
        Next
        RefreshView()
    End Sub
    Public Sub MoveSelectedUp()
        ' --------------------------------------------------------------
        ' Move all selected items up
        ' --------------------------------------------------------------
        If AllowMoveSelectedUp Then
            Dim FirstSelectedData As APSIMData = AllData.Find(MySelectedData(0))
            Dim ParentData As APSIMData = FirstSelectedData.Parent
            ParentData.BeginUpdate()
            For i As Integer = 0 To MySelectedData.Count - 1
                Dim PosDelimiter As Integer = MySelectedData(i).LastIndexOf("\")
                ParentData.MoveUp(MySelectedData(i).Substring(PosDelimiter + 1), "")
            Next
            ParentData.EndUpdate()
            RefreshView()
        End If
    End Sub
    Public Sub MoveSelectedDown()
        ' --------------------------------------------------------------
        ' Move all selected items down
        ' --------------------------------------------------------------
        If AllowMoveSelectedDown Then
            Dim LastSelectedData As APSIMData = AllData.Find(MySelectedData(MySelectedData.Count - 1))
            Dim ParentData As APSIMData = LastSelectedData.Parent
            ParentData.BeginUpdate()
            For i As Integer = MySelectedData.Count - 1 To 0 Step -1
                Dim PosDelimiter As Integer = MySelectedData(i).LastIndexOf("\")
                ParentData.MoveDown(MySelectedData(i).Substring(PosDelimiter + 1), "")
            Next
            ParentData.EndUpdate()
            RefreshView()
        End If
    End Sub
    Public Sub RefreshView()
        RaiseEvent RefreshRequiredEvent(Me)
    End Sub
    Private ReadOnly Property AllSelectedNodesAreSiblings() As Boolean
        Get
            ' --------------------------------------------------------------
            ' Return true if all selected nodes are siblings.
            ' --------------------------------------------------------------
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
                    Me.MsgBoxString = "The " + Prop.Type + " selected is after the dates in the Met file." & vbCrLf & _
                                "Automatically adjusting simulation end date to match the Met file end date"
                    DateEditor.MaximumDate = ubound
                End If
            End If
            If (Prop.Attribute("lbound")) <> "" Then
                lbound = Prop.Attribute("lbound")
                If dtValue < lbound Then
                    Prop.Value = Prop.Attribute("lbound")
                    DateEditor.DateDefault = Prop.Value
                    Me.MsgBoxString = "The " + Prop.Type + " selected is before the dates in the Met file." & vbCrLf & _
                                "Automatically adjusting simulation start date to match the Met file start date"
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

        ElseIf Prop.Attribute("type") = "filenames" Or Prop.Attribute("type") = "filename" Then
            If Prop.Attribute("type") = "filenames" Then
                Dim Text As FarPoint.Win.Spread.CellType.TextCellType = New FarPoint.Win.Spread.CellType.TextCellType
                Text.Multiline = True
                Text.MaxLength = 65536
                Text.ScrollBars = ScrollBars.Both
                Grid.Cells(Row, 1).CellType = Text
            End If

            Grid.Columns(2).Visible = True
            Grid.Cells(Row, 2).Locked = False
            Dim Button As FarPoint.Win.Spread.CellType.ButtonCellType = New FarPoint.Win.Spread.CellType.ButtonCellType
            Button.Picture = My.Resources.folder
            Grid.Cells(Row, 2).CellType = Button

        ElseIf Prop.Attribute("type") = "multiedit" Then
            Dim Text As FarPoint.Win.Spread.CellType.TextCellType = New FarPoint.Win.Spread.CellType.TextCellType
            Text.Multiline = True
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

End Class
