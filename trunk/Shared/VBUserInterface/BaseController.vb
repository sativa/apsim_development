Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.io
Imports VBGeneral
Imports CSGeneral
Imports System.Reflection
Imports System.Xml

Public Class BaseController
    ' Simple base class for a user interface manager
    Private MySelectedData As New StringCollection
    Private ActionFile As XmlNode
    Private MyExplorer As ExplorerUI
    Private MyMainForm As Form

    Public ApsimData As ApsimFile.ApsimFile
    Public Configuration As ApsimFile.Configuration

    Delegate Sub NotifyEventHandler()
    Delegate Sub SelectionChangedHandler(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)

    Public Event BeforeSaveEvent As NotifyEventHandler              ' Fired immediately before data is saved.
    Public Event AfterSaveEvent As NotifyEventHandler               ' Fired immediately after data is saved.
    Public Event SelectionChangingEvent As NotifyEventHandler       ' Fired when the current selection is about to change.
    Public Event SelectionChangedEvent As SelectionChangedHandler   ' Fired when the current selection has changed.


    Sub New(ByVal MainForm As Form, ByVal SectionName As String, ByVal MainController As Boolean)
        ' -----------------------
        ' constructor
        ' -----------------------
        MyMainForm = MainForm

        Configuration = New ApsimFile.Configuration(SectionName)
        ApsimData = New ApsimFile.ApsimFile(Configuration)

        ' Setup the actions file and load all images specified by it.
        Dim ActionFileName As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), SectionName, "ActionFile")
        If ActionFileName <> "" Then
            Dim ActionDoc As New XmlDocument()
            ActionDoc.Load(ActionFileName)
            ActionFile = ActionDoc.DocumentElement
            Configuration.LoadAllImages(XmlHelper.Find(ActionDoc.DocumentElement, "Actions"))
        End If
        If MainController Then
            AddHandler ApsimData.FileNameChanged, AddressOf OnFileNameChanged
        End If
    End Sub
    Public Function CreateUI(ByVal ComponentType As String) As BaseView
        ' -------------------------------------
        ' Create a User interface form for the
        ' specified type.
        ' -------------------------------------
        Dim UIType As String = Configuration.Info(ComponentType, "UItype")
        If UIType <> "" Then
            Return CreateClass(UIType)
        End If
        Return Nothing
    End Function

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


#Region "File handling methods"
    Public Function FileSaveAfterPrompt() As Boolean
        ' --------------------------------------------------------
        ' Often called at program exit to optionally prompt the
        ' user to save the current data if something has changed.
        ' --------------------------------------------------------
        Explorer.SaveCurrentView()
        If ApsimData.IsDirty Then
            Dim DoSave As Integer = MessageBox.Show("The current file has changed. Do you want to save it before proceeding?", _
                                                    "Save?", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)
            Select Case DoSave
                Case DialogResult.Yes
                    ' Save the file
                    ApsimData.Save()

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
        For Each PreviousFileName As String In Configuration.GetFrequentList()
            If File.Exists(PreviousFileName) Then
                ApsimData.OpenFile(PreviousFileName)
                Exit For
            End If
        Next
    End Sub
    Private Sub OnFileNameChanged(ByVal FileName As String)
        If FileName.ToLower <> "untitled" Then
            Configuration.AddFileToFrequentList(FileName)
        End If
    End Sub
#End Region

#Region "Node selection methods. NB: Paths are delimited with '/' characters"
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

            ' Has anything changed?
            Dim Different As Boolean = OldSelections.Count <> Paths.Count
            If Not Different Then
                For i As Integer = 0 To OldSelections.Count - 1
                    Different = Different Or (OldSelections(i) <> Paths(i))
                Next
            End If

            ' Only flag the change is something is actually different.
            If Different Then
                RaiseEvent SelectionChangingEvent()
                MySelectedData = Paths
                RaiseEvent SelectionChangedEvent(OldSelections, MySelectedData)
                RefreshToolStrips()
            End If
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
    Public ReadOnly Property Selection() As ApsimFile.Component
        Get
            Return ApsimData.Find(SelectedPath)
        End Get
    End Property
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
        Dim ToolStripDescriptor As XmlNode = XmlHelper.Find(ActionFile, ToolStripName)
        If Not IsNothing(ToolStripDescriptor) Then
            Dim ImageAboveText As Boolean = False
            ImageAboveText = (XmlHelper.Attribute(ToolStripDescriptor, "ImageAboveText") = "yes")
            PopulateToolStrip(Strip, ToolStripDescriptor, ImageAboveText)
        End If
        ToolStrips.Add(Strip)
    End Sub
    Public Sub RemoveToolStrip(ByVal Strip As ToolStrip)
        ToolStrips.Remove(Strip)
    End Sub

    Private Sub PopulateToolStrip(ByVal Strip As ToolStrip, ByVal ToolStripDescriptor As XmlNode, ByVal ImageAboveText As Boolean)
        ' --------------------------------------------------------------
        ' This method populates the specified context menu given the
        ' specified descriptor data. 
        ' --------------------------------------------------------------

        For Each ToolDescriptor As XmlNode In XmlHelper.ChildNodes(ToolStripDescriptor, "")
            If XmlHelper.Type(ToolDescriptor) = "ImageSize" Then
                Strip.ImageList = Configuration.ImageList(ToolDescriptor.InnerText)

            ElseIf XmlHelper.Type(ToolDescriptor) = "Item" Then
                Dim Item As ToolStripItem = CreateToolStripItem(Strip, ToolDescriptor.InnerText)
                If ImageAboveText Then
                    Item.TextImageRelation = TextImageRelation.ImageAboveText
                End If

            ElseIf XmlHelper.Type(ToolDescriptor) = "DropDownItem" Then
                Dim DropDownActionName As String = XmlHelper.Attribute(ToolDescriptor, "action")
                Dim Action As XmlNode = XmlHelper.Find(ActionFile, "/Folder/Actions/" + DropDownActionName)
                Dim DropDownButton As ToolStripDropDownItem
                Dim DropDownStrip As ToolStripDropDown
                If Strip.GetType().ToString = "System.Windows.Forms.ToolStrip" Then
                    DropDownButton = New ToolStripDropDownButton
                    DropDownStrip = New ToolStripDropDownMenu
                Else
                    DropDownButton = New ToolStripMenuItem
                    DropDownStrip = New ToolStripDropDownMenu
                End If
                DropDownButton.Text = XmlHelper.Value(Action, "text")
                DropDownButton.ImageIndex = Configuration.ImageIndex(Action, Strip.ImageList.Tag.ToString)
                DropDownButton.ImageScaling = ToolStripItemImageScaling.None
                DropDownButton.AutoToolTip = False
                DropDownButton.Tag = DropDownActionName
                DropDownButton.Enabled = IsActionAllowed(Action)

                If ImageAboveText Then
                    DropDownButton.TextImageRelation = TextImageRelation.ImageAboveText
                End If

                DropDownStrip.ImageList = Configuration.ImageList("SmallIcon")
                AddHandler DropDownStrip.ItemClicked, AddressOf ActionOnClick
                PopulateToolStrip(DropDownStrip, ToolDescriptor, False)  ' recursion

                DropDownButton.DropDown = DropDownStrip
                Strip.Items.Add(DropDownButton)

            ElseIf XmlHelper.Type(ToolDescriptor) = "Separator" Then
                Strip.Items.Add(New ToolStripSeparator)

            ElseIf XmlHelper.Type(ToolDescriptor) = "RecentFileList" Then
                For Each FileName As String In Configuration.GetFrequentList()
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
        Dim Action As XmlNode = XmlHelper.Find(ActionFile, "/Folder/Actions/" + ActionName)
        If Not IsNothing(Action) Then
            Dim Item As ToolStripItem = Strip.Items.Add(XmlHelper.Value(Action, "text"))
            Item.ImageIndex = Configuration.ImageIndex(Action, Strip.ImageList.Tag.ToString)
            Item.ToolTipText = XmlHelper.Value(Action, "description")
            Item.Tag = ActionName
            Item.ImageScaling = ToolStripItemImageScaling.None
            Dim ShortCut As String = XmlHelper.Value(Action, "shortcut")
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
                ToolItem.Enabled = IsActionAllowed(XmlHelper.Find(ActionFile, "/Folder/Actions/" + ToolItem.Tag.ToString))
            End If
            If TypeOf ToolItem Is ToolStripDropDownItem Then
                Dim DropDownItem As ToolStripDropDownItem = ToolItem
                ToolItem.Enabled = IsActionAllowed(XmlHelper.Find(ActionFile, "/Folder/Actions/" + ToolItem.Tag.ToString))
                EnableActions(DropDownItem.DropDown)
            End If
        Next
    End Sub
    Private Function IsActionAllowed(ByVal Action As XmlNode) As Boolean
        Dim Allowed As Boolean = True
        If Not IsNothing(Action) Then
            For Each DisabledWhen As XmlNode In XmlHelper.ChildNodes(Action, "DisabledWhen")
                Dim DisabledWhenFlag As String = DisabledWhen.InnerText
                If DisabledWhenFlag = "ReadOnly" AndAlso ApsimData.IsReadOnly Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "RootNode" AndAlso MySelectedData.Count > 0 AndAlso MySelectedData(0).LastIndexOf("/") = 0 Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "MultipleNodesSelected" AndAlso MySelectedData.Count > 1 Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "NothingLoaded" AndAlso ApsimData.FileName = "Untitled" Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "NotShortcut" AndAlso MySelectedData.Count = 1 AndAlso Selection.ShortCutTo Is Nothing Then
                    Allowed = False
                End If
                If Not IsNothing(XmlHelper.Find(DisabledWhen, "call")) Then
                    Dim Arguments As New List(Of Object)
                    Arguments.Add(Me)
                    Allowed = Not CallDll.CallMethodOfClass(XmlHelper.Find(DisabledWhen, "call"), Arguments)
                End If
            Next

            If Allowed And MySelectedData.Count = 1 And XmlHelper.ChildNodes(Action, "AppliesTo").Count > 0 Then
                Allowed = False
                Dim SelectedType As String = Selection.Type
                For Each AppliesTo As XmlNode In XmlHelper.ChildNodes(Action, "AppliesTo")
                    If AppliesTo.InnerText.ToLower = SelectedType.ToLower Then
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
                ApsimData.OpenFile(E.ClickedItem.Text)
            Else
                InvokeAction(Sender, E.ClickedItem.Tag)
            End If
        End If
    End Sub
    Private Sub InvokeAction(ByVal Sender As Object, ByVal ActionName As String)
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

        Dim ActionInvoke As XmlNode = XmlHelper.Find(ActionFile, "/Folder/Actions/" + ActionName + "/OnInvoke/Call")
        If Not IsNothing(ActionInvoke) Then
            Dim Arguments As New List(Of Object)
            Arguments.Add(Me)
            CallDll.CallMethodOfClass(ActionInvoke, Arguments)
        End If
    End Sub
    Public Shared Function CreateClass(ByVal ClassToCall As String) As Object
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
