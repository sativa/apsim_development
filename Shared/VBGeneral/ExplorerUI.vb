Imports System.Windows.Forms
Imports System.Collections.Specialized
Imports System.IO
Imports VBGeneral
Public Class ExplorerUI
    Inherits BaseUI
    Private MyFileName As String
    Private MyApplicationSettings As ApplicationSettings
    Private CurrentUI As BaseUI
    Private MyDialogFilter As String
    Private MyExtension As String
    Private FrequentListSection As String
    Private MyDataHasChanged As Boolean
    Private MyParentForm As Form
    Private BaseName As String

    Public Event DataSelectedEvent As DataTree.DataSelectedEventHandler




#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.
    'Do not modify it using the code editor.
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents UIPanel As System.Windows.Forms.Panel
    Friend WithEvents DataTree As VBGeneral.DataTree
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.DataTree = New VBGeneral.DataTree
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.UIPanel = New System.Windows.Forms.Panel
        Me.SuspendLayout()
        '
        'DataTree
        '
        Me.DataTree.AllowDrop = True
        Me.DataTree.ApplicationSettings = Nothing
        Me.DataTree.Data = Nothing
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Left
        Me.DataTree.LabelEdit = True
        Me.DataTree.Location = New System.Drawing.Point(0, 20)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(256, 733)
        Me.DataTree.Sorted = False
        Me.DataTree.TabIndex = 3
        '
        'Splitter1
        '
        Me.Splitter1.Location = New System.Drawing.Point(256, 20)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(5, 733)
        Me.Splitter1.TabIndex = 4
        Me.Splitter1.TabStop = False
        '
        'UIPanel
        '
        Me.UIPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.UIPanel.Location = New System.Drawing.Point(261, 20)
        Me.UIPanel.Name = "UIPanel"
        Me.UIPanel.Size = New System.Drawing.Size(492, 733)
        Me.UIPanel.TabIndex = 5
        '
        'ExplorerUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(753, 788)
        Me.Controls.Add(Me.UIPanel)
        Me.Controls.Add(Me.Splitter1)
        Me.Controls.Add(Me.DataTree)
        Me.Name = "ExplorerUI"
        Me.Text = "explorerui"
        Me.Controls.SetChildIndex(Me.DataTree, 0)
        Me.Controls.SetChildIndex(Me.Splitter1, 0)
        Me.Controls.SetChildIndex(Me.UIPanel, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' --------------------------------------
    ' property for setting the explorer ui
    ' --------------------------------------
    Public Sub Setup(ByVal ParentForm As Form, _
                     ByVal DialogFilter As String, _
                     ByVal Extension As String, ByVal FrequentListSectionName As String)
        MyParentForm = ParentForm
        If Not IsNothing(MyParentForm) Then
            BaseName = MyParentForm.Text
        End If

        MyDialogFilter = DialogFilter
        MyExtension = Extension
        MyFileName = "Untitled" + Extension
        Me.FrequentListSection = FrequentListSectionName
        MyCaptionLabel.Visible = False
        MyHelpLabel.Visible = False
        MyDataHasChanged = False
    End Sub


    ' ------------------------------
    ' Provide access to our file.
    ' ------------------------------
    ReadOnly Property FileName() As String
        Get
            Return MyFileName
        End Get
    End Property


    ' ----------------------------------------------
    ' Provide access to our datahaschanged property
    ' ----------------------------------------------
    Public Property DataHasChanged() As Boolean
        Get
            Return MyDataHasChanged
        End Get
        Set(ByVal Value As Boolean)
            MyDataHasChanged = Value
        End Set
    End Property


    ' ------------------------------------------------
    ' Called by ExplorerUI to setup the ParentExplorer
    ' ------------------------------------------------
    Overrides Property Data() As APSIMData
        Get
            Return MyBase.Data
        End Get
        Set(ByVal Value As APSIMData)
            Try
                MyBase.Data = Value
                DataTree.Data = MyBase.Data
                CloseUI()
                'MyDataHasChanged = True
            Catch Err As System.Exception
                MsgBox(Err.Message, MsgBoxStyle.Critical, "Error")
            End Try
        End Set
    End Property


    ' -------------------------------------------
    ' Property allowing setting of uimanager.
    ' -------------------------------------------
    Property ApplicationSettings() As ApplicationSettings
        Set(ByVal Value As ApplicationSettings)
            MyApplicationSettings = Value
            DataTree.ApplicationSettings = MyApplicationSettings
        End Set
        Get
            Return MyApplicationSettings
        End Get
    End Property


    ' ------------------------------------------------------
    ' Set the expandAll property
    ' ------------------------------------------------------
    WriteOnly Property ExpandAll() As Boolean
        Set(ByVal Value As Boolean)
            DataTree.ExpandAll = Value
        End Set
    End Property



    ' -------------------------------------------
    ' Refresh ourselves.
    ' -------------------------------------------
    Overrides Sub Refresh()
        DataTree.Refresh()
        If Not IsNothing(CurrentUI) Then
            CurrentUI.Refresh()
        End If
    End Sub


    ' -------------------------------------------------
    ' Create and show a specific UI depending on the
    ' specified user interface type.
    ' -------------------------------------------------
    Sub ShowUI(ByVal Data As APSIMData)
        Dim UI As BaseUI = MyApplicationSettings.CreateUI(Data.Type)
        If Not IsNothing(UI) Then
            UI.Explorer = Me
            UI.Data = Data
            ShowUI(UI)
        Else
            CloseUI()
        End If
    End Sub


    ' -------------------------------------------------
    ' Show a specific UI.
    ' -------------------------------------------------
    Sub ShowUI(ByVal UserInterface As BaseUI)
        CloseUI()
        UserInterface.Explorer = Me
        UserInterface.TopLevel = False
        UserInterface.Parent = UIPanel
        UserInterface.Dock = DockStyle.Fill
        UserInterface.Show()
        CurrentUI = UserInterface
        ' force a resize so that soilui is shown properly
        UserInterface.Width = UserInterface.Width + 1
        UserInterface.Width = UserInterface.Width - 1
    End Sub


    ' -------------------------------------------------
    ' Close the current UI
    ' -------------------------------------------------
    Sub CloseUI()
        If CurrentUI Is Nothing Then
            'Dont need to do anthing here
        Else
            CurrentUI.Save()
            CurrentUI.Close()
            CurrentUI = Nothing
        End If
    End Sub


    ' -------------------------------------
    ' Create a new document in memory only.
    ' Uses the specified data as a template.
    ' -------------------------------------
    Public Sub FileNew(ByVal DataToUse As APSIMData)
        Data = DataToUse
        MyFileName = "Untitled" + MyExtension
        UpdateCaption()
        'MyDataHasChanged = True
    End Sub


    ' -------------------------------------
    ' Create a new document in memory only.
    ' Uses the specified filename as a template.
    ' -------------------------------------
    Public Sub FileNew(ByVal FileName As String)
        If FileOpen(FileName) Then
            MyFileName = "Untitled" + MyExtension
            UpdateCaption()
            'MyDataHasChanged = True
        End If
    End Sub


    ' ------------------------------------------------
    ' Called to open a new file.
    ' ------------------------------------------------
    Public Function FileOpen(ByVal FileName As String) As Boolean
        Dim FileData As New APSIMData
        If FileData.LoadFromFile(FileName) Then
            Data = FileData
            MyFileName = FileName
            UpdateCaption()
            AddFileToFrequentList(MyFileName)
            MyDataHasChanged = False
            Return True
        Else
            Return False
        End If
    End Function


    ' ------------------------------------------------
    ' Called when the user clicks on the File | Open
    ' menu item. Returns true if a new file was loaded.
    ' ------------------------------------------------
    Public Function FileOpen() As Boolean
        If DoSaveAfterPrompt() Then
            Dim dialog As New OpenFileDialog
            With dialog
                .Filter = MyDialogFilter
                .AddExtension = True
            End With
            Dim choice As DialogResult = dialog.ShowDialog
            If choice = DialogResult.OK Then
                Return FileOpen(dialog.FileName)
            Else
                Return False
            End If
        End If

    End Function


    ' ----------------
    ' Save everything.
    ' ----------------
    Function FileSave() As Boolean
        MyBase.Save()
        If Not IsNothing(CurrentUI) Then
            CurrentUI.Save()
        End If

        If MyFileName.IndexOf("Untitled.") <> -1 Then
            Return FileSaveAs()
        Else
            Data.SaveToFile(MyFileName)
            UpdateCaption()
            AddFileToFrequentList(MyFileName)
            MyDataHasChanged = False
            Return True
        End If
    End Function


    ' ------------------------------
    ' Save everything under new name
    ' ------------------------------
    Function FileSaveAs() As Boolean
        MyBase.Save()

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


    ' ----------------------------------------
    ' Called to update the main form's caption
    ' ----------------------------------------
    Private Sub UpdateCaption()
        If Not IsNothing(MyParentForm) Then
            If MyDataHasChanged Then
                MyParentForm.Text = BaseName + " - " + MyFileName + "*"
            Else
                MyParentForm.Text = BaseName + " - " + MyFileName
            End If
        End If
    End Sub


    ' -----------------------------------------
    ' Ask user to save work. Return true if 
    ' user is happy or false if user pressed
    ' cancel.
    ' -----------------------------------------
    Private Function DoSaveAfterPrompt() As Boolean
        If MyDataHasChanged Then
            Dim DoSave As Integer = MsgBox("Do you want to save your work?", MsgBoxStyle.YesNoCancel, "Question")
            Select Case DoSave
                Case MsgBoxResult.Yes
                    ' Save the file
                    FileSave()

                Case MsgBoxResult.No
                    ' Do not save

                Case MsgBoxResult.Cancel
                    ' Cancel pressed.
                    Return False
            End Select
        End If
        Return True
    End Function


    Const MAX_NUM_FREQUENT_SIMS As Integer = 10
    ' ------------------------------------------------------
    ' Add a file to the frequently accessed simulation list.
    ' ------------------------------------------------------
    Public Sub AddFileToFrequentList(ByVal filename As String)
        If FrequentListSection <> "" Then

            Dim FileNames() As String = GetFrequentList()

            ' Put this filename into the top of the NewFileList and
            ' then copy all existing filenames to NewFileList
            Dim NewFileList(MAX_NUM_FREQUENT_SIMS) As String
            NewFileList(0) = filename
            Dim NewFileListIndex As Integer = 1
            For i As Integer = 0 To FileNames.Length - 1
                If FileNames(i) <> filename And FileNames(i) <> "" Then
                    NewFileList(NewFileListIndex) = FileNames(i)
                    NewFileListIndex = NewFileListIndex + 1
                End If
            Next

            ' Write NewFileList back to .ini file.
            APSIMSettings.INIWriteMultiple(APSIMSettings.ApsimIniFile(), FrequentListSection, "RecentFile", NewFileList)
        End If
    End Sub


    ' ------------------------------------------------------
    ' Return a list of frequently accessed simulations
    ' ------------------------------------------------------
    Public Function GetFrequentList() As String()
        Dim FileNames As StringCollection = APSIMSettings.INIReadMultiple(APSIMSettings.ApsimIniFile(), FrequentListSection, "RecentFile")
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


    ' -----------------------------------------------------
    ' User has selected a node - update user interface
    ' -----------------------------------------------------
    Public Sub OnDataSelected(ByVal e As VBGeneral.APSIMData) Handles DataTree.DataSelectedEvent
        ShowUI(e)
        RaiseEvent DataSelectedEvent(e)
    End Sub


    ' -----------------------------------------
    ' Allow users to set the data tree caption
    ' -----------------------------------------
    WriteOnly Property DataTreeCaption() As String
        Set(ByVal Value As String)
            DataTree.CaptionLabel.Text = Value
        End Set
    End Property


    ' -----------------------------------
    ' Return the currently selected soil.
    ' -----------------------------------
    Public Function GetSelectedData() As APSIMData
        Return DataTree.SelectedNode
    End Function


    ' ---------------------------------
    ' Select a node in the data tree.
    ' ---------------------------------
    Public Sub SelectNode(ByVal ParentNode As TreeNode, ByVal NodeNameToSelect As String)
        For Each Node As TreeNode In ParentNode.Nodes
            If Node.Text.ToLower() = NodeNameToSelect Then
                DataTree.SelectNode(Node)
                Return
            ElseIf Node.GetNodeCount(False) > 0 Then
                SelectNode(Node, NodeNameToSelect)
            End If
        Next
    End Sub


    ' ---------------------------------
    ' Select a node in the data tree.
    ' ---------------------------------
    Public Sub SelectFirstNodeOfType(ByVal NodeType As String)
        FindFirstNodeWithUI(DataTree.Nodes(0), NodeType)
    End Sub


    ' ---------------------------------
    ' Select a node in the data tree.
    ' ---------------------------------
    Public Sub FindFirstNodeWithUI(ByVal ParentNode As TreeNode, ByVal NodeType As String)
        For Each Node As TreeNode In ParentNode.Nodes
            Dim Data As APSIMData = DataTree.GetDataForFullPath(Node.FullPath)
            If IsNothing(CurrentUI) And Data.Type.ToLower() = NodeType.ToLower() Then
                DataTree.SelectNode(Node)
            Else
                For Each Child As TreeNode In DataTree.Nodes
                    If CurrentUI Is Nothing Then
                        FindFirstNodeWithUI(Child, NodeType)
                    End If
                Next
            End If
        Next
    End Sub
End Class
