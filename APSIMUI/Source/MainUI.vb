Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.xml
Imports System.xml.XmlNodeList
Imports System.io


Public Class MainUI
    Inherits System.Windows.Forms.Form
    Private MainUImanager As UIManager
    Private CurrentAPSIMFile As APSIMFile
    'Private Toolbox As New OutlookBar


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
    Friend WithEvents MainMenu1 As System.Windows.Forms.MainMenu
    Friend WithEvents MenuItem4 As System.Windows.Forms.MenuItem
    Friend WithEvents StatusBar1 As System.Windows.Forms.StatusBar
    Friend WithEvents ToolBar1 As System.Windows.Forms.ToolBar
    Friend WithEvents ImageList1 As System.Windows.Forms.ImageList
    Friend WithEvents ButtonImageList As System.Windows.Forms.ImageList
    Friend WithEvents FileNewButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents FileOpenButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents FileSaveButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents Separator1 As System.Windows.Forms.ToolBarButton
    Friend WithEvents Separator2 As System.Windows.Forms.ToolBarButton
    Friend WithEvents RunButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents PageSetupDialog1 As System.Windows.Forms.PageSetupDialog
    Friend WithEvents SimulationExplorer As System.Windows.Forms.TreeView
    Friend WithEvents ContextMenu1 As System.Windows.Forms.ContextMenu
    Friend WithEvents TabMenuClose As System.Windows.Forms.MenuItem
    Friend WithEvents TabControl2 As System.Windows.Forms.TabControl
    Friend WithEvents FileMenuSave As System.Windows.Forms.MenuItem
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents TabContextMenu As System.Windows.Forms.ContextMenu
    Friend WithEvents TabContextMenuHelp As System.Windows.Forms.MenuItem
    Friend WithEvents TabContextMenuClose As System.Windows.Forms.MenuItem
    Friend WithEvents FileMenu As System.Windows.Forms.MenuItem
    Friend WithEvents FileMenuNew As System.Windows.Forms.MenuItem
    Friend WithEvents FileMenuOpen As System.Windows.Forms.MenuItem
    Friend WithEvents FileMenuExit As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem1 As System.Windows.Forms.MenuItem
    Friend WithEvents HelpMenu As System.Windows.Forms.MenuItem
    Friend WithEvents HelpMenuAbout As System.Windows.Forms.MenuItem
    Friend WithEvents ViewMenuOptions As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem3 As System.Windows.Forms.MenuItem
    Friend WithEvents EditMenu As System.Windows.Forms.MenuItem
    Friend WithEvents EditMenuCut As System.Windows.Forms.MenuItem
    Friend WithEvents EditMenuCopy As System.Windows.Forms.MenuItem
    Friend WithEvents EditMenuPaste As System.Windows.Forms.MenuItem
    Friend WithEvents SimulationMenu As System.Windows.Forms.MenuItem
    Friend WithEvents SimulationMenuMake As System.Windows.Forms.MenuItem
    Friend WithEvents ViewMenuToolboxes As System.Windows.Forms.MenuItem
    Friend WithEvents ViewMenuToolBoxesStandard As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem2 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem5 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem6 As System.Windows.Forms.MenuItem
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MainUI))
        Me.MainMenu1 = New System.Windows.Forms.MainMenu
        Me.FileMenu = New System.Windows.Forms.MenuItem
        Me.FileMenuNew = New System.Windows.Forms.MenuItem
        Me.FileMenuOpen = New System.Windows.Forms.MenuItem
        Me.FileMenuSave = New System.Windows.Forms.MenuItem
        Me.MenuItem4 = New System.Windows.Forms.MenuItem
        Me.FileMenuExit = New System.Windows.Forms.MenuItem
        Me.EditMenu = New System.Windows.Forms.MenuItem
        Me.EditMenuCut = New System.Windows.Forms.MenuItem
        Me.EditMenuCopy = New System.Windows.Forms.MenuItem
        Me.EditMenuPaste = New System.Windows.Forms.MenuItem
        Me.MenuItem1 = New System.Windows.Forms.MenuItem
        Me.ViewMenuToolboxes = New System.Windows.Forms.MenuItem
        Me.ViewMenuToolBoxesStandard = New System.Windows.Forms.MenuItem
        Me.MenuItem2 = New System.Windows.Forms.MenuItem
        Me.MenuItem5 = New System.Windows.Forms.MenuItem
        Me.MenuItem3 = New System.Windows.Forms.MenuItem
        Me.ViewMenuOptions = New System.Windows.Forms.MenuItem
        Me.SimulationMenu = New System.Windows.Forms.MenuItem
        Me.SimulationMenuMake = New System.Windows.Forms.MenuItem
        Me.MenuItem6 = New System.Windows.Forms.MenuItem
        Me.HelpMenu = New System.Windows.Forms.MenuItem
        Me.HelpMenuAbout = New System.Windows.Forms.MenuItem
        Me.StatusBar1 = New System.Windows.Forms.StatusBar
        Me.ToolBar1 = New System.Windows.Forms.ToolBar
        Me.FileNewButton = New System.Windows.Forms.ToolBarButton
        Me.FileOpenButton = New System.Windows.Forms.ToolBarButton
        Me.FileSaveButton = New System.Windows.Forms.ToolBarButton
        Me.Separator1 = New System.Windows.Forms.ToolBarButton
        Me.RunButton = New System.Windows.Forms.ToolBarButton
        Me.Separator2 = New System.Windows.Forms.ToolBarButton
        Me.ButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.TabControl1 = New System.Windows.Forms.TabControl
        Me.TabPage1 = New System.Windows.Forms.TabPage
        Me.SimulationExplorer = New System.Windows.Forms.TreeView
        Me.TabPage2 = New System.Windows.Forms.TabPage
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.PageSetupDialog1 = New System.Windows.Forms.PageSetupDialog
        Me.ContextMenu1 = New System.Windows.Forms.ContextMenu
        Me.TabMenuClose = New System.Windows.Forms.MenuItem
        Me.TabControl2 = New System.Windows.Forms.TabControl
        Me.TabContextMenu = New System.Windows.Forms.ContextMenu
        Me.TabContextMenuHelp = New System.Windows.Forms.MenuItem
        Me.TabContextMenuClose = New System.Windows.Forms.MenuItem
        Me.Panel1.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.SuspendLayout()
        '
        'MainMenu1
        '
        Me.MainMenu1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.FileMenu, Me.EditMenu, Me.MenuItem1, Me.SimulationMenu, Me.HelpMenu})
        '
        'FileMenu
        '
        Me.FileMenu.Index = 0
        Me.FileMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.FileMenuNew, Me.FileMenuOpen, Me.FileMenuSave, Me.MenuItem4, Me.FileMenuExit})
        Me.FileMenu.MergeType = System.Windows.Forms.MenuMerge.MergeItems
        Me.FileMenu.Text = "&File"
        '
        'FileMenuNew
        '
        Me.FileMenuNew.Index = 0
        Me.FileMenuNew.Text = "&New ..."
        '
        'FileMenuOpen
        '
        Me.FileMenuOpen.Index = 1
        Me.FileMenuOpen.Text = "&Open ..."
        '
        'FileMenuSave
        '
        Me.FileMenuSave.Index = 2
        Me.FileMenuSave.Text = "&Save"
        '
        'MenuItem4
        '
        Me.MenuItem4.Index = 3
        Me.MenuItem4.MergeOrder = 2
        Me.MenuItem4.Text = "-"
        '
        'FileMenuExit
        '
        Me.FileMenuExit.Index = 4
        Me.FileMenuExit.MergeOrder = 2
        Me.FileMenuExit.Text = "E&xit"
        '
        'EditMenu
        '
        Me.EditMenu.Index = 1
        Me.EditMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.EditMenuCut, Me.EditMenuCopy, Me.EditMenuPaste})
        Me.EditMenu.Text = "&Edit"
        '
        'EditMenuCut
        '
        Me.EditMenuCut.Enabled = False
        Me.EditMenuCut.Index = 0
        Me.EditMenuCut.Shortcut = System.Windows.Forms.Shortcut.CtrlX
        Me.EditMenuCut.Text = "Cu&t"
        '
        'EditMenuCopy
        '
        Me.EditMenuCopy.Enabled = False
        Me.EditMenuCopy.Index = 1
        Me.EditMenuCopy.Shortcut = System.Windows.Forms.Shortcut.CtrlC
        Me.EditMenuCopy.Text = "&Copy"
        '
        'EditMenuPaste
        '
        Me.EditMenuPaste.Enabled = False
        Me.EditMenuPaste.Index = 2
        Me.EditMenuPaste.Shortcut = System.Windows.Forms.Shortcut.CtrlV
        Me.EditMenuPaste.Text = "&Paste"
        '
        'MenuItem1
        '
        Me.MenuItem1.Index = 2
        Me.MenuItem1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.ViewMenuToolboxes, Me.MenuItem3, Me.ViewMenuOptions})
        Me.MenuItem1.Text = "&View"
        '
        'ViewMenuToolboxes
        '
        Me.ViewMenuToolboxes.Index = 0
        Me.ViewMenuToolboxes.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.ViewMenuToolBoxesStandard, Me.MenuItem2, Me.MenuItem5})
        Me.ViewMenuToolboxes.Text = "&ToolBoxes"
        '
        'ViewMenuToolBoxesStandard
        '
        Me.ViewMenuToolBoxesStandard.Index = 0
        Me.ViewMenuToolBoxesStandard.Text = "Standard"
        '
        'MenuItem2
        '
        Me.MenuItem2.Index = 1
        Me.MenuItem2.Text = "Dummy"
        '
        'MenuItem5
        '
        Me.MenuItem5.Index = 2
        Me.MenuItem5.Text = "Dummy2"
        '
        'MenuItem3
        '
        Me.MenuItem3.Index = 1
        Me.MenuItem3.Text = "-"
        '
        'ViewMenuOptions
        '
        Me.ViewMenuOptions.Index = 2
        Me.ViewMenuOptions.Text = "&Options ..."
        '
        'SimulationMenu
        '
        Me.SimulationMenu.Index = 3
        Me.SimulationMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.SimulationMenuMake, Me.MenuItem6})
        Me.SimulationMenu.Text = "&Simulation"
        '
        'SimulationMenuMake
        '
        Me.SimulationMenuMake.Index = 0
        Me.SimulationMenuMake.Text = "&Make Sim File"
        '
        'MenuItem6
        '
        Me.MenuItem6.Index = 1
        Me.MenuItem6.Text = "Test"
        '
        'HelpMenu
        '
        Me.HelpMenu.Index = 4
        Me.HelpMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.HelpMenuAbout})
        Me.HelpMenu.MergeOrder = 2
        Me.HelpMenu.Text = "&Help"
        '
        'HelpMenuAbout
        '
        Me.HelpMenuAbout.Index = 0
        Me.HelpMenuAbout.Text = "&About ..."
        '
        'StatusBar1
        '
        Me.StatusBar1.Location = New System.Drawing.Point(0, 552)
        Me.StatusBar1.Name = "StatusBar1"
        Me.StatusBar1.Size = New System.Drawing.Size(1016, 22)
        Me.StatusBar1.TabIndex = 0
        Me.StatusBar1.Text = "StatusBar1"
        '
        'ToolBar1
        '
        Me.ToolBar1.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.FileNewButton, Me.FileOpenButton, Me.FileSaveButton, Me.Separator1, Me.RunButton, Me.Separator2})
        Me.ToolBar1.DropDownArrows = True
        Me.ToolBar1.ImageList = Me.ButtonImageList
        Me.ToolBar1.Location = New System.Drawing.Point(0, 0)
        Me.ToolBar1.Name = "ToolBar1"
        Me.ToolBar1.ShowToolTips = True
        Me.ToolBar1.Size = New System.Drawing.Size(1016, 28)
        Me.ToolBar1.TabIndex = 1
        '
        'FileNewButton
        '
        Me.FileNewButton.ImageIndex = 0
        Me.FileNewButton.ToolTipText = "Start a new simulation"
        '
        'FileOpenButton
        '
        Me.FileOpenButton.ImageIndex = 1
        Me.FileOpenButton.ToolTipText = "Open as simulation"
        '
        'FileSaveButton
        '
        Me.FileSaveButton.ImageIndex = 2
        Me.FileSaveButton.ToolTipText = "Save current simulation"
        '
        'Separator1
        '
        Me.Separator1.Style = System.Windows.Forms.ToolBarButtonStyle.Separator
        '
        'Separator2
        '
        Me.Separator2.Style = System.Windows.Forms.ToolBarButtonStyle.Separator
        '
        'ButtonImageList
        '
        Me.ButtonImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ButtonImageList.ImageStream = CType(resources.GetObject("ButtonImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ButtonImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'ImageList1
        '
        Me.ImageList1.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList1.ImageStream = CType(resources.GetObject("ImageList1.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList1.TransparentColor = System.Drawing.Color.Transparent
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.Filter = "XML Files|*.xml|All Files|*.*"
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.TabControl1)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Left
        Me.Panel1.Location = New System.Drawing.Point(0, 28)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(200, 524)
        Me.Panel1.TabIndex = 3
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(0, 0)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(200, 524)
        Me.TabControl1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.SimulationExplorer)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Size = New System.Drawing.Size(192, 498)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Simulation Explorer"
        '
        'SimulationExplorer
        '
        Me.SimulationExplorer.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationExplorer.ImageIndex = -1
        Me.SimulationExplorer.LabelEdit = True
        Me.SimulationExplorer.Location = New System.Drawing.Point(0, 0)
        Me.SimulationExplorer.Name = "SimulationExplorer"
        Me.SimulationExplorer.PathSeparator = "/"
        Me.SimulationExplorer.SelectedImageIndex = -1
        Me.SimulationExplorer.Size = New System.Drawing.Size(192, 498)
        Me.SimulationExplorer.TabIndex = 0
        '
        'TabPage2
        '
        Me.TabPage2.Location = New System.Drawing.Point(4, 22)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Size = New System.Drawing.Size(192, 498)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "ToolBox"
        '
        'Splitter1
        '
        Me.Splitter1.Location = New System.Drawing.Point(200, 28)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(3, 524)
        Me.Splitter1.TabIndex = 4
        Me.Splitter1.TabStop = False
        '
        'ContextMenu1
        '
        Me.ContextMenu1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.TabMenuClose})
        '
        'TabMenuClose
        '
        Me.TabMenuClose.Index = 0
        Me.TabMenuClose.Text = "&Close"
        '
        'TabControl2
        '
        Me.TabControl2.CausesValidation = False
        Me.TabControl2.ContextMenu = Me.TabContextMenu
        Me.TabControl2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl2.Location = New System.Drawing.Point(203, 28)
        Me.TabControl2.Multiline = True
        Me.TabControl2.Name = "TabControl2"
        Me.TabControl2.SelectedIndex = 0
        Me.TabControl2.Size = New System.Drawing.Size(813, 524)
        Me.TabControl2.TabIndex = 6
        '
        'TabContextMenu
        '
        Me.TabContextMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.TabContextMenuHelp, Me.TabContextMenuClose})
        '
        'TabContextMenuHelp
        '
        Me.TabContextMenuHelp.Index = 0
        Me.TabContextMenuHelp.Text = "&Help"
        '
        'TabContextMenuClose
        '
        Me.TabContextMenuClose.Index = 1
        Me.TabContextMenuClose.Text = "&Close"
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1016, 574)
        Me.Controls.Add(Me.TabControl2)
        Me.Controls.Add(Me.Splitter1)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.ToolBar1)
        Me.Controls.Add(Me.StatusBar1)
        Me.Menu = Me.MainMenu1
        Me.Name = "MainUI"
        Me.Text = "APSIM"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.Panel1.ResumeLayout(False)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region
    Sub OpenAPSimFile(ByVal Filename As String)
        Try
            If File.Exists(Filename) Then
                CurrentAPSIMFile = New APSIMFile(Filename)
                FillSimulationExplorer()
                SimulationExplorer.SelectedNode = SimulationExplorer.Nodes(0)
                SimulationExplorer.SelectedNode.Expand()
                MainUImanager = New UIManager(TabControl2, CurrentAPSIMFile)
            Else
                MsgBox("Cannot open :" + Filename, MsgBoxStyle.Critical, "File does not exist")
            End If

        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Cannot open APSIM file")
        End Try

    End Sub
    Sub GetAndOpenAPSimFile()
        Try
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                OpenAPSimFile(OpenFileDialog.FileName)
            Else
                ' User cancelled file open operation
            End If
        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Cannot open APSIM file")
        End Try

    End Sub
    Private Sub FillSimulationExplorer()
        Try
            SimulationExplorer.Nodes.Clear()
            ' Ultimately this needs to be replaced with a simulation name - especially when sim files can have >1 simulation

            Dim newnode As TreeNode = SimulationExplorer.Nodes.Add(CurrentAPSIMFile.XMLFilename)

            buildtree("/", newnode)
        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building simulation tree")
        End Try
    End Sub
    Private Sub buildtree(ByVal path As String, ByRef parentnode As TreeNode)
        Try
            ' NOTE - Path must be fully qualified name 
            Dim ChildList As New StringCollection

            CurrentAPSIMFile.GetChildList(path, ChildList)

            Dim item As Object
            For Each item In ChildList
                Dim childnode As TreeNode = parentnode.Nodes.Add(item)
                Dim newpath As String
                If Mid(path, Len(path)) <> "/" Then
                    newpath = path + "/" + childnode.Text
                Else
                    newpath = path + childnode.Text
                End If
                buildtree(newpath, childnode)
            Next
        Catch e As Exception
            MsgBox("Error building tree for : " + path + vbCrLf + vbCrLf + e.Message, MsgBoxStyle.Critical, "Error building simulation tree")
        End Try
    End Sub

    Private Sub FileMenuExit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuExit.Click
        End
    End Sub
    Private Sub FileMenuOpen_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuOpen.Click
        GetAndOpenAPSimFile()
    End Sub

    Private Sub ToolBar1_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles ToolBar1.ButtonClick
        If e.Button Is FileNewButton Then
            'OpenNewFile()
        ElseIf e.Button Is FileOpenButton Then
            GetAndOpenAPSimFile()
        ElseIf e.Button Is FileSaveButton Then
            CurrentAPSIMFile.save()
        End If
    End Sub


    Private Sub SimulationExplorer_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles SimulationExplorer.DoubleClick

        Dim path As String = SimulationExplorer.SelectedNode.FullPath
        If InStr(path, "/") > 0 Then
            path = Mid$(path, InStr(path, "/"))
            MainUImanager.ShowUI(path)
        End If
    End Sub

    Private Sub FileMenuSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuSave.Click
        CurrentAPSIMFile.save()
    End Sub

    Private Sub TabContextMenuClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TabContextMenuClose.Click
        TabControl2.TabPages.Remove(TabControl2.SelectedTab)
    End Sub

    Private Sub MainUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ' Declare variables.
        Dim separators As String = " "
        Dim commands As String = Microsoft.VisualBasic.Command()
        Dim args() As String = commands.Split(separators.ToCharArray)
        If args.Length = 0 Then
            ' do nothing
        ElseIf args.Length = 1 Then
            OpenAPSimFile(args(0))
        Else
            MsgBox("cannot handle > 1 command line arguments", MsgBoxStyle.Critical, "Error")
        End If

        BuildToolBoxMenus()

    End Sub


    Private Sub TabControl2_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TabControl2.MouseUp
        If e.Button = MouseButtons.Right Then
            Dim point As System.drawing.Point
            point.X = e.X
            point.Y = e.Y
            Dim cont As Control = TabControl2.GetChildAtPoint(point)
            If TypeOf (cont) Is TabPage Then
                cont.Select()
            End If
        End If
    End Sub

    Private Sub HelpMenuAbout_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpMenuAbout.Click
        MsgBox("Version 0.0 alpha - where angels fear to tread.", MsgBoxStyle.OKOnly, "APSIM")
    End Sub

    Private Sub ViewMenuToolbox_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewMenuToolboxes.Click
    
    End Sub
    Public Sub OnToolBoxClosing(ByVal toolboxname As String)
        ViewMenuToolBoxesStandard.Checked = False
    End Sub

    Private Sub ViewMenuOptions_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewMenuOptions.Click
        Dim optfrm As New OptionsForm
        optfrm.Show()
    End Sub

    Private Sub SimulationExplorer_AfterSelect(ByVal sender As System.Object, ByVal e As System.Windows.Forms.TreeViewEventArgs) Handles SimulationExplorer.AfterSelect

    End Sub

    Private Sub SimulationExplorer_AfterLabelEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles SimulationExplorer.AfterLabelEdit
        Dim oldname As String = SimulationExplorer.SelectedNode.Text
        Dim newname As String = e.Label
    End Sub

    Private Sub SimulationMenuMake_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationMenuMake.Click
        Dim mf As New MacroFile
        mf.Transform(CurrentAPSIMFile.XMLFilename, "C:\temp\macro.txt", "C:\temp")
    End Sub

    Private Sub ViewMenuToolBoxesStandard_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewMenuToolBoxesStandard.Click
        '        Try
        '        ViewMenuToolBoxesStandard.Checked = Not ViewMenuToolBoxesStandard.Checked
        '        If ViewMenuToolBoxesStandard.Checked = True Then
        '        Dim Toolbox = New OutlookBar
        '        Toolbox.mainform = Me
        '        Dim inifile As New APSIMSettings
        '        Toolbox.ToolFileName = inifile.GetSetting("Toolboxes", "standard")
        '        Toolbox.Show()
        '        Else
        '            'Toolbox.Hide()
        '            'Toolbox = Nothing
        '        End If
        '        Catch ex As Exception
        '        MsgBox(ex.Message, MsgBoxStyle.Critical, "Error")
        '        End Try
        '        'viewmenutoolboxesstandard.
    End Sub

    Private Sub MenuItem6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuItem6.Click
        Dim inifile As New APSIMSettings
        Dim value As String
        value = inifile.GetSetting("Apsim", "left")
        inifile.SetSetting("Apsim", "left", value + "g")
    End Sub

    Private Sub BuildToolBoxMenus()
        Try

            ViewMenuToolboxes.MenuItems.Clear()
            Dim item As New MenuItem("standard")
            AddHandler item.Click, AddressOf Me.ViewMenuUserToolBoxes_Click

            ViewMenuToolboxes.MenuItems.Add(item)


            Dim inifile As New APSIMSettings
            Dim ToolboxesString As String = inifile.GetSetting("Toolboxes", "toolbox")
            If (Trim(ToolboxesString) <> "") Then

                Dim ToolBoxes() As String = Split(ToolboxesString, "|")

                For i As Integer = 0 To ToolBoxes.Length - 1

                    'Dim file As New XMLFile(ToolBoxes(1))
                    'Dim ToolBoxName As String = file.RootPath
                    item = New MenuItem(ToolBoxes(i))
                    AddHandler item.Click, AddressOf Me.ViewMenuUserToolBoxes_Click
                    ViewMenuToolboxes.MenuItems.Add(item)
                Next
            End If
        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building tool box menus")
        End Try

    End Sub
    Private Sub ViewMenuUserToolBoxes_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewMenuToolBoxesStandard.Click
        '        MsgBox(sender.text)
        Dim NewToolBox As New ToolBox
        NewToolBox.mainform = Me
        Dim filename As String
        If sender.text = "standard" Then
            Dim inifile As New APSIMSettings
            filename = inifile.GetSetting("Toolboxes", "standard")
        Else
            filename = sender.text
        End If
        NewToolBox.ToolFileName = filename
        NewToolBox.Show()
    End Sub
End Class
