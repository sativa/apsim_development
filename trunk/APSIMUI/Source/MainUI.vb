Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.xml
Imports System.xml.XmlNodeList
Imports System.io
Imports System.IO.Path


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
        HelpBrowser.Navigate("c:\development\docs\documentation.xml")

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
    Friend WithEvents PageSetupDialog1 As System.Windows.Forms.PageSetupDialog
    Friend WithEvents ContextMenu1 As System.Windows.Forms.ContextMenu
    Friend WithEvents TabMenuClose As System.Windows.Forms.MenuItem
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
    Friend WithEvents MenuItem6 As System.Windows.Forms.MenuItem
    Friend WithEvents UIHelpButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ExportButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents CutButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents copyButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents PasteButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolBoxButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents SimulationExplorer As System.Windows.Forms.TreeView
    Friend WithEvents MainTabControl As System.Windows.Forms.TabControl
    Friend WithEvents ViewMenuHelp As System.Windows.Forms.MenuItem
    Friend WithEvents VerticalSplitter As System.Windows.Forms.Splitter
    Friend WithEvents HorizontalSplitter As System.Windows.Forms.Splitter
    Friend WithEvents toolBoxContextMenu As System.Windows.Forms.ContextMenu
    Friend WithEvents MenuItem2 As System.Windows.Forms.MenuItem
    Friend WithEvents ComponentImageList As System.Windows.Forms.ImageList
    Friend WithEvents HelpBrowser As AxSHDocVw.AxWebBrowser
    Friend WithEvents BackButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ForwardButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents HelpBrowsertoolBar As System.Windows.Forms.ToolBar
    Friend WithEvents SmallButtonImageList As System.Windows.Forms.ImageList
    Friend WithEvents HelpBrowserPanel As System.Windows.Forms.Panel
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
        Me.MenuItem3 = New System.Windows.Forms.MenuItem
        Me.ViewMenuHelp = New System.Windows.Forms.MenuItem
        Me.MenuItem2 = New System.Windows.Forms.MenuItem
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
        Me.CutButton = New System.Windows.Forms.ToolBarButton
        Me.copyButton = New System.Windows.Forms.ToolBarButton
        Me.PasteButton = New System.Windows.Forms.ToolBarButton
        Me.Separator2 = New System.Windows.Forms.ToolBarButton
        Me.RunButton = New System.Windows.Forms.ToolBarButton
        Me.ExportButton = New System.Windows.Forms.ToolBarButton
        Me.UIHelpButton = New System.Windows.Forms.ToolBarButton
        Me.ToolBoxButton = New System.Windows.Forms.ToolBarButton
        Me.toolBoxContextMenu = New System.Windows.Forms.ContextMenu
        Me.ButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.PageSetupDialog1 = New System.Windows.Forms.PageSetupDialog
        Me.ContextMenu1 = New System.Windows.Forms.ContextMenu
        Me.TabMenuClose = New System.Windows.Forms.MenuItem
        Me.MainTabControl = New System.Windows.Forms.TabControl
        Me.TabContextMenu = New System.Windows.Forms.ContextMenu
        Me.TabContextMenuHelp = New System.Windows.Forms.MenuItem
        Me.TabContextMenuClose = New System.Windows.Forms.MenuItem
        Me.SimulationExplorer = New System.Windows.Forms.TreeView
        Me.ComponentImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.VerticalSplitter = New System.Windows.Forms.Splitter
        Me.HorizontalSplitter = New System.Windows.Forms.Splitter
        Me.HelpBrowserPanel = New System.Windows.Forms.Panel
        Me.HelpBrowsertoolBar = New System.Windows.Forms.ToolBar
        Me.BackButton = New System.Windows.Forms.ToolBarButton
        Me.ForwardButton = New System.Windows.Forms.ToolBarButton
        Me.SmallButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.HelpBrowser = New AxSHDocVw.AxWebBrowser
        Me.HelpBrowserPanel.SuspendLayout()
        CType(Me.HelpBrowser, System.ComponentModel.ISupportInitialize).BeginInit()
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
        Me.MenuItem1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuItem3, Me.ViewMenuHelp, Me.MenuItem2, Me.ViewMenuOptions})
        Me.MenuItem1.Text = "&View"
        '
        'MenuItem3
        '
        Me.MenuItem3.Index = 0
        Me.MenuItem3.Text = "-"
        '
        'ViewMenuHelp
        '
        Me.ViewMenuHelp.Checked = True
        Me.ViewMenuHelp.Index = 1
        Me.ViewMenuHelp.Text = "Help Window"
        '
        'MenuItem2
        '
        Me.MenuItem2.Index = 2
        Me.MenuItem2.Text = "-"
        '
        'ViewMenuOptions
        '
        Me.ViewMenuOptions.Index = 3
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
        Me.StatusBar1.Location = New System.Drawing.Point(0, 467)
        Me.StatusBar1.Name = "StatusBar1"
        Me.StatusBar1.Size = New System.Drawing.Size(960, 22)
        Me.StatusBar1.TabIndex = 0
        Me.StatusBar1.Text = "StatusBar1"
        '
        'ToolBar1
        '
        Me.ToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBar1.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.FileNewButton, Me.FileOpenButton, Me.FileSaveButton, Me.Separator1, Me.CutButton, Me.copyButton, Me.PasteButton, Me.Separator2, Me.RunButton, Me.ExportButton, Me.UIHelpButton, Me.ToolBoxButton})
        Me.ToolBar1.DropDownArrows = True
        Me.ToolBar1.ImageList = Me.ButtonImageList
        Me.ToolBar1.Location = New System.Drawing.Point(0, 0)
        Me.ToolBar1.Name = "ToolBar1"
        Me.ToolBar1.ShowToolTips = True
        Me.ToolBar1.Size = New System.Drawing.Size(960, 36)
        Me.ToolBar1.TabIndex = 1
        Me.ToolBar1.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        '
        'FileNewButton
        '
        Me.FileNewButton.ImageIndex = 0
        Me.FileNewButton.Text = "New"
        Me.FileNewButton.ToolTipText = "Start a new simulation"
        '
        'FileOpenButton
        '
        Me.FileOpenButton.ImageIndex = 1
        Me.FileOpenButton.Text = "Open"
        Me.FileOpenButton.ToolTipText = "Open as simulation"
        '
        'FileSaveButton
        '
        Me.FileSaveButton.ImageIndex = 2
        Me.FileSaveButton.Text = "Save"
        Me.FileSaveButton.ToolTipText = "Save current simulation"
        '
        'Separator1
        '
        Me.Separator1.Style = System.Windows.Forms.ToolBarButtonStyle.Separator
        '
        'CutButton
        '
        Me.CutButton.ImageIndex = 3
        Me.CutButton.Text = "Cut"
        '
        'copyButton
        '
        Me.copyButton.ImageIndex = 4
        Me.copyButton.Text = "Copy"
        '
        'PasteButton
        '
        Me.PasteButton.ImageIndex = 5
        Me.PasteButton.Text = "Paste"
        '
        'Separator2
        '
        Me.Separator2.Style = System.Windows.Forms.ToolBarButtonStyle.Separator
        '
        'RunButton
        '
        Me.RunButton.ImageIndex = 8
        Me.RunButton.Text = "Run"
        '
        'ExportButton
        '
        Me.ExportButton.ImageIndex = 7
        Me.ExportButton.Text = "Export"
        '
        'UIHelpButton
        '
        Me.UIHelpButton.ImageIndex = 6
        Me.UIHelpButton.Text = "Help"
        '
        'ToolBoxButton
        '
        Me.ToolBoxButton.DropDownMenu = Me.toolBoxContextMenu
        Me.ToolBoxButton.ImageIndex = 9
        Me.ToolBoxButton.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton
        Me.ToolBoxButton.Text = "ToolBoxes"
        Me.ToolBoxButton.ToolTipText = "Load a modelling toolbox for use"
        '
        'ButtonImageList
        '
        Me.ButtonImageList.ImageSize = New System.Drawing.Size(24, 24)
        Me.ButtonImageList.ImageStream = CType(resources.GetObject("ButtonImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ButtonImageList.TransparentColor = System.Drawing.SystemColors.Control
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
        'ContextMenu1
        '
        Me.ContextMenu1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.TabMenuClose})
        '
        'TabMenuClose
        '
        Me.TabMenuClose.Index = 0
        Me.TabMenuClose.Text = "&Close"
        '
        'MainTabControl
        '
        Me.MainTabControl.CausesValidation = False
        Me.MainTabControl.ContextMenu = Me.TabContextMenu
        Me.MainTabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.MainTabControl.Location = New System.Drawing.Point(197, 36)
        Me.MainTabControl.Multiline = True
        Me.MainTabControl.Name = "MainTabControl"
        Me.MainTabControl.SelectedIndex = 0
        Me.MainTabControl.ShowToolTips = True
        Me.MainTabControl.Size = New System.Drawing.Size(763, 226)
        Me.MainTabControl.TabIndex = 6
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
        'SimulationExplorer
        '
        Me.SimulationExplorer.AllowDrop = True
        Me.SimulationExplorer.Dock = System.Windows.Forms.DockStyle.Left
        Me.SimulationExplorer.HideSelection = False
        Me.SimulationExplorer.ImageList = Me.ComponentImageList
        Me.SimulationExplorer.LabelEdit = True
        Me.SimulationExplorer.Location = New System.Drawing.Point(0, 36)
        Me.SimulationExplorer.Name = "SimulationExplorer"
        Me.SimulationExplorer.PathSeparator = "/"
        Me.SimulationExplorer.Size = New System.Drawing.Size(192, 431)
        Me.SimulationExplorer.TabIndex = 8
        '
        'ComponentImageList
        '
        Me.ComponentImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ComponentImageList.ImageStream = CType(resources.GetObject("ComponentImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ComponentImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'VerticalSplitter
        '
        Me.VerticalSplitter.Location = New System.Drawing.Point(192, 36)
        Me.VerticalSplitter.Name = "VerticalSplitter"
        Me.VerticalSplitter.Size = New System.Drawing.Size(5, 431)
        Me.VerticalSplitter.TabIndex = 9
        Me.VerticalSplitter.TabStop = False
        '
        'HorizontalSplitter
        '
        Me.HorizontalSplitter.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.HorizontalSplitter.Location = New System.Drawing.Point(197, 262)
        Me.HorizontalSplitter.Name = "HorizontalSplitter"
        Me.HorizontalSplitter.Size = New System.Drawing.Size(763, 5)
        Me.HorizontalSplitter.TabIndex = 10
        Me.HorizontalSplitter.TabStop = False
        '
        'HelpBrowserPanel
        '
        Me.HelpBrowserPanel.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.HelpBrowserPanel.Controls.Add(Me.HelpBrowsertoolBar)
        Me.HelpBrowserPanel.Controls.Add(Me.HelpBrowser)
        Me.HelpBrowserPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.HelpBrowserPanel.Location = New System.Drawing.Point(197, 267)
        Me.HelpBrowserPanel.Name = "HelpBrowserPanel"
        Me.HelpBrowserPanel.Size = New System.Drawing.Size(763, 200)
        Me.HelpBrowserPanel.TabIndex = 11
        '
        'HelpBrowsertoolBar
        '
        Me.HelpBrowsertoolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.HelpBrowsertoolBar.AutoSize = False
        Me.HelpBrowsertoolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.BackButton, Me.ForwardButton})
        Me.HelpBrowsertoolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.HelpBrowsertoolBar.DropDownArrows = True
        Me.HelpBrowsertoolBar.ImageList = Me.SmallButtonImageList
        Me.HelpBrowsertoolBar.Location = New System.Drawing.Point(0, 0)
        Me.HelpBrowsertoolBar.Name = "HelpBrowsertoolBar"
        Me.HelpBrowsertoolBar.ShowToolTips = True
        Me.HelpBrowsertoolBar.Size = New System.Drawing.Size(759, 20)
        Me.HelpBrowsertoolBar.TabIndex = 9
        Me.HelpBrowsertoolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        '
        'BackButton
        '
        Me.BackButton.ImageIndex = 0
        Me.BackButton.Text = "Back"
        '
        'ForwardButton
        '
        Me.ForwardButton.ImageIndex = 1
        Me.ForwardButton.Text = "Forward"
        '
        'SmallButtonImageList
        '
        Me.SmallButtonImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.SmallButtonImageList.ImageStream = CType(resources.GetObject("SmallButtonImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.SmallButtonImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'HelpBrowser
        '
        Me.HelpBrowser.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.HelpBrowser.ContainingControl = Me
        Me.HelpBrowser.Enabled = True
        Me.HelpBrowser.Location = New System.Drawing.Point(0, 24)
        Me.HelpBrowser.OcxState = CType(resources.GetObject("HelpBrowser.OcxState"), System.Windows.Forms.AxHost.State)
        Me.HelpBrowser.Size = New System.Drawing.Size(763, 176)
        Me.HelpBrowser.TabIndex = 8
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(960, 489)
        Me.Controls.Add(Me.MainTabControl)
        Me.Controls.Add(Me.HorizontalSplitter)
        Me.Controls.Add(Me.HelpBrowserPanel)
        Me.Controls.Add(Me.VerticalSplitter)
        Me.Controls.Add(Me.SimulationExplorer)
        Me.Controls.Add(Me.ToolBar1)
        Me.Controls.Add(Me.StatusBar1)
        Me.Menu = Me.MainMenu1
        Me.Name = "MainUI"
        Me.Text = "APSIM"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.HelpBrowserPanel.ResumeLayout(False)
        CType(Me.HelpBrowser, System.ComponentModel.ISupportInitialize).EndInit()
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
                MainUImanager = New UIManager(MainTabControl, CurrentAPSIMFile)
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
                Dim newpath As String
                If Mid(path, Len(path)) <> "/" Then
                    newpath = path + "/" + item
                Else
                    newpath = path + item
                End If

                Dim type As String = CurrentAPSIMFile.GetDataType(newpath)
                Dim addthis As Boolean = False
                Dim openindex As Integer
                Dim closedindex As Integer
                Select Case LCase(type)
                    Case "simulation", "simulations"
                        addthis = True
                        openindex = 0
                        closedindex = 1
                    Case "soil", "outputfile", "metfile", "tracker", "area"
                        addthis = True
                        openindex = 2
                        closedindex = 3
                    Case Else
                        addthis = False
                End Select
                If addthis Then
                    Dim childnode As TreeNode = parentnode.Nodes.Add(item)
                    childnode.ImageIndex = closedindex
                    childnode.SelectedImageIndex = openindex

                    buildtree(newpath, childnode)

                End If
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
        ElseIf e.Button Is UIHelpButton Then
            UpdateHelpBrowser()
        End If
    End Sub


    Private Sub SimulationExplorer_DoubleClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationExplorer.DoubleClick

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
        MainTabControl.TabPages.Remove(MainTabControl.SelectedTab)
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


        PopulateToolBoxContextMenu()

    End Sub


    Private Sub TabControl2_MouseUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles MainTabControl.MouseUp
        If e.Button = MouseButtons.Right Then
            Dim point As System.drawing.Point
            point.X = e.X
            point.Y = e.Y
            Dim cont As Control = MainTabControl.GetChildAtPoint(point)
            If TypeOf (cont) Is TabPage Then
                cont.Select()
            End If
        End If
    End Sub

    Private Sub HelpMenuAbout_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpMenuAbout.Click
        MsgBox("Version 0.0 alpha - where angels fear to tread.", MsgBoxStyle.OKOnly, "APSIM")
    End Sub


    Private Sub ViewMenuOptions_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewMenuOptions.Click
        Dim optfrm As New OptionsForm
        optfrm.Show()
    End Sub

    Private Sub SimulationExplorer_AfterSelect(ByVal sender As System.Object, ByVal e As System.Windows.Forms.TreeViewEventArgs)

    End Sub

    Private Sub SimulationExplorer_AfterLabelEdit(ByVal sender As System.Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs)
        Dim oldname As String = SimulationExplorer.SelectedNode.Text
        Dim newname As String = e.Label
    End Sub

    Private Sub SimulationMenuMake_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationMenuMake.Click
        Dim mf As New MacroFile
        mf.Transform(CurrentAPSIMFile.XMLFilename, "C:\temp\macro.txt", "C:\temp")
    End Sub

    Private Sub MenuItem6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuItem6.Click
        Dim inifile As New APSIMSettings
        Dim value As String
        value = inifile.GetSetting("Apsim", "left")
        inifile.SetSetting("Apsim", "left", value + "g")
    End Sub

    Private Sub ToolBoxes_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
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


    Private Sub ViewMenuHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewMenuHelp.Click
        ViewMenuHelp.Checked = Not ViewMenuHelp.Checked
        If ViewMenuHelp.Checked = True Then
            HelpBrowser.Visible = True
            HelpBrowsertoolBar.Visible = True
            HelpBrowserPanel.Height = Me.Height / 3
            HorizontalSplitter.Enabled = True
        Else
            HelpBrowser.Visible = False
            HelpBrowsertoolBar.Visible = False
            HelpBrowserPanel.Height = 1
            HorizontalSplitter.Enabled = False
        End If
    End Sub

    Private Sub PopulateToolBoxContextMenu()
        Try

            toolBoxContextMenu.MenuItems.Clear()
            Dim item As New MenuItem("standard")
            AddHandler item.Click, AddressOf Me.ToolBoxes_Click
            toolBoxContextMenu.MenuItems.Add(item)


            Dim inifile As New APSIMSettings
            Dim ToolboxesString As String = inifile.GetSetting("Toolboxes", "toolbox")
            If (Trim(ToolboxesString) <> "") Then

                Dim ToolBoxes() As String = Split(ToolboxesString, "|")

                For i As Integer = 0 To ToolBoxes.Length - 1

                    'Dim file As New XMLFile(ToolBoxes(1))
                    'Dim ToolBoxName As String = file.RootPath
                    item = New MenuItem(ToolBoxes(i))
                    AddHandler item.Click, AddressOf Me.ToolBoxes_Click
                    toolBoxContextMenu.MenuItems.Add(item)
                Next
            End If
        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building tool box menus")
        End Try
    End Sub
    Private Sub UpdateHelpBrowser()
        Dim type As String = CurrentAPSIMFile.GetDataType(SimulationExplorer.SelectedNode.FullPath)
        MsgBox(type)

    End Sub

    Private Sub MainTabControl_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainTabControl.SelectedIndexChanged
        If MainTabControl.TabCount > 0 Then
            Dim path As String = MainTabControl.TabPages(MainTabControl.SelectedIndex).Tag

            FindTreeNode(path)

        End If
    End Sub
    Private Sub FindTreeNode(ByVal path As String)
        Try
            Dim name As String = ""

            ' Get rid of starting slash
            If InStr(path, "/") = 1 Then
                path = Mid$(path, 2)
            End If
            SimulationExplorer.SelectedNode = SimulationExplorer.TopNode

            ' Now loop through all steps on the path finding nodes with matching names
            Do While Len(path) <> 0
                If InStr(path, "/") <> 0 Then
                    name = Microsoft.VisualBasic.Left(path, InStr(path, "/") - 1)
                    path = Mid$(path, InStr(path, "/") + 1)
                Else
                    name = path
                    path = ""
                End If

                Dim node As TreeNode
                Dim ChildName As String
                For Each node In SimulationExplorer.SelectedNode.Nodes
                    ChildName = node.Text
                    If LCase(ChildName) = LCase(name) Then
                        SimulationExplorer.SelectedNode = node
                        Exit For
                    End If
                Next
            Loop
        Catch e As Exception
            MsgBox(e.Message + vbCrLf + "Data Path: " + path, MsgBoxStyle.Critical, "Error selecting data node in APSIM File")
        End Try

    End Sub

    Private Sub ToolBar2_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles HelpBrowsertoolBar.ButtonClick
        If e.Button Is ForwardButton Then
            HelpBrowser.GoForward()
        ElseIf e.Button Is BackButton Then
            HelpBrowser.GoBack()
        End If

    End Sub

    Private Sub SimulationExplorer_AfterSelect_1(ByVal sender As System.Object, ByVal e As System.Windows.Forms.TreeViewEventArgs) Handles SimulationExplorer.AfterSelect

    End Sub

    Private Sub SimulationExplorer_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles SimulationExplorer.DragDrop
        Dim target As TreeNode = SimulationExplorer.GetNodeAt(e.X, e.Y)
        MsgBox(target.Text)
    End Sub
End Class
