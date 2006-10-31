Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.xml
Imports System.xml.XmlNodeList
Imports System.io
Imports System.IO.Path
Imports General
Imports DataTreeControl

Public Class MainUI
    Inherits System.Windows.Forms.Form
    Private MainUImanager As New UIManager
    Private APSIMFile As New APSIMFile

    'Private Toolbox As New OutlookBar
    

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        HelpBrowser.Navigate("c:\development\docs\documentation.xml")

        ' Tell the UI manager where everything is
        MainUImanager.UIPanel = UIPanel
        MainUImanager.SimulationExplorer = SimulationExplorer
        MainUImanager.MainForm = Me

        '

        'Event Handlers
        AddHandler SimulationExplorer.DataSelectedEvent, AddressOf OnDataSelected


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
    Friend WithEvents UIHelpButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ExportButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents CutButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents copyButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents PasteButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolBoxButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ViewMenuHelp As System.Windows.Forms.MenuItem
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
    Friend WithEvents MenuItem5 As System.Windows.Forms.MenuItem
    Friend WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Friend WithEvents SaveFileDialog As System.Windows.Forms.SaveFileDialog
    Friend WithEvents DataTree1 As DataTree
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents SimulationExplorer As DataTree
    Friend WithEvents UIPanel As System.Windows.Forms.Panel
    Friend WithEvents EmailButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents NewsButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents SeparatorButton As System.Windows.Forms.ToolBarButton
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MainUI))
        Me.MainMenu1 = New System.Windows.Forms.MainMenu
        Me.FileMenu = New System.Windows.Forms.MenuItem
        Me.FileMenuNew = New System.Windows.Forms.MenuItem
        Me.FileMenuOpen = New System.Windows.Forms.MenuItem
        Me.FileMenuSave = New System.Windows.Forms.MenuItem
        Me.MenuItem5 = New System.Windows.Forms.MenuItem
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
        Me.HelpMenu = New System.Windows.Forms.MenuItem
        Me.HelpMenuAbout = New System.Windows.Forms.MenuItem
        Me.StatusBar1 = New System.Windows.Forms.StatusBar
        Me.ToolBar1 = New System.Windows.Forms.ToolBar
        Me.FileNewButton = New System.Windows.Forms.ToolBarButton
        Me.FileOpenButton = New System.Windows.Forms.ToolBarButton
        Me.FileSaveButton = New System.Windows.Forms.ToolBarButton
        Me.EmailButton = New System.Windows.Forms.ToolBarButton
        Me.Separator1 = New System.Windows.Forms.ToolBarButton
        Me.CutButton = New System.Windows.Forms.ToolBarButton
        Me.copyButton = New System.Windows.Forms.ToolBarButton
        Me.PasteButton = New System.Windows.Forms.ToolBarButton
        Me.Separator2 = New System.Windows.Forms.ToolBarButton
        Me.ToolBoxButton = New System.Windows.Forms.ToolBarButton
        Me.toolBoxContextMenu = New System.Windows.Forms.ContextMenu
        Me.RunButton = New System.Windows.Forms.ToolBarButton
        Me.ExportButton = New System.Windows.Forms.ToolBarButton
        Me.NewsButton = New System.Windows.Forms.ToolBarButton
        Me.UIHelpButton = New System.Windows.Forms.ToolBarButton
        Me.ButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.PageSetupDialog1 = New System.Windows.Forms.PageSetupDialog
        Me.ContextMenu1 = New System.Windows.Forms.ContextMenu
        Me.TabMenuClose = New System.Windows.Forms.MenuItem
        Me.ComponentImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.HorizontalSplitter = New System.Windows.Forms.Splitter
        Me.HelpBrowserPanel = New System.Windows.Forms.Panel
        Me.HelpBrowsertoolBar = New System.Windows.Forms.ToolBar
        Me.BackButton = New System.Windows.Forms.ToolBarButton
        Me.ForwardButton = New System.Windows.Forms.ToolBarButton
        Me.SmallButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.HelpBrowser = New AxSHDocVw.AxWebBrowser
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog
        Me.SaveFileDialog = New System.Windows.Forms.SaveFileDialog
        Me.DataTree1 = New DataTreeControl.DataTree
        Me.SimulationExplorer = New DataTreeControl.DataTree
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.UIPanel = New System.Windows.Forms.Panel
        Me.SeparatorButton = New System.Windows.Forms.ToolBarButton
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
        Me.FileMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.FileMenuNew, Me.FileMenuOpen, Me.FileMenuSave, Me.MenuItem5, Me.MenuItem4, Me.FileMenuExit})
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
        'MenuItem5
        '
        Me.MenuItem5.Index = 3
        Me.MenuItem5.Text = "Save &As ..."
        '
        'MenuItem4
        '
        Me.MenuItem4.Index = 4
        Me.MenuItem4.MergeOrder = 2
        Me.MenuItem4.Text = "-"
        '
        'FileMenuExit
        '
        Me.FileMenuExit.Index = 5
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
        Me.SimulationMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.SimulationMenuMake})
        Me.SimulationMenu.Text = "&Simulation"
        '
        'SimulationMenuMake
        '
        Me.SimulationMenuMake.Index = 0
        Me.SimulationMenuMake.Text = "&Make Sim File"
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
        Me.ToolBar1.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.FileNewButton, Me.FileOpenButton, Me.FileSaveButton, Me.EmailButton, Me.Separator1, Me.CutButton, Me.copyButton, Me.PasteButton, Me.Separator2, Me.ToolBoxButton, Me.RunButton, Me.ExportButton, Me.SeparatorButton, Me.NewsButton, Me.UIHelpButton})
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
        'EmailButton
        '
        Me.EmailButton.ImageIndex = 12
        Me.EmailButton.Text = "Email"
        Me.EmailButton.ToolTipText = "Email your simulation to a friend"
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
        'ToolBoxButton
        '
        Me.ToolBoxButton.DropDownMenu = Me.toolBoxContextMenu
        Me.ToolBoxButton.ImageIndex = 9
        Me.ToolBoxButton.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton
        Me.ToolBoxButton.Text = "ToolBoxes"
        Me.ToolBoxButton.ToolTipText = "Load a modelling toolbox for use"
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
        'NewsButton
        '
        Me.NewsButton.ImageIndex = 13
        Me.NewsButton.Text = "News"
        Me.NewsButton.ToolTipText = "Get the latest information from APSIM.info"
        '
        'UIHelpButton
        '
        Me.UIHelpButton.ImageIndex = 6
        Me.UIHelpButton.Text = "Help"
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
        'ComponentImageList
        '
        Me.ComponentImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ComponentImageList.ImageStream = CType(resources.GetObject("ComponentImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ComponentImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'HorizontalSplitter
        '
        Me.HorizontalSplitter.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.HorizontalSplitter.Location = New System.Drawing.Point(0, 262)
        Me.HorizontalSplitter.Name = "HorizontalSplitter"
        Me.HorizontalSplitter.Size = New System.Drawing.Size(960, 5)
        Me.HorizontalSplitter.TabIndex = 10
        Me.HorizontalSplitter.TabStop = False
        '
        'HelpBrowserPanel
        '
        Me.HelpBrowserPanel.Controls.Add(Me.HelpBrowsertoolBar)
        Me.HelpBrowserPanel.Controls.Add(Me.HelpBrowser)
        Me.HelpBrowserPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.HelpBrowserPanel.Location = New System.Drawing.Point(0, 267)
        Me.HelpBrowserPanel.Name = "HelpBrowserPanel"
        Me.HelpBrowserPanel.Size = New System.Drawing.Size(960, 200)
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
        Me.HelpBrowsertoolBar.Size = New System.Drawing.Size(960, 20)
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
        Me.HelpBrowser.Size = New System.Drawing.Size(964, 180)
        Me.HelpBrowser.TabIndex = 8
        '
        'SaveFileDialog
        '
        Me.SaveFileDialog.Filter = "XML Files|*.xml|All Files|*.*"
        '
        'DataTree1
        '
        Me.DataTree1.LabelEdit = False
        Me.DataTree1.Location = New System.Drawing.Point(0, 0)
        Me.DataTree1.Name = "DataTree1"
        Me.DataTree1.Size = New System.Drawing.Size(344, 488)
        Me.DataTree1.TabIndex = 0
        '
        'SimulationExplorer
        '
        Me.SimulationExplorer.AllowDrop = True
        Me.SimulationExplorer.Dock = System.Windows.Forms.DockStyle.Left
        Me.SimulationExplorer.LabelEdit = True
        Me.SimulationExplorer.Location = New System.Drawing.Point(0, 36)
        Me.SimulationExplorer.Name = "SimulationExplorer"
        Me.SimulationExplorer.Size = New System.Drawing.Size(240, 226)
        Me.SimulationExplorer.TabIndex = 13
        '
        'Splitter1
        '
        Me.Splitter1.Location = New System.Drawing.Point(240, 36)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(5, 226)
        Me.Splitter1.TabIndex = 14
        Me.Splitter1.TabStop = False
        '
        'UIPanel
        '
        Me.UIPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.UIPanel.Location = New System.Drawing.Point(245, 36)
        Me.UIPanel.Name = "UIPanel"
        Me.UIPanel.Size = New System.Drawing.Size(715, 226)
        Me.UIPanel.TabIndex = 15
        '
        'SeparatorButton
        '
        Me.SeparatorButton.Style = System.Windows.Forms.ToolBarButtonStyle.Separator
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(960, 489)
        Me.Controls.Add(Me.UIPanel)
        Me.Controls.Add(Me.Splitter1)
        Me.Controls.Add(Me.SimulationExplorer)
        Me.Controls.Add(Me.HorizontalSplitter)
        Me.Controls.Add(Me.HelpBrowserPanel)
        Me.Controls.Add(Me.ToolBar1)
        Me.Controls.Add(Me.StatusBar1)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Menu = Me.MainMenu1
        Me.Name = "MainUI"
        Me.Text = "APSIM"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.HelpBrowserPanel.ResumeLayout(False)
        CType(Me.HelpBrowser, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Sub OnDataSelected(ByVal sender As Object, ByVal e As APSIMData)

        MainUImanager.ShowUI(e)

    End Sub
    Sub OpenAPSimFile(Optional ByVal filename As String = "")
        Try
            If filename = "" Then
                APSIMFile.Open()
            Else
                APSIMFile.Open(filename)
            End If
            SimulationExplorer.Data = APSIMFile.data
            UpdateMainForm()

        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Cannot open APSIM file")
        End Try

    End Sub

    Sub OpenNewFile()
        Try
            APSIMFile = New APSIMFile
            SimulationExplorer.Data = APSIMFile.data
            UpdateMainForm()

        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error openinig document template")
        End Try
    End Sub
    Private Sub UpdateMainForm()

        Me.Text = APSIMFile.Filename
        'SimulationExplorer.DrawTree()
        'MainUImanager.FillSimulationExplorer(APSIMFile.Data)
        'SimulationExplorer.SelectedNode = SimulationExplorer.Nodes(0)
        'SimulationExplorer.SelectedNode.Expand()

    End Sub
    Private Sub FileMenuExit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuExit.Click
        End
    End Sub
    Private Sub FileMenuOpen_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuOpen.Click
        OpenAPSimFile()
    End Sub

    Private Sub ToolBar1_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles ToolBar1.ButtonClick
        If e.Button Is FileNewButton Then
            OpenNewFile()
        ElseIf e.Button Is FileOpenButton Then
            OpenAPSimFile()
        ElseIf e.Button Is FileSaveButton Then
            MainUImanager.SaveDocument(APSIMFile)
            APSIMFile.Save()
        ElseIf e.Button Is UIHelpButton Then
            UpdateHelpBrowser()
        ElseIf e.Button Is NewsButton Then
            GetLatestNews()
        End If
    End Sub


    Private Sub SimulationExplorer_DoubleClick(ByVal sender As System.Object, ByVal e As System.EventArgs)



    End Sub

    Private Sub FileMenuSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuSave.Click

        MainUImanager.SaveDocument(APSIMFile)
        APSIMFile.Save()

    End Sub


    Private Sub MainUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ' Declare variables.
        Dim separators As String = " "
        Dim commands As String = Microsoft.VisualBasic.Command()
        Dim args() As String = commands.Split(separators.ToCharArray)
        If args.Length = 0 Then
            ' do nothing
        ElseIf args.Length = 1 Then
            If args(0).Length() > 0 Then
                OpenAPSimFile(args(0))
            End If
        Else
            MsgBox("cannot handle > 1 command line arguments", MsgBoxStyle.Critical, "Error")
        End If

        PopulateToolBoxContextMenu()

    End Sub


    Private Sub HelpMenuAbout_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpMenuAbout.Click
        MsgBox("Version 0.0 alpha - where angels fear to tread.", MsgBoxStyle.OKOnly, "APSIM")
    End Sub


    Private Sub ViewMenuOptions_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewMenuOptions.Click
        Dim optfrm As New OptionsForm
        optfrm.ShowDialog(Me)
        ' Repopulate tool boxes just in case options have been changed
        PopulateToolBoxContextMenu()
    End Sub


    Private Sub SimulationMenuMake_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationMenuMake.Click
        Try
            Dim mf As New MacroFile
            Dim inifile As New APSIMSettings
            If File.Exists(APSIMFile.Filename) Then
                Dim DirectoryName As String = Path.GetDirectoryName(APSIMFile.Filename)
                Dim FileName As String = inifile.GetSetting("apsimui", "macrofile")
                If File.Exists(FileName) Then
                    mf.DoTransform(APSIMFile.Filename, FileName, DirectoryName)
                Else
                    MsgBox("Cannot find the simulation creation macro file", MsgBoxStyle.Critical, "Error")
                End If
            Else
                MsgBox("Cannot make simulation until apsim file has been saved to a target location.", MsgBoxStyle.Critical, "Error")
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error")
        End Try
    End Sub

    Private Sub MenuItem6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim inifile As New APSIMSettings
        Dim value As String
        value = inifile.GetSetting("Apsim", "left")
        inifile.SetSetting("Apsim", "left", value + "g")
    End Sub

    Private Sub ToolBoxes_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        'Dim NewToolBox As New ToolBox
        'NewToolBox.mainform = Me
        'Dim filename As String
        'If sender.text = "standard" Then
        '    Dim inifile As New APSIMSettings
        '    filename = inifile.GetSetting("Toolboxes", "standard")
        'Else
        '    filename = sender.text
        'End If
        'NewToolBox.ToolFileName = filename
        'NewToolBox.Show()
        Dim filename As String = sender.text
        Dim apsimfile As New APSIMFile
        apsimfile.Open(filename)
        MainUImanager.ShowUI(apsimfile.data)

    End Sub


    Private Sub ViewMenuHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewMenuHelp.Click
        ViewMenuHelp.Checked = Not ViewMenuHelp.Checked
        If ViewMenuHelp.Checked = True Then
            ShowHelpBrowser()
        Else
            CloseHelpBrowser()
        End If
    End Sub

    Private Sub ShowHelpBrowser()
        HelpBrowser.Visible = True
        HelpBrowsertoolBar.Visible = True
        HelpBrowserPanel.Height = Me.Height / 3
        HorizontalSplitter.Enabled = True
    End Sub
    Private Sub CloseHelpBrowser()
        HelpBrowser.Visible = False
        HelpBrowsertoolBar.Visible = False
        HelpBrowserPanel.Height = 1
        HorizontalSplitter.Enabled = False
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
        'Dim type As String = MainUImanager.APSIMFile.GetDataType(SimulationExplorer.SelectedNode.FullPath)
        'MsgBox(type)

    End Sub
    Private Sub GetLatestNews()
        HelpBrowser.Navigate("www.apsim.info")
    End Sub

    'Private Sub FindTreeNode(ByVal path As String)
    '    Try
    '        Dim name As String = ""

    '        ' Get rid of starting slash
    '        If InStr(path, "/") = 1 Then
    '            path = Mid$(path, 2)
    '        End If
    '        SimulationExplorer.SelectedNode = SimulationExplorer.TopNode

    '        ' Now loop through all steps on the path finding nodes with matching names
    '        Do While Len(path) <> 0
    '            If InStr(path, "/") <> 0 Then
    '                name = Microsoft.VisualBasic.Left(path, InStr(path, "/") - 1)
    '                path = Mid$(path, InStr(path, "/") + 1)
    '            Else
    '                name = path
    '                path = ""
    '            End If

    '            Dim node As TreeNode
    '            Dim ChildName As String
    '            For Each node In SimulationExplorer.SelectedNode.Nodes
    '                ChildName = node.Text
    '                If LCase(ChildName) = LCase(name) Then
    '                    SimulationExplorer.SelectedNode = node
    '                    Exit For
    '                End If
    '            Next
    '        Loop
    '    Catch e As Exception
    '        MsgBox(e.Message + vbCrLf + "Data Path: " + path, MsgBoxStyle.Critical, "Error selecting data node in APSIM File")
    '    End Try

    'End Sub

    Private Sub ToolBar2_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles HelpBrowsertoolBar.ButtonClick
        If e.Button Is ForwardButton Then
            HelpBrowser.GoForward()
        ElseIf e.Button Is BackButton Then
            HelpBrowser.GoBack()
        End If

    End Sub

    'Private Sub SimulationExplorer_AfterSelect(ByVal sender As System.Object, ByVal e As System.Windows.Forms.TreeViewEventArgs)
    '    Dim path As String = SimulationExplorer.SelectedNode.FullPath
    '    'If InStr(path, "|") > 0 Then
    '    'path = Mid(path, InStr(path, "|") + 1)
    '    MainUImanager.ShowUI(APSIMFile.data.FindChild(path))
    '    'Else
    '    '   MainUImanager.ShowUI(APSIMFile.data)
    '    'End If

    'End Sub

    'Public Sub SimulationExplorer_DragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs)
    '    Try
    '        Dim pt As Point
    '        Dim DestinationNode As TreeNode
    '        pt = CType(sender, TreeView).PointToClient(New Point(e.X, e.Y))
    '        DestinationNode = CType(sender, TreeView).GetNodeAt(pt)
    '        'MsgBox(DestinationNode.Text)
    '        'MsgBox(DestinationNode.FullPath)
    '        'MsgBox(e.Data.GetData(DataFormats.Text))
    '        APSIMFile.Data.Child(DestinationNode.Text).Add(e.Data.GetData(DataFormats.Text))



    '        UpdateMainForm()

    '    Catch ex As Exception
    '        MsgBox(ex.Message, MsgBoxStyle.Critical, "Error")
    '    End Try
    'End Sub


    Private Sub FileMenuNew_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuNew.Click
        OpenNewFile()
    End Sub

    Private Sub MenuItem5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuItem5.Click
        APSIMFile.SaveAs()
    End Sub


    Private Sub UIPanel_Paint(ByVal sender As System.Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles UIPanel.Paint

    End Sub
End Class
