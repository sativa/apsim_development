Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.xml
Imports System.xml.XmlNodeList
Imports System.io
Imports System.IO.Path
Imports VBGeneral
Imports CSGeneral

Public Class MainUI
    Inherits System.Windows.Forms.Form
    Private SimulationFile As New APSIMFile
    Private SimulationExplorer As New ExplorerUI
    Private ToolboxFile As New APSIMFile
    Private ToolboxExplorer As New ExplorerUI


#Region " Windows Form Designer generated code "

    <System.STAThread()> _
    Public Shared Sub Main()

        Application.EnableVisualStyles()
        Application.DoEvents()
        Application.Run(New MainUI)
    End Sub 'Main


    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        Xceed.Grid.Licenser.LicenseKey = "GRD22-KTL57-34ZF5-W4JA"

        'Add any initialization after the InitializeComponent() call
        Application.EnableVisualStyles()

        Dim settings As New APSIMSettings
        Dim documentationFile As String = settings.GetSetting("apsimui", "docfile")
        HelpBrowser.Navigate(documentationFile)
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
    Friend WithEvents EmailButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents NewsButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents SeparatorButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents SimulationMenuRun As System.Windows.Forms.MenuItem
    Friend WithEvents HelpButtonMenu As System.Windows.Forms.ContextMenu
    Friend WithEvents MenuItem6 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem7 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem8 As System.Windows.Forms.MenuItem
    Friend WithEvents HelpMenuContents As System.Windows.Forms.MenuItem
    Friend WithEvents HelpMenuNew As System.Windows.Forms.MenuItem
    Friend WithEvents HelpMenuDefect As System.Windows.Forms.MenuItem
    Friend WithEvents HelpMenuChange As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem9 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem10 As System.Windows.Forms.MenuItem
    Friend WithEvents ToolboxPanel As System.Windows.Forms.Panel
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents SimulationPanel As System.Windows.Forms.Panel
    Friend WithEvents ToolboxMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents CaptionLabel As System.Windows.Forms.Label
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
        Me.ToolboxMenuItem = New System.Windows.Forms.MenuItem
        Me.ViewMenuHelp = New System.Windows.Forms.MenuItem
        Me.MenuItem2 = New System.Windows.Forms.MenuItem
        Me.ViewMenuOptions = New System.Windows.Forms.MenuItem
        Me.SimulationMenu = New System.Windows.Forms.MenuItem
        Me.SimulationMenuMake = New System.Windows.Forms.MenuItem
        Me.SimulationMenuRun = New System.Windows.Forms.MenuItem
        Me.HelpMenu = New System.Windows.Forms.MenuItem
        Me.HelpMenuContents = New System.Windows.Forms.MenuItem
        Me.HelpMenuNew = New System.Windows.Forms.MenuItem
        Me.HelpMenuDefect = New System.Windows.Forms.MenuItem
        Me.HelpMenuChange = New System.Windows.Forms.MenuItem
        Me.MenuItem9 = New System.Windows.Forms.MenuItem
        Me.HelpMenuAbout = New System.Windows.Forms.MenuItem
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
        Me.SeparatorButton = New System.Windows.Forms.ToolBarButton
        Me.NewsButton = New System.Windows.Forms.ToolBarButton
        Me.UIHelpButton = New System.Windows.Forms.ToolBarButton
        Me.HelpButtonMenu = New System.Windows.Forms.ContextMenu
        Me.MenuItem6 = New System.Windows.Forms.MenuItem
        Me.MenuItem7 = New System.Windows.Forms.MenuItem
        Me.MenuItem8 = New System.Windows.Forms.MenuItem
        Me.MenuItem10 = New System.Windows.Forms.MenuItem
        Me.ButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.PageSetupDialog1 = New System.Windows.Forms.PageSetupDialog
        Me.ContextMenu1 = New System.Windows.Forms.ContextMenu
        Me.TabMenuClose = New System.Windows.Forms.MenuItem
        Me.ComponentImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.HorizontalSplitter = New System.Windows.Forms.Splitter
        Me.HelpBrowserPanel = New System.Windows.Forms.Panel
        Me.HelpBrowser = New AxSHDocVw.AxWebBrowser
        Me.HelpBrowsertoolBar = New System.Windows.Forms.ToolBar
        Me.BackButton = New System.Windows.Forms.ToolBarButton
        Me.ForwardButton = New System.Windows.Forms.ToolBarButton
        Me.SmallButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.CaptionLabel = New System.Windows.Forms.Label
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog
        Me.SaveFileDialog = New System.Windows.Forms.SaveFileDialog
        Me.ToolboxPanel = New System.Windows.Forms.Panel
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.SimulationPanel = New System.Windows.Forms.Panel
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
        Me.MenuItem1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.ToolboxMenuItem, Me.ViewMenuHelp, Me.MenuItem2, Me.ViewMenuOptions})
        Me.MenuItem1.Text = "&View"
        '
        'ToolboxMenuItem
        '
        Me.ToolboxMenuItem.Index = 0
        Me.ToolboxMenuItem.Text = "Toolbox Window"
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
        Me.SimulationMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.SimulationMenuMake, Me.SimulationMenuRun})
        Me.SimulationMenu.Text = "&Simulation"
        '
        'SimulationMenuMake
        '
        Me.SimulationMenuMake.Index = 0
        Me.SimulationMenuMake.Text = "&Make Sim File"
        '
        'SimulationMenuRun
        '
        Me.SimulationMenuRun.Index = 1
        Me.SimulationMenuRun.Shortcut = System.Windows.Forms.Shortcut.F5
        Me.SimulationMenuRun.Text = "&Run"
        '
        'HelpMenu
        '
        Me.HelpMenu.Index = 4
        Me.HelpMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.HelpMenuContents, Me.HelpMenuNew, Me.HelpMenuDefect, Me.HelpMenuChange, Me.MenuItem9, Me.HelpMenuAbout})
        Me.HelpMenu.MergeOrder = 2
        Me.HelpMenu.Text = "&Help"
        '
        'HelpMenuContents
        '
        Me.HelpMenuContents.Index = 0
        Me.HelpMenuContents.Text = "Contents"
        '
        'HelpMenuNew
        '
        Me.HelpMenuNew.Index = 1
        Me.HelpMenuNew.Text = "News (via APSIM.Info)"
        '
        'HelpMenuDefect
        '
        Me.HelpMenuDefect.Index = 2
        Me.HelpMenuDefect.Text = "Report problem (vio APSIM.Info)"
        '
        'HelpMenuChange
        '
        Me.HelpMenuChange.Index = 3
        Me.HelpMenuChange.Text = "Suggest change (via APSIM.Info)"
        '
        'MenuItem9
        '
        Me.MenuItem9.Index = 4
        Me.MenuItem9.Text = "-"
        '
        'HelpMenuAbout
        '
        Me.HelpMenuAbout.Index = 5
        Me.HelpMenuAbout.Text = "&About ..."
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
        Me.ToolBar1.Size = New System.Drawing.Size(1152, 36)
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
        Me.EmailButton.Visible = False
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
        Me.RunButton.ImageIndex = 14
        Me.RunButton.Text = "Run"
        Me.RunButton.ToolTipText = "Run APSIM on the current simulation set"
        '
        'ExportButton
        '
        Me.ExportButton.ImageIndex = 7
        Me.ExportButton.Text = "Export"
        Me.ExportButton.ToolTipText = "Export the simulation set to sim file format"
        Me.ExportButton.Visible = False
        '
        'SeparatorButton
        '
        Me.SeparatorButton.Style = System.Windows.Forms.ToolBarButtonStyle.Separator
        '
        'NewsButton
        '
        Me.NewsButton.ImageIndex = 13
        Me.NewsButton.Text = "News"
        Me.NewsButton.ToolTipText = "Get the latest information from APSIM.info"
        Me.NewsButton.Visible = False
        '
        'UIHelpButton
        '
        Me.UIHelpButton.DropDownMenu = Me.HelpButtonMenu
        Me.UIHelpButton.ImageIndex = 6
        Me.UIHelpButton.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton
        Me.UIHelpButton.Text = "Help"
        Me.UIHelpButton.ToolTipText = "Get contextual help"
        '
        'HelpButtonMenu
        '
        Me.HelpButtonMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuItem6, Me.MenuItem7, Me.MenuItem8, Me.MenuItem10})
        '
        'MenuItem6
        '
        Me.MenuItem6.Index = 0
        Me.MenuItem6.Text = "Contents"
        '
        'MenuItem7
        '
        Me.MenuItem7.Index = 1
        Me.MenuItem7.Text = "News (via APSIM.Info)"
        '
        'MenuItem8
        '
        Me.MenuItem8.Index = 2
        Me.MenuItem8.Text = "Report a problem (via APSIM.Info)"
        '
        'MenuItem10
        '
        Me.MenuItem10.Index = 3
        Me.MenuItem10.Text = "Suggest a change (via APSIM.Info)"
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
        Me.HorizontalSplitter.Location = New System.Drawing.Point(0, 327)
        Me.HorizontalSplitter.Name = "HorizontalSplitter"
        Me.HorizontalSplitter.Size = New System.Drawing.Size(1152, 6)
        Me.HorizontalSplitter.TabIndex = 10
        Me.HorizontalSplitter.TabStop = False
        '
        'HelpBrowserPanel
        '
        Me.HelpBrowserPanel.Controls.Add(Me.HelpBrowser)
        Me.HelpBrowserPanel.Controls.Add(Me.HelpBrowsertoolBar)
        Me.HelpBrowserPanel.Controls.Add(Me.CaptionLabel)
        Me.HelpBrowserPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.HelpBrowserPanel.Location = New System.Drawing.Point(0, 333)
        Me.HelpBrowserPanel.Name = "HelpBrowserPanel"
        Me.HelpBrowserPanel.Size = New System.Drawing.Size(1152, 231)
        Me.HelpBrowserPanel.TabIndex = 11
        '
        'HelpBrowser
        '
        Me.HelpBrowser.ContainingControl = Me
        Me.HelpBrowser.Dock = System.Windows.Forms.DockStyle.Fill
        Me.HelpBrowser.Enabled = True
        Me.HelpBrowser.Location = New System.Drawing.Point(0, 46)
        Me.HelpBrowser.OcxState = CType(resources.GetObject("HelpBrowser.OcxState"), System.Windows.Forms.AxHost.State)
        Me.HelpBrowser.Size = New System.Drawing.Size(1152, 185)
        Me.HelpBrowser.TabIndex = 8
        '
        'HelpBrowsertoolBar
        '
        Me.HelpBrowsertoolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.HelpBrowsertoolBar.AutoSize = False
        Me.HelpBrowsertoolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.BackButton, Me.ForwardButton})
        Me.HelpBrowsertoolBar.DropDownArrows = True
        Me.HelpBrowsertoolBar.ImageList = Me.SmallButtonImageList
        Me.HelpBrowsertoolBar.Location = New System.Drawing.Point(0, 23)
        Me.HelpBrowsertoolBar.Name = "HelpBrowsertoolBar"
        Me.HelpBrowsertoolBar.ShowToolTips = True
        Me.HelpBrowsertoolBar.Size = New System.Drawing.Size(1152, 23)
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
        'CaptionLabel
        '
        Me.CaptionLabel.BackColor = System.Drawing.SystemColors.ControlDark
        Me.CaptionLabel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.CaptionLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.CaptionLabel.Font = New System.Drawing.Font("Tahoma", 7.8!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.CaptionLabel.ForeColor = System.Drawing.SystemColors.HighlightText
        Me.CaptionLabel.Location = New System.Drawing.Point(0, 0)
        Me.CaptionLabel.Name = "CaptionLabel"
        Me.CaptionLabel.Size = New System.Drawing.Size(1152, 23)
        Me.CaptionLabel.TabIndex = 10
        Me.CaptionLabel.Text = "Help Window"
        Me.CaptionLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'SaveFileDialog
        '
        Me.SaveFileDialog.Filter = "XML Files|*.xml|All Files|*.*"
        '
        'ToolboxPanel
        '
        Me.ToolboxPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolboxPanel.Location = New System.Drawing.Point(0, 95)
        Me.ToolboxPanel.Name = "ToolboxPanel"
        Me.ToolboxPanel.Size = New System.Drawing.Size(1152, 232)
        Me.ToolboxPanel.TabIndex = 12
        '
        'Splitter1
        '
        Me.Splitter1.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Splitter1.Location = New System.Drawing.Point(0, 92)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(1152, 3)
        Me.Splitter1.TabIndex = 13
        Me.Splitter1.TabStop = False
        '
        'SimulationPanel
        '
        Me.SimulationPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationPanel.Location = New System.Drawing.Point(0, 36)
        Me.SimulationPanel.Name = "SimulationPanel"
        Me.SimulationPanel.Size = New System.Drawing.Size(1152, 56)
        Me.SimulationPanel.TabIndex = 14
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(6, 15)
        Me.ClientSize = New System.Drawing.Size(1152, 564)
        Me.Controls.Add(Me.SimulationPanel)
        Me.Controls.Add(Me.Splitter1)
        Me.Controls.Add(Me.ToolboxPanel)
        Me.Controls.Add(Me.HorizontalSplitter)
        Me.Controls.Add(Me.HelpBrowserPanel)
        Me.Controls.Add(Me.ToolBar1)
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

    Sub OpenAPSIMFile(Optional ByVal filename As String = "")
        Try
            If filename = "" Then
                SimulationFile.Open()
                filename = SimulationFile.Filename
            Else
                SimulationFile.Open(filename)
            End If
            Directory.SetCurrentDirectory(Path.GetDirectoryName(filename))
            SimulationExplorer.Data = SimulationFile.data
            UpdateMainForm()

        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Cannot open APSIM file")
        End Try

    End Sub

    Sub OpenNewFile()
        Try
            SimulationFile = New APSIMFile
            SimulationFile.OpenNewDocument()
            SimulationExplorer.Data = SimulationFile.data
            UpdateMainForm()

        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error openinig document template")
        End Try
    End Sub
    Private Sub UpdateMainForm()

        Me.Text = SimulationFile.Filename
        'SimulationExplorer.DrawTree()
        'MainUImanager.FillSimulationExplorer(SimulationFile.Data)
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
            SimulationExplorer.UIManager.SaveDocument(SimulationFile)
            SimulationFile.Save()
        ElseIf e.Button Is UIHelpButton Then
            UpdateHelpBrowser()
        ElseIf e.Button Is NewsButton Then
            GetLatestNews()
        ElseIf e.Button Is RunButton Then
            RunSimulations()
        ElseIf e.Button Is ExportButton Then
            MakeSimFiles()
        End If
    End Sub


    Private Sub SimulationExplorer_DoubleClick(ByVal sender As System.Object, ByVal e As System.EventArgs)



    End Sub

    Private Sub FileMenuSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuSave.Click

        SimulationExplorer.UIManager.SaveDocument(SimulationFile)
        SimulationFile.Save()

    End Sub


    ' --------------------------------------------
    ' Form has been loaded - set ourselves up.
    ' --------------------------------------------
    Private Sub MainUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ' Show the Simulation Explorer.
        SimulationExplorer.TopLevel = False
        SimulationExplorer.Dock = DockStyle.Fill
        SimulationExplorer.Parent = SimulationPanel
        SimulationExplorer.Visible = True

        ' Setup but don't show the Toolbox Explorer.
        ToolboxExplorer.TopLevel = False
        ToolboxExplorer.Dock = DockStyle.Fill
        ToolboxExplorer.Parent = ToolboxPanel
        ToolboxExplorer.Visible = True
        ToolboxPanel.Visible = False

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
        ReadWindowPosition()
    End Sub


    ' --------------------------------------------------
    ' Main form is closing - save everything.
    ' --------------------------------------------------
    Private Sub MainUI_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        WriteWindowPosition()
        SimulationFile.Save()
        ToolboxFile.Save()
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
        MakeSimFiles()
    End Sub

    Private Sub MenuItem6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim inifile As New APSIMSettings
        Dim value As String
        value = inifile.GetSetting("Apsim", "left")
        inifile.SetSetting("Apsim", "left", value + "g")
    End Sub

    Private Sub UpdateHelpBrowser()
        'Dim type As String = MainUImanager.SimulationFile.GetDataType(SimulationExplorer.SelectedNode.FullPath)
        'MsgBox(type)

    End Sub


    ' -----------------------------------------------------
    ' Populate the toolbox menu
    ' -----------------------------------------------------
    Private Sub PopulateToolBoxContextMenu()
        Try
            Dim toolboxes As New Toolboxes
            toolBoxContextMenu.MenuItems.Clear()
            For Each Filename As String In toolboxes.Names
                Dim Item As New MenuItem(Filename)
                AddHandler Item.Click, AddressOf Me.ToolBoxes_Click
                toolBoxContextMenu.MenuItems.Add(Item)
            Next
        Catch e As Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building tool box menus")
        End Try
    End Sub


    ' --------------------------------------------
    ' User has clicked on a tool box - show it
    ' --------------------------------------------
    Private Sub ToolBoxes_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim toolboxes As New Toolboxes
        ToolboxMenuItem.Checked = True
        Dim filename As String = toolboxes.NameToFileName(sender.text)
        ToolboxFile.Open(filename)
        ToolboxExplorer.Data = ToolboxFile.data
        ToolboxExplorer.UIManager.ShowUI(ToolboxFile.data)
        ToolboxPanel.Visible = True
    End Sub


    ' -----------------------------------------
    ' User has clicked on "View toolbox Window" item
    ' Show/Hide toolbox window.
    ' -----------------------------------------
    Private Sub ToolboxMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolboxMenuItem.Click
        ToolboxMenuItem.Checked = Not ToolboxMenuItem.Checked
        ToolboxPanel.Visible = ToolboxMenuItem.Checked
        ToolboxFile.Save()
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

    Private Sub GetLatestNews()
        HelpBrowser.Navigate("www.apsim.info")
    End Sub



    Private Sub ToolBar2_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles HelpBrowsertoolBar.ButtonClick
        If e.Button Is ForwardButton Then
            HelpBrowser.GoForward()
        ElseIf e.Button Is BackButton Then
            HelpBrowser.GoBack()
        End If

    End Sub

    Private Sub FileMenuNew_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuNew.Click
        OpenNewFile()
    End Sub

    Private Sub MenuItem5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuItem5.Click
        SimulationFile.SaveAs()
    End Sub


    Private Sub SimulationMenuRun_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationMenuRun.Click
        RunSimulations()
    End Sub


    ' ----------------------------------------------
    ' Make an APSIM sim file for each simulation
    ' in the currently open simulation set.
    ' ----------------------------------------------
    Private Function MakeSimFiles() As StringCollection
        Try
            Dim SimFiles As New StringCollection
            SimulationFile.Save()
            Dim M As New Macro
            Dim inifile As New APSIMSettings
            If File.Exists(SimulationFile.Filename) Then
                Dim DirectoryName As String = Path.GetDirectoryName(SimulationFile.Filename)
                Dim FileName As String = inifile.GetSetting("apsimui", "macrofile")
                If File.Exists(FileName) Then
                    Dim sr As New StreamReader(FileName)
                    Dim MacroContents As String = sr.ReadToEnd
                    sr.Close()

                    SimFiles = M.Go(SimulationFile.data, MacroContents, DirectoryName)
                Else
                    MsgBox("Cannot find the simulation creation macro file", MsgBoxStyle.Critical, "Error")
                End If
            Else
                MsgBox("Cannot make simulation until apsim file has been saved to a target location.", MsgBoxStyle.Critical, "Error")
            End If
            Return SimFiles
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error")
        End Try
    End Function


    ' -----------------------------------------------
    ' Go run the currently open set of simulations
    ' -----------------------------------------------
    Private Sub RunSimulations()
        Dim SimFiles As StringCollection = MakeSimFiles()
        For Each simfile As String In SimFiles
            Dim CommandLine As String = Path.GetDirectoryName(Application.ExecutablePath) + "\apsrun.exe /run """ + simfile + """"
            Dim ID As Integer = Shell(CommandLine, AppWinStyle.NormalFocus)
        Next
    End Sub

    Private Sub ReadWindowPosition()
        Try
            Dim inifile As New APSIMSettings
            Dim windowstate As String = inifile.GetSetting("apsimui", "windowstate")
            Dim top As String = inifile.GetSetting("apsimui", "top")
            Dim left As String = inifile.GetSetting("apsimui", "left")
            Dim height As String = inifile.GetSetting("apsimui", "height")
            Dim width As String = inifile.GetSetting("apsimui", "width")

            If windowstate <> "" And top <> "" And left <> "" And height <> "" And width <> "" Then
                Me.WindowState = Val(windowstate)
                Me.Left = Val(left)
                Me.Top = Val(top)
                Me.Height = Val(height)
                Me.Width = Val(width)
            Else
                Me.WindowState = FormWindowState.Maximized
            End If

        Catch ex As Exception

        End Try
    End Sub
    Private Sub WriteWindowPosition()
        Try
            Dim inifile As New APSIMSettings
            inifile.SetSetting("apsimui", "windowstate", Str(Me.WindowState))
            inifile.SetSetting("apsimui", "top", Str(Me.Top))
            inifile.SetSetting("apsimui", "left", Str(Me.Left))
            inifile.SetSetting("apsimui", "width", Str(Me.Width))
            inifile.SetSetting("apsimui", "height", Str(Me.Height))
        Catch ex As Exception

        End Try
    End Sub
End Class
