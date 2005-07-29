Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.xml
Imports System.xml.XmlNodeList
Imports System.io
Imports System.IO.Path
Imports VBGeneral
Imports CSGeneral
Imports ChangeTool

Public Class MainUI
    Inherits System.Windows.Forms.Form
    Private UIManager As UIManager
    Private SimulationExplorer As ExplorerUI
    Private ToolboxExplorer As ExplorerUI

#Region " Windows Form Designer generated code "

    <System.STAThread()> _
    Public Shared Sub Main()


        Application.EnableVisualStyles()
        Application.DoEvents()

        Application.DoEvents()
        Application.Run(New MainUI)
        Application.DoEvents()

        Application.DoEvents()

    End Sub 'Main


    Public Sub New()

        MyBase.New()
        Dim splash As New SplashScreen
        splash.Show()
        Application.DoEvents()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        Xceed.Grid.Licenser.LicenseKey = "GRD22-KTL57-34ZF5-W4JA"
        Xceed.SmartUI.Licenser.LicenseKey = "SUN31-9TL57-SUXL5-F4BA"
        Xceed.Chart.Licenser.LicenseKey = "CHT30-YTL57-0UXLJ-145A"

        'Add any initialization after the InitializeComponent() call

        splash.Hide()
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
    Friend WithEvents MenuItem4 As System.Windows.Forms.MenuItem
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
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents FileMenu As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem1 As System.Windows.Forms.MenuItem
    Friend WithEvents HelpMenu As System.Windows.Forms.MenuItem
    Friend WithEvents EditMenu As System.Windows.Forms.MenuItem
    Friend WithEvents EditMenuCut As System.Windows.Forms.MenuItem
    Friend WithEvents EditMenuCopy As System.Windows.Forms.MenuItem
    Friend WithEvents EditMenuPaste As System.Windows.Forms.MenuItem
    Friend WithEvents SimulationMenu As System.Windows.Forms.MenuItem
    Friend WithEvents UIHelpButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ExportButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents CutButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents copyButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents PasteButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolBoxButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents HorizontalSplitter As System.Windows.Forms.Splitter
    Friend WithEvents toolBoxContextMenu As System.Windows.Forms.ContextMenu
    Friend WithEvents MenuItem2 As System.Windows.Forms.MenuItem
    Friend WithEvents ComponentImageList As System.Windows.Forms.ImageList
    Friend WithEvents HelpBrowser As AxSHDocVw.AxWebBrowser
    Friend WithEvents SmallButtonImageList As System.Windows.Forms.ImageList
    Friend WithEvents HelpBrowserPanel As System.Windows.Forms.Panel
    Friend WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Friend WithEvents SaveFileDialog As System.Windows.Forms.SaveFileDialog
    Friend WithEvents EmailButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents NewsButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents SeparatorButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents HelpButtonMenu As System.Windows.Forms.ContextMenu
    Friend WithEvents MenuItem9 As System.Windows.Forms.MenuItem
    Friend WithEvents ToolboxPanel As System.Windows.Forms.Panel
    Friend WithEvents SimulationPanel As System.Windows.Forms.Panel
    Friend WithEvents CaptionLabel As System.Windows.Forms.Label
    Friend WithEvents HelpToolBarPanel As System.Windows.Forms.Panel
    Friend WithEvents HelpBrowsertoolBar As System.Windows.Forms.ToolBar
    Friend WithEvents BackButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ForwardButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents CloseButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolBoxSplitter As System.Windows.Forms.Splitter
    Friend WithEvents FileNew As System.Windows.Forms.MenuItem
    Friend WithEvents FileOpen As System.Windows.Forms.MenuItem
    Friend WithEvents FileSave As System.Windows.Forms.MenuItem
    Friend WithEvents FileSaveAs As System.Windows.Forms.MenuItem
    Friend WithEvents FileExit As System.Windows.Forms.MenuItem
    Friend WithEvents MainMenu As System.Windows.Forms.MainMenu
    Friend WithEvents ToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents ViewToolboxWindow As System.Windows.Forms.MenuItem
    Friend WithEvents ViewHelpWindow As System.Windows.Forms.MenuItem
    Friend WithEvents ViewOptions As System.Windows.Forms.MenuItem
    Friend WithEvents SimulationMakeSimFile As System.Windows.Forms.MenuItem
    Friend WithEvents SimulationRun As System.Windows.Forms.MenuItem
    Friend WithEvents HelpContents As System.Windows.Forms.MenuItem
    Friend WithEvents HelpNews As System.Windows.Forms.MenuItem
    Friend WithEvents HelpReportProblem As System.Windows.Forms.MenuItem
    Friend WithEvents HelpSuggestChange As System.Windows.Forms.MenuItem
    Friend WithEvents HelpAbout As System.Windows.Forms.MenuItem
    Friend WithEvents HelpContentsTool As System.Windows.Forms.MenuItem
    Friend WithEvents HelpNewsTool As System.Windows.Forms.MenuItem
    Friend WithEvents HelpReportProblemTool As System.Windows.Forms.MenuItem
    Friend WithEvents HelpSuggestChangeTool As System.Windows.Forms.MenuItem
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MainUI))
        Me.MainMenu = New System.Windows.Forms.MainMenu
        Me.FileMenu = New System.Windows.Forms.MenuItem
        Me.FileNew = New System.Windows.Forms.MenuItem
        Me.FileOpen = New System.Windows.Forms.MenuItem
        Me.FileSave = New System.Windows.Forms.MenuItem
        Me.FileSaveAs = New System.Windows.Forms.MenuItem
        Me.MenuItem4 = New System.Windows.Forms.MenuItem
        Me.FileExit = New System.Windows.Forms.MenuItem
        Me.EditMenu = New System.Windows.Forms.MenuItem
        Me.EditMenuCut = New System.Windows.Forms.MenuItem
        Me.EditMenuCopy = New System.Windows.Forms.MenuItem
        Me.EditMenuPaste = New System.Windows.Forms.MenuItem
        Me.MenuItem1 = New System.Windows.Forms.MenuItem
        Me.ViewToolboxWindow = New System.Windows.Forms.MenuItem
        Me.ViewHelpWindow = New System.Windows.Forms.MenuItem
        Me.MenuItem2 = New System.Windows.Forms.MenuItem
        Me.ViewOptions = New System.Windows.Forms.MenuItem
        Me.SimulationMenu = New System.Windows.Forms.MenuItem
        Me.SimulationMakeSimFile = New System.Windows.Forms.MenuItem
        Me.SimulationRun = New System.Windows.Forms.MenuItem
        Me.HelpMenu = New System.Windows.Forms.MenuItem
        Me.HelpContents = New System.Windows.Forms.MenuItem
        Me.HelpNews = New System.Windows.Forms.MenuItem
        Me.HelpReportProblem = New System.Windows.Forms.MenuItem
        Me.HelpSuggestChange = New System.Windows.Forms.MenuItem
        Me.MenuItem9 = New System.Windows.Forms.MenuItem
        Me.HelpAbout = New System.Windows.Forms.MenuItem
        Me.ToolBar = New System.Windows.Forms.ToolBar
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
        Me.HelpContentsTool = New System.Windows.Forms.MenuItem
        Me.HelpNewsTool = New System.Windows.Forms.MenuItem
        Me.HelpReportProblemTool = New System.Windows.Forms.MenuItem
        Me.HelpSuggestChangeTool = New System.Windows.Forms.MenuItem
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
        Me.HelpToolBarPanel = New System.Windows.Forms.Panel
        Me.HelpBrowsertoolBar = New System.Windows.Forms.ToolBar
        Me.BackButton = New System.Windows.Forms.ToolBarButton
        Me.ForwardButton = New System.Windows.Forms.ToolBarButton
        Me.CloseButton = New System.Windows.Forms.ToolBarButton
        Me.SmallButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.CaptionLabel = New System.Windows.Forms.Label
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog
        Me.SaveFileDialog = New System.Windows.Forms.SaveFileDialog
        Me.ToolboxPanel = New System.Windows.Forms.Panel
        Me.ToolBoxSplitter = New System.Windows.Forms.Splitter
        Me.SimulationPanel = New System.Windows.Forms.Panel
        Me.HelpBrowserPanel.SuspendLayout()
        CType(Me.HelpBrowser, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.HelpToolBarPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'MainMenu
        '
        Me.MainMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.FileMenu, Me.EditMenu, Me.MenuItem1, Me.SimulationMenu, Me.HelpMenu})
        '
        'FileMenu
        '
        Me.FileMenu.Index = 0
        Me.FileMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.FileNew, Me.FileOpen, Me.FileSave, Me.FileSaveAs, Me.MenuItem4, Me.FileExit})
        Me.FileMenu.MergeType = System.Windows.Forms.MenuMerge.MergeItems
        Me.FileMenu.Text = "&File"
        '
        'FileNew
        '
        Me.FileNew.Index = 0
        Me.FileNew.Text = "&New ..."
        '
        'FileOpen
        '
        Me.FileOpen.Index = 1
        Me.FileOpen.Text = "&Open ..."
        '
        'FileSave
        '
        Me.FileSave.Index = 2
        Me.FileSave.Text = "&Save"
        '
        'FileSaveAs
        '
        Me.FileSaveAs.Index = 3
        Me.FileSaveAs.Text = "Save &As ..."
        '
        'MenuItem4
        '
        Me.MenuItem4.Index = 4
        Me.MenuItem4.MergeOrder = 2
        Me.MenuItem4.Text = "-"
        '
        'FileExit
        '
        Me.FileExit.Index = 5
        Me.FileExit.MergeOrder = 2
        Me.FileExit.Text = "E&xit"
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
        Me.MenuItem1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.ViewToolboxWindow, Me.ViewHelpWindow, Me.MenuItem2, Me.ViewOptions})
        Me.MenuItem1.Text = "&View"
        '
        'ViewToolboxWindow
        '
        Me.ViewToolboxWindow.Index = 0
        Me.ViewToolboxWindow.Text = "Toolbox Window"
        '
        'ViewHelpWindow
        '
        Me.ViewHelpWindow.Checked = True
        Me.ViewHelpWindow.Index = 1
        Me.ViewHelpWindow.Text = "Help Window"
        '
        'MenuItem2
        '
        Me.MenuItem2.Index = 2
        Me.MenuItem2.Text = "-"
        '
        'ViewOptions
        '
        Me.ViewOptions.Index = 3
        Me.ViewOptions.Text = "&Options ..."
        '
        'SimulationMenu
        '
        Me.SimulationMenu.Index = 3
        Me.SimulationMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.SimulationMakeSimFile, Me.SimulationRun})
        Me.SimulationMenu.Text = "&Simulation"
        '
        'SimulationMakeSimFile
        '
        Me.SimulationMakeSimFile.Index = 0
        Me.SimulationMakeSimFile.Text = "&Make Sim File"
        '
        'SimulationRun
        '
        Me.SimulationRun.Index = 1
        Me.SimulationRun.Shortcut = System.Windows.Forms.Shortcut.F5
        Me.SimulationRun.Text = "&Run"
        '
        'HelpMenu
        '
        Me.HelpMenu.Index = 4
        Me.HelpMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.HelpContents, Me.HelpNews, Me.HelpReportProblem, Me.HelpSuggestChange, Me.MenuItem9, Me.HelpAbout})
        Me.HelpMenu.MergeOrder = 2
        Me.HelpMenu.Text = "&Help"
        '
        'HelpContents
        '
        Me.HelpContents.Index = 0
        Me.HelpContents.Text = "Contents"
        '
        'HelpNews
        '
        Me.HelpNews.Index = 1
        Me.HelpNews.Text = "News (via APSIM.Info)"
        '
        'HelpReportProblem
        '
        Me.HelpReportProblem.Index = 2
        Me.HelpReportProblem.Text = "Report problem (vio APSIM.Info)"
        '
        'HelpSuggestChange
        '
        Me.HelpSuggestChange.Index = 3
        Me.HelpSuggestChange.Text = "Suggest change (via APSIM.Info)"
        '
        'MenuItem9
        '
        Me.MenuItem9.Index = 4
        Me.MenuItem9.Text = "-"
        '
        'HelpAbout
        '
        Me.HelpAbout.Index = 5
        Me.HelpAbout.Text = "&About ..."
        '
        'ToolBar
        '
        Me.ToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.FileNewButton, Me.FileOpenButton, Me.FileSaveButton, Me.EmailButton, Me.Separator1, Me.CutButton, Me.copyButton, Me.PasteButton, Me.Separator2, Me.ToolBoxButton, Me.RunButton, Me.ExportButton, Me.SeparatorButton, Me.NewsButton, Me.UIHelpButton})
        Me.ToolBar.DropDownArrows = True
        Me.ToolBar.ImageList = Me.ButtonImageList
        Me.ToolBar.Location = New System.Drawing.Point(0, 0)
        Me.ToolBar.Name = "ToolBar"
        Me.ToolBar.ShowToolTips = True
        Me.ToolBar.Size = New System.Drawing.Size(1015, 36)
        Me.ToolBar.TabIndex = 1
        Me.ToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
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
        Me.CutButton.Enabled = False
        Me.CutButton.ImageIndex = 3
        Me.CutButton.Text = "Cut"
        '
        'copyButton
        '
        Me.copyButton.Enabled = False
        Me.copyButton.ImageIndex = 4
        Me.copyButton.Text = "Copy"
        '
        'PasteButton
        '
        Me.PasteButton.Enabled = False
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
        Me.HelpButtonMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.HelpContentsTool, Me.HelpNewsTool, Me.HelpReportProblemTool, Me.HelpSuggestChangeTool})
        '
        'HelpContentsTool
        '
        Me.HelpContentsTool.Index = 0
        Me.HelpContentsTool.Text = "Contents"
        '
        'HelpNewsTool
        '
        Me.HelpNewsTool.Index = 1
        Me.HelpNewsTool.Text = "News (via APSIM.Info)"
        '
        'HelpReportProblemTool
        '
        Me.HelpReportProblemTool.Index = 2
        Me.HelpReportProblemTool.Text = "Report a problem (via APSIM.Info)"
        '
        'HelpSuggestChangeTool
        '
        Me.HelpSuggestChangeTool.Index = 3
        Me.HelpSuggestChangeTool.Text = "Suggest a change (via APSIM.Info)"
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
        Me.HorizontalSplitter.Location = New System.Drawing.Point(0, 355)
        Me.HorizontalSplitter.Name = "HorizontalSplitter"
        Me.HorizontalSplitter.Size = New System.Drawing.Size(1015, 6)
        Me.HorizontalSplitter.TabIndex = 10
        Me.HorizontalSplitter.TabStop = False
        '
        'HelpBrowserPanel
        '
        Me.HelpBrowserPanel.Controls.Add(Me.HelpBrowser)
        Me.HelpBrowserPanel.Controls.Add(Me.HelpToolBarPanel)
        Me.HelpBrowserPanel.Controls.Add(Me.CaptionLabel)
        Me.HelpBrowserPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.HelpBrowserPanel.Location = New System.Drawing.Point(0, 361)
        Me.HelpBrowserPanel.Name = "HelpBrowserPanel"
        Me.HelpBrowserPanel.Size = New System.Drawing.Size(1015, 200)
        Me.HelpBrowserPanel.TabIndex = 11
        '
        'HelpBrowser
        '
        Me.HelpBrowser.ContainingControl = Me
        Me.HelpBrowser.Dock = System.Windows.Forms.DockStyle.Fill
        Me.HelpBrowser.Enabled = True
        Me.HelpBrowser.Location = New System.Drawing.Point(0, 48)
        Me.HelpBrowser.OcxState = CType(resources.GetObject("HelpBrowser.OcxState"), System.Windows.Forms.AxHost.State)
        Me.HelpBrowser.Size = New System.Drawing.Size(1015, 152)
        Me.HelpBrowser.TabIndex = 8
        '
        'HelpToolBarPanel
        '
        Me.HelpToolBarPanel.Controls.Add(Me.HelpBrowsertoolBar)
        Me.HelpToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.HelpToolBarPanel.Location = New System.Drawing.Point(0, 20)
        Me.HelpToolBarPanel.Name = "HelpToolBarPanel"
        Me.HelpToolBarPanel.Size = New System.Drawing.Size(1015, 28)
        Me.HelpToolBarPanel.TabIndex = 11
        '
        'HelpBrowsertoolBar
        '
        Me.HelpBrowsertoolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.HelpBrowsertoolBar.AutoSize = False
        Me.HelpBrowsertoolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.BackButton, Me.ForwardButton, Me.CloseButton})
        Me.HelpBrowsertoolBar.DropDownArrows = True
        Me.HelpBrowsertoolBar.ImageList = Me.SmallButtonImageList
        Me.HelpBrowsertoolBar.Location = New System.Drawing.Point(0, 0)
        Me.HelpBrowsertoolBar.Name = "HelpBrowsertoolBar"
        Me.HelpBrowsertoolBar.ShowToolTips = True
        Me.HelpBrowsertoolBar.Size = New System.Drawing.Size(1015, 20)
        Me.HelpBrowsertoolBar.TabIndex = 10
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
        'CloseButton
        '
        Me.CloseButton.ImageIndex = 2
        Me.CloseButton.Text = "Close"
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
        Me.CaptionLabel.Size = New System.Drawing.Size(1015, 20)
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
        Me.ToolboxPanel.Location = New System.Drawing.Point(0, 203)
        Me.ToolboxPanel.Name = "ToolboxPanel"
        Me.ToolboxPanel.Size = New System.Drawing.Size(1015, 152)
        Me.ToolboxPanel.TabIndex = 12
        Me.ToolboxPanel.Visible = False
        '
        'ToolBoxSplitter
        '
        Me.ToolBoxSplitter.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolBoxSplitter.Enabled = False
        Me.ToolBoxSplitter.Location = New System.Drawing.Point(0, 201)
        Me.ToolBoxSplitter.Name = "ToolBoxSplitter"
        Me.ToolBoxSplitter.Size = New System.Drawing.Size(1015, 2)
        Me.ToolBoxSplitter.TabIndex = 13
        Me.ToolBoxSplitter.TabStop = False
        '
        'SimulationPanel
        '
        Me.SimulationPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationPanel.Location = New System.Drawing.Point(0, 36)
        Me.SimulationPanel.Name = "SimulationPanel"
        Me.SimulationPanel.Size = New System.Drawing.Size(1015, 165)
        Me.SimulationPanel.TabIndex = 14
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1015, 561)
        Me.Controls.Add(Me.SimulationPanel)
        Me.Controls.Add(Me.ToolBoxSplitter)
        Me.Controls.Add(Me.ToolboxPanel)
        Me.Controls.Add(Me.HorizontalSplitter)
        Me.Controls.Add(Me.HelpBrowserPanel)
        Me.Controls.Add(Me.ToolBar)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Menu = Me.MainMenu
        Me.Name = "MainUI"
        Me.Text = "APSIM"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.HelpBrowserPanel.ResumeLayout(False)
        CType(Me.HelpBrowser, System.ComponentModel.ISupportInitialize).EndInit()
        Me.HelpToolBarPanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' --------------------------------------------
    ' Form has been loaded - set ourselves up.
    ' --------------------------------------------
    Private Sub MainUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        UIManager = New UIManager(Me)

        ' Show the Simulation Explorer.
        SimulationExplorer = New ExplorerUI
        SimulationExplorer.ApplicationSettings = UIManager
        SimulationExplorer.TopLevel = False
        SimulationExplorer.Dock = DockStyle.Fill
        SimulationExplorer.Parent = SimulationPanel
        SimulationExplorer.Visible = True
        SimulationExplorer.Setup(Me, "APSIM files (*.apsim)|*.apsim|" + _
                                 "Toolbox files (*.xml)|*.xml|" + _
                                 "Soils files (*.soils)|*.soils|" + _
                                 "All files (*.*)|*.*", _
                                 ".apsim", "apsimui")
        SimulationExplorer.ShowUI(New StartupUI)
        SimulationExplorer.DataTreeCaption = "Empty simulation"

        ' Setup but don't show the Toolbox Explorer.
        ToolboxExplorer = New ExplorerUI
        ToolboxExplorer.ApplicationSettings = UIManager
        ToolboxExplorer.TopLevel = False
        ToolboxExplorer.Dock = DockStyle.Fill
        ToolboxExplorer.Parent = ToolboxPanel
        ToolboxExplorer.Visible = True
        ToolboxPanel.Visible = False
        ToolboxExplorer.Setup(Nothing, "Toolbox files (*.xml)|*.xml|" + _
                              "All files (*.*)|*.*", _
                              ".xml", "")

        ToolboxPanel.Height = Val(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "toolboxheight"))

        ' Show some help
        HelpContents_Click(Nothing, Nothing)

        ' Load a default file if one was specified on the command line.
        Dim separators As String = " "
        Dim commands As String = Microsoft.VisualBasic.Command()
        Dim args() As String = commands.Split(separators.ToCharArray)
        If args.Length = 1 Then
            If args(0).Length() > 0 Then
                Dim FileName As String = args(0).Replace("""", "")
                SimulationExplorer.FileOpen(FileName)
            End If
        End If

        ' populate the toolbox menu.
        PopulateToolBoxContextMenu()

        ' readjust window positions based on previously saved positions.
        ReadWindowPosition()
    End Sub


    ' --------------------------------------------------
    ' Main form is closing - save everything.
    ' --------------------------------------------------
    Private Sub MainUI_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        WriteWindowPosition()
        SimulationExplorer.FileSave()
        If ToolboxExplorer.Visible And _
            Path.GetFileNameWithoutExtension(ToolboxExplorer.FileName).ToLower() <> "standard" Then
            ToolboxExplorer.FileSave()
        End If
    End Sub


    ' --------------------------
    ' User has clicked File|New
    ' --------------------------
    Private Sub FileNew_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileNew.Click
        Dim NewData As APSIMData = UIManager.LetUserSelectNewDocument()
        If Not IsNothing(NewData) Then
            SimulationExplorer.FileNew(NewData)
        End If
    End Sub


    ' ---------------------------
    ' User has clicked File|Open
    ' ---------------------------
    Private Sub FileOpen_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileOpen.Click
        If SimulationExplorer.FileOpen() Then
            APSIMChangeTool.Upgrade(SimulationExplorer.Data)
            SimulationExplorer.Refresh()
        End If
    End Sub


    ' --------------------------
    ' User has clicked File|Save
    ' --------------------------
    Private Sub FileSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileSave.Click
        SimulationExplorer.FileSave()
    End Sub


    ' -----------------------------
    ' User has clicked File|SaveAs
    ' -----------------------------
    Private Sub FileSaveAs_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileSaveAs.Click
        SimulationExplorer.FileSaveAs()
    End Sub


    ' --------------------------
    ' User has clicked File|Exit
    ' --------------------------
    Private Sub FileExit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileExit.Click
        Close()
    End Sub


    ' ---------------------------------
    ' User has clicked a toolbar button
    ' ---------------------------------
    Private Sub ToolBar_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles ToolBar.ButtonClick, HelpBrowsertoolBar.ButtonClick
        If e.Button Is FileNewButton Then
            FileNew_Click(sender, e)
        ElseIf e.Button Is FileOpenButton Then
            FileOpen_Click(sender, e)
        ElseIf e.Button Is FileSaveButton Then
            FileSave_Click(sender, e)
        ElseIf e.Button Is UIHelpButton Then
            HelpContents_Click(sender, e)
        ElseIf e.Button Is NewsButton Then
            HelpNews_Click(sender, e)
        ElseIf e.Button Is RunButton Then
            SimulationRun_Click(sender, e)
        ElseIf e.Button Is ExportButton Then
            SimulationMakeSimFile_Click(sender, e)
        ElseIf e.Button Is ToolBoxButton Then
            If ViewToolboxWindow.Checked Then
                ViewToolboxWindow.PerformClick()
            Else
                If Not IsNothing(toolBoxContextMenu.MenuItems(0)) Then
                    toolBoxContextMenu.MenuItems(0).PerformClick()
                End If
            End If
        ElseIf e.Button Is ForwardButton Then
            HelpBrowser.GoForward()
        ElseIf e.Button Is BackButton Then
            HelpBrowser.GoBack()
        ElseIf e.Button Is CloseButton Then
            UIManager.ShowHelpBrowser(False)
        End If

    End Sub


    ' -----------------------------------------
    ' User has clicked on "View toolbox Window" item
    ' Show/Hide toolbox window.
    ' -----------------------------------------
    Private Sub ViewToolboxWindow_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewToolboxWindow.Click
        ViewToolboxWindow.Checked = Not ViewToolboxWindow.Checked
        ToolboxPanel.Visible = ViewToolboxWindow.Checked
        ToolBoxSplitter.Enabled = ViewToolboxWindow.Checked
        ToolboxExplorer.FileSave()
    End Sub


    ' -----------------------------------------
    ' User has clicked on "View help Window" item
    ' Show/Hide help window.
    ' -----------------------------------------
    Private Sub ViewHelpWindow_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewHelpWindow.Click
        ViewHelpWindow.Checked = Not ViewHelpWindow.Checked
        UIManager.ShowHelpBrowser(ViewHelpWindow.Checked)
    End Sub


    ' -----------------------------------------
    ' User has clicked on "View options" item
    ' -----------------------------------------
    Private Sub ViewOptions_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ViewOptions.Click
        Dim Form As New OptionsForm
        Form.ShowDialog(Me)
        PopulateToolBoxContextMenu()
    End Sub


    ' ---------------------------------------------
    ' User has clicked on Simulation|Make Sim File
    '  --------------------------------------------
    Private Sub SimulationMakeSimFile_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationMakeSimFile.Click
        MakeSimFiles()
    End Sub


    ' ---------------------------------------------
    ' User has clicked on Simulation|Run
    '  --------------------------------------------
    Private Sub SimulationRun_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationRun.Click
        RunSimulations()
    End Sub


    ' ----------------------------------
    ' User has clicked on Help|Contents
    ' ----------------------------------
    Private Sub HelpContents_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpContents.Click, HelpContentsTool.Click
        UIManager.ShowHelp(APSIMSettings.ApsimDirectory + "\docs\documentation.xml")
    End Sub


    ' -------------------------------------
    ' User has clicked on Help | News
    ' -------------------------------------
    Private Sub HelpNews_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpNews.Click, HelpNewsTool.Click
        UIManager.ShowHelp("www.apsim.info")
    End Sub


    ' -----------------------------------------
    ' User has clicked on Help | Report problem
    ' -----------------------------------------
    Private Sub HelpReportProblem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpReportProblem.Click, HelpReportProblemTool.Click
        UIManager.ShowHelp("www.apsim.info/apsim/helpdesk/submit-defect.asp")
    End Sub


    ' -----------------------------------------
    ' User has clicked on Help | Suggest Change
    ' -----------------------------------------
    Private Sub HelpSuggestChange_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpSuggestChange.Click, HelpSuggestChangeTool.Click
        UIManager.ShowHelp("www.apsim.info/apsim/helpdesk/submit-change.asp")
    End Sub


    ' --------------------------------
    ' User has clicked on Help | About
    ' --------------------------------
    Private Sub HelpAbout_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpAbout.Click
        Dim Form As New HelpAboutForm
        Form.ShowDialog()
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
        Catch e As System.Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building tool box menus")
        End Try
    End Sub


    ' --------------------------------------------
    ' User has clicked on a tool box - show it
    ' --------------------------------------------
    Private Sub ToolBoxes_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        ToolboxExplorer.CloseUI()

        Dim toolboxes As New Toolboxes
        ViewToolboxWindow.Checked = True
        Dim inifile As New APSIMSettings
        ToolboxPanel.Height = Val(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "toolboxheight"))

        Dim filename As String = toolboxes.NameToFileName(sender.text)
        ToolboxExplorer.FileOpen(filename)
        ToolboxPanel.Visible = True
        ToolBoxSplitter.Enabled = ViewToolboxWindow.Checked

        ToolboxPanel.Height = ToolboxPanel.Height + 1
        ToolboxPanel.Height = ToolboxPanel.Height - 1
    End Sub


    ' ----------------------------------------------
    ' Make an APSIM sim file for each simulation
    ' in the currently open simulation set.
    ' ----------------------------------------------
    Private Function MakeSimFiles() As StringCollection
        Dim inifile As New APSIMSettings
        Dim TypesData As New APSIMData
        TypesData.LoadFromFile(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "typesfile"))

        Try
            Dim SimFiles As New StringCollection
            SimulationExplorer.Save()

            Dim M As New Macro
            If File.Exists(SimulationExplorer.FileName) Then
                Dim DirectoryName As String = Path.GetDirectoryName(SimulationExplorer.FileName)
                Dim FileName As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "macrofile")
                If File.Exists(FileName) Then
                    For Each Sim As APSIMData In SimulationExplorer.Data.Children("simulation")
                        Dim MacroContents As String = CreateMacroFile(TypesData, Sim, 1)
                        SimFiles = M.Go(SimulationExplorer.Data, MacroContents, DirectoryName)
                    Next
                Else
                    MsgBox("Cannot find the simulation creation macro file", MsgBoxStyle.Critical, "Error")
                End If
            Else
                ' MsgBox("Cannot make simulation until apsim file has been saved to a target location.", MsgBoxStyle.Critical, "Error")
            End If
            Return SimFiles
        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error")
        End Try
    End Function


    ' -----------------------------------------------
    ' Create a macro file by going through the specified
    ' simulation and for each node, goto types.xml and
    ' pull out the SimMacro for that node.
    ' -----------------------------------------------
    Private Function CreateMacroFile(ByVal TypesData As APSIMData, ByVal Data As APSIMData, ByVal Level As Integer) As String
        Dim MacroString As String
        Dim TypesNode As APSIMData = TypesData.Child(Data.Type)
        If Not IsNothing(TypesNode) Then
            Dim SimMacroNode As APSIMData = TypesNode.Child("SimMacro")
            If Not IsNothing(SimMacroNode) Then
                MacroString = SimMacroNode.Value
                MacroString = StringManip.UnIndentText(MacroString, 9)
                MacroString = StringManip.IndentText(MacroString, (Level - 1) * 3)
                If Not IsNothing(MacroString) Then
                    For Each Child As APSIMData In Data.Children()
                        Dim ChildrenMacroString As String = CreateMacroFile(TypesData, Child, Level + 1)
                        If Not IsNothing(ChildrenMacroString) Then
                            Dim SearchString As String = "[insert " + Child.Type + "]"
                            ChildrenMacroString = ChildrenMacroString.TrimStart()
                            MacroString = MacroString.Replace(SearchString, ChildrenMacroString)
                        End If
                    Next
                End If
                'ChildrenMacroString = StringManip.UnIndentText(ChildrenMacroString, 9)
                'ChildrenMacroString = StringManip.IndentText(ChildrenMacroString, Level * 3)
            End If
        End If

        Return MacroString
    End Function


    ' -----------------------------------------------
    ' Go run the currently open set of simulations
    ' -----------------------------------------------
    Private Sub RunSimulations()
        SimulationExplorer.FileSave()

        ' kill old apsim processes
        Dim AllProcesses As Process() = Process.GetProcesses()
        For Each proc As Process In AllProcesses
            If Path.GetFileName(proc.ProcessName) = "apsim" Then
                proc.Kill()
            End If
        Next

        ' create a .run file to pass to apsrun.
        Dim TempFileName As String = Path.GetTempPath() + "\\apsimui.run"
        Dim Out As New StreamWriter(TempFileName)
        Dim SimFiles As StringCollection = MakeSimFiles()
        For Each simfile As String In SimFiles
            If Path.GetExtension(simfile) = ".sim" Then
                Out.WriteLine("[" + simfile + "]")
                Out.WriteLine("Simulation_file=" + simfile)
            End If
        Next
        Out.Close()

        Dim CommandLine As String
        If SimFiles.Count = 1 Then
            CommandLine = "/run "
        Else
            CommandLine = ""
        End If
        CommandLine = CommandLine + TempFileName

        Dim ApsRunFileName As String = Path.GetDirectoryName(Application.ExecutablePath) + "\apsrun.exe"
        Dim ApsimStartInfo As New ProcessStartInfo(ApsRunFileName, CommandLine)
        Dim ApsimProcess As New Process
        ApsimProcess.StartInfo = ApsimStartInfo
        ApsimProcess.Start()

    End Sub


    ' --------------------------------------------
    ' Read in position of windows and move windows
    ' around to reflect saved locations.
    ' --------------------------------------------
    Private Sub ReadWindowPosition()
        Try
            Dim inifile As New APSIMSettings
            Dim windowstate As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "windowstate")
            Dim top As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "top")
            Dim left As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "left")
            Dim height As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "height")
            Dim width As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "width")

            If windowstate <> "" And top <> "" And left <> "" And height <> "" And width <> "" Then
                Me.WindowState = Val(windowstate)
                Me.Left = Val(left)
                Me.Top = Val(top)
                Me.Height = Val(height)
                Me.Width = Val(width)
            Else
                Me.WindowState = FormWindowState.Maximized
            End If

        Catch ex As System.Exception

        End Try
    End Sub


    ' --------------------------------------------
    ' Write out position of windows
    ' --------------------------------------------
    Private Sub WriteWindowPosition()
        Try
            Dim inifile As New APSIMSettings
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "windowstate", Str(Me.WindowState))
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "top", Str(Me.Top))
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "left", Str(Me.Left))
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "width", Str(Me.Width))
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "height", Str(Me.Height))
        Catch ex As System.Exception

        End Try
    End Sub


    ' --------------------------------------------
    ' User has repositioned the help window. Save
    ' location.
    ' --------------------------------------------
    Private Sub HelpBrowserPanel_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles HelpBrowserPanel.Resize
        If HelpBrowserPanel.Visible = True Then
            Dim inifile As New APSIMSettings
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "helpheight", Str(HelpBrowserPanel.Height))
        End If
    End Sub


    ' --------------------------------------------
    ' User has repositioned the toolbox window. Save
    ' location.
    ' --------------------------------------------
    Private Sub ToolBoxSplitter_LocationChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolBoxSplitter.LocationChanged
        If ToolboxPanel.Visible Then
            Dim inifile As New APSIMSettings
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "toolboxheight", Str(ToolboxPanel.Height))
        End If
    End Sub

End Class
