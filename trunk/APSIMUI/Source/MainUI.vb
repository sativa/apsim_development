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
Imports Xceed.SmartUI

Public Class MainUI
    Inherits System.Windows.Forms.Form

    Private Declare Ansi Sub excelFiles Lib "ApsimContextMenu.dll" _
        Alias "excelFiles" (ByVal outFileList As String)

    Private Declare Ansi Sub apsvisFiles Lib "ApsimContextMenu.dll" _
            Alias "apsvisFiles" (ByVal outFileList As String)

    Private Declare Ansi Sub apsimoutlookFiles Lib "ApsimContextMenu.dll" _
            Alias "apsimoutlookFiles" (ByVal outFileList As String)




    Private ApsimUI As ApsimUIController
    Private Toolbox As ApsimUIController
    Private SimulationExplorer As ExplorerUI
    Private ToolboxExplorer As ExplorerUI
    Private ToolBoxSplitterPoint As Integer

#Region "Constructor / Destructor / Main"
    <System.STAThread()> _
    Public Shared Sub Main()
        Application.EnableVisualStyles()
        Application.DoEvents()
        Application.DoEvents()
        Application.Run(New MainUI)
        Application.DoEvents()
        Application.DoEvents()
    End Sub
    Public Sub New()
        MyBase.New()

        Dim splash As New SplashScreenForm
        splash.VersionText = "Version " & New VBGeneral.APSIMSettings().ApsimVersion
        splash.Show()

        Xceed.Chart.Licenser.LicenseKey = "CHT30-YTL57-0UXLJ-145A"
        Xceed.SmartUI.Licenser.LicenseKey = "SUN31-9TL57-SUXL5-F4BA"

        Application.DoEvents()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        ' Position window correctly.
        Try
            Dim inifile As New APSIMSettings
            WindowState = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "windowstate"))
            Top = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "top"))
            Left = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "left"))
            Height = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "height"))
            Width = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "width"))
        Catch ex As System.Exception
            Me.WindowState = FormWindowState.Maximized
        End Try

    End Sub
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub
#End Region
#Region "Windows Form Designer generated code "


    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.
    'Do not modify it using the code editor.
    Friend WithEvents ButtonImageList As System.Windows.Forms.ImageList
    Friend WithEvents toolBoxContextMenu As System.Windows.Forms.ContextMenu
    Friend WithEvents ToolBoxSplitter As System.Windows.Forms.Splitter
    Friend WithEvents SimulationToolBarPanel As System.Windows.Forms.Panel
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents SimulationPanel2 As System.Windows.Forms.Panel
    Friend WithEvents SimulationToolBar2 As System.Windows.Forms.ToolBar
    Friend WithEvents SimulationLabel2 As System.Windows.Forms.Label
    Friend WithEvents Splitter2 As System.Windows.Forms.Splitter
    Friend WithEvents SimulationPanel1 As System.Windows.Forms.Panel
    Friend WithEvents SimulationToolBar1 As System.Windows.Forms.ToolBar
    Friend WithEvents SimulationLabel1 As System.Windows.Forms.Label
    Friend WithEvents RunButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents GraphButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ApsimOutlookButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolboxesToolbarPanel As System.Windows.Forms.Panel
    Friend WithEvents Panel3 As System.Windows.Forms.Panel
    Friend WithEvents ToolboxLabel As System.Windows.Forms.Label
    Friend WithEvents Splitter3 As System.Windows.Forms.Splitter
    Friend WithEvents HelpPanel As System.Windows.Forms.Panel
    Friend WithEvents HelpToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents HelpLabel As System.Windows.Forms.Label
    Friend WithEvents Splitter7 As System.Windows.Forms.Splitter
    Friend WithEvents ManageToolBoxesButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ApsimHelpButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolBoxToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents HelpImageList As System.Windows.Forms.ImageList
    Friend WithEvents ToolBoxPanel As System.Windows.Forms.Panel
    Friend WithEvents ToolBoxPanelToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents ToolBoxToolBarPanel As System.Windows.Forms.Panel
    Friend WithEvents FileMenu As Xceed.SmartUI.Controls.ToolBar.MenuTool
    Friend WithEvents NewFileMenu As Xceed.SmartUI.Controls.MenuBar.MenuItem
    Friend WithEvents OpenFileMenu As Xceed.SmartUI.Controls.MenuBar.MenuItem
    Friend WithEvents SaveFileMenu As Xceed.SmartUI.Controls.MenuBar.MenuItem
    Friend WithEvents SaveAsMenu As Xceed.SmartUI.Controls.MenuBar.MenuItem
    Friend WithEvents separatorMenuItem1 As Xceed.SmartUI.Controls.MenuBar.SeparatorMenuItem
    Friend WithEvents ExitMenu As Xceed.SmartUI.Controls.MenuBar.MenuItem
    Friend WithEvents separatorTool2 As Xceed.SmartUI.Controls.ToolBar.SeparatorTool
    Friend WithEvents SaveSmallButton As Xceed.SmartUI.Controls.ToolBar.Tool
    Friend WithEvents CutSmallButton As Xceed.SmartUI.Controls.ToolBar.Tool
    Friend WithEvents CopySmallButton As Xceed.SmartUI.Controls.ToolBar.Tool
    Friend WithEvents PasteSmallButton As Xceed.SmartUI.Controls.ToolBar.Tool
    Friend WithEvents separatorTool1 As Xceed.SmartUI.Controls.ToolBar.SeparatorTool
    Friend WithEvents SimulationButton As Xceed.SmartUI.Controls.ToolBar.Tool
    Friend WithEvents SmallImages As System.Windows.Forms.ImageList
    Friend WithEvents MainToolBar As Xceed.SmartUI.Controls.ToolBar.SmartToolBar
    Friend WithEvents ToolboxButton As Xceed.SmartUI.Controls.ToolBar.Tool
    Friend WithEvents ExcelToolBarButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents PrintDialog1 As System.Windows.Forms.PrintDialog
    Friend WithEvents RecentSimulationsMenu As Xceed.SmartUI.Controls.MenuBar.PopupMenuItem
    Friend WithEvents btnClose As System.Windows.Forms.Button
    Friend WithEvents btnMaximise As System.Windows.Forms.Button
    Friend WithEvents WindowToolTip As System.Windows.Forms.ToolTip
    Friend WithEvents btnRestore As System.Windows.Forms.Button
    Friend WithEvents ToolboxesToolBarHelp As System.Windows.Forms.ToolBar
    Friend WithEvents ToolboxesToolbarHelpButton As System.Windows.Forms.ToolBarButton
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MainUI))
        Me.toolBoxContextMenu = New System.Windows.Forms.ContextMenu
        Me.ButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.HelpImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.ToolBoxPanel = New System.Windows.Forms.Panel
        Me.ToolBoxToolBarPanel = New System.Windows.Forms.Panel
        Me.btnMaximise = New System.Windows.Forms.Button
        Me.btnRestore = New System.Windows.Forms.Button
        Me.btnClose = New System.Windows.Forms.Button
        Me.ToolBoxPanelToolBar = New System.Windows.Forms.ToolBar
        Me.ToolBoxSplitter = New System.Windows.Forms.Splitter
        Me.SimulationToolBarPanel = New System.Windows.Forms.Panel
        Me.Splitter7 = New System.Windows.Forms.Splitter
        Me.Splitter2 = New System.Windows.Forms.Splitter
        Me.SimulationPanel1 = New System.Windows.Forms.Panel
        Me.SimulationToolBar1 = New System.Windows.Forms.ToolBar
        Me.GraphButton = New System.Windows.Forms.ToolBarButton
        Me.ApsimOutlookButton = New System.Windows.Forms.ToolBarButton
        Me.ExcelToolBarButton = New System.Windows.Forms.ToolBarButton
        Me.SimulationLabel1 = New System.Windows.Forms.Label
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.SimulationPanel2 = New System.Windows.Forms.Panel
        Me.SimulationToolBar2 = New System.Windows.Forms.ToolBar
        Me.RunButton = New System.Windows.Forms.ToolBarButton
        Me.SimulationLabel2 = New System.Windows.Forms.Label
        Me.HelpPanel = New System.Windows.Forms.Panel
        Me.HelpToolBar = New System.Windows.Forms.ToolBar
        Me.ApsimHelpButton = New System.Windows.Forms.ToolBarButton
        Me.HelpLabel = New System.Windows.Forms.Label
        Me.ToolboxesToolbarPanel = New System.Windows.Forms.Panel
        Me.Splitter3 = New System.Windows.Forms.Splitter
        Me.Panel3 = New System.Windows.Forms.Panel
        Me.ToolboxesToolBarHelp = New System.Windows.Forms.ToolBar
        Me.ToolboxesToolbarHelpButton = New System.Windows.Forms.ToolBarButton
        Me.ToolBoxToolBar = New System.Windows.Forms.ToolBar
        Me.ManageToolBoxesButton = New System.Windows.Forms.ToolBarButton
        Me.ToolboxLabel = New System.Windows.Forms.Label
        Me.MainToolBar = New Xceed.SmartUI.Controls.ToolBar.SmartToolBar(Me.components)
        Me.FileMenu = New Xceed.SmartUI.Controls.ToolBar.MenuTool("&File")
        Me.NewFileMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("&New file", 5)
        Me.OpenFileMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("&Open file ...", 6)
        Me.SaveFileMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("&Save file", 7)
        Me.SaveAsMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("Save &As file ...", 8)
        Me.RecentSimulationsMenu = New Xceed.SmartUI.Controls.MenuBar.PopupMenuItem("Recent Simulations", 14)
        Me.separatorMenuItem1 = New Xceed.SmartUI.Controls.MenuBar.SeparatorMenuItem
        Me.ExitMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("E&xit", 15)
        Me.separatorTool2 = New Xceed.SmartUI.Controls.ToolBar.SeparatorTool
        Me.SaveSmallButton = New Xceed.SmartUI.Controls.ToolBar.Tool(7)
        Me.CutSmallButton = New Xceed.SmartUI.Controls.ToolBar.Tool(9)
        Me.CopySmallButton = New Xceed.SmartUI.Controls.ToolBar.Tool(10)
        Me.PasteSmallButton = New Xceed.SmartUI.Controls.ToolBar.Tool(11)
        Me.separatorTool1 = New Xceed.SmartUI.Controls.ToolBar.SeparatorTool
        Me.SimulationButton = New Xceed.SmartUI.Controls.ToolBar.Tool("&Simulation")
        Me.ToolboxButton = New Xceed.SmartUI.Controls.ToolBar.Tool("&Toolboxes")
        Me.SmallImages = New System.Windows.Forms.ImageList(Me.components)
        Me.PrintDialog1 = New System.Windows.Forms.PrintDialog
        Me.WindowToolTip = New System.Windows.Forms.ToolTip(Me.components)
        Me.ToolBoxPanel.SuspendLayout()
        Me.ToolBoxToolBarPanel.SuspendLayout()
        Me.SimulationToolBarPanel.SuspendLayout()
        Me.SimulationPanel1.SuspendLayout()
        Me.SimulationPanel2.SuspendLayout()
        Me.HelpPanel.SuspendLayout()
        Me.ToolboxesToolbarPanel.SuspendLayout()
        Me.Panel3.SuspendLayout()
        Me.SuspendLayout()
        '
        'ButtonImageList
        '
        Me.ButtonImageList.ImageSize = New System.Drawing.Size(24, 24)
        Me.ButtonImageList.ImageStream = CType(resources.GetObject("ButtonImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ButtonImageList.TransparentColor = System.Drawing.SystemColors.Control
        '
        'HelpImageList
        '
        Me.HelpImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.HelpImageList.ImageStream = CType(resources.GetObject("HelpImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.HelpImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'ToolBoxPanel
        '
        Me.ToolBoxPanel.Controls.Add(Me.ToolBoxToolBarPanel)
        Me.ToolBoxPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolBoxPanel.Location = New System.Drawing.Point(0, 442)
        Me.ToolBoxPanel.Name = "ToolBoxPanel"
        Me.ToolBoxPanel.Size = New System.Drawing.Size(632, 104)
        Me.ToolBoxPanel.TabIndex = 12
        Me.ToolBoxPanel.Visible = False
        '
        'ToolBoxToolBarPanel
        '
        Me.ToolBoxToolBarPanel.BackColor = System.Drawing.Color.LightSteelBlue
        Me.ToolBoxToolBarPanel.Controls.Add(Me.btnMaximise)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.btnRestore)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.btnClose)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.ToolBoxPanelToolBar)
        Me.ToolBoxToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ToolBoxToolBarPanel.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxToolBarPanel.Name = "ToolBoxToolBarPanel"
        Me.ToolBoxToolBarPanel.Size = New System.Drawing.Size(632, 32)
        Me.ToolBoxToolBarPanel.TabIndex = 19
        '
        'btnMaximise
        '
        Me.btnMaximise.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnMaximise.BackColor = System.Drawing.Color.Transparent
        Me.btnMaximise.BackgroundImage = CType(resources.GetObject("btnMaximise.BackgroundImage"), System.Drawing.Image)
        Me.btnMaximise.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.btnMaximise.Location = New System.Drawing.Point(576, 3)
        Me.btnMaximise.Name = "btnMaximise"
        Me.btnMaximise.Size = New System.Drawing.Size(24, 24)
        Me.btnMaximise.TabIndex = 21
        Me.btnMaximise.TabStop = False
        Me.WindowToolTip.SetToolTip(Me.btnMaximise, "Maximise")
        '
        'btnRestore
        '
        Me.btnRestore.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnRestore.BackColor = System.Drawing.Color.Transparent
        Me.btnRestore.BackgroundImage = CType(resources.GetObject("btnRestore.BackgroundImage"), System.Drawing.Image)
        Me.btnRestore.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.btnRestore.Location = New System.Drawing.Point(576, 3)
        Me.btnRestore.Name = "btnRestore"
        Me.btnRestore.Size = New System.Drawing.Size(24, 24)
        Me.btnRestore.TabIndex = 22
        Me.btnRestore.TabStop = False
        Me.WindowToolTip.SetToolTip(Me.btnRestore, "Restore")
        '
        'btnClose
        '
        Me.btnClose.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnClose.BackColor = System.Drawing.Color.Transparent
        Me.btnClose.BackgroundImage = CType(resources.GetObject("btnClose.BackgroundImage"), System.Drawing.Image)
        Me.btnClose.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.btnClose.Location = New System.Drawing.Point(603, 3)
        Me.btnClose.Name = "btnClose"
        Me.btnClose.Size = New System.Drawing.Size(24, 24)
        Me.btnClose.TabIndex = 20
        Me.btnClose.TabStop = False
        Me.WindowToolTip.SetToolTip(Me.btnClose, "Close")
        '
        'ToolBoxPanelToolBar
        '
        Me.ToolBoxPanelToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBoxPanelToolBar.Divider = False
        Me.ToolBoxPanelToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ToolBoxPanelToolBar.DropDownArrows = True
        Me.ToolBoxPanelToolBar.ImageList = Me.ButtonImageList
        Me.ToolBoxPanelToolBar.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxPanelToolBar.Name = "ToolBoxPanelToolBar"
        Me.ToolBoxPanelToolBar.ShowToolTips = True
        Me.ToolBoxPanelToolBar.Size = New System.Drawing.Size(632, 26)
        Me.ToolBoxPanelToolBar.TabIndex = 17
        Me.ToolBoxPanelToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        '
        'ToolBoxSplitter
        '
        Me.ToolBoxSplitter.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolBoxSplitter.Location = New System.Drawing.Point(0, 438)
        Me.ToolBoxSplitter.Name = "ToolBoxSplitter"
        Me.ToolBoxSplitter.Size = New System.Drawing.Size(632, 4)
        Me.ToolBoxSplitter.TabIndex = 13
        Me.ToolBoxSplitter.TabStop = False
        Me.ToolBoxSplitter.Visible = False
        '
        'SimulationToolBarPanel
        '
        Me.SimulationToolBarPanel.BackColor = System.Drawing.Color.LightSteelBlue
        Me.SimulationToolBarPanel.Controls.Add(Me.Splitter7)
        Me.SimulationToolBarPanel.Controls.Add(Me.Splitter2)
        Me.SimulationToolBarPanel.Controls.Add(Me.SimulationPanel1)
        Me.SimulationToolBarPanel.Controls.Add(Me.Splitter1)
        Me.SimulationToolBarPanel.Controls.Add(Me.SimulationPanel2)
        Me.SimulationToolBarPanel.Controls.Add(Me.HelpPanel)
        Me.SimulationToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.SimulationToolBarPanel.Location = New System.Drawing.Point(0, 26)
        Me.SimulationToolBarPanel.Name = "SimulationToolBarPanel"
        Me.SimulationToolBarPanel.Size = New System.Drawing.Size(632, 71)
        Me.SimulationToolBarPanel.TabIndex = 19
        '
        'Splitter7
        '
        Me.Splitter7.BackColor = System.Drawing.Color.LightGray
        Me.Splitter7.Enabled = False
        Me.Splitter7.Location = New System.Drawing.Point(274, 0)
        Me.Splitter7.Name = "Splitter7"
        Me.Splitter7.Size = New System.Drawing.Size(1, 71)
        Me.Splitter7.TabIndex = 27
        Me.Splitter7.TabStop = False
        '
        'Splitter2
        '
        Me.Splitter2.BackColor = System.Drawing.Color.LightGray
        Me.Splitter2.Enabled = False
        Me.Splitter2.Location = New System.Drawing.Point(273, 0)
        Me.Splitter2.Name = "Splitter2"
        Me.Splitter2.Size = New System.Drawing.Size(1, 71)
        Me.Splitter2.TabIndex = 21
        Me.Splitter2.TabStop = False
        '
        'SimulationPanel1
        '
        Me.SimulationPanel1.BackColor = System.Drawing.Color.Transparent
        Me.SimulationPanel1.Controls.Add(Me.SimulationToolBar1)
        Me.SimulationPanel1.Controls.Add(Me.SimulationLabel1)
        Me.SimulationPanel1.Dock = System.Windows.Forms.DockStyle.Left
        Me.SimulationPanel1.Location = New System.Drawing.Point(65, 0)
        Me.SimulationPanel1.Name = "SimulationPanel1"
        Me.SimulationPanel1.Size = New System.Drawing.Size(208, 71)
        Me.SimulationPanel1.TabIndex = 20
        '
        'SimulationToolBar1
        '
        Me.SimulationToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.SimulationToolBar1.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.GraphButton, Me.ApsimOutlookButton, Me.ExcelToolBarButton})
        Me.SimulationToolBar1.Divider = False
        Me.SimulationToolBar1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationToolBar1.DropDownArrows = True
        Me.SimulationToolBar1.ImageList = Me.ButtonImageList
        Me.SimulationToolBar1.Location = New System.Drawing.Point(0, 20)
        Me.SimulationToolBar1.Name = "SimulationToolBar1"
        Me.SimulationToolBar1.ShowToolTips = True
        Me.SimulationToolBar1.Size = New System.Drawing.Size(208, 48)
        Me.SimulationToolBar1.TabIndex = 16
        '
        'GraphButton
        '
        Me.GraphButton.ImageIndex = 15
        Me.GraphButton.Text = "&Graph"
        Me.GraphButton.ToolTipText = "Graph simulation output files"
        '
        'ApsimOutlookButton
        '
        Me.ApsimOutlookButton.ImageIndex = 16
        Me.ApsimOutlookButton.Text = "&ApsimOutlook"
        Me.ApsimOutlookButton.ToolTipText = "Graph output files using ApsimOutlook"
        '
        'ExcelToolBarButton
        '
        Me.ExcelToolBarButton.ImageIndex = 23
        Me.ExcelToolBarButton.Text = "&Excel"
        Me.ExcelToolBarButton.ToolTipText = "Export output file to Excel."
        '
        'SimulationLabel1
        '
        Me.SimulationLabel1.BackColor = System.Drawing.Color.SteelBlue
        Me.SimulationLabel1.Dock = System.Windows.Forms.DockStyle.Top
        Me.SimulationLabel1.ForeColor = System.Drawing.Color.White
        Me.SimulationLabel1.Location = New System.Drawing.Point(0, 0)
        Me.SimulationLabel1.Name = "SimulationLabel1"
        Me.SimulationLabel1.Size = New System.Drawing.Size(208, 20)
        Me.SimulationLabel1.TabIndex = 15
        Me.SimulationLabel1.Text = "Output File Export"
        Me.SimulationLabel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Splitter1
        '
        Me.Splitter1.BackColor = System.Drawing.Color.LightGray
        Me.Splitter1.Enabled = False
        Me.Splitter1.Location = New System.Drawing.Point(64, 0)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(1, 71)
        Me.Splitter1.TabIndex = 23
        Me.Splitter1.TabStop = False
        '
        'SimulationPanel2
        '
        Me.SimulationPanel2.BackColor = System.Drawing.Color.Transparent
        Me.SimulationPanel2.Controls.Add(Me.SimulationToolBar2)
        Me.SimulationPanel2.Controls.Add(Me.SimulationLabel2)
        Me.SimulationPanel2.Dock = System.Windows.Forms.DockStyle.Left
        Me.SimulationPanel2.Location = New System.Drawing.Point(0, 0)
        Me.SimulationPanel2.Name = "SimulationPanel2"
        Me.SimulationPanel2.Size = New System.Drawing.Size(64, 71)
        Me.SimulationPanel2.TabIndex = 22
        '
        'SimulationToolBar2
        '
        Me.SimulationToolBar2.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.SimulationToolBar2.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.RunButton})
        Me.SimulationToolBar2.Divider = False
        Me.SimulationToolBar2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationToolBar2.DropDownArrows = True
        Me.SimulationToolBar2.ImageList = Me.ButtonImageList
        Me.SimulationToolBar2.Location = New System.Drawing.Point(0, 20)
        Me.SimulationToolBar2.Name = "SimulationToolBar2"
        Me.SimulationToolBar2.ShowToolTips = True
        Me.SimulationToolBar2.Size = New System.Drawing.Size(64, 48)
        Me.SimulationToolBar2.TabIndex = 16
        '
        'RunButton
        '
        Me.RunButton.ImageIndex = 14
        Me.RunButton.Text = "&Run"
        Me.RunButton.ToolTipText = "Run APSIM"
        '
        'SimulationLabel2
        '
        Me.SimulationLabel2.BackColor = System.Drawing.Color.SteelBlue
        Me.SimulationLabel2.Dock = System.Windows.Forms.DockStyle.Top
        Me.SimulationLabel2.ForeColor = System.Drawing.Color.White
        Me.SimulationLabel2.Location = New System.Drawing.Point(0, 0)
        Me.SimulationLabel2.Name = "SimulationLabel2"
        Me.SimulationLabel2.Size = New System.Drawing.Size(64, 20)
        Me.SimulationLabel2.TabIndex = 15
        Me.SimulationLabel2.Text = "Simulation"
        Me.SimulationLabel2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'HelpPanel
        '
        Me.HelpPanel.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.HelpPanel.BackColor = System.Drawing.Color.Transparent
        Me.HelpPanel.Controls.Add(Me.HelpToolBar)
        Me.HelpPanel.Controls.Add(Me.HelpLabel)
        Me.HelpPanel.Location = New System.Drawing.Point(272, 0)
        Me.HelpPanel.Name = "HelpPanel"
        Me.HelpPanel.Size = New System.Drawing.Size(358, 71)
        Me.HelpPanel.TabIndex = 26
        '
        'HelpToolBar
        '
        Me.HelpToolBar.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.HelpToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.HelpToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.ApsimHelpButton})
        Me.HelpToolBar.Divider = False
        Me.HelpToolBar.Dock = System.Windows.Forms.DockStyle.None
        Me.HelpToolBar.DropDownArrows = True
        Me.HelpToolBar.ImageList = Me.ButtonImageList
        Me.HelpToolBar.Location = New System.Drawing.Point(312, 20)
        Me.HelpToolBar.Name = "HelpToolBar"
        Me.HelpToolBar.ShowToolTips = True
        Me.HelpToolBar.Size = New System.Drawing.Size(72, 48)
        Me.HelpToolBar.TabIndex = 16
        '
        'ApsimHelpButton
        '
        Me.ApsimHelpButton.ImageIndex = 6
        Me.ApsimHelpButton.Text = "&Help"
        Me.ApsimHelpButton.ToolTipText = "Display the main help page"
        '
        'HelpLabel
        '
        Me.HelpLabel.BackColor = System.Drawing.Color.SteelBlue
        Me.HelpLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.HelpLabel.ForeColor = System.Drawing.Color.White
        Me.HelpLabel.Location = New System.Drawing.Point(0, 0)
        Me.HelpLabel.Name = "HelpLabel"
        Me.HelpLabel.Size = New System.Drawing.Size(358, 20)
        Me.HelpLabel.TabIndex = 15
        Me.HelpLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'ToolboxesToolbarPanel
        '
        Me.ToolboxesToolbarPanel.BackColor = System.Drawing.Color.LightSteelBlue
        Me.ToolboxesToolbarPanel.Controls.Add(Me.Splitter3)
        Me.ToolboxesToolbarPanel.Controls.Add(Me.Panel3)
        Me.ToolboxesToolbarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ToolboxesToolbarPanel.Location = New System.Drawing.Point(0, 97)
        Me.ToolboxesToolbarPanel.Name = "ToolboxesToolbarPanel"
        Me.ToolboxesToolbarPanel.Size = New System.Drawing.Size(632, 71)
        Me.ToolboxesToolbarPanel.TabIndex = 20
        Me.ToolboxesToolbarPanel.Visible = False
        '
        'Splitter3
        '
        Me.Splitter3.BackColor = System.Drawing.Color.LightGray
        Me.Splitter3.Enabled = False
        Me.Splitter3.Location = New System.Drawing.Point(0, 0)
        Me.Splitter3.Name = "Splitter3"
        Me.Splitter3.Size = New System.Drawing.Size(1, 71)
        Me.Splitter3.TabIndex = 21
        Me.Splitter3.TabStop = False
        '
        'Panel3
        '
        Me.Panel3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Panel3.BackColor = System.Drawing.Color.Transparent
        Me.Panel3.Controls.Add(Me.ToolboxesToolBarHelp)
        Me.Panel3.Controls.Add(Me.ToolBoxToolBar)
        Me.Panel3.Controls.Add(Me.ToolboxLabel)
        Me.Panel3.Location = New System.Drawing.Point(0, 0)
        Me.Panel3.Name = "Panel3"
        Me.Panel3.Size = New System.Drawing.Size(632, 71)
        Me.Panel3.TabIndex = 22
        '
        'ToolboxesToolBarHelp
        '
        Me.ToolboxesToolBarHelp.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ToolboxesToolBarHelp.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolboxesToolBarHelp.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.ToolboxesToolbarHelpButton})
        Me.ToolboxesToolBarHelp.Divider = False
        Me.ToolboxesToolBarHelp.Dock = System.Windows.Forms.DockStyle.None
        Me.ToolboxesToolBarHelp.DropDownArrows = True
        Me.ToolboxesToolBarHelp.ImageList = Me.ButtonImageList
        Me.ToolboxesToolBarHelp.Location = New System.Drawing.Point(584, 20)
        Me.ToolboxesToolBarHelp.Name = "ToolboxesToolBarHelp"
        Me.ToolboxesToolBarHelp.ShowToolTips = True
        Me.ToolboxesToolBarHelp.Size = New System.Drawing.Size(48, 48)
        Me.ToolboxesToolBarHelp.TabIndex = 17
        '
        'ToolboxesToolbarHelpButton
        '
        Me.ToolboxesToolbarHelpButton.ImageIndex = 6
        Me.ToolboxesToolbarHelpButton.Text = "&Help"
        Me.ToolboxesToolbarHelpButton.ToolTipText = "Display the main help page"
        '
        'ToolBoxToolBar
        '
        Me.ToolBoxToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBoxToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.ManageToolBoxesButton})
        Me.ToolBoxToolBar.Divider = False
        Me.ToolBoxToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ToolBoxToolBar.DropDownArrows = True
        Me.ToolBoxToolBar.ImageList = Me.ButtonImageList
        Me.ToolBoxToolBar.Location = New System.Drawing.Point(0, 20)
        Me.ToolBoxToolBar.Name = "ToolBoxToolBar"
        Me.ToolBoxToolBar.ShowToolTips = True
        Me.ToolBoxToolBar.Size = New System.Drawing.Size(632, 48)
        Me.ToolBoxToolBar.TabIndex = 16
        '
        'ManageToolBoxesButton
        '
        Me.ManageToolBoxesButton.ImageIndex = 20
        Me.ManageToolBoxesButton.Text = "&Manage toolboxes"
        Me.ManageToolBoxesButton.ToolTipText = "Add or remove toolboxes from APSIM"
        '
        'ToolboxLabel
        '
        Me.ToolboxLabel.BackColor = System.Drawing.Color.SteelBlue
        Me.ToolboxLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ToolboxLabel.ForeColor = System.Drawing.Color.White
        Me.ToolboxLabel.Location = New System.Drawing.Point(0, 0)
        Me.ToolboxLabel.Name = "ToolboxLabel"
        Me.ToolboxLabel.Size = New System.Drawing.Size(632, 20)
        Me.ToolboxLabel.TabIndex = 15
        Me.ToolboxLabel.Text = "Toolbox"
        Me.ToolboxLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'MainToolBar
        '
        Me.MainToolBar.BackColor = System.Drawing.Color.Gainsboro
        Me.MainToolBar.Cursor = System.Windows.Forms.Cursors.Default
        Me.MainToolBar.Items.AddRange(New Object() {Me.FileMenu, Me.separatorTool2, Me.SaveSmallButton, Me.CutSmallButton, Me.CopySmallButton, Me.PasteSmallButton, Me.separatorTool1, Me.SimulationButton, Me.ToolboxButton})
        Me.MainToolBar.ItemsImageList = Me.SmallImages
        Me.MainToolBar.Location = New System.Drawing.Point(0, 0)
        Me.MainToolBar.Name = "MainToolBar"
        Me.MainToolBar.Size = New System.Drawing.Size(632, 26)
        Me.MainToolBar.TabIndex = 23
        Me.MainToolBar.Text = "MainToolBar"
        '
        'FileMenu
        '
        Me.FileMenu.Items.AddRange(New Object() {Me.NewFileMenu, Me.OpenFileMenu, Me.SaveFileMenu, Me.SaveAsMenu, Me.RecentSimulationsMenu, Me.separatorMenuItem1, Me.ExitMenu})
        Me.FileMenu.Text = "&File"
        '
        'NewFileMenu
        '
        Me.NewFileMenu.ImageIndex = 5
        Me.NewFileMenu.Text = "&New file"
        '
        'OpenFileMenu
        '
        Me.OpenFileMenu.ImageIndex = 6
        Me.OpenFileMenu.Text = "&Open file ..."
        '
        'SaveFileMenu
        '
        Me.SaveFileMenu.ImageIndex = 7
        Me.SaveFileMenu.Text = "&Save file"
        '
        'SaveAsMenu
        '
        Me.SaveAsMenu.ImageIndex = 8
        Me.SaveAsMenu.Text = "Save &As file ..."
        '
        'RecentSimulationsMenu
        '
        Me.RecentSimulationsMenu.ImageIndex = 14
        Me.RecentSimulationsMenu.Text = "Recent Simulations"
        '
        'ExitMenu
        '
        Me.ExitMenu.ImageIndex = 15
        Me.ExitMenu.Text = "E&xit"
        '
        'SaveSmallButton
        '
        Me.SaveSmallButton.ImageIndex = 7
        Me.SaveSmallButton.ToolTipText = "Save current file"
        '
        'CutSmallButton
        '
        Me.CutSmallButton.ImageIndex = 9
        Me.CutSmallButton.ToolTipText = "Cut the selected items to the clipboard"
        '
        'CopySmallButton
        '
        Me.CopySmallButton.ImageIndex = 10
        Me.CopySmallButton.ToolTipText = "Copy the selected items to the clipboard"
        '
        'PasteSmallButton
        '
        Me.PasteSmallButton.ImageIndex = 11
        Me.PasteSmallButton.ToolTipText = "Paste clipboard contents into file"
        '
        'SimulationButton
        '
        Me.SimulationButton.Tag = "down"
        Me.SimulationButton.Text = "&Simulation"
        Me.SimulationButton.TextWidth = 70
        '
        'ToolboxButton
        '
        Me.ToolboxButton.Tag = "up"
        Me.ToolboxButton.Text = "&Toolboxes"
        Me.ToolboxButton.TextWidth = 70
        '
        'SmallImages
        '
        Me.SmallImages.ImageSize = New System.Drawing.Size(16, 16)
        Me.SmallImages.ImageStream = CType(resources.GetObject("SmallImages.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.SmallImages.TransparentColor = System.Drawing.Color.Transparent
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(632, 546)
        Me.Controls.Add(Me.ToolBoxSplitter)
        Me.Controls.Add(Me.ToolBoxPanel)
        Me.Controls.Add(Me.ToolboxesToolbarPanel)
        Me.Controls.Add(Me.SimulationToolBarPanel)
        Me.Controls.Add(Me.MainToolBar)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.KeyPreview = True
        Me.Name = "MainUI"
        Me.Text = "APSIM"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.ToolBoxPanel.ResumeLayout(False)
        Me.ToolBoxToolBarPanel.ResumeLayout(False)
        Me.SimulationToolBarPanel.ResumeLayout(False)
        Me.SimulationPanel1.ResumeLayout(False)
        Me.SimulationPanel2.ResumeLayout(False)
        Me.HelpPanel.ResumeLayout(False)
        Me.ToolboxesToolbarPanel.ResumeLayout(False)
        Me.Panel3.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region
#Region "Startup methods"
    Private Sub MainUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ApsimUI = New ApsimUIController(".apsim", _
                                         "APSIM files (*.apsim)|*.apsim|" + _
                                            "Toolbox files (*.xml)|*.xml|" + _
                                            "Soils files (*.soils)|*.soils|" + _
                                            "All files (*.*)|*.*", _
                                        "apsimui")
        AddHandler ApsimUI.NewDataEvent, AddressOf OnNewDataEvent
        AddHandler ApsimUI.SelectionChangedEvent, AddressOf SetFunctionality
        AddHandler ApsimUI.DataChangedEvent, AddressOf SetFunctionality
        AddHandler ApsimUI.AddEvent, AddressOf OnAddEvent
        AddHandler ApsimUI.RenameEvent, AddressOf OnAddEvent
        AddHandler ToolboxesToolBarHelp.ButtonClick, AddressOf ToolBar_ButtonClick


        ' Show the Simulation Explorer.
        SimulationExplorer = New ExplorerUI(Me, ApsimUI)
        SimulationExplorer.Dock = DockStyle.Fill
        SimulationExplorer.Parent = Me
        SimulationExplorer.Visible = True
        SimulationExplorer.BringToFront()

        SimulationExplorer.ShowUI(New APSIMData("startup", ""))

        ' Setup but don't show the Toolbox Explorer.
        Toolbox = New ApsimUIController(".xml", _
                                         "Toolbox files (*.xml)|*.xml|" + _
                                            "Soils files (*.soils)|*.soils|" + _
                                            "All files (*.*)|*.*", _
                                        "")
        ToolboxExplorer = New ExplorerUI(Nothing, Toolbox)
        ToolboxExplorer.Dock = DockStyle.Fill
        ToolboxExplorer.Parent = ToolBoxPanel
        ToolboxExplorer.Visible = True
        ToolboxExplorer.BringToFront()
        ToolBoxSplitter.BringToFront()

        ' Load a default file if one was specified on the command line.
        Dim separators As String = " "
        Dim commands As String = Microsoft.VisualBasic.Command()
        Dim args() As String = commands.Split(separators.ToCharArray)
        Dim FileName As String = ""

        For Each Part As String In args
            If FileName <> "" Then
                FileName = FileName + " "
            End If
            FileName = FileName + Part
        Next
        If FileName.Length() > 0 Then
            FileName = FileName.Replace("""", "")
            ApsimUI.FileOpen(FileName)
        End If

        CreateRecentFileListMenu()

        ConstructToolBoxToolBar()
        SetFunctionality()
    End Sub

#End Region

#Region "Application level methods"

    Private Sub OnNewDataEvent()
        ' New data has entered the system.
        ' This is usually caused by FileNew,
        ' FileOpen etc.        APSIMChangeTool.Upgrade(ApsimUI.Data)
        OnAddEvent()
        SetFunctionality()

    End Sub

    Private Sub OnAddEvent()
        ' Called when the tree view control is alterted
        ApsimUI.CheckAllComponents(ApsimUI.AllData)

    End Sub


    Private Sub CreateRecentFileListMenu()
        'Creates a list of recent files under the 'File' menu

        Dim FileNames() As String = ApsimUI.GetFrequentList()

        For Each str As String In FileNames
            Dim newItem As New Xceed.SmartUI.Controls.MenuBar.MenuItem(str)
            newItem.ImageIndex = Me.RecentSimulationsMenu.ImageIndex

            AddHandler newItem.Click, AddressOf OpenSimulation

            Me.RecentSimulationsMenu.Items.Add(newItem)

        Next

    End Sub

    Private Sub OpenSimulation(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs)
        'Open the simulation provided by the recent simulations menu item

        ApsimUI.FileOpen(e.Item.Text)

    End Sub



    Private Sub SetFunctionality()
        ' Enable / Disable bits of functionality as 
        ' required. i.e. ensure program is in a 
        ' consistant state.
        Dim SomethingInTree As Boolean = Not IsNothing(ApsimUI.AllData) AndAlso ApsimUI.AllData.ChildList.Count > 0

        CutSmallButton.Enabled = ApsimUI.AllowCut
        CopySmallButton.Enabled = ApsimUI.AllowCopy
        PasteSmallButton.Enabled = ApsimUI.AllowPaste

        RunButton.Enabled = SomethingInTree

        GraphButton.Enabled = SomethingInTree
        ApsimOutlookButton.Enabled = SomethingInTree

        Me.ExcelToolBarButton.Enabled = SomethingInTree

    End Sub

    Private Sub MainUI_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        ' User is closing down - save our work.
        e.Cancel = Not ApsimUI.FileSaveAfterPrompt()
        If Not e.Cancel Then
            Try
                Dim inifile As New APSIMSettings
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "windowstate", Str(Me.WindowState))
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "top", Str(Me.Top))
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "left", Str(Me.Left))
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "width", Str(Me.Width))
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "height", Str(Me.Height))
            Catch ex As System.Exception
            End Try

            If ToolboxExplorer.Visible And Toolbox.AllowFileSave Then
                Toolbox.FileSave()
            End If
        End If
    End Sub

    Private Sub ToolBar_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles SimulationToolBar1.ButtonClick, SimulationToolBar2.ButtonClick, HelpToolBar.ButtonClick, ToolBoxToolBar.ButtonClick
        'Catch which button on the toolboar the user clicked.
        'Please Note: To catch user defined toolbox button events its code is in the 'else'
        'section of the parent 'if' statement.  an 'exit sub' statement must be added to all
        'preceeding if statements.

        'Run button
        If e.Button Is RunButton Then
            Try
                RunSimulations()
            Catch fnf As System.IO.FileNotFoundException
                MessageBox.Show(fnf.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

            End Try

            'APSIM Help Button
        ElseIf e.Button Is ApsimHelpButton Or e.Button Is ToolboxesToolbarHelpButton Then
            Dim HelpURL As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "docfile")
            ApsimUI.ShowHelp(HelpURL)

        ElseIf e.Button Is ManageToolBoxesButton Then
            Dim Form As New OptionsForm
            Form.ShowDialog(Me)
            ConstructToolBoxToolBar()

            'The Output File Export buttons (Excel, Graph and APSIM Outlook)
        ElseIf e.Button Is ExcelToolBarButton Or _
                            e.Button Is GraphButton Or _
                            e.Button Is ApsimOutlookButton Then

            Dim arrFiles As StringCollection = GetAllOutputFiles(ApsimUI.AllData)


            'Only show export screen if more than one output file
            If arrFiles.Count > 1 Then
                Dim frmOutput As New OutputFileExport(arrFiles)

                ' Quit routine if user chose "Cancel" or did not select any output files from the dialog
                ' box list
                If frmOutput.ShowDialog(Me) = DialogResult.Cancel Or frmOutput.SelectedOutputFiles.Count <= 0 Then
                    Exit Sub

                End If

                arrFiles = frmOutput.SelectedOutputFiles

            End If

            Dim OutputFileList As String = ComposeOutputFileCommandLineArgs(arrFiles)

            Select Case e.Button.Text

                Case ExcelToolBarButton.Text
                    Me.excelFiles(OutputFileList)

                Case GraphButton.Text
                    Me.apsvisFiles(OutputFileList)

                Case Me.ApsimOutlookButton.Text
                    Me.apsimoutlookFiles(OutputFileList)

            End Select

            ' User has selected a user defined toolbox button on the toolboxes toolbar (read it again slowly :-)
        ElseIf e.Button.Tag = "User Defined" Then
            ShowToolBoxWindow(e.Button.Text)

        End If

    End Sub


    Private Function ComposeOutputFileCommandLineArgs(ByVal OutputFiles As System.Collections.Specialized.StringCollection) As String
        'formats a collection of output file paths into a single comma delimited string

        Dim returnString As String

        For Each str As String In OutputFiles
            returnString += "," & str

        Next

        Return returnString.Remove(0, 1)

    End Function
#End Region

#Region "Toolbox methods"

    Private Sub ConstructToolBoxToolBar()
        'Create a toolbar button for each toolbox.

        'Remove user defined buttons first
        For Each button As Windows.Forms.ToolBarButton In GetUserDefinedButtons(Me.ToolBoxToolBar.Buttons)
            Me.ToolBoxToolBar.Buttons.Remove(button)

        Next

        'then reset the list of user defined toolboxes.
        Try
            Dim toolboxes As New Toolboxes

            ' Loop through each of the known toolboxes
            For Each FileName As String In toolboxes.Names
                Dim apsimData As New VBGeneral.APSIMData
                Dim toolBoxPath As String = toolboxes.NameToFileName(FileName)

                apsimData.LoadFromFile(toolBoxPath)

                ' Get the image attribute from the root node of the loaded xml file
                Dim image As String = apsimData.Attribute("image")

                If image.IndexOf(":") = -1 Then
                    image = APSIMSettings.ApsimDirectory() + "\ApsimUI\" + image
                End If
                Dim item As New Windows.Forms.ToolBarButton(FileName)
                item.Tag = "User Defined"

                ' If a predefined image has been given then attempt to add it.  
                If System.IO.File.Exists(image) Then
                    Me.ToolBoxToolBar.ImageList.Images.Add( _
                        New System.Drawing.Bitmap(image))

                    item.ImageIndex = Me.ToolBoxToolBar.ImageList.Images.Count() - 1

                    'Otherwise use the default image in the images directory under the APSIMUI root.
                Else
                    Dim alternateImage As String = APSIMSettings.ApsimDirectory() + "\ApsimUI\Images\toolbox.png"

                    If System.IO.File.Exists(alternateImage) Then
                        Me.ToolBoxToolBar.ImageList.Images.Add(New System.Drawing.Bitmap(alternateImage))
                        item.ImageIndex = Me.ToolBoxToolBar.ImageList.Images.Count() - 1
                    End If

                End If

                Me.ToolBoxToolBar.Buttons.Add(item)

            Next

        Catch e As System.Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building tool box menus")
        End Try

    End Sub

    Private Function GetUserDefinedButtons(ByVal buttonArray As Windows.Forms.ToolBar.ToolBarButtonCollection) As _
                            System.Collections.ArrayList
        'Iterates through given button array finding those with the tag of "User Defined"

        Dim userDefinedButtons As New System.Collections.ArrayList

        For Each button As Windows.Forms.ToolBarButton In buttonArray
            If button.Tag = "User Defined" Then userDefinedButtons.Add(button)

        Next

        Return userDefinedButtons

    End Function
    Private Sub ShowToolBoxWindow(ByVal ToolBoxName As String)
        'Display the given ToolBoxName in the toolbox panel at

        Dim inifile As New APSIMSettings
        ToolBoxPanel.Height = Val(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "toolboxheight"))
        ToolBoxPanel.Height = ToolBoxPanel.Height - 1
        ToolBoxPanel.Height = ToolBoxPanel.Height + 1

        ToolBoxSplitter.Visible = True
        ToolBoxPanel.Visible = True
        Me.ToolBoxSplitterPoint = ToolBoxSplitter.SplitPosition

        Dim toolboxes As New Toolboxes
        Dim filename As String = toolboxes.NameToFileName(ToolBoxName)
        ToolboxExplorer.ExpandAll = False
        Toolbox.FileOpen(filename)
    End Sub

    Private Sub HideToolBoxWindow()

        If Toolbox.AllowFileSave Then
            Toolbox.FileSave()
        End If

        ToolBoxPanel.Visible = False
        ToolBoxSplitter.Visible = ToolBoxPanel.Visible
    End Sub

    Private Sub OnToolBoxClick(ByVal sender As System.Object, ByVal e As System.EventArgs)
        ShowToolBoxWindow(sender.text)
    End Sub

    Private Sub ToolBoxSplitter_LocationChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolBoxSplitter.LocationChanged
        If ToolBoxPanel.Visible Then
            Dim inifile As New APSIMSettings
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "toolboxheight", Str(ToolBoxPanel.Height))
        End If
    End Sub

#End Region
#Region "Run simulation"
    Private Sub RunSimulations()
        ' Do a save.
        If ApsimUI.FileSave() Then
            ' kill old apsim processes
            Dim AllProcesses As Process() = Process.GetProcesses()
            For Each proc As Process In AllProcesses
                If Path.GetFileName(proc.ProcessName) = "apsim" Then
                    proc.Kill()
                End If
            Next

            Dim ApsRunFileName As String = Path.GetDirectoryName(Application.ExecutablePath) + "\apsrun.exe"

            ' Check if The APSIM run application is where it should be, if not throw a FileNotFoundException
            If System.IO.File.Exists(ApsRunFileName) Then
                Process.Start(ApsRunFileName, """" + ApsimUI.FileName + """")
            Else
                Throw New System.IO.FileNotFoundException("The file '" & ApsRunFileName & "' could not be found.")
            End If


        End If
    End Sub
#End Region
#Region "Graphing methods"

    'return a StringCollection of all the output files currently in APSIM Project
    Private Function GetAllOutputFiles(ByVal Data As APSIMData) As StringCollection

        Dim OutputFileCollection As New StringCollection

        'loop through each node in the tree
        For Each Child As APSIMData In Data.Children

            ' If child node is an "area", "simulation" or "simulations" then node is not a leaf
            ' and a recursive call is made
            If Child.Type.ToLower() = "area" Or Child.Type.ToLower() = "simulation" Or Child.Type.ToLower() = "simulations" Then

                'On a recursive call, repopulate the string collection object with those
                'output file paths already found.
                For Each str As String In GetAllOutputFiles(Child)   ' recursion
                    OutputFileCollection.Add(str)

                Next


                ' If leaf is an "Outputfile" type then check its path, making it absolute if neccesary
                ' adding it to the string collection object
            ElseIf Child.Type.ToLower() = "outputfile" Then
                Dim FullFileName As String = Child.ChildValue("filename")

                If ApsimUI.FileName <> "" Then
                    FullFileName = Path.Combine(Path.GetDirectoryName(ApsimUI.FileName), FullFileName)

                End If

                OutputFileCollection.Add(FullFileName)

            End If

        Next

        Return OutputFileCollection

    End Function

#End Region
#Region "Top level menu bar"
    Private Sub MainButtonClick(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles SimulationButton.Click, ToolboxButton.Click
        ' User has clicked a top level menu button

        For Each Item As SmartItem In MainToolBar.Items
            If Not Item Is e.Item AndAlso Not Item.Tag Is Nothing AndAlso Item.Tag.ToString() <> "up" Then
                Item.Tag = "up"
                Item.Refresh()
            End If
        Next

        e.Item.Tag = "down"

        SimulationToolBarPanel.Visible = SimulationButton.Tag.ToString() = "down"
        ToolboxesToolbarPanel.Visible = ToolboxButton.Tag.ToString() = "down"

    End Sub

    Private Sub MainButtonPaint(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemPaintEventArgs) Handles SimulationButton.Paint, ToolboxButton.Paint

        If e.Item.Tag.ToString() = "down" Then
            e.SmartPaint.DrawThemePart(Xceed.SmartUI.UIStyle.ThemePart.ToolDown, e.Bounds)
        Else
            e.SmartPaint.FillRectangle(e.Item.ParentSmartControl.BackColor, e.Bounds)
        End If
        e.SmartPaint.DrawString(e.Item.Text, ContentAlignment.MiddleCenter)
    End Sub

    Private Sub NewFileMenu_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles NewFileMenu.Click
        Dim NewData As APSIMData = ApsimUI.LetUserSelectNewDocument()
        If Not IsNothing(NewData) Then
            ApsimUI.FileNew(NewData)
        End If
    End Sub

    Private Sub OpenFileMenu_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles OpenFileMenu.Click
        ApsimUI.FileOpen()
    End Sub

    Private Sub SaveFileMenu_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles SaveFileMenu.Click
        ApsimUI.FileSave()
    End Sub

    Private Sub SaveAsMenu_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles SaveAsMenu.Click
        ApsimUI.FileSaveAs()
    End Sub

    Private Sub ExitMenu_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles ExitMenu.Click
        Close()
    End Sub
    Private Sub SaveSmallButton_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles SaveSmallButton.Click
        ApsimUI.FileSave()
    End Sub
    Private Sub CutSmallButton_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles CutSmallButton.Click
        ApsimUI.Cut()
    End Sub

    Private Sub CopySmallButton_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles CopySmallButton.Click
        ApsimUI.Copy()
    End Sub

    Private Sub PasteSmallButton_Click(ByVal sender As Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs) Handles PasteSmallButton.Click
        ApsimUI.Paste()
    End Sub
#End Region

    Protected Overrides Function ProcessDialogKey(ByVal keyData As System.Windows.Forms.Keys) As Boolean
        Dim Button As ToolBarButton = ApsimUI.ProcessDialogKey(Me, keyData)
        If Not Button Is Nothing Then
            ToolBar_ButtonClick(Nothing, New ToolBarButtonClickEventArgs(Button))
            Return True
        End If
    End Function

    Private Sub FileExportExcelMenu_Click(ByVal sender As System.Object, ByVal e As Xceed.SmartUI.SmartItemClickEventArgs)
        Dim clickEvent As New System.Windows.Forms.ToolBarButtonClickEventArgs(Me.ExcelToolBarButton)
        Me.ToolBar_ButtonClick(Me.ExcelToolBarButton, clickEvent)

    End Sub

    Private Sub btnClose_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles btnClose.Paint
        ControlPaint.DrawBorder(e.Graphics, e.ClipRectangle, Me.ToolboxesToolbarPanel.BackColor, ButtonBorderStyle.Solid)
    End Sub

    Private Sub btnClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClose.Click
        HideToolBoxWindow()
    End Sub

    Private Sub btnMaximise_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles btnMaximise.Paint
        ControlPaint.DrawBorder(e.Graphics, e.ClipRectangle, Me.ToolboxesToolbarPanel.BackColor, ButtonBorderStyle.Solid)
    End Sub

    Private Sub btnMaximise_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnMaximise.Click

        Me.ToolBoxSplitterPoint = Me.ToolBoxSplitter.SplitPosition
        Me.ToolBoxSplitter.SplitPosition = _
            Me.ToolBoxToolBarPanel.Top + Me.ToolBoxToolBarPanel.Width + Me.ToolBoxSplitter.MinSize
        Me.btnMaximise.Visible = False
        Me.btnRestore.Visible = True

    End Sub

    Private Sub btnRestore_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles btnRestore.Paint
        ControlPaint.DrawBorder(e.Graphics, e.ClipRectangle, Me.ToolboxesToolbarPanel.BackColor, ButtonBorderStyle.Solid)
    End Sub

    Private Sub btnRestore_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRestore.Click

        If Me.ToolBoxSplitter.SplitPosition = Me.ToolBoxSplitterPoint Then
            Me.ToolBoxSplitterPoint = Me.ToolBoxSplitter.SplitPosition - 10

        End If

        Me.ToolBoxSplitter.SplitPosition = Me.ToolBoxSplitterPoint
        Me.ToolBoxSplitterPoint = -1
        Me.btnMaximise.Visible = True
        Me.btnRestore.Visible = False
    End Sub

End Class
