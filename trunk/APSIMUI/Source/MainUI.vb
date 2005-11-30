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
    Private ApsimUI As ApsimUIController
    Private Toolbox As ApsimUIController
    Private SimulationExplorer As ExplorerUI
    Private ToolboxExplorer As ExplorerUI

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
        Dim splash As New SplashScreen
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

        splash.Hide()
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
    Friend WithEvents OpenToolboxButton As System.Windows.Forms.ToolBarButton
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
    Friend WithEvents ToolBarCloseButton As System.Windows.Forms.ToolBarButton
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
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MainUI))
        Me.toolBoxContextMenu = New System.Windows.Forms.ContextMenu
        Me.ButtonImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.HelpImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.ToolBoxPanel = New System.Windows.Forms.Panel
        Me.ToolBoxToolBarPanel = New System.Windows.Forms.Panel
        Me.ToolBoxPanelToolBar = New System.Windows.Forms.ToolBar
        Me.ToolBarCloseButton = New System.Windows.Forms.ToolBarButton
        Me.ToolBoxSplitter = New System.Windows.Forms.Splitter
        Me.SimulationToolBarPanel = New System.Windows.Forms.Panel
        Me.Splitter7 = New System.Windows.Forms.Splitter
        Me.HelpPanel = New System.Windows.Forms.Panel
        Me.HelpToolBar = New System.Windows.Forms.ToolBar
        Me.ApsimHelpButton = New System.Windows.Forms.ToolBarButton
        Me.HelpLabel = New System.Windows.Forms.Label
        Me.Splitter2 = New System.Windows.Forms.Splitter
        Me.SimulationPanel1 = New System.Windows.Forms.Panel
        Me.SimulationToolBar1 = New System.Windows.Forms.ToolBar
        Me.GraphButton = New System.Windows.Forms.ToolBarButton
        Me.ApsimOutlookButton = New System.Windows.Forms.ToolBarButton
        Me.SimulationLabel1 = New System.Windows.Forms.Label
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.SimulationPanel2 = New System.Windows.Forms.Panel
        Me.SimulationToolBar2 = New System.Windows.Forms.ToolBar
        Me.RunButton = New System.Windows.Forms.ToolBarButton
        Me.SimulationLabel2 = New System.Windows.Forms.Label
        Me.ToolboxesToolbarPanel = New System.Windows.Forms.Panel
        Me.Splitter3 = New System.Windows.Forms.Splitter
        Me.Panel3 = New System.Windows.Forms.Panel
        Me.ToolBoxToolBar = New System.Windows.Forms.ToolBar
        Me.OpenToolboxButton = New System.Windows.Forms.ToolBarButton
        Me.ManageToolBoxesButton = New System.Windows.Forms.ToolBarButton
        Me.ToolboxLabel = New System.Windows.Forms.Label
        Me.MainToolBar = New Xceed.SmartUI.Controls.ToolBar.SmartToolBar(Me.components)
        Me.FileMenu = New Xceed.SmartUI.Controls.ToolBar.MenuTool("&File")
        Me.NewFileMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("&New file", 5)
        Me.OpenFileMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("&Open file", 6)
        Me.SaveFileMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("&Save file", 7)
        Me.SaveAsMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("Save &As file", 8)
        Me.separatorMenuItem1 = New Xceed.SmartUI.Controls.MenuBar.SeparatorMenuItem
        Me.ExitMenu = New Xceed.SmartUI.Controls.MenuBar.MenuItem("E&xit")
        Me.separatorTool2 = New Xceed.SmartUI.Controls.ToolBar.SeparatorTool
        Me.SaveSmallButton = New Xceed.SmartUI.Controls.ToolBar.Tool(7)
        Me.CutSmallButton = New Xceed.SmartUI.Controls.ToolBar.Tool(9)
        Me.CopySmallButton = New Xceed.SmartUI.Controls.ToolBar.Tool(10)
        Me.PasteSmallButton = New Xceed.SmartUI.Controls.ToolBar.Tool(11)
        Me.separatorTool1 = New Xceed.SmartUI.Controls.ToolBar.SeparatorTool
        Me.SimulationButton = New Xceed.SmartUI.Controls.ToolBar.Tool("&Simulation")
        Me.ToolboxButton = New Xceed.SmartUI.Controls.ToolBar.Tool("&Toolboxes")
        Me.SmallImages = New System.Windows.Forms.ImageList(Me.components)
        Me.ToolBoxPanel.SuspendLayout()
        Me.ToolBoxToolBarPanel.SuspendLayout()
        Me.SimulationToolBarPanel.SuspendLayout()
        Me.HelpPanel.SuspendLayout()
        Me.SimulationPanel1.SuspendLayout()
        Me.SimulationPanel2.SuspendLayout()
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
        Me.ToolBoxToolBarPanel.Controls.Add(Me.ToolBoxPanelToolBar)
        Me.ToolBoxToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ToolBoxToolBarPanel.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxToolBarPanel.Name = "ToolBoxToolBarPanel"
        Me.ToolBoxToolBarPanel.Size = New System.Drawing.Size(632, 32)
        Me.ToolBoxToolBarPanel.TabIndex = 19
        '
        'ToolBoxPanelToolBar
        '
        Me.ToolBoxPanelToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBoxPanelToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.ToolBarCloseButton})
        Me.ToolBoxPanelToolBar.Divider = False
        Me.ToolBoxPanelToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ToolBoxPanelToolBar.DropDownArrows = True
        Me.ToolBoxPanelToolBar.ImageList = Me.ButtonImageList
        Me.ToolBoxPanelToolBar.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxPanelToolBar.Name = "ToolBoxPanelToolBar"
        Me.ToolBoxPanelToolBar.ShowToolTips = True
        Me.ToolBoxPanelToolBar.Size = New System.Drawing.Size(632, 34)
        Me.ToolBoxPanelToolBar.TabIndex = 17
        Me.ToolBoxPanelToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        '
        'ToolBarCloseButton
        '
        Me.ToolBarCloseButton.ImageIndex = 22
        Me.ToolBarCloseButton.Text = "Close"
        Me.ToolBarCloseButton.ToolTipText = "Close this toolbar window"
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
        Me.SimulationToolBarPanel.Controls.Add(Me.HelpPanel)
        Me.SimulationToolBarPanel.Controls.Add(Me.Splitter2)
        Me.SimulationToolBarPanel.Controls.Add(Me.SimulationPanel1)
        Me.SimulationToolBarPanel.Controls.Add(Me.Splitter1)
        Me.SimulationToolBarPanel.Controls.Add(Me.SimulationPanel2)
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
        Me.Splitter7.Location = New System.Drawing.Point(287, 0)
        Me.Splitter7.Name = "Splitter7"
        Me.Splitter7.Size = New System.Drawing.Size(1, 71)
        Me.Splitter7.TabIndex = 27
        Me.Splitter7.TabStop = False
        '
        'HelpPanel
        '
        Me.HelpPanel.BackColor = System.Drawing.Color.Transparent
        Me.HelpPanel.Controls.Add(Me.HelpToolBar)
        Me.HelpPanel.Controls.Add(Me.HelpLabel)
        Me.HelpPanel.Dock = System.Windows.Forms.DockStyle.Left
        Me.HelpPanel.Location = New System.Drawing.Point(233, 0)
        Me.HelpPanel.Name = "HelpPanel"
        Me.HelpPanel.Size = New System.Drawing.Size(54, 71)
        Me.HelpPanel.TabIndex = 26
        '
        'HelpToolBar
        '
        Me.HelpToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.HelpToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.ApsimHelpButton})
        Me.HelpToolBar.Divider = False
        Me.HelpToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.HelpToolBar.DropDownArrows = True
        Me.HelpToolBar.ImageList = Me.ButtonImageList
        Me.HelpToolBar.Location = New System.Drawing.Point(0, 20)
        Me.HelpToolBar.Name = "HelpToolBar"
        Me.HelpToolBar.ShowToolTips = True
        Me.HelpToolBar.Size = New System.Drawing.Size(54, 48)
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
        Me.HelpLabel.Size = New System.Drawing.Size(54, 20)
        Me.HelpLabel.TabIndex = 15
        Me.HelpLabel.Text = "Help"
        Me.HelpLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Splitter2
        '
        Me.Splitter2.BackColor = System.Drawing.Color.LightGray
        Me.Splitter2.Enabled = False
        Me.Splitter2.Location = New System.Drawing.Point(232, 0)
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
        Me.SimulationPanel1.Location = New System.Drawing.Point(80, 0)
        Me.SimulationPanel1.Name = "SimulationPanel1"
        Me.SimulationPanel1.Size = New System.Drawing.Size(152, 71)
        Me.SimulationPanel1.TabIndex = 20
        '
        'SimulationToolBar1
        '
        Me.SimulationToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.SimulationToolBar1.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.GraphButton, Me.ApsimOutlookButton})
        Me.SimulationToolBar1.Divider = False
        Me.SimulationToolBar1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationToolBar1.DropDownArrows = True
        Me.SimulationToolBar1.ImageList = Me.ButtonImageList
        Me.SimulationToolBar1.Location = New System.Drawing.Point(0, 20)
        Me.SimulationToolBar1.Name = "SimulationToolBar1"
        Me.SimulationToolBar1.ShowToolTips = True
        Me.SimulationToolBar1.Size = New System.Drawing.Size(152, 48)
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
        'SimulationLabel1
        '
        Me.SimulationLabel1.BackColor = System.Drawing.Color.SteelBlue
        Me.SimulationLabel1.Dock = System.Windows.Forms.DockStyle.Top
        Me.SimulationLabel1.ForeColor = System.Drawing.Color.White
        Me.SimulationLabel1.Location = New System.Drawing.Point(0, 0)
        Me.SimulationLabel1.Name = "SimulationLabel1"
        Me.SimulationLabel1.Size = New System.Drawing.Size(152, 20)
        Me.SimulationLabel1.TabIndex = 15
        Me.SimulationLabel1.Text = "Output file graphics"
        Me.SimulationLabel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Splitter1
        '
        Me.Splitter1.BackColor = System.Drawing.Color.LightGray
        Me.Splitter1.Enabled = False
        Me.Splitter1.Location = New System.Drawing.Point(79, 0)
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
        Me.SimulationPanel2.Size = New System.Drawing.Size(79, 71)
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
        Me.SimulationToolBar2.Size = New System.Drawing.Size(79, 48)
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
        Me.SimulationLabel2.Size = New System.Drawing.Size(79, 20)
        Me.SimulationLabel2.TabIndex = 15
        Me.SimulationLabel2.Text = "Simulation"
        Me.SimulationLabel2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
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
        Me.Splitter3.Location = New System.Drawing.Point(224, 0)
        Me.Splitter3.Name = "Splitter3"
        Me.Splitter3.Size = New System.Drawing.Size(1, 71)
        Me.Splitter3.TabIndex = 21
        Me.Splitter3.TabStop = False
        '
        'Panel3
        '
        Me.Panel3.BackColor = System.Drawing.Color.Transparent
        Me.Panel3.Controls.Add(Me.ToolBoxToolBar)
        Me.Panel3.Controls.Add(Me.ToolboxLabel)
        Me.Panel3.Dock = System.Windows.Forms.DockStyle.Left
        Me.Panel3.Location = New System.Drawing.Point(0, 0)
        Me.Panel3.Name = "Panel3"
        Me.Panel3.Size = New System.Drawing.Size(224, 71)
        Me.Panel3.TabIndex = 22
        '
        'ToolBoxToolBar
        '
        Me.ToolBoxToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBoxToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.OpenToolboxButton, Me.ManageToolBoxesButton})
        Me.ToolBoxToolBar.Divider = False
        Me.ToolBoxToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ToolBoxToolBar.DropDownArrows = True
        Me.ToolBoxToolBar.ImageList = Me.ButtonImageList
        Me.ToolBoxToolBar.Location = New System.Drawing.Point(0, 20)
        Me.ToolBoxToolBar.Name = "ToolBoxToolBar"
        Me.ToolBoxToolBar.ShowToolTips = True
        Me.ToolBoxToolBar.Size = New System.Drawing.Size(224, 48)
        Me.ToolBoxToolBar.TabIndex = 16
        '
        'OpenToolboxButton
        '
        Me.OpenToolboxButton.DropDownMenu = Me.toolBoxContextMenu
        Me.OpenToolboxButton.ImageIndex = 19
        Me.OpenToolboxButton.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton
        Me.OpenToolboxButton.Text = "&Open toolbox"
        Me.OpenToolboxButton.ToolTipText = "Open a toolbox"
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
        Me.ToolboxLabel.Size = New System.Drawing.Size(224, 20)
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
        Me.FileMenu.Items.AddRange(New Object() {Me.NewFileMenu, Me.OpenFileMenu, Me.SaveFileMenu, Me.SaveAsMenu, Me.separatorMenuItem1, Me.ExitMenu})
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
        Me.OpenFileMenu.Text = "&Open file"
        '
        'SaveFileMenu
        '
        Me.SaveFileMenu.ImageIndex = 7
        Me.SaveFileMenu.Text = "&Save file"
        '
        'SaveAsMenu
        '
        Me.SaveAsMenu.ImageIndex = 8
        Me.SaveAsMenu.Text = "Save &As file"
        '
        'ExitMenu
        '
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
        Me.HelpPanel.ResumeLayout(False)
        Me.SimulationPanel1.ResumeLayout(False)
        Me.SimulationPanel2.ResumeLayout(False)
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
        PopulateToolBoxContextMenu()
        SetFunctionality()
    End Sub
#End Region
#Region "Application level methods"
    Private Sub OnNewDataEvent()
        ' New data has entered the system.
        ' This is usually caused by FileNew,
        ' FileOpen etc.        APSIMChangeTool.Upgrade(ApsimUI.Data)
        ApsimUI.CheckAllComponents(ApsimUI.AllData)
        SetFunctionality()
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
        If e.Button Is RunButton Then
            RunSimulations()
        ElseIf e.Button Is GraphButton Then
            Graph()
        ElseIf e.Button Is ApsimOutlookButton Then
            ApsimOutlook()
        ElseIf e.Button Is ApsimHelpButton Then
            Dim HelpURL As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "docfile")
            ApsimUI.ShowHelp(HelpURL)
        ElseIf e.Button Is OpenToolboxButton Then

        ElseIf e.Button Is ManageToolBoxesButton Then
            Dim Form As New OptionsForm
            Form.ShowDialog(Me)
            PopulateToolBoxContextMenu()
        End If


    End Sub
#End Region
#Region "Toolbox methods"
    Private Sub PopulateToolBoxContextMenu()
        Try
            Dim toolboxes As New Toolboxes
            toolBoxContextMenu.MenuItems.Clear()
            For Each Filename As String In toolboxes.Names
                Dim Item As New MenuItem(Filename)
                AddHandler Item.Click, AddressOf OnToolBoxClick
                toolBoxContextMenu.MenuItems.Add(Item)
            Next
        Catch e As System.Exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building tool box menus")
        End Try
    End Sub
    Private Sub ShowToolBoxWindow(ByVal ToolBoxName As String)
        Dim inifile As New APSIMSettings
        ToolBoxPanel.Height = Val(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "toolboxheight"))
        ToolBoxPanel.Height = ToolBoxPanel.Height - 1
        ToolBoxPanel.Height = ToolBoxPanel.Height + 1

        ToolBoxSplitter.Visible = True
        ToolBoxPanel.Visible = True

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
    Private Sub ToolPanelToolBar_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles ToolBoxPanelToolBar.ButtonClick
        HideToolBoxWindow()
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
            Process.Start(ApsRunFileName, """" + ApsimUI.FileName + """")
        End If
    End Sub
#End Region
#Region "Graphing methods"
    Private Sub Graph()
        Dim Filename As String = Path.GetTempFileName()
        Dim Writer As New StreamWriter(Filename)
        Dim OutputFileNames As String = ""
        GetAllOutputFiles(ApsimUI.Data, Writer)
        Writer.Close()
        Writer = Nothing

        Dim CommandLine As String = APSIMSettings.ApsimDirectory() + "\bin\apsvis.exe"
        Process.Start(CommandLine, Filename)
    End Sub
    Private Sub ApsimOutlook()
        Dim Filename As String = Path.GetTempFileName()
        Dim Writer As New StreamWriter(Filename)
        Dim OutputFileNames As String = ""
        GetAllOutputFiles(ApsimUI.Data, Writer)
        Writer.Close()
        Writer = Nothing

        Dim CommandLine As String = APSIMSettings.ApsimDirectory() + "\bin\apsimoutlook.exe"
        Process.Start(CommandLine, Filename)
    End Sub
    Private Sub GetAllOutputFiles(ByVal Data As APSIMData, ByRef OutputFileNames As StreamWriter)
        For Each Child As APSIMData In Data.Children
            If Child.Type.ToLower() = "area" Or Child.Type.ToLower() = "simulation" Or Child.Type.ToLower() = "simulations" Then
                GetAllOutputFiles(Child, OutputFileNames)  ' recursion
            ElseIf Child.Type.ToLower() = "outputfile" Then
                Dim FullFileName As String = Child.ChildValue("filename")
                If ApsimUI.FileName <> "" Then
                    FullFileName = Path.Combine(Path.GetDirectoryName(ApsimUI.FileName), FullFileName)
                End If
                OutputFileNames.WriteLine(FullFileName)
            End If
        Next
    End Sub
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

        Dim NewData As New APSIMData("soils", "")
        NewData.Add(New Soil(New APSIMData("soil", "Blank soil")).Data)
        ApsimUI.FileNew(NewData)
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
End Class
