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

        Xceed.Chart.Licenser.LicenseKey = "CHT30-YTL57-0UXLJ-145A"
        Xceed.SmartUI.Licenser.LicenseKey = "SUN31-9TL57-SUXL5-F4BA"

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
    Friend WithEvents MainToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents FileButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents FileToolBarPanel As System.Windows.Forms.Panel
    Friend WithEvents splitter5 As System.Windows.Forms.Splitter
    Friend WithEvents FilePanel2 As System.Windows.Forms.Panel
    Friend WithEvents FileLabel2 As System.Windows.Forms.Label
    Friend WithEvents splitter4 As System.Windows.Forms.Splitter
    Friend WithEvents FilePanel1 As System.Windows.Forms.Panel
    Friend WithEvents FileToolBar1 As System.Windows.Forms.ToolBar
    Friend WithEvents FileLabel1 As System.Windows.Forms.Label
    Friend WithEvents VersionButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents NewFileButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents OpenFileButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents SaveFileButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents SaveFileAsButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents SimulationButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolboxButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents FileToolBar2 As System.Windows.Forms.ToolBar
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
    Friend WithEvents MakeSimFileButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolboxesToolbarPanel As System.Windows.Forms.Panel
    Friend WithEvents Panel3 As System.Windows.Forms.Panel
    Friend WithEvents ToolboxLabel As System.Windows.Forms.Label
    Friend WithEvents OpenToolboxButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents Splitter3 As System.Windows.Forms.Splitter
    Friend WithEvents ClipboardPanel As System.Windows.Forms.Panel
    Friend WithEvents ClipboardToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents ClipboardLabel As System.Windows.Forms.Label
    Friend WithEvents CutButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents CopyButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents PasteButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents Splitter6 As System.Windows.Forms.Splitter
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
        Me.MainToolBar = New System.Windows.Forms.ToolBar
        Me.FileButton = New System.Windows.Forms.ToolBarButton
        Me.SimulationButton = New System.Windows.Forms.ToolBarButton
        Me.ToolboxButton = New System.Windows.Forms.ToolBarButton
        Me.FileToolBarPanel = New System.Windows.Forms.Panel
        Me.splitter5 = New System.Windows.Forms.Splitter
        Me.FilePanel2 = New System.Windows.Forms.Panel
        Me.FileToolBar2 = New System.Windows.Forms.ToolBar
        Me.VersionButton = New System.Windows.Forms.ToolBarButton
        Me.FileLabel2 = New System.Windows.Forms.Label
        Me.splitter4 = New System.Windows.Forms.Splitter
        Me.FilePanel1 = New System.Windows.Forms.Panel
        Me.FileToolBar1 = New System.Windows.Forms.ToolBar
        Me.NewFileButton = New System.Windows.Forms.ToolBarButton
        Me.OpenFileButton = New System.Windows.Forms.ToolBarButton
        Me.SaveFileButton = New System.Windows.Forms.ToolBarButton
        Me.SaveFileAsButton = New System.Windows.Forms.ToolBarButton
        Me.FileLabel1 = New System.Windows.Forms.Label
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
        Me.MakeSimFileButton = New System.Windows.Forms.ToolBarButton
        Me.SimulationLabel2 = New System.Windows.Forms.Label
        Me.Splitter6 = New System.Windows.Forms.Splitter
        Me.ClipboardPanel = New System.Windows.Forms.Panel
        Me.ClipboardToolBar = New System.Windows.Forms.ToolBar
        Me.CutButton = New System.Windows.Forms.ToolBarButton
        Me.CopyButton = New System.Windows.Forms.ToolBarButton
        Me.PasteButton = New System.Windows.Forms.ToolBarButton
        Me.ClipboardLabel = New System.Windows.Forms.Label
        Me.ToolboxesToolbarPanel = New System.Windows.Forms.Panel
        Me.Splitter3 = New System.Windows.Forms.Splitter
        Me.Panel3 = New System.Windows.Forms.Panel
        Me.ToolBoxToolBar = New System.Windows.Forms.ToolBar
        Me.OpenToolboxButton = New System.Windows.Forms.ToolBarButton
        Me.ManageToolBoxesButton = New System.Windows.Forms.ToolBarButton
        Me.ToolboxLabel = New System.Windows.Forms.Label
        Me.ToolBoxPanel.SuspendLayout()
        Me.ToolBoxToolBarPanel.SuspendLayout()
        Me.FileToolBarPanel.SuspendLayout()
        Me.FilePanel2.SuspendLayout()
        Me.FilePanel1.SuspendLayout()
        Me.SimulationToolBarPanel.SuspendLayout()
        Me.HelpPanel.SuspendLayout()
        Me.SimulationPanel1.SuspendLayout()
        Me.SimulationPanel2.SuspendLayout()
        Me.ClipboardPanel.SuspendLayout()
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
        'MainToolBar
        '
        Me.MainToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.MainToolBar.AutoSize = False
        Me.MainToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.FileButton, Me.SimulationButton, Me.ToolboxButton})
        Me.MainToolBar.ButtonSize = New System.Drawing.Size(65, 30)
        Me.MainToolBar.Divider = False
        Me.MainToolBar.DropDownArrows = True
        Me.MainToolBar.Location = New System.Drawing.Point(0, 0)
        Me.MainToolBar.Name = "MainToolBar"
        Me.MainToolBar.ShowToolTips = True
        Me.MainToolBar.Size = New System.Drawing.Size(632, 24)
        Me.MainToolBar.TabIndex = 15
        Me.MainToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        Me.MainToolBar.Wrappable = False
        '
        'FileButton
        '
        Me.FileButton.Pushed = True
        Me.FileButton.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton
        Me.FileButton.Text = "&File"
        '
        'SimulationButton
        '
        Me.SimulationButton.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton
        Me.SimulationButton.Text = "&Simulation"
        '
        'ToolboxButton
        '
        Me.ToolboxButton.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton
        Me.ToolboxButton.Text = "&Toolboxes"
        '
        'FileToolBarPanel
        '
        Me.FileToolBarPanel.BackColor = System.Drawing.Color.LightSteelBlue
        Me.FileToolBarPanel.Controls.Add(Me.splitter5)
        Me.FileToolBarPanel.Controls.Add(Me.FilePanel2)
        Me.FileToolBarPanel.Controls.Add(Me.splitter4)
        Me.FileToolBarPanel.Controls.Add(Me.FilePanel1)
        Me.FileToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.FileToolBarPanel.Location = New System.Drawing.Point(0, 24)
        Me.FileToolBarPanel.Name = "FileToolBarPanel"
        Me.FileToolBarPanel.Size = New System.Drawing.Size(632, 71)
        Me.FileToolBarPanel.TabIndex = 18
        '
        'splitter5
        '
        Me.splitter5.BackColor = System.Drawing.Color.LightGray
        Me.splitter5.Enabled = False
        Me.splitter5.Location = New System.Drawing.Point(334, 0)
        Me.splitter5.Name = "splitter5"
        Me.splitter5.Size = New System.Drawing.Size(1, 71)
        Me.splitter5.TabIndex = 23
        Me.splitter5.TabStop = False
        '
        'FilePanel2
        '
        Me.FilePanel2.BackColor = System.Drawing.Color.Transparent
        Me.FilePanel2.Controls.Add(Me.FileToolBar2)
        Me.FilePanel2.Controls.Add(Me.FileLabel2)
        Me.FilePanel2.Dock = System.Windows.Forms.DockStyle.Left
        Me.FilePanel2.Location = New System.Drawing.Point(273, 0)
        Me.FilePanel2.Name = "FilePanel2"
        Me.FilePanel2.Size = New System.Drawing.Size(61, 71)
        Me.FilePanel2.TabIndex = 22
        '
        'FileToolBar2
        '
        Me.FileToolBar2.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.FileToolBar2.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.VersionButton})
        Me.FileToolBar2.Divider = False
        Me.FileToolBar2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FileToolBar2.DropDownArrows = True
        Me.FileToolBar2.ImageList = Me.ButtonImageList
        Me.FileToolBar2.Location = New System.Drawing.Point(0, 20)
        Me.FileToolBar2.Name = "FileToolBar2"
        Me.FileToolBar2.ShowToolTips = True
        Me.FileToolBar2.Size = New System.Drawing.Size(61, 48)
        Me.FileToolBar2.TabIndex = 16
        '
        'VersionButton
        '
        Me.VersionButton.ImageIndex = 17
        Me.VersionButton.Text = "&Version"
        '
        'FileLabel2
        '
        Me.FileLabel2.BackColor = System.Drawing.Color.SteelBlue
        Me.FileLabel2.Dock = System.Windows.Forms.DockStyle.Top
        Me.FileLabel2.ForeColor = System.Drawing.Color.White
        Me.FileLabel2.Location = New System.Drawing.Point(0, 0)
        Me.FileLabel2.Name = "FileLabel2"
        Me.FileLabel2.Size = New System.Drawing.Size(61, 20)
        Me.FileLabel2.TabIndex = 15
        Me.FileLabel2.Text = "About"
        Me.FileLabel2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'splitter4
        '
        Me.splitter4.BackColor = System.Drawing.Color.LightGray
        Me.splitter4.Enabled = False
        Me.splitter4.Location = New System.Drawing.Point(272, 0)
        Me.splitter4.Name = "splitter4"
        Me.splitter4.Size = New System.Drawing.Size(1, 71)
        Me.splitter4.TabIndex = 21
        Me.splitter4.TabStop = False
        '
        'FilePanel1
        '
        Me.FilePanel1.BackColor = System.Drawing.Color.Transparent
        Me.FilePanel1.Controls.Add(Me.FileToolBar1)
        Me.FilePanel1.Controls.Add(Me.FileLabel1)
        Me.FilePanel1.Dock = System.Windows.Forms.DockStyle.Left
        Me.FilePanel1.Location = New System.Drawing.Point(0, 0)
        Me.FilePanel1.Name = "FilePanel1"
        Me.FilePanel1.Size = New System.Drawing.Size(272, 71)
        Me.FilePanel1.TabIndex = 20
        '
        'FileToolBar1
        '
        Me.FileToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.FileToolBar1.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.NewFileButton, Me.OpenFileButton, Me.SaveFileButton, Me.SaveFileAsButton})
        Me.FileToolBar1.Divider = False
        Me.FileToolBar1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FileToolBar1.DropDownArrows = True
        Me.FileToolBar1.ImageList = Me.ButtonImageList
        Me.FileToolBar1.Location = New System.Drawing.Point(0, 20)
        Me.FileToolBar1.Name = "FileToolBar1"
        Me.FileToolBar1.ShowToolTips = True
        Me.FileToolBar1.Size = New System.Drawing.Size(272, 48)
        Me.FileToolBar1.TabIndex = 16
        '
        'NewFileButton
        '
        Me.NewFileButton.ImageIndex = 1
        Me.NewFileButton.Text = "&New file"
        Me.NewFileButton.ToolTipText = "Create a new soils file"
        '
        'OpenFileButton
        '
        Me.OpenFileButton.ImageIndex = 0
        Me.OpenFileButton.Text = "&Open file"
        Me.OpenFileButton.ToolTipText = "Open a new soils file"
        '
        'SaveFileButton
        '
        Me.SaveFileButton.ImageIndex = 2
        Me.SaveFileButton.Text = "&Save file"
        Me.SaveFileButton.ToolTipText = "Save the current file"
        '
        'SaveFileAsButton
        '
        Me.SaveFileAsButton.ImageIndex = 21
        Me.SaveFileAsButton.Text = "Save &As file"
        Me.SaveFileAsButton.ToolTipText = "Save file under new name"
        '
        'FileLabel1
        '
        Me.FileLabel1.BackColor = System.Drawing.Color.SteelBlue
        Me.FileLabel1.Dock = System.Windows.Forms.DockStyle.Top
        Me.FileLabel1.ForeColor = System.Drawing.Color.White
        Me.FileLabel1.Location = New System.Drawing.Point(0, 0)
        Me.FileLabel1.Name = "FileLabel1"
        Me.FileLabel1.Size = New System.Drawing.Size(272, 20)
        Me.FileLabel1.TabIndex = 15
        Me.FileLabel1.Text = "File management"
        Me.FileLabel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
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
        Me.SimulationToolBarPanel.Controls.Add(Me.Splitter6)
        Me.SimulationToolBarPanel.Controls.Add(Me.ClipboardPanel)
        Me.SimulationToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.SimulationToolBarPanel.Location = New System.Drawing.Point(0, 95)
        Me.SimulationToolBarPanel.Name = "SimulationToolBarPanel"
        Me.SimulationToolBarPanel.Size = New System.Drawing.Size(632, 71)
        Me.SimulationToolBarPanel.TabIndex = 19
        Me.SimulationToolBarPanel.Visible = False
        '
        'Splitter7
        '
        Me.Splitter7.BackColor = System.Drawing.Color.LightGray
        Me.Splitter7.Enabled = False
        Me.Splitter7.Location = New System.Drawing.Point(480, 0)
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
        Me.HelpPanel.Location = New System.Drawing.Point(426, 0)
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
        Me.Splitter2.Location = New System.Drawing.Point(425, 0)
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
        Me.SimulationPanel1.Location = New System.Drawing.Point(273, 0)
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
        Me.Splitter1.Location = New System.Drawing.Point(272, 0)
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
        Me.SimulationPanel2.Location = New System.Drawing.Point(137, 0)
        Me.SimulationPanel2.Name = "SimulationPanel2"
        Me.SimulationPanel2.Size = New System.Drawing.Size(135, 71)
        Me.SimulationPanel2.TabIndex = 22
        '
        'SimulationToolBar2
        '
        Me.SimulationToolBar2.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.SimulationToolBar2.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.RunButton, Me.MakeSimFileButton})
        Me.SimulationToolBar2.Divider = False
        Me.SimulationToolBar2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationToolBar2.DropDownArrows = True
        Me.SimulationToolBar2.ImageList = Me.ButtonImageList
        Me.SimulationToolBar2.Location = New System.Drawing.Point(0, 20)
        Me.SimulationToolBar2.Name = "SimulationToolBar2"
        Me.SimulationToolBar2.ShowToolTips = True
        Me.SimulationToolBar2.Size = New System.Drawing.Size(135, 48)
        Me.SimulationToolBar2.TabIndex = 16
        '
        'RunButton
        '
        Me.RunButton.ImageIndex = 14
        Me.RunButton.Text = "&Run"
        Me.RunButton.ToolTipText = "Run APSIM"
        '
        'MakeSimFileButton
        '
        Me.MakeSimFileButton.ImageIndex = 18
        Me.MakeSimFileButton.Text = "&Make .sim file"
        Me.MakeSimFileButton.ToolTipText = "Make APSIM .sim files for simulations"
        '
        'SimulationLabel2
        '
        Me.SimulationLabel2.BackColor = System.Drawing.Color.SteelBlue
        Me.SimulationLabel2.Dock = System.Windows.Forms.DockStyle.Top
        Me.SimulationLabel2.ForeColor = System.Drawing.Color.White
        Me.SimulationLabel2.Location = New System.Drawing.Point(0, 0)
        Me.SimulationLabel2.Name = "SimulationLabel2"
        Me.SimulationLabel2.Size = New System.Drawing.Size(135, 20)
        Me.SimulationLabel2.TabIndex = 15
        Me.SimulationLabel2.Text = "Run simulation"
        Me.SimulationLabel2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Splitter6
        '
        Me.Splitter6.BackColor = System.Drawing.Color.LightGray
        Me.Splitter6.Enabled = False
        Me.Splitter6.Location = New System.Drawing.Point(136, 0)
        Me.Splitter6.Name = "Splitter6"
        Me.Splitter6.Size = New System.Drawing.Size(1, 71)
        Me.Splitter6.TabIndex = 25
        Me.Splitter6.TabStop = False
        '
        'ClipboardPanel
        '
        Me.ClipboardPanel.BackColor = System.Drawing.Color.Transparent
        Me.ClipboardPanel.Controls.Add(Me.ClipboardToolBar)
        Me.ClipboardPanel.Controls.Add(Me.ClipboardLabel)
        Me.ClipboardPanel.Dock = System.Windows.Forms.DockStyle.Left
        Me.ClipboardPanel.Location = New System.Drawing.Point(0, 0)
        Me.ClipboardPanel.Name = "ClipboardPanel"
        Me.ClipboardPanel.Size = New System.Drawing.Size(136, 71)
        Me.ClipboardPanel.TabIndex = 24
        '
        'ClipboardToolBar
        '
        Me.ClipboardToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ClipboardToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.CutButton, Me.CopyButton, Me.PasteButton})
        Me.ClipboardToolBar.Divider = False
        Me.ClipboardToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ClipboardToolBar.DropDownArrows = True
        Me.ClipboardToolBar.ImageList = Me.ButtonImageList
        Me.ClipboardToolBar.Location = New System.Drawing.Point(0, 20)
        Me.ClipboardToolBar.Name = "ClipboardToolBar"
        Me.ClipboardToolBar.ShowToolTips = True
        Me.ClipboardToolBar.Size = New System.Drawing.Size(136, 48)
        Me.ClipboardToolBar.TabIndex = 16
        '
        'CutButton
        '
        Me.CutButton.ImageIndex = 3
        Me.CutButton.Text = "Cu&t"
        Me.CutButton.ToolTipText = "Cut the selected soil(s) to the clipboard"
        '
        'CopyButton
        '
        Me.CopyButton.ImageIndex = 4
        Me.CopyButton.Text = "&Copy"
        Me.CopyButton.ToolTipText = "Copy the seleted soil(s) to the clipboard"
        '
        'PasteButton
        '
        Me.PasteButton.ImageIndex = 5
        Me.PasteButton.Text = "&Paste"
        Me.PasteButton.ToolTipText = "Paste the soils from the clipboard"
        '
        'ClipboardLabel
        '
        Me.ClipboardLabel.BackColor = System.Drawing.Color.SteelBlue
        Me.ClipboardLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ClipboardLabel.ForeColor = System.Drawing.Color.White
        Me.ClipboardLabel.Location = New System.Drawing.Point(0, 0)
        Me.ClipboardLabel.Name = "ClipboardLabel"
        Me.ClipboardLabel.Size = New System.Drawing.Size(136, 20)
        Me.ClipboardLabel.TabIndex = 15
        Me.ClipboardLabel.Text = "Clipboard"
        Me.ClipboardLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'ToolboxesToolbarPanel
        '
        Me.ToolboxesToolbarPanel.BackColor = System.Drawing.Color.LightSteelBlue
        Me.ToolboxesToolbarPanel.Controls.Add(Me.Splitter3)
        Me.ToolboxesToolbarPanel.Controls.Add(Me.Panel3)
        Me.ToolboxesToolbarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ToolboxesToolbarPanel.Location = New System.Drawing.Point(0, 166)
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
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(632, 546)
        Me.Controls.Add(Me.ToolBoxSplitter)
        Me.Controls.Add(Me.ToolBoxPanel)
        Me.Controls.Add(Me.ToolboxesToolbarPanel)
        Me.Controls.Add(Me.SimulationToolBarPanel)
        Me.Controls.Add(Me.FileToolBarPanel)
        Me.Controls.Add(Me.MainToolBar)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.KeyPreview = True
        Me.Name = "MainUI"
        Me.Text = "APSIM"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.ToolBoxPanel.ResumeLayout(False)
        Me.ToolBoxToolBarPanel.ResumeLayout(False)
        Me.FileToolBarPanel.ResumeLayout(False)
        Me.FilePanel2.ResumeLayout(False)
        Me.FilePanel1.ResumeLayout(False)
        Me.SimulationToolBarPanel.ResumeLayout(False)
        Me.HelpPanel.ResumeLayout(False)
        Me.SimulationPanel1.ResumeLayout(False)
        Me.SimulationPanel2.ResumeLayout(False)
        Me.ClipboardPanel.ResumeLayout(False)
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

        SaveFileButton.Enabled = ApsimUI.AllowFileSave
        SaveFileAsButton.Enabled = ApsimUI.AllowFileSaveAs
        CutButton.Enabled = ApsimUI.AllowCut
        CopyButton.Enabled = ApsimUI.AllowCopy
        PasteButton.Enabled = ApsimUI.AllowPaste

        RunButton.Enabled = SomethingInTree
        MakeSimFileButton.Enabled = SomethingInTree

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
    Private Sub ToolBar_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles MainToolBar.ButtonClick, FileToolBar1.ButtonClick, FileToolBar2.ButtonClick, ClipboardToolBar.ButtonClick, SimulationToolBar1.ButtonClick, SimulationToolBar2.ButtonClick, HelpToolBar.ButtonClick, ToolBoxToolBar.ButtonClick
        If e.Button.Parent Is MainToolBar Then
            ' Use has clicked a top level menu button
            For Each Button As ToolBarButton In MainToolBar.Buttons
                If Not Button Is e.Button Then
                    Button.Pushed = False
                End If
            Next
            e.Button.Pushed = True
            FileToolBarPanel.Visible = FileButton.Pushed
            SimulationToolBarPanel.Visible = SimulationButton.Pushed
            ToolboxesToolbarPanel.Visible = ToolboxButton.Pushed

        ElseIf e.Button Is NewFileButton Then
            Dim NewData As APSIMData = ApsimUI.LetUserSelectNewDocument()
            If Not IsNothing(NewData) Then
                ApsimUI.FileNew(NewData)
            End If
            ToolBar_ButtonClick(Nothing, New ToolBarButtonClickEventArgs(SimulationButton))
        ElseIf e.Button Is OpenFileButton Then
            ApsimUI.FileOpen()
            ToolBar_ButtonClick(Nothing, New ToolBarButtonClickEventArgs(SimulationButton))
        ElseIf e.Button Is SaveFileButton Then
            ApsimUI.FileSave()
        ElseIf e.Button Is SaveFileAsButton Then
            ApsimUI.FileSaveAs()
            ApsimUI.ShowHelp(APSIMSettings.ApsimDirectory + "\docs\documentation.xml")
        ElseIf e.Button Is VersionButton Then
            Dim Form As New HelpAboutForm
            Form.ShowDialog()

        ElseIf e.Button Is CutButton Then
            ApsimUI.Cut()
        ElseIf e.Button Is CopyButton Then
            ApsimUI.Copy()
        ElseIf e.Button Is PasteButton Then
            ApsimUI.Paste()
        ElseIf e.Button Is RunButton Then
            RunSimulations()
        ElseIf e.Button Is MakeSimFileButton Then
            MakeSimFiles()
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
#Region "Simulation run methods"
    Private Function MakeSimFiles() As StringCollection
        ApsimUI.FileSave()

        Dim inifile As New APSIMSettings
        Dim TypesData As New APSIMData
        TypesData.LoadFromFile(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "typesfile"))

        Try
            Dim SimFiles As New StringCollection
            SimulationExplorer.Save()

            Dim M As New Macro
            If File.Exists(ApsimUI.FileName) Then
                Dim DirectoryName As String = Path.GetDirectoryName(ApsimUI.FileName)
                For Each Sim As APSIMData In ApsimUI.AllData.Children("simulation")
                    Dim MacroContents As String = CreateMacroFile(TypesData, Sim, 1)
                    SimFiles = M.Go(ApsimUI.AllData, MacroContents, DirectoryName, False)
                Next
            End If
            Return SimFiles
        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error")
        End Try
    End Function
    Private Function CreateMacroFile(ByVal TypesData As APSIMData, ByVal Data As APSIMData, ByVal Level As Integer) As String
        ' Create a macro file by going through the specified
        ' simulation and for each node, goto types.xml and
        ' pull out the SimMacro for that node.
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
                            Dim SearchString As String = "[insert " + Child.Type.ToLower() + "]"
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
    Private Sub RunSimulations()
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


    Protected Overrides Function ProcessDialogKey(ByVal keyData As System.Windows.Forms.Keys) As Boolean
        Dim Button As ToolBarButton = ApsimUI.ProcessDialogKey(Me, keyData)
        If Not Button Is Nothing Then
            ToolBar_ButtonClick(Nothing, New ToolBarButtonClickEventArgs(Button))
            Return True
        End If
    End Function
End Class
