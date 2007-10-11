Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.xml
Imports System.xml.XmlNodeList
Imports System.io
Imports System.IO.Path
Imports System.Reflection
Imports VBGeneral
Imports CSGeneral
Imports ApsimFile
Imports VBUserInterface

Public Class MainUI
    Inherits System.Windows.Forms.Form

    Private ApsimUI As BaseController
    Private Toolbox As BaseController
    Private SimulationExplorer As ExplorerUI
    Private ToolboxExplorer As ExplorerUI
    Private CurrentRunningSimulationIndex As Integer
    Private ApsimProcess As ProcessCaller
    Private SimFileName As String
    Private CurrentSummaryFile As StreamWriter = Nothing
    Private CurrentStartDate As Date
    Private CurrentEndDate As Date
    Private CurrentErrors As New StringCollection
    Private ApplicationName As String


    Friend WithEvents SimulationContainer As System.Windows.Forms.ToolStripContainer
    Friend WithEvents SimulationToolStrip As System.Windows.Forms.ToolStrip
    Friend WithEvents ToolboxSplitter As System.Windows.Forms.Splitter
    Friend WithEvents ToolBoxesToolStrip As System.Windows.Forms.ToolStrip
    Friend WithEvents ManageToolboxesButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Private ToolBoxSplitterPoint As Integer
    Friend WithEvents RunPanel As System.Windows.Forms.Panel
    Friend WithEvents RunToolBarPanel As System.Windows.Forms.Panel
    Friend WithEvents RunPanelButtonClose As System.Windows.Forms.Button
    Friend WithEvents RunPanelToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents RunPanelSplitter As System.Windows.Forms.Splitter
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents RunProgressPanel As System.Windows.Forms.Panel
    Friend WithEvents OverallProgressBar As System.Windows.Forms.ProgressBar
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents CurrentProgressBar As System.Windows.Forms.ProgressBar
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents ToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents RunPanelListBox As System.Windows.Forms.RichTextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Private Args As StringCollection


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

        ' get command line arguments.
        Args = StringManip.SplitStringHonouringQuotes(Microsoft.VisualBasic.Command, " ")

        'This call is required by the Windows Form Designer.
        InitializeComponent()

    End Sub
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub
    Public Sub Go(ByVal CommandLine As String)
        Args = StringManip.SplitStringHonouringQuotes(CommandLine, " ")
        Me.Show()
        Application.Run(Me)
    End Sub
#End Region

#Region "Windows Form Designer generated code "


    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.
    'Do not modify it using the code editor.
    Friend WithEvents ToolBoxPanel As System.Windows.Forms.Panel
    Friend WithEvents ToolBoxPanelToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents ToolBoxToolBarPanel As System.Windows.Forms.Panel
    Friend WithEvents ToolboxButtonClose As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MainUI))
        Me.ToolBoxPanel = New System.Windows.Forms.Panel
        Me.ToolBoxToolBarPanel = New System.Windows.Forms.Panel
        Me.Label6 = New System.Windows.Forms.Label
        Me.Label5 = New System.Windows.Forms.Label
        Me.ToolboxButtonClose = New System.Windows.Forms.Button
        Me.ToolBoxPanelToolBar = New System.Windows.Forms.ToolBar
        Me.SimulationContainer = New System.Windows.Forms.ToolStripContainer
        Me.ToolBoxesToolStrip = New System.Windows.Forms.ToolStrip
        Me.ManageToolboxesButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator
        Me.Label1 = New System.Windows.Forms.Label
        Me.RunPanelSplitter = New System.Windows.Forms.Splitter
        Me.RunPanel = New System.Windows.Forms.Panel
        Me.RunPanelListBox = New System.Windows.Forms.RichTextBox
        Me.RunProgressPanel = New System.Windows.Forms.Panel
        Me.OverallProgressBar = New System.Windows.Forms.ProgressBar
        Me.Label4 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.CurrentProgressBar = New System.Windows.Forms.ProgressBar
        Me.RunToolBarPanel = New System.Windows.Forms.Panel
        Me.Label2 = New System.Windows.Forms.Label
        Me.RunPanelButtonClose = New System.Windows.Forms.Button
        Me.RunPanelToolBar = New System.Windows.Forms.ToolBar
        Me.ToolboxSplitter = New System.Windows.Forms.Splitter
        Me.SimulationToolStrip = New System.Windows.Forms.ToolStrip
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolBoxPanel.SuspendLayout()
        Me.ToolBoxToolBarPanel.SuspendLayout()
        Me.SimulationContainer.BottomToolStripPanel.SuspendLayout()
        Me.SimulationContainer.ContentPanel.SuspendLayout()
        Me.SimulationContainer.TopToolStripPanel.SuspendLayout()
        Me.SimulationContainer.SuspendLayout()
        Me.ToolBoxesToolStrip.SuspendLayout()
        Me.RunPanel.SuspendLayout()
        Me.RunProgressPanel.SuspendLayout()
        Me.RunToolBarPanel.SuspendLayout()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolBoxPanel
        '
        Me.ToolBoxPanel.Controls.Add(Me.ToolBoxToolBarPanel)
        Me.ToolBoxPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolBoxPanel.Location = New System.Drawing.Point(0, 423)
        Me.ToolBoxPanel.Name = "ToolBoxPanel"
        Me.ToolBoxPanel.Size = New System.Drawing.Size(735, 104)
        Me.ToolBoxPanel.TabIndex = 12
        Me.ToolBoxPanel.Visible = False
        '
        'ToolBoxToolBarPanel
        '
        Me.ToolBoxToolBarPanel.BackColor = System.Drawing.Color.SkyBlue
        Me.ToolBoxToolBarPanel.Controls.Add(Me.Label6)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.Label5)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.ToolboxButtonClose)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.ToolBoxPanelToolBar)
        Me.ToolBoxToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ToolBoxToolBarPanel.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxToolBarPanel.Name = "ToolBoxToolBarPanel"
        Me.ToolBoxToolBarPanel.Size = New System.Drawing.Size(735, 24)
        Me.ToolBoxToolBarPanel.TabIndex = 19
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label6.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label6.Location = New System.Drawing.Point(3, 3)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(58, 16)
        Me.Label6.TabIndex = 22
        Me.Label6.Text = "Toolbox"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label5.Location = New System.Drawing.Point(3, 7)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(58, 16)
        Me.Label5.TabIndex = 22
        Me.Label5.Text = "Toolbox"
        '
        'ToolboxButtonClose
        '
        Me.ToolboxButtonClose.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ToolboxButtonClose.BackColor = System.Drawing.Color.Transparent
        Me.ToolboxButtonClose.BackgroundImage = CType(resources.GetObject("ToolboxButtonClose.BackgroundImage"), System.Drawing.Image)
        Me.ToolboxButtonClose.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ToolboxButtonClose.Location = New System.Drawing.Point(706, -1)
        Me.ToolboxButtonClose.Name = "ToolboxButtonClose"
        Me.ToolboxButtonClose.Size = New System.Drawing.Size(24, 24)
        Me.ToolboxButtonClose.TabIndex = 20
        Me.ToolboxButtonClose.TabStop = False
        Me.ToolboxButtonClose.UseVisualStyleBackColor = False
        '
        'ToolBoxPanelToolBar
        '
        Me.ToolBoxPanelToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBoxPanelToolBar.Divider = False
        Me.ToolBoxPanelToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ToolBoxPanelToolBar.DropDownArrows = True
        Me.ToolBoxPanelToolBar.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxPanelToolBar.Name = "ToolBoxPanelToolBar"
        Me.ToolBoxPanelToolBar.ShowToolTips = True
        Me.ToolBoxPanelToolBar.Size = New System.Drawing.Size(735, 26)
        Me.ToolBoxPanelToolBar.TabIndex = 17
        Me.ToolBoxPanelToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        '
        'SimulationContainer
        '
        '
        'SimulationContainer.BottomToolStripPanel
        '
        Me.SimulationContainer.BottomToolStripPanel.Controls.Add(Me.ToolBoxesToolStrip)
        '
        'SimulationContainer.ContentPanel
        '
        Me.SimulationContainer.ContentPanel.AutoScroll = True
        Me.SimulationContainer.ContentPanel.BackColor = System.Drawing.SystemColors.Window
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.Label1)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.RunPanelSplitter)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.RunPanel)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.ToolboxSplitter)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.ToolBoxPanel)
        Me.SimulationContainer.ContentPanel.Size = New System.Drawing.Size(735, 527)
        Me.SimulationContainer.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationContainer.Location = New System.Drawing.Point(0, 0)
        Me.SimulationContainer.Name = "SimulationContainer"
        Me.SimulationContainer.Size = New System.Drawing.Size(735, 583)
        Me.SimulationContainer.TabIndex = 4
        Me.SimulationContainer.Text = "ToolStripContainer1"
        '
        'SimulationContainer.TopToolStripPanel
        '
        Me.SimulationContainer.TopToolStripPanel.Controls.Add(Me.SimulationToolStrip)
        '
        'ToolBoxesToolStrip
        '
        Me.ToolBoxesToolStrip.Dock = System.Windows.Forms.DockStyle.None
        Me.ToolBoxesToolStrip.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ManageToolboxesButton, Me.ToolStripSeparator2})
        Me.ToolBoxesToolStrip.Location = New System.Drawing.Point(3, 0)
        Me.ToolBoxesToolStrip.Name = "ToolBoxesToolStrip"
        Me.ToolBoxesToolStrip.Size = New System.Drawing.Size(96, 31)
        Me.ToolBoxesToolStrip.TabIndex = 2
        '
        'ManageToolboxesButton
        '
        Me.ManageToolboxesButton.Image = Global.APSIMUI.My.Resources.Resources.toolbox_add_delete
        Me.ManageToolboxesButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.ManageToolboxesButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ManageToolboxesButton.Name = "ManageToolboxesButton"
        Me.ManageToolboxesButton.Size = New System.Drawing.Size(78, 28)
        Me.ManageToolboxesButton.Text = "&Manage"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(6, 31)
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.ForeColor = System.Drawing.SystemColors.Highlight
        Me.Label1.Location = New System.Drawing.Point(19, 39)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(525, 16)
        Me.Label1.TabIndex = 35
        Me.Label1.Text = "To get started click on the NEW button above or the OPEN button to load an existi" & _
            "ng file."
        '
        'RunPanelSplitter
        '
        Me.RunPanelSplitter.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.RunPanelSplitter.Location = New System.Drawing.Point(0, 281)
        Me.RunPanelSplitter.Name = "RunPanelSplitter"
        Me.RunPanelSplitter.Size = New System.Drawing.Size(735, 3)
        Me.RunPanelSplitter.TabIndex = 34
        Me.RunPanelSplitter.TabStop = False
        Me.RunPanelSplitter.Visible = False
        '
        'RunPanel
        '
        Me.RunPanel.Controls.Add(Me.RunPanelListBox)
        Me.RunPanel.Controls.Add(Me.RunProgressPanel)
        Me.RunPanel.Controls.Add(Me.RunToolBarPanel)
        Me.RunPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.RunPanel.Location = New System.Drawing.Point(0, 284)
        Me.RunPanel.Name = "RunPanel"
        Me.RunPanel.Size = New System.Drawing.Size(735, 136)
        Me.RunPanel.TabIndex = 33
        Me.RunPanel.Visible = False
        '
        'RunPanelListBox
        '
        Me.RunPanelListBox.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.RunPanelListBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RunPanelListBox.Font = New System.Drawing.Font("Courier New", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.RunPanelListBox.Location = New System.Drawing.Point(0, 55)
        Me.RunPanelListBox.Name = "RunPanelListBox"
        Me.RunPanelListBox.Size = New System.Drawing.Size(735, 81)
        Me.RunPanelListBox.TabIndex = 23
        Me.RunPanelListBox.Text = ""
        Me.RunPanelListBox.WordWrap = False
        '
        'RunProgressPanel
        '
        Me.RunProgressPanel.BackColor = System.Drawing.Color.AntiqueWhite
        Me.RunProgressPanel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.RunProgressPanel.Controls.Add(Me.OverallProgressBar)
        Me.RunProgressPanel.Controls.Add(Me.Label4)
        Me.RunProgressPanel.Controls.Add(Me.Label3)
        Me.RunProgressPanel.Controls.Add(Me.CurrentProgressBar)
        Me.RunProgressPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.RunProgressPanel.Location = New System.Drawing.Point(0, 24)
        Me.RunProgressPanel.Name = "RunProgressPanel"
        Me.RunProgressPanel.Size = New System.Drawing.Size(735, 31)
        Me.RunProgressPanel.TabIndex = 22
        '
        'OverallProgressBar
        '
        Me.OverallProgressBar.Location = New System.Drawing.Point(462, 2)
        Me.OverallProgressBar.Name = "OverallProgressBar"
        Me.OverallProgressBar.Size = New System.Drawing.Size(183, 23)
        Me.OverallProgressBar.TabIndex = 3
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(370, 6)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(86, 13)
        Me.Label4.TabIndex = 2
        Me.Label4.Text = "Overall progress:"
        Me.Label4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(18, 8)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(136, 13)
        Me.Label3.TabIndex = 1
        Me.Label3.Text = "Current simulation progress:"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'CurrentProgressBar
        '
        Me.CurrentProgressBar.Location = New System.Drawing.Point(171, 3)
        Me.CurrentProgressBar.Name = "CurrentProgressBar"
        Me.CurrentProgressBar.Size = New System.Drawing.Size(183, 23)
        Me.CurrentProgressBar.TabIndex = 0
        '
        'RunToolBarPanel
        '
        Me.RunToolBarPanel.BackColor = System.Drawing.Color.SkyBlue
        Me.RunToolBarPanel.Controls.Add(Me.Label2)
        Me.RunToolBarPanel.Controls.Add(Me.RunPanelButtonClose)
        Me.RunToolBarPanel.Controls.Add(Me.RunPanelToolBar)
        Me.RunToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.RunToolBarPanel.Location = New System.Drawing.Point(0, 0)
        Me.RunToolBarPanel.Name = "RunToolBarPanel"
        Me.RunToolBarPanel.Size = New System.Drawing.Size(735, 24)
        Me.RunToolBarPanel.TabIndex = 20
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label2.Location = New System.Drawing.Point(3, 4)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(79, 16)
        Me.Label2.TabIndex = 21
        Me.Label2.Text = "Run window"
        '
        'RunPanelButtonClose
        '
        Me.RunPanelButtonClose.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.RunPanelButtonClose.BackColor = System.Drawing.Color.Transparent
        Me.RunPanelButtonClose.BackgroundImage = CType(resources.GetObject("RunPanelButtonClose.BackgroundImage"), System.Drawing.Image)
        Me.RunPanelButtonClose.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.RunPanelButtonClose.Location = New System.Drawing.Point(706, 0)
        Me.RunPanelButtonClose.Name = "RunPanelButtonClose"
        Me.RunPanelButtonClose.Size = New System.Drawing.Size(24, 24)
        Me.RunPanelButtonClose.TabIndex = 20
        Me.RunPanelButtonClose.TabStop = False
        Me.RunPanelButtonClose.UseVisualStyleBackColor = False
        '
        'RunPanelToolBar
        '
        Me.RunPanelToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.RunPanelToolBar.Divider = False
        Me.RunPanelToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RunPanelToolBar.DropDownArrows = True
        Me.RunPanelToolBar.Location = New System.Drawing.Point(0, 0)
        Me.RunPanelToolBar.Name = "RunPanelToolBar"
        Me.RunPanelToolBar.ShowToolTips = True
        Me.RunPanelToolBar.Size = New System.Drawing.Size(735, 26)
        Me.RunPanelToolBar.TabIndex = 17
        Me.RunPanelToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        '
        'ToolboxSplitter
        '
        Me.ToolboxSplitter.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolboxSplitter.Location = New System.Drawing.Point(0, 420)
        Me.ToolboxSplitter.Name = "ToolboxSplitter"
        Me.ToolboxSplitter.Size = New System.Drawing.Size(735, 3)
        Me.ToolboxSplitter.TabIndex = 25
        Me.ToolboxSplitter.TabStop = False
        Me.ToolboxSplitter.Visible = False
        '
        'SimulationToolStrip
        '
        Me.SimulationToolStrip.Dock = System.Windows.Forms.DockStyle.None
        Me.SimulationToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.SimulationToolStrip.Location = New System.Drawing.Point(3, 0)
        Me.SimulationToolStrip.Name = "SimulationToolStrip"
        Me.SimulationToolStrip.Size = New System.Drawing.Size(102, 25)
        Me.SimulationToolStrip.TabIndex = 1
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem1})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(184, 26)
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        Me.ToolStripMenuItem1.Size = New System.Drawing.Size(183, 22)
        Me.ToolStripMenuItem1.Text = "ToolStripMenuItem1"
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(735, 583)
        Me.Controls.Add(Me.SimulationContainer)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.KeyPreview = True
        Me.Name = "MainUI"
        Me.Text = "APSIM"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.ToolBoxPanel.ResumeLayout(False)
        Me.ToolBoxToolBarPanel.ResumeLayout(False)
        Me.ToolBoxToolBarPanel.PerformLayout()
        Me.SimulationContainer.BottomToolStripPanel.ResumeLayout(False)
        Me.SimulationContainer.BottomToolStripPanel.PerformLayout()
        Me.SimulationContainer.ContentPanel.ResumeLayout(False)
        Me.SimulationContainer.ContentPanel.PerformLayout()
        Me.SimulationContainer.TopToolStripPanel.ResumeLayout(False)
        Me.SimulationContainer.TopToolStripPanel.PerformLayout()
        Me.SimulationContainer.ResumeLayout(False)
        Me.SimulationContainer.PerformLayout()
        Me.ToolBoxesToolStrip.ResumeLayout(False)
        Me.ToolBoxesToolStrip.PerformLayout()
        Me.RunPanel.ResumeLayout(False)
        Me.RunProgressPanel.ResumeLayout(False)
        Me.RunProgressPanel.PerformLayout()
        Me.RunToolBarPanel.ResumeLayout(False)
        Me.RunToolBarPanel.PerformLayout()
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub OnMainFormLoad(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ' Get application name.
        If Args.Count > 0 Then
            If (Args(0)(0) = "/") Then
                ApplicationName = Args(0).Substring(1)
                Args.RemoveAt(0)
            Else
                ApplicationName = "ApsimUI"
            End If
        Else
            ApplicationName = "ApsimUI"
        End If
        Text = ApplicationName

        ' Position window correctly.
        Try
            Dim inifile As New APSIMSettings
            WindowState = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "windowstate"))
            Top = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "top"))
            Left = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "left"))
            Height = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "height"))
            Width = Convert.ToInt32(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "width"))
        Catch ex As System.Exception
            Me.WindowState = FormWindowState.Maximized
        End Try

        ' Display splash screen.
        Dim Splash As SplashScreenForm = Nothing
        If APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "SplashScreen") <> "" Then
            Splash = New SplashScreenForm(ApplicationName)
            If APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "SplashScreenButtonVisible").ToLower = "yes" Then
                Splash.ShowDialog()
            Else
                Splash.Show()
                Application.DoEvents()
            End If
        End If

        ApsimUI = New BaseController(Me, ApplicationName)
        ' Show the Simulation Explorer.
        SimulationExplorer = New ExplorerUI(ApsimUI)
        SimulationExplorer.OnLoad(ApsimUI, "")
        SimulationExplorer.Dock = DockStyle.Fill
        SimulationExplorer.Parent = SimulationContainer.ContentPanel
        SimulationExplorer.Visible = False
        SimulationExplorer.BringToFront()

        AddHandler ApsimUI.ApsimData.NewDataEvent, AddressOf OnNewData

        ' Load some assemblies for later. The code for some actions are found in
        ' these assemblies.
        Assembly.Load("Graph")
        Assembly.Load("CSUserInterface")

        ' Give the explorer ui to the controller.
        ApsimUI.Explorer = SimulationExplorer

        Dim ToolboxesVisible As Boolean = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "ToolboxesVisible").ToLower = "yes"
        If ToolboxesVisible Then
            ' Setup but don't show the Toolbox Explorer.
            Toolbox = New BaseController(Nothing, ApplicationName)
            ToolboxExplorer = New ExplorerUI(Toolbox)
            ToolboxExplorer.OnLoad(Toolbox, "")
            ToolboxExplorer.Parent = ToolBoxPanel
            ToolboxExplorer.Dock = DockStyle.Fill
            ToolboxExplorer.BringToFront()

            Toolbox.Explorer = ToolboxExplorer
            PopulateToolBoxStrip()
        Else
            ToolBoxesToolStrip.Visible = False
        End If

        ' Load a file if one was specified on the command line.
        If Args.Count > 0 Then
            Dim FileName As String = Args(0).Replace("""", "")
            If FileName.Length() > 0 Then
                ApsimUI.FileOpen(FileName)
            End If
        End If

        ' If no file loaded then load previous one.
        If ApsimUI.FileName = Nothing Then
            ApsimUI.LoadPreviousFile()
        End If

        ApsimUI.ProvideToolStrip(SimulationToolStrip, "MainToolBar")

    End Sub
    Private Sub OnMainFormClosing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        ' User is closing down - save our work.
        e.Cancel = Not ApsimUI.FileSaveAfterPrompt()
        If Not e.Cancel Then
            Try
                Dim inifile As New APSIMSettings
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), ApplicationName, "windowstate", Str(Me.WindowState))
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), ApplicationName, "top", Str(Me.Top))
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), ApplicationName, "left", Str(Me.Left))
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), ApplicationName, "width", Str(Me.Width))
                APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), ApplicationName, "height", Str(Me.Height))
            Catch ex As System.Exception
            End Try

            If Not IsNothing(ToolboxExplorer) AndAlso ToolboxExplorer.Visible AndAlso Not Toolbox.ApsimData.IsReadOnly Then
                Toolbox.ApsimData.Save(Toolbox.FileName)
            End If
        End If
    End Sub
    Private Sub OnRunPanelCloseClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunPanelButtonClose.Click
        ' -----------------------------------------
        ' User wants to close the run window.
        ' -----------------------------------------
        RunPanel.Visible = False
        RunPanelSplitter.Visible = False
    End Sub
    Private Sub OnNewData()
        If ApsimUI.ApsimData.ChildNames(ApsimUI.ApsimData.RootNodeName).Length = 1 Then
            Dim ChildName As String = ApsimUI.ApsimData.RootNodeName + "\" + ApsimUI.ApsimData.ChildNames(ApsimUI.ApsimData.RootNodeName)(0)
            Dim ChildType As String = ApsimUI.ApsimData.Find(ChildName).Type
            If ChildType.ToLower = "simulation" Then
                SimulationExplorer.ExpandAll()
                Return
            End If
        End If
        SimulationExplorer.ExpandAllFolders()
    End Sub

#Region "Toolbox button bar"

    Private Sub ManageToolboxesButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ManageToolboxesButton.Click
        ' ---------------------------------------------------------------
        ' User wants to manage toolboxes.
        ' ---------------------------------------------------------------
        Dim Form As New OptionsForm
        Form.ShowDialog(Me)
        PopulateToolBoxStrip()
    End Sub
    Private Sub PopulateToolBoxStrip()
        ' ---------------------------------------------------------------
        ' Populate the toolbox strip with buttons for each toolbox.
        ' ---------------------------------------------------------------

        'Remove existing buttons first.
        Dim NumButtons As Integer = ToolBoxesToolStrip.Items.Count
        For I As Integer = 2 To NumButtons - 1
            Dim Button As ToolStripButton = ToolBoxesToolStrip.Items(2)
            ToolBoxesToolStrip.Items.Remove(Button)
        Next

        Dim toolboxes As New Toolboxes

        ' Loop through each of the known toolboxes
        For Each FileName As String In toolboxes.Names
            Dim apsimData As New VBGeneral.APSIMData
            Dim toolBoxPath As String = toolboxes.NameToFileName(FileName)

            apsimData.LoadFromFile(toolBoxPath)

            ' Get the image attribute from the root node of the loaded xml file
            Dim ImageFileName As String = apsimData.Attribute("image")

            If ImageFileName.IndexOf(":") = -1 Then
                ImageFileName = APSIMSettings.ApsimDirectory() + "\ApsimUI\" + ImageFileName
            End If

            ' If a predefined image has been given then attempt to add it.  
            If Not System.IO.File.Exists(ImageFileName) Then
                ImageFileName = APSIMSettings.ApsimDirectory() + "\ApsimUI\Images\toolbox24.png"
            End If

            Dim NewItem As New ToolStripButton(FileName, New System.Drawing.Bitmap(ImageFileName))
            NewItem.TextImageRelation = TextImageRelation.ImageBeforeText
            NewItem.ImageScaling = ToolStripItemImageScaling.None
            NewItem.CheckOnClick = True
            AddHandler NewItem.Click, AddressOf OnToolBoxClick
            ToolBoxesToolStrip.Items.Add(NewItem)
        Next
    End Sub
    Private Sub OnToolBoxClick(ByVal Sender As Object, ByVal e As System.EventArgs)
        ' ---------------------------------------------------------------
        ' Display the given ToolBoxName in the toolbox panel at
        ' ---------------------------------------------------------------
        Dim ButtonThatWasClicked As ToolStripButton = Sender
        If Not ButtonThatWasClicked.Checked Then
            HideToolBoxWindow(ButtonThatWasClicked, e)
        Else
            ' Turn off the checked status of all toolbox buttons - except the one
            ' that was just clicked.
            For i As Integer = 2 To ToolBoxesToolStrip.Items.Count - 1
                Dim Button As ToolStripButton = ToolBoxesToolStrip.Items(i)
                If Not Button Is ButtonThatWasClicked Then
                    Button.Checked = False
                End If
            Next

            Dim inifile As New APSIMSettings
            ToolBoxPanel.Height = Val(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "toolboxheight"))
            ToolBoxPanel.Height = ToolBoxPanel.Height - 1
            ToolBoxPanel.Height = ToolBoxPanel.Height + 1

            ToolboxSplitter.Visible = True
            ToolBoxPanel.Visible = True
            Me.ToolBoxSplitterPoint = ToolboxSplitter.SplitPosition

            Dim toolboxes As New Toolboxes
            Dim filename As String = toolboxes.NameToFileName(Sender.ToString)
            Toolbox.FileOpen(filename)
            ToolboxExplorer.OnRefresh("\")
            Toolbox.Explorer.ExpandOneLevel()
        End If
    End Sub
    Private Sub HideToolBoxWindow(ByVal Sender As Object, ByVal e As EventArgs) Handles ToolboxButtonClose.Click
        ' ---------------------------------------------------------------
        ' Hide the toolbox window.
        ' ---------------------------------------------------------------

        ' Turn off the checked status of all toolbox buttons.
        For i As Integer = 2 To ToolBoxesToolStrip.Items.Count - 1
            Dim Button As ToolStripButton = ToolBoxesToolStrip.Items(i)
            Button.Checked = False
        Next

        Toolbox.ApsimData.Save(Toolbox.FileName)

        ToolBoxPanel.Visible = False
        ToolboxSplitter.Visible = ToolBoxPanel.Visible
    End Sub
    Private Sub ToolBoxSplitter_LocationChanged(ByVal sender As Object, ByVal e As SplitterEventArgs) Handles ToolboxSplitter.SplitterMoved
        ' ---------------------------------------------------------------
        ' Whenever the user moves the toolbox splitter, save the position
        ' ---------------------------------------------------------------
        If ToolBoxPanel.Visible Then
            Dim inifile As New APSIMSettings
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), ApplicationName, "toolboxheight", Str(ToolBoxPanel.Height))
        End If
    End Sub

#End Region


End Class
