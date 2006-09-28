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
    Friend WithEvents SimulationContainer As System.Windows.Forms.ToolStripContainer
    Friend WithEvents SimulationToolStrip As System.Windows.Forms.ToolStrip
    Friend WithEvents RunButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator5 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents GraphButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ApsimOutlookButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ExcelButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolboxSplitter As System.Windows.Forms.Splitter
    Friend WithEvents NewButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents OpenButton As System.Windows.Forms.ToolStripSplitButton
    Friend WithEvents SaveButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents SaveAsButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents HelpContentsButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolBoxesToolStrip As System.Windows.Forms.ToolStrip
    Friend WithEvents ManageToolboxesButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents OpenButton2 As System.Windows.Forms.Button
    Friend WithEvents SimulationList As System.Windows.Forms.ListView
    Friend WithEvents NewButton2 As System.Windows.Forms.Button
    Private ToolBoxSplitterPoint As Integer
    Private SimulationHasRun As Boolean = False

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
        splash.VersionText = "Version " & APSIMSettings.ApsimVersion
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
        splash.Close()

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
    Friend WithEvents ToolBoxPanel As System.Windows.Forms.Panel
    Friend WithEvents ToolBoxPanelToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents ToolBoxToolBarPanel As System.Windows.Forms.Panel
    Friend WithEvents ToolboxButtonClose As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MainUI))
        Me.ToolBoxPanel = New System.Windows.Forms.Panel
        Me.ToolBoxToolBarPanel = New System.Windows.Forms.Panel
        Me.ToolboxButtonClose = New System.Windows.Forms.Button
        Me.ToolBoxPanelToolBar = New System.Windows.Forms.ToolBar
        Me.SimulationContainer = New System.Windows.Forms.ToolStripContainer
        Me.ToolBoxesToolStrip = New System.Windows.Forms.ToolStrip
        Me.ManageToolboxesButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator
        Me.NewButton2 = New System.Windows.Forms.Button
        Me.SimulationList = New System.Windows.Forms.ListView
        Me.OpenButton2 = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.ToolboxSplitter = New System.Windows.Forms.Splitter
        Me.SimulationToolStrip = New System.Windows.Forms.ToolStrip
        Me.NewButton = New System.Windows.Forms.ToolStripButton
        Me.OpenButton = New System.Windows.Forms.ToolStripSplitButton
        Me.SaveButton = New System.Windows.Forms.ToolStripButton
        Me.SaveAsButton = New System.Windows.Forms.ToolStripButton
        Me.HelpContentsButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStripSeparator5 = New System.Windows.Forms.ToolStripSeparator
        Me.RunButton = New System.Windows.Forms.ToolStripButton
        Me.GraphButton = New System.Windows.Forms.ToolStripButton
        Me.ApsimOutlookButton = New System.Windows.Forms.ToolStripButton
        Me.ExcelButton = New System.Windows.Forms.ToolStripButton
        Me.ToolBoxPanel.SuspendLayout()
        Me.ToolBoxToolBarPanel.SuspendLayout()
        Me.SimulationContainer.BottomToolStripPanel.SuspendLayout()
        Me.SimulationContainer.ContentPanel.SuspendLayout()
        Me.SimulationContainer.TopToolStripPanel.SuspendLayout()
        Me.SimulationContainer.SuspendLayout()
        Me.ToolBoxesToolStrip.SuspendLayout()
        Me.SimulationToolStrip.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolBoxPanel
        '
        Me.ToolBoxPanel.Controls.Add(Me.ToolBoxToolBarPanel)
        Me.ToolBoxPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolBoxPanel.Location = New System.Drawing.Point(0, 337)
        Me.ToolBoxPanel.Name = "ToolBoxPanel"
        Me.ToolBoxPanel.Size = New System.Drawing.Size(705, 104)
        Me.ToolBoxPanel.TabIndex = 12
        Me.ToolBoxPanel.Visible = False
        '
        'ToolBoxToolBarPanel
        '
        Me.ToolBoxToolBarPanel.BackColor = System.Drawing.Color.LightSteelBlue
        Me.ToolBoxToolBarPanel.Controls.Add(Me.ToolboxButtonClose)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.ToolBoxPanelToolBar)
        Me.ToolBoxToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ToolBoxToolBarPanel.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxToolBarPanel.Name = "ToolBoxToolBarPanel"
        Me.ToolBoxToolBarPanel.Size = New System.Drawing.Size(705, 32)
        Me.ToolBoxToolBarPanel.TabIndex = 19
        '
        'ToolboxButtonClose
        '
        Me.ToolboxButtonClose.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ToolboxButtonClose.BackColor = System.Drawing.Color.Transparent
        Me.ToolboxButtonClose.BackgroundImage = CType(resources.GetObject("ToolboxButtonClose.BackgroundImage"), System.Drawing.Image)
        Me.ToolboxButtonClose.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ToolboxButtonClose.Location = New System.Drawing.Point(676, 3)
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
        Me.ToolBoxPanelToolBar.Size = New System.Drawing.Size(705, 26)
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
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.NewButton2)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.SimulationList)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.OpenButton2)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.Label1)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.ToolboxSplitter)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.ToolBoxPanel)
        Me.SimulationContainer.ContentPanel.Size = New System.Drawing.Size(705, 441)
        Me.SimulationContainer.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationContainer.Location = New System.Drawing.Point(0, 0)
        Me.SimulationContainer.Name = "SimulationContainer"
        Me.SimulationContainer.Size = New System.Drawing.Size(705, 519)
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
        Me.ToolBoxesToolStrip.Size = New System.Drawing.Size(91, 31)
        Me.ToolBoxesToolStrip.TabIndex = 2
        '
        'ManageToolboxesButton
        '
        Me.ManageToolboxesButton.Image = Global.APSIMUI.My.Resources.Resources.toolbox_add_delete
        Me.ManageToolboxesButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.ManageToolboxesButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ManageToolboxesButton.Name = "ManageToolboxesButton"
        Me.ManageToolboxesButton.Size = New System.Drawing.Size(73, 28)
        Me.ManageToolboxesButton.Text = "&Manage"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(6, 31)
        '
        'NewButton2
        '
        Me.NewButton2.Cursor = System.Windows.Forms.Cursors.Hand
        Me.NewButton2.FlatAppearance.BorderSize = 0
        Me.NewButton2.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.NewButton2.Image = Global.APSIMUI.My.Resources.Resources.document_new
        Me.NewButton2.Location = New System.Drawing.Point(12, 52)
        Me.NewButton2.Name = "NewButton2"
        Me.NewButton2.Size = New System.Drawing.Size(184, 35)
        Me.NewButton2.TabIndex = 31
        Me.NewButton2.Text = "Create a new simulation"
        Me.NewButton2.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
        Me.NewButton2.UseVisualStyleBackColor = True
        '
        'SimulationList
        '
        Me.SimulationList.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.SimulationList.ForeColor = System.Drawing.SystemColors.ControlText
        Me.SimulationList.Location = New System.Drawing.Point(56, 134)
        Me.SimulationList.Name = "SimulationList"
        Me.SimulationList.Size = New System.Drawing.Size(439, 158)
        Me.SimulationList.TabIndex = 30
        Me.SimulationList.UseCompatibleStateImageBehavior = False
        Me.SimulationList.View = System.Windows.Forms.View.List
        '
        'OpenButton2
        '
        Me.OpenButton2.Cursor = System.Windows.Forms.Cursors.Hand
        Me.OpenButton2.FlatAppearance.BorderSize = 0
        Me.OpenButton2.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.OpenButton2.Image = Global.APSIMUI.My.Resources.Resources.folder_document
        Me.OpenButton2.Location = New System.Drawing.Point(12, 93)
        Me.OpenButton2.Name = "OpenButton2"
        Me.OpenButton2.Size = New System.Drawing.Size(184, 35)
        Me.OpenButton2.TabIndex = 32
        Me.OpenButton2.Text = "Open an existing simulation"
        Me.OpenButton2.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
        Me.OpenButton2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label1.BackColor = System.Drawing.SystemColors.InactiveCaptionText
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.ForeColor = System.Drawing.SystemColors.ActiveCaption
        Me.Label1.Location = New System.Drawing.Point(0, 26)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(705, 23)
        Me.Label1.TabIndex = 26
        Me.Label1.Text = "  APSIM Quick Start Menu"
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'ToolboxSplitter
        '
        Me.ToolboxSplitter.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolboxSplitter.Location = New System.Drawing.Point(0, 334)
        Me.ToolboxSplitter.Name = "ToolboxSplitter"
        Me.ToolboxSplitter.Size = New System.Drawing.Size(705, 3)
        Me.ToolboxSplitter.TabIndex = 25
        Me.ToolboxSplitter.TabStop = False
        '
        'SimulationToolStrip
        '
        Me.SimulationToolStrip.Dock = System.Windows.Forms.DockStyle.None
        Me.SimulationToolStrip.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewButton, Me.OpenButton, Me.SaveButton, Me.SaveAsButton, Me.HelpContentsButton, Me.ToolStripSeparator5, Me.RunButton, Me.GraphButton, Me.ApsimOutlookButton, Me.ExcelButton})
        Me.SimulationToolStrip.Location = New System.Drawing.Point(3, 0)
        Me.SimulationToolStrip.Name = "SimulationToolStrip"
        Me.SimulationToolStrip.Size = New System.Drawing.Size(505, 47)
        Me.SimulationToolStrip.TabIndex = 1
        '
        'NewButton
        '
        Me.NewButton.Image = CType(resources.GetObject("NewButton.Image"), System.Drawing.Image)
        Me.NewButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.NewButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.NewButton.Name = "NewButton"
        Me.NewButton.Size = New System.Drawing.Size(44, 44)
        Me.NewButton.Text = "&New..."
        Me.NewButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'OpenButton
        '
        Me.OpenButton.Image = Global.APSIMUI.My.Resources.Resources.folder_document
        Me.OpenButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.OpenButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.OpenButton.Name = "OpenButton"
        Me.OpenButton.Size = New System.Drawing.Size(61, 44)
        Me.OpenButton.Text = "&Open..."
        Me.OpenButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'SaveButton
        '
        Me.SaveButton.Image = Global.APSIMUI.My.Resources.Resources.disk_blue
        Me.SaveButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.SaveButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.SaveButton.Name = "SaveButton"
        Me.SaveButton.Size = New System.Drawing.Size(35, 44)
        Me.SaveButton.Text = "&Save"
        Me.SaveButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'SaveAsButton
        '
        Me.SaveAsButton.Image = Global.APSIMUI.My.Resources.Resources.disk_blue_window
        Me.SaveAsButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.SaveAsButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.SaveAsButton.Name = "SaveAsButton"
        Me.SaveAsButton.Size = New System.Drawing.Size(61, 44)
        Me.SaveAsButton.Text = "Save &as..."
        Me.SaveAsButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'HelpContentsButton
        '
        Me.HelpContentsButton.Image = Global.APSIMUI.My.Resources.Resources.help2
        Me.HelpContentsButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.HelpContentsButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.HelpContentsButton.Name = "HelpContentsButton"
        Me.HelpContentsButton.Size = New System.Drawing.Size(32, 44)
        Me.HelpContentsButton.Text = "&Help"
        Me.HelpContentsButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'ToolStripSeparator5
        '
        Me.ToolStripSeparator5.Name = "ToolStripSeparator5"
        Me.ToolStripSeparator5.Size = New System.Drawing.Size(6, 47)
        '
        'RunButton
        '
        Me.RunButton.AutoSize = False
        Me.RunButton.Image = Global.APSIMUI.My.Resources.Resources.media_play
        Me.RunButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.RunButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.RunButton.Name = "RunButton"
        Me.RunButton.Size = New System.Drawing.Size(68, 44)
        Me.RunButton.Text = "&Run"
        Me.RunButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'GraphButton
        '
        Me.GraphButton.Image = Global.APSIMUI.My.Resources.Resources.chart
        Me.GraphButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.GraphButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.GraphButton.Name = "GraphButton"
        Me.GraphButton.Size = New System.Drawing.Size(40, 44)
        Me.GraphButton.Text = "&Graph"
        Me.GraphButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'ApsimOutlookButton
        '
        Me.ApsimOutlookButton.Image = Global.APSIMUI.My.Resources.Resources.boxplot
        Me.ApsimOutlookButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.ApsimOutlookButton.ImageTransparentColor = System.Drawing.SystemColors.ControlLight
        Me.ApsimOutlookButton.Name = "ApsimOutlookButton"
        Me.ApsimOutlookButton.Size = New System.Drawing.Size(79, 44)
        Me.ApsimOutlookButton.Text = "Apsim Outlook"
        Me.ApsimOutlookButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'ExcelButton
        '
        Me.ExcelButton.Image = Global.APSIMUI.My.Resources.Resources.excel
        Me.ExcelButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.ExcelButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ExcelButton.Name = "ExcelButton"
        Me.ExcelButton.Size = New System.Drawing.Size(36, 44)
        Me.ExcelButton.Text = "E&xcel"
        Me.ExcelButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(705, 519)
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
        Me.SimulationContainer.TopToolStripPanel.ResumeLayout(False)
        Me.SimulationContainer.TopToolStripPanel.PerformLayout()
        Me.SimulationContainer.ResumeLayout(False)
        Me.SimulationContainer.PerformLayout()
        Me.ToolBoxesToolStrip.ResumeLayout(False)
        Me.ToolBoxesToolStrip.PerformLayout()
        Me.SimulationToolStrip.ResumeLayout(False)
        Me.SimulationToolStrip.PerformLayout()
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
        AddHandler ApsimUI.NewDataEvent, AddressOf OnNewData
        AddHandler ApsimUI.SelectionChangedEvent, AddressOf OnSelectionChanged

        ' Show the Simulation Explorer.
        SimulationExplorer = New ExplorerUI(Me)
        SimulationExplorer.Dock = DockStyle.Fill
        SimulationExplorer.Parent = SimulationContainer.ContentPanel
        SimulationExplorer.Visible = False
        SimulationExplorer.BringToFront()

        ' Setup but don't show the Toolbox Explorer.
        Toolbox = New ApsimUIController(".xml", _
                                         "Toolbox files (*.xml)|*.xml|" + _
                                            "Soils files (*.soils)|*.soils|" + _
                                            "All files (*.*)|*.*", _
                                        "")
        ToolboxExplorer = New ExplorerUI(Nothing)
        ToolboxExplorer.Parent = ToolBoxPanel
        ToolboxExplorer.Dock = DockStyle.Fill
        ToolboxExplorer.BringToFront()

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

        ' Populate the simulation list 
        Dim FileNames() As String = ApsimUI.GetFrequentList()
        For Each File As String In FileNames
            Dim item As New ListViewItem(File, 0)
            item.ImageIndex = 0
            SimulationList.Items.Add(item)
        Next
        SimulationList.Visible = FileNames.Length > 0

        CreateRecentFileListMenu()
        PopulateToolBoxStrip()
        SetFunctionality()
    End Sub

#End Region
#Region "Application level methods"


    Private Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        SetFunctionality()
    End Sub
    Private Sub OnDataChanged(ByVal ChangedData As APSIMData)
        SetFunctionality()
    End Sub
    Private Sub OnNewData()
        ' New data has entered the system.
        ' This is usually caused by FileNew,
        ' FileOpen etc.        
        AddHandler ApsimUI.AllData.DataChanged, AddressOf OnDataChanged
        APSIMChangeTool.Upgrade(ApsimUI.AllData)
        SimulationExplorer.Visible = True
        SimulationExplorer.RefreshView(ApsimUI)
        SetFunctionality()
    End Sub

    Private Sub CreateRecentFileListMenu()
        'Creates a list of recent files under the 'File' menu

        Dim FileNames() As String = ApsimUI.GetFrequentList()

        For Each str As String In FileNames
            Dim NewItem As New System.Windows.Forms.ToolStripMenuItem
            NewItem.Text = str
            AddHandler NewItem.Click, AddressOf OpenSimulation
            OpenButton.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {NewItem})
        Next

    End Sub

    Private Sub OpenSimulation(ByVal sender As Object, ByVal e As EventArgs)
        'Open the simulation provided by the recent simulations menu item
        Dim MenuItem As ToolStripMenuItem = sender
        ApsimUI.FileOpen(MenuItem.Text)
    End Sub



    Private Sub SetFunctionality()
        ' Enable / Disable bits of functionality as 
        ' required. i.e. ensure program is in a 
        ' consistant state.
        Dim SomethingInTree As Boolean = Not IsNothing(ApsimUI.AllData) AndAlso ApsimUI.AllData.ChildNames.Length > 0

        RunButton.Enabled = SomethingInTree
        GraphButton.Enabled = SomethingInTree
        ApsimOutlookButton.Enabled = SomethingInTree
        Me.ExcelButton.Enabled = SomethingInTree

        If Not SimulationHasRun Then
            GraphButton.Enabled = False
            ApsimOutlookButton.Enabled = False
            Me.ExcelButton.Enabled = False
        End If
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

            If ToolboxExplorer.Visible And Toolbox.AllowDataChanges Then
                Toolbox.FileSave()
            End If
        End If
    End Sub
#End Region
#Region "Main button bar"

    Private Sub SimulationList_DoubleClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationList.DoubleClick
        If SimulationList.SelectedItems.Count > 0 Then
            Dim SelectedFile As String = SimulationList.SelectedItems(0).Text
            ApsimUI.FileOpen(SelectedFile)
            SimulationHasRun = False
            SetFunctionality()
        End If
    End Sub

    Private Sub OnNewFileClick(ByVal sender As Object, ByVal e As EventArgs) Handles NewButton.Click, NewButton2.Click
        Dim NewData As APSIMData = ApsimUI.LetUserSelectNewDocument()
        If Not IsNothing(NewData) Then
            ApsimUI.FileNew(NewData)
        End If
        SimulationHasRun = False
        SetFunctionality()
    End Sub

    Private Sub OnOpenFileClick(ByVal sender As Object, ByVal e As EventArgs) Handles OpenButton2.Click, OpenButton.ButtonClick
        ApsimUI.FileOpen()
        SimulationHasRun = False
        SetFunctionality()
    End Sub

    Private Sub OnSaveFileClick(ByVal sender As Object, ByVal e As EventArgs) Handles SaveButton.Click
        ApsimUI.FileSave()
    End Sub

    Private Sub OnSaveAsClick(ByVal sender As Object, ByVal e As EventArgs) Handles SaveAsButton.Click
        ApsimUI.FileSaveAs()
    End Sub

    Private Sub ONCutClick(ByVal sender As Object, ByVal e As EventArgs)
        ApsimUI.Cut()
    End Sub

    Private Sub OnCopyClick(ByVal sender As Object, ByVal e As EventArgs)
        ApsimUI.Copy()
    End Sub

    Private Sub OnPasteClick(ByVal sender As Object, ByVal e As EventArgs)
        ApsimUI.Paste()
    End Sub

    Private Sub OnHelpClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpContentsButton.Click
        Dim HelpURL As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "docfile")
        ApsimUI.ShowHelp(HelpURL)
    End Sub

    Private Sub OnRunClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunButton.Click
        ' ---------------------------------------------------------------
        ' Go run APSIM.
        ' ---------------------------------------------------------------
        Try
            SimulationHasRun = RunSimulations()
            SetFunctionality()
        Catch fnf As System.IO.FileNotFoundException
            MessageBox.Show(fnf.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Sub OnGraphClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GraphButton.Click
        ' ---------------------------------------------------------------
        ' Send output files to APSVis
        ' ---------------------------------------------------------------
        apsvisFiles(GetCSVListOfOutputFiles())
    End Sub

    Private Sub OnApsimOutlookClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ApsimOutlookButton.Click
        ' ---------------------------------------------------------------
        ' Send output files to ApsimOutlook
        ' ---------------------------------------------------------------
        apsimoutlookFiles(GetCSVListOfOutputFiles())
    End Sub

    Private Sub OnExcelClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExcelButton.Click
        ' ---------------------------------------------------------------
        ' Send output files to Excel
        ' ---------------------------------------------------------------
        excelFiles(GetCSVListOfOutputFiles())
    End Sub

    Private Function GetCSVListOfOutputFiles() As String
        ' ---------------------------------------------------------------
        ' Return to caller a list of all output files that the user has 
        ' selected to export. Returns "" if user hits cancel.
        ' ---------------------------------------------------------------
        Dim OutputFiles As StringCollection = ApsimUI.GetOutputFilesUnder(ApsimUI.AllData)
        If OutputFiles.Count <> 0 Then
            'Only show export screen if more than one output file
            If OutputFiles.Count = 1 Then
                Return OutputFiles(0)
            Else
                Dim ExportForm As New OutputFileExport(OutputFiles)
                If ExportForm.ShowDialog() = Windows.Forms.DialogResult.Cancel Then
                    Return ""
                Else
                    Dim ReturnString As String = ""
                    For Each St As String In ExportForm.SelectedOutputFiles
                        ReturnString += "," & St
                    Next
                    Return ReturnString
                End If
            End If
        Else
            MessageBox.Show("Please run the simulation first")
        End If
    End Function

    Private Function RunSimulations() As Boolean
        ' ---------------------------------------------------------------
        ' Save the simulation and then do an APSIM run.
        ' ---------------------------------------------------------------
        If ApsimUI.FileSave() Then
            ' kill old apsim processes
            Dim AllProcesses As Process() = Process.GetProcesses()
            For Each proc As Process In AllProcesses
                If Path.GetFileName(proc.ProcessName) = "apsrun" Then
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
        Return True
    End Function

#End Region
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
                ImageFileName = APSIMSettings.ApsimDirectory() + "\ApsimUI\Images\toolbox.png"
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
            ToolBoxPanel.Height = Val(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "toolboxheight"))
            ToolBoxPanel.Height = ToolBoxPanel.Height - 1
            ToolBoxPanel.Height = ToolBoxPanel.Height + 1

            ToolboxSplitter.Visible = True
            ToolBoxPanel.Visible = True
            Me.ToolBoxSplitterPoint = ToolboxSplitter.SplitPosition

            Dim toolboxes As New Toolboxes
            Dim filename As String = toolboxes.NameToFileName(Sender.ToString)
            ToolboxExplorer.ExpandAll = False
            Toolbox.FileOpen(filename)
            ToolboxExplorer.RefreshView(Toolbox)

            APSIMChangeTool.Upgrade(Toolbox.AllData)
            If Toolbox.DirtyData Then
                Toolbox.FileSave()
            End If
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

        If Toolbox.AllowDataChanges Then
            Toolbox.FileSave()
        End If

        ToolBoxPanel.Visible = False
        ToolboxSplitter.Visible = ToolBoxPanel.Visible
    End Sub

    Private Sub ToolBoxSplitter_LocationChanged(ByVal sender As Object, ByVal e As SplitterEventArgs) Handles ToolboxSplitter.SplitterMoved
        ' ---------------------------------------------------------------
        ' Whenever the user moves the toolbox splitter, save the position
        ' ---------------------------------------------------------------
        If ToolBoxPanel.Visible Then
            Dim inifile As New APSIMSettings
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "apsimui", "toolboxheight", Str(ToolBoxPanel.Height))
        End If
    End Sub

#End Region

End Class
