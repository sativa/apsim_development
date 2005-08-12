Imports VBGeneral
Imports ChangeTool

Public Class StartupUI
    Inherits VBGeneral.BaseUI

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
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents ImageList1 As System.Windows.Forms.ImageList
    Friend WithEvents NewLinkLabel As System.Windows.Forms.LinkLabel
    Friend WithEvents SavedFilesPanel As System.Windows.Forms.Panel
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents SimulationList As System.Windows.Forms.ListView
    Friend WithEvents Label4 As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(StartupUI))
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.NewLinkLabel = New System.Windows.Forms.LinkLabel
        Me.Label3 = New System.Windows.Forms.Label
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.SavedFilesPanel = New System.Windows.Forms.Panel
        Me.Label5 = New System.Windows.Forms.Label
        Me.SimulationList = New System.Windows.Forms.ListView
        Me.Label4 = New System.Windows.Forms.Label
        Me.SavedFilesPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(128, 40)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(200, 32)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Welcome to APSIM."
        '
        'Label2
        '
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(128, 80)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(440, 24)
        Me.Label2.TabIndex = 3
        Me.Label2.Text = "To get started with creating a new simulation you should goto"
        '
        'NewLinkLabel
        '
        Me.NewLinkLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.NewLinkLabel.Location = New System.Drawing.Point(176, 104)
        Me.NewLinkLabel.Name = "NewLinkLabel"
        Me.NewLinkLabel.Size = New System.Drawing.Size(296, 23)
        Me.NewLinkLabel.TabIndex = 4
        Me.NewLinkLabel.TabStop = True
        Me.NewLinkLabel.Text = "File | New or click the New button"
        '
        'Label3
        '
        Me.Label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label3.Location = New System.Drawing.Point(128, 128)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(440, 40)
        Me.Label3.TabIndex = 5
        Me.Label3.Text = "and select a pre-built simulation that is closest to the type of simulation you w" & _
        "ant to build."
        '
        'PictureBox1
        '
        Me.PictureBox1.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(0, 20)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(100, 709)
        Me.PictureBox1.TabIndex = 6
        Me.PictureBox1.TabStop = False
        '
        'ImageList1
        '
        Me.ImageList1.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList1.ImageStream = CType(resources.GetObject("ImageList1.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList1.TransparentColor = System.Drawing.Color.Transparent
        '
        'SavedFilesPanel
        '
        Me.SavedFilesPanel.Controls.Add(Me.Label5)
        Me.SavedFilesPanel.Controls.Add(Me.SimulationList)
        Me.SavedFilesPanel.Controls.Add(Me.Label4)
        Me.SavedFilesPanel.Location = New System.Drawing.Point(128, 176)
        Me.SavedFilesPanel.Name = "SavedFilesPanel"
        Me.SavedFilesPanel.Size = New System.Drawing.Size(496, 376)
        Me.SavedFilesPanel.TabIndex = 10
        '
        'Label5
        '
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(24, 304)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(440, 40)
        Me.Label5.TabIndex = 12
        Me.Label5.Text = "The simulation components will then appear in the box to the left."
        '
        'SimulationList
        '
        Me.SimulationList.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.SimulationList.Location = New System.Drawing.Point(64, 64)
        Me.SimulationList.Name = "SimulationList"
        Me.SimulationList.Size = New System.Drawing.Size(320, 216)
        Me.SimulationList.SmallImageList = Me.ImageList1
        Me.SimulationList.TabIndex = 11
        Me.SimulationList.View = System.Windows.Forms.View.List
        '
        'Label4
        '
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(16, 8)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(440, 40)
        Me.Label4.TabIndex = 10
        Me.Label4.Text = "Alternatively you can re-open a simulation you were working on last time by doubl" & _
        "e clicking on one of the following simulations."
        '
        'StartupUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(817, 764)
        Me.Controls.Add(Me.SavedFilesPanel)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.NewLinkLabel)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Name = "StartupUI"
        Me.Controls.SetChildIndex(Me.Label1, 0)
        Me.Controls.SetChildIndex(Me.Label2, 0)
        Me.Controls.SetChildIndex(Me.NewLinkLabel, 0)
        Me.Controls.SetChildIndex(Me.Label3, 0)
        Me.Controls.SetChildIndex(Me.PictureBox1, 0)
        Me.Controls.SetChildIndex(Me.SavedFilesPanel, 0)
        Me.SavedFilesPanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    ' ------------------------
    ' Load up simulation list.
    ' ------------------------
    Private Sub StartupUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim FileNames() As String = Explorer.GetFrequentList()
        For Each File As String In FileNames
            Dim item As New ListViewItem(File, 0)
            item.ImageIndex = 0
            SimulationList.Items.Add(item)
        Next
        SavedFilesPanel.Visible = FileNames.Length > 0
    End Sub


    ' ------------------------------------
    ' User is wanting to open a simulation
    ' ------------------------------------
    Private Sub SimulationList_DoubleClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SimulationList.DoubleClick
        Dim SelectedFile As String = SimulationList.SelectedItems(0).Text
        Explorer.FileOpen(SelectedFile)
        APSIMChangeTool.Upgrade(Explorer.Data)
        Explorer.Refresh()
    End Sub


    ' ------------------------------------------
    ' User is wanting to create a new simulation
    ' ------------------------------------------
    Private Sub NewLinkLabel_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles NewLinkLabel.Click
        Dim UIManager As New UIManager(Nothing)
        Dim NewData As APSIMData = UIManager.LetUserSelectNewDocument()
        If Not IsNothing(NewData) Then
            Explorer.FileNew(NewData)
        End If
    End Sub
End Class
