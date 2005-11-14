Imports VBGeneral
Imports ChangeTool

Public Class StartupUI
    Inherits VBGeneral.BaseView

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
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents ImageList1 As System.Windows.Forms.ImageList
    Friend WithEvents SavedFilesPanel As System.Windows.Forms.Panel
    Friend WithEvents SimulationList As System.Windows.Forms.ListView
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents NewButton As System.Windows.Forms.Button
    Friend WithEvents OpenButton As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(StartupUI))
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.SavedFilesPanel = New System.Windows.Forms.Panel
        Me.SimulationList = New System.Windows.Forms.ListView
        Me.Label4 = New System.Windows.Forms.Label
        Me.NewButton = New System.Windows.Forms.Button
        Me.OpenButton = New System.Windows.Forms.Button
        Me.SavedFilesPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.ForeColor = System.Drawing.SystemColors.ActiveCaption
        Me.Label1.Location = New System.Drawing.Point(134, 48)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(200, 32)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Welcome to APSIM."
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(136, 96)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(164, 18)
        Me.Label2.TabIndex = 3
        Me.Label2.Text = "What would you like to do?"
        '
        'PictureBox1
        '
        Me.PictureBox1.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(0, 40)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(100, 733)
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
        Me.SavedFilesPanel.Controls.Add(Me.SimulationList)
        Me.SavedFilesPanel.Controls.Add(Me.Label4)
        Me.SavedFilesPanel.Location = New System.Drawing.Point(136, 184)
        Me.SavedFilesPanel.Name = "SavedFilesPanel"
        Me.SavedFilesPanel.Size = New System.Drawing.Size(488, 304)
        Me.SavedFilesPanel.TabIndex = 10
        '
        'SimulationList
        '
        Me.SimulationList.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.SimulationList.Location = New System.Drawing.Point(0, 32)
        Me.SimulationList.Name = "SimulationList"
        Me.SimulationList.Size = New System.Drawing.Size(488, 272)
        Me.SimulationList.SmallImageList = Me.ImageList1
        Me.SimulationList.TabIndex = 11
        Me.SimulationList.View = System.Windows.Forms.View.List
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(0, 8)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(280, 18)
        Me.Label4.TabIndex = 10
        Me.Label4.Text = "Open a previous simulation by double clicking."
        '
        'NewButton
        '
        Me.NewButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.NewButton.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.NewButton.ForeColor = System.Drawing.SystemColors.ActiveCaption
        Me.NewButton.Location = New System.Drawing.Point(136, 120)
        Me.NewButton.Name = "NewButton"
        Me.NewButton.Size = New System.Drawing.Size(160, 23)
        Me.NewButton.TabIndex = 11
        Me.NewButton.Text = "Create new simulation..."
        '
        'OpenButton
        '
        Me.OpenButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.OpenButton.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.OpenButton.ForeColor = System.Drawing.SystemColors.ActiveCaption
        Me.OpenButton.Location = New System.Drawing.Point(320, 120)
        Me.OpenButton.Name = "OpenButton"
        Me.OpenButton.Size = New System.Drawing.Size(160, 23)
        Me.OpenButton.TabIndex = 12
        Me.OpenButton.Text = "Open existing..."
        '
        'StartupUI
        '
        Me.BackColor = System.Drawing.SystemColors.Window
        Me.Controls.Add(Me.OpenButton)
        Me.Controls.Add(Me.NewButton)
        Me.Controls.Add(Me.SavedFilesPanel)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Name = "StartupUI"
        Me.Size = New System.Drawing.Size(705, 773)
        Me.Controls.SetChildIndex(Me.Label1, 0)
        Me.Controls.SetChildIndex(Me.Label2, 0)
        Me.Controls.SetChildIndex(Me.PictureBox1, 0)
        Me.Controls.SetChildIndex(Me.SavedFilesPanel, 0)
        Me.Controls.SetChildIndex(Me.NewButton, 0)
        Me.Controls.SetChildIndex(Me.OpenButton, 0)
        Me.SavedFilesPanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    ' ------------------------
    ' Load up simulation list.
    ' ------------------------
    Private Sub StartupUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim FileNames() As String = Controller.GetFrequentList()
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
        If SimulationList.SelectedItems.Count > 0 Then
            Dim SelectedFile As String = SimulationList.SelectedItems(0).Text
            Controller.FileOpen(SelectedFile)
        End If
    End Sub


    ' ------------------------------------------
    ' User is wanting to create a new simulation
    ' ------------------------------------------
    Private Sub NewButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NewButton.Click
        Dim ApsimUI As ApsimUIController = Controller
        Dim NewData As APSIMData = ApsimUI.LetUserSelectNewDocument()
        If Not IsNothing(NewData) Then
            Controller.FileNew(NewData)
        End If
    End Sub

    ' ------------------------------------------
    ' User is wanting to open a simulation
    ' ------------------------------------------
    Private Sub OpenButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OpenButton.Click
        Controller.FileOpen()
    End Sub

End Class
