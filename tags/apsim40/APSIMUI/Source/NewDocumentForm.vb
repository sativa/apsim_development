Imports VBGeneral
Imports System
Imports System.Drawing
Imports System.Collections.Specialized
Public Class NewDocumentForm
    Inherits System.Windows.Forms.Form
    Protected SelectedData As APSIMData
    Protected MyFile As New APSIMFile
    Private UImanager As New UIManager
#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        FillListView()

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
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents OKButton As System.Windows.Forms.Button
    Friend WithEvents CancelButton1 As System.Windows.Forms.Button
    Friend WithEvents ListView As System.Windows.Forms.ListView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(NewDocumentForm))
        Me.Label1 = New System.Windows.Forms.Label
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.ListView = New System.Windows.Forms.ListView
        Me.OKButton = New System.Windows.Forms.Button
        Me.CancelButton1 = New System.Windows.Forms.Button
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(128, 8)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(248, 23)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Choose a simulation type from the list below"
        '
        'PictureBox1
        '
        Me.PictureBox1.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(0, 0)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(120, 400)
        Me.PictureBox1.TabIndex = 1
        Me.PictureBox1.TabStop = False
        '
        'ListView
        '
        Me.ListView.Location = New System.Drawing.Point(128, 40)
        Me.ListView.MultiSelect = False
        Me.ListView.Name = "ListView"
        Me.ListView.Size = New System.Drawing.Size(464, 320)
        Me.ListView.TabIndex = 2
        '
        'OKButton
        '
        Me.OKButton.Location = New System.Drawing.Point(432, 368)
        Me.OKButton.Name = "OKButton"
        Me.OKButton.TabIndex = 3
        Me.OKButton.Text = "OK"
        '
        'CancelButton1
        '
        Me.CancelButton1.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.CancelButton1.Location = New System.Drawing.Point(520, 368)
        Me.CancelButton1.Name = "CancelButton1"
        Me.CancelButton1.TabIndex = 4
        Me.CancelButton1.Text = "Cancel"
        '
        'NewDocumentForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.CancelButton = Me.CancelButton1
        Me.ClientSize = New System.Drawing.Size(602, 400)
        Me.Controls.Add(Me.CancelButton1)
        Me.Controls.Add(Me.OKButton)
        Me.Controls.Add(Me.ListView)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "NewDocumentForm"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "APSIM"
        Me.ResumeLayout(False)

    End Sub

#End Region


    ReadOnly Property Selection() As APSIMData
        Get
            Return SelectedData
        End Get

    End Property

    Private Sub CancelButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CancelButton1.Click
        SelectedData = Nothing
        Me.Hide()
    End Sub

    Private Sub OKButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OKButton.Click
        SelectedData = MyFile.data.Child(ListView.SelectedItems(0).Text)
        Me.Hide()
    End Sub
    Private Sub FillListView()
        Dim inifile As New APSIMSettings
        Dim TemplateFile As String = inifile.GetSetting("apsimui", "new_docs")
        MyFile.Open(TemplateFile)
        ListView.Clear()
        ListView.LargeImageList = UIManager.LargeImageList

        ' Add an item for all children of this system.
        Dim ChildList As StringCollection = UImanager.GetUserVisibleComponents(MyFile.data)
        Dim ChildName As String
        For Each ChildName In ChildList
            'create new item
            Dim item As New ListViewItem(ChildName, 0)
            item.ImageIndex = UImanager.LargeImageIndex(MyFile.data.Child(ChildName).Type)
            ListView.Items.Add(item)

        Next
    End Sub

    Private Sub ListView_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ListView.SelectedIndexChanged

    End Sub

    Private Sub ListView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles ListView.DoubleClick
        If ListView.SelectedItems.Count <> 0 Then
            OKButton.PerformClick()
        End If
    End Sub
End Class
