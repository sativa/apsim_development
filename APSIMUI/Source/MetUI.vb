Imports System
Imports System.IO
Imports scpl
Public Class MetUI
    Inherits BaseUI

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
    Friend WithEvents MetFileTextBox As System.Windows.Forms.TextBox
    Friend WithEvents BrowseButton As System.Windows.Forms.Button
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ImageList2 As System.Windows.Forms.ImageList
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents FileContentsTab As System.Windows.Forms.TabPage
    Friend WithEvents GraphTab As System.Windows.Forms.TabPage
    Friend WithEvents RichTextBox As System.Windows.Forms.RichTextBox
    Friend WithEvents MetGraphControl1 As APSIMUI.MetGraphControl
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MetUI))
        Me.MetFileTextBox = New System.Windows.Forms.TextBox
        Me.BrowseButton = New System.Windows.Forms.Button
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.Label1 = New System.Windows.Forms.Label
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.ImageList2 = New System.Windows.Forms.ImageList(Me.components)
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.FileContentsTab = New System.Windows.Forms.TabPage
        Me.RichTextBox = New System.Windows.Forms.RichTextBox
        Me.GraphTab = New System.Windows.Forms.TabPage
        Me.MetGraphControl1 = New APSIMUI.MetGraphControl
        Me.TabControl.SuspendLayout()
        Me.FileContentsTab.SuspendLayout()
        Me.GraphTab.SuspendLayout()
        Me.SuspendLayout()
        '
        'MetFileTextBox
        '
        Me.MetFileTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.MetFileTextBox.AutoSize = False
        Me.MetFileTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.MetFileTextBox.ForeColor = System.Drawing.SystemColors.WindowText
        Me.MetFileTextBox.Location = New System.Drawing.Point(80, 8)
        Me.MetFileTextBox.Name = "MetFileTextBox"
        Me.MetFileTextBox.Size = New System.Drawing.Size(896, 24)
        Me.MetFileTextBox.TabIndex = 0
        Me.MetFileTextBox.Text = ""
        '
        'BrowseButton
        '
        Me.BrowseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.BrowseButton.ImageIndex = 0
        Me.BrowseButton.ImageList = Me.ImageList
        Me.BrowseButton.Location = New System.Drawing.Point(984, 8)
        Me.BrowseButton.Name = "BrowseButton"
        Me.BrowseButton.Size = New System.Drawing.Size(80, 24)
        Me.BrowseButton.TabIndex = 1
        Me.BrowseButton.Text = "Browse"
        Me.BrowseButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(8, 11)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(72, 16)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Weather File:"
        '
        'ImageList2
        '
        Me.ImageList2.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList2.TransparentColor = System.Drawing.Color.Transparent
        '
        'TabControl
        '
        Me.TabControl.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TabControl.Controls.Add(Me.FileContentsTab)
        Me.TabControl.Controls.Add(Me.GraphTab)
        Me.TabControl.Location = New System.Drawing.Point(0, 40)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(1064, 448)
        Me.TabControl.TabIndex = 4
        '
        'FileContentsTab
        '
        Me.FileContentsTab.Controls.Add(Me.RichTextBox)
        Me.FileContentsTab.Location = New System.Drawing.Point(4, 22)
        Me.FileContentsTab.Name = "FileContentsTab"
        Me.FileContentsTab.Size = New System.Drawing.Size(1056, 422)
        Me.FileContentsTab.TabIndex = 0
        Me.FileContentsTab.Text = "FileContents"
        '
        'RichTextBox
        '
        Me.RichTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RichTextBox.Font = New System.Drawing.Font("Courier New", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.RichTextBox.Location = New System.Drawing.Point(0, 0)
        Me.RichTextBox.Name = "RichTextBox"
        Me.RichTextBox.ReadOnly = True
        Me.RichTextBox.Size = New System.Drawing.Size(1056, 422)
        Me.RichTextBox.TabIndex = 4
        Me.RichTextBox.Text = ""
        '
        'GraphTab
        '
        Me.GraphTab.Controls.Add(Me.MetGraphControl1)
        Me.GraphTab.Location = New System.Drawing.Point(4, 22)
        Me.GraphTab.Name = "GraphTab"
        Me.GraphTab.Size = New System.Drawing.Size(1056, 422)
        Me.GraphTab.TabIndex = 1
        Me.GraphTab.Text = "Graph"
        '
        'MetGraphControl1
        '
        Me.MetGraphControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.MetGraphControl1.Location = New System.Drawing.Point(0, 0)
        Me.MetGraphControl1.Name = "MetGraphControl1"
        Me.MetGraphControl1.Size = New System.Drawing.Size(1056, 422)
        Me.MetGraphControl1.TabIndex = 0
        '
        'MetUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1072, 488)
        Me.Controls.Add(Me.TabControl)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.BrowseButton)
        Me.Controls.Add(Me.MetFileTextBox)
        Me.Name = "MetUI"
        Me.TabControl.ResumeLayout(False)
        Me.FileContentsTab.ResumeLayout(False)
        Me.GraphTab.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        Try
            MyBase.Refresh()
            Dim filename As String = GetValue("filename")
            MetFileTextBox.Text = filename
            OpenFileDialog.InitialDirectory = Path.GetDirectoryName(filename)


            If File.Exists(filename) Then
                Dim text As String
                Dim sr As StreamReader = New StreamReader(filename)
                text = sr.ReadToEnd
                RichTextBox.Text = text
            Else
                MsgBox("The specified meteorological data file does not exist at the given location.  Please check file specification.", MsgBoxStyle.Critical, "File does not exist.")

            End If



        Catch E As Exception
            MsgBox(E.Message, MsgBoxStyle.Critical, "Error in refreshing Met UI")
        End Try

    End Sub

    Private Sub BrowseButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BrowseButton.Click
        Try
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                MetFileTextBox.Text = OpenFileDialog.FileName
                MyData.Child("filename").Value = MetFileTextBox.Text
                Me.Refresh()
            Else
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in browsing to new met file")
        End Try
    End Sub

    Private Sub MetFileTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles MetFileTextBox.Leave
        Try
            If MetFileTextBox.Visible = True Then
                MyData.Child("filename").Value = MetFileTextBox.Text
                Me.Refresh()
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating met file name information")
        End Try
    End Sub



    Private Sub MetUI_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class
