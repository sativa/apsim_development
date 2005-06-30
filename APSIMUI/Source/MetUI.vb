Imports System
Imports System.IO

Public Class MetUI
    Inherits VBGeneral.BaseUI

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()
        Xceed.Chart.Licenser.LicenseKey = "CHT30-YTL57-0UXLJ-145A"
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
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ImageList2 As System.Windows.Forms.ImageList
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents FileContentsTab As System.Windows.Forms.TabPage
    Friend WithEvents GraphTab As System.Windows.Forms.TabPage
    Friend WithEvents RichTextBox As System.Windows.Forms.RichTextBox
    Friend WithEvents MetGraphControl1 As APSIMUI.MetGraphControl
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents FileMissingLabel As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents BrowseButton As System.Windows.Forms.Button
    Friend WithEvents MetFileTextBox As System.Windows.Forms.TextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MetUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.ImageList2 = New System.Windows.Forms.ImageList(Me.components)
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.FileContentsTab = New System.Windows.Forms.TabPage
        Me.RichTextBox = New System.Windows.Forms.RichTextBox
        Me.GraphTab = New System.Windows.Forms.TabPage
        Me.MetGraphControl1 = New APSIMUI.MetGraphControl
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.FileMissingLabel = New System.Windows.Forms.Label
        Me.Label1 = New System.Windows.Forms.Label
        Me.BrowseButton = New System.Windows.Forms.Button
        Me.MetFileTextBox = New System.Windows.Forms.TextBox
        Me.TabControl.SuspendLayout()
        Me.FileContentsTab.SuspendLayout()
        Me.GraphTab.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'ImageList2
        '
        Me.ImageList2.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList2.TransparentColor = System.Drawing.Color.Transparent
        '
        'TabControl
        '
        Me.TabControl.Controls.Add(Me.FileContentsTab)
        Me.TabControl.Controls.Add(Me.GraphTab)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl.Location = New System.Drawing.Point(0, 83)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(984, 365)
        Me.TabControl.TabIndex = 4
        '
        'FileContentsTab
        '
        Me.FileContentsTab.Controls.Add(Me.RichTextBox)
        Me.FileContentsTab.Location = New System.Drawing.Point(4, 22)
        Me.FileContentsTab.Name = "FileContentsTab"
        Me.FileContentsTab.Size = New System.Drawing.Size(976, 339)
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
        Me.RichTextBox.Size = New System.Drawing.Size(976, 339)
        Me.RichTextBox.TabIndex = 4
        Me.RichTextBox.Text = ""
        Me.RichTextBox.WordWrap = False
        '
        'GraphTab
        '
        Me.GraphTab.Controls.Add(Me.MetGraphControl1)
        Me.GraphTab.Location = New System.Drawing.Point(4, 22)
        Me.GraphTab.Name = "GraphTab"
        Me.GraphTab.Size = New System.Drawing.Size(976, 339)
        Me.GraphTab.TabIndex = 1
        Me.GraphTab.Text = "Graph"
        '
        'MetGraphControl1
        '
        Me.MetGraphControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.MetGraphControl1.Location = New System.Drawing.Point(0, 0)
        Me.MetGraphControl1.Name = "MetGraphControl1"
        Me.MetGraphControl1.Size = New System.Drawing.Size(976, 339)
        Me.MetGraphControl1.TabIndex = 0
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.FileMissingLabel)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.BrowseButton)
        Me.Panel1.Controls.Add(Me.MetFileTextBox)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Top
        Me.Panel1.Location = New System.Drawing.Point(0, 23)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(984, 60)
        Me.Panel1.TabIndex = 6
        '
        'FileMissingLabel
        '
        Me.FileMissingLabel.AutoSize = True
        Me.FileMissingLabel.ForeColor = System.Drawing.Color.Red
        Me.FileMissingLabel.Location = New System.Drawing.Point(82, 36)
        Me.FileMissingLabel.Name = "FileMissingLabel"
        Me.FileMissingLabel.Size = New System.Drawing.Size(530, 16)
        Me.FileMissingLabel.TabIndex = 9
        Me.FileMissingLabel.Text = "The specified meteorological data file does not exist at the given location.  Ple" & _
        "ase check file specification."
        Me.FileMissingLabel.Visible = False
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(8, 9)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(72, 15)
        Me.Label1.TabIndex = 8
        Me.Label1.Text = "Weather File:"
        '
        'BrowseButton
        '
        Me.BrowseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.BrowseButton.ImageIndex = 0
        Me.BrowseButton.ImageList = Me.ImageList
        Me.BrowseButton.Location = New System.Drawing.Point(896, 9)
        Me.BrowseButton.Name = "BrowseButton"
        Me.BrowseButton.Size = New System.Drawing.Size(80, 24)
        Me.BrowseButton.TabIndex = 7
        Me.BrowseButton.Text = "Browse"
        Me.BrowseButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MetFileTextBox
        '
        Me.MetFileTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.MetFileTextBox.AutoSize = False
        Me.MetFileTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.MetFileTextBox.ForeColor = System.Drawing.SystemColors.WindowText
        Me.MetFileTextBox.Location = New System.Drawing.Point(82, 9)
        Me.MetFileTextBox.Name = "MetFileTextBox"
        Me.MetFileTextBox.Size = New System.Drawing.Size(808, 24)
        Me.MetFileTextBox.TabIndex = 6
        Me.MetFileTextBox.Text = ""
        '
        'MetUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(984, 488)
        Me.Controls.Add(Me.TabControl)
        Me.Controls.Add(Me.Panel1)
        Me.Name = "MetUI"
        Me.Controls.SetChildIndex(Me.Panel1, 0)
        Me.Controls.SetChildIndex(Me.TabControl, 0)
        Me.TabControl.ResumeLayout(False)
        Me.FileContentsTab.ResumeLayout(False)
        Me.GraphTab.ResumeLayout(False)
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        Try
            MyBase.Refresh()
            MetGraphControl1.Data = Data
            MetGraphControl1.Refresh()
            Dim filename As String = Data.ChildValue("filename", True)
            MetFileTextBox.Text = filename
            OpenFileDialog.InitialDirectory = Path.GetDirectoryName(filename)


            If File.Exists(filename) Then
                Dim text As String
                Dim sr As StreamReader = New StreamReader(filename)
                text = sr.ReadToEnd
                RichTextBox.Text = text
                FileMissingLabel.Visible = False
            Else
                FileMissingLabel.Visible = True

            End If

        Catch E as system.exception
            MsgBox(E.Message, MsgBoxStyle.Critical, "Error in refreshing Met UI")
        End Try

    End Sub

    Private Sub BrowseButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BrowseButton.Click
        Try
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                MetFileTextBox.Text = OpenFileDialog.FileName
                GetNewMetFile()
            Else
            End If
        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in browsing to new met file")
        End Try
    End Sub

    Private Sub MetFileTextBox_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MetFileTextBox.Leave
        GetNewMetFile()
    End Sub

    Private Sub GetNewMetFile()
        Try
            Data.Child("filename").Value = MetFileTextBox.Text
            Me.Refresh()
        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating met file name information")
        End Try
    End Sub

    Private Sub MetFileTextBox_KeyUp(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MetFileTextBox.KeyUp
        If e.KeyCode = Keys.Enter Then
            GetNewMetFile()
        End If
    End Sub

    Private Sub TabControl_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TabControl.SelectedIndexChanged
        If TabControl.SelectedTab.Text = "Graph" Then
            MetGraphControl1.PopulateGraph()
        End If
    End Sub
End Class
