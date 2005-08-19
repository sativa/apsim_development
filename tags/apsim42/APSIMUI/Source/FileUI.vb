Imports System
Imports System.IO
Imports VBGeneral
Public Class FileUI
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
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents FileNameLabel As System.Windows.Forms.Label
    Friend WithEvents APSVisButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ExcelButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents OutlookButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents FileContentsBox As System.Windows.Forms.RichTextBox
    Friend WithEvents BrowseButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents ToolBar As System.Windows.Forms.ToolBar
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(FileUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.FileContentsBox = New System.Windows.Forms.RichTextBox
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.ToolBar = New System.Windows.Forms.ToolBar
        Me.BrowseButton = New System.Windows.Forms.ToolBarButton
        Me.APSVisButton = New System.Windows.Forms.ToolBarButton
        Me.ExcelButton = New System.Windows.Forms.ToolBarButton
        Me.OutlookButton = New System.Windows.Forms.ToolBarButton
        Me.FileNameLabel = New System.Windows.Forms.Label
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(24, 24)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'FileContentsBox
        '
        Me.FileContentsBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FileContentsBox.Font = New System.Drawing.Font("Courier New", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.FileContentsBox.Location = New System.Drawing.Point(0, 80)
        Me.FileContentsBox.Name = "FileContentsBox"
        Me.FileContentsBox.ReadOnly = True
        Me.FileContentsBox.Size = New System.Drawing.Size(846, 570)
        Me.FileContentsBox.TabIndex = 3
        Me.FileContentsBox.Text = ""
        Me.FileContentsBox.WordWrap = False
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.Filter = "All files|*.*"
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.FileNameLabel)
        Me.Panel1.Controls.Add(Me.ToolBar)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Top
        Me.Panel1.Location = New System.Drawing.Point(0, 20)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(846, 60)
        Me.Panel1.TabIndex = 4
        '
        'ToolBar
        '
        Me.ToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.BrowseButton, Me.APSVisButton, Me.ExcelButton, Me.OutlookButton})
        Me.ToolBar.ButtonSize = New System.Drawing.Size(65, 26)
        Me.ToolBar.Divider = False
        Me.ToolBar.DropDownArrows = True
        Me.ToolBar.ImageList = Me.ImageList
        Me.ToolBar.Location = New System.Drawing.Point(0, 0)
        Me.ToolBar.Name = "ToolBar"
        Me.ToolBar.ShowToolTips = True
        Me.ToolBar.Size = New System.Drawing.Size(846, 34)
        Me.ToolBar.TabIndex = 10
        Me.ToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        Me.ToolBar.Wrappable = False
        '
        'BrowseButton
        '
        Me.BrowseButton.ImageIndex = 0
        Me.BrowseButton.Text = "Browse"
        '
        'APSVisButton
        '
        Me.APSVisButton.ImageIndex = 1
        Me.APSVisButton.Text = "APSVis"
        '
        'ExcelButton
        '
        Me.ExcelButton.ImageIndex = 2
        Me.ExcelButton.Text = "Excel"
        '
        'OutlookButton
        '
        Me.OutlookButton.ImageIndex = 3
        Me.OutlookButton.Text = "ApsimOutlook"
        '
        'FileNameLabel
        '
        Me.FileNameLabel.BackColor = System.Drawing.SystemColors.Info
        Me.FileNameLabel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.FileNameLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.FileNameLabel.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.FileNameLabel.Location = New System.Drawing.Point(0, 34)
        Me.FileNameLabel.Name = "FileNameLabel"
        Me.FileNameLabel.Size = New System.Drawing.Size(846, 29)
        Me.FileNameLabel.TabIndex = 8
        Me.FileNameLabel.Text = "xxx"
        Me.FileNameLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'FileUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(846, 685)
        Me.Controls.Add(Me.FileContentsBox)
        Me.Controls.Add(Me.Panel1)
        Me.Name = "FileUI"
        Me.Controls.SetChildIndex(Me.Panel1, 0)
        Me.Controls.SetChildIndex(Me.FileContentsBox, 0)
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' ------------------------------
    ' Refresh this window.
    ' ------------------------------
    Overrides Sub Refresh()
        MyBase.Refresh()

        ' setup all controls.
        If Not IsNothing(Data) Then
            Dim FileName As String = Data.ChildValue("filename")
            Dim FullFileName As String
            If Explorer.FileName <> "" Then
                FullFileName = Path.Combine(Path.GetDirectoryName(Explorer.FileName), filename)
            Else
                FullFileName = filename
            End If

            FileNameLabel.Text = FullFileName
            HelpLabel.Text = "The filename of the summary and output file is autogenerated (fixed). The contents of the file will be displayed in the large window above."
            If File.Exists(FullFileName) Then
                Dim text As String
                Dim sr As StreamReader = New StreamReader(FullFileName)
                text = sr.ReadToEnd
                sr.Close()
                sr = Nothing
                FileContentsBox.Text = text
            Else
                FileContentsBox.Text = ""
            End If
            BrowseButton.Visible = (Data.Type.ToLower() <> "outputfile" And Data.Type.ToLower() <> "summaryfile")
            APSVisButton.Visible = (Data.Type.ToLower() = "outputfile")
            ExcelButton.Visible = (Data.Type.ToLower() = "outputfile")
            OutlookButton.Visible = (Data.Type.ToLower() = "outputfile")
        End If
    End Sub


    ' ----------------------------------------------
    ' User has clicked on a toolbar button
    ' ----------------------------------------------
    Private Sub ToolBar_ButtonClick(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles ToolBar.ButtonClick
        If e.Button Is BrowseButton Then
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                FileNameLabel.Text = OpenFileDialog.FileName
                Data.Child("filename").Value = FileNameLabel.Text
                Me.Refresh()
            End If
        ElseIf e.Button Is APSVisButton Then
            Dim CommandLine As String = APSIMSettings.ApsimDirectory() + "\bin\apsvis.exe"
            Process.Start(CommandLine, CreateTempFile())
        ElseIf e.Button Is ExcelButton Then
            Dim CommandLine As String = "excel.exe"
            Process.Start(CommandLine, FileNameLabel.Text)
        ElseIf e.Button Is OutlookButton Then
            Dim CommandLine As String = APSIMSettings.ApsimDirectory() + "\bin\apsimoutlook.exe"
            Process.Start(CommandLine, CreateTempFile())
        End If
    End Sub


    ' ------------------------------------------------------
    ' Create a temporary file the we can pass to APSVis
    ' or ApsimOutlook
    ' ------------------------------------------------------
    Private Function CreateTempFile() As String
        Dim Filename As String = Path.GetTempFileName()
        Dim Writer As New StreamWriter(Filename)
        Writer.WriteLine(FileNameLabel.Text)
        Writer.Close()
        Return Filename
    End Function



End Class
