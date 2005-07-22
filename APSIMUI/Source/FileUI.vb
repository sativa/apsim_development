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
    Friend WithEvents RichTextBox As System.Windows.Forms.RichTextBox
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents SummaryFileTextBox As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ContextMenu1 As System.Windows.Forms.ContextMenu
    Friend WithEvents BrowseMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents ApsvisMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents ExcelMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents OutlookMenuItem As System.Windows.Forms.MenuItem
    Friend WithEvents ToolBar1 As System.Windows.Forms.ToolBar
    Friend WithEvents GoButton As System.Windows.Forms.ToolBarButton
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(FileUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.RichTextBox = New System.Windows.Forms.RichTextBox
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.ToolBar1 = New System.Windows.Forms.ToolBar
        Me.GoButton = New System.Windows.Forms.ToolBarButton
        Me.ContextMenu1 = New System.Windows.Forms.ContextMenu
        Me.BrowseMenuItem = New System.Windows.Forms.MenuItem
        Me.ApsvisMenuItem = New System.Windows.Forms.MenuItem
        Me.ExcelMenuItem = New System.Windows.Forms.MenuItem
        Me.OutlookMenuItem = New System.Windows.Forms.MenuItem
        Me.SummaryFileTextBox = New System.Windows.Forms.TextBox
        Me.Label1 = New System.Windows.Forms.Label
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(20, 20)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'RichTextBox
        '
        Me.RichTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RichTextBox.Font = New System.Drawing.Font("Courier New", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.RichTextBox.Location = New System.Drawing.Point(0, 52)
        Me.RichTextBox.Name = "RichTextBox"
        Me.RichTextBox.ReadOnly = True
        Me.RichTextBox.Size = New System.Drawing.Size(984, 396)
        Me.RichTextBox.TabIndex = 3
        Me.RichTextBox.Text = ""
        Me.RichTextBox.WordWrap = False
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.Filter = "Output files|*.out|Summary file|*.sum|All files|*.*"
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.ToolBar1)
        Me.Panel1.Controls.Add(Me.SummaryFileTextBox)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Top
        Me.Panel1.Location = New System.Drawing.Point(0, 23)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(984, 29)
        Me.Panel1.TabIndex = 4
        '
        'ToolBar1
        '
        Me.ToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBar1.AutoSize = False
        Me.ToolBar1.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.GoButton})
        Me.ToolBar1.ButtonSize = New System.Drawing.Size(65, 26)
        Me.ToolBar1.Divider = False
        Me.ToolBar1.Dock = System.Windows.Forms.DockStyle.Right
        Me.ToolBar1.DropDownArrows = True
        Me.ToolBar1.ImageList = Me.ImageList
        Me.ToolBar1.Location = New System.Drawing.Point(904, 0)
        Me.ToolBar1.Name = "ToolBar1"
        Me.ToolBar1.ShowToolTips = True
        Me.ToolBar1.Size = New System.Drawing.Size(80, 29)
        Me.ToolBar1.TabIndex = 10
        Me.ToolBar1.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        '
        'GoButton
        '
        Me.GoButton.DropDownMenu = Me.ContextMenu1
        Me.GoButton.ImageIndex = 0
        Me.GoButton.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton
        Me.GoButton.Text = "Go"
        '
        'ContextMenu1
        '
        Me.ContextMenu1.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.BrowseMenuItem, Me.ApsvisMenuItem, Me.ExcelMenuItem, Me.OutlookMenuItem})
        '
        'BrowseMenuItem
        '
        Me.BrowseMenuItem.Index = 0
        Me.BrowseMenuItem.Text = "&Browse for a file"
        '
        'ApsvisMenuItem
        '
        Me.ApsvisMenuItem.Index = 1
        Me.ApsvisMenuItem.Text = "Send output to APS&Vis"
        '
        'ExcelMenuItem
        '
        Me.ExcelMenuItem.Index = 2
        Me.ExcelMenuItem.Text = "Send output to &Excel"
        '
        'OutlookMenuItem
        '
        Me.OutlookMenuItem.Index = 3
        Me.OutlookMenuItem.Text = "Send output to Apsim &Outlook"
        '
        'SummaryFileTextBox
        '
        Me.SummaryFileTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.SummaryFileTextBox.AutoSize = False
        Me.SummaryFileTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.SummaryFileTextBox.ForeColor = System.Drawing.SystemColors.WindowText
        Me.SummaryFileTextBox.Location = New System.Drawing.Point(62, 3)
        Me.SummaryFileTextBox.Name = "SummaryFileTextBox"
        Me.SummaryFileTextBox.Size = New System.Drawing.Size(841, 25)
        Me.SummaryFileTextBox.TabIndex = 9
        Me.SummaryFileTextBox.Text = ""
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(7, 7)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(54, 16)
        Me.Label1.TabIndex = 8
        Me.Label1.Text = "File name"
        '
        'FileUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(984, 488)
        Me.Controls.Add(Me.RichTextBox)
        Me.Controls.Add(Me.Panel1)
        Me.Name = "FileUI"
        Me.Controls.SetChildIndex(Me.Panel1, 0)
        Me.Controls.SetChildIndex(Me.RichTextBox, 0)
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub Refresh()
        Try
            MyBase.Refresh()
            Dim filename As String = Data.ChildValueWithError("filename")
            SummaryFileTextBox.Text = filename
            OpenFileDialog.InitialDirectory = Path.GetDirectoryName(filename)
            HelpLabel.Text = "Enter the name of the file in the edit box at the top. The contents of the file will be displayed in the large window above."
            If Path.GetExtension(SummaryFileTextBox.Text) = ".out" Then
                HelpLabel.Text = HelpLabel.Text + " Click the go button at the top right to send the output file to APSVis, Excel and Apsim Outlook."
            End If

            Dim FullFileName As String = ""
            If Explorer.FileName <> "" Then
                FullFileName = Path.Combine(Path.GetDirectoryName(Explorer.FileName), filename)
            Else
                FullFileName = filename
            End If
            If File.Exists(FullFileName) Then
                Dim text As String
                Dim sr As StreamReader = New StreamReader(FullFileName)

                text = sr.ReadToEnd
                RichTextBox.Text = text
            Else
                RichTextBox.Text = ""
                ' It may be that the summary file is not yet created so let's not put out the error just yet
                'MsgBox("The specified summary data file does not exist at the given location.  Please check file specification.", MsgBoxStyle.Critical, "File does not exist.")
            End If

            ' Hide some of the 'go' menu items if it isn't an output file.
            ApsvisMenuItem.Visible = (Path.GetExtension(SummaryFileTextBox.Text) = ".out")
            ExcelMenuItem.Visible = ApsvisMenuItem.Visible
            OutlookMenuItem.Visible = ApsvisMenuItem.Visible

        Catch E As System.Exception
            MsgBox(E.Message, MsgBoxStyle.Critical, "Error in refreshing Summary File UI")
        End Try

    End Sub

    Private Sub SummaryFileTextBox_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SummaryFileTextBox.Leave
        Try
            If SummaryFileTextBox.Visible = True Then
                Data.Child("filename").Value = SummaryFileTextBox.Text
                Me.Refresh()
            End If
        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating summary file name information")
        End Try
    End Sub

    Private Sub SummaryFileTextBox_KeyUp(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles SummaryFileTextBox.KeyUp
        If e.KeyCode = Keys.Enter Then
            Data.Child("filename").Value = SummaryFileTextBox.Text
            Me.Refresh()
        End If
    End Sub


    ' ----------------------------------------------
    ' User has clicked on the browse menu item
    ' ----------------------------------------------
    Private Sub BrowseMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles BrowseMenuItem.Click
        Try
            Select Case LCase(Path.GetExtension(SummaryFileTextBox.Text))
                Case ".out"
                    OpenFileDialog.FilterIndex = 1
                Case ".sum"
                    OpenFileDialog.FilterIndex = 2
                Case Else
                    OpenFileDialog.FilterIndex = 3
            End Select

            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                SummaryFileTextBox.Text = OpenFileDialog.FileName
                Data.Child("filename").Value = SummaryFileTextBox.Text
                Me.Refresh()
            Else
            End If

        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in browsing to new summary file")
        End Try
    End Sub

    ' ----------------------------------------------
    ' User has clicked on the APSVis menu item
    ' ----------------------------------------------
    Private Sub ApsvisMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ApsvisMenuItem.Click
        Dim CommandLine As String = APSIMSettings.ApsimDirectory() + "\bin\apsvis.exe"
        Process.Start(CommandLine, CreateTempFile())
    End Sub

    ' ----------------------------------------------
    ' User has clicked on the EXCEL menu item
    ' ----------------------------------------------
    Private Sub ExcelMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ExcelMenuItem.Click
        Dim CommandLine As String = "excel.exe"
        Process.Start(CommandLine, SummaryFileTextBox.Text)
    End Sub

    ' ------------------------------------------------------
    ' User has clicked on the APSIM Outlook menu item
    ' ------------------------------------------------------
    Private Sub OutlookMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles OutlookMenuItem.Click
        Dim CommandLine As String = APSIMSettings.ApsimDirectory() + "\bin\apsimoutlook.exe"
        Process.Start(CommandLine, CreateTempFile())
    End Sub


    ' ------------------------------------------------------
    ' Create a temporary file the we can pass to APSVis
    ' or ApsimOutlook
    ' ------------------------------------------------------
    Private Function CreateTempFile() As String
        Dim Filename As String = Path.GetTempFileName()
        Dim Writer As New StreamWriter(Filename)
        Writer.WriteLine(SummaryFileTextBox.Text)
        Writer.Close()
        Return Filename
    End Function


End Class
