Imports System
Imports System.IO
Public Class SummaryFileUI
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
    Friend WithEvents BrowseButton As System.Windows.Forms.Button
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents RichTextBox As System.Windows.Forms.RichTextBox
    Friend WithEvents ImageList2 As System.Windows.Forms.ImageList
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents SummaryFileTextBox As System.Windows.Forms.TextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(SummaryFileUI))
        Me.SummaryFileTextBox = New System.Windows.Forms.TextBox
        Me.BrowseButton = New System.Windows.Forms.Button
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.Label1 = New System.Windows.Forms.Label
        Me.RichTextBox = New System.Windows.Forms.RichTextBox
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.ImageList2 = New System.Windows.Forms.ImageList(Me.components)
        Me.SuspendLayout()
        '
        'SummaryFileTextBox
        '
        Me.SummaryFileTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.SummaryFileTextBox.AutoSize = False
        Me.SummaryFileTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.SummaryFileTextBox.ForeColor = System.Drawing.SystemColors.WindowText
        Me.SummaryFileTextBox.Location = New System.Drawing.Point(96, 8)
        Me.SummaryFileTextBox.Name = "SummaryFileTextBox"
        Me.SummaryFileTextBox.Size = New System.Drawing.Size(664, 24)
        Me.SummaryFileTextBox.TabIndex = 0
        Me.SummaryFileTextBox.Text = ""
        '
        'BrowseButton
        '
        Me.BrowseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.BrowseButton.ImageIndex = 0
        Me.BrowseButton.ImageList = Me.ImageList
        Me.BrowseButton.Location = New System.Drawing.Point(772, 8)
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
        Me.Label1.Size = New System.Drawing.Size(80, 16)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Summary File:"
        '
        'RichTextBox
        '
        Me.RichTextBox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.RichTextBox.Font = New System.Drawing.Font("Courier New", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.RichTextBox.Location = New System.Drawing.Point(0, 40)
        Me.RichTextBox.Name = "RichTextBox"
        Me.RichTextBox.ReadOnly = True
        Me.RichTextBox.Size = New System.Drawing.Size(860, 543)
        Me.RichTextBox.TabIndex = 3
        Me.RichTextBox.Text = ""
        '
        'ImageList2
        '
        Me.ImageList2.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList2.TransparentColor = System.Drawing.Color.Transparent
        '
        'SummaryFileUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(860, 584)
        Me.Controls.Add(Me.RichTextBox)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.BrowseButton)
        Me.Controls.Add(Me.SummaryFileTextBox)
        Me.Name = "SummaryFileUI"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        Try
            MyBase.Refresh()
            Dim filename As String = GetValue("filename")
            SummaryFileTextBox.Text = filename
            OpenFileDialog.InitialDirectory = Path.GetDirectoryName(filename)


            If File.Exists(filename) Then
                Dim text As String
                Dim sr As StreamReader = New StreamReader(filename)
                text = sr.ReadToEnd
                RichTextBox.Text = text
            Else
                ' It may be that the summary file is not yet created so let's not put out the error just yet
                'MsgBox("The specified summary data file does not exist at the given location.  Please check file specification.", MsgBoxStyle.Critical, "File does not exist.")

            End If
        Catch E As Exception
            MsgBox(E.Message, MsgBoxStyle.Critical, "Error in refreshing Summary File UI")
        End Try

    End Sub
    Private Sub BrowseButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BrowseButton.Click
        Try
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                SummaryFileTextBox.Text = OpenFileDialog.FileName
                APSIMData.Child("filename").Value = SummaryFileTextBox.Text
                Me.Refresh()
            Else
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in browsing to new summary file")
        End Try
    End Sub

    Private Sub SummaryFileTextBox_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SummaryFileTextBox.Leave
        Try
            If SummaryFileTextBox.Visible = True Then
                APSIMData.Child("filename").Value = SummaryFileTextBox.Text
                Me.Refresh()
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating summary file name information")
        End Try
    End Sub



    Private Sub SummaryFileUI_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class
