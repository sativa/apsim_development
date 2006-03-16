Imports System
Imports System.IO
Imports VBGeneral
Public Class FileUI
    Inherits BaseView
    Dim FileDateTime As DateTime
    Dim FullFileName As String

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
    Friend WithEvents FileContentsBox As System.Windows.Forms.RichTextBox
    Friend WithEvents BrowseButton As System.Windows.Forms.ToolBarButton
    Friend WithEvents BrowseToolBar As System.Windows.Forms.ToolBar
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(FileUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.FileContentsBox = New System.Windows.Forms.RichTextBox
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.BrowseToolBar = New System.Windows.Forms.ToolBar
        Me.BrowseButton = New System.Windows.Forms.ToolBarButton
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
        Me.FileContentsBox.Location = New System.Drawing.Point(0, 40)
        Me.FileContentsBox.Name = "FileContentsBox"
        Me.FileContentsBox.ReadOnly = True
        Me.FileContentsBox.Size = New System.Drawing.Size(794, 428)
        Me.FileContentsBox.TabIndex = 3
        Me.FileContentsBox.Text = ""
        Me.FileContentsBox.WordWrap = False
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.Filter = "All files|*.*"
        '
        'BrowseToolBar
        '
        Me.BrowseToolBar.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.BrowseToolBar.AutoSize = False
        Me.BrowseToolBar.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.BrowseToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.BrowseButton})
        Me.BrowseToolBar.ButtonSize = New System.Drawing.Size(65, 26)
        Me.BrowseToolBar.Divider = False
        Me.BrowseToolBar.Dock = System.Windows.Forms.DockStyle.None
        Me.BrowseToolBar.DropDownArrows = True
        Me.BrowseToolBar.ImageList = Me.ImageList
        Me.BrowseToolBar.Location = New System.Drawing.Point(708, 3)
        Me.BrowseToolBar.Name = "BrowseToolBar"
        Me.BrowseToolBar.ShowToolTips = True
        Me.BrowseToolBar.Size = New System.Drawing.Size(80, 32)
        Me.BrowseToolBar.TabIndex = 11
        Me.BrowseToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        Me.BrowseToolBar.Wrappable = False
        '
        'BrowseButton
        '
        Me.BrowseButton.ImageIndex = 0
        Me.BrowseButton.Text = "Browse"
        '
        'FileUI
        '
        Me.Controls.Add(Me.BrowseToolBar)
        Me.Controls.Add(Me.FileContentsBox)
        Me.Name = "FileUI"
        Me.Size = New System.Drawing.Size(794, 468)
        Me.Controls.SetChildIndex(Me.FileContentsBox, 0)
        Me.Controls.SetChildIndex(Me.BrowseToolBar, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' ------------------------------
    ' Refresh this window.
    ' ------------------------------
    Overrides Sub Refresh()
        MyBase.Refresh()

        ' setup all controls.
        If Not IsNothing(Controller) Then
            Dim FileName As String = Controller.Data.ChildValue("filename")
            If Controller.FileName <> "" Then
                FullFileName = Path.Combine(Path.GetDirectoryName(Controller.FileName), FileName)
            Else
                FullFileName = FileName
            End If

            HelpText = FullFileName
            If File.Exists(FullFileName) Then
                Dim text As String
                Dim sr As StreamReader

                Try
                    sr = New StreamReader(FullFileName)
                    text = sr.ReadToEnd
                    sr.Close()
                    sr = Nothing
                    FileContentsBox.Text = text
                    FileDateTime = File.GetLastWriteTime(FullFileName)
                Catch e As System.Exception
                End Try

            Else
                FileContentsBox.Text = "<File doesn't exist>"
            End If
            If Not IsNothing(Parent) Then
                Dim F As Form = Parent.Parent.Parent
                AddHandler F.Activated, AddressOf OnActivate
            End If

            BrowseToolBar.Visible = (Controller.Data.Type.ToLower() <> "outputfile" And Controller.Data.Type.ToLower() <> "summaryfile")
        End If
    End Sub


    ' ----------------------------------------------
    ' User has clicked on a toolbar button
    ' ----------------------------------------------
    Private Sub ToolBar_ButtonClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles BrowseToolBar.ButtonClick
        If e.Button Is BrowseButton Then
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                HelpText = OpenFileDialog.FileName
                Controller.Data.Child("filename").Value = OpenFileDialog.FileName
                Me.Refresh()
            End If
        End If
    End Sub


    Private Sub OnActivate(ByVal sender As Object, ByVal e As EventArgs)
        If File.Exists(FullFileName) AndAlso FileDateTime <> File.GetLastWriteTime(FullFileName) Then
            Refresh()
        End If
    End Sub
End Class
