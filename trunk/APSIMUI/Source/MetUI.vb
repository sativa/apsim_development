Imports System
Imports System.IO
Imports VBGeneral
Public Class MetUI
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
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents MetGraphControl1 As APSIMUI.MetGraphControl
    Friend WithEvents btnBrowse As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MetUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.btnBrowse = New System.Windows.Forms.Button
        Me.MetGraphControl1 = New APSIMUI.MetGraphControl
        Me.SuspendLayout()
        '
        'ImageList
        '
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        Me.ImageList.Images.SetKeyName(0, "")
        '
        'btnBrowse
        '
        Me.btnBrowse.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnBrowse.BackColor = System.Drawing.SystemColors.Info
        Me.btnBrowse.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.btnBrowse.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.btnBrowse.ImageIndex = 0
        Me.btnBrowse.ImageList = Me.ImageList
        Me.btnBrowse.Location = New System.Drawing.Point(909, 5)
        Me.btnBrowse.Name = "btnBrowse"
        Me.btnBrowse.Size = New System.Drawing.Size(88, 29)
        Me.btnBrowse.TabIndex = 13
        Me.btnBrowse.Text = "Browse ..."
        Me.btnBrowse.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.btnBrowse.UseVisualStyleBackColor = False
        '
        'MetGraphControl1
        '
        Me.MetGraphControl1.AutoScroll = True
        Me.MetGraphControl1.BackColor = System.Drawing.SystemColors.Control
        Me.MetGraphControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.MetGraphControl1.HelpText = ""
        Me.MetGraphControl1.Location = New System.Drawing.Point(0, 40)
        Me.MetGraphControl1.Name = "MetGraphControl1"
        Me.MetGraphControl1.Size = New System.Drawing.Size(1015, 679)
        Me.MetGraphControl1.TabIndex = 14
        '
        'MetUI
        '
        Me.Controls.Add(Me.btnBrowse)
        Me.Controls.Add(Me.MetGraphControl1)
        Me.Name = "MetUI"
        Me.Size = New System.Drawing.Size(1015, 719)
        Me.Controls.SetChildIndex(Me.MetGraphControl1, 0)
        Me.Controls.SetChildIndex(Me.btnBrowse, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overrides Sub RefreshView(ByVal Controller As BaseController)
        MyBase.RefreshView(Controller)

        Dim FileName As String = Controller.Data.ChildValue("filename")
        MetGraphControl1.RefreshView(Controller)
        HelpText = FileName
        OpenFileDialog.InitialDirectory = Path.GetDirectoryName(FileName)
        MetGraphControl1.SetFileName(FileName)
    End Sub

    Private Sub btnBrowse_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles btnBrowse.Paint
        ControlPaint.DrawBorder3D(e.Graphics, e.ClipRectangle, Border3DStyle.Etched)
    End Sub

    Private Sub btnBrowse_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnBrowse.Click

        If OpenFileDialog.ShowDialog() = DialogResult.OK Then
            MetGraphControl1.SetFileName(OpenFileDialog.FileName)
            Me.HelpText = OpenFileDialog.FileName

        End If

    End Sub

    Private Sub MetGraphControl1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

End Class
