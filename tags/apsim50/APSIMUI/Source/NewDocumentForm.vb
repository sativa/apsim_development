Imports VBGeneral
Imports System
Imports System.Drawing
Imports System.Collections.Specialized
Public Class NewDocumentForm
    Inherits System.Windows.Forms.Form
    Protected SelectedData As APSIMData
    Protected ApsimUI As New ApsimUIController("", "", "")
#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub
    Private components As System.ComponentModel.IContainer
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents OKButton As System.Windows.Forms.Button
    Friend WithEvents CancelButton1 As System.Windows.Forms.Button
    Friend WithEvents DataTree As VBGeneral.DataTree
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(NewDocumentForm))
        Me.Label1 = New System.Windows.Forms.Label
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.OKButton = New System.Windows.Forms.Button
        Me.CancelButton1 = New System.Windows.Forms.Button
        Me.DataTree = New VBGeneral.DataTree
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
        Me.PictureBox1.Size = New System.Drawing.Size(120, 398)
        Me.PictureBox1.TabIndex = 1
        Me.PictureBox1.TabStop = False
        '
        'OKButton
        '
        Me.OKButton.DialogResult = System.Windows.Forms.DialogResult.OK
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
        'DataTree
        '
        Me.DataTree.AllowDrop = True
        Me.DataTree.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.DataTree.AutoScroll = True
        Me.DataTree.BackColor = System.Drawing.SystemColors.Control
        Me.DataTree.Controller = Nothing
        Me.DataTree.HelpText = ""
        Me.DataTree.Location = New System.Drawing.Point(136, 32)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(456, 716)
        Me.DataTree.Sorted = False
        Me.DataTree.TabIndex = 5
        '
        'NewDocumentForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.CancelButton = Me.CancelButton1
        Me.ClientSize = New System.Drawing.Size(610, 398)
        Me.Controls.Add(Me.OKButton)
        Me.Controls.Add(Me.CancelButton1)
        Me.Controls.Add(Me.DataTree)
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


    '-----------------------------------------------------
    ' Document has just been displayed - set everything up
    ' ----------------------------------------------------
    Private Sub NewDocumentForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim inifile As New APSIMSettings
        Dim TemplateFile As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "new_docs")
        ApsimUI.FileOpen(TemplateFile)
        DataTree.MaximumNumLevels = 1
        DataTree.Controller = ApsimUI
        DataTree.HelpText = "Select a new simulation"
        DataTree.Dock = DockStyle.None
        DataTree.Refresh()
    End Sub


    ' -----------------------------------
    ' Return selection to caller.
    ' -----------------------------------
    Public ReadOnly Property Selection() As APSIMData
        Get
            Return DataTree.Controller.Data
        End Get
    End Property


    ' -----------------------------------
    ' User has double clicked.
    ' -----------------------------------
    Private Sub DataTree_DoubleClick() Handles DataTree.DoubleClickEvent
        OKButton.PerformClick()
    End Sub
End Class
