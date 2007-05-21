Public Class OutputFileExport
    Inherits System.Windows.Forms.Form

    Private m_OutputFileCollection As System.Collections.Specialized.StringCollection
    Private m_SelectedFileCollection As System.Collections.Specialized.StringCollection


    Public Property OutputFileCollection() As System.Collections.Specialized.StringCollection
        Get
            Return Me.m_OutputFileCollection

        End Get

        ' Reset the list in the list box
        Set(ByVal OutputFiles As System.Collections.Specialized.StringCollection)
            Me.m_OutputFileCollection = OutputFiles

            Me.OutputFileListbox.Items.Clear()

            For Each str As String In OutputFiles
                Me.OutputFileListbox.Items.Add(System.IO.Path.GetFileName(str), True)

            Next

        End Set
    End Property

    Public ReadOnly Property SelectedOutputFiles() As System.Collections.Specialized.StringCollection
        Get
            Return Me.m_SelectedFileCollection

        End Get
    End Property

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()
        Me.m_SelectedFileCollection = New System.Collections.Specialized.StringCollection

    End Sub

    Public Sub New(ByVal OutputFileList As System.Collections.Specialized.StringCollection)
        Me.New()
        Me.OutputFileCollection = OutputFileList
        Me.m_SelectedFileCollection = New System.Collections.Specialized.StringCollection

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
    Friend WithEvents OKButton As System.Windows.Forms.Button
    Friend WithEvents ButtonPanel As System.Windows.Forms.Panel
    Friend WithEvents HeadingLabel As System.Windows.Forms.Label
    Friend WithEvents OutputFileListbox As System.Windows.Forms.CheckedListBox
    Friend WithEvents CancelBtn As System.Windows.Forms.Button
    Friend WithEvents UnselectButton As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(OutputFileExport))
        Me.HeadingLabel = New System.Windows.Forms.Label
        Me.OutputFileListbox = New System.Windows.Forms.CheckedListBox
        Me.OKButton = New System.Windows.Forms.Button
        Me.ButtonPanel = New System.Windows.Forms.Panel
        Me.UnselectButton = New System.Windows.Forms.Button
        Me.CancelBtn = New System.Windows.Forms.Button
        Me.ButtonPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'HeadingLabel
        '
        Me.HeadingLabel.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.HeadingLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.HeadingLabel.Font = New System.Drawing.Font("Arial", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.HeadingLabel.Image = CType(resources.GetObject("HeadingLabel.Image"), System.Drawing.Image)
        Me.HeadingLabel.ImageAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.HeadingLabel.Location = New System.Drawing.Point(0, 0)
        Me.HeadingLabel.Name = "HeadingLabel"
        Me.HeadingLabel.Size = New System.Drawing.Size(472, 40)
        Me.HeadingLabel.TabIndex = 0
        Me.HeadingLabel.Text = "  Choose output files to export..."
        Me.HeadingLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'OutputFileListbox
        '
        Me.OutputFileListbox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OutputFileListbox.CheckOnClick = True
        Me.OutputFileListbox.Location = New System.Drawing.Point(8, 48)
        Me.OutputFileListbox.Name = "OutputFileListbox"
        Me.OutputFileListbox.Size = New System.Drawing.Size(456, 214)
        Me.OutputFileListbox.TabIndex = 1
        '
        'OKButton
        '
        Me.OKButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OKButton.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.OKButton.Location = New System.Drawing.Point(280, 16)
        Me.OKButton.Name = "OKButton"
        Me.OKButton.Size = New System.Drawing.Size(88, 32)
        Me.OKButton.TabIndex = 0
        Me.OKButton.Text = "OK"
        '
        'ButtonPanel
        '
        Me.ButtonPanel.Controls.Add(Me.UnselectButton)
        Me.ButtonPanel.Controls.Add(Me.CancelBtn)
        Me.ButtonPanel.Controls.Add(Me.OKButton)
        Me.ButtonPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ButtonPanel.Location = New System.Drawing.Point(0, 270)
        Me.ButtonPanel.Name = "ButtonPanel"
        Me.ButtonPanel.Size = New System.Drawing.Size(472, 64)
        Me.ButtonPanel.TabIndex = 2
        '
        'UnselectButton
        '
        Me.UnselectButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.UnselectButton.Location = New System.Drawing.Point(8, 16)
        Me.UnselectButton.Name = "UnselectButton"
        Me.UnselectButton.Size = New System.Drawing.Size(88, 32)
        Me.UnselectButton.TabIndex = 2
        Me.UnselectButton.Text = "Unselect all"
        '
        'CancelBtn
        '
        Me.CancelBtn.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.CancelBtn.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.CancelBtn.Location = New System.Drawing.Point(376, 16)
        Me.CancelBtn.Name = "CancelBtn"
        Me.CancelBtn.Size = New System.Drawing.Size(88, 32)
        Me.CancelBtn.TabIndex = 1
        Me.CancelBtn.Text = "Cancel"
        '
        'OutputFileExport
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(472, 334)
        Me.Controls.Add(Me.ButtonPanel)
        Me.Controls.Add(Me.OutputFileListbox)
        Me.Controls.Add(Me.HeadingLabel)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "OutputFileExport"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Output File Export"
        Me.ButtonPanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub OKButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OKButton.Click
        Me.m_SelectedFileCollection.Clear()

        For Each checkedIndex As Integer In Me.OutputFileListbox.CheckedIndices
            Me.SelectedOutputFiles.Add(Me.OutputFileCollection(checkedIndex))
        Next
    End Sub

    Private Sub UnselectButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnselectButton.Click
        For i As Integer = 0 To Me.OutputFileListbox.Items.Count - 1
            Me.OutputFileListbox.SetItemChecked(i, False)
        Next
    End Sub
End Class
