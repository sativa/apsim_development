Public Class CropUI
    Inherits APSIMUI.BaseUI

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
    Friend WithEvents PictureBox As System.Windows.Forms.PictureBox
    Friend WithEvents Label As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(CropUI))
        Me.PictureBox = New System.Windows.Forms.PictureBox
        Me.Label = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'PictureBox
        '
        Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox.Image = CType(resources.GetObject("PictureBox.Image"), System.Drawing.Image)
        Me.PictureBox.Location = New System.Drawing.Point(0, 0)
        Me.PictureBox.Name = "PictureBox"
        Me.PictureBox.Size = New System.Drawing.Size(272, 705)
        Me.PictureBox.TabIndex = 0
        Me.PictureBox.TabStop = False
        '
        'Label
        '
        Me.Label.Font = New System.Drawing.Font("Microsoft Sans Serif", 36.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label.Location = New System.Drawing.Point(400, 144)
        Me.Label.Name = "Label"
        Me.Label.Size = New System.Drawing.Size(512, 64)
        Me.Label.TabIndex = 1
        Me.Label.Text = "CROP"
        '
        'CropUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1072, 705)
        Me.Controls.Add(Me.Label)
        Me.Controls.Add(Me.PictureBox)
        Me.Name = "CropUI"
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub refresh()
        Label.Text = GetValue("type")
    End Sub
End Class
