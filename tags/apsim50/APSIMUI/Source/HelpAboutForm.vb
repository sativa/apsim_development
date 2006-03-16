Imports VBGeneral
Public Class HelpAboutForm
    Inherits System.Windows.Forms.Form

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

        Dim inifile As New APSIMSettings
        VersionLabel.Text = "APSIM Version: " + inifile.ApsimVersion
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
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents PictureBoxTop As System.Windows.Forms.PictureBox
    Friend WithEvents OKButton As System.Windows.Forms.Button
    Friend WithEvents VersionLabel As System.Windows.Forms.Label
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents LinkLabel1 As System.Windows.Forms.LinkLabel
    Friend WithEvents LinkLabel2 As System.Windows.Forms.LinkLabel
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(HelpAboutForm))
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.PictureBoxTop = New System.Windows.Forms.PictureBox
        Me.OKButton = New System.Windows.Forms.Button
        Me.VersionLabel = New System.Windows.Forms.Label
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.LinkLabel1 = New System.Windows.Forms.LinkLabel
        Me.LinkLabel2 = New System.Windows.Forms.LinkLabel
        Me.SuspendLayout()
        '
        'PictureBox1
        '
        Me.PictureBox1.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(0, 0)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(100, 224)
        Me.PictureBox1.TabIndex = 10
        Me.PictureBox1.TabStop = False
        '
        'PictureBoxTop
        '
        Me.PictureBoxTop.Image = CType(resources.GetObject("PictureBoxTop.Image"), System.Drawing.Image)
        Me.PictureBoxTop.Location = New System.Drawing.Point(96, 0)
        Me.PictureBoxTop.Name = "PictureBoxTop"
        Me.PictureBoxTop.Size = New System.Drawing.Size(646, 88)
        Me.PictureBoxTop.TabIndex = 11
        Me.PictureBoxTop.TabStop = False
        '
        'OKButton
        '
        Me.OKButton.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.OKButton.Location = New System.Drawing.Point(472, 192)
        Me.OKButton.Name = "OKButton"
        Me.OKButton.Size = New System.Drawing.Size(80, 24)
        Me.OKButton.TabIndex = 12
        Me.OKButton.Text = "OK"
        '
        'VersionLabel
        '
        Me.VersionLabel.Location = New System.Drawing.Point(192, 96)
        Me.VersionLabel.Name = "VersionLabel"
        Me.VersionLabel.Size = New System.Drawing.Size(216, 24)
        Me.VersionLabel.TabIndex = 13
        Me.VersionLabel.Text = "Label1"
        '
        'Panel1
        '
        Me.Panel1.BackColor = System.Drawing.SystemColors.ControlDark
        Me.Panel1.ForeColor = System.Drawing.SystemColors.ControlDark
        Me.Panel1.Location = New System.Drawing.Point(112, 168)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(432, 3)
        Me.Panel1.TabIndex = 14
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(112, 176)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(368, 56)
        Me.Label1.TabIndex = 15
        Me.Label1.Text = "Warning: This product is protected under copyright law and international treaties" & _
        ".  Unauthorised reproduction or copying of this program, or any portion of it, m" & _
        "ay result in  severe civil or criminal penalties."
        '
        'Label2
        '
        Me.Label2.Location = New System.Drawing.Point(112, 96)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(80, 16)
        Me.Label2.TabIndex = 16
        Me.Label2.Text = "Product name:"
        '
        'Label3
        '
        Me.Label3.Location = New System.Drawing.Point(112, 120)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(208, 16)
        Me.Label3.TabIndex = 17
        Me.Label3.Text = "Copyright APSRU.  All rights reserved."
        '
        'LinkLabel1
        '
        Me.LinkLabel1.Location = New System.Drawing.Point(112, 136)
        Me.LinkLabel1.Name = "LinkLabel1"
        Me.LinkLabel1.Size = New System.Drawing.Size(88, 16)
        Me.LinkLabel1.TabIndex = 18
        Me.LinkLabel1.TabStop = True
        Me.LinkLabel1.Text = "www.apsim.info"
        '
        'LinkLabel2
        '
        Me.LinkLabel2.Location = New System.Drawing.Point(112, 152)
        Me.LinkLabel2.Name = "LinkLabel2"
        Me.LinkLabel2.Size = New System.Drawing.Size(112, 24)
        Me.LinkLabel2.TabIndex = 19
        Me.LinkLabel2.TabStop = True
        Me.LinkLabel2.Text = "www.apsru.gov.au"
        '
        'HelpAboutForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.CancelButton = Me.OKButton
        Me.ClientSize = New System.Drawing.Size(562, 224)
        Me.Controls.Add(Me.LinkLabel1)
        Me.Controls.Add(Me.OKButton)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.VersionLabel)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.PictureBoxTop)
        Me.Controls.Add(Me.LinkLabel2)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "HelpAboutForm"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "About APSIM"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub OKButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OKButton.Click
        Me.Close()
    End Sub

 
    Private Sub LinkLabel1_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked

    End Sub
End Class
