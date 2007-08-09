Imports VBGeneral

Public Class SplashScreenForm
    Inherits System.Windows.Forms.Form
    Private ApplicationName As String

#Region " Windows Form Designer generated code "

    Public Sub New(ByVal ApplicationName As String)
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        Me.ApplicationName = ApplicationName

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
    Private WithEvents VersionLabel As System.Windows.Forms.Label
    Friend WithEvents PictureBoxTop As System.Windows.Forms.PictureBox
    Friend WithEvents OkButton As System.Windows.Forms.Button

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents CloseTimer As System.Windows.Forms.Timer
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.CloseTimer = New System.Windows.Forms.Timer(Me.components)
        Me.VersionLabel = New System.Windows.Forms.Label
        Me.PictureBoxTop = New System.Windows.Forms.PictureBox
        Me.OkButton = New System.Windows.Forms.Button
        CType(Me.PictureBoxTop, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'CloseTimer
        '
        Me.CloseTimer.Enabled = True
        Me.CloseTimer.Interval = 1500
        '
        'VersionLabel
        '
        Me.VersionLabel.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.VersionLabel.BackColor = System.Drawing.Color.White
        Me.VersionLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.VersionLabel.ForeColor = System.Drawing.Color.Black
        Me.VersionLabel.Location = New System.Drawing.Point(1, 157)
        Me.VersionLabel.Name = "VersionLabel"
        Me.VersionLabel.Size = New System.Drawing.Size(107, 20)
        Me.VersionLabel.TabIndex = 14
        Me.VersionLabel.Text = "Version 1.0"
        Me.VersionLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'PictureBoxTop
        '
        Me.PictureBoxTop.BackColor = System.Drawing.Color.White
        Me.PictureBoxTop.Location = New System.Drawing.Point(0, 0)
        Me.PictureBoxTop.Name = "PictureBoxTop"
        Me.PictureBoxTop.Size = New System.Drawing.Size(50, 50)
        Me.PictureBoxTop.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBoxTop.TabIndex = 15
        Me.PictureBoxTop.TabStop = False
        '
        'OkButton
        '
        Me.OkButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OkButton.AutoSize = True
        Me.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.OkButton.Location = New System.Drawing.Point(20, 12)
        Me.OkButton.Name = "OkButton"
        Me.OkButton.Size = New System.Drawing.Size(75, 23)
        Me.OkButton.TabIndex = 16
        Me.OkButton.Text = "Ok"
        Me.OkButton.UseVisualStyleBackColor = True
        '
        'SplashScreenForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.AutoSize = True
        Me.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink
        Me.ClientSize = New System.Drawing.Size(107, 177)
        Me.ControlBox = False
        Me.Controls.Add(Me.OkButton)
        Me.Controls.Add(Me.VersionLabel)
        Me.Controls.Add(Me.PictureBoxTop)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.Location = New System.Drawing.Point(100, 100)
        Me.Name = "SplashScreenForm"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.TopMost = True
        CType(Me.PictureBoxTop, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region

    Private Sub OnFormLoad(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        PictureBoxTop.Image = Image.FromFile(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "SplashScreen"))
        VersionLabel.Text = "Version " & APSIMSettings.ApsimVersion
        OkButton.Visible = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "SplashScreenButtonVisible").ToLower = "yes"
        OkButton.Text = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), ApplicationName, "SplashScreenButtonText")
    End Sub

    Private Sub OnTimerTick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CloseTimer.Tick
        If Not OkButton.Visible Then
            Me.Close()
        End If
    End Sub

End Class
