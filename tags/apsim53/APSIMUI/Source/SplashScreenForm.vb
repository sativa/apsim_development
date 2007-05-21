Public Class SplashScreenForm
    Inherits System.Windows.Forms.Form

    Public Property VersionText() As String
        ' Get/Sets the entire version text label.
        Get
            Return Me.VersionLabel.Text

        End Get
        Set(ByVal newText As String)
            Me.VersionLabel.Text = newText

        End Set
    End Property

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()



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
    Friend WithEvents CloseTimer As System.Windows.Forms.Timer
    Friend WithEvents SplashPanel As System.Windows.Forms.Panel
    Friend WithEvents PictureBoxTop As System.Windows.Forms.PictureBox
    Friend WithEvents APSIMLabel As System.Windows.Forms.Label
    Private WithEvents VersionLabel As System.Windows.Forms.Label
    Friend WithEvents LeftBanner As System.Windows.Forms.PictureBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(SplashScreenForm))
        Me.CloseTimer = New System.Windows.Forms.Timer(Me.components)
        Me.SplashPanel = New System.Windows.Forms.Panel
        Me.VersionLabel = New System.Windows.Forms.Label
        Me.APSIMLabel = New System.Windows.Forms.Label
        Me.PictureBoxTop = New System.Windows.Forms.PictureBox
        Me.LeftBanner = New System.Windows.Forms.PictureBox
        Me.SplashPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'CloseTimer
        '
        Me.CloseTimer.Interval = 1500
        '
        'SplashPanel
        '
        Me.SplashPanel.BackColor = System.Drawing.Color.White
        Me.SplashPanel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.SplashPanel.Controls.Add(Me.VersionLabel)
        Me.SplashPanel.Controls.Add(Me.APSIMLabel)
        Me.SplashPanel.Controls.Add(Me.PictureBoxTop)
        Me.SplashPanel.Controls.Add(Me.LeftBanner)
        Me.SplashPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplashPanel.Location = New System.Drawing.Point(0, 0)
        Me.SplashPanel.Name = "SplashPanel"
        Me.SplashPanel.Size = New System.Drawing.Size(608, 294)
        Me.SplashPanel.TabIndex = 0
        '
        'VersionLabel
        '
        Me.VersionLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.VersionLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 27.75!, CType((System.Drawing.FontStyle.Bold Or System.Drawing.FontStyle.Italic), System.Drawing.FontStyle), System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.VersionLabel.ForeColor = System.Drawing.Color.DarkOrange
        Me.VersionLabel.Location = New System.Drawing.Point(95, 208)
        Me.VersionLabel.Name = "VersionLabel"
        Me.VersionLabel.Size = New System.Drawing.Size(511, 40)
        Me.VersionLabel.TabIndex = 13
        Me.VersionLabel.Text = "Version 1.0"
        Me.VersionLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'APSIMLabel
        '
        Me.APSIMLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.APSIMLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 72.0!, CType((System.Drawing.FontStyle.Bold Or System.Drawing.FontStyle.Italic), System.Drawing.FontStyle), System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.APSIMLabel.ForeColor = System.Drawing.Color.RoyalBlue
        Me.APSIMLabel.Location = New System.Drawing.Point(95, 112)
        Me.APSIMLabel.Name = "APSIMLabel"
        Me.APSIMLabel.Size = New System.Drawing.Size(511, 96)
        Me.APSIMLabel.TabIndex = 12
        Me.APSIMLabel.Text = "APSIM"
        Me.APSIMLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'PictureBoxTop
        '
        Me.PictureBoxTop.Dock = System.Windows.Forms.DockStyle.Top
        Me.PictureBoxTop.Image = CType(resources.GetObject("PictureBoxTop.Image"), System.Drawing.Image)
        Me.PictureBoxTop.Location = New System.Drawing.Point(95, 0)
        Me.PictureBoxTop.Name = "PictureBoxTop"
        Me.PictureBoxTop.Size = New System.Drawing.Size(511, 112)
        Me.PictureBoxTop.TabIndex = 11
        Me.PictureBoxTop.TabStop = False
        '
        'LeftBanner
        '
        Me.LeftBanner.Dock = System.Windows.Forms.DockStyle.Left
        Me.LeftBanner.Image = CType(resources.GetObject("LeftBanner.Image"), System.Drawing.Image)
        Me.LeftBanner.Location = New System.Drawing.Point(0, 0)
        Me.LeftBanner.Name = "LeftBanner"
        Me.LeftBanner.Size = New System.Drawing.Size(95, 292)
        Me.LeftBanner.TabIndex = 10
        Me.LeftBanner.TabStop = False
        '
        'SplashScreenForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(608, 294)
        Me.ControlBox = False
        Me.Controls.Add(Me.SplashPanel)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.Name = "SplashScreenForm"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.TopMost = True
        Me.SplashPanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub CloseTimer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CloseTimer.Tick
        ' Close the form on timer tick.
        Me.Close()

    End Sub

    Private Sub SplashScreenForm_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Activated
        ' When form is activated, start the countdown timer.
        Me.CloseTimer.Enabled = True
    End Sub

End Class
