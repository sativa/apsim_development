Public Class TrackerSlider
    Inherits System.Windows.Forms.Form



#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        Me.lblSliderValue.Text = Me.TrackerNumber.Value.ToString()

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
    Friend components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Public WithEvents TrackerNumber As System.Windows.Forms.TrackBar
    Friend WithEvents lblSliderValue As System.Windows.Forms.Label
    Friend WithEvents btnCancel As System.Windows.Forms.Button
    Friend WithEvents btnOK As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.lblSliderValue = New System.Windows.Forms.Label
        Me.TrackerNumber = New System.Windows.Forms.TrackBar
        Me.btnCancel = New System.Windows.Forms.Button
        Me.btnOK = New System.Windows.Forms.Button
        Me.Panel1.SuspendLayout()
        CType(Me.TrackerNumber, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.lblSliderValue)
        Me.Panel1.Controls.Add(Me.TrackerNumber)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Top
        Me.Panel1.Location = New System.Drawing.Point(0, 0)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(336, 56)
        Me.Panel1.TabIndex = 5
        '
        'lblSliderValue
        '
        Me.lblSliderValue.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblSliderValue.Location = New System.Drawing.Point(296, 18)
        Me.lblSliderValue.Name = "lblSliderValue"
        Me.lblSliderValue.Size = New System.Drawing.Size(32, 24)
        Me.lblSliderValue.TabIndex = 6
        Me.lblSliderValue.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'TrackerNumber
        '
        Me.TrackerNumber.Location = New System.Drawing.Point(8, 8)
        Me.TrackerNumber.Maximum = 50
        Me.TrackerNumber.Minimum = 1
        Me.TrackerNumber.Name = "TrackerNumber"
        Me.TrackerNumber.Size = New System.Drawing.Size(288, 45)
        Me.TrackerNumber.TabIndex = 5
        Me.TrackerNumber.TickStyle = System.Windows.Forms.TickStyle.TopLeft
        Me.TrackerNumber.Value = 1
        '
        'btnCancel
        '
        Me.btnCancel.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.btnCancel.Location = New System.Drawing.Point(256, 56)
        Me.btnCancel.Name = "btnCancel"
        Me.btnCancel.Size = New System.Drawing.Size(72, 32)
        Me.btnCancel.TabIndex = 6
        Me.btnCancel.Text = "Cancel"
        '
        'btnOK
        '
        Me.btnOK.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnOK.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.btnOK.Location = New System.Drawing.Point(176, 56)
        Me.btnOK.Name = "btnOK"
        Me.btnOK.Size = New System.Drawing.Size(72, 32)
        Me.btnOK.TabIndex = 7
        Me.btnOK.Text = "OK"
        '
        'TrackerSlider
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(336, 96)
        Me.ControlBox = False
        Me.Controls.Add(Me.btnOK)
        Me.Controls.Add(Me.btnCancel)
        Me.Controls.Add(Me.Panel1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Name = "TrackerSlider"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.Panel1.ResumeLayout(False)
        CType(Me.TrackerNumber, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub TrackerSlider_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles MyBase.KeyPress
        If Asc(e.KeyChar) = Keys.Escape Then
            DialogResult = Windows.Forms.DialogResult.Cancel

        End If
    End Sub



    Private Sub btnCancel_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        Me.DialogResult = Windows.Forms.DialogResult.Cancel
    End Sub

    Private Sub btnOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnOK.Click
        DialogResult = Windows.Forms.DialogResult.OK
    End Sub

    Private Sub TrackerNumber_ValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TrackerNumber.ValueChanged
        Me.lblSliderValue.Text = Me.TrackerNumber.Value.ToString()
    End Sub
End Class

