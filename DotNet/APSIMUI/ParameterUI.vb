Imports System.Xml
Public Class ParameterUI
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
    Friend WithEvents TextBox As System.Windows.Forms.TextBox
    Friend WithEvents Label As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.TextBox = New System.Windows.Forms.TextBox
        Me.Label = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'TextBox
        '
        Me.TextBox.Location = New System.Drawing.Point(56, 288)
        Me.TextBox.Name = "TextBox"
        Me.TextBox.Size = New System.Drawing.Size(280, 20)
        Me.TextBox.TabIndex = 0
        Me.TextBox.Text = "TextBox1"
        '
        'Label
        '
        Me.Label.Location = New System.Drawing.Point(80, 32)
        Me.Label.Name = "Label"
        Me.Label.Size = New System.Drawing.Size(128, 24)
        Me.Label.TabIndex = 1
        Me.Label.Text = "Label1"
        '
        'ParameterUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(560, 414)
        Me.Controls.Add(Me.Label)
        Me.Controls.Add(Me.TextBox)
        Me.Name = "ParameterUI"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        MyBase.Refresh()
        TextBox.Text = APSIMData.Value
        Dim x As String = APSIMData.Type
        Label.Text = "Data Type is " + x
    End Sub

    Private Sub TextBox_Enter(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox.Enter
        If TextBox.Visible = True Then APSIMData.Value = TextBox.Text
    End Sub
End Class
