Public Class AreaUI
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
    Friend WithEvents EnhancedListView1 As EnhancedListView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.EnhancedListView1 = New EnhancedListView
        Me.SuspendLayout()
        '
        'EnhancedListView1
        '
        Me.EnhancedListView1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.EnhancedListView1.Location = New System.Drawing.Point(0, 0)
        Me.EnhancedListView1.Name = "EnhancedListView1"
        Me.EnhancedListView1.Size = New System.Drawing.Size(656, 542)
        Me.EnhancedListView1.TabIndex = 0
        '
        'AreaUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(656, 542)
        Me.Controls.Add(Me.EnhancedListView1)
        Me.Name = "AreaUI"
        Me.ResumeLayout(False)

    End Sub

#End Region

End Class
