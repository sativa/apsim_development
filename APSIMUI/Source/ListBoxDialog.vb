Public Class ListBoxDialog
    Inherits System.Windows.Forms.Form



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
    Friend components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Public WithEvents ListView As System.Windows.Forms.ListView
    Friend WithEvents btnCancel As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.ListView = New System.Windows.Forms.ListView
        Me.btnCancel = New System.Windows.Forms.Button
        Me.SuspendLayout()
        '
        'ListView
        '
        Me.ListView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ListView.Location = New System.Drawing.Point(0, 0)
        Me.ListView.Name = "ListView"
        Me.ListView.Size = New System.Drawing.Size(178, 202)
        Me.ListView.TabIndex = 2
        Me.ListView.View = System.Windows.Forms.View.SmallIcon
        '
        'btnCancel
        '
        Me.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.btnCancel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.btnCancel.Location = New System.Drawing.Point(0, 170)
        Me.btnCancel.Name = "btnCancel"
        Me.btnCancel.Size = New System.Drawing.Size(178, 32)
        Me.btnCancel.TabIndex = 3
        Me.btnCancel.Text = "Cancel"
        '
        'ListBoxDialog
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(178, 202)
        Me.ControlBox = False
        Me.Controls.Add(Me.btnCancel)
        Me.Controls.Add(Me.ListView)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "ListBoxDialog"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub ListBoxDialog_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles MyBase.KeyPress, ListView.KeyPress
        If Asc(e.KeyChar) = Keys.Escape Then
            DialogResult = DialogResult.Cancel

        End If

    End Sub

    Private Sub ListView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles ListView.DoubleClick
        Me.DialogResult = DialogResult.OK
    End Sub
End Class
