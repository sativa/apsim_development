Imports VBGeneral
Public Class GenericUI
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
    Friend WithEvents Grid As System.Windows.Forms.DataGrid
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.Grid = New System.Windows.Forms.DataGrid
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Grid
        '
        Me.Grid.CaptionBackColor = System.Drawing.SystemColors.Control
        Me.Grid.DataMember = ""
        Me.Grid.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Grid.HeaderForeColor = System.Drawing.SystemColors.ControlText
        Me.Grid.Location = New System.Drawing.Point(0, 0)
        Me.Grid.Name = "Grid"
        Me.Grid.Size = New System.Drawing.Size(984, 611)
        Me.Grid.TabIndex = 0
        '
        'GenericUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(984, 611)
        Me.Controls.Add(Me.Grid)
        Me.Name = "GenericUI"
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        MyBase.Refresh()

        Grid.DataSource = MyData.DataTable

        Grid.Show()

    End Sub


    Private Sub Grid_CurrentCellChanged1(ByVal sender As Object, ByVal e As System.EventArgs) Handles Grid.CurrentCellChanged
        If Me.Visible Then
            MyData.DataTable = Grid.DataSource
        End If
    End Sub
End Class
