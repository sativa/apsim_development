Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports VBGeneral

Public Class OutputFileDescUI
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
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ReportingControl As APSIMUI.ReportingControl

    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.ReportingControl = New APSIMUI.ReportingControl
        Me.SuspendLayout()
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.CheckFileExists = False
        Me.OpenFileDialog.DefaultExt = "out"
        Me.OpenFileDialog.Filter = "APSIM output files(*.out)|*.out|All Files (*.*)|*.*"
        Me.OpenFileDialog.Title = "Enter output file name"
        '
        'ReportingControl
        '
        Me.ReportingControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ReportingControl.Location = New System.Drawing.Point(0, 23)
        Me.ReportingControl.Name = "ReportingControl"
        Me.ReportingControl.Size = New System.Drawing.Size(937, 575)
        Me.ReportingControl.TabIndex = 2
        Me.ReportingControl.UIManager = Nothing
        '
        'OutputFileDescUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(6, 15)
        Me.ClientSize = New System.Drawing.Size(937, 638)
        Me.Controls.Add(Me.ReportingControl)
        Me.Name = "OutputFileDescUI"
        Me.Controls.SetChildIndex(Me.ReportingControl, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub refresh()
        MyBase.Refresh()
        ReportingControl.UIManager = UIManager
        ReportingControl.Data = Data
        ReportingControl.fill()
        HelpLabel.Text = "Use the variables tab to specify APSIM variables that should be written to the output file. Use the file contents tab to view the contents of the output file."
    End Sub
End Class
