Imports System
Imports System.IO
Public Class VineLogicUI
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
    Friend WithEvents ImageList2 As System.Windows.Forms.ImageList
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents TextBox As System.Windows.Forms.RichTextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(VineLogicUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.TextBox = New System.Windows.Forms.RichTextBox
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.ImageList2 = New System.Windows.Forms.ImageList(Me.components)
        Me.SuspendLayout()
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'TextBox
        '
        Me.TextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TextBox.Font = New System.Drawing.Font("Courier New", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextBox.Location = New System.Drawing.Point(0, 23)
        Me.TextBox.Name = "TextBox"
        Me.TextBox.Size = New System.Drawing.Size(984, 425)
        Me.TextBox.TabIndex = 3
        Me.TextBox.Text = ""
        Me.TextBox.WordWrap = False
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.Filter = "Output files|*.out|Summary file|*.sum|All files|*.*"
        '
        'ImageList2
        '
        Me.ImageList2.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList2.TransparentColor = System.Drawing.Color.Transparent
        '
        'VineLogicUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(984, 488)
        Me.Controls.Add(Me.TextBox)
        Me.Name = "VineLogicUI"
        Me.Controls.SetChildIndex(Me.TextBox, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        Try
            MyBase.Refresh()
            HelpLabel.Text = "Parameterisation of this vinelogic component is via the standard VineLogic Input file structure shown above."
            TextBox.Text = MyData.Child("data").Value
        Catch E As System.Exception
            MsgBox(E.Message, MsgBoxStyle.Critical, "Error in refreshing Summary File UI")
        End Try

    End Sub
    Private Sub TextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox.Leave
        Data.Child("data").Value = TextBox.Text
    End Sub
End Class
