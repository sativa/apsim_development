Public Class SoilUI
    Inherits APSIMUI.BaseUI
    Declare Ansi Function CreateSoilForm Lib "soil.dll" Alias "CreateSoilForm" (ByVal parentHandle As IntPtr, ByVal xml As String) As IntPtr
    Declare Ansi Sub DeleteSoilForm Lib "soil.dll" Alias "DeleteSoilForm" (ByVal formHandle As IntPtr)
    Declare Ansi Sub MoveWindow Lib "user32.dll" Alias "MoveWindow" (ByVal hWnd As IntPtr, _
                                                                      ByVal X As Integer, _
                                                                      ByVal Y As Integer, _
                                                                      ByVal Width As Integer, _
                                                                      ByVal Height As Integer, _
                                                                      ByVal Repaint As Integer)
    Private childHandle As IntPtr


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
        DeleteSoilForm(childHandle)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.TabControl1 = New System.Windows.Forms.TabControl
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(0, 0)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(1124, 676)
        Me.TabControl1.TabIndex = 0
        '
        'SoilUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1124, 676)
        Me.Controls.Add(Me.TabControl1)
        Me.Name = "SoilUI"
        Me.ResumeLayout(False)

    End Sub


#End Region
    Overrides Sub setup()
        Dim xml As String = APSIMData.XML
        childHandle = CreateSoilForm(TabControl1.Handle, xml)
        ResizeChild()
    End Sub
    Sub ResizeChild()
        MoveWindow(childHandle, Left, Top, Width, Height, 0)
    End Sub
    Private Sub TabControl1_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles TabControl1.Resize
        ResizeChild()
    End Sub
End Class
