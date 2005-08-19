Public Class LogicUI
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
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents InitTab As System.Windows.Forms.TabPage
    Friend WithEvents StartOfDayTab As System.Windows.Forms.TabPage
    Friend WithEvents EndOfDayTab As System.Windows.Forms.TabPage
    Friend WithEvents InitTextBox As System.Windows.Forms.RichTextBox
    Friend WithEvents StartOfDayTextBox As System.Windows.Forms.RichTextBox
    Friend WithEvents EndOfDayTextBox As System.Windows.Forms.RichTextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.InitTab = New System.Windows.Forms.TabPage
        Me.StartOfDayTab = New System.Windows.Forms.TabPage
        Me.EndOfDayTab = New System.Windows.Forms.TabPage
        Me.InitTextBox = New System.Windows.Forms.RichTextBox
        Me.StartOfDayTextBox = New System.Windows.Forms.RichTextBox
        Me.EndOfDayTextBox = New System.Windows.Forms.RichTextBox
        Me.TabControl.SuspendLayout()
        Me.InitTab.SuspendLayout()
        Me.StartOfDayTab.SuspendLayout()
        Me.EndOfDayTab.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl
        '
        Me.TabControl.Controls.Add(Me.InitTab)
        Me.TabControl.Controls.Add(Me.StartOfDayTab)
        Me.TabControl.Controls.Add(Me.EndOfDayTab)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl.Location = New System.Drawing.Point(0, 0)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(1072, 705)
        Me.TabControl.TabIndex = 0
        '
        'InitTab
        '
        Me.InitTab.Controls.Add(Me.InitTextBox)
        Me.InitTab.Location = New System.Drawing.Point(4, 22)
        Me.InitTab.Name = "InitTab"
        Me.InitTab.Size = New System.Drawing.Size(1064, 679)
        Me.InitTab.TabIndex = 0
        Me.InitTab.Text = "Initialisation"
        '
        'StartOfDayTab
        '
        Me.StartOfDayTab.Controls.Add(Me.StartOfDayTextBox)
        Me.StartOfDayTab.Location = New System.Drawing.Point(4, 22)
        Me.StartOfDayTab.Name = "StartOfDayTab"
        Me.StartOfDayTab.Size = New System.Drawing.Size(1064, 679)
        Me.StartOfDayTab.TabIndex = 1
        Me.StartOfDayTab.Text = "Start Of Day"
        '
        'EndOfDayTab
        '
        Me.EndOfDayTab.Controls.Add(Me.EndOfDayTextBox)
        Me.EndOfDayTab.Location = New System.Drawing.Point(4, 22)
        Me.EndOfDayTab.Name = "EndOfDayTab"
        Me.EndOfDayTab.Size = New System.Drawing.Size(1064, 679)
        Me.EndOfDayTab.TabIndex = 2
        Me.EndOfDayTab.Text = "End Of Day"
        '
        'InitTextBox
        '
        Me.InitTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.InitTextBox.Location = New System.Drawing.Point(0, 0)
        Me.InitTextBox.Name = "InitTextBox"
        Me.InitTextBox.Size = New System.Drawing.Size(1064, 679)
        Me.InitTextBox.TabIndex = 0
        Me.InitTextBox.Text = ""
        Me.InitTextBox.WordWrap = False
        '
        'StartOfDayTextBox
        '
        Me.StartOfDayTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.StartOfDayTextBox.Location = New System.Drawing.Point(0, 0)
        Me.StartOfDayTextBox.Name = "StartOfDayTextBox"
        Me.StartOfDayTextBox.Size = New System.Drawing.Size(1064, 679)
        Me.StartOfDayTextBox.TabIndex = 1
        Me.StartOfDayTextBox.Text = ""
        Me.StartOfDayTextBox.WordWrap = False
        '
        'EndOfDayTextBox
        '
        Me.EndOfDayTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.EndOfDayTextBox.Location = New System.Drawing.Point(0, 0)
        Me.EndOfDayTextBox.Name = "EndOfDayTextBox"
        Me.EndOfDayTextBox.Size = New System.Drawing.Size(1064, 679)
        Me.EndOfDayTextBox.TabIndex = 2
        Me.EndOfDayTextBox.Text = ""
        Me.EndOfDayTextBox.WordWrap = False
        '
        'LogicUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1072, 705)
        Me.Controls.Add(Me.TabControl)
        Me.Name = "LogicUI"
        Me.TabControl.ResumeLayout(False)
        Me.InitTab.ResumeLayout(False)
        Me.StartOfDayTab.ResumeLayout(False)
        Me.EndOfDayTab.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        MyBase.Refresh()
        InitTextBox.Text = Replace(GetValue("init"), "[cr]", vbCrLf)
        StartOfDayTextBox.Text = Replace(GetValue("startofday"), "[cr]", vbCrLf)
        EndOfDayTextBox.Text = Replace(GetValue("endofday"), "[cr]", vbCrLf)
    End Sub


    Private Sub InitTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles InitTextBox.Leave
        Data.Child("init").Value = Replace(InitTextBox.Text, vbCrLf, "[cr]")
    End Sub
    Private Sub StartOfDayTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles StartOfDayTextBox.Leave
        Data.Child("startofday").Value = Replace(StartOfDayTextBox.Text, vbCrLf, "[cr]")
    End Sub
    Private Sub EndOfDayTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles EndOfDayTextBox.Leave
        Data.Child("endofday").Value = Replace(EndOfDayTextBox.Text, vbCrLf, "[cr]")
    End Sub
End Class
