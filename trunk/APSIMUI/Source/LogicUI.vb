Public Class LogicUI
    Inherits VBGeneral.BaseView

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
        Me.InitTextBox = New System.Windows.Forms.RichTextBox
        Me.StartOfDayTab = New System.Windows.Forms.TabPage
        Me.StartOfDayTextBox = New System.Windows.Forms.RichTextBox
        Me.EndOfDayTab = New System.Windows.Forms.TabPage
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
        Me.TabControl.Location = New System.Drawing.Point(0, 40)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(688, 552)
        Me.TabControl.TabIndex = 0
        '
        'InitTab
        '
        Me.InitTab.Controls.Add(Me.InitTextBox)
        Me.InitTab.Location = New System.Drawing.Point(4, 22)
        Me.InitTab.Name = "InitTab"
        Me.InitTab.Size = New System.Drawing.Size(680, 526)
        Me.InitTab.TabIndex = 0
        Me.InitTab.Text = "Initialisation"
        '
        'InitTextBox
        '
        Me.InitTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.InitTextBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.InitTextBox.Location = New System.Drawing.Point(0, 0)
        Me.InitTextBox.Name = "InitTextBox"
        Me.InitTextBox.Size = New System.Drawing.Size(680, 526)
        Me.InitTextBox.TabIndex = 0
        Me.InitTextBox.Text = ""
        Me.InitTextBox.WordWrap = False
        '
        'StartOfDayTab
        '
        Me.StartOfDayTab.Controls.Add(Me.StartOfDayTextBox)
        Me.StartOfDayTab.Location = New System.Drawing.Point(4, 22)
        Me.StartOfDayTab.Name = "StartOfDayTab"
        Me.StartOfDayTab.Size = New System.Drawing.Size(680, 526)
        Me.StartOfDayTab.TabIndex = 1
        Me.StartOfDayTab.Text = "Start Of Day"
        '
        'StartOfDayTextBox
        '
        Me.StartOfDayTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.StartOfDayTextBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.StartOfDayTextBox.Location = New System.Drawing.Point(0, 0)
        Me.StartOfDayTextBox.Name = "StartOfDayTextBox"
        Me.StartOfDayTextBox.Size = New System.Drawing.Size(680, 526)
        Me.StartOfDayTextBox.TabIndex = 1
        Me.StartOfDayTextBox.Text = ""
        Me.StartOfDayTextBox.WordWrap = False
        '
        'EndOfDayTab
        '
        Me.EndOfDayTab.Controls.Add(Me.EndOfDayTextBox)
        Me.EndOfDayTab.Location = New System.Drawing.Point(4, 22)
        Me.EndOfDayTab.Name = "EndOfDayTab"
        Me.EndOfDayTab.Size = New System.Drawing.Size(680, 526)
        Me.EndOfDayTab.TabIndex = 2
        Me.EndOfDayTab.Text = "End Of Day"
        '
        'EndOfDayTextBox
        '
        Me.EndOfDayTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.EndOfDayTextBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.EndOfDayTextBox.Location = New System.Drawing.Point(0, 0)
        Me.EndOfDayTextBox.Name = "EndOfDayTextBox"
        Me.EndOfDayTextBox.Size = New System.Drawing.Size(680, 526)
        Me.EndOfDayTextBox.TabIndex = 2
        Me.EndOfDayTextBox.Text = ""
        Me.EndOfDayTextBox.WordWrap = False
        '
        'LogicUI
        '
        Me.Controls.Add(Me.TabControl)
        Me.Name = "LogicUI"
        Me.Size = New System.Drawing.Size(688, 592)
        Me.Controls.SetChildIndex(Me.TabControl, 0)
        Me.TabControl.ResumeLayout(False)
        Me.InitTab.ResumeLayout(False)
        Me.StartOfDayTab.ResumeLayout(False)
        Me.EndOfDayTab.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub Refresh()
        MyBase.Refresh()
        InitTextBox.Text = Replace(Controller.Data.ChildValue("init"), "[cr]", vbCrLf)
        StartOfDayTextBox.Text = Replace(Controller.Data.ChildValue("startofday"), "[cr]", vbCrLf)

        EndOfDayTextBox.Text = Replace(Controller.Data.ChildValue("endofday"), "[cr]", vbCrLf)
        HelpText = "Enter your management logic into the edit box above."
    End Sub


    Private Sub InitTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles InitTextBox.Leave
        Dim text As String = Replace(InitTextBox.Text, vbCrLf, "[cr]")
        If text = Nothing Then
            text = ""
        End If
        Controller.Data.Child("init").Value = text
    End Sub
    Private Sub StartOfDayTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles StartOfDayTextBox.Leave
        Dim text As String = Replace(StartOfDayTextBox.Text, vbCrLf, "[cr]")
        If text = Nothing Then
            text = ""
        End If
        Controller.Data.Child("startofday").Value = text
    End Sub
    Private Sub EndOfDayTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles EndOfDayTextBox.Leave
        Dim text As String = Replace(EndOfDayTextBox.Text, vbCrLf, "[cr]")
        If text = Nothing Then
            text = ""
        End If
        Controller.Data.Child("endofday").Value = text
    End Sub

    Public Overrides Sub Save()
        InitTextBox_Leave(Nothing, Nothing)
        StartOfDayTextBox_Leave(Nothing, Nothing)
        EndOfDayTextBox_Leave(Nothing, Nothing)
    End Sub
End Class
