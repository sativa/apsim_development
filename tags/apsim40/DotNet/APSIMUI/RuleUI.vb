Public Class RuleUI
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
    Friend WithEvents LogicTextBox As System.Windows.Forms.TextBox
    Friend WithEvents EventComboBox As System.Windows.Forms.ComboBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents RuleNameLabel As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.LogicTextBox = New System.Windows.Forms.TextBox
        Me.EventComboBox = New System.Windows.Forms.ComboBox
        Me.Label1 = New System.Windows.Forms.Label
        Me.RuleNameLabel = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'LogicTextBox
        '
        Me.LogicTextBox.AcceptsReturn = True
        Me.LogicTextBox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.LogicTextBox.AutoSize = False
        Me.LogicTextBox.Location = New System.Drawing.Point(8, 48)
        Me.LogicTextBox.Multiline = True
        Me.LogicTextBox.Name = "LogicTextBox"
        Me.LogicTextBox.Size = New System.Drawing.Size(844, 653)
        Me.LogicTextBox.TabIndex = 0
        Me.LogicTextBox.Text = ""
        '
        'EventComboBox
        '
        Me.EventComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.EventComboBox.Items.AddRange(New Object() {"start_of_day", "end_of_day"})
        Me.EventComboBox.Location = New System.Drawing.Point(624, 10)
        Me.EventComboBox.Name = "EventComboBox"
        Me.EventComboBox.Size = New System.Drawing.Size(168, 21)
        Me.EventComboBox.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(547, 10)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(64, 24)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Execute at:"
        '
        'RuleNameLabel
        '
        Me.RuleNameLabel.Location = New System.Drawing.Point(16, 10)
        Me.RuleNameLabel.Name = "RuleNameLabel"
        Me.RuleNameLabel.Size = New System.Drawing.Size(496, 24)
        Me.RuleNameLabel.TabIndex = 4
        Me.RuleNameLabel.Text = "Rule Name:"
        '
        'RuleUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(860, 707)
        Me.Controls.Add(Me.RuleNameLabel)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.EventComboBox)
        Me.Controls.Add(Me.LogicTextBox)
        Me.Name = "RuleUI"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        MyBase.Refresh()
        Dim LogicText As String = GetValue("logic")
        LogicText = Replace(LogicText, "[cr]", vbCrLf)
        LogicTextBox.Text = LogicText

        RuleNameLabel.Text = "Rule Name: " + GetAttribute("name")
        Dim eventname As String = GetValue("event")
        Dim i As Integer = EventComboBox.FindStringExact(eventname)
        If i >= 0 Then
            EventComboBox.SelectedIndex = i
        End If
    End Sub

    Private Sub LogicTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles LogicTextBox.Leave
        Try
            MsgBox("leave")
            If LogicTextBox.Visible = True Then
                Dim logic As String = Replace(LogicTextBox.Text, vbCrLf, "[cr]")
                APSIMData.Child("logic").Value = logic
                Me.Refresh()
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating met file name information")
        End Try
    End Sub

    Private Sub EventComboBox_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EventComboBox.SelectedIndexChanged

    End Sub

    Private Sub EventComboBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles EventComboBox.Leave
        Try
            If EventComboBox.Visible = True Then
                APSIMData.Child("event").Value = EventComboBox.Text
                Me.Refresh()
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating met file name information")
        End Try
    End Sub


    Private Sub LogicTextBox_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles LogicTextBox.LostFocus
        MsgBox("lost focus")
    End Sub

    Private Sub RuleUI_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class
