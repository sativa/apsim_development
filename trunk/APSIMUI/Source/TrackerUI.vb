Public Class TrackerUI
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
    Friend WithEvents TrackerListBox As System.Windows.Forms.ListBox
    Friend WithEvents TrackerTextBox As System.Windows.Forms.TextBox
    Friend WithEvents ReplaceButton As System.Windows.Forms.Button
    Friend WithEvents RemoveButton As System.Windows.Forms.Button
    Friend WithEvents AddButton As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(TrackerUI))
        Me.TrackerListBox = New System.Windows.Forms.ListBox
        Me.TrackerTextBox = New System.Windows.Forms.TextBox
        Me.ReplaceButton = New System.Windows.Forms.Button
        Me.RemoveButton = New System.Windows.Forms.Button
        Me.AddButton = New System.Windows.Forms.Button
        Me.SuspendLayout()
        '
        'TrackerListBox
        '
        Me.TrackerListBox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TrackerListBox.ItemHeight = 16
        Me.TrackerListBox.Items.AddRange(New Object() {"variable = sum of rain on met.newmet from wheat.sowing to now as accumulated_rain" & _
        "", "variable = sum of rain on last 3 start_of_day as rain3", "variable = value of wheat.yield on wheat.harvesting as wheat_yield", "variable = sum of wheat.ep on end_of_day from wheat.sowing to wheat.harvesting as" & _
        " accumulated_wheat_ep", "variable = maximum of wheat.lai on end_of_day from wheat.sowing to wheat.harvesti" & _
        "ng as max_lai", "variable = average of wheat.swdef_photo on last 7 end_of_day from wheat.sowing to" & _
        " wheat.harvesting as wheat_weekly_stress"})
        Me.TrackerListBox.Location = New System.Drawing.Point(0, 72)
        Me.TrackerListBox.Name = "TrackerListBox"
        Me.TrackerListBox.Size = New System.Drawing.Size(931, 1316)
        Me.TrackerListBox.TabIndex = 0
        '
        'TrackerTextBox
        '
        Me.TrackerTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TrackerTextBox.Location = New System.Drawing.Point(336, 32)
        Me.TrackerTextBox.Name = "TrackerTextBox"
        Me.TrackerTextBox.Size = New System.Drawing.Size(595, 22)
        Me.TrackerTextBox.TabIndex = 1
        Me.TrackerTextBox.Text = ""
        '
        'ReplaceButton
        '
        Me.ReplaceButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ReplaceButton.Image = CType(resources.GetObject("ReplaceButton.Image"), System.Drawing.Image)
        Me.ReplaceButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.ReplaceButton.Location = New System.Drawing.Point(192, 32)
        Me.ReplaceButton.Name = "ReplaceButton"
        Me.ReplaceButton.Size = New System.Drawing.Size(106, 28)
        Me.ReplaceButton.TabIndex = 2
        Me.ReplaceButton.Text = "Replace"
        Me.ReplaceButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'RemoveButton
        '
        Me.RemoveButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.RemoveButton.Image = CType(resources.GetObject("RemoveButton.Image"), System.Drawing.Image)
        Me.RemoveButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.RemoveButton.Location = New System.Drawing.Point(96, 32)
        Me.RemoveButton.Name = "RemoveButton"
        Me.RemoveButton.Size = New System.Drawing.Size(96, 28)
        Me.RemoveButton.TabIndex = 3
        Me.RemoveButton.Text = "Remove"
        Me.RemoveButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'AddButton
        '
        Me.AddButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.AddButton.Image = CType(resources.GetObject("AddButton.Image"), System.Drawing.Image)
        Me.AddButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.AddButton.Location = New System.Drawing.Point(0, 32)
        Me.AddButton.Name = "AddButton"
        Me.AddButton.Size = New System.Drawing.Size(96, 28)
        Me.AddButton.TabIndex = 4
        Me.AddButton.Text = "Add..."
        Me.AddButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TrackerUI
        '
        Me.ClientSize = New System.Drawing.Size(940, 668)
        Me.Controls.Add(Me.AddButton)
        Me.Controls.Add(Me.RemoveButton)
        Me.Controls.Add(Me.ReplaceButton)
        Me.Controls.Add(Me.TrackerTextBox)
        Me.Controls.Add(Me.TrackerListBox)
        Me.Name = "TrackerUI"
        Me.Controls.SetChildIndex(Me.TrackerListBox, 0)
        Me.Controls.SetChildIndex(Me.TrackerTextBox, 0)
        Me.Controls.SetChildIndex(Me.ReplaceButton, 0)
        Me.Controls.SetChildIndex(Me.RemoveButton, 0)
        Me.Controls.SetChildIndex(Me.AddButton, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    
    Private Sub TrackerListBox_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackerListBox.SelectedIndexChanged
        TrackerTextBox.Text = TrackerListBox.Text
    End Sub
End Class
