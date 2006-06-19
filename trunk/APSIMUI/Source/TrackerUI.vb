Public Class TrackerUI
    Inherits VBGeneral.BaseView

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()
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
    Friend WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Friend WithEvents AddButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents DeleteButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents EditButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents SmallDialogImages As System.Windows.Forms.ImageList
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(TrackerUI))
        Me.TrackerListBox = New System.Windows.Forms.ListBox
        Me.SmallDialogImages = New System.Windows.Forms.ImageList(Me.components)
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip
        Me.AddButton = New System.Windows.Forms.ToolStripButton
        Me.DeleteButton = New System.Windows.Forms.ToolStripButton
        Me.EditButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TrackerListBox
        '
        Me.TrackerListBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TrackerListBox.Location = New System.Drawing.Point(0, 87)
        Me.TrackerListBox.Name = "TrackerListBox"
        Me.TrackerListBox.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
        Me.TrackerListBox.Size = New System.Drawing.Size(843, 498)
        Me.TrackerListBox.TabIndex = 0
        '
        'SmallDialogImages
        '
        Me.SmallDialogImages.ImageStream = CType(resources.GetObject("SmallDialogImages.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.SmallDialogImages.TransparentColor = System.Drawing.Color.Transparent
        Me.SmallDialogImages.Images.SetKeyName(0, "")
        Me.SmallDialogImages.Images.SetKeyName(1, "")
        Me.SmallDialogImages.Images.SetKeyName(2, "")
        Me.SmallDialogImages.Images.SetKeyName(3, "")
        Me.SmallDialogImages.Images.SetKeyName(4, "")
        Me.SmallDialogImages.Images.SetKeyName(5, "")
        '
        'ToolStrip1
        '
        Me.ToolStrip1.ImageScalingSize = New System.Drawing.Size(24, 24)
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AddButton, Me.DeleteButton, Me.EditButton})
        Me.ToolStrip1.Location = New System.Drawing.Point(0, 40)
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.Size = New System.Drawing.Size(843, 47)
        Me.ToolStrip1.TabIndex = 2
        Me.ToolStrip1.Text = "ToolStrip1"
        '
        'AddButton
        '
        Me.AddButton.Image = Global.APSIMUI.My.Resources.Resources.add2
        Me.AddButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.AddButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.AddButton.Name = "AddButton"
        Me.AddButton.Size = New System.Drawing.Size(34, 44)
        Me.AddButton.Text = "&Add"
        Me.AddButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.AddButton.ToolTipText = "Add a tracker variable"
        '
        'DeleteButton
        '
        Me.DeleteButton.Image = Global.APSIMUI.My.Resources.Resources.delete2
        Me.DeleteButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.DeleteButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.DeleteButton.Name = "DeleteButton"
        Me.DeleteButton.Size = New System.Drawing.Size(48, 44)
        Me.DeleteButton.Text = "&Delete"
        Me.DeleteButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'EditButton
        '
        Me.EditButton.Image = Global.APSIMUI.My.Resources.Resources.edit
        Me.EditButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.EditButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.EditButton.Name = "EditButton"
        Me.EditButton.Size = New System.Drawing.Size(33, 44)
        Me.EditButton.Text = "&Edit"
        Me.EditButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        '
        'TrackerUI
        '
        Me.Controls.Add(Me.TrackerListBox)
        Me.Controls.Add(Me.ToolStrip1)
        Me.Name = "TrackerUI"
        Me.Size = New System.Drawing.Size(843, 589)
        Me.Controls.SetChildIndex(Me.ToolStrip1, 0)
        Me.Controls.SetChildIndex(Me.TrackerListBox, 0)
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region

    Public Overrides Sub Refresh()
        PopulateVariableList()

    End Sub

    Private Sub PopulateVariableList()
        ' ---------------------------------------------------
        'Retrieves the current list of user defined variables
        ' ---------------------------------------------------
        Me.TrackerListBox.Items.Clear()

        For Each child As VBGeneral.APSIMData In Me.Controller.Data.Children
            If child.Type.ToLower = "variable" Then
                Me.TrackerListBox.Items.Add(child.Value)
            End If
        Next
    End Sub

    Private Sub AddButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddButton.Click
        ' ---------------------------------------------------
        ' User has clicked the add button
        ' ---------------------------------------------------
        Dim VariableForm As New TrackerVariableForm(Controller)
        If VariableForm.ShowDialog() = DialogResult.OK Then
            Dim Variable As String = VariableForm.VariableTemplateOK()
            Me.TrackerListBox.Items.Add(Variable)
        End If
    End Sub

    Private Sub TrackerListBox_KeyUp(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TrackerListBox.KeyUp
        ' ---------------------------------------------------
        ' User has clicked a key - go do something.
        ' ---------------------------------------------------
        If Not e.KeyData = Keys.Delete Then Exit Sub

        Dim prompt As String = "Do you wish to deleted the selected variables?"

        If MessageBox.Show(prompt, "Deleting Selected Variables", MessageBoxButtons.YesNo, MessageBoxIcon.Information, MessageBoxDefaultButton.Button2) = DialogResult.Yes Then
            For Each strItem As String In Me.TrackerListBox.SelectedItems
                MessageBox.Show(strItem)

            Next
        End If
    End Sub

    Public Overrides Sub Save()
        ' ------------------------------------------------------
        ' UI Form is about to shut down - save all variables.
        ' ------------------------------------------------------
        Me.Controller.Data.Clear()

        For Each strVariable As String In Me.TrackerListBox.Items
            Dim apData As New VBGeneral.APSIMData("variable", "")
            apData.Value = strVariable

            Me.Controller.Data.Add(apData)

        Next

    End Sub





End Class
