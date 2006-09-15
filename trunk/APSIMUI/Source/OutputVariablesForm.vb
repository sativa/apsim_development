Imports vbgeneral
Public Class OutputVariablesForm
    Inherits System.Windows.Forms.Form

    Private mVariableName As String
    Private Controller As BaseController

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()
        InitializeComponent()
    End Sub

    Public Sub New(ByVal DataTreeController As VBGeneral.BaseController, ByVal treeType As OutputVariablesDataTree.TreeTypeEnum)
        MyBase.New()
        Controller = DataTreeController
        VariableDataTree.TreeType = treeType
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
    Friend WithEvents btnCancel As System.Windows.Forms.Button
    Friend WithEvents GlobalCheckBox As System.Windows.Forms.CheckBox
    Friend WithEvents VariableDataTree As APSIMUI.OutputVariablesDataTree
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.btnCancel = New System.Windows.Forms.Button
        Me.GlobalCheckBox = New System.Windows.Forms.CheckBox
        Me.VariableDataTree = New APSIMUI.OutputVariablesDataTree
        Me.SuspendLayout()
        '
        'btnCancel
        '
        Me.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.btnCancel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.btnCancel.Location = New System.Drawing.Point(0, 234)
        Me.btnCancel.Name = "btnCancel"
        Me.btnCancel.Size = New System.Drawing.Size(176, 32)
        Me.btnCancel.TabIndex = 3
        Me.btnCancel.Text = "Cancel"
        '
        'GlobalCheckBox
        '
        Me.GlobalCheckBox.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.GlobalCheckBox.Location = New System.Drawing.Point(0, 210)
        Me.GlobalCheckBox.Name = "GlobalCheckBox"
        Me.GlobalCheckBox.Size = New System.Drawing.Size(176, 24)
        Me.GlobalCheckBox.TabIndex = 2
        Me.GlobalCheckBox.Text = "Is Global?"
        '
        'VariableDataTree
        '
        Me.VariableDataTree.AutoScroll = True
        Me.VariableDataTree.BackColor = System.Drawing.SystemColors.Control
        Me.VariableDataTree.Dock = System.Windows.Forms.DockStyle.Fill
        Me.VariableDataTree.HelpText = ""
        Me.VariableDataTree.Location = New System.Drawing.Point(0, 0)
        Me.VariableDataTree.Name = "VariableDataTree"
        Me.VariableDataTree.Size = New System.Drawing.Size(176, 210)
        Me.VariableDataTree.TabIndex = 1
        Me.VariableDataTree.TreeType = APSIMUI.OutputVariablesDataTree.TreeTypeEnum.Variables
        '
        'OutputVariablesForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(176, 266)
        Me.ControlBox = False
        Me.Controls.Add(Me.VariableDataTree)
        Me.Controls.Add(Me.GlobalCheckBox)
        Me.Controls.Add(Me.btnCancel)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "OutputVariablesForm"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public ReadOnly Property VariableName() As String
        Get
            Return Me.mVariableName
        End Get

    End Property
    Private Sub OutputVariablesForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.VariableDataTree.RefreshView(Controller)

    End Sub

    Private Sub VariableDataTree_DataTreeDoubleClick(ByVal node As VBGeneral.APSIMData) Handles VariableDataTree.DataTreeDoubleClick
        ' Return only the node name if user has marked it as global.  Otherwise
        ' get the node's grandparent name and return it as well.

        If Me.GlobalCheckBox.CheckState = CheckState.Checked Then
            Me.mVariableName = node.Name

        Else
            Me.mVariableName = node.Parent.Parent.Name() & "." & node.Name

        End If

        Me.DialogResult = Windows.Forms.DialogResult.OK

    End Sub

    Private Sub VariableDataTree_TreeKeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles VariableDataTree.TreeKeyPress

        If Asc(e.KeyChar) = System.Windows.Forms.Keys.Escape Then
            Me.btnCancel.PerformClick()
        End If
    End Sub
End Class
