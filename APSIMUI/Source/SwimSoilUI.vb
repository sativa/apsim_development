Imports VBGeneral
Public Class SwimSoilUI
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
    Friend WithEvents HypropsTab As System.Windows.Forms.TabPage
    Friend WithEvents NumericalTab As System.Windows.Forms.TabPage
    Friend WithEvents SoluteTab As System.Windows.Forms.TabPage
    Friend WithEvents NodesTab As System.Windows.Forms.TabPage
    Friend WithEvents SBCTab As System.Windows.Forms.TabPage
    Friend WithEvents BBCTab As System.Windows.Forms.TabPage
    Friend WithEvents RunoffTab As System.Windows.Forms.TabPage
    Friend WithEvents BypassTab As System.Windows.Forms.TabPage
    Friend WithEvents DrainTab As System.Windows.Forms.TabPage
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents PropertyGrid1 As System.Windows.Forms.PropertyGrid
    Friend WithEvents HypropsControl As APSIMUI.HypropsControl
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(SwimSoilUI))
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.NodesTab = New System.Windows.Forms.TabPage
        Me.HypropsTab = New System.Windows.Forms.TabPage
        Me.HypropsControl = New APSIMUI.HypropsControl
        Me.NumericalTab = New System.Windows.Forms.TabPage
        Me.SoluteTab = New System.Windows.Forms.TabPage
        Me.SBCTab = New System.Windows.Forms.TabPage
        Me.BBCTab = New System.Windows.Forms.TabPage
        Me.RunoffTab = New System.Windows.Forms.TabPage
        Me.BypassTab = New System.Windows.Forms.TabPage
        Me.DrainTab = New System.Windows.Forms.TabPage
        Me.PropertyGrid1 = New System.Windows.Forms.PropertyGrid
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.Label1 = New System.Windows.Forms.Label
        Me.TabControl.SuspendLayout()
        Me.HypropsTab.SuspendLayout()
        Me.DrainTab.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl
        '
        Me.TabControl.Controls.Add(Me.NodesTab)
        Me.TabControl.Controls.Add(Me.HypropsTab)
        Me.TabControl.Controls.Add(Me.NumericalTab)
        Me.TabControl.Controls.Add(Me.SoluteTab)
        Me.TabControl.Controls.Add(Me.SBCTab)
        Me.TabControl.Controls.Add(Me.BBCTab)
        Me.TabControl.Controls.Add(Me.RunoffTab)
        Me.TabControl.Controls.Add(Me.BypassTab)
        Me.TabControl.Controls.Add(Me.DrainTab)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl.Location = New System.Drawing.Point(0, 0)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(1018, 611)
        Me.TabControl.TabIndex = 0
        '
        'NodesTab
        '
        Me.NodesTab.Location = New System.Drawing.Point(4, 22)
        Me.NodesTab.Name = "NodesTab"
        Me.NodesTab.Size = New System.Drawing.Size(1010, 585)
        Me.NodesTab.TabIndex = 3
        Me.NodesTab.Text = "Nodes"
        '
        'HypropsTab
        '
        Me.HypropsTab.Controls.Add(Me.HypropsControl)
        Me.HypropsTab.Location = New System.Drawing.Point(4, 22)
        Me.HypropsTab.Name = "HypropsTab"
        Me.HypropsTab.Size = New System.Drawing.Size(1010, 585)
        Me.HypropsTab.TabIndex = 0
        Me.HypropsTab.Text = "Hyprops"
        '
        'HypropsControl
        '
        Me.HypropsControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.HypropsControl.Location = New System.Drawing.Point(0, 0)
        Me.HypropsControl.Name = "HypropsControl"
        Me.HypropsControl.Size = New System.Drawing.Size(1010, 585)
        Me.HypropsControl.TabIndex = 0
        '
        'NumericalTab
        '
        Me.NumericalTab.Location = New System.Drawing.Point(4, 22)
        Me.NumericalTab.Name = "NumericalTab"
        Me.NumericalTab.Size = New System.Drawing.Size(1010, 585)
        Me.NumericalTab.TabIndex = 1
        Me.NumericalTab.Text = "Numerical"
        '
        'SoluteTab
        '
        Me.SoluteTab.Location = New System.Drawing.Point(4, 22)
        Me.SoluteTab.Name = "SoluteTab"
        Me.SoluteTab.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.SoluteTab.Size = New System.Drawing.Size(1010, 585)
        Me.SoluteTab.TabIndex = 2
        Me.SoluteTab.Text = "Solutes"
        '
        'SBCTab
        '
        Me.SBCTab.Location = New System.Drawing.Point(4, 22)
        Me.SBCTab.Name = "SBCTab"
        Me.SBCTab.Size = New System.Drawing.Size(1010, 585)
        Me.SBCTab.TabIndex = 4
        Me.SBCTab.Text = "Suface"
        '
        'BBCTab
        '
        Me.BBCTab.Location = New System.Drawing.Point(4, 22)
        Me.BBCTab.Name = "BBCTab"
        Me.BBCTab.Size = New System.Drawing.Size(1010, 585)
        Me.BBCTab.TabIndex = 5
        Me.BBCTab.Text = "Bottom Boundary"
        '
        'RunoffTab
        '
        Me.RunoffTab.Location = New System.Drawing.Point(4, 22)
        Me.RunoffTab.Name = "RunoffTab"
        Me.RunoffTab.Size = New System.Drawing.Size(1010, 585)
        Me.RunoffTab.TabIndex = 6
        Me.RunoffTab.Text = "Runoff"
        '
        'BypassTab
        '
        Me.BypassTab.Location = New System.Drawing.Point(4, 22)
        Me.BypassTab.Name = "BypassTab"
        Me.BypassTab.Size = New System.Drawing.Size(1010, 585)
        Me.BypassTab.TabIndex = 7
        Me.BypassTab.Text = "Bypass Flow"
        '
        'DrainTab
        '
        Me.DrainTab.Controls.Add(Me.PropertyGrid1)
        Me.DrainTab.Controls.Add(Me.Panel1)
        Me.DrainTab.Location = New System.Drawing.Point(4, 22)
        Me.DrainTab.Name = "DrainTab"
        Me.DrainTab.Size = New System.Drawing.Size(1010, 585)
        Me.DrainTab.TabIndex = 8
        Me.DrainTab.Text = "Subsurface Drain"
        '
        'PropertyGrid1
        '
        Me.PropertyGrid1.CommandsVisibleIfAvailable = True
        Me.PropertyGrid1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PropertyGrid1.HelpVisible = False
        Me.PropertyGrid1.LargeButtons = False
        Me.PropertyGrid1.LineColor = System.Drawing.SystemColors.ScrollBar
        Me.PropertyGrid1.Location = New System.Drawing.Point(504, 0)
        Me.PropertyGrid1.Name = "PropertyGrid1"
        Me.PropertyGrid1.Size = New System.Drawing.Size(506, 585)
        Me.PropertyGrid1.TabIndex = 1
        Me.PropertyGrid1.Text = "PropertyGrid1"
        Me.PropertyGrid1.ToolbarVisible = False
        Me.PropertyGrid1.ViewBackColor = System.Drawing.SystemColors.Window
        Me.PropertyGrid1.ViewForeColor = System.Drawing.SystemColors.WindowText
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.PictureBox1)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Left
        Me.Panel1.Location = New System.Drawing.Point(0, 0)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(504, 585)
        Me.Panel1.TabIndex = 0
        '
        'PictureBox1
        '
        Me.PictureBox1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(0, 0)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(504, 401)
        Me.PictureBox1.TabIndex = 1
        Me.PictureBox1.TabStop = False
        '
        'Label1
        '
        Me.Label1.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Label1.Location = New System.Drawing.Point(0, 401)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(504, 184)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Label1"
        '
        'SwimSoilUI
        '
        Me.ClientSize = New System.Drawing.Size(1018, 611)
        Me.Controls.Add(Me.TabControl)
        Me.Name = "SwimSoilUI"
        Me.TabControl.ResumeLayout(False)
        Me.HypropsTab.ResumeLayout(False)
        Me.DrainTab.ResumeLayout(False)
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub RefreshView(ByVal Controller As BaseController)
        MyBase.RefreshView(Controller)
        HypropsControl.RefreshView(Controller)
    End Sub



End Class
