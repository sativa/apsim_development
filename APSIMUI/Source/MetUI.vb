Imports System
Imports System.IO

Public Class MetUI
    Inherits VBGeneral.BaseView

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()
        Xceed.Chart.Licenser.LicenseKey = "CHT30-YTL57-0UXLJ-145A"
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
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents MetGraphControl1 As APSIMUI.MetGraphControl
    Friend WithEvents BrowseToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents BrowseButton As System.Windows.Forms.ToolBarButton
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MetUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.MetGraphControl1 = New APSIMUI.MetGraphControl
        Me.BrowseToolBar = New System.Windows.Forms.ToolBar
        Me.BrowseButton = New System.Windows.Forms.ToolBarButton
        Me.SuspendLayout()
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(24, 24)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'MetGraphControl1
        '
        Me.MetGraphControl1.Controller = Nothing
        Me.MetGraphControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.MetGraphControl1.Location = New System.Drawing.Point(0, 40)
        Me.MetGraphControl1.Name = "MetGraphControl1"
        Me.MetGraphControl1.Size = New System.Drawing.Size(726, 693)
        Me.MetGraphControl1.TabIndex = 8
        '
        'BrowseToolBar
        '
        Me.BrowseToolBar.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.BrowseToolBar.AutoSize = False
        Me.BrowseToolBar.Buttons.AddRange(New System.Windows.Forms.ToolBarButton() {Me.BrowseButton})
        Me.BrowseToolBar.ButtonSize = New System.Drawing.Size(65, 26)
        Me.BrowseToolBar.Divider = False
        Me.BrowseToolBar.Dock = System.Windows.Forms.DockStyle.None
        Me.BrowseToolBar.DropDownArrows = True
        Me.BrowseToolBar.ImageList = Me.ImageList
        Me.BrowseToolBar.Location = New System.Drawing.Point(643, 3)
        Me.BrowseToolBar.Name = "BrowseToolBar"
        Me.BrowseToolBar.ShowToolTips = True
        Me.BrowseToolBar.Size = New System.Drawing.Size(69, 29)
        Me.BrowseToolBar.TabIndex = 12
        Me.BrowseToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        Me.BrowseToolBar.Wrappable = False
        '
        'BrowseButton
        '
        Me.BrowseButton.ImageIndex = 0
        Me.BrowseButton.Text = "Browse"
        '
        'MetUI
        '
        Me.Controls.Add(Me.BrowseToolBar)
        Me.Controls.Add(Me.MetGraphControl1)
        Me.Name = "MetUI"
        Me.Size = New System.Drawing.Size(726, 733)
        Me.Controls.SetChildIndex(Me.MetGraphControl1, 0)
        Me.Controls.SetChildIndex(Me.BrowseToolBar, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        MyBase.Refresh()
        Dim FileName As String = Controller.Data.ChildValueWithError("filename")
        MetGraphControl1.Controller = Controller
        HelpText = FileName
        OpenFileDialog.InitialDirectory = Path.GetDirectoryName(FileName)
        MetGraphControl1.SetFileName(FileName)
    End Sub

    Private Sub BrowseToolBar_ButtonClick(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolBarButtonClickEventArgs) Handles BrowseToolBar.ButtonClick
        If OpenFileDialog.ShowDialog() = DialogResult.OK Then
            MetGraphControl1.SetFileName(OpenFileDialog.FileName)
        End If
    End Sub
End Class
