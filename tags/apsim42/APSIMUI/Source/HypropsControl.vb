Imports VBGeneral

Public Class HypropsControl
    Inherits VBGeneral.BaseDataControl
    Private Hypropsdata As New Hyprops
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
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents PropertyGrid As System.Windows.Forms.PropertyGrid
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents MCPage As System.Windows.Forms.TabPage
    Friend WithEvents HKPage As System.Windows.Forms.TabPage

    Friend WithEvents DataTree As VBGeneral.DataTree
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.DataTree = New VBGeneral.DataTree
        Me.PropertyGrid = New System.Windows.Forms.PropertyGrid
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.MCPage = New System.Windows.Forms.TabPage
        Me.HKPage = New System.Windows.Forms.TabPage

        Me.Panel1.SuspendLayout()
        Me.TabControl.SuspendLayout()
        Me.MCPage.SuspendLayout()
        Me.HKPage.SuspendLayout()
        Me.SuspendLayout()
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.PropertyGrid)
        Me.Panel1.Controls.Add(Me.DataTree)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Left
        Me.Panel1.Location = New System.Drawing.Point(0, 0)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(320, 520)
        Me.Panel1.TabIndex = 1
        '
        'DataTree
        '
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Top
        Me.DataTree.LabelEdit = False
        Me.DataTree.Location = New System.Drawing.Point(0, 0)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(320, 248)
        Me.DataTree.TabIndex = 0
        '
        'PropertyGrid
        '
        Me.PropertyGrid.CommandsVisibleIfAvailable = True
        Me.PropertyGrid.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PropertyGrid.LargeButtons = False
        Me.PropertyGrid.LineColor = System.Drawing.SystemColors.ScrollBar
        Me.PropertyGrid.Location = New System.Drawing.Point(0, 248)
        Me.PropertyGrid.Name = "PropertyGrid"
        Me.PropertyGrid.Size = New System.Drawing.Size(320, 272)
        Me.PropertyGrid.TabIndex = 1
        Me.PropertyGrid.Text = "PropertyGrid1"
        Me.PropertyGrid.ViewBackColor = System.Drawing.SystemColors.Window
        Me.PropertyGrid.ViewForeColor = System.Drawing.SystemColors.WindowText
        '
        'TabControl
        '
        Me.TabControl.Controls.Add(Me.MCPage)
        Me.TabControl.Controls.Add(Me.HKPage)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl.Location = New System.Drawing.Point(320, 0)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(456, 520)
        Me.TabControl.TabIndex = 2
        '
        'MCPage
        '

        Me.MCPage.Location = New System.Drawing.Point(4, 22)
        Me.MCPage.Name = "MCPage"
        Me.MCPage.Size = New System.Drawing.Size(448, 494)
        Me.MCPage.TabIndex = 0
        Me.MCPage.Text = "Moisture Characteristic"
        '
        'HKPage
        '
        Me.HKPage.Location = New System.Drawing.Point(4, 22)
        Me.HKPage.Name = "HKPage"
        Me.HKPage.Size = New System.Drawing.Size(448, 494)
        Me.HKPage.TabIndex = 1
        Me.HKPage.Text = "Hydraulic Conductivity"
        '

        '
        'HypropsControl
        '
        Me.Controls.Add(Me.TabControl)
        Me.Controls.Add(Me.Panel1)
        Me.Name = "HypropsControl"
        Me.Size = New System.Drawing.Size(776, 520)
        Me.Panel1.ResumeLayout(False)
        Me.TabControl.ResumeLayout(False)
        Me.MCPage.ResumeLayout(False)
        Me.HKPage.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region


    Public Overrides Sub Refresh()
        MyBase.Refresh()
        Hypropsdata.Data = Data.Child("hyprops")
        DataTree.Data = Data
        PropertyGrid.SelectedObject = Hypropsdata
    End Sub

    Private Class Hyprops
        Private APSIMData As APSIMData
        WriteOnly Property Data() As APSIMData
            Set(ByVal Value As APSIMData)
                APSIMData = Value

                If APSIMData.Type <> "hyprops" Then
                    Throw New System.Exception("Hyprops class not provided with correct data type")
                End If
            End Set
        End Property
        Property Ks() As String
            Get
                Return APSIMData.Child("ks").Value
            End Get
            Set(ByVal Value As String)
                APSIMData.Child("ks").Value = Value
            End Set
        End Property
        Property sat() As String
            Get
                Return APSIMData.Child("sat").Value
            End Get
            Set(ByVal Value As String)
                APSIMData.Child("sat").Value = Value
            End Set
        End Property
        Property dul() As String
            Get
                Return APSIMData.Child("dul").Value
            End Get
            Set(ByVal Value As String)
                APSIMData.Child("dul").Value = Value
            End Set
        End Property
        Property ll15() As String
            Get
                Return APSIMData.Child("ll15").Value
            End Get
            Set(ByVal Value As String)
                APSIMData.Child("ll15").Value = Value
            End Set
        End Property
    End Class
End Class
