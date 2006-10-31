Public Class DocumentationUI
    Inherits BaseUI

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        Me.WindowState = FormWindowState.Maximized

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
    Friend WithEvents Browser As AxSHDocVw.AxWebBrowser
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(DocumentationUI))
        Me.Browser = New AxSHDocVw.AxWebBrowser
        CType(Me.Browser, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Browser
        '
        Me.Browser.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Browser.Enabled = True
        Me.Browser.Location = New System.Drawing.Point(0, 0)
        Me.Browser.OcxState = CType(resources.GetObject("Browser.OcxState"), System.Windows.Forms.AxHost.State)
        Me.Browser.Size = New System.Drawing.Size(656, 568)
        Me.Browser.TabIndex = 0
        '
        'DocumentationUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(656, 568)
        Me.Controls.Add(Me.Browser)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.Name = "DocumentationUI"
        CType(Me.Browser, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overrides Property AutoScroll() As Boolean
        Get

        End Get
        Set(ByVal Value As Boolean)

        End Set
    End Property

    Private Sub DocumentationUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Browser.Navigate("file:\\c:\temp\test.html")
        'Me.WindowState = FormWindowState.Maximized

    End Sub

    Private Sub DocumentationUI_SizeChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.SizeChanged
        'MsgBox("I resized")
    End Sub
End Class
