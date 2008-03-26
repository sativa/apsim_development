Imports System
Imports System.IO

' ----------------------------------
' Base class for all user interfaces
' It has data and knowledge of a
' user interface.
' All user interfaces should override the
' 'Refresh' method and optionally the 
' 'Save' method.
' ----------------------------------
Public Class BaseUI
    Inherits System.Windows.Forms.Form
    Private ParentExplorer As ExplorerUI
    Private MyData As APSIMData

#Region " Windows Form Designer generated code "
    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

        'Me.WindowState = FormWindowState.Maximized
        Me.Dock = Windows.Forms.DockStyle.Fill

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
    Friend WithEvents MyCaptionLabel As System.Windows.Forms.Label
    Friend WithEvents MyHelpLabel As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.MyCaptionLabel = New System.Windows.Forms.Label
        Me.MyHelpLabel = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'MyCaptionLabel
        '
        Me.MyCaptionLabel.BackColor = System.Drawing.SystemColors.ControlDark
        Me.MyCaptionLabel.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.MyCaptionLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.MyCaptionLabel.Font = New System.Drawing.Font("Tahoma", 7.8!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.MyCaptionLabel.ForeColor = System.Drawing.SystemColors.HighlightText
        Me.MyCaptionLabel.Location = New System.Drawing.Point(0, 0)
        Me.MyCaptionLabel.Name = "MyCaptionLabel"
        Me.MyCaptionLabel.Size = New System.Drawing.Size(655, 20)
        Me.MyCaptionLabel.TabIndex = 0
        Me.MyCaptionLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'MyHelpLabel
        '
        Me.MyHelpLabel.BackColor = System.Drawing.SystemColors.Info
        Me.MyHelpLabel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.MyHelpLabel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.MyHelpLabel.Font = New System.Drawing.Font("Tahoma", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.MyHelpLabel.ForeColor = System.Drawing.SystemColors.InfoText
        Me.MyHelpLabel.Location = New System.Drawing.Point(0, 506)
        Me.MyHelpLabel.Name = "MyHelpLabel"
        Me.MyHelpLabel.Size = New System.Drawing.Size(655, 35)
        Me.MyHelpLabel.TabIndex = 1
        '
        'BaseUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.AutoScroll = True
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(655, 541)
        Me.ControlBox = False
        Me.Controls.Add(Me.MyHelpLabel)
        Me.Controls.Add(Me.MyCaptionLabel)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "BaseUI"
        Me.ResumeLayout(False)

    End Sub
#End Region

    ' ------------------------------------------------
    ' Called by ExplorerUI to setup the ParentExplorer
    ' ------------------------------------------------
    Property Explorer() As ExplorerUI
        Get
            Return ParentExplorer
        End Get
        Set(ByVal Value As ExplorerUI)
            ParentExplorer = Value
        End Set
    End Property


    ' ------------------------------------------------
    ' Called by ExplorerUI to setup the ParentExplorer
    ' ------------------------------------------------
    Overridable Property Data() As APSIMData
        Get
            Return MyData
        End Get
        Set(ByVal Value As APSIMData)
            Try
                MyData = Value
                If Not IsNothing(MyData) Then
                    MyCaptionLabel.Text = MyData.Name + " properties"
                End If
                Refresh()
            Catch Err As System.Exception
                MsgBox(Err.Message, MsgBoxStyle.Critical, "Error")
            End Try
        End Set
    End Property


    ' ---------------------------------------------
    ' An overridable method that is called whenever
    ' data should be saved back to the APSIMData 
    ' instance.
    ' ---------------------------------------------
    Overridable Sub Save()
    End Sub


    ' ---------------------------------------------
    ' Provide access to the help label of this ui
    ' ---------------------------------------------
    Public ReadOnly Property HelpLabel() As Windows.Forms.Label
        Get
            Return MyHelpLabel
        End Get
    End Property

End Class
