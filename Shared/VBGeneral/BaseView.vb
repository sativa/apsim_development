Imports System
Imports System.IO


Public Class BaseView
    ' ----------------------------------
    ' Base class for all user interfaces
    ' It has data and knowledge of a
    ' user interface.
    ' All user interfaces should override the
    ' 'Refresh' method and optionally the 
    ' 'Save' method.
    ' ----------------------------------   
    Inherits System.Windows.Forms.UserControl
    Protected Controller As BaseController

#Region " Windows Form Designer generated code "
    Public Sub New()
        MyBase.New()
        InitializeComponent()
    End Sub

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
    Friend WithEvents MyHelpLabel As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.MyHelpLabel = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'MyHelpLabel
        '
        Me.MyHelpLabel.BackColor = System.Drawing.SystemColors.Info
        Me.MyHelpLabel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.MyHelpLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.MyHelpLabel.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.MyHelpLabel.ForeColor = System.Drawing.SystemColors.Highlight
        Me.MyHelpLabel.Location = New System.Drawing.Point(0, 0)
        Me.MyHelpLabel.Name = "MyHelpLabel"
        Me.MyHelpLabel.Size = New System.Drawing.Size(655, 40)
        Me.MyHelpLabel.TabIndex = 1
        Me.MyHelpLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'BaseView
        '
        Me.AutoScroll = True
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.Controls.Add(Me.MyHelpLabel)
        Me.Name = "BaseView"
        Me.Size = New System.Drawing.Size(655, 541)
        Me.ResumeLayout(False)

    End Sub
#End Region

    Public Overridable Sub RefreshView(ByVal Controller As BaseController)
        ' ------------------------------------------------
        ' Called to setup the view object. The controller
        ' passed in is guarenteed to have data.
        ' ------------------------------------------------
        HelpText = ""
        Me.Controller = Controller
    End Sub

    Public Overridable Sub Save()
        ' ---------------------------------------------
        ' An overridable method that is called whenever
        ' data should be saved back to the APSIMData 
        ' instance.
        ' ---------------------------------------------
    End Sub

    Public Property HelpText() As String
        ' ---------------------------------------------
        ' Provide access to the help label of this ui
        ' ---------------------------------------------
        Get
            Return MyHelpLabel.Text
        End Get
        Set(ByVal Value As String)
            MyHelpLabel.Text = Value
            MyHelpLabel.Visible = Value <> ""
        End Set
    End Property

End Class
