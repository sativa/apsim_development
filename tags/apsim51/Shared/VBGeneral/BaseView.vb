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
Public Class BaseView
    Inherits System.Windows.Forms.UserControl
    Private MyController As BaseController



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
        If Not IsNothing(MyController) Then
            RemoveHandler MyController.NewDataEvent, AddressOf OnNewData
        End If
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

    ' ------------------------------------------------
    ' Called to setup the global application object
    ' ------------------------------------------------
    Overridable Property Controller() As BaseController
        Get
            Return MyController
        End Get
        Set(ByVal Value As BaseController)
            HelpText = ""
            If Not Value Is Nothing Then
                If MyController Is Nothing OrElse Not MyController Is Value OrElse Not MyController.Data Is Value.Data Then
                    MyController = Value
                    If Not MyController Is Nothing Then
                        AddHandler MyController.NewDataEvent, AddressOf OnNewData
                        If Not MyController.Data Is Nothing Then
                            Refresh()
                        End If
                    End If
                End If
            End If
        End Set
    End Property


    Private Sub OnNewData()
        If Parent Is Nothing Then
            RemoveHandler MyController.NewDataEvent, AddressOf OnNewData
        Else
            Refresh()
        End If
    End Sub

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
    Public Property HelpText() As String
        Get
            Return MyHelpLabel.Text
        End Get
        Set(ByVal Value As String)
            MyHelpLabel.Text = Value
            MyHelpLabel.Visible = Value <> ""
        End Set
    End Property


End Class
