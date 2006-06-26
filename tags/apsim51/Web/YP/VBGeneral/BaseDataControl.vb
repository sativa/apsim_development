Imports VBGeneral

' ---------------------------------
' Base class for all data controls
' It has data and knowledge of a
' user interface.
' All derived data controls should override the
' 'Refresh' method. 
' ---------------------------------
Public Class BaseDataControl
    Inherits System.Windows.Forms.UserControl
    Private MyData As APSIMData
    Protected MyApplicationSettings As ApplicationSettings

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'UserControl overrides dispose to clean up the component list.
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
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.MyCaptionLabel = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'MyCaptionLabel
        '
        Me.MyCaptionLabel.AccessibleRole = System.Windows.Forms.AccessibleRole.None
        Me.MyCaptionLabel.BackColor = System.Drawing.SystemColors.ControlDark
        Me.MyCaptionLabel.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.MyCaptionLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.MyCaptionLabel.Font = New System.Drawing.Font("Tahoma", 7.8!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.MyCaptionLabel.ForeColor = System.Drawing.SystemColors.HighlightText
        Me.MyCaptionLabel.Location = New System.Drawing.Point(0, 0)
        Me.MyCaptionLabel.Name = "MyCaptionLabel"
        Me.MyCaptionLabel.Size = New System.Drawing.Size(808, 20)
        Me.MyCaptionLabel.TabIndex = 0
        Me.MyCaptionLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'BaseDataControl
        '
        Me.Controls.Add(Me.MyCaptionLabel)
        Me.Name = "BaseDataControl"
        Me.Size = New System.Drawing.Size(808, 528)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' -------------------------------------------
    ' Property allowing setting of caption label
    ' -------------------------------------------
    ReadOnly Property CaptionLabel() As System.Windows.Forms.Label
        Get
            Return Me.MyCaptionLabel
        End Get
    End Property


    ' -------------------------------------------
    ' Property allowing setting of data. Whenever
    ' this happens, refresh is called allowing
    ' derived classes to refresh their displays.
    ' -------------------------------------------
    Property Data() As APSIMData
        Set(ByVal Value As APSIMData)
            Try
                MyData = Value
                Refresh()
            Catch e As System.Exception
                MsgBox(e.Message, MsgBoxStyle.Critical, "Error")
            End Try
        End Set
        Get
            Return MyData
        End Get
    End Property


    ' -------------------------------------------
    ' Property allowing setting of uimanager. 
    ' -------------------------------------------
    Property ApplicationSettings() As ApplicationSettings
        Set(ByVal Value As ApplicationSettings)
            MyApplicationSettings = Value
            Refresh()
        End Set
        Get
            Return MyApplicationSettings
        End Get
    End Property


End Class
