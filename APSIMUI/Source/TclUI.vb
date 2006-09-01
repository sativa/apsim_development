Public Class TclUI
    Inherits VBGeneral.BaseView

    Overrides Sub Refresh()
        MyBase.Refresh()
        Dim UIFileName As String = Controller.Data.ChildValueWithError("uifilename")
        AxTclControl1.Eval("source " + UIFileName)
    End Sub

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
    Friend WithEvents AxTclControl1 As AxTCLCONTROLPRJ2Lib.AxTclControl

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(TclUI))
        Me.AxTclControl1 = New AxTCLCONTROLPRJ2Lib.AxTclControl
        CType(Me.AxTclControl1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'AxTclControl1
        '
        Me.AxTclControl1.Enabled = True
        Me.AxTclControl1.Location = New System.Drawing.Point(0, 43)
        Me.AxTclControl1.Name = "AxTclControl1"
        Me.AxTclControl1.OcxState = CType(resources.GetObject("AxTclControl1.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxTclControl1.Size = New System.Drawing.Size(933, 483)
        Me.AxTclControl1.TabIndex = 2
        '
        'TclUI
        '
        Me.Controls.Add(Me.AxTclControl1)
        Me.Name = "TclUI"
        Me.Size = New System.Drawing.Size(936, 526)
        Me.Controls.SetChildIndex(Me.AxTclControl1, 0)
        CType(Me.AxTclControl1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region


    Public Overrides Sub Save()
        AxTclControl1.Eval("Save")
    End Sub
End Class
