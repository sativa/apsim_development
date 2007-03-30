Imports System.Xml
Imports VBGeneral.ApsimSettings
Imports VBGeneral
Imports VBUserInterface

Public Class TclUI
    Inherits BaseView

    Overrides Sub RefreshView(ByVal tController As BaseController)
        MyBase.RefreshView(tController)
        'AxTclControl1.TraceVar("GlobalXMLDoc", TRACE_READS + GLOBAL_ONLY)
        'AxTclControl1.SetVar("GlobalXMLDoc", Me.Controller.AllData.XML(), 1)
        AxTclControl1.SetVar("XMLDoc", Me.Controller.Data.XML(), 1)
        AxTclControl1.SetVar("apsuite", ApsimDirectory(), 1)

        Dim UIScript As String = Me.Controller.Data.ChildValue("uiscript")

        If (AxTclControl1.Eval(UIScript) = False) Then
            MessageBox.Show(AxTclControl1.Result, "Tcl Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If
        AxTclControl1.Focus()
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
        Me.AxTclControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.AxTclControl1.Enabled = True
        Me.AxTclControl1.Location = New System.Drawing.Point(0, 40)
        Me.AxTclControl1.Name = "AxTclControl1"
        Me.AxTclControl1.OcxState = CType(resources.GetObject("AxTclControl1.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxTclControl1.Size = New System.Drawing.Size(0, 0)
        Me.AxTclControl1.TabIndex = 2
        '
        'TclUI
        '
        Me.AutoSize = True
        Me.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink
        Me.Controls.Add(Me.AxTclControl1)
        Me.Name = "TclUI"
        Me.Size = New System.Drawing.Size(0, 40)
        Me.Controls.SetChildIndex(Me.AxTclControl1, 0)
        CType(Me.AxTclControl1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region
    Public Overrides Sub OnClose()
        'Dim script As String = Me.Controller.Data.ChildValue("uiCloseScript")
        Dim script As String = "foreach w [winfo chi .] {destroy $w} ;# trace remove variable XMLDoc read setXML"
        If (script <> "") Then
            AxTclControl1.Eval(script)
        End If
    End Sub

    Public Overrides Sub Save()
        If Not IsNothing(Me.Controller.Data) Then
            Dim newXML As String
            newXML = AxTclControl1.GetVar("XMLDoc", 1)
            If (newXML <> "") Then
                ' Butt-ugly way of rewriting the whole inner xml document 
                Dim frag As String
                frag = newXML.Substring(newXML.IndexOf(">") + 1)
                frag = frag.Substring(frag.IndexOf("<"))
                frag = frag.Substring(0, frag.LastIndexOf("<"))

                Me.Controller.Data.InnerXML = frag
            End If
        End If
    End Sub

End Class
