Imports System
Imports System.IO
Imports General
Public Class BaseUI
    Inherits System.Windows.Forms.Form
    Protected APSIMData As APSIMData
    Private _UImanager As UIManager

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

        'Me.WindowState = FormWindowState.Maximized
        Me.Dock = DockStyle.Fill

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
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        '
        'BaseUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.AutoScroll = True
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(656, 542)
        Me.ControlBox = False
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "BaseUI"

    End Sub

#End Region
    Overridable Property Data() As APSIMData
        Get
            Return APSIMData
        End Get
        Set(ByVal Value As APSIMData)
            Try
                APSIMData = Value

                Refresh()
            Catch e As Exception
                MsgBox("Error in updating data ", MsgBoxStyle.Critical, "Error")
            End Try
        End Set
    End Property
    Property UIManager() As UIManager
        Get
            Return _UImanager
        End Get
        Set(ByVal Value As UIManager)
            Try
                _UImanager = Value
            Catch e As Exception
                MsgBox("Error determining UI manager", MsgBoxStyle.Critical, "Error")
            End Try
        End Set
    End Property

    Overrides Sub Refresh()
        MyBase.Refresh()
    End Sub
    Function GetValue(ByVal key As String) As String
        Try
            GetValue = APSIMData.Child(Trim(key)).Value
        Catch e As Exception
            MsgBox("Error in returning value for data name: " + Trim(key), MsgBoxStyle.Critical, "Error")
        End Try
    End Function
    Sub SetValue(ByVal key As String, ByVal value As String)
        Try
            APSIMData.Child(Trim(key)).Value = value
        Catch e As Exception
            MsgBox("Error in setting value for data name: " + Trim(key), MsgBoxStyle.Critical, "Error")
        End Try
    End Sub
    Function GetAttribute(ByVal Attribute As String) As String
        Try

            GetAttribute = APSIMData.Attribute(Attribute)

        Catch e As Exception
            MsgBox("Error in returning attribute for given attribute: " + Trim(Attribute), MsgBoxStyle.Critical, "Error")
        End Try
    End Function
    Overridable Sub setup()

    End Sub
    Overridable Sub SaveToAPSIMFile()
        'This subroutine is called by UIManager to force a UI to save its settings back into the
        'APSIMFile DOM.
    End Sub

End Class
