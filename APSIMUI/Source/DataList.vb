Imports System.Collections.Specialized
Imports General
Public Class DataList
    Inherits BaseDataControl

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
    Friend WithEvents ListBox As System.Windows.Forms.ListBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.ListBox = New System.Windows.Forms.ListBox
        Me.SuspendLayout()
        '
        'ListBox
        '
        Me.ListBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.ListBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ListBox.Location = New System.Drawing.Point(0, 0)
        Me.ListBox.Name = "ListBox"
        Me.ListBox.Size = New System.Drawing.Size(456, 288)
        Me.ListBox.TabIndex = 0
        '
        'DataList
        '
        Me.Controls.Add(Me.ListBox)
        Me.Name = "DataList"
        Me.Size = New System.Drawing.Size(456, 288)
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub fill()
        For Each child As String In MyData.ChildList
            ListBox.Items.Add(child)
        Next
    End Sub
End Class
