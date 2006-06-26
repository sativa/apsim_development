Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports VBGeneral
Imports CSGeneral

Public Class OutputVariablesDataTree
    Inherits BaseView

    Public Enum TreeTypeEnum
        Variables = 0
        Events = 1

    End Enum

    Private mTreeType As TreeTypeEnum

    Public Event DataTreeDoubleClick(ByVal path As VBGeneral.APSIMData)
    Public Event TreeKeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)



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
    Friend WithEvents DataTree As VBGeneral.DataTree

    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.DataTree = New VBGeneral.DataTree
        Me.SuspendLayout()
        '
        'DataTree
        '
        Me.DataTree.AllowDrop = True
        Me.DataTree.AutoScroll = True
        Me.DataTree.BackColor = System.Drawing.SystemColors.Control
        Me.DataTree.Controller = Nothing
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Fill
        Me.DataTree.HelpText = ""
        Me.DataTree.Location = New System.Drawing.Point(0, 40)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(815, 781)
        Me.DataTree.Sorted = False
        Me.DataTree.TabIndex = 9
        '
        'OutputVariablesDataTree
        '
        Me.Controls.Add(Me.DataTree)
        Me.Name = "OutputVariablesDataTree"
        Me.Size = New System.Drawing.Size(815, 821)
        Me.Controls.SetChildIndex(Me.DataTree, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Property TreeType() As TreeTypeEnum
        Get
            Return mTreeType

        End Get

        Set(ByVal value As TreeTypeEnum)
            Me.mTreeType = value
        End Set

    End Property

    Private Function TreeTypeText() As String

        If TreeType = TreeTypeEnum.Events Then
            Return "event"

        Else
            Return "variable"
        End If

    End Function


    ' ----------------------------------
    ' Refresh the variable tree.
    ' ----------------------------------
    Overrides Sub refresh()

        MyBase.Refresh()

        ' Try
        DataTree.Sorted = True
        DataTree.ExpandAll = False
        DataTree.HelpText = "Variable and events"
        DataTree.Controller = New ApsimUIController("", "", "")
        DataTree.ShowAll = True
        DataTree.Controller.AllData = BuildDataTree()
        'Catch


        'End Try


    End Sub


    ' ------------------------------------
    ' Create an APSIMData for the 
    ' tree control
    ' ------------------------------------
    Private Function BuildDataTree() As APSIMData
        Dim VariablesXML As String = "<components/>"
        Dim UIManager As ApsimUIController = Controller

        Return New APSIMData(UIManager.GetOutputFileDescriptions(Controller.Data.Parent))

    End Function

    Private Sub DataTree_DoubleClickEvent() Handles DataTree.DoubleClickEvent

        If Me.DataTree.Controller.Data.Type = Me.TreeTypeText Then
            RaiseEvent DataTreeDoubleClick(Me.DataTree.Controller.Data)

        End If


    End Sub

    Private Sub DataTree_DataTreeKeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles DataTree.DataTreeKeyPress
        RaiseEvent TreeKeyPress(sender, e)
    End Sub
End Class
