Imports VBGeneral
Imports System.Collections.Specialized
Public Class EventsListView
    Inherits BaseDataControl

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
    Friend WithEvents ListView As System.Windows.Forms.ListView
    Friend WithEvents NameColumn As System.Windows.Forms.ColumnHeader
    Friend WithEvents DescriptionColumn As System.Windows.Forms.ColumnHeader
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.ListView = New System.Windows.Forms.ListView
        Me.NameColumn = New System.Windows.Forms.ColumnHeader
        Me.DescriptionColumn = New System.Windows.Forms.ColumnHeader
        Me.SuspendLayout()
        '
        'ListView
        '
        Me.ListView.AllowDrop = True
        Me.ListView.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.ListView.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.NameColumn, Me.DescriptionColumn})
        Me.ListView.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ListView.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable
        Me.ListView.Location = New System.Drawing.Point(0, 23)
        Me.ListView.Name = "ListView"
        Me.ListView.Size = New System.Drawing.Size(648, 313)
        Me.ListView.TabIndex = 0
        Me.ListView.View = System.Windows.Forms.View.Details
        '
        'NameColumn
        '
        Me.NameColumn.Text = "Name"
        Me.NameColumn.Width = 135
        '
        'DescriptionColumn
        '
        Me.DescriptionColumn.Text = "Description"
        Me.DescriptionColumn.Width = 366
        '
        'EventsListView
        '
        Me.Controls.Add(Me.ListView)
        Me.Name = "EventsListView"
        Me.Size = New System.Drawing.Size(648, 336)
        Me.Controls.SetChildIndex(Me.ListView, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub fill()
        CaptionLabel.Text = "Events (output frequency)"
        ListView.Items.Clear()
        Dim EventsNode As APSIMData = MyData.Child("events")
        For Each child As String In EventsNode.ChildList("event")
            Dim item As New ListViewItem
            item.Text = EventsNode.Child(child).Attribute("name")
            item.SubItems.Add(EventsNode.Child(child).Attribute("description"))
            ListView.Items.Add(item)
        Next
    End Sub

    Private Sub ListView_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles ListView.DragEnter
        If AllowDrop = True Then
            e.Effect = DragDropEffects.Copy
        Else
            e.Effect = DragDropEffects.None
        End If

    End Sub

    Private Sub ListView_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles ListView.DragDrop
        Dim NewDataString As String = e.Data.GetData(DataFormats.Text)
        Dim NewData As New APSIMData(NewDataString)
        If NewData.Type = "event" Then
            NewData.SetAttribute("eventname", NewData.Attribute("name"))
            NewData.SetAttribute("name", NewData.Attribute("module") + "." + NewData.Attribute("name"))
            Dim EventsNode As APSIMData = MyData.Child("events")
            EventsNode.Add(NewData)
            fill()
        Else
            MsgBox("You can only add variables to the output variables list.", MsgBoxStyle.Critical, "Error")
        End If
    End Sub


    Private Sub ListView_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles ListView.KeyDown
        If e.KeyValue = 46 Then
            For Each item As ListViewItem In ListView.SelectedItems
                Dim EventsNode As APSIMData = MyData.Child("events")
                EventsNode.Delete(item.Text)
                ListView.Items.Remove(item)
            Next

        End If
    End Sub
End Class
