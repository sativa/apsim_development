Imports VBGeneral
Public Class LogicUI
    Inherits VBGeneral.BaseView
    Private CurrentScriptNode As APSIMData = Nothing

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
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents InitTab As System.Windows.Forms.TabPage
    Friend WithEvents PopupMenu As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents AddMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DeleteMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents ScriptBox As System.Windows.Forms.RichTextBox
    Friend WithEvents PropertiesMenuItem As System.Windows.Forms.ToolStripMenuItem
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.PopupMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.AddMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.DeleteMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator
        Me.PropertiesMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.InitTab = New System.Windows.Forms.TabPage
        Me.ScriptBox = New System.Windows.Forms.RichTextBox
        Me.TabControl.SuspendLayout()
        Me.PopupMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl
        '
        Me.TabControl.ContextMenuStrip = Me.PopupMenu
        Me.TabControl.Controls.Add(Me.InitTab)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Top
        Me.TabControl.Location = New System.Drawing.Point(0, 40)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(1020, 24)
        Me.TabControl.TabIndex = 0
        '
        'PopupMenu
        '
        Me.PopupMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AddMenuItem, Me.DeleteMenuItem, Me.ToolStripSeparator1, Me.PropertiesMenuItem})
        Me.PopupMenu.Name = "ContextMenuStrip"
        Me.PopupMenu.Size = New System.Drawing.Size(200, 98)
        '
        'AddMenuItem
        '
        Me.AddMenuItem.Name = "AddMenuItem"
        Me.AddMenuItem.Size = New System.Drawing.Size(199, 22)
        Me.AddMenuItem.Text = "&Add another script item"
        '
        'DeleteMenuItem
        '
        Me.DeleteMenuItem.Name = "DeleteMenuItem"
        Me.DeleteMenuItem.Size = New System.Drawing.Size(199, 22)
        Me.DeleteMenuItem.Text = "&Delete this script item"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(196, 6)
        '
        'PropertiesMenuItem
        '
        Me.PropertiesMenuItem.Name = "PropertiesMenuItem"
        Me.PropertiesMenuItem.Size = New System.Drawing.Size(199, 22)
        Me.PropertiesMenuItem.Text = "&Properties"
        '
        'InitTab
        '
        Me.InitTab.Location = New System.Drawing.Point(4, 22)
        Me.InitTab.Name = "InitTab"
        Me.InitTab.Size = New System.Drawing.Size(1012, 0)
        Me.InitTab.TabIndex = 0
        Me.InitTab.Text = "<default>"
        Me.InitTab.UseVisualStyleBackColor = True
        '
        'ScriptBox
        '
        Me.ScriptBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ScriptBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ScriptBox.Location = New System.Drawing.Point(0, 64)
        Me.ScriptBox.Name = "ScriptBox"
        Me.ScriptBox.Size = New System.Drawing.Size(1020, 528)
        Me.ScriptBox.TabIndex = 3
        Me.ScriptBox.Text = ""
        Me.ScriptBox.WordWrap = False
        '
        'LogicUI
        '
        Me.Controls.Add(Me.ScriptBox)
        Me.Controls.Add(Me.TabControl)
        Me.Name = "LogicUI"
        Me.Size = New System.Drawing.Size(1020, 592)
        Me.Controls.SetChildIndex(Me.TabControl, 0)
        Me.Controls.SetChildIndex(Me.ScriptBox, 0)
        Me.TabControl.ResumeLayout(False)
        Me.PopupMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub RefreshView(ByVal Controller As BaseController)
        MyBase.RefreshView(Controller)

        TabControl.TabPages.Clear()
        CurrentScriptNode = Nothing

        For Each Script As APSIMData In Controller.Data.Children
            Dim TabName As String = ""
            For Each EventData As APSIMData In Script.Children("event")
                If TabName <> "" Then
                    TabName = TabName + ","
                End If
                TabName = TabName + EventData.Value
            Next
            TabControl.TabPages.Add(TabName)
        Next
        HelpText = "Enter your management logic into the edit box below."
        TabControl_SelectedIndexChanged(Nothing, Nothing)
    End Sub

    Private Sub TabControl_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TabControl.SelectedIndexChanged
        ' save old script.
        Save()

        ' load new script
        If TabControl.SelectedIndex >= 0 Then
            CurrentScriptNode = Controller.Data.Children("script")(TabControl.SelectedIndex)
            ScriptBox.Text = CurrentScriptNode.ChildValue("text").Replace("[cr]", vbCrLf)
        End If
    End Sub

    Public Overrides Sub Save()
        If Not IsNothing(CurrentScriptNode) Then
            Dim text As String = Replace(ScriptBox.Text, vbCrLf, "[cr]")
            If text = Nothing Then
                text = ""
            ElseIf Not text.StartsWith(" ") Then
                text = " " + text
            End If
            CurrentScriptNode.ChildValue("text") = text
        End If
    End Sub

    Private Sub TabControl_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TabControl.MouseDown
        For i As Integer = 0 To TabControl.TabCount - 1
            If TabControl.GetTabRect(i).Contains(e.Location) Then
                TabControl.SelectTab(i)
            End If
        Next
        DeleteMenuItem.Enabled = TabControl.TabCount > 1
    End Sub

    Private Sub AddMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddMenuItem.Click
        Dim EventNamesString As String = InputDialog.InputBox("Enter event name(s) to run script on", "APSIM event names (comma separated)", "", False)
        If EventNamesString <> "" Then
            Dim EventNames() As String = EventNamesString.Split(",".ToCharArray())
            Dim NewScriptNode As APSIMData = Controller.Data.Add(New APSIMData("script", ""))
            For Each EventName As String In EventNames
                NewScriptNode.Add(New APSIMData("event", "")).Value = EventName
            Next
            TabControl.TabPages.Add(EventNamesString)
            TabControl.SelectedIndex = TabControl.TabCount - 1
        End If
    End Sub

    Private Sub DeleteMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DeleteMenuItem.Click
        Dim CurrentTabName As String = TabControl.TabPages(TabControl.SelectedIndex).Text
        If MessageBox.Show("Are you sure you want to delete " + CurrentTabName + "?", "Confirmation required", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then
            CurrentScriptNode.Parent.DeleteNode(CurrentScriptNode)
            CurrentScriptNode = Nothing
            TabControl.TabPages.Remove(TabControl.SelectedTab)
        End If
    End Sub

    Private Sub PropertiesMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PropertiesMenuItem.Click
        Dim EventNamesString As String = InputDialog.InputBox("Enter event name(s) to run script on", "APSIM event names (comma separated)", TabControl.SelectedTab.Text, False)
        If EventNamesString <> TabControl.SelectedTab.Text Then
            Dim EventNames() As String = EventNamesString.Split(",".ToCharArray())
            CurrentScriptNode.DeleteByType("event")
            For Each EventName As String In EventNames
                CurrentScriptNode.Add(New APSIMData("event", "")).Value = EventName
            Next
            TabControl.SelectedTab.Text = EventNamesString
        End If
    End Sub
End Class
