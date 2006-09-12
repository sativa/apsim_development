Imports VBGeneral
Imports System.Collections
Imports System.Collections.Specialized
Imports System.IO
Imports FarPoint.Win.Spread

Public Class RuleUI
    Inherits VBGeneral.BaseView
    Dim InRefresh As Boolean
    Friend WithEvents GenericUI As VBGeneral.GenericUI
    Dim Cultivars As APSIMData


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
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.TabControl1 = New System.Windows.Forms.TabControl
        Me.TabPage1 = New System.Windows.Forms.TabPage
        Me.GenericUI = New VBGeneral.GenericUI
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(0, 40)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(1022, 776)
        Me.TabControl1.TabIndex = 3
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.GenericUI)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Size = New System.Drawing.Size(1014, 750)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Properties"
        '
        'GenericUI
        '
        Me.GenericUI.AutoScroll = True
        Me.GenericUI.BackColor = System.Drawing.SystemColors.Control
        Me.GenericUI.Controller = Nothing
        Me.GenericUI.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GenericUI.HelpText = ""
        Me.GenericUI.Location = New System.Drawing.Point(0, 0)
        Me.GenericUI.Name = "GenericUI"
        Me.GenericUI.Size = New System.Drawing.Size(1014, 750)
        Me.GenericUI.TabIndex = 0
        '
        'RuleUI
        '
        Me.Controls.Add(Me.TabControl1)
        Me.Name = "RuleUI"
        Me.Size = New System.Drawing.Size(1022, 816)
        Me.Controls.SetChildIndex(Me.TabControl1, 0)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    ' -----------------------------------
    ' Refresh the UI
    ' -----------------------------------
    Overrides Sub refresh()
        MyBase.Refresh()

        InRefresh = True

        ' Fill the property grid.
        GenericUI.Controller = Controller
        GenericUI.Refresh()

        ' Create a tab for each condition.
        While TabControl1.TabPages.Count > 1
            TabControl1.TabPages.RemoveAt(1)
        End While
        For Each Condition As APSIMData In Controller.Data.Children("condition")
            Dim page As New TabPage(Condition.Name)
            Dim ScriptBox As New RichTextBox
            ScriptBox.Text = Condition.Value
            ScriptBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            page.Controls.Add(ScriptBox)
            ScriptBox.Dock = DockStyle.Fill
            TabControl1.TabPages.Add(page)
        Next

        InRefresh = False
    End Sub


    Overrides Sub Save()
        ' --------------------------------------
        ' Save the script box if it has changd.
        ' --------------------------------------
        GenericUI.Save()

        Dim index As Integer = 1
        For Each Condition As APSIMData In Controller.Data.Children("condition")
            Dim page As TabPage = TabControl1.TabPages.Item(index)
            Dim ScriptBox As RichTextBox = page.Controls.Item(0)
            Condition.Value = ScriptBox.Text
            index = index + 1
        Next
    End Sub

End Class
