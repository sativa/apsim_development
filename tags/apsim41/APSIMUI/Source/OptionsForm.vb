Imports System.Collections
Imports System.Collections.Specialized
Imports VBGeneral
Public Class OptionsForm
    Inherits System.Windows.Forms.Form

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
    Friend WithEvents OKButton As System.Windows.Forms.Button
    Friend WithEvents ToolBoxesPage As System.Windows.Forms.TabPage
    Friend WithEvents OtherPage As System.Windows.Forms.TabPage
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ToolBoxListBox As System.Windows.Forms.ListBox
    Friend WithEvents AddButton As System.Windows.Forms.Button
    Friend WithEvents RemoveButton As System.Windows.Forms.Button
    Friend WithEvents OptionCancelButton As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents NewButton As System.Windows.Forms.Button
    Friend WithEvents SaveFileDialog As System.Windows.Forms.SaveFileDialog
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.TabControl1 = New System.Windows.Forms.TabControl
        Me.ToolBoxesPage = New System.Windows.Forms.TabPage
        Me.NewButton = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.RemoveButton = New System.Windows.Forms.Button
        Me.AddButton = New System.Windows.Forms.Button
        Me.ToolBoxListBox = New System.Windows.Forms.ListBox
        Me.OtherPage = New System.Windows.Forms.TabPage
        Me.OKButton = New System.Windows.Forms.Button
        Me.OptionCancelButton = New System.Windows.Forms.Button
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.SaveFileDialog = New System.Windows.Forms.SaveFileDialog
        Me.TabControl1.SuspendLayout()
        Me.ToolBoxesPage.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.ToolBoxesPage)
        Me.TabControl1.Controls.Add(Me.OtherPage)
        Me.TabControl1.Location = New System.Drawing.Point(0, 0)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(470, 462)
        Me.TabControl1.TabIndex = 0
        '
        'ToolBoxesPage
        '
        Me.ToolBoxesPage.Controls.Add(Me.NewButton)
        Me.ToolBoxesPage.Controls.Add(Me.Label1)
        Me.ToolBoxesPage.Controls.Add(Me.RemoveButton)
        Me.ToolBoxesPage.Controls.Add(Me.AddButton)
        Me.ToolBoxesPage.Controls.Add(Me.ToolBoxListBox)
        Me.ToolBoxesPage.Location = New System.Drawing.Point(4, 25)
        Me.ToolBoxesPage.Name = "ToolBoxesPage"
        Me.ToolBoxesPage.Size = New System.Drawing.Size(462, 433)
        Me.ToolBoxesPage.TabIndex = 0
        Me.ToolBoxesPage.Text = "ToolBoxes"
        '
        'NewButton
        '
        Me.NewButton.Location = New System.Drawing.Point(24, 368)
        Me.NewButton.Name = "NewButton"
        Me.NewButton.Size = New System.Drawing.Size(90, 27)
        Me.NewButton.TabIndex = 4
        Me.NewButton.Text = "New ..."
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(29, 9)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(403, 56)
        Me.Label1.TabIndex = 3
        Me.Label1.Text = "The following toolbox files are used to provide user-defined data types within AP" & _
        "SIM at simulation design."
        '
        'RemoveButton
        '
        Me.RemoveButton.Location = New System.Drawing.Point(304, 368)
        Me.RemoveButton.Name = "RemoveButton"
        Me.RemoveButton.Size = New System.Drawing.Size(90, 27)
        Me.RemoveButton.TabIndex = 2
        Me.RemoveButton.Text = "Remove"
        '
        'AddButton
        '
        Me.AddButton.Location = New System.Drawing.Point(144, 368)
        Me.AddButton.Name = "AddButton"
        Me.AddButton.Size = New System.Drawing.Size(128, 27)
        Me.AddButton.TabIndex = 1
        Me.AddButton.Text = "Add existing ..."
        '
        'ToolBoxListBox
        '
        Me.ToolBoxListBox.ItemHeight = 16
        Me.ToolBoxListBox.Location = New System.Drawing.Point(19, 74)
        Me.ToolBoxListBox.Name = "ToolBoxListBox"
        Me.ToolBoxListBox.Size = New System.Drawing.Size(423, 260)
        Me.ToolBoxListBox.TabIndex = 0
        '
        'OtherPage
        '
        Me.OtherPage.Location = New System.Drawing.Point(4, 25)
        Me.OtherPage.Name = "OtherPage"
        Me.OtherPage.Size = New System.Drawing.Size(462, 433)
        Me.OtherPage.TabIndex = 1
        Me.OtherPage.Text = "Other"
        '
        'OKButton
        '
        Me.OKButton.Location = New System.Drawing.Point(480, 28)
        Me.OKButton.Name = "OKButton"
        Me.OKButton.Size = New System.Drawing.Size(108, 34)
        Me.OKButton.TabIndex = 1
        Me.OKButton.Text = "OK"
        '
        'OptionCancelButton
        '
        Me.OptionCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.OptionCancelButton.Location = New System.Drawing.Point(480, 74)
        Me.OptionCancelButton.Name = "OptionCancelButton"
        Me.OptionCancelButton.Size = New System.Drawing.Size(108, 34)
        Me.OptionCancelButton.TabIndex = 2
        Me.OptionCancelButton.Text = "Cancel"
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.DefaultExt = "*.xml"
        Me.OpenFileDialog.Filter = "toolbox files (*.xml; *.soils)|*.xml;*.soils|All Files (*.*)|*.*"
        '
        'SaveFileDialog
        '
        Me.SaveFileDialog.DefaultExt = "xml"
        Me.SaveFileDialog.Filter = "toolbox files (*.xml)|*.xml|All Files (*.*)|*.*"
        '
        'OptionsForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(6, 15)
        Me.CancelButton = Me.OptionCancelButton
        Me.ClientSize = New System.Drawing.Size(592, 456)
        Me.Controls.Add(Me.OptionCancelButton)
        Me.Controls.Add(Me.OKButton)
        Me.Controls.Add(Me.TabControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "OptionsForm"
        Me.Text = "APSIM Options"
        Me.TabControl1.ResumeLayout(False)
        Me.ToolBoxesPage.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region


    ' ------------------------------------------------
    ' Form has just been shown - load all controls
    ' ------------------------------------------------
    Private Sub OptionsForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        LoadToolBoxes()
    End Sub


    ' -------------------
    ' OK button clicked
    ' -------------------
    Private Sub OKButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OKButton.Click
        SaveToolBoxSettings()
        Me.Close()
    End Sub


    ' -------------------------------------------------
    ' User is closing the form - don't save anything
    ' -------------------------------------------------
    Private Sub CancelButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OptionCancelButton.Click
        Me.Close()
    End Sub


    ' ------------------------------------------------
    ' User is wanting to create a new toolbox.
    ' ------------------------------------------------
    Private Sub NewButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NewButton.Click
        If SaveFileDialog.ShowDialog() = DialogResult.OK Then
            Dim toolboxes As New Toolboxes
            toolboxes.CreateNew(SaveFileDialog.FileName)
            ToolBoxListBox.Items.Add(SaveFileDialog.FileName)
        End If
    End Sub


    ' --------------------------------------
    ' user has clicked add existing button
    ' --------------------------------------
    Private Sub AddButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddButton.Click
        Try
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                ToolBoxListBox.Items.Add(OpenFileDialog.FileName)
            Else
                ' User cancelled file open operation
            End If
        Catch ex as system.exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error Selecting new Toolbox file")
        End Try

    End Sub


    ' -----------------------------------
    ' User has clicked remove button
    ' -----------------------------------
    Private Sub RemoveButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RemoveButton.Click
        Try
            If ToolBoxListBox.SelectedIndex >= 0 Then
                ToolBoxListBox.Items.Remove(ToolBoxListBox.SelectedItem)
            End If
        Catch ex as system.exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error removing Toolbox from list")
        End Try
    End Sub


    ' ----------------------------------------------
    ' Load all toolbox names into control
    ' ----------------------------------------------
    Private Sub LoadToolBoxes()
        Try
            Dim toolboxes As New Toolboxes
            For Each FileName As String In toolboxes.Filenames
                ToolBoxListBox.Items.Add(FileName)
            Next
        Catch e as system.exception
            MsgBox(e.Message, MsgBoxStyle.Critical, "Error building tool box List")
        End Try
    End Sub


    ' ---------------------------------------
    ' Save all toolbox settings
    ' ---------------------------------------
    Private Sub SaveToolBoxSettings()
        Dim Filenames As New StringCollection

        For Each item As String In ToolBoxListBox.Items
            Filenames.Add(item)
        Next
        Dim toolboxes As New Toolboxes
        toolboxes.Filenames = Filenames
    End Sub


End Class
