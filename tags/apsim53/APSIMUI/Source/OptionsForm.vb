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
    Friend WithEvents OKButton As System.Windows.Forms.Button
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents OptionCancelButton As System.Windows.Forms.Button
    Friend WithEvents SaveFileDialog As System.Windows.Forms.SaveFileDialog
    Friend WithEvents NewButton As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents RemoveButton As System.Windows.Forms.Button
    Friend WithEvents AddButton As System.Windows.Forms.Button
    Friend WithEvents ToolBoxListBox As System.Windows.Forms.ListBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.OKButton = New System.Windows.Forms.Button
        Me.OptionCancelButton = New System.Windows.Forms.Button
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.SaveFileDialog = New System.Windows.Forms.SaveFileDialog
        Me.NewButton = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.RemoveButton = New System.Windows.Forms.Button
        Me.AddButton = New System.Windows.Forms.Button
        Me.ToolBoxListBox = New System.Windows.Forms.ListBox
        Me.SuspendLayout()
        '
        'OKButton
        '
        Me.OKButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OKButton.Location = New System.Drawing.Point(384, 24)
        Me.OKButton.Name = "OKButton"
        Me.OKButton.Size = New System.Drawing.Size(104, 30)
        Me.OKButton.TabIndex = 1
        Me.OKButton.Text = "OK"
        '
        'OptionCancelButton
        '
        Me.OptionCancelButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OptionCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.OptionCancelButton.Location = New System.Drawing.Point(384, 64)
        Me.OptionCancelButton.Name = "OptionCancelButton"
        Me.OptionCancelButton.Size = New System.Drawing.Size(104, 30)
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
        'NewButton
        '
        Me.NewButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.NewButton.Location = New System.Drawing.Point(384, 128)
        Me.NewButton.Name = "NewButton"
        Me.NewButton.Size = New System.Drawing.Size(104, 32)
        Me.NewButton.TabIndex = 9
        Me.NewButton.Text = "New ..."
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(24, 8)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(336, 48)
        Me.Label1.TabIndex = 8
        Me.Label1.Text = "The following toolbox files are used to provide user-defined data types within AP" & _
        "SIM at simulation design."
        '
        'RemoveButton
        '
        Me.RemoveButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.RemoveButton.Location = New System.Drawing.Point(384, 208)
        Me.RemoveButton.Name = "RemoveButton"
        Me.RemoveButton.Size = New System.Drawing.Size(104, 32)
        Me.RemoveButton.TabIndex = 7
        Me.RemoveButton.Text = "Remove"
        '
        'AddButton
        '
        Me.AddButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.AddButton.Location = New System.Drawing.Point(384, 168)
        Me.AddButton.Name = "AddButton"
        Me.AddButton.Size = New System.Drawing.Size(104, 32)
        Me.AddButton.TabIndex = 6
        Me.AddButton.Text = "Add existing ..."
        '
        'ToolBoxListBox
        '
        Me.ToolBoxListBox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ToolBoxListBox.Location = New System.Drawing.Point(16, 64)
        Me.ToolBoxListBox.Name = "ToolBoxListBox"
        Me.ToolBoxListBox.Size = New System.Drawing.Size(352, 212)
        Me.ToolBoxListBox.TabIndex = 5
        '
        'OptionsForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.CancelButton = Me.OptionCancelButton
        Me.ClientSize = New System.Drawing.Size(493, 288)
        Me.Controls.Add(Me.NewButton)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.RemoveButton)
        Me.Controls.Add(Me.AddButton)
        Me.Controls.Add(Me.ToolBoxListBox)
        Me.Controls.Add(Me.OptionCancelButton)
        Me.Controls.Add(Me.OKButton)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "OptionsForm"
        Me.Text = "APSIM Options"
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
        If SaveFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
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
            If OpenFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
                ToolBoxListBox.Items.Add(OpenFileDialog.FileName)
            Else
                ' User cancelled file open operation
            End If
        Catch ex As System.Exception
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
        Catch ex As System.Exception
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
        Catch e As System.Exception
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
