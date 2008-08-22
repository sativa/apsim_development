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
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents CreateLink As System.Windows.Forms.LinkLabel
    Friend WithEvents AddLink As System.Windows.Forms.LinkLabel
    Friend WithEvents RemoveLink As System.Windows.Forms.LinkLabel
    Friend WithEvents ToolBoxListBox As System.Windows.Forms.ListBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.OKButton = New System.Windows.Forms.Button
        Me.OptionCancelButton = New System.Windows.Forms.Button
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.SaveFileDialog = New System.Windows.Forms.SaveFileDialog
        Me.Label1 = New System.Windows.Forms.Label
        Me.ToolBoxListBox = New System.Windows.Forms.ListBox
        Me.CreateLink = New System.Windows.Forms.LinkLabel
        Me.AddLink = New System.Windows.Forms.LinkLabel
        Me.RemoveLink = New System.Windows.Forms.LinkLabel
        Me.SuspendLayout()
        '
        'OKButton
        '
        Me.OKButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OKButton.Location = New System.Drawing.Point(338, 24)
        Me.OKButton.Name = "OKButton"
        Me.OKButton.Size = New System.Drawing.Size(80, 29)
        Me.OKButton.TabIndex = 1
        Me.OKButton.Text = "OK"
        '
        'OptionCancelButton
        '
        Me.OptionCancelButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OptionCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.OptionCancelButton.Location = New System.Drawing.Point(338, 64)
        Me.OptionCancelButton.Name = "OptionCancelButton"
        Me.OptionCancelButton.Size = New System.Drawing.Size(80, 29)
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
        Me.SaveFileDialog.Title = "Select a filename for your new toolbox"
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(16, 5)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(316, 35)
        Me.Label1.TabIndex = 8
        Me.Label1.Text = "The following toolbox files are used to provide user-defined data types within AP" & _
            "SIM at simulation design."
        '
        'ToolBoxListBox
        '
        Me.ToolBoxListBox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ToolBoxListBox.HorizontalScrollbar = True
        Me.ToolBoxListBox.Location = New System.Drawing.Point(19, 43)
        Me.ToolBoxListBox.Name = "ToolBoxListBox"
        Me.ToolBoxListBox.Size = New System.Drawing.Size(305, 160)
        Me.ToolBoxListBox.TabIndex = 5
        '
        'CreateLink
        '
        Me.CreateLink.AutoSize = True
        Me.CreateLink.Location = New System.Drawing.Point(16, 206)
        Me.CreateLink.Name = "CreateLink"
        Me.CreateLink.Size = New System.Drawing.Size(138, 13)
        Me.CreateLink.TabIndex = 10
        Me.CreateLink.TabStop = True
        Me.CreateLink.Text = "Create a new empty toolbox"
        '
        'AddLink
        '
        Me.AddLink.AutoSize = True
        Me.AddLink.Location = New System.Drawing.Point(16, 222)
        Me.AddLink.Name = "AddLink"
        Me.AddLink.Size = New System.Drawing.Size(170, 13)
        Me.AddLink.TabIndex = 11
        Me.AddLink.TabStop = True
        Me.AddLink.Text = "Add an existing toolbox to ApsimUI"
        '
        'RemoveLink
        '
        Me.RemoveLink.AutoSize = True
        Me.RemoveLink.Location = New System.Drawing.Point(16, 239)
        Me.RemoveLink.Name = "RemoveLink"
        Me.RemoveLink.Size = New System.Drawing.Size(192, 13)
        Me.RemoveLink.TabIndex = 12
        Me.RemoveLink.TabStop = True
        Me.RemoveLink.Text = "Remove selected toolbox from ApsimUI"
        '
        'OptionsForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.CancelButton = Me.OptionCancelButton
        Me.ClientSize = New System.Drawing.Size(423, 259)
        Me.Controls.Add(Me.RemoveLink)
        Me.Controls.Add(Me.AddLink)
        Me.Controls.Add(Me.CreateLink)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.ToolBoxListBox)
        Me.Controls.Add(Me.OptionCancelButton)
        Me.Controls.Add(Me.OKButton)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "OptionsForm"
        Me.Text = "APSIM Toolbox Management"
        Me.ResumeLayout(False)
        Me.PerformLayout()

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

    Private Sub OnCreateLink(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles CreateLink.LinkClicked
        ' ------------------------------------------------
        ' User is wanting to create a new toolbox.
        ' ------------------------------------------------
        If SaveFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Dim toolboxes As New Toolboxes
            toolboxes.CreateNew(SaveFileDialog.FileName)
            ToolBoxListBox.Items.Add(SaveFileDialog.FileName)
        End If
    End Sub

    Private Sub OnAddLink(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles AddLink.LinkClicked
        ' --------------------------------------
        ' user has clicked add existing button
        ' --------------------------------------
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

    Private Sub OnRemoveLink(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles RemoveLink.LinkClicked
        ' -----------------------------------
        ' User has clicked remove button
        ' -----------------------------------
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
