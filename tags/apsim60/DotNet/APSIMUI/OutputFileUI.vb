Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized

Public Class OutputFileUI
    Inherits BaseUI


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
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents FileNameTextBox As System.Windows.Forms.TextBox
    Friend WithEvents BrowseButton As System.Windows.Forms.Button
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents FrequencyListBox As System.Windows.Forms.ListBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents VariablesListBox As System.Windows.Forms.ListBox
    Friend WithEvents AddButton As System.Windows.Forms.Button
    Friend WithEvents RemoveButton As System.Windows.Forms.Button
    Friend WithEvents Label3 As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(OutputFileUI))
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.Label1 = New System.Windows.Forms.Label
        Me.FileNameTextBox = New System.Windows.Forms.TextBox
        Me.BrowseButton = New System.Windows.Forms.Button
        Me.FrequencyListBox = New System.Windows.Forms.ListBox
        Me.Label2 = New System.Windows.Forms.Label
        Me.VariablesListBox = New System.Windows.Forms.ListBox
        Me.AddButton = New System.Windows.Forms.Button
        Me.RemoveButton = New System.Windows.Forms.Button
        Me.Label3 = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.CheckFileExists = False
        Me.OpenFileDialog.DefaultExt = "out"
        Me.OpenFileDialog.Filter = "APSIM output files(*.out)|*.out|All Files (*.*)|*.*"
        Me.OpenFileDialog.Title = "Enter output file name"
        '
        'Label1
        '
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(189, 2)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(72, 24)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Filename:"
        '
        'FileNameTextBox
        '
        Me.FileNameTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.FileNameTextBox.AutoSize = False
        Me.FileNameTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.FileNameTextBox.Location = New System.Drawing.Point(256, 0)
        Me.FileNameTextBox.Name = "FileNameTextBox"
        Me.FileNameTextBox.Size = New System.Drawing.Size(704, 25)
        Me.FileNameTextBox.TabIndex = 1
        Me.FileNameTextBox.Text = ""
        '
        'BrowseButton
        '
        Me.BrowseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.BrowseButton.Image = CType(resources.GetObject("BrowseButton.Image"), System.Drawing.Image)
        Me.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.BottomLeft
        Me.BrowseButton.Location = New System.Drawing.Point(962, 1)
        Me.BrowseButton.Name = "BrowseButton"
        Me.BrowseButton.Size = New System.Drawing.Size(96, 24)
        Me.BrowseButton.TabIndex = 2
        Me.BrowseButton.Text = "Browse"
        Me.BrowseButton.TextAlign = System.Drawing.ContentAlignment.BottomCenter
        '
        'FrequencyListBox
        '
        Me.FrequencyListBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.FrequencyListBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.FrequencyListBox.Items.AddRange(New Object() {"End_of_Day", "End_of_Week", "End_of_Month", "End_of_Year"})
        Me.FrequencyListBox.Location = New System.Drawing.Point(626, 48)
        Me.FrequencyListBox.Name = "FrequencyListBox"
        Me.FrequencyListBox.Size = New System.Drawing.Size(438, 613)
        Me.FrequencyListBox.TabIndex = 3
        '
        'Label2
        '
        Me.Label2.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label2.Location = New System.Drawing.Point(625, 28)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(120, 16)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "Reporting Frequency"
        '
        'VariablesListBox
        '
        Me.VariablesListBox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.VariablesListBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.VariablesListBox.Location = New System.Drawing.Point(0, 48)
        Me.VariablesListBox.Name = "VariablesListBox"
        Me.VariablesListBox.Size = New System.Drawing.Size(624, 613)
        Me.VariablesListBox.TabIndex = 5
        '
        'AddButton
        '
        Me.AddButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.AddButton.Image = CType(resources.GetObject("AddButton.Image"), System.Drawing.Image)
        Me.AddButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.AddButton.Location = New System.Drawing.Point(0, 0)
        Me.AddButton.Name = "AddButton"
        Me.AddButton.Size = New System.Drawing.Size(96, 24)
        Me.AddButton.TabIndex = 6
        Me.AddButton.Text = "Add New... "
        Me.AddButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'RemoveButton
        '
        Me.RemoveButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.RemoveButton.Image = CType(resources.GetObject("RemoveButton.Image"), System.Drawing.Image)
        Me.RemoveButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.RemoveButton.Location = New System.Drawing.Point(95, 0)
        Me.RemoveButton.Name = "RemoveButton"
        Me.RemoveButton.Size = New System.Drawing.Size(88, 24)
        Me.RemoveButton.TabIndex = 7
        Me.RemoveButton.Text = "Remove"
        Me.RemoveButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label3
        '
        Me.Label3.Location = New System.Drawing.Point(0, 29)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(120, 16)
        Me.Label3.TabIndex = 8
        Me.Label3.Text = "Reporting Variables"
        '
        'OutputFileUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1072, 672)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.RemoveButton)
        Me.Controls.Add(Me.AddButton)
        Me.Controls.Add(Me.VariablesListBox)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.FrequencyListBox)
        Me.Controls.Add(Me.BrowseButton)
        Me.Controls.Add(Me.FileNameTextBox)
        Me.Controls.Add(Me.Label1)
        Me.Name = "OutputFileUI"
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub refresh()
        MyBase.Refresh()
        Dim filename As String = GetValue("filename")
        FileNameTextBox.Text = filename
        OpenFileDialog.InitialDirectory = Path.GetDirectoryName(filename)

        Dim frequency As String = GetValue("frequency")
        If FrequencyListBox.Items.IndexOf(frequency) <> -1 Then
            FrequencyListBox.SelectedIndex = FrequencyListBox.Items.IndexOf(frequency)
        Else
            FrequencyListBox.SelectedIndex = 0
        End If
        Dim variables As New StringCollection
        variables = APSIMData.ChildList("variable")
        Dim variable As String
        For Each variable In variables
            VariablesListBox.Items.Add(variable)
        Next


    End Sub
    Private Sub BrowseButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BrowseButton.Click
        If OpenFileDialog.ShowDialog() = DialogResult.OK Then
            ' Do something here
            FileNameTextBox.Text = OpenFileDialog.FileName

        Else

        End If
    End Sub

    Private Sub OutputFileUI_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
    Public Overrides Sub SaveToAPSIMFile()
        SetValue("frequency", FrequencyListBox.SelectedItem)
        SetValue("filename", FileNameTextBox.Text)
    End Sub

    Private Sub Label2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label2.Click

    End Sub

    Private Sub RemoveButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RemoveButton.Click
        Try
            If VariablesListBox.SelectedIndex >= 0 Then
                VariablesListBox.Items.Remove(VariablesListBox.SelectedItem)
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error removing Output Variable from list")
        End Try
    End Sub

    Private Sub AddButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddButton.Click
        Try
            Dim variablename As String = InputBox("Enter Variable Name", "Add reporting variable")
            If variablename <> "" Then
                VariablesListBox.Items.Add(variablename)
            Else
                ' User cancelled
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error Adding New Reporting Variable")
        End Try

    End Sub
End Class
