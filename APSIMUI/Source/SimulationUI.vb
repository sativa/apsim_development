Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports System.Xml
Public Class SimulationUI
    Inherits BaseUI
    'Inherits System.Windows.Forms.Form
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
    Friend WithEvents TitleTextBox As System.Windows.Forms.TextBox
    Friend WithEvents SummaryFileTextBox As System.Windows.Forms.TextBox
    Friend WithEvents BrowseButton As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents StartDatePicker As System.Windows.Forms.DateTimePicker
    Friend WithEvents EndDatePicker As System.Windows.Forms.DateTimePicker
    Friend WithEvents SmallImageList As System.Windows.Forms.ImageList
    Friend WithEvents ListView As System.Windows.Forms.ListView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(SimulationUI))
        Me.StartDatePicker = New System.Windows.Forms.DateTimePicker
        Me.EndDatePicker = New System.Windows.Forms.DateTimePicker
        Me.TitleTextBox = New System.Windows.Forms.TextBox
        Me.SummaryFileTextBox = New System.Windows.Forms.TextBox
        Me.BrowseButton = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.SmallImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.ListView = New System.Windows.Forms.ListView
        Me.SuspendLayout()
        '
        'StartDatePicker
        '
        Me.StartDatePicker.Location = New System.Drawing.Point(80, 10)
        Me.StartDatePicker.Name = "StartDatePicker"
        Me.StartDatePicker.Size = New System.Drawing.Size(190, 20)
        Me.StartDatePicker.TabIndex = 0
        '
        'EndDatePicker
        '
        Me.EndDatePicker.Location = New System.Drawing.Point(80, 40)
        Me.EndDatePicker.Name = "EndDatePicker"
        Me.EndDatePicker.Size = New System.Drawing.Size(190, 20)
        Me.EndDatePicker.TabIndex = 1
        '
        'TitleTextBox
        '
        Me.TitleTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TitleTextBox.AutoSize = False
        Me.TitleTextBox.Location = New System.Drawing.Point(355, 10)
        Me.TitleTextBox.Name = "TitleTextBox"
        Me.TitleTextBox.Size = New System.Drawing.Size(518, 20)
        Me.TitleTextBox.TabIndex = 2
        Me.TitleTextBox.Text = ""
        '
        'SummaryFileTextBox
        '
        Me.SummaryFileTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.SummaryFileTextBox.AutoSize = False
        Me.SummaryFileTextBox.Location = New System.Drawing.Point(355, 40)
        Me.SummaryFileTextBox.Name = "SummaryFileTextBox"
        Me.SummaryFileTextBox.Size = New System.Drawing.Size(518, 20)
        Me.SummaryFileTextBox.TabIndex = 3
        Me.SummaryFileTextBox.Text = ""
        '
        'BrowseButton
        '
        Me.BrowseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseButton.Location = New System.Drawing.Point(875, 40)
        Me.BrowseButton.Name = "BrowseButton"
        Me.BrowseButton.Size = New System.Drawing.Size(60, 20)
        Me.BrowseButton.TabIndex = 4
        Me.BrowseButton.Text = "Browse..."
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(291, 19)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(40, 24)
        Me.Label1.TabIndex = 5
        Me.Label1.Text = "Title:"
        '
        'Label2
        '
        Me.Label2.Location = New System.Drawing.Point(283, 48)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(80, 24)
        Me.Label2.TabIndex = 6
        Me.Label2.Text = "Summary File:"
        '
        'Label3
        '
        Me.Label3.Location = New System.Drawing.Point(0, 16)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(86, 20)
        Me.Label3.TabIndex = 7
        Me.Label3.Text = "Starting Date:"
        '
        'Label4
        '
        Me.Label4.Location = New System.Drawing.Point(0, 48)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(96, 20)
        Me.Label4.TabIndex = 8
        Me.Label4.Text = "Ending Date:"
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.CheckFileExists = False
        Me.OpenFileDialog.DefaultExt = "sum"
        Me.OpenFileDialog.Filter = "APSIM Summary Files (*.sum)|*.sum|All Files (*.*)|*.*"
        '
        'SmallImageList
        '
        Me.SmallImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.SmallImageList.ImageStream = CType(resources.GetObject("SmallImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.SmallImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'ListView
        '
        Me.ListView.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ListView.Location = New System.Drawing.Point(8, 96)
        Me.ListView.Name = "ListView"
        Me.ListView.Size = New System.Drawing.Size(920, 584)
        Me.ListView.SmallImageList = Me.SmallImageList
        Me.ListView.TabIndex = 9
        Me.ListView.View = System.Windows.Forms.View.SmallIcon
        '
        'SimulationUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(944, 686)
        Me.Controls.Add(Me.ListView)
        Me.Controls.Add(Me.EndDatePicker)
        Me.Controls.Add(Me.StartDatePicker)
        Me.Controls.Add(Me.SummaryFileTextBox)
        Me.Controls.Add(Me.TitleTextBox)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.BrowseButton)
        Me.Name = "SimulationUI"
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub refresh()
        MyBase.Refresh()
        Dim filename As String = GetValue("summaryfile")
        ' This may not be what we really want here
        'If Path.GetDirectoryName(filename) = "" Then
        'filename = APSIMFile.XMLFilePath + "\" + filename
        'End If
        SummaryFileTextBox.Text = filename
        OpenFileDialog.InitialDirectory = Path.GetDirectoryName(filename)
        StartDatePicker.Value = GetValue("start_date")
        EndDatePicker.Value = GetValue("end_date")
        TitleTextBox.Text = GetValue("title")

        ListView.Clear()
        Dim AreaList As New StringCollection
        APSIMFile.GetChildListByType(DataPath, "area", AreaList)
        Dim AreaName As String
        For Each AreaName In AreaList
            Dim item As New ListViewItem(AreaName, 0)
            ListView.Items.Add(item)
        Next
    End Sub
    Private Sub BrowseButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BrowseButton.Click

        Try
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                ' Do something here
                SummaryFileTextBox.Text = OpenFileDialog.FileName
                Dim path As String = DataPath + "/" + "summaryfile"
                APSIMFile.SetValue(path, SummaryFileTextBox.Text)
                Me.Refresh()
            Else
                ' User cancelled the action so do nothing
            End If

        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in summary file information")
        End Try

    End Sub

    Private Sub ListView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles ListView.DoubleClick
        Dim item As ListViewItem
        For Each item In ListView.SelectedItems
            Dim childdatapath As String = DataPath + "/" + item.Text
            UIManager.ShowUI(childdatapath)

        Next
    End Sub


    Private Sub StartDatePicker_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles StartDatePicker.Leave
        Try
            Dim path As String = DataPath + "/" + "start_date"
            APSIMFile.SetValue(path, StartDatePicker.Value.Date)
            Me.Refresh()

        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating simulation starting date information")
        End Try
    End Sub

    Private Sub EndDatePicker_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles EndDatePicker.Leave
        Try
            Dim path As String = DataPath + "/" + "end_date"
            APSIMFile.SetValue(path, EndDatePicker.Value.Date)
            Me.Refresh()

        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating simulation ending date information")
        End Try
    End Sub

    Private Sub TitleTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles TitleTextBox.Leave
        Try
            Dim path As String = DataPath + "/" + "title"
            APSIMFile.SetValue(path, TitleTextBox.Text)
            Me.Refresh()

        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating title information")
        End Try
    End Sub

    Private Sub SummaryFileTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles SummaryFileTextBox.Leave
        Try
            Dim path As String = DataPath + "/" + "summaryfile"
            APSIMFile.SetValue(path, SummaryFileTextBox.Text)
            Me.Refresh()

        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in summary file information")
        End Try
    End Sub
End Class
