Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports System.Xml
Public Class SimulationUI
    Inherits BaseUI
    'Inherits System.Windows.Forms.Form
    Private listview As Object
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
    Friend WithEvents MapTextBox As System.Windows.Forms.TextBox
    Friend WithEvents MapBrowseButton As System.Windows.Forms.Button
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(SimulationUI))
        Me.StartDatePicker = New System.Windows.Forms.DateTimePicker
        Me.EndDatePicker = New System.Windows.Forms.DateTimePicker
        Me.TitleTextBox = New System.Windows.Forms.TextBox
        Me.SummaryFileTextBox = New System.Windows.Forms.TextBox
        Me.BrowseButton = New System.Windows.Forms.Button
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.SmallImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.MapTextBox = New System.Windows.Forms.TextBox
        Me.MapBrowseButton = New System.Windows.Forms.Button
        Me.Label5 = New System.Windows.Forms.Label
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
        Me.TitleTextBox.Size = New System.Drawing.Size(600, 20)
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
        Me.SummaryFileTextBox.Size = New System.Drawing.Size(600, 20)
        Me.SummaryFileTextBox.TabIndex = 3
        Me.SummaryFileTextBox.Text = ""
        '
        'BrowseButton
        '
        Me.BrowseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.BrowseButton.ImageIndex = 0
        Me.BrowseButton.ImageList = Me.ImageList
        Me.BrowseButton.Location = New System.Drawing.Point(976, 40)
        Me.BrowseButton.Name = "BrowseButton"
        Me.BrowseButton.Size = New System.Drawing.Size(96, 24)
        Me.BrowseButton.TabIndex = 4
        Me.BrowseButton.Text = "Browse..."
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(285, 19)
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
        'MapTextBox
        '
        Me.MapTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.MapTextBox.Location = New System.Drawing.Point(355, 75)
        Me.MapTextBox.Name = "MapTextBox"
        Me.MapTextBox.Size = New System.Drawing.Size(600, 20)
        Me.MapTextBox.TabIndex = 10
        Me.MapTextBox.Text = ""
        '
        'MapBrowseButton
        '
        Me.MapBrowseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.MapBrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.MapBrowseButton.ImageIndex = 0
        Me.MapBrowseButton.ImageList = Me.ImageList
        Me.MapBrowseButton.Location = New System.Drawing.Point(976, 72)
        Me.MapBrowseButton.Name = "MapBrowseButton"
        Me.MapBrowseButton.Size = New System.Drawing.Size(98, 24)
        Me.MapBrowseButton.TabIndex = 11
        Me.MapBrowseButton.Text = "Browse..."
        '
        'Label5
        '
        Me.Label5.Location = New System.Drawing.Point(283, 75)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(48, 24)
        Me.Label5.TabIndex = 12
        Me.Label5.Text = "Map:"
        '
        'SimulationUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1095, 625)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.MapBrowseButton)
        Me.Controls.Add(Me.MapTextBox)
        Me.Controls.Add(Me.SummaryFileTextBox)
        Me.Controls.Add(Me.TitleTextBox)
        Me.Controls.Add(Me.EndDatePicker)
        Me.Controls.Add(Me.StartDatePicker)
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
        MapTextBox.Text = GetValue("map")


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

    Private Sub MapTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles MapTextBox.Leave
        Try
            Dim path As String = DataPath + "/" + "map"
            APSIMFile.SetValue(path, MapTextBox.Text)

            Me.Refresh()

        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in summary file information")
        End Try
    End Sub

    Private Sub SimulationUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim listview As New areaui

        With listview
            .Left = 10
            .Top = 100
            .Width = Me.Width - 20
            .Height = .Height - 110
            .Anchor = AnchorStyles.Bottom Or AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
            .APSIMFile = APSIMFile ' this must be done before you can set the path
            .DataPath = DataPath
            .UIManager = Me.UIManager
            .TopLevel = False
            .Parent = Me
            .Show()
        End With


    End Sub
End Class
