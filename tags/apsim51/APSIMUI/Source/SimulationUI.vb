Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports System.Xml
Public Class SimulationUI
    Inherits VBGeneral.BaseView
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
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents StartDatePicker As System.Windows.Forms.DateTimePicker
    Friend WithEvents EndDatePicker As System.Windows.Forms.DateTimePicker
    Friend WithEvents SmallImageList As System.Windows.Forms.ImageList
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(SimulationUI))
        Me.StartDatePicker = New System.Windows.Forms.DateTimePicker
        Me.EndDatePicker = New System.Windows.Forms.DateTimePicker
        Me.TitleTextBox = New System.Windows.Forms.TextBox
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.SmallImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.SuspendLayout()
        '
        'StartDatePicker
        '
        Me.StartDatePicker.CustomFormat = "d MMMM yyyy"
        Me.StartDatePicker.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.StartDatePicker.Location = New System.Drawing.Point(80, 3)
        Me.StartDatePicker.Name = "StartDatePicker"
        Me.StartDatePicker.Size = New System.Drawing.Size(136, 20)
        Me.StartDatePicker.TabIndex = 0
        '
        'EndDatePicker
        '
        Me.EndDatePicker.CustomFormat = "d MMMM yyyy"
        Me.EndDatePicker.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.EndDatePicker.Location = New System.Drawing.Point(286, 2)
        Me.EndDatePicker.Name = "EndDatePicker"
        Me.EndDatePicker.Size = New System.Drawing.Size(136, 20)
        Me.EndDatePicker.TabIndex = 1
        '
        'TitleTextBox
        '
        Me.TitleTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TitleTextBox.AutoSize = False
        Me.TitleTextBox.Location = New System.Drawing.Point(464, 3)
        Me.TitleTextBox.Name = "TitleTextBox"
        Me.TitleTextBox.Size = New System.Drawing.Size(384, 20)
        Me.TitleTextBox.TabIndex = 2
        Me.TitleTextBox.Text = ""
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(431, 7)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(40, 24)
        Me.Label1.TabIndex = 5
        Me.Label1.Text = "Title:"
        '
        'Label3
        '
        Me.Label3.Location = New System.Drawing.Point(0, 8)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(86, 20)
        Me.Label3.TabIndex = 7
        Me.Label3.Text = "Starting Date:"
        '
        'Label4
        '
        Me.Label4.Location = New System.Drawing.Point(216, 7)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(96, 20)
        Me.Label4.TabIndex = 8
        Me.Label4.Text = "Ending Date:"
        '
        'SmallImageList
        '
        Me.SmallImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.SmallImageList.ImageStream = CType(resources.GetObject("SmallImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.SmallImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'SimulationUI
        '
        Me.ClientSize = New System.Drawing.Size(860, 624)
        Me.Controls.Add(Me.TitleTextBox)
        Me.Controls.Add(Me.EndDatePicker)
        Me.Controls.Add(Me.StartDatePicker)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label1)
        Me.Name = "SimulationUI"
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub refresh()
        MyBase.Refresh()
        'Dim filename As String = GetValue("summaryfile")
        ' This may not be what we really want here
        'If Path.GetDirectoryName(filename) = "" Then
        'filename = APSIMFile.XMLFilePath + "\" + filename
        'End If

        StartDatePicker.Value = Controller.Data.ChildValueWithError("start_date")
        EndDatePicker.Value = Controller.Data.ChildValueWithError("end_date")
        TitleTextBox.Text = Controller.Data.ChildValueWithError("title")



    End Sub

    Private Sub StartDatePicker_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles StartDatePicker.Leave
        Try
            Controller.Data.Child("start_date").Value = StartDatePicker.Value.Date
            Me.Refresh()

        Catch ex as system.exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating simulation starting date information")
        End Try
    End Sub

    Private Sub EndDatePicker_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles EndDatePicker.Leave
        Try
            Controller.Data.Child("end_date").Value = EndDatePicker.Value.Date
            Me.Refresh()

        Catch ex as system.exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating simulation ending date information")
        End Try
    End Sub

    Private Sub TitleTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles TitleTextBox.Leave
        Try
            Controller.Data.Child("title").Value = TitleTextBox.Text
            Me.Refresh()

        Catch ex as system.exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating title information")
        End Try
    End Sub



    'Private Sub SimulationUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
    '    Dim listview As New areaui

    '    With listview
    '        .Left = 1
    '        .Top = 30
    '        .Width = Me.Width - 2
    '        .Height = Me.Height - 60
    '        .Anchor = AnchorStyles.Bottom Or AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
    '        .Data = Data ' this must be done before you can set the path
    '        .Parent = Me
    '        .Show()
    '    End With


    'End Sub
End Class
