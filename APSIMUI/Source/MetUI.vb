Imports System
Imports System.IO
Imports scpl
Public Class MetUI
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
    Friend WithEvents MetFileTextBox As System.Windows.Forms.TextBox
    Friend WithEvents BrowseButton As System.Windows.Forms.Button
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ImageList2 As System.Windows.Forms.ImageList
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents FileContentsTab As System.Windows.Forms.TabPage
    Friend WithEvents GraphTab As System.Windows.Forms.TabPage
    Friend WithEvents RichTextBox As System.Windows.Forms.RichTextBox
    Friend WithEvents Graph As scpl.Windows.PlotSurface2D
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(MetUI))
        Me.MetFileTextBox = New System.Windows.Forms.TextBox
        Me.BrowseButton = New System.Windows.Forms.Button
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.Label1 = New System.Windows.Forms.Label
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.ImageList2 = New System.Windows.Forms.ImageList(Me.components)
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.FileContentsTab = New System.Windows.Forms.TabPage
        Me.RichTextBox = New System.Windows.Forms.RichTextBox
        Me.GraphTab = New System.Windows.Forms.TabPage
        Me.Graph = New scpl.Windows.PlotSurface2D
        Me.TabControl.SuspendLayout()
        Me.FileContentsTab.SuspendLayout()
        Me.GraphTab.SuspendLayout()
        Me.SuspendLayout()
        '
        'MetFileTextBox
        '
        Me.MetFileTextBox.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.MetFileTextBox.AutoSize = False
        Me.MetFileTextBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.MetFileTextBox.ForeColor = System.Drawing.SystemColors.WindowText
        Me.MetFileTextBox.Location = New System.Drawing.Point(80, 8)
        Me.MetFileTextBox.Name = "MetFileTextBox"
        Me.MetFileTextBox.Size = New System.Drawing.Size(896, 24)
        Me.MetFileTextBox.TabIndex = 0
        Me.MetFileTextBox.Text = ""
        '
        'BrowseButton
        '
        Me.BrowseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.BrowseButton.ImageIndex = 0
        Me.BrowseButton.ImageList = Me.ImageList
        Me.BrowseButton.Location = New System.Drawing.Point(984, 8)
        Me.BrowseButton.Name = "BrowseButton"
        Me.BrowseButton.Size = New System.Drawing.Size(80, 24)
        Me.BrowseButton.TabIndex = 1
        Me.BrowseButton.Text = "Browse"
        Me.BrowseButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'ImageList
        '
        Me.ImageList.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(8, 11)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(72, 16)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Weather File:"
        '
        'ImageList2
        '
        Me.ImageList2.ImageSize = New System.Drawing.Size(16, 16)
        Me.ImageList2.TransparentColor = System.Drawing.Color.Transparent
        '
        'TabControl
        '
        Me.TabControl.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TabControl.Controls.Add(Me.FileContentsTab)
        Me.TabControl.Controls.Add(Me.GraphTab)
        Me.TabControl.Location = New System.Drawing.Point(0, 40)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(1064, 448)
        Me.TabControl.TabIndex = 4
        '
        'FileContentsTab
        '
        Me.FileContentsTab.Controls.Add(Me.RichTextBox)
        Me.FileContentsTab.Location = New System.Drawing.Point(4, 22)
        Me.FileContentsTab.Name = "FileContentsTab"
        Me.FileContentsTab.Size = New System.Drawing.Size(1056, 422)
        Me.FileContentsTab.TabIndex = 0
        Me.FileContentsTab.Text = "FileContents"
        '
        'RichTextBox
        '
        Me.RichTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RichTextBox.Font = New System.Drawing.Font("Courier New", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.RichTextBox.Location = New System.Drawing.Point(0, 0)
        Me.RichTextBox.Name = "RichTextBox"
        Me.RichTextBox.ReadOnly = True
        Me.RichTextBox.Size = New System.Drawing.Size(1056, 422)
        Me.RichTextBox.TabIndex = 4
        Me.RichTextBox.Text = ""
        '
        'GraphTab
        '
        Me.GraphTab.Controls.Add(Me.Graph)
        Me.GraphTab.Location = New System.Drawing.Point(4, 22)
        Me.GraphTab.Name = "GraphTab"
        Me.GraphTab.Size = New System.Drawing.Size(1056, 422)
        Me.GraphTab.TabIndex = 1
        Me.GraphTab.Text = "Graph"
        '
        'Graph
        '
        Me.Graph.AllowSelection = False
        Me.Graph.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.Graph.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Graph.HorizontalEdgeLegendPlacement = scpl.Legend.Placement.Inside
        Me.Graph.LegendBorderStyle = scpl.Legend.BorderType.Shadow
        Me.Graph.LegendXOffset = 10.0!
        Me.Graph.LegendYOffset = 1.0!
        Me.Graph.Location = New System.Drawing.Point(0, 0)
        Me.Graph.Name = "Graph"
        Me.Graph.Padding = 10
        Me.Graph.PlotBackColor = System.Drawing.Color.White
        Me.Graph.ShowLegend = False
        Me.Graph.Size = New System.Drawing.Size(1056, 422)
        Me.Graph.TabIndex = 0
        Me.Graph.Title = ""
        Me.Graph.TitleFont = New System.Drawing.Font("Arial", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Pixel)
        Me.Graph.VerticalEdgeLegendPlacement = scpl.Legend.Placement.Outside
        Me.Graph.XAxis1 = Nothing
        Me.Graph.XAxis2 = Nothing
        Me.Graph.YAxis1 = Nothing
        Me.Graph.YAxis2 = Nothing
        '
        'MetUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(1072, 488)
        Me.Controls.Add(Me.TabControl)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.BrowseButton)
        Me.Controls.Add(Me.MetFileTextBox)
        Me.Name = "MetUI"
        Me.TabControl.ResumeLayout(False)
        Me.FileContentsTab.ResumeLayout(False)
        Me.GraphTab.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub refresh()
        Try
            MyBase.Refresh()
            Dim filename As String = GetValue("filename")
            MetFileTextBox.Text = filename
            OpenFileDialog.InitialDirectory = Path.GetDirectoryName(filename)


            If File.Exists(filename) Then
                Dim text As String
                Dim sr As StreamReader = New StreamReader(filename)
                text = sr.ReadToEnd
                RichTextBox.Text = text
            Else
                MsgBox("The specified meteorological data file does not exist at the given location.  Please check file specification.", MsgBoxStyle.Critical, "File does not exist.")

            End If

            With Graph
                .Clear()
                'Create a new line plot from array data via the ArrayAdapter class.
                Dim lp As New LinePlot(New ArrayAdapter(makeDaub(256)))
                lp.Color = Color.Green

                ' Add it to the plot Surface
                .Add(lp)
                .Title = "Daubechies Wavelet"
                ' Ok, the above will produce a decent default plot, but we would like to change
                ' some of the Y Axis details. First, we'd like lots of small ticks (10) between 
                ' large tick values. Secondly, we'd like to draw a grid for the Y values. To do 
                ' this, we create a new LinearAxis (we could also use Label, Log etc). Rather than
                ' starting from scratch, we use the constructor that takes an existing axis and
                ' clones it (values in the superclass Axis only are cloned). PlotSurface2D
                ' automatically determines a suitable axis when we add plots to it (merging
                ' current requirements with old requirements), and we use this as our starting
                ' point. Because we didn't specify which Y Axis we are using when we added the 
                ' above line plot (there is one on the left - YAxis1 and one on the right - YAxis2)
                ' PlotSurface2D.Add assumed we were using YAxis1. So, we create a new axis based on
                ' YAxis1, update the details we want, then set the YAxis1 to be our updated one.
                Dim lax As New LinearAxis(.YAxis1)
                lax.NumberSmallTicks = 10
                lax.GridDetail = Axis.GridType.Fine
                .YAxis1 = lax

                ' We would also like to modify the way in which the X Axis is printed. This time,
                ' we'll just modify the relevant PlotSurface2D Axis directly. 
                .XAxis1.GridDetail = Axis.GridType.Coarse
                .XAxis1.WorldMax = 100.0F

                .PlotBackColor = Color.Beige

                ' Force a re-draw the control. 
                .Refresh()

            End With


        Catch E As Exception
            MsgBox(E.Message, MsgBoxStyle.Critical, "Error in refreshing Met UI")
        End Try

    End Sub
    Public Function makeDaub(ByVal len As Integer) As Single()

        Dim daub4_h() As Single = {0.4829629F, 0.8365163F, 0.224143863F, -0.129409522F}
        Dim daub4_g() As Single = {-0.129409522F, -0.224143863F, 0.8365163F, -0.4829629F}

        Dim a(len) As Single
        a(8) = 1
        Dim t() As Single

        Dim ns As Integer = 4

        While (ns < len / 2)
            t = a.Clone()

            ns = ns * 2
            Dim i As Integer
            For i = 0 To ns * 2 - 1
                a(i) = 0.0F
            Next

            ' wavelet contribution
            For i = 0 To ns - 1
                Dim j As Integer
                For j = 0 To 3
                    a((2 * i + j) Mod (2 * ns)) = a((2 * i + j) Mod (2 * ns)) + daub4_g(j) * t(i + ns)
                Next j
            Next i

            ' smooth contribution
            For i = 0 To ns - 1
                Dim j As Integer
                For j = 0 To 3
                    a((2 * i + j) Mod (2 * ns)) = a((2 * i + j) Mod (2 * ns)) + daub4_h(j) * t(i)
                Next j
            Next i
        End While
        Return a
    End Function

    Private Sub BrowseButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BrowseButton.Click
        Try
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                MetFileTextBox.Text = OpenFileDialog.FileName
                MyData.Child("filename").Value = MetFileTextBox.Text
                Me.Refresh()
            Else
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in browsing to new met file")
        End Try
    End Sub

    Private Sub MetFileTextBox_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles MetFileTextBox.Leave
        Try
            If MetFileTextBox.Visible = True Then
                MyData.Child("filename").Value = MetFileTextBox.Text
                Me.Refresh()
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error in updating met file name information")
        End Try
    End Sub



    Private Sub MetUI_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class
