Imports VBGeneral
Imports System.Collections
Imports System.Collections.Specialized

Public Class EmptyUI
    Inherits VBGeneral.BaseUI

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
    Friend WithEvents Panel2 As System.Windows.Forms.Panel
    Friend WithEvents PictureBox As System.Windows.Forms.PictureBox
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents DocumentationLink As System.Windows.Forms.LinkLabel
    Friend WithEvents MainLabel As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(EmptyUI))
        Me.Panel2 = New System.Windows.Forms.Panel
        Me.PictureBox = New System.Windows.Forms.PictureBox
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.Label1 = New System.Windows.Forms.Label
        Me.DocumentationLink = New System.Windows.Forms.LinkLabel
        Me.MainLabel = New System.Windows.Forms.Label
        Me.Panel2.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Panel2
        '
        Me.Panel2.Controls.Add(Me.Panel1)
        Me.Panel2.Controls.Add(Me.PictureBox)
        Me.Panel2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Panel2.Location = New System.Drawing.Point(0, 23)
        Me.Panel2.Name = "Panel2"
        Me.Panel2.Size = New System.Drawing.Size(884, 618)
        Me.Panel2.TabIndex = 8
        '
        'PictureBox
        '
        Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox.Image = CType(resources.GetObject("PictureBox.Image"), System.Drawing.Image)
        Me.PictureBox.Location = New System.Drawing.Point(0, 0)
        Me.PictureBox.Name = "PictureBox"
        Me.PictureBox.Size = New System.Drawing.Size(255, 563)
        Me.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox.TabIndex = 1
        Me.PictureBox.TabStop = False
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.DocumentationLink)
        Me.Panel1.Controls.Add(Me.MainLabel)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Panel1.Location = New System.Drawing.Point(255, 0)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(629, 618)
        Me.Panel1.TabIndex = 7
        '
        'Label1
        '
        Me.Label1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label1.Location = New System.Drawing.Point(16, 64)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(566, 88)
        Me.Label1.TabIndex = 8
        Me.Label1.Text = "Most APSIM crop or plant modules do not require extra user input.  Crop specific " & _
        "parameters are either provided for your chosen soil, or are captured as part of " & _
        "the standard model validation process."
        '
        'DocumentationLink
        '
        Me.DocumentationLink.Location = New System.Drawing.Point(24, 184)
        Me.DocumentationLink.Name = "DocumentationLink"
        Me.DocumentationLink.Size = New System.Drawing.Size(208, 24)
        Me.DocumentationLink.TabIndex = 7
        Me.DocumentationLink.TabStop = True
        Me.DocumentationLink.Text = "See Module Documentation for details."
        '
        'MainLabel
        '
        Me.MainLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.MainLabel.Location = New System.Drawing.Point(16, 24)
        Me.MainLabel.Name = "MainLabel"
        Me.MainLabel.Size = New System.Drawing.Size(208, 22)
        Me.MainLabel.TabIndex = 6
        Me.MainLabel.Text = "Crop type"
        '
        'EmptyUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(884, 681)
        Me.Controls.Add(Me.Panel2)
        Me.Name = "EmptyUI"
        Me.Controls.SetChildIndex(Me.Panel2, 0)
        Me.Panel2.ResumeLayout(False)
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region
    Overrides Sub Refresh()
        MainLabel.Text = Data.Type
        HelpLabel.Text = "This module does not have any editable properties."
        Dim inifile As New APSIMSettings
        Dim UIManager As UIManager = Explorer.ApplicationSettings
        Dim imagefile As String = UIManager.ImageFileForType(Data.Type)
        PictureBox.Image = Image.FromFile(imagefile)
        Try
            Label1.Text = UIManager.DescriptionForType(Data.Type)
        Catch ex As System.Exception
            ' Don't update label.
        End Try
    End Sub

    Private Sub DocumentationLink_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles DocumentationLink.LinkClicked
        Dim UIManager As UIManager = Explorer.ApplicationSettings
        Try
            Dim url As String = APSIMSettings.ApsimDirectory + "\apsimui\types.xml#" + Data.Type
            UIManager.ShowHelp(url)
        Catch ex As System.Exception
            UIManager.ShowHelp("www.apsim.info")
        End Try
    End Sub
End Class
