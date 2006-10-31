Imports System
Imports System.IO
Imports System.Collections
Imports System.Collections.Specialized
Imports System.Runtime.InteropServices
Imports System.Xml
Imports System.IO.File
Imports System.Windows.Forms

Public Class APSIMFile
    Private APSIMFilename As String
    Private FileData As APSIMData
    Private HasChanged As Boolean
    Const DefaultName = "Untitled.apsim"
    Sub New()
        HasChanged = False
        ' Needs to go and get blank document 

        Dim inifile As New APSIMSettings
        Dim newfile As String = inifile.GetSetting("apsimui", "newfile")
        Dim text As String
        Dim sr As StreamReader = New StreamReader(newfile)
        text = sr.ReadToEnd
        sr.Close()
        FileData = New APSIMData(text)
        APSIMFilename = DefaultName
    End Sub
    Private Sub OpenFile()
        If File.Exists(APSIMFilename) Then
            Dim sr As StreamReader = New StreamReader(APSIMFilename)
            Dim FileXML As String = sr.ReadToEnd()
            FileData = New APSIMData(FileXML)
            sr.Close()
        Else
            Throw New Exception("Cannot find file: " + APSIMFilename)
        End If

    End Sub
    Public Sub Open(ByVal newfilename As String)
        APSIMFilename = newfilename
        OpenFile()
    End Sub
    Public Sub Open()
        If HasChanged Then
            Dim DoSave As Integer = MsgBox("Do you want to save your work?", MsgBoxStyle.YesNoCancel, "APSIM")
            Select Case DoSave
                Case MsgBoxResult.Yes
                    ' Save the file
                    Save()

                Case MsgBoxResult.No
                    ' Do not save

                Case MsgBoxResult.Cancel
                    ' I don't like exiting here!!!!!
                    ' But will anyway until I get a better design

                    Exit Sub
            End Select

        End If
        Dim dialog As New OpenFileDialog
        With dialog
            .Filter = "APSIM Data files (*.apsim)|*.apsim|All files (*.*)|*.*"
            .AddExtension = True
        End With
        Dim choice As DialogResult = dialog.ShowDialog
        If choice = DialogResult.OK Then
            APSIMFilename = dialog.FileName
            OpenFile()
        End If

    End Sub
    ReadOnly Property Filename() As String
        Get
            Return APSIMFilename
        End Get
    End Property
    Public Sub Save(Optional ByVal fname As String = "")
        If fname <> "" Then
            APSIMFilename = fname
            SaveFile()
        ElseIf APSIMFilename = DefaultName Then
            SaveAs()
            HasChanged = False
        Else
            SaveFile()
        End If
    End Sub
    Public Sub SaveAs()
        Dim dialog As New SaveFileDialog
        With dialog
            .Filter = "APSIM Data files (*.apsim)|*.apsim|All files (*.*)|*.*"
            .AddExtension = True
            .OverwritePrompt = True
        End With
        Dim choice As DialogResult = dialog.ShowDialog
        If choice = DialogResult.OK Then
            APSIMFilename = dialog.FileName
            SaveFile()
        Else
            ' User has cancelled - do nothing
        End If
    End Sub
    ReadOnly Property data() As APSIMData
        Get
            Return FileData
        End Get
    End Property

    Private Sub SaveFile()
        'Dim sw As StreamWriter = New StreamWriter(APSIMFilename)
        'sw.Write(FileData.XML)
        'sw.Close()

        Dim doc As New XmlDocument
        doc.LoadXml(FileData.XML)
        doc.Save(APSIMFilename)

        'Dim writer As New XmlTextWriter(APSIMFilename, System.Text.Encoding.ASCII)
        'With writer
        '    .Formatting = Formatting.Indented
        '    .Indentation = 3
        'End With
        'writer..WriteString(FileData.XML)
        'writer.Flush()

    End Sub
    ReadOnly Property caption() As String
        Get
            If HasChanged Then
                Return APSIMFilename + "*"
            Else
                Return APSIMFilename
            End If
        End Get
    End Property
End Class
