Imports System.Collections
Imports System.Collections.Specialized
Imports System.IO
Imports VBGeneral
Public Class Toolboxes


    ' ------------------------------------
    ' file names property
    ' ------------------------------------
    Public Property Filenames() As StringCollection
        Get
            Dim Files As New StringCollection
            Dim inifile As New APSIMSettings
            Dim ToolboxesString As String = inifile.GetSetting("Toolboxes", "toolbox")
            If (Trim(ToolboxesString) <> "") Then
                Dim ToolBoxes() As String = Split(ToolboxesString, "|")

                For i As Integer = 0 To ToolBoxes.Length - 1
                    If File.Exists(ToolBoxes(i)) Then
                        Files.Add(ToolBoxes(i))
                    End If
                Next
            End If
            Return Files
        End Get
        Set(ByVal Value As StringCollection)
            Dim ToolBoxesString As String = ""
            For Each item As String In Value
                If ToolBoxesString = "" Then
                    ToolBoxesString = item.ToString
                Else
                    ToolBoxesString = ToolBoxesString + "|" + item.ToString
                End If
            Next
            Dim inifile As New APSIMSettings
            inifile.SetSetting("Toolboxes", "toolbox", ToolBoxesString)
        End Set
    End Property


    ' ------------------------------------
    ' names property
    ' ------------------------------------
    Public ReadOnly Property Names() As StringCollection
        Get
            Dim Files As New StringCollection
            Dim inifile As New APSIMSettings
            Dim ToolboxesString As String = inifile.GetSetting("Toolboxes", "toolbox")
            If (Trim(ToolboxesString) <> "") Then
                Dim ToolBoxes() As String = Split(ToolboxesString, "|")

                For i As Integer = 0 To ToolBoxes.Length - 1
                    If File.Exists(ToolBoxes(i)) Then
                        Files.Add(Path.GetFileNameWithoutExtension(ToolBoxes(i)))
                    End If
                Next
            End If
            Return Files
        End Get
    End Property


    ' ---------------------------------------------------
    ' create a new empty toolbox at the specified file
    ' ---------------------------------------------------
    Public Sub CreateNew(ByVal Filename As String)
        Dim sr As StreamWriter = File.CreateText(Filename)
        sr.WriteLine("<folder name=""" + Path.GetFileNameWithoutExtension(Filename) + """>")
        sr.WriteLine("</folder>")
        sr.Close()
    End Sub


    ' ---------------------------------------------------
    ' create a new empty toolbox at the specified file
    ' ---------------------------------------------------
    Public Function NameToFileName(ByVal Name As String) As String
        For Each FName As String In Filenames
            If Path.GetFileNameWithoutExtension(FName) = Name Then
                Return FName
            End If
        Next
        Throw New System.Exception("Cannot find toolbox filename for name: " + Name)
    End Function

End Class
