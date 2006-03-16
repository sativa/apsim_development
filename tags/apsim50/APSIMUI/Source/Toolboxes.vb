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
            Dim Toolboxes As StringCollection = APSIMSettings.INIReadMultiple(APSIMSettings.ApsimIniFile(), "Toolboxes", "toolbox")
            For i As Integer = 0 To Toolboxes.Count - 1
                If File.Exists(Toolboxes(i)) Then
                    Files.Add(Toolboxes(i))
                End If
            Next
            Return Files
        End Get
        Set(ByVal Value As StringCollection)
            Dim Values(Value.Count) As String
            Value.CopyTo(Values, 0)
            APSIMSettings.INIWriteMultiple(APSIMSettings.ApsimIniFile(), "Toolboxes", "toolbox", Values)
        End Set
    End Property


    ' ------------------------------------
    ' names property
    ' ------------------------------------
    Public ReadOnly Property Names() As StringCollection
        Get
            Dim Files As New StringCollection
            Dim Toolboxes As StringCollection = APSIMSettings.INIReadMultiple(APSIMSettings.ApsimIniFile(), "Toolboxes", "toolbox")
            For i As Integer = 0 To Toolboxes.Count - 1
                If File.Exists(Toolboxes(i)) Then
                    Files.Add(Path.GetFileNameWithoutExtension(Toolboxes(i)))
                End If
            Next
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
