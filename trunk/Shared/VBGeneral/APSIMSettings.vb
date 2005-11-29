Imports System.Windows.Forms
Imports System.IO
Imports System.Text
Imports System.Collections.Specialized

Public Class APSIMSettings
    Private Declare Ansi Function WritePrivateProfileString Lib "kernel32" _
        Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As String, _
        ByVal lpKeyName As String, ByVal lpString As String, _
        ByVal lpFileName As String) As Int32
    Private Declare Ansi Function GetPrivateProfileString Lib "kernel32" _
        Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, _
        ByVal lpKeyName As String, ByVal lpDefault As String, _
        ByVal lpReturnedString As String, ByVal nSize As Int32, _
        ByVal lpFileName As String) As Int32
    Private Declare Ansi Function GetPrivateProfileSection Lib "kernel32" _
        Alias "GetPrivateProfileSectionA" (ByVal lpApplicationName As String, _
        ByVal lpReturnedString As String, ByVal nSize As Int32, _
        ByVal lpFileName As String) As Int32


    ' ---------------------------
    ' Return APSIM directory root
    ' ---------------------------
    Public Shared Function ApsimDirectory() As String
        Dim ExeFolder As String = Path.GetDirectoryName(Application.ExecutablePath)
        If File.Exists(ExeFolder + "\\apsim.ini") Then
            Return ExeFolder
        Else
            Return Directory.GetParent(ExeFolder).ToString
        End If
    End Function


    ' ---------------------------
    ' Return APSIM version number
    ' ---------------------------
    Public Shared Function ApsimVersion() As String
        Dim version As String = INIRead(ApsimIniFile(), "version", "apsim")
        Return version
    End Function


    ' ---------------------------
    ' Return APSIM directory root
    ' ---------------------------
    Public Shared Function ApsimIniFile() As String
        Return ApsimDirectory() + "\apsim.ini"
    End Function


    ' ----------------------------------------
    ' Returns a key value from an .ini file.
    ' ----------------------------------------
    Private Overloads Shared Function INIRead(ByVal INIPath As String, _
        ByVal SectionName As String, ByVal KeyName As String, _
        ByVal DefaultValue As String) As String
        ' primary version of call gets single value given all parameters
        Dim n As Int32
        Dim sData As String
        sData = Space$(1024) ' allocate some room 
        n = GetPrivateProfileString(SectionName, KeyName, DefaultValue, _
        sData, sData.Length, INIPath)
        If n > 0 Then ' return whatever it gave us
            INIRead = sData.Substring(0, n).Replace("%apsuite", ApsimDirectory())
        Else
            INIRead = ""
        End If
    End Function

    ' ----------------------------------------
    ' Returns the contents of a section
    ' ----------------------------------------
    Public Overloads Shared Function INIReadSection(ByVal INIPath As String, _
        ByVal SectionName As String) As String
        Dim n As Int32
        Dim sData As String
        sData = Space$(5000) ' allocate some room 
        n = GetPrivateProfileSection(SectionName, sData, sData.Length, INIPath)
        Dim Values As String() = sData.Split(ControlChars.NullChar) ' change embedded NULLs to pipe chars
        Dim ReturnString As String
        For Each Line As String In Values
            If ReturnString <> "" Then
                ReturnString = ReturnString + vbCrLf
            End If
            ReturnString = ReturnString + Line
        Next
        Return ReturnString
    End Function


    ' ----------------------------------------
    ' Returns a key value from an .ini file.
    ' ----------------------------------------
    Public Overloads Shared Function INIRead(ByVal INIPath As String, _
        ByVal SectionName As String, ByVal KeyName As String) As String
        Return INIRead(INIPath, SectionName, KeyName, "")
    End Function


    ' --------------------------------------------------
    ' Returns multiple key values from an .ini file.
    ' Assumes the values use a numbering system appended
    ' to the key name.
    ' --------------------------------------------------
    Public Overloads Shared Function INIReadMultiple(ByVal INIPath As String, _
        ByVal SectionName As String, ByVal KeyName As String) As StringCollection
        Dim Values As New StringCollection
        Dim KeyNumber As Integer = 1
        Dim Value As String = INIRead(INIPath, SectionName, KeyName + KeyNumber.ToString())
        While Value <> ""
            Values.Add(Value)
            KeyNumber = KeyNumber + 1
            Value = INIRead(INIPath, SectionName, KeyName + KeyNumber.ToString())
        End While
        Return Values
    End Function


    ' -----------------------------------------------------
    ' Returns all key names from a section of an .ini file.
    ' -----------------------------------------------------
    Public Overloads Shared Function INIReadAllKeys(ByVal INIPath As String, _
        ByVal SectionName As String) As String()
        Dim Value As String = INIRead(INIPath, SectionName, Nothing, "")
        Dim Values As String() = Value.Split(ControlChars.NullChar) ' change embedded NULLs to pipe chars
        Dim ReturnValues(Values.Length - 2) As String
        For i As Integer = 0 To Values.Length - 2
            ReturnValues(i) = Values(i)
        Next
        Return ReturnValues
    End Function



    Public Overloads Shared Function INIReadAllSections(ByVal INIPath As String) As String()
        ' returns all section names given just path
        Dim Value As String = INIRead(INIPath, Nothing, Nothing, "")
        Dim Values As String() = Value.Split(ControlChars.NullChar) ' change embedded NULLs to pipe chars
        Dim ReturnValues(Values.Length - 2) As String
        For i As Integer = 0 To Values.Length - 2
            ReturnValues(i) = Values(i)
        Next
        Return ReturnValues

    End Function

    Public Shared Sub INIWrite(ByVal INIPath As String, ByVal SectionName As String, _
        ByVal KeyName As String, ByVal TheValue As String)
        Call WritePrivateProfileString(SectionName, KeyName, TheValue, INIPath)
    End Sub


    ' --------------------------------------------------
    ' Writes multiple key values from an .ini file.
    ' Assumes the values use a numbering system appended
    ' to the key name.
    ' --------------------------------------------------
    Public Shared Sub INIWriteMultiple(ByVal INIPath As String, _
        ByVal SectionName As String, ByVal KeyName As String, ByVal Values As String())
        Dim KeyNumber As Integer = 1
        Dim Value As String = INIRead(INIPath, SectionName, KeyName + KeyNumber.ToString())
        While Value <> ""
            INIDeleteKey(INIPath, SectionName, KeyName + KeyNumber.ToString())
            KeyNumber = KeyNumber + 1
            Value = INIRead(INIPath, SectionName, KeyName + KeyNumber.ToString())
        End While

        KeyNumber = 1
        For Each Value In Values
            INIWrite(INIPath, SectionName, KeyName + KeyNumber.ToString(), Value)
            KeyNumber = KeyNumber + 1
        Next
    End Sub


    Public Shared Sub INIDeleteKey(ByVal INIPath As String, ByVal SectionName As String, _
    ByVal KeyName As String) ' delete single line from section
        Call WritePrivateProfileString(SectionName, KeyName, Nothing, INIPath)
    End Sub

    Public Shared Sub INIDeleteSection(ByVal INIPath As String, ByVal SectionName As String)
        ' delete section from INI file
        Call WritePrivateProfileString(SectionName, Nothing, Nothing, INIPath)
    End Sub

End Class
