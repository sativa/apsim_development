Imports System.Windows.Forms
Imports System.IO
Imports System.Text
Public Class APSIMSettings
    Declare Ansi Sub GetAPSIMSetting Lib "APSIMShared.dll" Alias "SettingsRead" (ByVal key As String, ByVal returnstring As String, ByVal replacemacros As Integer)
    Declare Ansi Sub SetAPSIMSetting Lib "APSIMShared.dll" Alias "SettingsWrite" (ByVal key As String, ByVal returnstring As String)
    Private Declare Unicode Function WritePrivateProfileString Lib "kernel32" _
        Alias "WritePrivateProfileStringW" (ByVal lpApplicationName As String, _
        ByVal lpKeyName As String, ByVal lpString As String, _
        ByVal lpFileName As String) As Int32
    Private Declare Unicode Function GetPrivateProfileString Lib "kernel32" _
        Alias "GetPrivateProfileStringW" (ByVal lpApplicationName As String, _
        ByVal lpKeyName As String, ByVal lpDefault As String, _
        ByVal lpReturnedString As String, ByVal nSize As Int32, _
        ByVal lpFileName As String) As Int32

    Public Function GetSetting(ByVal section As String, ByVal keyword As String) As String
        Try
            Dim Setting As String = Space(Utility.MaxStringLength)
            Dim key As String = Trim(section) + "|" + Trim(keyword)
            GetAPSIMSetting(key, Setting, 1) '1 is true in C++
            GetSetting = Utility.CStringToVBString(Setting)
        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error reading APSIM Setting")
        End Try
    End Function
    Public Sub SetSetting(ByVal section As String, ByVal keyword As String, ByVal value As String)
        Try
            Dim key As String = Trim(section) + "|" + Trim(keyword)
            SetAPSIMSetting(key, value)
        Catch ex As System.Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error writting APSIM Setting")
        End Try
    End Sub

    Public Shared Function ApsimDirectory() As String
        Return Directory.GetParent(Path.GetDirectoryName(Application.ExecutablePath)).ToString
    End Function
    Public Function ApsimVersion() As String
        Dim version As String = GetSetting("version", "apsim")
        Return version
    End Function


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
            INIRead = sData.Substring(0, n)
        Else
            INIRead = ""
        End If
    End Function


    Public Overloads Shared Function INIRead(ByVal INIPath As String, _
        ByVal SectionName As String, ByVal KeyName As String) As String
        ' assumes zero-length default
        Return INIRead(INIPath, SectionName, KeyName, "")
    End Function

    Public Overloads Shared Function INIReadAllKeys(ByVal INIPath As String, _
        ByVal SectionName As String) As String()
        ' returns all keys in a given section of the given file
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

    Public Overloads Sub INIDeleteKey(ByVal INIPath As String, ByVal SectionName As String, _
    ByVal KeyName As String) ' delete single line from section
        Call WritePrivateProfileString(SectionName, KeyName, Nothing, INIPath)
    End Sub

    Public Overloads Sub INIDeleteSection(ByVal INIPath As String, ByVal SectionName As String)
        ' delete section from INI file
        Call WritePrivateProfileString(SectionName, Nothing, Nothing, INIPath)
    End Sub

End Class
