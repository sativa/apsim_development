Imports System.Windows.Forms
Imports System.IO
Public Class APSIMSettings
    Declare Ansi Sub GetAPSIMSetting Lib "APSIMShared.dll" Alias "SettingsRead" (ByVal key As String, ByVal returnstring As String, ByVal replacemacros As Integer)
    Declare Ansi Sub SetAPSIMSetting Lib "APSIMShared.dll" Alias "SettingsWrite" (ByVal key As String, ByVal returnstring As String)

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
        Dim version As String = GetSetting("versions", "apsim")
        Return version
    End Function
End Class
