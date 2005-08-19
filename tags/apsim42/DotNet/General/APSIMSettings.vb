Public Class APSIMSettings
    Declare Ansi Sub GetAPSIMSetting Lib "APSIMShared.dll" Alias "SettingsRead" (ByVal key As String, ByVal returnstring As String, ByVal replacemacros As Integer)
    Declare Ansi Sub SetAPSIMSetting Lib "APSIMShared.dll" Alias "SettingsWrite" (ByVal key As String, ByVal returnstring As String)
    Private Const MaxStringLength = 255
    Public Function GetSetting(ByVal section As String, ByVal keyword As String) As String
        Try
            Dim Setting As String = Space(MaxStringLength)
            Dim key As String = Trim(section) + "|" + Trim(keyword)
            GetAPSIMSetting(key, Setting, 1) '1 is true in C++
            GetSetting = CStringToVBString(Setting)
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error reading APSIM Setting")
        End Try
    End Function
    Public Sub SetSetting(ByVal section As String, ByVal keyword As String, ByVal value As String)
        Try
            Dim key As String = Trim(section) + "|" + Trim(keyword)
            SetAPSIMSetting(key, value)
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error writting APSIM Setting")
        End Try
    End Sub
    Private Function CStringToVBString(ByVal Cstring As String) As String
        ' This function converts a C string to a vb string by returning everything
        ' up to the null character
        Try
            Dim NullChar As New Char
            CStringToVBString = Cstring.Substring(0, Cstring.IndexOf(NullChar))
        Catch e As Exception
            MsgBox("Error converting string types", MsgBoxStyle.Critical)
        End Try
    End Function
End Class
