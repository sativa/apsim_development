'Imports DotNetComponentInterface

Imports VBGeneral
Imports System.Math
Imports VBMet
Imports CSGeneral.MathUtility

'Imports VBMath
Public Class Sounds
    Inherits ApsimComponent
    Private Declare Auto Function PlaySound Lib "winmm.dll" (ByVal lpszSoundName As String, ByVal hModule As Integer, ByVal dwFlags As Integer) As Integer

    ' ----------------------  Component Constants-------------------------

    ' ------------------------  Component Data ---------------------------

    Public Overrides Sub init1()
        ' Do Registrations
        ' ----------------
        events.RegisterHandler("error", AddressOf OnError)

    End Sub

    Public Overrides Sub init2()
        ReadParameters()
    End Sub

    Private Sub ReadParameters()
        '        UptakeSource = Data.ChildValue("uptake_source")
    End Sub

#Region "Events"
    Private Sub OnError(ByVal ErrorMsg As Null)
        'Dim fileName As String = String.Concat(c_soundPath, "\", waveList.Text)
        Dim FileName As String = "c:\windows\media\windows xp error.wav"

        Const SND_FILENAME As Integer = &H20000

        PlaySound(FileName, 0, SND_FILENAME)
  
    End Sub


#End Region

End Class
