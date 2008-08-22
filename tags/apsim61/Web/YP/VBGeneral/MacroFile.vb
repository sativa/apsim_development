
Public Class MacroFile
    Declare Ansi Sub Transform Lib "APSIMShared.dll" Alias "doMacroTransform" (ByVal xmlfilename As String, ByVal macrofilename As String, ByVal outputdirectory As String)
    Public Sub DoTransform(ByVal xmlfilename As String, ByVal macrofilename As String, ByVal outputdirectory As String)
        Transform(xmlfilename, macrofilename, outputdirectory)
    End Sub
End Class
