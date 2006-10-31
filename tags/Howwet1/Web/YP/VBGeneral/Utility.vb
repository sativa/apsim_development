Public Class Utility
    Public Const MaxStringLength = 50000
    Public Shared Function CStringToVBString(ByVal Cstring As String) As String
        ' This function converts a C string to a vb string by returning everything
        ' up to the null character
        Try
            Dim NullChar As New Char
            CStringToVBString = Cstring.Substring(0, Cstring.IndexOf(NullChar))
        Catch e As System.Exception
            MsgBox("Error converting string types", MsgBoxStyle.Critical)
        End Try
    End Function

End Class
