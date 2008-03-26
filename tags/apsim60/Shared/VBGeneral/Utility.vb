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
            Return ""
        End Try
    End Function

    Public Shared Function IndexOfCaseInsensitive(ByVal Values() As String, ByVal St As String) As Integer
        ' -------------------------------------------------------
        ' A version of IndexOf that is case insensitive.
        ' -------------------------------------------------------
        Dim StLower As String = St.ToLower()
        For i As Integer = 0 To Values.Length - 1
            If Values(i).ToLower() = StLower Then
                Return i
            End If
        Next
        Return -1
    End Function

    Public Shared Function IndexOfCaseInsensitive(ByVal Values As Specialized.StringCollection, ByVal St As String) As Integer
        ' -------------------------------------------------------
        ' A version of IndexOf that is case insensitive.
        ' -------------------------------------------------------
        Dim StLower As String = St.ToLower()
        For i As Integer = 0 To Values.Count - 1
            If Values(i).ToLower() = StLower Then
                Return i
            End If
        Next
        Return -1
    End Function

End Class
