Imports System.Collections.Specialized
Public Class MacroV2File
    Private MacroData As APSIMData
    Private MacroFileName As String

    WriteOnly Property Data() As APSIMData
        Set(ByVal Value As APSIMData)
            MacroData = Value
        End Set
    End Property
    Property filename() As String
        Get
            Return MacroFileName
        End Get
        Set(ByVal Value As String)
            MacroFileName = Value
        End Set
    End Property
    Public Sub Process(ByRef CreatedFiles As StringCollection)

    End Sub

End Class
