Imports VBUserInterface
Imports VBGeneral
Public Class ApsimUIActions
    Public Shared Sub FileNew(ByVal Controller As BaseController)
        If Controller.FileSaveAfterPrompt() Then
            Dim NewDocForm As New NewDocumentForm
            If NewDocForm.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                Dim newsim As New APSIMData("folder", "Simulations")
                Dim Data As APSIMData = newsim.Add(NewDocForm.Selection)
                newsim.SetAttribute("version", Data.Attribute("version"))
                Data.DeleteAttribute("version")
                NewDocForm.Close()
                Controller.FileName = "Untitled.apsim"
                Controller.ApsimData.[New](newsim.XML)
            End If
        End If
    End Sub

    Public Shared Sub HelpDocumentation(ByVal Controller As BaseController)
        Dim HelpURL As String = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "docfile")
        Process.Start(HelpURL)
    End Sub
    Public Shared Sub HelpAbout(ByVal Controller As BaseController)
        Dim about As New AboutBox
        about.Show()
    End Sub

    Public Shared Sub Run(ByVal Controller As BaseController)

    End Sub

    Public Shared Sub RunAll(ByVal Controller As BaseController)

    End Sub


End Class
