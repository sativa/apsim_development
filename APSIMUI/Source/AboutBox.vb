
Imports VBGeneral

Public NotInheritable Class AboutBox

    Private Sub AboutBox1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ' Set the title of the form.
        Dim ApplicationTitle As String
        If My.Application.Info.Title <> "" Then
            ApplicationTitle = My.Application.Info.Title
        Else
            ApplicationTitle = System.IO.Path.GetFileNameWithoutExtension(My.Application.Info.AssemblyName)
        End If
        Me.Text = String.Format("About {0}", ApplicationTitle)
        ' Initialize all of the text displayed on the About Box.
        ' TODO: Customize the application's assembly information in the "Application" pane of the project 
        '    properties dialog (under the "Project" menu).
        Me.LabelProductName.Text = "APSIM"
        Me.LabelVersion.Text = "Version " & APSIMSettings.ApsimVersion
        Me.LabelBuildDate.Text = "Build Date" & APSIMSettings.ApsimBuildDate
        Me.LabelBuildNumber.Text = "Build Number " & APSIMSettings.ApsimBuildNumber
        Me.TextBoxDescription.Text = "For more information go to www.apsim.info or contact the Agricultural Production Systems Research Unit at www.apsru.gov.au"
    End Sub

    Private Sub OKButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OKButton.Click
        Me.Close()
    End Sub
End Class
