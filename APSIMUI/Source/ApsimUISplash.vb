
Imports VBGeneral

Public NotInheritable Class ApsimUISplash

    Private Sub AboutBox1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If Not Modal Then
            Timer.Enabled = True
            OkButton.Visible = False
        End If

        Me.LabelVersion.Text = "Version: " & APSIMSettings.ApsimVersion
        Me.LabelBuildDate.Text = "Build date: " & APSIMSettings.ApsimBuildDate
        Me.LabelBuildNumber.Text = "Build number: " & APSIMSettings.ApsimBuildNumber
    End Sub

    Private Sub OKButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OkButton.Click
        Close()
    End Sub

    Private Sub OnTick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer.Tick
        Close()
    End Sub
End Class
