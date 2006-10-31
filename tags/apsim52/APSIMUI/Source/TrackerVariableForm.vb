Public Class TrackerVariableForm
    Private Controller As VBGeneral.BaseController

    Public Sub New(ByVal C As VBGeneral.BaseController)
        ' --------------------
        ' Constructor
        ' --------------------
        InitializeComponent()

        Controller = C
        Me.SetHyperTextLinks()
    End Sub


    Public Property Variable() As String
        Get
            Return VariableTemplateOK()
        End Get
        Set(ByVal value As String)

        End Set
    End Property

    Private Sub SetLinkLabel(ByVal Variable As String)
    End Sub


    Private Sub TextBuilder_LinkClicked(ByVal sender As System.Object, ByVal link As CSGeneral.HyperTextLink) Handles TextBuilder.LinkClicked

        Select Case link.HyperTextData.ToUpper

            Case "CALCULATION TYPE"
                Dim strCalculationType As String = GetCalculationType()

                If Not IsNothing(strCalculationType) Then
                    Me.TextBuilder.ReplaceLink(link, strCalculationType, "Calculation Type").Tag = strCalculationType
                End If

            Case "VARIABLE NAME"
                Dim strVariableName As String = Me.GetValueFromTree(OutputVariablesDataTree.TreeTypeEnum.Variables)

                If Not IsNothing(strVariableName) Then
                    Me.TextBuilder.ReplaceLink(link, strVariableName, "Variable Name").Tag = strVariableName

                End If

            Case "EVENT NAME 1", "EVENT NAME 2", "EVENT NAME 3"
                Dim strEventName As String = Me.GetValueFromTree(OutputVariablesDataTree.TreeTypeEnum.Events)

                If Not IsNothing(strEventName) Then
                    Me.TextBuilder.ReplaceLink(link, strEventName, link.HyperTextData).Tag = strEventName

                End If

            Case "LAST N"

                Dim intLastNValue As Integer = GetLastNValue(IIf(IsNothing(link.Tag), 1, link.Tag))

                If intLastNValue > 0 Then
                    Me.TextBuilder.ReplaceLink(link, "Last " & intLastNValue.ToString(), "Last n").Tag = intLastNValue

                End If

            Case "ALIAS NAME"
                Dim strAlias As String = GetAliasName()

                If Not IsNothing(strAlias) And (Not strAlias = "") Then
                    Me.TextBuilder.ReplaceLink(link, strAlias, "Alias Name").Tag = strAlias

                End If

        End Select

    End Sub

    Private Sub SetHyperTextLinks()
        Me.TextBuilder.AddHyperText("Calculation Type", "Calculation Type")
        Me.TextBuilder.AddHyperText("Variable Name", "Variable Name")
        Me.TextBuilder.AddHyperText("Last n", "Last n")
        Dim EventName1 As CSGeneral.HyperTextLink = Me.TextBuilder.AddHyperText("Event Name 1", "Event Name 1")
        Dim EventName2 As CSGeneral.HyperTextLink = Me.TextBuilder.AddHyperText("Event Name 2", "Event Name 2", EventName1.StartPosition + 1)
        Me.TextBuilder.AddHyperText("Event Name 3", "Event Name 3", EventName2.StartPosition + 1)
        Me.TextBuilder.AddHyperText("Alias Name", "Alias Name")
    End Sub


    Public Function VariableTemplateOK() As String
        ' Gets the template variable string and checks its validity, returning the parsed line if successfull

        Dim strReturnValue As String = ""
        Dim strErrorMessage As String = ""
        Dim EventName2Link As CSGeneral.HyperTextLink = Nothing


        For Each link As CSGeneral.HyperTextLink In Me.TextBuilder.HyperTextCollection

            Select Case link.HyperTextData.ToUpper

                Case "CALCULATION TYPE"
                    If Not UserHasSetLink(link.Text, link.HyperTextData) Then
                        strErrorMessage = "Calculation Type" & vbCrLf
                    Else
                        strReturnValue = strReturnValue.Insert(link.StartPosition, link.Tag & " of ")
                    End If

                Case "VARIABLE NAME"
                    If Not UserHasSetLink(link.Text, link.HyperTextData) Then
                        strErrorMessage += "Variable Name" & vbCrLf
                    Else
                        strReturnValue = strReturnValue.Insert(link.StartPosition, link.Tag & " on ")
                    End If

                Case "LAST N"
                    If Not IsNothing(link.Tag) Then
                        strReturnValue += "last " & link.Tag & " "
                    End If

                Case "EVENT NAME 1"
                    If Not UserHasSetLink(link.Text, link.HyperTextData) Then
                        strErrorMessage += "Event Name 1" & vbCrLf
                    Else
                        strReturnValue += link.Tag & " "
                    End If

                Case "EVENT NAME 2"
                    EventName2Link = link

                Case "EVENT NAME 3"
                    If Not IsNothing(link.Tag) And Not IsNothing(EventName2Link.Tag) Then
                        strReturnValue += "from " & EventName2Link.Tag & " to " & link.Tag & " "

                    ElseIf IsNothing(link.Tag) And IsNothing(EventName2Link.Tag) Then
                        ' do nothing

                    Else
                        strErrorMessage += "Either Event Name 2 and Event Name 3, or both"
                    End If

                Case "ALIAS NAME"
                    If Not IsNothing(link.Tag) Then
                        strReturnValue += "as " & link.Tag

                    End If

            End Select
        Next

        If strErrorMessage <> "" Then
            MessageBox.Show("The following sections have not been set:" & vbCrLf & vbCrLf & strErrorMessage, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return ""

        End If

        Return strReturnValue

    End Function

    Private Function GetCalculationType() As String
        'Asks the user to choose a calculation type from a list box and returns that value

        Dim alistbox As New ListBoxDialog

        With alistbox.ListView
            .SmallImageList = Me.SmallDialogImages
            .Items.Add("Sum", 0)
            .Items.Add("Value", 1)
            .Items.Add("Count", 2)
            .Items.Add("Average", 3)
            .Items.Add("Maximum", 4)
            .Items.Add("Minimum", 5)

            .MultiSelect = False


        End With

        alistbox.Location = Windows.Forms.Cursor.Position
        alistbox.Size = New System.Drawing.Size(112, 150)

        alistbox.Top += 10

        If alistbox.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            Return alistbox.ListView.SelectedItems(0).Text
        Else
            Return ""
        End If
    End Function

    Private Function GetValueFromTree(ByVal TreeType As OutputVariablesDataTree.TreeTypeEnum) As String
        'Asks the user to choose a variable name from the tree view

        Dim VariableTree As New OutputVariablesForm(Me.Controller, TreeType)

        With VariableTree
            .Location = Windows.Forms.Cursor.Position
            .Top += 10

        End With

        If VariableTree.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Return VariableTree.VariableName
        Else
            Return ""
        End If


    End Function



    Private Function GetAliasName() As String
        'display an input dialog box to retrieve and alias name

        Dim aDialogBox As New VBGeneral.InputDialog

        With aDialogBox
            .ShowInTaskbar = False
            .StartPosition = FormStartPosition.Manual
            .ControlBox = False
            .Location = Windows.Forms.Cursor.Position
            .Width = 300
            .Top += 10
        End With

        If aDialogBox.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            Return aDialogBox.Value.Trim.Replace(" ", "_")
        Else
            Return ""
        End If

    End Function

    Private Function GetLastNValue(Optional ByVal StartValue As Integer = 1) As Integer
        'Shows a slider bar between 1 to 50

        Dim NumberSlider As New TrackerSlider

        With NumberSlider
            .TrackerNumber.Minimum = 1
            .TrackerNumber.Maximum = 50
            .Location = Windows.Forms.Cursor.Position
            .Top += 10
            .TrackerNumber.Value = StartValue

        End With


        If NumberSlider.ShowDialog() = Windows.Forms.DialogResult.OK Then Return NumberSlider.TrackerNumber.Value


    End Function


    Private Function UserHasSetLink(ByVal linkText As String, ByVal linkData As String) As String

        Return Not linkText.ToUpper = linkData.ToUpper
    End Function

    Private Sub TrackerTemplatePanel_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs)
        ControlPaint.DrawBorder3D(e.Graphics, e.ClipRectangle, Border3DStyle.Etched)
    End Sub

    Private Sub LinkLabel1_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked

    End Sub
End Class