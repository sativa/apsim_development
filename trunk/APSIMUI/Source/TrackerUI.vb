Public Class TrackerUI
    Inherits VBGeneral.BaseView

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        Me.SetHyperTextLinks()



    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents TrackerListBox As System.Windows.Forms.ListBox
    Friend WithEvents RemoveButton As System.Windows.Forms.Button
    Friend WithEvents SmallDialogImages As System.Windows.Forms.ImageList
    Friend WithEvents TrackerTemplatePanel As System.Windows.Forms.Panel
    Friend WithEvents TextBuilder As CSGeneral.HyperTextLabel
    Friend WithEvents lblTrackerTemplatePanel As System.Windows.Forms.Label
    Friend WithEvents AddButton As System.Windows.Forms.Button
    Friend WithEvents ResetButton As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(TrackerUI))
        Me.TrackerListBox = New System.Windows.Forms.ListBox
        Me.ResetButton = New System.Windows.Forms.Button
        Me.RemoveButton = New System.Windows.Forms.Button
        Me.SmallDialogImages = New System.Windows.Forms.ImageList(Me.components)
        Me.TrackerTemplatePanel = New System.Windows.Forms.Panel
        Me.TextBuilder = New CSGeneral.HyperTextLabel
        Me.AddButton = New System.Windows.Forms.Button
        Me.lblTrackerTemplatePanel = New System.Windows.Forms.Label
        Me.TrackerTemplatePanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'TrackerListBox
        '
        Me.TrackerListBox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TrackerListBox.Location = New System.Drawing.Point(0, 200)
        Me.TrackerListBox.Name = "TrackerListBox"
        Me.TrackerListBox.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
        Me.TrackerListBox.Size = New System.Drawing.Size(1106, 3033)
        Me.TrackerListBox.TabIndex = 0
        '
        'ResetButton
        '
        Me.ResetButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ResetButton.Image = CType(resources.GetObject("ResetButton.Image"), System.Drawing.Image)
        Me.ResetButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.ResetButton.Location = New System.Drawing.Point(776, 64)
        Me.ResetButton.Name = "ResetButton"
        Me.ResetButton.Size = New System.Drawing.Size(80, 28)
        Me.ResetButton.TabIndex = 2
        Me.ResetButton.Text = "Reset"
        Me.ResetButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'RemoveButton
        '
        Me.RemoveButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.RemoveButton.Image = CType(resources.GetObject("RemoveButton.Image"), System.Drawing.Image)
        Me.RemoveButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.RemoveButton.Location = New System.Drawing.Point(688, 40)
        Me.RemoveButton.Name = "RemoveButton"
        Me.RemoveButton.Size = New System.Drawing.Size(96, 28)
        Me.RemoveButton.TabIndex = 3
        Me.RemoveButton.Text = "Remove"
        Me.RemoveButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.RemoveButton.Visible = False
        '
        'SmallDialogImages
        '
        Me.SmallDialogImages.ImageSize = New System.Drawing.Size(16, 16)
        Me.SmallDialogImages.ImageStream = CType(resources.GetObject("SmallDialogImages.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.SmallDialogImages.TransparentColor = System.Drawing.Color.Transparent
        '
        'TrackerTemplatePanel
        '
        Me.TrackerTemplatePanel.Controls.Add(Me.TextBuilder)
        Me.TrackerTemplatePanel.Controls.Add(Me.AddButton)
        Me.TrackerTemplatePanel.Controls.Add(Me.ResetButton)
        Me.TrackerTemplatePanel.Location = New System.Drawing.Point(0, 72)
        Me.TrackerTemplatePanel.Name = "TrackerTemplatePanel"
        Me.TrackerTemplatePanel.Size = New System.Drawing.Size(1015, 104)
        Me.TrackerTemplatePanel.TabIndex = 8
        '
        'TextBuilder
        '
        Me.TextBuilder.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextBuilder.LinkBehaviour = System.Windows.Forms.LinkBehavior.SystemDefault
        Me.TextBuilder.LinkColour = System.Drawing.Color.FromArgb(CType(0, Byte), CType(0, Byte), CType(255, Byte))
        Me.TextBuilder.Location = New System.Drawing.Point(24, 24)
        Me.TextBuilder.Name = "TextBuilder"
        Me.TextBuilder.Size = New System.Drawing.Size(696, 40)
        Me.TextBuilder.TabIndex = 10
        Me.TextBuilder.Text = "Calculation Type of Variable Name on [Last n] Event Name 1 [from Event Name 2 to " & _
        "Event Name 3] [as Alias Name]"
        Me.TextBuilder.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'AddButton
        '
        Me.AddButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.AddButton.Image = CType(resources.GetObject("AddButton.Image"), System.Drawing.Image)
        Me.AddButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.AddButton.Location = New System.Drawing.Point(864, 64)
        Me.AddButton.Name = "AddButton"
        Me.AddButton.Size = New System.Drawing.Size(80, 28)
        Me.AddButton.TabIndex = 12
        Me.AddButton.Text = "Add ..."
        Me.AddButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'lblTrackerTemplatePanel
        '
        Me.lblTrackerTemplatePanel.Font = New System.Drawing.Font("Tahoma", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblTrackerTemplatePanel.ForeColor = System.Drawing.Color.MediumBlue
        Me.lblTrackerTemplatePanel.Location = New System.Drawing.Point(20, 61)
        Me.lblTrackerTemplatePanel.Name = "lblTrackerTemplatePanel"
        Me.lblTrackerTemplatePanel.Size = New System.Drawing.Size(132, 24)
        Me.lblTrackerTemplatePanel.TabIndex = 9
        Me.lblTrackerTemplatePanel.Text = "Template Builder"
        Me.lblTrackerTemplatePanel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'TrackerUI
        '
        Me.Controls.Add(Me.lblTrackerTemplatePanel)
        Me.Controls.Add(Me.TrackerTemplatePanel)
        Me.Controls.Add(Me.RemoveButton)
        Me.Controls.Add(Me.TrackerListBox)
        Me.Name = "TrackerUI"
        Me.Size = New System.Drawing.Size(958, 721)
        Me.Controls.SetChildIndex(Me.TrackerListBox, 0)
        Me.Controls.SetChildIndex(Me.RemoveButton, 0)
        Me.Controls.SetChildIndex(Me.TrackerTemplatePanel, 0)
        Me.Controls.SetChildIndex(Me.lblTrackerTemplatePanel, 0)
        Me.TrackerTemplatePanel.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region


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

        Me.TrackerTemplatePanel.Refresh()


    End Sub

    Private Function GetAliasName() As String
        'display an input dialog box to retrieve and alias name

        Dim aDialogBox As New VBGeneral.InputDialog

        With aDialogBox
            .ShowInTaskbar = False
            .StartPosition = FormStartPosition.Manual
            .ControlBox = False
            .Location = Cursor.Position
            .Width = 300
            .Top += 10



        End With

        If aDialogBox.ShowDialog(Me) = DialogResult.OK Then Return aDialogBox.Value.Trim.Replace(" ", "_")

    End Function

    Private Function GetLastNValue(Optional ByVal StartValue As Integer = 1) As Integer
        'Shows a slider bar between 1 to 50

        Dim NumberSlider As New TrackerSlider

        With NumberSlider
            .TrackerNumber.Minimum = 1
            .TrackerNumber.Maximum = 50
            .Location = Cursor.Position
            .Top += 10
            .TrackerNumber.Value = StartValue

        End With


        If NumberSlider.ShowDialog() = DialogResult.OK Then Return NumberSlider.TrackerNumber.Value


    End Function

    Private Function GetValueFromTree(ByVal TreeType As OutputVariablesDataTree.TreeTypeEnum) As String
        'Asks the user to choose a variable name from the tree view

        Dim VariableTree As New OutputVariablesForm(Me.Controller, TreeType)

        With VariableTree
            .Location = Cursor.Position
            .Top += 10

        End With

        If VariableTree.ShowDialog() = DialogResult.OK Then Return VariableTree.VariableName

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

        alistbox.Location = Cursor.Position
        alistbox.Size = New System.Drawing.Size(112, 150)

        alistbox.Top += 10

        If alistbox.ShowDialog(Me) = DialogResult.OK Then Return alistbox.ListView.SelectedItems(0).Text

    End Function

    Private Sub TrackerTemplatePanel_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles TrackerTemplatePanel.Paint
        ControlPaint.DrawBorder3D(e.Graphics, e.ClipRectangle, Border3DStyle.Etched)
    End Sub

    Private Sub AddButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddButton.Click

        Dim strParsedVariable As String = VariableTemplateOK()

        If Not strParsedVariable = "" Then
            Me.TrackerListBox.Items.Add(strParsedVariable)
            Me.ResetButton.PerformClick()

        End If
    End Sub

    Private Function VariableTemplateOK() As String
        ' Gets the template variable string and checks its validity, returning the parsed line if successfull

        Dim strReturnValue As String = ""
        Dim strErrorMessage As String = ""
        Dim EventName2Link As CSGeneral.HyperTextLink


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

    Private Function UserHasSetLink(ByVal linkText As String, ByVal linkData As String)

        Return Not linkText.ToUpper = linkData.ToUpper
    End Function

    Private Sub ResetButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ResetButton.Click

        Me.TextBuilder.ClearHyperTextLinks()
        Me.TextBuilder.Text = "Calculation Type of Variable Name on [Last n] Event Name 1 [from Event Name 2 to Event Name 3] [as Alias Name]"
        Me.SetHyperTextLinks()



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

    Private Sub TrackerListBox_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TrackerListBox.KeyPress


    End Sub

    Private Sub TrackerListBox_KeyUp(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TrackerListBox.KeyUp
        If Not e.KeyData = Keys.Delete Then Exit Sub

        Dim prompt As String = "Do you wish to deleted the selected variables?"

        If MessageBox.Show(prompt, "Deleting Selected Variables", MessageBoxButtons.YesNo, MessageBoxIcon.Information, MessageBoxDefaultButton.Button2) = DialogResult.Yes Then
            For Each strItem As String In Me.TrackerListBox.SelectedItems
                MessageBox.Show(strItem)

            Next
        End If
    End Sub

    Public Overrides Sub Save()
        Me.Controller.Data.Clear()

        For Each strVariable As String In Me.TrackerListBox.Items
            Dim apData As New VBGeneral.APSIMData("variable", "")
            apData.Value = strVariable

            Me.Controller.Data.Add(apData)

        Next

    End Sub

    Private Sub GetExistingVariables()
        'Retrieves the current list of user defined variables

        Me.TrackerListBox.Items.Clear()

        For Each child As VBGeneral.APSIMData In Me.Controller.Data.Children

            If child.Type.ToLower = "variable" Then
                Me.TrackerListBox.Items.Add(child.Value)

            End If
        Next
    End Sub

    Public Overrides Sub Refresh()
        GetExistingVariables()

    End Sub
End Class
