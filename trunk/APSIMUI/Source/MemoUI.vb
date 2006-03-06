Public Class MemoUI
    Inherits VBGeneral.BaseView

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

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
    Friend WithEvents Memo As CSGeneral.MemoUI
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.Memo = New CSGeneral.MemoUI
        Me.SuspendLayout()
        '
        'Memo
        '
        Me.Memo.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Memo.Location = New System.Drawing.Point(0, 40)
        Me.Memo.Name = "Memo"
        Me.Memo.Size = New System.Drawing.Size(762, 525)
        Me.Memo.TabIndex = 2
        '
        'MemoUI
        '
        Me.Controls.Add(Me.Memo)
        Me.Name = "MemoUI"
        Me.Size = New System.Drawing.Size(762, 565)
        Me.Controls.SetChildIndex(Me.Memo, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub MemoUI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        MyBase.HelpText = "Add notes to simulation components here.  A date/time stamp can be added by " & _
            "clicking the 'Add' button below."

    End Sub

    Public Overrides Sub Save()
        'Encode rtf syntax to base64 and save out to xml file.  Encoding to base64 overcomes
        'issues with xml keyword syntax.
        Controller.Data.Value = CSGeneral.CSUtility.EncodeStringToBase64(Me.Memo.TextRichFormat)
    End Sub

    Public Overrides Sub Refresh()
        'update the memo field with rich text syntax, after decoding the string from base64.
        Me.Memo.TextRichFormat(CSGeneral.CSUtility.EncodeBase64ToString(Controller.Data.InnerXML))
    End Sub
End Class
