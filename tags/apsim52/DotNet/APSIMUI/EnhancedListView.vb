Public Class EnhancedListView
    Inherits System.Windows.Forms.ListView
    Dim GraphicFile As String

#Region " Component Designer generated code "

    Public Sub New()
        MyBase.New()

        ' This call is required by the Component Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        SetStyle(ControlStyles.UserPaint, True)

        SetStyle(ControlStyles.ResizeRedraw, True)

        SetStyle(ControlStyles.DoubleBuffer, True)
        SetStyle(ControlStyles.AllPaintingInWmPaint, True)
    End Sub

    'Control overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Control Designer
    Private components As System.ComponentModel.IContainer

    ' NOTE: The following procedure is required by the Component Designer
    ' It can be modified using the Component Designer.  Do not modify it
    ' using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()

    End Sub

#End Region
    Public Property graphic() As String
        Get
            Return GraphicFile
        End Get
        Set(ByVal Value As String)
            GraphicFile = Value
            Me.Refresh()
        End Set
    End Property
    Protected Overrides Sub OnPaint(ByVal pe As System.Windows.Forms.PaintEventArgs)


        MyBase.OnPaint(pe)

        'Add your custom paint code here
        Dim x As New Bitmap(GraphicFile)

        pe.Graphics.DrawImage(x, 0, 0)

    End Sub

End Class
