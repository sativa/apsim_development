Imports System.Collections
Imports System.Collections.Specialized
Imports DataTreeControl
Imports General
Public Class UIManager
    Private _UI As BaseUI
    Private _UIPanel As Panel
    Private _SimulationExplorer As DataTree
    Private _MainForm As Form

    Sub New()

    End Sub
    WriteOnly Property MainForm() As Form
        Set(ByVal Value As Form)
            _MainForm = Value
        End Set
    End Property
    WriteOnly Property UIPanel() As Panel
        Set(ByVal Value As Panel)
            _UIPanel = Value
        End Set
    End Property
    WriteOnly Property SimulationExplorer() As DataTree
        Set(ByVal Value As DataTree)
            _SimulationExplorer = Value
        End Set
    End Property

    Public Sub ShowUI(ByVal APSIMData As APSIMData)

        Try
            ' Get rid of existing UI if it is there
            If _UI Is Nothing Then
                'Dont need to do anthing here
            Else
                _UI.SaveToAPSIMFile()
                _UI.Close()
            End If

            ' Find out what type of UI is required
            Dim nodetype As String = APSIMData.Type

            Select Case LCase(nodetype)
                Case "outputfile"
                    _UI = New OutputFileUI
                Case "simulation"
                    _UI = New SimulationUI
                Case "metfile"
                    _UI = New MetUI
                Case "tracker"
                    _UI = New TrackerUI
                Case "soil"
                    _UI = New SoilUI
                Case "area"
                    _UI = New areaui
                Case "toolbox"
                    _UI = New explorerui
                Case "summaryfile"
                    _UI = New SummaryFileUI
                Case "logic"
                    _UI = New LogicUI
                Case "crop"
                    _UI = New CropUI
                Case "sheep"
                    _UI = New SheepUI
                Case "stock"
                    _UI = New StockUI
                Case Else
                    _UI = New ParameterUI
            End Select

            _UI.Data = APSIMData

            _UI.UIManager = Me

            _UI.TopLevel = False
            _UI.Parent = _UIPanel
            _UI.Show()
            _UI.setup()


        Catch e As Exception
            MsgBox("Error loading new UI component", MsgBoxStyle.Critical, "Error")
        End Try
    End Sub

    'Public Sub FillSimulationExplorer(ByVal APSIMData As APSIMData)
    '    Try
    '        _SimulationExplorer.Nodes.Clear()
    '        ' Ultimately this needs to be replaced with a simulation name - especially when sim files can have >1 simulation

    '        'Dim newnode As TreeNode = _SimulationExplorer.Nodes.Add(APSIMData.Name)

    '        buildtree(APSIMData, Nothing)
    '        _SimulationExplorer.Expand()

    '    Catch e As Exception
    '        MsgBox(e.Message, MsgBoxStyle.Critical, "Error building simulation tree")
    '    End Try
    'End Sub
    'Private Sub buildtree(ByVal data As APSIMData, ByRef parentnode As TreeNode)
    '    Try

    '        'Dim type As String = data.DataType
    '        'Dim addthis As Boolean = False
    '        'Dim openindex As Integer
    '        'Dim closedindex As Integer
    '        'Select Case LCase(type)
    '        '    Case "simulation", "simulations", "toolbox"
    '        '        addthis = True
    '        '        openindex = 0
    '        '        closedindex = 1
    '        '    Case "soil", "outputfile", "metfile", "tracker", "area"
    '        '        addthis = True
    '        '        openindex = 2
    '        '        closedindex = 3
    '        '    Case Else
    '        '        addthis = False
    '        'End Select
    '        'If addthis Then
    '        '    Dim childnode As TreeNode = parentnode.Nodes.Add(data.Name)
    '        '    childnode.ImageIndex = closedindex
    '        '    childnode.SelectedImageIndex = openindex

    '        '    Dim ChildList As New StringCollection
    '        '    ChildList = data.ChildList

    '        '    For Each item As String In ChildList
    '        '        Dim child As APSIMData = data.Child(item)
    '        '        buildtree(child, childnode)
    '        '    Next
    '        'End If

    '        ' NOTE - Path must be fully qualified name 
    '        Dim ChildList As New StringCollection
    '        ChildList = data.ChildList

    '        Dim item As String
    '        For Each item In ChildList
    '            Dim child As APSIMData = data.Child(item)

    '            Dim type As String = child.Type
    '            Dim addthis As Boolean = False
    '            Dim openindex As Integer
    '            Dim closedindex As Integer
    '            Select Case LCase(type)
    '                Case "simulation", "simulations"
    '                    addthis = True
    '                    openindex = 0
    '                    closedindex = 1
    '                Case "soil", "outputfile", "metfile", "tracker", "area", "summaryfile"
    '                    addthis = True
    '                    openindex = 2
    '                    closedindex = 3
    '                Case Else
    '                    addthis = False
    '            End Select
    '            If addthis Then
    '                Dim childnode As TreeNode
    '                If parentnode Is Nothing Then
    '                    childnode = _SimulationExplorer.Nodes.Add(item)
    '                Else
    '                    childnode = parentnode.Nodes.Add(item)
    '                End If

    '                childnode.ImageIndex = closedindex
    '                childnode.SelectedImageIndex = openindex

    '                buildtree(child, childnode)

    '            End If
    '        Next

    '    Catch e As Exception
    '        MsgBox("Error building tree for : " + data.Name + vbCrLf + vbCrLf + e.Message, MsgBoxStyle.Critical, "Error building simulation tree")
    '    End Try
    'End Sub
    'Public Sub AddComponent(ByVal path As String, ByVal datastring As String)
    '    '        APSIMFile.AddData(path, datastring)
    '    '        FillSimulationExplorer()
    '    '        ShowChanged()
    'End Sub
    Public Sub SaveDocument(ByVal APSIMFile As APSIMFile)
        If _UI Is Nothing Then
            ' dont save anything back
        Else
            _UI.SaveToAPSIMFile()
            ShowChanged(APSIMFile)
        End If
    End Sub
    Private Sub ShowChanged(ByVal APSIMFile As APSIMFile)
        _MainForm.Text = APSIMFile.caption
    End Sub
    Public Function RenameComponent(ByVal datapath As String, ByVal newname As String) As Boolean
        If _UI Is Nothing Then
            ' do nothing
            RenameComponent = False
        Else
            _UI.Data.Name = newname
            RenameComponent = True
        End If

    End Function

End Class
