Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.Xml

' This class encapsulates the way we use xml to pass information around.

Public Class APSIMData
    Private Node As XmlNode

    Sub New(ByRef DataNode As XmlNode)
        Node = DataNode
    End Sub
    Sub New(ByVal XMLString As String)
        Dim data As New XmlDocument
        data.LoadXml(XMLString)
        Node = data.DocumentElement
    End Sub
    Sub New(ByVal type As String, ByVal name As String)
        name = name.Replace("&", "&amp;")
        Dim XMLString As String
        XMLString = "<" + type
        If name <> "" Then
            XMLString = XMLString + " name = """ + name + """"
        End If
        XMLString = XMLString + "/>"

        Dim data As New XmlDocument
        data.LoadXml(XMLString)
        Node = data.DocumentElement
    End Sub
    ReadOnly Property Parent() As APSIMData
        Get
            Dim A As New APSIMData(Node.ParentNode)
            If A.Type = "#document" Then
                Return Nothing
            Else
                Return A
            End If

        End Get
    End Property
    Function Child(ByVal ChildName As String) As APSIMData
        For Each ChildData As APSIMData In Me.Children
            If LCase(ChildName) = LCase(ChildData.Name) Then
                Return ChildData
            End If
        Next
        Return Nothing
    End Function
    Public Sub Clear()
        For Each Child As String In ChildList()
            Delete(Child)
        Next
    End Sub
    Function FindChild(ByVal ChildPath As String, Optional ByVal Delimiter As Char = "|") As APSIMData
        Dim name As String
        Dim CurrentData As New APSIMData(Node)
        Dim Path As String = ChildPath

        Do Until Path = ""

            If InStr(Path, Delimiter) <> 0 Then
                name = Left$(Path, InStr(Path, Delimiter) - 1)
                Path = Mid$(Path, InStr(Path, Delimiter) + 1)
            Else
                name = Path
                Path = ""
            End If

            CurrentData = CurrentData.Child(name)
            If IsNothing(CurrentData) Then
                Exit Do
            End If
        Loop

        If IsNothing(CurrentData) Then
            Throw New System.Exception("Cannot find child " + ChildPath)
        Else
            Return CurrentData
        End If
    End Function
    Function ChildList(Optional ByVal type As String = Nothing) As StringCollection
        Dim List As New StringCollection
        For Each child As APSIMData In Me.Children(type)
            List.Add(child.Name)
        Next
        Return List

    End Function
    Property Value() As String
        Get
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource = "library" + "|" + Me.Attribute("shortcut")
                Return New APSIMData(Node.OwnerDocument.DocumentElement).FindChild(RemoteSource, "|").Value
            Else

                Return Node.InnerText.Replace("%apsuite", APSIMSettings.ApsimDirectory())
            End If
        End Get
        Set(ByVal value As String)
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource = "library" + "|" + Me.Attribute("shortcut")
                Dim RootNode As New APSIMData(Node.OwnerDocument.DocumentElement)
                RootNode.FindChild(RemoteSource, "|").Value = value
            Else
                Dim InvalidChars As String = "&<>"
                If value.IndexOfAny(InvalidChars.ToCharArray()) <> -1 Then
                    Dim cdata As XmlCDataSection = Node.OwnerDocument.CreateCDataSection(value)
                    'Node.AppendChild(cdata)
                    Node.InnerXml = cdata.OuterXml
                Else
                    Node.InnerXml = value
                End If
            End If
        End Set
    End Property
    Property Values(ByVal ChildName As String) As StringCollection
        Get
            Dim ReturnValues As New StringCollection
            For Each Child As APSIMData In Children(ChildName)
                ReturnValues.Add(Child.Value)
            Next
            Return ReturnValues
        End Get
        Set(ByVal Values As StringCollection)
            Clear()
            For Each Value As String In Values
                Dim NewNode As New APSIMData(ChildName)
                NewNode.Value = Value
                Add(NewNode)
            Next
        End Set
    End Property

    Function AttributeExists(ByVal AttributeName As String) As Boolean
        Dim A As XmlAttribute = Node.Attributes.GetNamedItem(AttributeName)
        return Not IsNothing(A)
    End Function

    Function Attribute(ByVal AttributeName As String) As String
        Dim A As XmlAttribute = Node.Attributes.GetNamedItem(AttributeName)
        If Not IsNothing(A) Then
            Return A.InnerText
        Else
            Return ""
        End If
    End Function


    Sub SetAttribute(ByVal AttributeName As String, ByVal AttributeValue As String)
        Dim attr As XmlNode = Node.OwnerDocument.CreateNode(XmlNodeType.Attribute, AttributeName, "")
        attr.Value = AttributeValue
        Node.Attributes.SetNamedItem(attr)
    End Sub
    ReadOnly Property Type() As String
        Get
            Return Node.Name
        End Get
    End Property
    Property XML() As String
        Get
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource = "library" + "|" + Me.Attribute("shortcut")
                Return New APSIMData(Node.OwnerDocument.DocumentElement).FindChild(RemoteSource, "|").XML
            Else
                Return Node.OuterXml()
            End If
        End Get
        Set(ByVal value As String)
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource = "library" + "|" + Me.Attribute("shortcut")
                Dim RootNode As New APSIMData(Node.OwnerDocument.DocumentElement)
                RootNode.FindChild(RemoteSource, "|").XML = value
            Else
                Dim newnode As New APSIMData(value)
                Node.InnerXml = newnode.Node.InnerXml

            End If

        End Set
    End Property
    ReadOnly Property InnerXML() As String
        Get
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource = "library" + "|" + Me.Attribute("shortcut")
                Return New APSIMData(Node.OwnerDocument.DocumentElement).FindChild(RemoteSource, "|").InnerXML
            Else
                Return Node.InnerXml()
            End If
        End Get
    End Property
    Public Sub Add(ByVal Data As APSIMData)
        If IsNothing(Data) Then
            ' Do Nothing
        ElseIf Me.Attribute("shortcut") <> "" Then
            MsgBox("Cannot add data to a short cut.  You must add this data to the data source in the library.", MsgBoxStyle.Critical, "User Error")
        Else
            Dim NewName as string = UniqueName(Data.Name, ChildList)
            if NewName <> Data.Name then
                Data.Name = NewName
            End If

            Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
            Node.AppendChild(newnode)
        End If
    End Sub
    Public Sub AddBefore(ByVal Data As APSIMData, ByVal ReferenceNode As APSIMData)
        If IsNothing(Data) Then
            ' Do Nothing
        ElseIf Me.Attribute("shortcut") <> "" Then
            MsgBox("Cannot add data to a short cut.  You must add this data to the data source in the library.", MsgBoxStyle.Critical, "User Error")
        Else
            Data.Name = UniqueName(Data.Name, ChildList)
            Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
            Node.InsertBefore(newnode, ReferenceNode.Node)
        End If
    End Sub
    Public Sub Delete(ByVal ChildName As String)
        If Me.Attribute("shortcut") <> "" Then
            MsgBox("Cannot delete data from a short cut.  You must delete this data from the data source in the library.", MsgBoxStyle.Critical, "User Error")
        Else
            Node.RemoveChild(Child(ChildName).Node)
        End If

    End Sub
    Private Function UniqueName(ByVal ProposedName As String, ByVal UsedNames As StringCollection) As String

        Dim NewName As String
        Dim Found As Boolean = False
        Dim counter As Integer = 0

        Do
            Found = False
            counter = counter + 1

            If counter = 1 Then
                NewName = ProposedName
            Else
                'NewName = ProposedName + "(" + Trim(Str(counter)) + ")"
                NewName = ProposedName + "{" + Trim(Str(counter)) + "}"
            End If
            For Each name As String In UsedNames
                If name = NewName Then
                    Found = True
                    Exit For
                End If
            Next

        Loop Until Not Found

        UniqueName = NewName


    End Function
    Property Name() As String
        Get
            Dim AttributeNode As XmlNode = Node.Attributes.GetNamedItem("name")
            If AttributeNode Is Nothing Then
                Return Node.Name
            Else
                Return AttributeNode.Value
            End If
        End Get
        Set(ByVal Value As String)
            SetAttribute("name", Value)
        End Set
    End Property
    ReadOnly Property Children(Optional ByVal Type As String = Nothing) As System.Collections.ArrayList
        Get
            Dim ChildrenCollection As New ArrayList
            If Node Is Nothing Then
                ' do nothing
            Else
                If Me.Attribute("shortcut") = "" Then
                    ' No shortcut so return MY children
                    Dim i As Integer
                    For i = 0 To Node.ChildNodes.Count - 1
                        Dim nodename As String
                        nodename = Node.ChildNodes(i).Name
                        If nodename <> "#text" And nodename <> "#comment" Then
                            If Type Is Nothing Or Type = nodename Then
                                ChildrenCollection.Add(New APSIMData(Node.ChildNodes(i)))
                            End If
                        End If
                    Next i

                Else
                    ' There is a shortcut so return REMOTE children
                    Dim RemoteSource = "library" + "|" + Me.Attribute("shortcut")
                    ChildrenCollection = New APSIMData(Node.OwnerDocument.DocumentElement).FindChild(RemoteSource, "|").Children
                End If


            End If
            Return ChildrenCollection

        End Get
    End Property
    Property DataTable() As DataTable
        Get
            Dim DT As New DataTable
            DT.Columns.Add("Name", System.Type.GetType("System.String"))
            DT.Columns.Add("Value", System.Type.GetType("System.String"))

            For Each child As APSIMData In Me.Children
                Dim r As DataRow
                r = DT.NewRow
                r("Name") = child.Name
                r("Value") = child.Value
                DT.Rows.Add(r)
            Next
            Return DT
        End Get
        Set(ByVal Value As DataTable)

            For Each row As DataRow In Value.Rows
                Me.Child(row("Name")).Value = row("Value")
            Next

        End Set
    End Property
End Class
