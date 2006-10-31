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
    Function Child(ByVal ChildName As String) As APSIMData
        Dim found As Boolean = False
        Dim FoundNode As XmlNode
        For Each ChildNode As XmlNode In Node.ChildNodes
            If New APSIMData(ChildNode).Name = ChildName Then
                FoundNode = ChildNode
                found = True
                Exit For
            End If
        Next

        If found Then
            Return New APSIMData(FoundNode)
        Else
            Throw New Exception("Cannot find child " + ChildName)
        End If
    End Function
    Function FindChild(ByVal ChildPath As String) As APSIMData
        Dim name As String
        Dim CurrentNode As XmlNode = Node
        Dim Found As Boolean = False
        Dim Path As String = ChildPath

        Do Until Path = ""

            If InStr(Path, "|") <> 0 Then
                name = Left$(Path, InStr(Path, "|") - 1)
                Path = Mid$(Path, InStr(Path, "|") + 1)
            Else
                name = Path
                Path = ""
            End If
            Found = False
            For Each ChildNode As XmlNode In CurrentNode.ChildNodes
                If New APSIMData(ChildNode).Name = name Then
                    CurrentNode = ChildNode
                    Found = True
                    Exit For
                End If
            Next

        Loop

        If Found Then
            Return New APSIMData(CurrentNode)
        Else
            Throw New Exception("Cannot find child " + ChildPath)
        End If
    End Function

    Function ChildList(Optional ByVal type As String = Nothing) As StringCollection
        Dim Children As New StringCollection
        If Node Is Nothing Then
            ' do nothing
        Else
            Dim i As Integer
            For i = 0 To Node.ChildNodes.Count - 1
                Dim nodename As String
                nodename = Node.ChildNodes(i).Name
                If nodename <> "#text" And nodename <> "#comment" Then
                    If type Is Nothing Or type = nodename Then
                        Children.Add(New APSIMData(Node.ChildNodes(i)).Name)
                    End If
                End If
            Next i
        End If
        Return Children
    End Function
    Property Value() As String
        Get
            Return Node.InnerXml
        End Get
        Set(ByVal value As String)
            Node.InnerXml = value

        End Set
    End Property
    Function Attribute(ByVal AttributeName As String) As String
        Return Node.Attributes.GetNamedItem(AttributeName).InnerText
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
    ReadOnly Property XML() As String
        Get
            Return Node.OuterXml
        End Get
    End Property
    Public Sub Add(ByVal Data As APSIMData)
        Dim NewName As String = UniqueName(Data.Name, ChildList)
        Data.Name = NewName

        Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
        Node.AppendChild(newnode)
    End Sub
    Public Sub Delete(ByVal ChildName As String)
        Node.RemoveChild(Child(ChildName).Node)
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
                NewName = ProposedName + "(" + Trim(Str(counter)) + ")"
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
            Node.Attributes.GetNamedItem("name").Value = Value
        End Set
    End Property



End Class
