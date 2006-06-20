Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.Xml
Imports System.IO

' ----------------------------------------------------------------------
' This class encapsulates the way we use xml to pass information around.
' ----------------------------------------------------------------------
Public Class APSIMData
    Private Node As XmlNode
    Delegate Sub DataChangedEventHandler()
    Public Event DataChanged As DataChangedEventHandler

    ' --------------------
    ' constructors
    ' --------------------
    Sub New()
        Node = Nothing
    End Sub
    Private Sub New(ByRef DataNode As XmlNode, ByVal ChangedHandler As DataChangedEventHandler)
        Node = DataNode
        AddHandler DataChanged, ChangedHandler
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


    ' ----------------------------
    ' Load from specified file
    ' ----------------------------
    Public Function LoadFromFile(ByVal FileName As String) As Boolean
        Dim MyFileName As String = Path.GetFullPath(FileName)

        If File.Exists(MyFileName) Then
            Dim data As New XmlDocument
            data.Load(MyFileName)
            Node = data.DocumentElement
            RaiseEvent DataChanged()
            Return True
        Else
            MsgBox("Cannot find file: " + FileName)
            Return False
        End If
    End Function


    ' ----------------------------
    ' Save to the specified file
    ' ----------------------------
    Public Sub SaveToFile(ByVal FileName As String)
        Node.OwnerDocument.Save(FileName)
    End Sub




    ' ------------------------------------------------
    ' Return parent node data or nothing if root node
    ' ------------------------------------------------
    ReadOnly Property Parent() As APSIMData
        Get
            Dim A As New APSIMData(Node.ParentNode, DataChangedEvent)
            If A.Type = "#document" Then
                Return Nothing
            Else
                Return A
            End If
        End Get
    End Property


    ' -----------------------------------------------
    ' Return child node data or nothing if not found
    ' -----------------------------------------------
    Function Child(ByVal ChildName As String) As APSIMData
        For Each ChildData As APSIMData In Me.Children
            If LCase(ChildName) = LCase(ChildData.Name) Then
                Return ChildData
            End If
        Next
        Return Nothing
    End Function


    ' -------------------------------
    ' Clear all children nodes
    ' -------------------------------
    Public Sub Clear()
        For Each Child As String In ChildList()
            Delete(Child)
        Next
    End Sub


    ' --------------------------------------------------
    ' Find and return a specific child from a child path.
    ' --------------------------------------------------
    Function FindChild(ByVal ChildPath As String, Optional ByVal Delimiter As Char = "|") As APSIMData
        Dim name As String
        Dim CurrentData As New APSIMData(Node, DataChangedEvent)
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
                Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                Return New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent).FindChild(RemoteSource, "|").Value
            Else

                Return Node.InnerText.Replace("%apsuite", APSIMSettings.ApsimDirectory())
            End If
        End Get
        Set(ByVal value As String)
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                Dim RootNode As New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent)
                RootNode.FindChild(RemoteSource, "|").Value = value
            Else
                Dim InvalidChars As String = "&<>"
                If value.IndexOfAny(InvalidChars.ToCharArray()) <> -1 Then
                    Dim cdata As XmlCDataSection = Node.OwnerDocument.CreateCDataSection(value)
                    value = cdata.OuterXml
                End If
                If Node.InnerXml <> value Then
                    Node.InnerXml = value
                    RaiseEvent DataChanged()
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
        Return Not IsNothing(A)
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
        If Attribute(AttributeName) <> AttributeValue Then
            Dim attr As XmlNode = Node.OwnerDocument.CreateNode(XmlNodeType.Attribute, AttributeName, "")
            attr.Value = AttributeValue
            Node.Attributes.SetNamedItem(attr)
            RaiseEvent DataChanged()
        End If
    End Sub
    ReadOnly Property Type() As String
        Get
            Return Node.Name
        End Get
    End Property
    Property XML() As String
        Get
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                Return New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent).FindChild(RemoteSource, "|").XML
            Else
                Return Node.OuterXml()
            End If
        End Get
        Set(ByVal value As String)
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                Dim RootNode As New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent)
                RootNode.FindChild(RemoteSource, "|").XML = value
            Else
                Dim newnode As New APSIMData(value)
                If Node.InnerXml <> newnode.Node.InnerXml Then
                    Node.InnerXml = newnode.Node.InnerXml
                    RaiseEvent DataChanged()
                End If
            End If
            RaiseEvent DataChanged()
        End Set
    End Property
    Property InnerXML() As String
        Get
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                Return New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent).FindChild(RemoteSource, "|").InnerXML
            Else
                Return Node.InnerXml()
            End If
        End Get
        Set(ByVal Value As String)
            Node.InnerXml = Value
        End Set
    End Property

    ' ----------------------------
    ' Save to the specified stream
    ' ----------------------------
    Public Shared Function FormatXML(ByVal XML As String) As String
        Dim Data As New APSIMData("<dummy>" + XML + "</dummy>")
        Dim TextWriter As New StringWriter
        Dim Out As New XmlTextWriter(TextWriter)
        Out.Formatting = Formatting.Indented
        Data.Node.WriteContentTo(Out)
        Return TextWriter.ToString
    End Function

    Public Function Add(ByVal Data As APSIMData) As APSIMData
        If Not IsNothing(Data) Then
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                Dim ParentData As APSIMData = New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent)
                ParentData = ParentData.FindChild(RemoteSource, "|")
                If IsNothing(ParentData) Then
                    Throw New System.Exception("Cannot find shared node.")
                End If
                Return ParentData.Add(Data)
            Else
                Dim NewName As String = UniqueName(Data.Name, ChildList)
                If NewName <> Data.Name Then
                    Data.Name = NewName
                End If

                Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
                Node.AppendChild(newnode)
                RaiseEvent DataChanged()
                Return New APSIMData(newnode, DataChangedEvent)
            End If
        End If
        Return Nothing
    End Function
    Public Function AddBefore(ByVal Data As APSIMData, ByVal ReferenceNode As APSIMData) As APSIMData
        If Not IsNothing(Data) Then
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                Dim ParentData As APSIMData = New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent)
                ParentData = ParentData.FindChild(RemoteSource, "|")
                If IsNothing(ParentData) Then
                    Throw New System.Exception("Cannot find shared node.")
                End If
                Return ParentData.AddBefore(Data, ReferenceNode)
            Else
                Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
                newnode = Node.InsertBefore(newnode, ReferenceNode.Node)
                Dim NewData As APSIMData = New APSIMData(newnode, DataChangedEvent)
                NewData.Name = UniqueName(Data.Name, ChildList)
                RaiseEvent DataChanged()
                Return NewData
            End If
        Else
            Return Nothing
        End If
    End Function
    Public Function AddAfter(ByVal Data As APSIMData, ByVal ReferenceNode As APSIMData) As APSIMData
        If Not IsNothing(Data) Then
            If Me.Attribute("shortcut") <> "" Then
                Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                Dim ParentData As APSIMData = New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent)
                ParentData = ParentData.FindChild(RemoteSource, "|")
                If IsNothing(ParentData) Then
                    Throw New System.Exception("Cannot find shared node.")
                End If
                Return ParentData.AddAfter(Data, ReferenceNode)
            Else
                Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
                newnode = Node.InsertAfter(newnode, ReferenceNode.Node)
                Dim NewData As APSIMData = New APSIMData(newnode, DataChangedEvent)
                NewData.Name = UniqueName(Data.Name, ChildList)
                RaiseEvent DataChanged()
                Return NewData
            End If
        Else
            Return Nothing
        End If
    End Function
    Public Sub Delete(ByVal ChildName As String)
        If Me.Attribute("shortcut") <> "" Then
            Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
            Dim ParentData As APSIMData = New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent)
            ParentData = ParentData.FindChild(RemoteSource, "|")
            If IsNothing(ParentData) Then
                Throw New System.Exception("Cannot find shared node.")
            End If
            ParentData.Delete(ChildName)
        Else
            Node.RemoveChild(Child(ChildName).Node)
            RaiseEvent DataChanged()
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
            If Me.Attribute("shortcut") <> "" Then
                Return Me.Attribute("shortcut")
            Else
                Dim AttributeNode As XmlNode = Node.Attributes.GetNamedItem("name")
                If AttributeNode Is Nothing Then
                    Return Node.Name
                Else
                    Return AttributeNode.Value
                End If
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
                            Dim AddChild As Boolean = IsNothing(Type)
                            If Not AddChild Then
                                If Type.ToLower() = nodename.ToLower() Then
                                    AddChild = True
                                End If
                            End If
                            If AddChild Then
                                ChildrenCollection.Add(New APSIMData(Node.ChildNodes(i), DataChangedEvent))
                            End If
                        End If
                    Next i

                Else
                    ' There is a shortcut so return REMOTE children
                    Dim RemoteSource As String = "shared" + "|" + Me.Attribute("shortcut")
                    ChildrenCollection = New APSIMData(Node.OwnerDocument.DocumentElement, DataChangedEvent).FindChild(RemoteSource, "|").Children(Type)
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


    ' -----------------------------------------------------
    ' Return a child value to caller or blank if not found.
    ' -----------------------------------------------------
    Property ChildValue(ByVal key As String) As String
        Get
            Try
                Return FindChild(key, "|").Value
            Catch e As System.Exception
                Return ""
            End Try
        End Get
        Set(ByVal Value As String)
            Dim name As String
            Dim CurrentData As New APSIMData(Node, DataChangedEvent)
            Dim Path As String = key

            Do Until Path = ""
                If InStr(Path, "|") <> 0 Then
                    name = Left$(Path, InStr(Path, "|") - 1)
                    Path = Mid$(Path, InStr(Path, "|") + 1)
                Else
                    name = Path
                    Path = ""
                End If

                Dim Child As APSIMData = CurrentData.Child(name)
                If IsNothing(Child) Then
                    CurrentData.Add(New APSIMData(name, ""))
                    CurrentData = CurrentData.Child(name)
                Else
                    CurrentData = Child
                End If
            Loop
            CurrentData.Value = Value
        End Set
    End Property


    ' -----------------------------------------------------
    ' Return a child value to caller. Shows MsgBox on error
    ' -----------------------------------------------------
    Property ChildValueWithError(ByVal key As String) As String
        Get
            Dim Value As String = ChildValue(key)
            If Value = "" Then
                MsgBox("Error in returning value for child: " + Trim(key), MsgBoxStyle.Critical, "Error")
            End If
            Return Value
        End Get
        Set(ByVal Value As String)
            Try
                Child(Trim(key)).Value = Value
                RaiseEvent DataChanged()
            Catch e As System.Exception
                MsgBox("Error in setting value for child: " + Trim(key), MsgBoxStyle.Critical, "Error")
            End Try
        End Set
    End Property


    Public Sub MoveUp(ByVal ChildName As String, ByVal ChildType As String)
        Dim ChildData As APSIMData = Child(ChildName)

        Dim ReferenceNode As XmlNode = ChildData.Node.PreviousSibling()
        While (ChildType <> "" And ReferenceNode.Name.ToLower <> ChildType.ToLower)
            ReferenceNode = ReferenceNode.PreviousSibling()
        End While
        If Not IsNothing(ReferenceNode) Then
            Node.InsertBefore(ChildData.Node, ReferenceNode)
            RaiseEvent DataChanged()
        End If
    End Sub

    Public Sub MoveDown(ByVal ChildName As String, ByVal ChildType As String)
        Dim ChildData As APSIMData = Child(ChildName)

        Dim ReferenceNode As XmlNode = ChildData.Node.NextSibling()
        While (ChildType <> "" And ReferenceNode.Name.ToLower <> ChildType.ToLower)
            ReferenceNode = ReferenceNode.NextSibling()
        End While

        If Not IsNothing(ReferenceNode) Then
            Node.InsertAfter(ChildData.Node, ReferenceNode)
            RaiseEvent DataChanged()
        End If
    End Sub


End Class
