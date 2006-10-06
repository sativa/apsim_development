Imports System
Imports System.Xml
Imports System.IO
Imports System.Collections.Specialized

Public Class APSIMData
    ' ----------------------------------------------------------------------
    ' This class encapsulates the way we use xml to pass information around.
    ' ----------------------------------------------------------------------
    Private InternalNode As XmlNode
    Private InUpdate As Boolean = False
    Private InInternalUpdate As Boolean = False
    Private Delimiter As String = "\"
    Private ChildrenList As APSIMData()
    Private ChildrenLoaded As Boolean = False

    Delegate Sub DataChangedEventHandler(ByVal ChangedData As APSIMData)
    Public Event DataChanged As DataChangedEventHandler

    Public Sub New()
        ' -----------------------------------------------------------------------
        ' constructor taking no arguments - creates a completely empty APSIMData
        ' -----------------------------------------------------------------------
        InternalNode = Nothing
    End Sub
    Private Sub New(ByRef DataNode As XmlNode, ByVal ChangedHandler As DataChangedEventHandler)
        ' -----------------------------------------------------------------------
        ' internal constructor 
        ' -----------------------------------------------------------------------
        InternalNode = DataNode
        AddHandler DataChanged, ChangedHandler
    End Sub
    Public Sub New(ByVal XMLString As String)
        ' -----------------------------------------------------------------------
        ' constructor taking a single XML argument
        ' -----------------------------------------------------------------------
        Dim data As New XmlDocument
        data.LoadXml(XMLString)
        InternalNode = data.DocumentElement
    End Sub
    Public Sub New(ByVal type As String, ByVal name As String)
        ' -----------------------------------------------------------------------
        ' constructor taking a type and a name as arguments.
        ' -----------------------------------------------------------------------
        name = name.Replace("&", "&amp;")
        Dim XMLString As String
        XMLString = "<" + type
        If name <> "" And name <> type Then
            XMLString = XMLString + " name = """ + name + """"
        End If
        XMLString = XMLString + "/>"

        Dim data As New XmlDocument
        data.LoadXml(XMLString)
        InternalNode = data.DocumentElement
    End Sub

    Public Property PathDelimiter() As String
        ' ---------------------------------------------------
        ' Allows caller to change the delimiter used to find
        ' a node
        ' ---------------------------------------------------
        Get
            Return Delimiter
        End Get
        Set(ByVal value As String)
            Delimiter = value
        End Set
    End Property
    Private ReadOnly Property Node() As XmlNode
        ' -----------------------------------------------------------------------
        ' Returns an XmlNode that takes shortcuts into account.
        ' -----------------------------------------------------------------------
        Get
            If Not IsNothing(InternalNode.Attributes) Then
                Dim ShortCut As XmlAttribute = InternalNode.Attributes.GetNamedItem("shortcut")
                If Not IsNothing(ShortCut) Then
                    Dim TopNode As APSIMData = New APSIMData(InternalNode.OwnerDocument.DocumentElement, DataChangedEvent)
                    Dim ShortCutPath As String = TopNode.Name + Delimiter + "shared" + Delimiter + ShortCut.InnerText
                    Dim SharedChild As APSIMData = TopNode.Find(ShortCutPath)
                    Return SharedChild.InternalNode
                End If
            End If
            Return InternalNode
        End Get
    End Property
    Public Sub BeginUpdate()
        ' -----------------------------------------------------------------------
        ' Signals that a whole chunk of stuff is about to change. Don't fire 
        ' any events until EndUpdate is called.
        ' -----------------------------------------------------------------------
        InUpdate = True
    End Sub
    Public Sub EndUpdate()
        ' -----------------------------------------------------------------------
        ' Signals that updating has finished. Events can now be fired.
        ' -----------------------------------------------------------------------
        InUpdate = False
        FireDataChangedEvent()
    End Sub
    Private Sub FireDataChangedEvent()
        ' -----------------------------------------------------------------------
        ' If BeginUpdate hasn't been called then fire off a DataChanged event.
        ' -----------------------------------------------------------------------
        If Not InUpdate And Not InInternalUpdate Then
            RaiseEvent DataChanged(Me)
        End If

    End Sub
    Private Sub BeginInternalUpdate()
        ' -----------------------------------------------------------------------
        ' Signals that a whole chunk of stuff is about to change. Don't fire 
        ' any events until EndUpdate is called.
        ' -----------------------------------------------------------------------
        InInternalUpdate = True
    End Sub
    Private Sub EndInternalUpdate()
        ' -----------------------------------------------------------------------
        ' Signals that updating has finished. Events can now be fired.
        ' -----------------------------------------------------------------------
        InInternalUpdate = False
        ChildrenLoaded = False   'stuff has been added, the Children property is not in the correct state. (Speed Profiling)
        FireDataChangedEvent()
    End Sub
    Public Property Name() As String
        ' -----------------------------------------------------------------------
        ' Provides read and write access to the name of this data.
        ' -----------------------------------------------------------------------
        Get
            Dim NameAttribute As String = Attribute("name")
            If NameAttribute = "" Then
                Return InternalNode.Name
            Else
                Return NameAttribute
            End If
        End Get
        Set(ByVal NewName As String)
            SetAttribute("name", NewName)
        End Set
    End Property
    Public ReadOnly Property Type() As String
        ' -----------------------------------------------------------------------
        ' Provides read access to the type of this data.
        ' -----------------------------------------------------------------------
        Get
            Return Node.Name
        End Get
    End Property
    Public Function LoadFromFile(ByVal FileName As String) As Boolean
        ' ----------------------------
        ' Load from specified file
        ' ----------------------------
        Dim MyFileName As String = Path.GetFullPath(FileName)

        If File.Exists(MyFileName) Then
            Dim data As New XmlDocument
            data.Load(MyFileName)
            InternalNode = data.DocumentElement
            FireDataChangedEvent()
            Return True
        Else
            Throw New Exception("Cannot file file " + MyFileName)
        End If
    End Function
    Public Sub SaveToFile(ByVal FileName As String)
        ' ----------------------------
        ' Save to the specified file
        ' ----------------------------
        Node.OwnerDocument.Save(FileName)
    End Sub
    Public ReadOnly Property Parent() As APSIMData
        ' ------------------------------------------------
        ' Return parent node data or nothing if root node
        ' ------------------------------------------------
        Get
            Dim A As New APSIMData(InternalNode.ParentNode, DataChangedEvent)
            If A.Type = "#document" Then
                Return Nothing
            Else
                Return A
            End If
        End Get
    End Property
    ReadOnly Property Children(Optional ByVal ChildTypeFilter As String = Nothing) As APSIMData()
        ' ------------------------------------------------
        ' Return an array of children.
        ' ------------------------------------------------
        Get
            If Not IsNothing(ChildrenList) Then
                If Node.ChildNodes.Count <> ChildrenList.Length Then
                    ChildrenLoaded = False
                End If
            End If
            'If (Not ChildrenLoaded) Or ChildTypeFilter <> "" Then
            Dim ReturnList(Node.ChildNodes.Count - 1) As APSIMData
            Dim NumSoFar As Integer = 0
            For i As Integer = 0 To Node.ChildNodes.Count - 1
                Dim ChildType As String = Node.ChildNodes(i).Name
                If ChildType <> "#text" And ChildType <> "#comment" And ChildType <> "#cdata-section" Then
                    If IsNothing(ChildTypeFilter) OrElse ChildTypeFilter.ToLower() = ChildType.ToLower() Then
                        ReturnList(NumSoFar) = New APSIMData(Node.ChildNodes(i), DataChangedEvent)
                        NumSoFar += 1
                    End If
                End If
            Next
            Array.Resize(ReturnList, NumSoFar)
            ChildrenList = ReturnList
            ChildrenLoaded = True
            Return ReturnList
            'Else
            'Return ChildrenList
            'End If
        End Get
    End Property
    Public Function Child(ByVal ChildName As String) As APSIMData
        ' ------------------------------------------------------------------------------
        ' Return a specific child with the specified name
        ' Returns Nothing if not found.
        ' ------------------------------------------------------------------------------
        For Each ChildNode As APSIMData In Children
            If ChildNode.Name.ToLower = ChildName.ToLower Then
                Return ChildNode
            End If
        Next
        Return Nothing
    End Function
    Public Property ChildValue(ByVal ChildName As String) As String
        ' ------------------------------------------------------------------------------
        ' Gets and sets the value of child element. Getter and Setter can handle
        ' the child not existing.
        ' ------------------------------------------------------------------------------
        Get
            Dim LocalChild As APSIMData = Find(Name + Delimiter + ChildName)
            If IsNothing(LocalChild) Then
                Return ""
            Else
                Return LocalChild.Value
            End If
        End Get
        Set(ByVal value As String)
            Dim LocalChild As APSIMData = Child(ChildName)
            If LocalChild.Value <> value Then
                BeginInternalUpdate()
                If IsNothing(LocalChild) Then
                    LocalChild = Add(New APSIMData(ChildName, ""))
                End If
                LocalChild.Value = value
                EndInternalUpdate()
            End If
        End Set
    End Property
    Public Function ChildByType(ByVal ChildType As String) As APSIMData
        ' ------------------------------------------------------------------------------
        ' Return a specific child with the specified type
        ' Returns Nothing if not found.
        ' ------------------------------------------------------------------------------
        For Each ChildNode As APSIMData In Children
            If ChildNode.Type.ToLower = ChildType.ToLower Then
                Return ChildNode
            End If
        Next
        Return Nothing
    End Function
    Public Function Find(ByVal FullPath As String) As APSIMData
        ' --------------------------------------------------------
        ' Find a specific data node from the specified full path.
        ' Full path must be a fully qualified path using '\'
        ' as a delimiter. It must include the name of this data
        ' e.g. RootNode\ChildNode\SubChildNode
        ' --------------------------------------------------------
        Dim PosDelimiter As Integer = FullPath.IndexOf(Delimiter)
        If PosDelimiter = -1 Then
            If FullPath.ToLower() = Name.ToLower() Then
                Return Me
            End If
        Else
            If FullPath.Substring(0, PosDelimiter).ToLower() = Name.ToLower() Then
                For Each ChildNode As APSIMData In Children
                    Dim ReturnData As APSIMData = ChildNode.Find(FullPath.Substring(PosDelimiter + 1))
                    If Not IsNothing(ReturnData) Then
                        Return ReturnData
                    End If
                Next
            End If
        End If
        Return Nothing
    End Function
    '*****************************************************************************
    Function FindChildByType(ByVal ChildPath As String, Optional ByVal Delimiter As Char = "|") As APSIMData
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

            CurrentData = CurrentData.Type1(name)
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
    ' -----------------------------------------------
    ' Return child node data or nothing if not found
    ' -----------------------------------------------
    Function Type1(ByVal TypeName As String) As APSIMData
        For Each ChildData As APSIMData In Me.Children
            If LCase(TypeName) = LCase(ChildData.Type) Then
                Return ChildData
            End If
        Next
        Return Nothing
    End Function
    '*************************************************************
    Public ReadOnly Property FullPath() As String
        Get
            ' --------------------------------------------------------
            ' Return a full path for this data node using '\' as a 
            ' delimiter.
            ' --------------------------------------------------------
            Dim LocalData As APSIMData = Me
            Dim Path As String = LocalData.Name
            LocalData = LocalData.Parent
            While Not IsNothing(LocalData)
                Path = LocalData.Name + Delimiter + Path
                LocalData = LocalData.Parent
            End While
            Return Path
        End Get
    End Property
    Public Sub Clear()
        ' -------------------------------
        ' Clear all children nodes
        ' -------------------------------
        Node.RemoveAll()
        ChildrenLoaded = False
        FireDataChangedEvent()
    End Sub
    Public Function ChildNames(Optional ByVal type As String = Nothing) As String()
        ' -------------------------------------
        ' Clear a list of all child names
        ' -------------------------------------
        Dim ChildNodes() As APSIMData = Children(type)
        Dim Names(ChildNodes.Length - 1) As String
        For i As Integer = 0 To ChildNodes.Length - 1
            Names(i) = ChildNodes(i).Name
        Next
        Return Names
    End Function
    Public Property Value() As String
        ' -----------------------------------------------------------------
        ' Provides access (read and write) to the value for this data node
        ' -----------------------------------------------------------------
        Get
            Return Node.InnerText.Replace("%apsuite", APSIMSettings.ApsimDirectory())
        End Get
        Set(ByVal value As String)
            Dim InvalidChars As String = "&<>"
            If value.IndexOfAny(InvalidChars.ToCharArray()) <> -1 Then
                Dim cdata As XmlCDataSection = Node.OwnerDocument.CreateCDataSection(value)
                value = cdata.OuterXml
            End If
            If Node.InnerXml <> value Then
                Node.InnerXml = value
                FireDataChangedEvent()
            End If
        End Set
    End Property
    Public Function AttributeExists(ByVal AttributeName As String) As Boolean
        ' -----------------------------------------------------------------
        ' Return true if the specified attribute exists
        ' -----------------------------------------------------------------
        Return Not IsNothing(InternalNode.Attributes.GetNamedItem(AttributeName))
    End Function
    Public Function Attribute(ByVal AttributeName As String) As String
        ' -----------------------------------------------------------------
        ' Return the specified attribute or "" if not found
        ' -----------------------------------------------------------------
        Dim A As XmlAttribute = InternalNode.Attributes.GetNamedItem(AttributeName)
        If Not IsNothing(A) Then
            Return A.InnerText
        Else
            Return ""
        End If
    End Function
    Public Sub SetAttribute(ByVal AttributeName As String, ByVal AttributeValue As String)
        ' ----------------------------------------
        ' Set the value of the specified attribute
        ' ----------------------------------------
        If Attribute(AttributeName) <> AttributeValue Then
            Dim attr As XmlNode = InternalNode.OwnerDocument.CreateNode(XmlNodeType.Attribute, AttributeName, "")
            attr.Value = AttributeValue
            InternalNode.Attributes.SetNamedItem(attr)
            FireDataChangedEvent()
        End If
    End Sub
    Public Sub DeleteAttribute(ByVal AttributeName As String)
        ' ----------------------------------------
        ' Delete the specified attribute
        ' ----------------------------------------
        Dim A As XmlAttribute = InternalNode.Attributes.GetNamedItem(AttributeName)
        If Not IsNothing(A) Then
            InternalNode.Attributes.Remove(A)
            ChildrenLoaded = False
            FireDataChangedEvent()
        End If
    End Sub
    Public Property XML() As String
        ' ----------------------------------------
        ' Return this node's XML
        ' ----------------------------------------
        Get
            Return Node.OuterXml()
        End Get
        Set(ByVal value As String)
            Node.InnerXml = value
            FireDataChangedEvent()
        End Set
    End Property
    Public Property InnerXML() As String
        ' ----------------------------------------
        ' Return this node's Inner XML
        ' ----------------------------------------
        Get
            Return Node.InnerXml()
        End Get
        Set(ByVal Value As String)
            Node.InnerXml = Value
            FireDataChangedEvent()
        End Set
    End Property
    Public Shared Function FormatXML(ByVal XML As String) As String
        ' -------------------------------------------------
        ' Format the specified XML using indentation etc.
        ' -------------------------------------------------
        Dim Data As New APSIMData("<dummy>" + XML + "</dummy>")
        Dim TextWriter As New StringWriter
        Dim Out As New XmlTextWriter(TextWriter)
        Out.Formatting = Formatting.Indented
        Data.Node.WriteContentTo(Out)
        Return TextWriter.ToString
    End Function
    Public Function Add(ByVal Data As APSIMData) As APSIMData
        ' -------------------------------------------------
        ' Add the specified data to this node.
        ' -------------------------------------------------
        If Not IsNothing(Data) Then
            BeginInternalUpdate()
            Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
            Dim NewData As APSIMData = New APSIMData(Node.AppendChild(newnode), DataChangedEvent)
            EndInternalUpdate()
            Return NewData
        Else
            Return Nothing
        End If
    End Function
    Public Function AddBefore(ByVal Data As APSIMData, ByVal ReferenceNode As APSIMData) As APSIMData
        ' -------------------------------------------------------------------------
        ' Add the specified data to this node before the specified reference node
        ' -------------------------------------------------------------------------
        If Not IsNothing(Data) Then
            BeginInternalUpdate()
            Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
            Dim NewData = New APSIMData(Node.InsertBefore(newnode, ReferenceNode.Node), DataChangedEvent)
            EndInternalUpdate()
            Return NewData
        Else
            Return Nothing
        End If
    End Function
    Public Function AddAfter(ByVal Data As APSIMData, ByVal ReferenceNode As APSIMData) As APSIMData
        ' -------------------------------------------------------------------------
        ' Add the specified data to this node after the specified reference node
        ' -------------------------------------------------------------------------
        If Not IsNothing(Data) Then
            BeginInternalUpdate()
            Dim newnode As XmlNode = Node.OwnerDocument.ImportNode(Data.Node, True)
            Dim NewData = New APSIMData(Node.InsertAfter(newnode, ReferenceNode.Node), DataChangedEvent)
            NewData.EnsureNameIsUnique()
            EndInternalUpdate()
            Return NewData
        Else
            Return Nothing
        End If
    End Function
    Public Sub Delete(ByVal ChildName As String)
        ' ---------------------------------
        ' Delete the specified child
        ' ---------------------------------
        InternalNode.RemoveChild(Child(ChildName).InternalNode)
        ChildrenLoaded = False
        FireDataChangedEvent()
    End Sub
    Public Sub EnsureNameIsUnique()
        ' ------------------------------------------------
        ' Ensure our name is unique among our siblings.
        ' ------------------------------------------------
        If Not IsNothing(Parent) Then
            Dim BaseName As String = Name
            Dim Found As Boolean = False
            Dim counter As Integer = 0
            Dim Siblings() As String = Parent.ChildNames

            For i As Integer = 1 To 100000
                Dim Count As Integer = 0
                For Each Sibling As String In Siblings
                    If Sibling.ToLower = Name.ToLower Then
                        Count += 1
                    End If
                Next
                If Count <= 1 Then
                    Return
                Else
                    Name = BaseName + "{" + i.ToString + "}"
                End If
            Next
            Throw New Exception("Internal error in APSIMData.CalcUniqueName")
        End If
    End Sub
    Public Sub MoveUp(ByVal ChildName As String, ByVal ChildType As String)
        ' -------------------------------------------------------------------------
        ' Move the specified child up. If ChildType is specified then the child
        ' will be moved above the next sibling that matches 'ChildType'
        ' -------------------------------------------------------------------------
        Dim ChildData As APSIMData = Child(ChildName)

        Dim ReferenceNode As XmlNode = ChildData.Node.PreviousSibling()
        While (ChildType <> "" And ReferenceNode.Name.ToLower <> ChildType.ToLower)
            ReferenceNode = ReferenceNode.PreviousSibling()
        End While
        If Not IsNothing(ReferenceNode) Then
            Node.InsertBefore(ChildData.Node, ReferenceNode)
            ChildrenLoaded = False
            FireDataChangedEvent()
        End If
    End Sub
    Public Sub MoveDown(ByVal ChildName As String, ByVal ChildType As String)
        ' -------------------------------------------------------------------------
        ' Move the specified child down. If ChildType is specified then the child
        ' will be moved below the next sibling that matches 'ChildType'
        ' -------------------------------------------------------------------------
        Dim ChildData As APSIMData = Child(ChildName)

        Dim ReferenceNode As XmlNode = ChildData.Node.NextSibling()
        While (ChildType <> "" And ReferenceNode.Name.ToLower <> ChildType.ToLower)
            ReferenceNode = ReferenceNode.NextSibling()
        End While

        If Not IsNothing(ReferenceNode) Then
            Node.InsertAfter(ChildData.Node, ReferenceNode)
            ChildrenLoaded = False
            FireDataChangedEvent()
        End If
    End Sub
    Public Sub EnsureNumberOfChildren(ByVal ChildType As String, ByVal ChildName As String, ByVal NumChildren As Integer)
        ' -------------------------------------------------------------------------
        ' Ensure there are the specified number of children with the speciifed type
        ' -------------------------------------------------------------------------
        Dim ChildrenNames() As String = ChildNames(ChildType)
        Dim NumChildrenToAdd As Integer = NumChildren - ChildrenNames.Length
        Dim NumChildrenToDelete As Integer = ChildrenNames.Length - NumChildren
        If NumChildrenToAdd > 0 Or NumChildrenToDelete > 0 Then
            BeginInternalUpdate()
        End If
        For i As Integer = 1 To NumChildrenToAdd
            Add(New APSIMData(ChildType, ChildName))
        Next
        For i As Integer = 1 To NumChildrenToDelete
            Delete(ChildrenNames(ChildrenNames.Length - 1))
        Next
        If NumChildrenToAdd > 0 Or NumChildrenToDelete > 0 Then
            EndInternalUpdate()
        End If
    End Sub


End Class
