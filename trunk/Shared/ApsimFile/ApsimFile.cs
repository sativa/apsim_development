namespace ApsimFile
    {
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Collections.Specialized;
    using System.Text;
    using System.Xml;
    using System.IO;
    using VBGeneral;

    public class ApsimFile
        {
        // ---------------------------------------------
        //   This class encapsulates a .apsim file.
        // ---------------------------------------------
        private XmlDocument doc;
        private bool DataIsDirty = false;
        private bool DataIsReadOnly = false;
        private bool Updating = false;
        private const char Delimiter = '\\';

        public enum NodeStatus { Normal, Inherited, Removed, Broken };
        public delegate void DataChangedDelegate(string NodePath);
        public delegate void DataDirtyDelegate(bool IsDirty);
        public delegate void VoidDelegate();
        public event DataChangedDelegate DataStructureChangedEvent;
        public event DataChangedDelegate DataChangedEvent;
        public event VoidDelegate NewDataEvent;
        public event DataDirtyDelegate DataDirtyChangedEvent;
        public bool IsDirty { get { return DataIsDirty; } }
        private void SetDataIsDirty(bool value)
            {
            if (!Updating && DataIsDirty != value)
                {
                DataIsDirty = value;
                if (DataDirtyChangedEvent != null)
                    DataDirtyChangedEvent.Invoke(DataIsDirty);
                }

            }
        private void SetDataStructureChanged(string NodePathChanged)
            {
            SetDataIsDirty(true);
            if (!Updating && DataStructureChangedEvent != null)
                DataStructureChangedEvent.Invoke(NodePathChanged);
            }
        public bool IsReadOnly { get { return DataIsReadOnly; } }

        public ApsimFile()
            {
            doc = new XmlDocument();
            }
        public string RootNodeName
            {
            get { return NodeName(doc.DocumentElement); }
            }

        public APSIMData AllData
            {
            // --------------------------------------------------------
            // Provides readwrite access to the entire data, usually
            // the contents of a file. This will clear the current
            // selections.
            // --------------------------------------------------------
            get {return new APSIMData(doc.DocumentElement, new APSIMData.DataChangedEventHandler(OnDataChanged));}
            }

        public void Rename(string NodePath, string NewName)
            {
            XmlNode FoundNode = Find(NodePath, doc.DocumentElement, true);
            if (FoundNode != null && NodeName(FoundNode) != NewName)
                {
                SetAttribute(FoundNode, "name", NewName);
                SetDataStructureChanged(NodePath);
                }
            }
        public void Delete(string NodePath)
            {
            XmlNode FoundNode = Find(NodePath, doc.DocumentElement, true);
            if (FoundNode != null && FoundNode.ParentNode != null)
                {
                XmlNode ParentNode = FoundNode.ParentNode;
                ParentNode.RemoveChild(FoundNode);
                SetDataStructureChanged(NodePath);
                }
            else
                throw new Exception("Cannot delete node: " + NodePath);
            }
        public void Delete(StringCollection NodePaths)
            {
            XmlNode ParentNode = null;
            foreach (string NodePath in NodePaths)
                {
                XmlNode FoundNode = Find(NodePath, doc.DocumentElement, true);
                if (FoundNode != null && FoundNode.ParentNode != null)
                    {
                    ParentNode = FoundNode.ParentNode;
                    ParentNode.RemoveChild(FoundNode);
                    }
                }
            if (ParentNode != null)
                SetDataStructureChanged(FullPath(ParentNode));
            }

        public void MoveUp(StringCollection NodePaths)
            {
            if (NodePaths.Count > 0)
                {
                XmlNode FirstNode = Find(NodePaths[0], doc.DocumentElement, true);
                XmlNode ReferenceNode = FirstNode.PreviousSibling;
                if (ReferenceNode != null)
                    {
                    for (int i = 0; i != NodePaths.Count; i++)
                        {
                        XmlNode NodeToMove = Find(NodePaths[i], doc.DocumentElement, true);
                        NodeToMove.ParentNode.InsertBefore(NodeToMove, ReferenceNode);
                        }
                    }
                SetDataStructureChanged(FullPath(FirstNode.ParentNode));
                }
            }
        public void MoveDown(StringCollection NodePaths)
            {
            if (NodePaths.Count > 0)
                {
                XmlNode LastNode = Find(NodePaths[NodePaths.Count - 1], doc.DocumentElement, true);
                XmlNode ReferenceNode = LastNode.NextSibling;
                if (ReferenceNode != null)
                    {
                    for (int i = NodePaths.Count - 1; i >= 0; i--)
                        {
                        XmlNode NodeToMove = Find(NodePaths[i], doc.DocumentElement, true);
                        NodeToMove.ParentNode.InsertAfter(NodeToMove, ReferenceNode);
                        }
                    SetDataStructureChanged(FullPath(LastNode.ParentNode));
                    }
                }
            }

        public APSIMData Find(string NodePath)
            {
            if (NodePath == "")
                return new APSIMData(doc.DocumentElement, new APSIMData.DataChangedEventHandler(OnDataChanged));
            else
                return new APSIMData(Find(NodePath, doc.DocumentElement, true), new APSIMData.DataChangedEventHandler(OnDataChanged));
            }

        public void New()
            {
            // Create a new .apsim file in memory.
            Open("<folder/>", false);
            }
        public void New(string XML)
            {
            // Create a new .apsim file in memory.
            Open(XML, false);
            }
        public void Open(string XML, bool ReadOnly) 
            {
            doc.LoadXml(XML);
            APSIMChangeTool.Upgrade(AllData);
            DataIsReadOnly = ReadOnly;
            SetDataStructureChanged(RootNodeName);
            SetDataIsDirty(false);
            if (NewDataEvent != null)
                NewDataEvent.Invoke();
            }
        public void Save(string FileName)
            {
            // ---------------------------------------------------------
            // Save the contents of this apsim document to the specified 
            // file - formatted nicely.
            // ---------------------------------------------------------
            if (!DataIsReadOnly && FileName != "" && FileName != null)
                {
                doc.Save(FileName);
                SetDataIsDirty(false);
                if (DataChangedEvent != null)
                    DataChangedEvent.Invoke(NodeName(doc.DocumentElement));
                }
            }
        public void SaveAs(string FileName)
            {
            DataIsReadOnly = false;
            Save(FileName);
            }
        public void Save(TextWriter Writer)
            {
            // ---------------------------------------------------------
            // Save the contents of this apsim document to the specified 
            // writer - formatted nicely.
            // ---------------------------------------------------------
            //doc.SaveToStream(Writer);

            XmlTextWriter Out = new XmlTextWriter(Writer);
            Out.Formatting = Formatting.Indented;
            doc.Save(Out);
            }
        public string[] ChildNames(string NodePath) 
            {
            // -------------------------------------------------------------------------
            // Return a list of child names (NOT PATHS) to caller.
            // -------------------------------------------------------------------------
            StringCollection ChildNames = new StringCollection();
            StringCollection DeletedChildNames = new StringCollection();
            Children(NodePath, ChildNames, DeletedChildNames);
            string[] ReturnNames = new string[ChildNames.Count];
            ChildNames.CopyTo(ReturnNames, 0);
            return ReturnNames;
            }
        public void MetaData(string NodePath, out string Type, out NodeStatus Status, out string InheritedFrom)
            {
            // -------------------------------------------------------------------------
            // Return a node description to caller.
            // ------------------------------------------------------------------------
            XmlNode FoundNode = Find(NodePath, doc.DocumentElement, true);
            if (FoundNode == null)
                throw new Exception("Node doesn't exist: " + NodePath);
            Type = FoundNode.Name;
            XmlAttribute InheritedFromAttribute = FoundNode.Attributes["InheritedFrom"];
            if (InheritedFromAttribute != null)
                {
                InheritedFrom = InheritedFromAttribute.Value;
                Status = NodeStatus.Inherited;
                }
            else
                {
                InheritedFrom = "";
                Status = NodeStatus.Normal;
                }
            }
        public string Contents(string NodePath)
            {
            // -------------------------------------------------------------------------
            // Return the contents of a node as a string (OuterXML)
            // -------------------------------------------------------------------------
            XmlNode FoundNode = Find(NodePath, doc.DocumentElement, true);
            if (FoundNode == null)
                throw new Exception("Cannot find node: " + NodePath);

            return FormattedXML(FoundNode);
            }
        public void SetContents(string NodePath, string InnerXml)
            {
            // -------------------------------------------------------------------------
            // Set the contents of a node using the specified innerxml
            // -------------------------------------------------------------------------
            XmlNode Node = EnsureNodeExists(NodePath, doc.DocumentElement);
            Node.InnerXml = InnerXml;
            }
        public void Add(string ParentNodePath, string ChildXML) 
            {
            // --------------------------------------------------------------------
            // Add the specified xml as a child to the specified parent node.
            // --------------------------------------------------------------------
            XmlNode ParentNode = Find(ParentNodePath, doc.DocumentElement, true);
            if (ParentNode == null)
                throw new Exception("Cannot add node to parent node: " + ParentNodePath + ". Node doesn't exist");

            try
                {
                XmlDocument NewDoc = new XmlDocument();
                NewDoc.LoadXml("<dummy>" + ChildXML + "</dummy>");
                foreach (XmlNode Child in NewDoc.DocumentElement.ChildNodes)
                    {
                    XmlNode NewNode = ParentNode.AppendChild(doc.ImportNode(Child, true));
                    EnsureChildIsUnique(NewNode);
                    }
                if (NewDoc.DocumentElement.ChildNodes.Count > 0)
                    {
                    SetDataStructureChanged(ParentNodePath);
                    }
                }
            catch (Exception)
                { }
            }
        public void AddInherited(string ParentNodePath, string ChildType, string ChildName, string InheritedFromNodePath)
            {
            // --------------------------------------------------------------------
            // Add a child node inherited from another node 
            // --------------------------------------------------------------------
            XmlNode ParentNode = Find(ParentNodePath, doc.DocumentElement, true);
            if (ParentNode == null)
                throw new Exception("Cannot add node to parent node: " + ParentNodePath + ". Node doesn't exist");

            XmlNode NewNode = ParentNode.AppendChild(doc.CreateElement(ChildType));
            SetAttribute(NewNode, "name", ChildName);
            SetAttribute(NewNode, "InheritedFrom", InheritedFromNodePath);
            EnsureChildIsUnique(NewNode);
            }
        public void RevertToBase(string NodePath) 
            {
            // -----------------------------------------------------------
            // revert the specified node to the base node
            // -----------------------------------------------------------
            XmlNode FoundNode = Find(NodePath, doc.DocumentElement, true);
            if (FoundNode == null)
                throw new Exception("Cannot revert node: " + NodePath + ". Node not found");
            if (FoundNode.Attributes["InheritedFrom"] == null)
                throw new Exception("Cannot revert a non inherited node: " + NodePath);
            FoundNode.ParentNode.RemoveChild(FoundNode);
            }
        public void Sort(string NodePath)
            {
            XmlNode NodeToSort = Find(NodePath, doc.DocumentElement, false);
            if (NodeToSort != null)
                {
                XmlDocument NewDoc = new XmlDocument();
                NewDoc.AppendChild(NewDoc.ImportNode(NodeToSort, false));
                Sort(NodeToSort, NewDoc.DocumentElement);
                if (NodeToSort.ParentNode == null)
                    doc = NewDoc;
                else
                    {
                    BeginUpdate();
                    while (NodeToSort.HasChildNodes)
                        NodeToSort.RemoveChild(NodeToSort.FirstChild);
                    foreach (XmlNode Child in NewDoc.DocumentElement)
                        NodeToSort.AppendChild(doc.ImportNode(Child, true));
                    EndUpdate();
                    }

                SetDataStructureChanged(NodePath);
                SetDataIsDirty(true);
                }
            }

        // -------------------------------------------------------------------------------
        // -------------------------------------------------------------------------------
        // Low level routines.
        // -------------------------------------------------------------------------------
        // -------------------------------------------------------------------------------

        private void BeginUpdate()
            {
            Updating = true;
            }
        private void EndUpdate()
            {
            Updating = false;
            }

        private string FullPath(XmlNode FoundNode)
            {
            // --------------------------------------------------------
            // Return a full path for this data node using the delimiter
            // --------------------------------------------------------
            XmlNode LocalData = FoundNode;
            string Path = NodeName(LocalData);
            LocalData = LocalData.ParentNode;
            while (LocalData.NodeType != XmlNodeType.Document)
                {
                Path = NodeName(LocalData) + Delimiter + Path;
                LocalData = LocalData.ParentNode;
                }
            return Path;
            }
        private void OnDataChanged(APSIMData ChangedData)
            {
            SetDataIsDirty(true);
            if (!Updating && DataChangedEvent != null)
                DataChangedEvent.Invoke(NodeName(doc.DocumentElement));
            }

        private static string FormattedXML(XmlNode Node)
            {
            StringWriter TextWriter = new StringWriter();
            XmlTextWriter Out = new XmlTextWriter(TextWriter);
            Out.Formatting = Formatting.Indented;
            Node.WriteTo(Out);
            return TextWriter.ToString();
            }
        private string ParentNodePath(string NodePath)
            {
            int PosDelimiter = NodePath.LastIndexOf(Delimiter);
            if (PosDelimiter == -1)
                throw new Exception("Cannot get the parent of the specified node: " + NodePath);
            string ParentName = NodePath.Remove(PosDelimiter);
            if (ParentName == "")
                throw new Exception("Cannot get the parent of the root node");
            return ParentName;
            }
        private XmlNode EnsureChildIsUnique(XmlNode Child)
            {
            // -------------------------------------------------------------
            // Make sure the child's name is unique amongst it's siblings.
            // -------------------------------------------------------------
            string UniqueChildName = NodeName(Child);
            for (int i = 1; i != 10000; i++)
                {
                int Count = 0;
                foreach (XmlNode Sibling in Child.ParentNode.ChildNodes)
                    {
                    if (NodeName(Sibling).ToLower() == UniqueChildName.ToLower())
                        Count++;
                    }
                if (Count == 1)
                    return Child;

                int BraceLocStart = UniqueChildName.IndexOf("{");
                int BraceLocEnd = UniqueChildName.IndexOf("}");
                if (BraceLocStart != -1)
                    UniqueChildName = UniqueChildName.Remove(BraceLocStart);
                SetAttribute(Child, "name", UniqueChildName + "{" + i.ToString() + "}");
                }
            throw new Exception("Cannot find a unique name for child: " + NodeName(Child));
            }

        private void SetAttribute(XmlNode Child, string Name, string Value)
            {
            if (Child.Attributes[Name] == null)
                {
                XmlAttribute Attribute = Child.OwnerDocument.CreateAttribute(null, Name, "");
                Attribute.Value = Value;
                Child.Attributes.SetNamedItem(Attribute);
                }
            else
                Child.Attributes[Name].Value = Value;
            }
        private XmlNode Find(string NodePath, XmlNode ReferenceNode, bool UseInheritance)
            {
            // --------------------------------------------------------
            // Find a specific data node from the specified full path.
            // Full path must be a fully qualified path using 
            // a delimiter. e.g. RootNode\ChildNode\SubChildNode
            // --------------------------------------------------------
            if (NodePath.Length == 0)
                throw new Exception("Cannot pass an empty path to Find");
            
            int PosDelimiter = NodePath.IndexOf(Delimiter);
            string ChildNameToMatch = NodePath;
            if (PosDelimiter != -1)
                ChildNameToMatch = NodePath.Substring(0, PosDelimiter);
            if (ChildNameToMatch == NodeName(ReferenceNode))
                {
                if (PosDelimiter == -1)
                    return ReferenceNode;

                foreach (XmlNode Child in ReferenceNode.ChildNodes)
                    {
                    XmlNode FoundNode = Find(NodePath.Substring(PosDelimiter + 1), Child, UseInheritance);
                    if (FoundNode != null)
                        return FoundNode;
                    }

                // check the inherited base node.
                if (UseInheritance)
                    {
                    XmlAttribute InheritedFrom = ReferenceNode.Attributes["InheritedFrom"];
                    if (InheritedFrom != null)
                        {
                        XmlNode InheritedNode = Find(InheritedFrom.Value, doc.DocumentElement, UseInheritance);
                        if (InheritedNode != null)
                            {
                            string NodePathToFind = NodeName(InheritedNode) + NodePath.Substring(PosDelimiter);
                            return Find(NodePathToFind, InheritedNode, UseInheritance);
                            }
                        }
                    }
                }
            return null;
            }
        private void Children(string NodePath, StringCollection ChildNames, StringCollection DeletedChildNames)
            {
            // -------------------------------------------------------------------------
            // Return a list of child names to caller.
            // -------------------------------------------------------------------------
            XmlNode FoundNode = Find(NodePath, doc.DocumentElement, true);
            if (FoundNode != null)
                {
                foreach (XmlNode Child in FoundNode.ChildNodes)
                    {
                    if (Child.Attributes["status"] != null && Child.Attributes["status"].Value == "removed")
                        DeletedChildNames.Add(NodeName(Child));
                    else if (ChildNames.IndexOf(NodeName(Child)) == -1 && DeletedChildNames.IndexOf(NodeName(Child)) == -1)
                        ChildNames.Add(NodeName(Child));
                    }

                // check for any inherited nodes.
                XmlAttribute InheritedFrom = FoundNode.Attributes["InheritedFrom"];
                if (InheritedFrom != null)
                    Children(InheritedFrom.Value, ChildNames, DeletedChildNames);
                }
            }
        private static string NodeName(XmlNode Node)
            {
            if (Node.Attributes["name"] == null)
                return Node.Name;
            else
                return Node.Attributes["name"].Value;
            }
        private XmlNode EnsureNodeExists(string NodePath, XmlNode ReferenceNode)
            {
            // --------------------------------------------------------
            // Ensure a node exists by creating nodes as necessary
            // for the specified node path.
            // --------------------------------------------------------

            if (NodePath.Length == 0)
                throw new Exception("Cannot pass an empty path to EnsureNodeExists");

            int PosDelimiter = NodePath.IndexOf(Delimiter);
            string ChildNameToMatch = NodePath;
            if (PosDelimiter != -1)
                ChildNameToMatch = NodePath.Substring(0, PosDelimiter);
            if (ChildNameToMatch == NodeName(ReferenceNode))
                {
                if (PosDelimiter == -1)
                    return ReferenceNode;

                foreach (XmlNode Child in ReferenceNode.ChildNodes)
                    {
                    XmlNode FoundNode = EnsureNodeExists(NodePath.Substring(PosDelimiter + 1), Child);
                    if (FoundNode != null)
                        return FoundNode;
                    }

                // check the inherited base node.
                XmlAttribute InheritedFrom = ReferenceNode.Attributes["InheritedFrom"];
                if (InheritedFrom != null)
                    {
                    string BaseChildPath = InheritedFrom.Value + NodePath.Substring(PosDelimiter);
                    XmlNode InheritedNode = Find(BaseChildPath, doc.DocumentElement, false);
                    if (InheritedNode != null)
                        {
                        XmlNode NewChild = ReferenceNode.AppendChild(InheritedNode.CloneNode(true));
                        SetAttribute(NewChild, "InheritedFrom", BaseChildPath);
                        return EnsureNodeExists(NodePath.Substring(PosDelimiter + 1), NewChild);
                        }
                    }
                }
            return null;
            }

        public class XmlNodeComparer : System.Collections.IComparer
            {
            // Calls CaseInsensitiveComparer.Compare with the parameters reversed.
            int System.Collections.IComparer.Compare(Object x, Object y)
                {
                XmlNode yNode = (XmlNode)y;
                XmlNode xNode = (XmlNode)x;
                return ((new CaseInsensitiveComparer()).Compare(NodeName(xNode), NodeName(yNode)));
                }

            }
        private static void Sort(XmlNode Node, XmlNode DestinationNode)
            {
            XmlNode[] SortedNodes = new XmlNode[Node.ChildNodes.Count];
            for (int i = 0; i != Node.ChildNodes.Count; i++)
                {
                SortedNodes[i] = Node.ChildNodes[i];
                }
            Array.Sort(SortedNodes, new XmlNodeComparer());

            foreach (XmlNode Child in SortedNodes)
                {
                DestinationNode.AppendChild(DestinationNode.OwnerDocument.ImportNode(Child, false));
                }

            foreach (XmlNode Child in Node.ChildNodes)
                {
                if (Child.Name.ToLower() == "folder")
                    {
                    // find this child in our destination node.
                    foreach (XmlNode DestChild in DestinationNode.ChildNodes)
                        {
                        if (NodeName(Child) == NodeName(DestChild))
                            Sort(Child, DestChild);
                        }
                    }
                }
            }
        }
    }
