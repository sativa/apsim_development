namespace ApsimFile
    {
    using System;
    using System.Collections.Generic;
    using System.Collections.Specialized;
    using System.Text;
    using System.Xml;
    using System.IO;
    using VBGeneral;

    // ---------------------------------------------
    //   This class encapsulates a .apsim file.
    // ---------------------------------------------
    public class ApsimFile
        {
        private APSIMData doc;
        public enum NodeStatus { Normal, Inherited, Removed, Broken };

        public void New()
            {
            // Create a new .apsim file in memory.
            Open("<folder name=\"Simulations\"/>");
            }
        public void Open(string XML) 
            {
            doc = new APSIMData(XML);
            }
        public void Save(string FileName)
            {
            // ---------------------------------------------------------
            // Save the contents of this apsim document to the specified 
            // file - formatted nicely.
            // ---------------------------------------------------------
            doc.SaveToFile(FileName);
            }
        public void Save(TextWriter Writer)
            {
            // ---------------------------------------------------------
            // Save the contents of this apsim document to the specified 
            // writer - formatted nicely.
            // ---------------------------------------------------------
            doc.SaveToStream(Writer);
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
            APSIMData FoundNode = Find(NodePath, doc, true);
            if (FoundNode == null)
                throw new Exception("Node doesn't exist: " + NodePath);
            Type = FoundNode.Type;
            InheritedFrom = FoundNode.Attribute("InheritedFrom");
            if (InheritedFrom != "")
                Status = NodeStatus.Inherited;
            else
                Status = NodeStatus.Normal;
            }
        public string Contents(string NodePath)
            {
            // -------------------------------------------------------------------------
            // Return the contents of a node as a string (OuterXML)
            // -------------------------------------------------------------------------
            APSIMData FoundNode = Find(NodePath, doc, true);
            if (FoundNode == null)
                throw new Exception("Cannot find node: " + NodePath);
            return FoundNode.ToFormattedXML();
            }
        public void SetContents(string NodePath, string InnerXml)
            {
            // -------------------------------------------------------------------------
            // Set the contents of a node using the specified innerxml
            // -------------------------------------------------------------------------
            APSIMData Node = EnsureNodeExists(NodePath, doc);
            Node.InnerXML = InnerXml;
            }
        public void AddNode(string ParentNodePath, string ChildXML) 
            {
            // --------------------------------------------------------------------
            // Add the specified xml as a child to the specified parent node.
            // --------------------------------------------------------------------
            APSIMData ParentNode = Find(ParentNodePath, doc, true);
            if (ParentNode == null)
                throw new Exception("Cannot add node to parent node: " + ParentNodePath + ". Node doesn't exist");
            APSIMData NewNode = ParentNode.Add(new APSIMData(ChildXML));
            EnsureChildIsUnique(NewNode);
            }
        public void AddInheritedNode(string ParentNodePath, string ChildType, string ChildName, string InheritedFromNodePath)
            {
            // --------------------------------------------------------------------
            // Add a child node inherited from another node 
            // --------------------------------------------------------------------
            APSIMData ParentNode = Find(ParentNodePath, doc, true);
            APSIMData NewNode = ParentNode.Add(new APSIMData(ChildType, ChildName));
            NewNode.SetAttribute("InheritedFrom", InheritedFromNodePath);
            EnsureChildIsUnique(NewNode);
            }
        public void RevertToBase(string NodePath) 
            {
            // -----------------------------------------------------------
            // revert the specified node to the base node
            // -----------------------------------------------------------
            APSIMData FoundNode = Find(NodePath, doc, true);
            if (FoundNode == null)
                throw new Exception("Cannot revert node: " + NodePath + ". Node not found");
            if (FoundNode.Attribute("InheritedFrom") == "")
                throw new Exception("Cannot revert a non inherited node: " + NodePath);
            FoundNode.Parent.Delete(FoundNode.Name);
            }

        // -------------------------------------------------------------------------------
        // -------------------------------------------------------------------------------
        // Low level routines.
        // -------------------------------------------------------------------------------
        // -------------------------------------------------------------------------------

        private string ParentNodePath(string NodePath)
            {
            int PosDelimiter = NodePath.LastIndexOf('/');
            if (PosDelimiter == -1)
                throw new Exception("Cannot get the parent of the specified node: " + NodePath);
            string ParentName = NodePath.Remove(PosDelimiter);
            if (ParentName == "")
                throw new Exception("Cannot get the parent of the root node");
            return ParentName;
            }
        private APSIMData EnsureChildIsUnique(APSIMData ChildNode)
            {
            // -------------------------------------------------------------
            // Make sure the child's name is unique amongst it's siblings.
            // -------------------------------------------------------------
            APSIMData ParentNode = ChildNode.Parent;
            string ChildName = ChildNode.Name;
            for (int i = 1; i != 100000; i++)
                {
                int Count = 0;
                foreach (APSIMData Sibling in ParentNode.get_Children(null))
                    {
                    if (Sibling.Name.ToLower() == ChildName.ToLower())
                        Count++;
                    }
                if (Count == 1)
                    return ChildNode;

                int BraceLocStart = ChildName.IndexOf("{");
                int BraceLocEnd = ChildName.IndexOf("}");
                if (BraceLocStart != -1)
                    ChildName = ChildName.Remove(BraceLocStart);
                ChildNode.SetAttribute("name", ChildName + "{" + i.ToString() + "}");
                }
            throw new Exception("Internal error in APSIMData.CalcUniqueName");
            }
        private APSIMData Find(string NodePath, APSIMData ReferenceNode, bool UseInheritance)
            {
            // --------------------------------------------------------
            // Find a specific data node from the specified full path.
            // Full path must be a fully qualified path using '/'
            // as a delimiter. e.g. /RootNode/ChildNode/SubChildNode
            // --------------------------------------------------------
            if (NodePath.Length == 0)
                throw new Exception("Cannot pass an empty path to Find");
            
            if (NodePath[0] == '/')
                {
                ReferenceNode = doc;
                NodePath = NodePath.Remove(0, 1); 
                }
            int PosDelimiter = NodePath.IndexOf('/');
            string ChildName;
            
            if (PosDelimiter == -1)
                ChildName = NodePath;
            else
                ChildName = NodePath.Substring(0, PosDelimiter);
            if (ChildName == "")
                return ReferenceNode;

            foreach (APSIMData Child in ReferenceNode.get_Children(null))
                {
                if (ChildName.ToLower() == Child.Name.ToLower())
                    {
                    if (PosDelimiter == -1)
                        return Child;
                    else
                        return Find(NodePath.Substring(PosDelimiter+1), Child, UseInheritance);
                   }

                }

            // check the inherited base node.
            if (UseInheritance)
                {
                string InheritedFrom = ReferenceNode.Attribute("InheritedFrom");
                if (InheritedFrom != "")
                    {
                    APSIMData InheritedNode = Find(InheritedFrom, doc, UseInheritance);
                    if (InheritedNode != null)
                        return Find(ChildName, InheritedNode, UseInheritance);
                    }
                }
            return null;
            }
        private void Children(string NodePath, StringCollection ChildNames, StringCollection DeletedChildNames)
            {
            // -------------------------------------------------------------------------
            // Return a list of child names to caller.
            // -------------------------------------------------------------------------
            APSIMData FoundNode = Find(NodePath, null, true);
            if (FoundNode != null)
                {
                foreach (APSIMData Child in FoundNode.get_Children(null))
                    {
                    string ChildName = Child.Name;
                    if (Child.Attribute("status") == "removed")
                        DeletedChildNames.Add(ChildName);
                    else if (ChildNames.IndexOf(ChildName) == -1 && DeletedChildNames.IndexOf(ChildName) == -1)
                        ChildNames.Add(ChildName);
                    }

                // check for any inherited nodes.
                string InheritedFrom = FoundNode.Attribute("InheritedFrom");
                if (InheritedFrom != "")
                    Children(InheritedFrom, ChildNames, DeletedChildNames);
                }
            }
        private APSIMData EnsureNodeExists(string NodePath, APSIMData ReferenceNode)
            {
            // --------------------------------------------------------
            // Ensure a node exists by creating nodes as necessary
            // for the specified node path.
            // --------------------------------------------------------
            if (NodePath.Length == 0)
                throw new Exception("Cannot pass an empty path to EnsureNodeExists");
            
            if (NodePath[0] == '/')
                {
                ReferenceNode = doc;
                NodePath = NodePath.Remove(0, 1); 
                }
            int PosDelimiter = NodePath.IndexOf('/');
            string ChildName;
            
            if (PosDelimiter == -1)
                ChildName = NodePath;
            else
                ChildName = NodePath.Substring(0, PosDelimiter);
            if (ChildName == "")
                return ReferenceNode;

            foreach (APSIMData Child in ReferenceNode.get_Children(null))
                {
                if (ChildName.ToLower() == Child.Name.ToLower())
                    {
                    if (PosDelimiter == -1)
                        return Child;
                    else
                        return EnsureNodeExists(NodePath.Substring(PosDelimiter+1), Child);
                   }

                }

            // check the inherited base node.
            string InheritedFrom = ReferenceNode.Attribute("InheritedFrom");
            if (InheritedFrom != "")
                {
                string BaseChildPath = InheritedFrom + "/" + ChildName;
                APSIMData InheritedNode = Find(BaseChildPath, doc, false);
                if (InheritedNode != null)
                    {
                    APSIMData NewChild = ReferenceNode.Add(InheritedNode);
                    NewChild.SetAttribute("InheritedFrom", BaseChildPath);
                    return EnsureNodeExists(NodePath.Substring(PosDelimiter+1), ReferenceNode);
                    }
                }
            return null;
            }
            
        }
    }
