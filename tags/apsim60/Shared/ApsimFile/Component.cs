using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;
using VBGeneral;
using System.Collections;
using System.Collections.Specialized;
using System.IO;

namespace ApsimFile
    {
    public class Component : IComparable
        {
        // Some private stuff
        private string MyName;
        private string MyType;
        private string MyContents = "";
        private string MyDescription = "";
        private bool MyEnabled = true;
        private List<Component> MyChildNodes = new List<Component>();
        private Component MyShortCutTo = null;
        private ApsimFile MyFile = null;
        private Component MyParent = null;
        private string TempShortCutName = "";
        private void EnsureNameIsUnique()
        {
            // -------------------------------------------------------------
            // Make sure our name is unique amongst our siblings.
            // -------------------------------------------------------------
            string BaseName = Name;
            string UniqueChildName = BaseName;

            if (Parent != null)
            {
                for (int i = 1; i != 10000; i++)
                {
                    int Count = 0;
                    foreach (Component Sibling in Parent.ChildNodes)
                    {
                        if (Sibling.Name.ToLower() == UniqueChildName.ToLower())
                            Count++;
                    }
                    if (Count == 1)
                        return;
                    UniqueChildName = BaseName + i.ToString();
                    MyName = UniqueChildName;
                }
                throw new Exception("Cannot find a unique name for component: " + BaseName);

            }
        }
        private int ChildNameToIndex(string Name)
            {
            // ---------------------------------------------------------------------
            // Find a child component by name and return it's index. Returns -1 if 
            // not found.
            // ---------------------------------------------------------------------
            for (int i = 0; i != ChildNodes.Count; i++)
                if (ChildNodes[i].Name == Name)
                    return i;
            return -1;
            }
        internal Component(ApsimFile File, Component Parent)
            {
            this.MyFile = File;
            this.MyParent = Parent;
            }
        internal Component(ApsimFile File, Component Parent, Component Source)
            {
            this.MyFile = File;
            Name = Source.Name;
            Type = Source.Type;
            this.MyParent = Parent;
            MyShortCutTo = Source;
            }
        internal void Read(XmlNode Node)
            {
            // ------------------------------------------------------
            // Read this component from the specified XmlNode. Also
            // create child components as necessary.
            // ------------------------------------------------------
            Type = Node.Name;
            if (Type.ToLower() == "simulations")
                Type = "folder";
            Name = XmlHelper.Name(Node);
            if (XmlHelper.Attribute(Node, "shortcut") != "")
                TempShortCutName = XmlHelper.Attribute(Node, "shortcut");
            if (XmlHelper.Attribute(Node, "enabled") == "no")
                MyEnabled = false;
            foreach (XmlNode Child in Node.ChildNodes)
                {
                if (MyFile.IsComponentVisible(Child.Name) && XmlHelper.Attribute(Child, "invisible") != "yes")
                    {
                    Component ChildComponent = new Component(MyFile, this);
                    ChildNodes.Add(ChildComponent);
                    ChildComponent.Read(Child);
                    }
                else
                    MyContents += Child.OuterXml;
                }
            }
        internal void ResolveShortcuts()
            {
            // ------------------------------------------------------
            // Now that reading is fully done we can go and resolve
            // any shortcuts that we found during reading. Do
            // this recursively for all children.
            // ------------------------------------------------------
            if (TempShortCutName != "")
                {
                MyShortCutTo = MyFile.Find(TempShortCutName);
                TempShortCutName = "";
                }
            foreach (Component Child in ChildNodes)
                Child.ResolveShortcuts();
            }
        internal void Write(XmlNode Node)
            {
            // ------------------------------------------------------
            // Write this component to the specified XmlNode. Also
            // create child nodes as necessary.
            // ------------------------------------------------------
            XmlHelper.SetName(Node, Name);
            if (!Enabled)
                XmlHelper.SetAttribute(Node, "enabled", "no");
            if (ShortCutTo != null)
                XmlHelper.SetAttribute(Node, "shortcut", ShortCutTo.FullPath);
            Node.InnerXml = MyContents;
            foreach (Component Child in ChildNodes)
                {
                XmlNode NewNode = Node.AppendChild(Node.OwnerDocument.CreateElement(Child.Type));
                Child.Write(NewNode);
                }
            }

        // Public properties and methods.
        public string Name
            {
            get { return MyName; }
            set
                {
                MyName = value;
                EnsureNameIsUnique();
                MyFile.PublishComponentChanged(this.Parent);
                }
            }
        public string Type
            {
            get { return MyType; }
            set
                {
                MyType = value;
                MyFile.PublishComponentChanged(this);
                }
            }
        public string Description
            {
            get { return MyDescription; }
            set
                {
                MyDescription = value;
                MyFile.PublishComponentChanged(this);
                }
            }
        public string Contents
            {
            get
                {
                string OuterContents = "<" + Type;
                if (Name != Type)
                    OuterContents += " name=\"" + Name + "\"";
                OuterContents += ">";

                // Get the absolute base component.
                Component BaseComponent = this;
                while (BaseComponent.MyShortCutTo != null)
                    BaseComponent = BaseComponent.MyShortCutTo;

                OuterContents += BaseComponent.MyContents;
                OuterContents += "</" + Type + ">";
                return OuterContents;
                }
            set
                {
                if (MyShortCutTo == null)
                    {
                    XmlDocument Doc = new XmlDocument();
                    Doc.LoadXml(value);
                    string InnerContents = Doc.DocumentElement.InnerXml;
                    if (MyContents != InnerContents)
                        {
                        MyContents = InnerContents;
                        MyFile.PublishContentChanged(this);
                        }
                    }
                else
                    MyShortCutTo.Contents = value;
                }
            }
        public const char Delimiter = '/';
        public string FullPath
            {
            get
                {
                // The path returned is of the form: /RootComponent/Child/SubChild
                string Path = Name;
                Component Comp = Parent;
                while (Comp != null)
                    {
                    Path = Comp.Name + Delimiter + Path;
                    Comp = Comp.Parent;
                    }
                return Delimiter + Path;
                }
            }
        public bool Enabled
            {
            get { 
                if (ShortCutTo == null)
                    return MyEnabled; 
                else 
                    return ShortCutTo.Enabled && MyEnabled;
                }
            set { 
                MyEnabled = value;
                MyFile.PublishComponentChanged(this);
                }    
            }
        public Component Parent
            {
            get { return MyParent; }
            }
        public Component ShortCutTo
            {
            get { return MyShortCutTo; }
            }
        public int CompareTo(object rhs)
            {
            return Name.CompareTo((rhs as Component).Name);
            }
        public Component Find(string RelativePath)
            {
            // ------------------------------------------------------
            // Will look for a child component using the relative
            // path passed in. e.g. Child/SubChild
            // ------------------------------------------------------
            string ChildName, Remainder;
            int PosDelimiter = RelativePath.IndexOf(Delimiter);
            if (PosDelimiter != -1)
                {
                ChildName = RelativePath.Substring(0, PosDelimiter);
                Remainder = RelativePath.Substring(PosDelimiter + 1);
                }
            else
                {
                ChildName = RelativePath;
                Remainder = "";
                }
            if (ChildName == "..")
                {
                if (Remainder == "")
                    return Parent;
                else
                    return Parent.Find(Remainder);
                }
            foreach (Component Child in ChildNodes)
                {
                if (Child.Name.ToLower() == ChildName.ToLower())
                    {
                    if (Remainder == "")
                        return Child;
                    else
                        return Child.Find(Remainder);
                    }
                }
            return null;
            }
        public List<Component> ChildNodes
            {
            get { return MyChildNodes; }
            }
        public Component Add(string Xml)
            {
            // ---------------------------------------------------------------------
            // Add the specified xml as children. The xml might be multiple children
            // i.e. have multiple root nodes and so not valid XML. Add a dummy
            // root node around the xml so that we can parse it. Returns the first
            // child component
            // ---------------------------------------------------------------------
            XmlDocument Doc;

            try
                {
                Doc = new XmlDocument();
                Doc.LoadXml("<dummy>" + Xml + "</dummy>");
                if (Doc.DocumentElement.ChildNodes.Count == 1 &&
                    Doc.DocumentElement.ChildNodes[0].Name.ToLower() == "soil")
                    {
                    // A special test for when the user drops a soil on a component node or when
                    // the user pastes a soil node. We want to make sure there are <initwater> and
                    // <initnitrogen> nodes.
                    XmlNode SoilNode = Doc.DocumentElement.ChildNodes[0];
                    XmlNode InitWaterNode = XmlHelper.Find(SoilNode, "InitWater");
                    XmlNode InitNitrogenNode = XmlHelper.Find(SoilNode, "InitNitrogen");
                    XmlNode SoilSampleNode = XmlHelper.Find(SoilNode, "SoilSample");
                    if (InitWaterNode == null && InitNitrogenNode == null && SoilSampleNode == null)
                        {
                        SoilNode.AppendChild(SoilNode.OwnerDocument.CreateElement("InitWater"));
                        SoilNode.AppendChild(SoilNode.OwnerDocument.CreateElement("InitNitrogen"));
                        }
                    }

                }
            catch (Exception)
                {
                return null;
                }

            Component FirstChildComponent = null;
            MyFile.BeginUpdate();
            foreach (XmlNode Child in Doc.DocumentElement.ChildNodes)
                {
                if (MyFile.AllowComponentAdd(Child.Name, Type))
                    {
                    Component ChildComponent = new Component(MyFile, this);
                    MyChildNodes.Add(ChildComponent);
                    ChildComponent.Read(Child);
                    ChildComponent.ResolveShortcuts();
                    ChildComponent.EnsureNameIsUnique();
                    if (FirstChildComponent == null)
                        FirstChildComponent = ChildComponent;
                    }
                }
            MyFile.EndUpdate();
            MyFile.PublishComponentChanged(this);
            return FirstChildComponent;
            }
        public bool AllowAdd(string Xml)
            {
            // -------------------------------------------------------------- 
            // Do we allow the specified XML to be added to this node?
            // -------------------------------------------------------------- 
            if (MyFile.IsReadOnly)
                return false;
            else
                {
                // Do we allow the specified xml to be added to the selected node? 
                XmlDocument TempDoc = new XmlDocument();
                TempDoc.LoadXml("<dummy>" + Xml + "</dummy>");
                bool ok = true;
                foreach (XmlNode Child in XmlHelper.ChildNodes(TempDoc.DocumentElement, ""))
                    ok = ok & MyFile.AllowComponentAdd(Child.Name, Type);
                return ok & TempDoc.DocumentElement.ChildNodes.Count > 0;
                }
            }
        public string[] ChildNames
            {
            get
                {
                string[] names = new string[MyChildNodes.Count];
                for (int i = 0; i != MyChildNodes.Count; i++)
                    names[i] = MyChildNodes[i].Name;
                return names;
                }
            }
        public Component AddShortCut(Component SourceComponent)
            {
            // ---------------------------------------------------------------------
            // Add the source component as a child shortcut component.
            // ---------------------------------------------------------------------
            MyFile.BeginUpdate();
            Component ShortCutComponent = new Component(MyFile, this, SourceComponent);
            MyChildNodes.Add(ShortCutComponent);
            ShortCutComponent.EnsureNameIsUnique();

            // Now we need to go through all child nodes of the shortcut and make 
            // them shortcuts as well.
            foreach (Component Child in SourceComponent.ChildNodes)
                ShortCutComponent.AddShortCut(Child);

            MyFile.EndUpdate();
            MyFile.PublishComponentChanged(this);
            return ShortCutComponent;
            }
        public void Delete(Component ComponentToDelete)
            {
            // ---------------------------------------------------------------------
            // Delete the specified child component. We need to scan all components
            // recursively looking for a shortcut that points to this component or
            // any of our children that we're about to delete. If found then make 
            // the shortcut node concrete.
            // ---------------------------------------------------------------------
            List<Component> AllChildNodes = new List<Component>();
            ComponentToDelete.ChildNodesRecursively(AllChildNodes);
            AllChildNodes.Add(ComponentToDelete);
            MyFile.RootComponent.MakeShortCutsConrete(AllChildNodes);
            ChildNodes.Remove(ComponentToDelete);
            MyFile.PublishComponentChanged(this);
            }
        public Component Duplicate(Component ComponentToDuplicate)
            {
            XmlDocument Doc = new XmlDocument();
            Doc.AppendChild(Doc.CreateElement(ComponentToDuplicate.Type));
            ComponentToDuplicate.Write(Doc.DocumentElement);
            if (XmlHelper.Attribute(Doc.DocumentElement, "shortcut") == "")
                return Add(Doc.DocumentElement.OuterXml);
            else
                return AddShortCut(ComponentToDuplicate);
            }
        private void ChildNodesRecursively(List<Component> AllChildNodes)
            {
            // ---------------------------------------------------------------------
            // Fill the specified list will all child nodes recursively.
            // ---------------------------------------------------------------------
            foreach (Component Child in ChildNodes)
                {
                AllChildNodes.Add(Child);
                Child.ChildNodesRecursively(AllChildNodes);
                }
            }

        private void MakeShortCutsConrete(List<Component> Nodes)
            {
            // ---------------------------------------------------------------------
            // Scan all children looking for a shortcut that points to any node in
            // the specified list of nodes. When found then make the shortcut concete.
            // ---------------------------------------------------------------------
            if (ShortCutTo != null && Nodes.Contains(ShortCutTo))
                {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(ShortCutTo.Contents);
                MyContents = Doc.DocumentElement.InnerXml;
                MyShortCutTo = null;
                MyFile.PublishComponentChanged(this);
                }
            foreach (Component Child in ChildNodes)
                Child.MakeShortCutsConrete(Nodes);
            }
        public void Sort()
            {
            // ---------------------------------------------------------------------
            // Sort the childnodes alphabetically
            // ---------------------------------------------------------------------
            ChildNodes.Sort();
            MyFile.PublishComponentChanged(this);
            }
        public void MoveUp(List<string> NamesToMoveUp)
            {
            // ---------------------------------------------------------------------
            // Move the specified child component names up one spot.
            // ---------------------------------------------------------------------
            for (int i = 0; i != NamesToMoveUp.Count; i++)
                {
                int ReferencePosition = ChildNameToIndex(NamesToMoveUp[i]);
                if (ReferencePosition > 0)
                    {
                    Component ComponentToMove = ChildNodes[ReferencePosition];
                    ChildNodes.Remove(ComponentToMove);
                    ChildNodes.Insert(ReferencePosition - 1, ComponentToMove);
                    }
                else
                    return;
                }
            MyFile.PublishComponentChanged(this);
            }
        public void MoveDown(List<string> NamesToMoveDown)
            {
            // ---------------------------------------------------------------------
            // Move the specified child component names down one spot.
            // ---------------------------------------------------------------------
            for (int i = NamesToMoveDown.Count - 1; i >= 0; i--)
                {
                int ReferencePosition = ChildNameToIndex(NamesToMoveDown[i]);
                if (ReferencePosition < ChildNodes.Count - 1)
                    {
                    Component ComponentToMove = ChildNodes[ReferencePosition];
                    ChildNodes.Remove(ComponentToMove);
                    ChildNodes.Insert(ReferencePosition + 1, ComponentToMove);
                    }
                else
                    return;
                }
            MyFile.PublishComponentChanged(this);
            }
        public void MakeConcrete()
            {
            // ---------------------------------------------------------------------
            // Convert this component from a shortcut node to a concrete node.
            // ---------------------------------------------------------------------
            if (ShortCutTo != null)
                {
                List<Component> ShortCuts = new List<Component>();
                ShortCuts.Add(ShortCutTo);
                MakeShortCutsConrete(ShortCuts);
                MyFile.PublishComponentChanged(this);
                }
            }
        public Component FindContainingPaddock()
            {
            Component Paddock = this;
            while (Paddock != null && Paddock.Type != "area" && Paddock.Type != "simulation" &&
                                      Paddock.Type != "folder")
                Paddock = Paddock.Parent;
            return Paddock;
            }

        public void WriteSim(XmlDocument Doc, Configuration Configuration)
            {
            WriteSim(Doc, null, Configuration);
            }
        private void WriteSim(XmlDocument Doc, XmlNode ParentNode, Configuration Configuration)
            {
            XmlNode ApsimToSim = Configuration.TypeNode(Type + "/ApsimToSim");
            if (ApsimToSim != null && Enabled)
                {
                string ModuleType = XmlHelper.Value(ApsimToSim, "type");
                XmlNode SimNode = XmlHelper.Find(ApsimToSim, "sim");
                XmlNode CallDllNode = XmlHelper.Find(ApsimToSim, "calldll");
                XmlNode Node = ParentNode;
                if (ParentNode != null)
                    {
                    Node = XmlHelper.Find(ParentNode, "initdata");
                    if (Node == null)
                        Node = ParentNode;
                    }
                if (ModuleType != "")
                    {
                    string ModuleDLL = XmlHelper.Value(ApsimToSim, "dll").Replace(APSIMSettings.ApsimDirectory(), "%apsuite");
                    Node = Doc.CreateElement(ModuleType);
                    if (ParentNode == null)
                        Doc.AppendChild(Node);
                    else
                        ParentNode.AppendChild(Node);

                    XmlHelper.SetName(Node, Name);
                    XmlHelper.SetAttribute(Node, "executable", ModuleDLL);
                    XmlHelper.SetValue(Node, "executable", ModuleDLL);
                    string ComponentInterfaceType = XmlHelper.Value(ApsimToSim, "componentinterface");
                    if (ComponentInterfaceType != "")
                        XmlHelper.SetValue(Node, "componentinterface", ComponentInterfaceType);
                    if (ModuleType != "system" && ModuleType != "simulation")
                        Node = Node.AppendChild(Doc.CreateElement("initdata"));

                    // See if user has an ini component in the .apsim file.
                    List<string> IniFileNames = new List<string>();
                    foreach (Component Child in ChildNodes)
                        if (Child.Type.ToLower() == "ini")
                            {
                            XmlDocument IniDoc = new XmlDocument();
                            IniDoc.LoadXml(Child.Contents);
                            string IniFileName = XmlHelper.Value(IniDoc.DocumentElement, "filename");
                            IniFileName.Replace(APSIMSettings.ApsimDirectory(), "%apsuite");
                            IniFileNames.Add(IniFileName);
                            }

                    // If user didn't specify an ini component then see if types has an ini for us
                    string defaultIni = XmlHelper.Value(ApsimToSim, "ini");
                    defaultIni.Replace(APSIMSettings.ApsimDirectory(), "%apsuite");

                    if (IniFileNames.Count == 0 && defaultIni != "")
                        IniFileNames.Add(defaultIni);

                    // If we have an ini filename then write it to the sim.
                    if (IniFileNames.Count > 0)
                        XmlHelper.SetValues(Node, "include", IniFileNames);
                    }

                // write module contents bit.
                string NewContents = "";
                if (SimNode != null)
                    {
                    XmlDocument ContentsDoc = new XmlDocument();
                    ContentsDoc.LoadXml(Contents);
                    Macro macro = new Macro();
                    NewContents = macro.Go(ContentsDoc.DocumentElement, XmlHelper.FormattedXML(SimNode.InnerXml));
                    }
                else if (CallDllNode == null)
                    {
                    XmlDocument ContentsDoc = new XmlDocument();
                    ContentsDoc.LoadXml(Contents);
                    NewContents = ContentsDoc.DocumentElement.InnerXml;
                    }
                if (Contents != "")
                    {
                    XmlDocument ContentsDoc = new XmlDocument();
                    ContentsDoc.LoadXml("<dummy>" + NewContents + "</dummy>");
                    foreach (XmlNode Child in ContentsDoc.DocumentElement)
                        Node.AppendChild(Node.OwnerDocument.ImportNode(Child, true));
                    }
                    
                if (CallDllNode != null)
                    {
                    List<object> Arguments = new List<object>();
                    Arguments.Add(this);
                    Arguments.Add(Node);
                    Node = (XmlNode) CallDll.CallMethodOfClass(CallDllNode, Arguments);
                    }

                if (Node != null)
                    {
                    // Iterate through all children and call their WriteSim methods.
                    foreach (Component Child in ChildNodes)
                        Child.WriteSim(Doc, Node, Configuration);

                    if (ModuleType == "system" || ModuleType == "simulation")
                        {
                        // Sort the nodes into component order.
                        XmlHelper.Sort(Node, new ComponentSorter());
                        }
                    }
               }
           else if (Type == "folder")
               {
               // Iterate through all children and call their WriteSim methods.
               foreach (Component Child in ChildNodes)
                   Child.WriteSim(Doc, ParentNode, Configuration);
               XmlHelper.Sort(ParentNode, new ComponentSorter());
               }
            }

        private class ComponentSorter : IComparer
            {
            private CaseInsensitiveComparer StringComparer = new CaseInsensitiveComparer();
            private StringCollection Components = new StringCollection();
            public ComponentSorter()
                {
                string Contents = APSIMSettings.INIReadSection(APSIMSettings.ApsimIniFile(), "component order");
                StringReader In = new StringReader(Contents);
                string Line;
                while ((Line = In.ReadLine()) != null)
                    {
                    if (Line.StartsWith("component"))
                        {
                        int PosEquals = Line.IndexOf('=');
                        if (PosEquals != -1)
                            Components.Add(Line.Substring(PosEquals + 1).Trim());
                        }
                    }

                In.Close();
                }
            int IComparer.Compare(object x, object y)
                {
                XmlNode Data1 = (XmlNode)x;
                XmlNode Data2 = (XmlNode)y;
                string ModuleName1 = Path.GetFileNameWithoutExtension(XmlHelper.Attribute(Data1, "executable")).ToLower();
                string ModuleName2 = Path.GetFileNameWithoutExtension(XmlHelper.Attribute(Data2, "executable")).ToLower();

                if (x == y)
                    return 0;
                if (Data1.Name == "executable")
                    return -1;
                if (Data2.Name == "executable")
                    return 1;
                if (ModuleName1 == ModuleName2)
                    {
                    int ChildIndex1 = Array.IndexOf(XmlHelper.ChildNames(Data1.ParentNode, ""), XmlHelper.Name(Data1));
                    int ChildIndex2 = Array.IndexOf(XmlHelper.ChildNames(Data2.ParentNode, ""), XmlHelper.Name(Data2));
                    if (ChildIndex1 < ChildIndex2)
                        return -1;
                    else
                        return 1;
                    }
                if (XmlHelper.Type(Data1) == "title")
                    return -1;
                for (int i = 0; i != Components.Count; i++)
                    {
                    if (StringManip.StringsAreEqual(Components[i], ModuleName1))
                        return -1;
                    if (StringManip.StringsAreEqual(Components[i], ModuleName2))
                        return 1;
                    }
                return 0; // neither are in list!!
                }
            }

        public void Replace(string Xml)
            {
            // -------------------------------------------------------
            // We need to replace 'this' component with the one
            // passed in - need to be mindfull of shortcuts.
            // -------------------------------------------------------
            Component TempComponent = Parent.Add(Xml);
            Replace(TempComponent);
            Parent.Delete(TempComponent);
            MyFile.PublishComponentChanged(this);
            }
        private void Replace(Component Rhs)
            {
            // -------------------------------------------------------
            // Replace 'this' component with 'Rhs'
            // -------------------------------------------------------
            Contents = Rhs.Contents;
            string[] Children = ChildNames;
            foreach (Component TempChild in Rhs.MyChildNodes)
                {
                int Pos = VBGeneral.Utility.IndexOfCaseInsensitive(Children, TempChild.Name);
                if (Pos == -1)
                    Add(TempChild.Contents);
                else
                    MyChildNodes[Pos].Replace(TempChild);
                }

            // Remove unwanted children.
            for (int i = ChildNames.Length - 1; i >= 0; i--)
                {
                bool Found = false;
                foreach (Component RhsChild in Rhs.MyChildNodes)
                    {
                    if (ChildNames[i].ToLower() == RhsChild.Name.ToLower())
                        Found = true;
                    }
                if (!Found)
                    Delete(MyChildNodes[i]);
                }
            }




        public string FullXML()
            {
            XmlDocument Doc = new XmlDocument();
            Doc.AppendChild(Doc.CreateElement(Type));
            Write(Doc.DocumentElement);
            return Doc.OuterXml;
            }


        }
    }