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
    using CSGeneral;
    using System.Windows.Forms;


    public class ApsimFile
        {
        // ---------------------------------------------
        // This class encapsulates a .apsim file.
        // ---------------------------------------------

        // private stuff
        private Component MyRootNode = null;
        private Configuration MyConfiguration;
        private bool Dirty = false;
        private bool ReadOnly = false;
        private string MyFileName = "";
        private int DisabledEventCount = 0;
        internal bool IsComponentVisible(string ComponentType)
            {
            return MyConfiguration.IsComponentVisible(ComponentType);
            }
        internal bool AllowComponentAdd(string ChildType, string ParentType)
            {
            return MyConfiguration.AllowComponentAdd(ChildType, ParentType);
            }


        // events
        public delegate void FileDelegate<T>(T arg);
        public delegate void EmptyDelegate();
        public event FileDelegate<Component> ComponentChangedEvent;
        public event FileDelegate<Component> ContentChangedEvent;
        public event FileDelegate<bool> DirtyChanged;
        public event FileDelegate<string> FileNameChanged;
        public event EmptyDelegate BeforeSave;
        internal void PublishComponentChanged(Component Comp)
            {
            if (Comp != null && DisabledEventCount == 0 && ComponentChangedEvent != null)
                ComponentChangedEvent.Invoke(Comp);
            SetDirty(true);
            }
        internal void PublishContentChanged(Component Comp)
            {
            if (Comp != null && DisabledEventCount == 0 && ContentChangedEvent != null)
                ContentChangedEvent.Invoke(Comp);
            SetDirty(true);
            }
        internal void BeginUpdate()
            {
            DisabledEventCount++;
            }
        internal void EndUpdate()
            {
            DisabledEventCount--;
            }

        private void SetDirty(bool Dirty)
            {
            if (this.Dirty != Dirty)
                {
                this.Dirty = Dirty;
                if (DisabledEventCount == 0 && DirtyChanged != null)
                    DirtyChanged.Invoke(Dirty);
                }
            }
        private void SetFileName(string FileName)
            {
            MyFileName = FileName;
            if (DisabledEventCount == 0 && FileNameChanged != null)
                FileNameChanged.Invoke(FileName);
            }

        // properties
        public bool IsDirty 
            { get { return Dirty; } }
        public bool IsReadOnly { get { return ReadOnly; } }
        public Component RootComponent
            {
            // ------------------------------------------------------
            // Return the root node to the caller.
            // ------------------------------------------------------

            get { return MyRootNode; }
            }
        public string FileName
            { get { return MyFileName; } }

        public ApsimFile(Configuration Config)
            {
            // ------------------------------------------------------
            // Constructor
            // ------------------------------------------------------
            MyConfiguration = Config;
            }

        public Component Find(string FullPath)
            {
            // ------------------------------------------------------
            // Locates a component with the specified full path
            // e.g. /RootComponent/Child/SubChild
            // ------------------------------------------------------

            if (FullPath == "")
                throw new Exception("Cannot call Find with a blank path");
            if (FullPath[0] != Component.Delimiter)
                throw new Exception("Path must be fully qualified: " + FullPath);

            int Pos = FullPath.IndexOf(Component.Delimiter, 1);
            string RootName, NamePath;
            if (Pos == -1)
                {
                RootName = FullPath.Substring(1);
                NamePath = "";
                }
            else
                {
                RootName = FullPath.Substring(1, Pos - 1);
                NamePath = FullPath.Substring(Pos + 1);
                }
            if (RootName.ToLower() != RootComponent.Name.ToLower())
                return null;
            if (NamePath == "")
                return RootComponent;
            else
                return RootComponent.Find(NamePath);
            }
        public void New()
            {
            New("<folder/>");
            }
        public void New(string Xml)
            {
            // Create a new .apsim file in memory.
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(Xml);
            Open(Doc.DocumentElement);
            }
        public void Open(XmlNode Node) 
            {
            APSIMChangeTool.Upgrade(Node);
            this.ReadOnly = false;

            DisabledEventCount++;
            MyRootNode = new Component(this, null);
            MyRootNode.Read(Node);
            MyRootNode.ResolveShortcuts();
            DisabledEventCount--;
            PublishComponentChanged(MyRootNode);
            SetFileName("Untitled");
            }
        public void OpenFile(string FileName)
            {
            DisabledEventCount++;
            XmlDocument doc = new XmlDocument();
            doc.Load(FileName);
            Open(doc.DocumentElement);
            StringCollection ReadOnlyFileNames = MyConfiguration.Settings("ReadOnlyFiles");
            ReadOnly = (Utility.IndexOfCaseInsensitive(ReadOnlyFileNames, Path.GetFileName(FileName)) != -1);
            DisabledEventCount--;
            PublishComponentChanged(MyRootNode);
            SetDirty(false);
            SetFileName(FileName);
            }

        public void Save()
            {
            // ---------------------------------------------------------
            // Save the contents of this apsim document to the specified 
            // file - formatted nicely.
            // ---------------------------------------------------------
            if (!ReadOnly && FileName != "Untitled")
                {
                if (BeforeSave != null)
                    BeforeSave.Invoke();
                XmlDocument doc = new XmlDocument();
                XmlNode RootNode = doc.AppendChild(doc.CreateElement(RootComponent.Type));
                XmlHelper.SetAttribute(RootNode, "version", APSIMChangeTool.CurrentVersion.ToString());
                RootComponent.Write(RootNode);
                doc.Save(FileName);
                Dirty = true;   
                SetDirty(false);
                }
            }
        public void SaveAs(string FileName)
            {
            ReadOnly = false;
            SetFileName(FileName);
            Save();
            }

        public void CopyToClipboard(StringCollection Paths)
            {
            XmlDocument Doc = new XmlDocument();
            XmlNode Root = Doc.AppendChild(Doc.CreateElement("dummy"));
            foreach (string ComponentPath in Paths)
                {
                Component Component = Find(ComponentPath);
                if (Component != null)
                    {
                    XmlNode Node = Root.AppendChild(Doc.CreateElement(Component.Type));
                    Component.Write(Node);
                    }
                }
            Clipboard.SetDataObject(Root.InnerXml, true);
            }


        }
    }
