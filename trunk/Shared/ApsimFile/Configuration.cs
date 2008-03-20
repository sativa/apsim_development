using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Windows.Forms;
using System.Drawing;
using CSGeneral;
using VBGeneral;
using System.IO;
using System.Collections.Specialized;

namespace ApsimFile
    {
    public class Configuration
        {
        private XmlNode TypesFile;
        private ImageList LargeIcons = new ImageList();
        private ImageList MediumIcons = new ImageList();
        private ImageList SmallIcons = new ImageList();
        private string TypesFileName;
        private XmlNode ComponentDescriptionData = null;
        private string SectionName;

        public Configuration(string SectionName)
            {
            this.SectionName = SectionName;
            LargeIcons.ImageSize = new Size(32, 32);
            MediumIcons.ImageSize = new Size(24, 24);
            SmallIcons.ImageSize = new Size(16, 16);
            LargeIcons.Tag = "LargeIcon";
            MediumIcons.Tag = "MediumIcon";
            SmallIcons.Tag = "SmallIcon";

            // Setup the types file and load all images specified by it.
            TypesFileName = Setting("TypesFile");
            if (TypesFileName != "")
                {
                XmlDocument Doc = new XmlDocument();
                Doc.Load(TypesFileName);
                TypesFile = Doc.DocumentElement;
                LoadAllImages(TypesFile);
                }
            }
        public void LoadAllImages(XmlNode Node)
            {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
                {
                LoadIcon(Child, "LargeIcon", ref LargeIcons);
                LoadIcon(Child, "MediumIcon", ref MediumIcons);
                LoadIcon(Child, "SmallIcon", ref SmallIcons);
                }
            }
        public string Setting(string SettingName)
            {
            return APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), SectionName, SettingName);
            }
        public StringCollection Settings(string SettingName)
            {
            return APSIMSettings.INIReadMultiple(APSIMSettings.ApsimIniFile(), SectionName, SettingName);
            }

        public static void LoadIcon(XmlNode Data, string Specifier, ref ImageList Icons)
            {
            // -----------------------------------------------------------------
            // Load an icon for the 'Data' type using the specifier. The icon
            // is stored in the specified imagelist and an index node createdef
            // to store the position of the icon in the imagelist.
            // -----------------------------------------------------------------
            XmlNode IconChild = XmlHelper.Find(Data, Specifier);
            if (IconChild != null)
                {
                string FileName = IconChild.InnerText.Replace("%apsuite", APSIMSettings.ApsimDirectory());

                if (File.Exists(FileName))
                    {
                    Bitmap Icon = new Bitmap(FileName);
                    Icons.Images.Add(Icon);
                    int IconIndex = Icons.Images.Count - 1;
                    XmlHelper.SetValue(Data, Specifier + "Index", IconIndex.ToString());
                    }
                }
            }
        public ImageList ImageList(string ImageType)
            { 
            if (ImageType == "SmallIcon")
                return SmallIcons;
            else if (ImageType == "MediumIcon")
                return MediumIcons;
            else
                return LargeIcons;
            }        
        public int ImageIndex(string Type, string ImageType)
            {
            return ImageIndex(XmlHelper.Find(TypesFile, Type), ImageType);
            }
        public int ImageIndex(XmlNode Node, string ImageType)
            {
            if (Node != null)
                {
                XmlNode ImageIndexChild = XmlHelper.Find(Node, ImageType + "Index");
                if (ImageIndexChild != null)
                    return Convert.ToInt32(ImageIndexChild.InnerText);
                }
            return -1;
            }

        public string Info(string Type, string InfoType)
            {
            // -----------------------------------------------------------------
            // Return description for the specified type.
            // -----------------------------------------------------------------
            XmlNode Node = XmlHelper.Find(TypesFile, Type + "/" + InfoType);
            if (Node != null)
                return Node.InnerText.Replace("%apsuite", APSIMSettings.ApsimDirectory());
            else
                return "";
            }
        public XmlNode TypeNode(string Type)
            {
            // ------------------------------------------------------------------
            // Return type node for the specified type. Returns null if not found
            // ------------------------------------------------------------------
            return XmlHelper.Find(TypesFile, Type);
            }
        public bool IsComponentVisible(string ComponentType)
            {
            if ((XmlHelper.Find(TypesFile, ComponentType) != null))
                {
                if (XmlHelper.Value(TypesFile, ComponentType + "/ShowInMainTree") == "Yes")
                    return true;
                }
            return false;
            } 
        public void GetVariablesForComponent(string Type, string InstanceName, 
                                             string PropertyGroup, XmlNode ReturnVariables)
            {
            // -----------------------------------------------------------------
            // Add variable info for the specified type and instance name to the
            // "VariableData" argument.
            // -----------------------------------------------------------------
            XmlNode TypeInfo = XmlHelper.Find(TypesFile, Type);
            if (TypeInfo != null)
                {
                foreach (XmlNode TypeVariables in XmlHelper.ChildNodes(TypeInfo, PropertyGroup))
                    {
                    XmlNode Variables = TypeVariables;

                    if (XmlHelper.Attribute(TypeVariables, "link") != "")
                        {
                        // Load components description file if necessary
                        if (ComponentDescriptionData == null)
                            {
                            XmlDocument Doc = new XmlDocument();
                            Doc.Load(APSIMSettings.ApsimDirectory() + "/ApsimUI/ComponentDescription.xml");
                            ComponentDescriptionData = Doc.DocumentElement;
                            }

                        XmlNode Component = XmlHelper.Find(ComponentDescriptionData, XmlHelper.Attribute(TypeVariables, "link"));
                        if (Component != null)
                            {
                            XmlNode ComponentNode = XmlHelper.FindByType(Component, PropertyGroup);
                            Variables = ReturnVariables.AppendChild(ReturnVariables.OwnerDocument.ImportNode(ComponentNode, true));
                            XmlHelper.SetName(Variables, TypeVariables.Name);
                            XmlHelper.SetAttribute(Variables, "module", XmlHelper.Attribute(TypeVariables, "module"));
                            }
                        }
                    else if (Variables != null)
                        Variables = ReturnVariables.AppendChild(ReturnVariables.OwnerDocument.ImportNode(Variables, true));
                    

                    if (Variables != null)
                        {
                        if (XmlHelper.Attribute(Variables, "name") == XmlHelper.Type(Variables))
                            XmlHelper.SetName(Variables, InstanceName);
                       
                        if (XmlHelper.Attribute(Variables, "module") == "")
                            XmlHelper.SetAttribute(Variables, "module", InstanceName);
                        else
                            XmlHelper.SetAttribute(Variables, "module", XmlHelper.Attribute(Variables, "module").Replace("[name]", InstanceName));
                        }
                    }
                }
            }
        public string[] GetCultivarsForCrop(string CropName)
            {
            if (CropName != "")
                {
                XmlNode Crop = XmlHelper.Find(TypesFile, CropName);
                if (Crop != null)
                    {
                    List<string> Cultivars = new List<string>();
                    foreach (XmlNode Cultivar in XmlHelper.ChildNodes(Crop, "cultivar"))
                        Cultivars.Add(XmlHelper.Name(Cultivar));
                    string[] ReturnCultivars = new string[Cultivars.Count];
                    Cultivars.CopyTo(ReturnCultivars, 0);
                    return ReturnCultivars;
                    }
                }
            return null;
            }
        public bool AllowComponentAdd(string ChildComponentType, string ParentComponentType)
            {
            // ------------------------------------------------- 
            // Return true if the specified component type can 
            // be added as a child to the specified parent type. 
            // ------------------------------------------------- 
            // Look in the componentinfo's drop targets. 
            XmlNode DropNode = XmlHelper.Find(TypesFile, ChildComponentType + "/drops");
            foreach (XmlNode Drop in XmlHelper.ChildNodes(DropNode, ""))
                {
                if (XmlHelper.Name(Drop).ToLower() == ParentComponentType.ToLower())
                    return true;
                }
            // if we get here we haven't found what we're after 
            return false;
            }

        private const int MAX_NUM_FREQUENT_SIMS = 10;
        public void AddFileToFrequentList(string filename)
            {
            string[] FileNames = GetFrequentList();

            // Put this filename into the top of the NewFileList and 
            // then copy all existing filenames to NewFileList 
            string[] NewFileList = new string[MAX_NUM_FREQUENT_SIMS + 1];
            NewFileList[0] = filename;
            int NewFileListIndex = 1;
            for (int i = 0; i <= FileNames.Length - 1; i++)
                {
                if (FileNames[i] != filename & FileNames[i] != "" & NewFileListIndex < MAX_NUM_FREQUENT_SIMS)
                    {
                    NewFileList[NewFileListIndex] = FileNames[i];
                    NewFileListIndex = NewFileListIndex + 1;
                    }
                }

            // Write NewFileList back to .ini file. 
            APSIMSettings.INIWriteMultiple(APSIMSettings.ApsimIniFile(), SectionName, "RecentFile", NewFileList);
            }
        public string[] GetFrequentList()
            {
            StringCollection FileNames = Settings("RecentFile");
            StringCollection GoodFileNames = new StringCollection();
            foreach (string FileName in FileNames)
                {
                if (File.Exists(FileName))
                    {
                    GoodFileNames.Add(FileName);
                    }
                }
            string[] ReturnArray = new string[GoodFileNames.Count];
            GoodFileNames.CopyTo(ReturnArray, 0);
            return ReturnArray;
            } 

        }
    }
