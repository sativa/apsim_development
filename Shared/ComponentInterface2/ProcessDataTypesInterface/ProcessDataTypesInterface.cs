using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Text;
using System.Xml;

namespace ProcessDataTypesInterface
    {
    class ProcessDataTypesInterface
        {
        static StringCollection TypesAlreadyDone = new StringCollection();

        static int Main(string[] args)
            {
            // -------------------------------------------------------------
            // Main entry point into program.
            // -------------------------------------------------------------
            try
                {
                // Comment the next 3 lines when not debugging.
                //string StdInPath = "c:\\development\\apsim\\infra\\datatypes.interface";
                //System.IO.TextReader StdInTextReader = new System.IO.StreamReader(StdInPath);
                //Console.SetIn(StdInTextReader);

                // read from stdin all contents, loop through all child nodes and create
                // a new <type> under 'NewInterfaceFile'
                XmlDocument InterfaceFile = new XmlDocument();
                InterfaceFile.Load(Console.In);

                XmlDocument NewInterfaceFile = new XmlDocument();
                NewInterfaceFile.Load(new StringReader("<?xml version=\"1.0\"?><types/>"));

                foreach (XmlNode DataType in InterfaceFile.DocumentElement.ChildNodes)
                    ProcessType(DataType, NewInterfaceFile, DataType.Name);

                // write new interface file to stdout.
                NewInterfaceFile.Save(Console.Out);
                return 0;
                }
            catch (Exception err)
                {
                Console.WriteLine(err.Message);
                }
            return 1;
            }

        private static void ProcessType(XmlNode OldDataType, XmlDocument NewDataTypes, string Type)
            {
            if (OldDataType.Name == "#comment") return;
            // -------------------------------------------------------------
            // Process the specified type and create a new <type> under
            // 'NewDataTypes'
            // -------------------------------------------------------------
            bool IsArray = hasAttribute(OldDataType, "array");

            string TypeName = getAttribute(OldDataType, "name");
            if (TypeName == "type")
                TypeName = "Null";

            if (TypesAlreadyDone.IndexOf(TypeName) == -1)
                {
                TypesAlreadyDone.Add(TypeName);

                // If this type is an array type then skip past the <element> tag.
                XmlNode ThisNode = OldDataType; 
                if (IsArray && hasChildTypeNamed(OldDataType, "element"))
                    ThisNode = getChildTypeNamed(OldDataType, "element");

                // Go through all child types first and process them.
                foreach (XmlNode child in ThisNode.ChildNodes)
                    if (child.ChildNodes.Count > 0)
                        ProcessType(child, NewDataTypes, Type);

                // Now create a new type and process all fields.
                XmlElement NewDataType = NewDataTypes.CreateElement(Type);
                NewDataTypes.DocumentElement.AppendChild(NewDataType);
                NewDataType.SetAttribute("name", TypeName);
                NewDataType.SetAttribute("cpptype", CalcCPPType(OldDataType));
                NewDataType.SetAttribute("ctype", CalcCType(OldDataType));
                NewDataType.SetAttribute("fortype", CalcForType(OldDataType));
                if (getAttribute(OldDataType,"boundable") == "T")
                    NewDataType.SetAttribute("boundable", "T");

                createCDataChild(NewDataType, "cddml", DDMLToCPP(OldDataType));
                createCDataChild(NewDataType, "forddml", DDMLToFOR(OldDataType));
                createCDataChild(NewDataType, "dotnetddml", DDMLToCPP(OldDataType));
                if (IsArray)
                    NewDataType.SetAttribute("array", "T");

                // copy fields to new data type.
                foreach (XmlNode child in ThisNode.ChildNodes)
                    {
                    XmlElement FieldDataType = createEmptyChild(NewDataType, "field");
                    FieldDataType.SetAttribute("name", getAttribute(child, "name"));
                    string Kind = getAttribute(child, "kind");
                    if (Kind != "")
                        {
                        FieldDataType.SetAttribute("kind", Kind);
                        FieldDataType.SetAttribute("dotnettype", DDMLKindToDotNet(Kind));
                        }
                    FieldDataType.SetAttribute("cpptype", CalcCPPType(child));
                    FieldDataType.SetAttribute("ctype", CalcCType(child));
                    }
                }
            }

        private static string CalcCPPType(XmlNode DataType)
            {
            // ------------------------------------------------------------------
            // convert a DDML 'kind' string to a CPP built in type.
            // ------------------------------------------------------------------
            string TypeName = getAttribute(DataType,"kind");
            if (TypeName == "")
                TypeName = getAttribute(DataType,"name");
            string LowerTypeName = TypeName.ToLower();
            string CTypeName;
            if (LowerTypeName == "integer4")
                CTypeName = "int";
            else if (LowerTypeName == "single")
                CTypeName = "float";
            else if (LowerTypeName == "double")
                CTypeName = "double";
            else if (LowerTypeName == "boolean")
                CTypeName = "bool";
            else if (LowerTypeName == "char")
                CTypeName = "char";
            else if (LowerTypeName == "string")
                CTypeName = "std::string";
            else
                CTypeName = TypeName;
            if (getAttribute(DataType,"array") == "T")
                CTypeName = "std::vector<" + CTypeName + ">";
            return CTypeName;
            }

        private static string CalcCType(XmlNode DataType)
            {
            // ------------------------------------------------------------------
            // convert a DDML 'kind' string to a CPP built in type.
            // ------------------------------------------------------------------
                string TypeName = getAttribute(DataType,"kind");
            if (TypeName == "")
                TypeName = getAttribute(DataType, "name");
            string LowerTypeName = TypeName.ToLower();
            string CTypeName;
            if (LowerTypeName == "integer4")
                CTypeName = "int";
            else if (LowerTypeName == "single")
                CTypeName = "float";
            else if (LowerTypeName == "double")
                CTypeName = "double";
            else if (LowerTypeName == "boolean")
                CTypeName = "bool";
            else if (LowerTypeName == "char")
                CTypeName = "char";
            else if (LowerTypeName == "string")
                CTypeName = "char*";
            else
                CTypeName = TypeName;
            return CTypeName;
            }

        private static string DDMLKindToDotNet(string kind)
            {
            // ------------------------------------------------------------------
            // convert a DDML 'kind' string to a CPP in .NET built in type.
            // ------------------------------------------------------------------
            string LowerKind = kind.ToLower();
            if (LowerKind == "integer4")
                return "Int32";
            else if (LowerKind == "single")
                return "Single";
            else if (LowerKind == "double")
                return "Double";
            else if (LowerKind == "boolean")
                return "Boolean";
            else if (LowerKind == "char")
                return "Char";
            else if (LowerKind == "string")
                return "String^";
            else
                return kind;
            }

        private static string CalcForType(XmlNode DataType)
            {
            // ------------------------------------------------------------------
            // convert a DDML 'kind' string to a FOR built in type.
            // ------------------------------------------------------------------
            string LowerKind = getAttribute(DataType,"kind").ToLower();
            if (LowerKind == "integer4")
                return "integer";
            else if (LowerKind == "single")
                return "real";
            else if (LowerKind == "double")
                return "double precision";
            else if (LowerKind == "boolean")
                return "logical";
            else if (LowerKind == "char")
                return "character(len=1)";
            else if (LowerKind == "string")
                return "character(len=100)";
            else
                return "";
            }

        private static string DDMLToCPP(XmlNode DataType)
            {
            // ------------------------------------------------------------------
            // convert a DDML string to a C formatted string
            // ------------------------------------------------------------------
            string st = DataType.OuterXml;
            st = "\"" + st.Replace("\"", "\\\"") + "\"";
            st = st.Replace("\r\n", "");
            st = st.Replace("><", ">\"\r\n               \"<");
            return st;
            }

        private static string DDMLToFOR(XmlNode DataType)
            {
            // ------------------------------------------------------------------
            // convert a DDML string to a C formatted string
            // ------------------------------------------------------------------
                string st = DataType.OuterXml;
            st = "'" + st + "'";
            st = st.Replace("\n", "");
            st = st.Replace("\r", "");
            return st;
            }

        private static bool hasAttribute(XmlNode node, string attribute)
            {
            if (node != null) 
                foreach (XmlAttribute a in node.Attributes)
                    if (a.Name == attribute)
                        return true;
            return false;
            }
        private static string getAttribute(XmlNode node, string attribute)
            {
            if (node != null)
                foreach (XmlAttribute a in node.Attributes)
                    if (a.Name == attribute)
                        return a.Value;
            return "";
            }
        private static void setAttribute(XmlElement node, string name, string attribute)
            {
            if (node != null)
                node.SetAttribute(name, attribute);
            }
        private static bool hasChildTypeNamed(XmlNode node, string name)
            {
            if (node != null)
                foreach (XmlNode child in node.ChildNodes)
                   if (child.Name == name)
                      return true;
            return false;
            }
        private static XmlNode getChildTypeNamed(XmlNode node, string name)
            {
            if (node != null)
                foreach (XmlNode child in node.ChildNodes) 
                   if (child.Name == name)
                      return child;
            return null;
            }
            private static XmlElement createCDataChild(XmlNode node, string name, string value)
            {
                if (node != null)
                {
                    XmlElement elem = node.OwnerDocument.CreateElement(name);
                    elem.AppendChild(node.OwnerDocument.CreateCDataSection(value));
                    node.AppendChild(elem);
                    return elem;
                }
                return null;
            }
            private static XmlElement createTextChild(XmlNode node, string name, string value)
            {
                if (node != null)
                {
                    XmlElement elem = node.OwnerDocument.CreateElement(name);
                    elem.AppendChild(node.OwnerDocument.CreateTextNode(value));
                    node.AppendChild(elem);
                    return elem;
                }
                return null;
            }
            private static XmlElement createEmptyChild(XmlNode node, string name)
            {
                if (node != null)
                {
                    XmlElement elem = node.OwnerDocument.CreateElement(name);
                    node.AppendChild(elem);
                    return elem;
                }
                return null;
            }
        }
}
