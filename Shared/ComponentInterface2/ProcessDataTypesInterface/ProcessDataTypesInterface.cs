using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using CSGeneral;
using VBGeneral;

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
                //string StdInPath = "d:\\development\\shared\\componentinterface\\datatypes.interface";
                //System.IO.TextReader StdInTextReader = new System.IO.StreamReader(StdInPath);
                //Console.SetIn(StdInTextReader);

                // read from stdin all contents, loop through all child nodes and create
                // a new <type> under 'NewInterfaceFile'
                string Contents = Console.In.ReadToEnd();
                APSIMData InterfaceFile = new APSIMData(Contents);
                APSIMData NewInterfaceFile = new APSIMData("types", "");
                foreach (APSIMData DataType in InterfaceFile.get_Children(null))
                    {
                    if (DataType.Type == "type")
                        ProcessType(DataType, NewInterfaceFile);
                    else if (DataType.Type == "builtin")
                        ProcessBuiltIn(DataType, NewInterfaceFile);
                    else
                        NewInterfaceFile.Add(DataType); // some other type
                    }

                // write new interface file to stdout.
                Console.Out.Write(NewInterfaceFile.XML);
                return 0;
                }
            catch (Exception err)
                {
                Console.WriteLine(err.Message);
                }
            return 1;
            }

        private static void ProcessType(APSIMData OldDataType, APSIMData NewDataTypes)
            {
            // -------------------------------------------------------------
            // Process the specified type and create a new <type> under
            // 'NewDataTypes'
            // -------------------------------------------------------------
            bool IsArray = (OldDataType.Attribute("array") == "T");

            string TypeName = OldDataType.Name;
            if (TypeName == "type")
                TypeName = "Null";

            if (TypesAlreadyDone.IndexOf(TypeName) == -1)
                {
                TypesAlreadyDone.Add(TypeName);

                // If this type is an array type then skip past the <element> tag.
                APSIMData ThisNode = OldDataType;
                if (IsArray)
                    ThisNode = OldDataType.Child("element");

                // Go through all child types first and process them.
                foreach (APSIMData child in ThisNode.get_Children(null))
                    {
                    if (child.ChildNames(null).Length > 0)
                        ProcessType(child, NewDataTypes);
                    }

                // Now create a new type and process all fields.
                APSIMData NewDataType = NewDataTypes.Add(new APSIMData("type", TypeName));
                NewDataType.SetAttribute("ctype", CalcCPPType(OldDataType));
                NewDataType.set_ChildValue("cddml", DDMLToCPP(OldDataType));
                NewDataType.set_ChildValue("forddml", DDMLToFOR(OldDataType));
                NewDataType.set_ChildValue("dotnetddml", DDMLToCPP(OldDataType));
                if (IsArray)
                    NewDataType.SetAttribute("array", "T");

                // copy fields to new data type.
                foreach (APSIMData child in ThisNode.get_Children(null))
                    {
                    APSIMData FieldDataType = NewDataType.Add(new APSIMData("field", child.Name));
                    string Kind = child.Attribute("kind");
                    if (Kind != "")
                        {
                        string KKind = Kind.Substring(0, 1).ToUpper() + Kind.Substring(1);
                        FieldDataType.SetAttribute("kind", Kind);
                        //FieldDataType.SetAttribute("KKind", KKind);
                        FieldDataType.SetAttribute("dotnettype", DDMLKindToDotNet(Kind));
                        }
                    FieldDataType.SetAttribute("ctype", CalcCPPType(child));
                    }
                }

            }

        private static void ProcessBuiltIn(APSIMData OldDataType, APSIMData NewDataTypes)
            {
            // -------------------------------------------------------------
            // Process the specified type and create a new <type> under
            // 'NewDataTypes'
            // -------------------------------------------------------------
            bool IsArray = (OldDataType.Attribute("array") == "T");

            string TypeName = OldDataType.Name;

            // Now create a new type and process all fields.
            APSIMData NewDataType = NewDataTypes.Add(new APSIMData("builtin", TypeName));
            NewDataType.SetAttribute("ctype", CalcCPPType(OldDataType));
            if (IsArray)
                NewDataType.SetAttribute("array", "T");
            }

        private static string CalcCPPType(APSIMData DataType)
            {
            // ------------------------------------------------------------------
            // convert a DDML 'kind' string to a CPP built in type.
            // ------------------------------------------------------------------
            string TypeName = DataType.Attribute("kind");
            if (TypeName == "")
                TypeName = DataType.Name;
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
            if (DataType.Attribute("array") == "T")
                CTypeName = "std::vector<" + CTypeName + ">";
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

        private static string DDMLKindToFOR(string kind)
            {
            // ------------------------------------------------------------------
            // convert a DDML 'kind' string to a FOR built in type.
            // ------------------------------------------------------------------
            string LowerKind = kind.ToLower();
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
                return kind;
            }

        private static string DDMLToCPP(APSIMData DataType)
            {
            // ------------------------------------------------------------------
            // convert a DDML string to a C formatted string
            // ------------------------------------------------------------------
            string st = DataType.XML;
            st = "\"" + st.Replace("\"", "\\\"") + "\"";
            st = st.Replace("\r\n", "");
            st = st.Replace("><", ">\"\r\n               \"<");
            return st;
            }

        private static string DDMLToFOR(APSIMData DataType)
            {
            // ------------------------------------------------------------------
            // convert a DDML string to a C formatted string
            // ------------------------------------------------------------------
            string st = DataType.XML;
            st = "'" + st + "'";
            st = st.Replace("\n", "");
            st = st.Replace("\r", "");
            return st;
            }

        }
    }
