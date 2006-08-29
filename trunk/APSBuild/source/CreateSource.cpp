//---------------------------------------------------------------------------

#include <stdio.h>
#include <set>
#include <stdexcept>
#include <general/macro.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <general/stl_functions.h>
#include <ApsimShared/ApsimDataTypeData.h>
#include <ApsimShared/ApsimDataTypesFile.h>
#include "CreateSource.h"

using namespace std;

// ------------------------------------------------------------------
// convert a DDML 'kind' string to a CPP built in type.
// ------------------------------------------------------------------
std::string ddmlKindToCPP(const std::string& kind)
   {
   if (Str_i_Eq(kind, "integer4"))
      return "int";
   else if (Str_i_Eq(kind, "single"))
      return "float";
   else if (Str_i_Eq(kind, "double"))
      return "double";
   else if (Str_i_Eq(kind, "boolean"))
      return "bool";
   else if (Str_i_Eq(kind, "char"))
      return "char";
   else if (Str_i_Eq(kind, "string"))
      return "std::string";
   else
      return kind;
   }
// ------------------------------------------------------------------
// convert a DDML 'kind' string to a CPP in .NET built in type.
// ------------------------------------------------------------------
std::string ddmlKindToDotNet(const std::string& kind)
   {
   if (Str_i_Eq(kind, "integer4"))
      return "Int32";
   else if (Str_i_Eq(kind, "single"))
      return "Single";
   else if (Str_i_Eq(kind, "double"))
      return "Double";
   else if (Str_i_Eq(kind, "boolean"))
      return "Boolean";
   else if (Str_i_Eq(kind, "char"))
      return "Char";
   else if (Str_i_Eq(kind, "string"))
      return "String^";
   else
      return kind;   
   }

// ------------------------------------------------------------------
// convert a DDML 'kind' string to a FOR built in type.
// ------------------------------------------------------------------
std::string ddmlKindToFOR(const std::string& kind)
   {
   if (Str_i_Eq(kind, "integer4"))
      return "integer";
   else if (Str_i_Eq(kind, "single"))
      return "real";
   else if (Str_i_Eq(kind, "double"))
      return "double precision";
   else if (Str_i_Eq(kind, "boolean"))
      return "logical";
   else if (Str_i_Eq(kind, "char"))
      return "character(len=1)";
   else if (Str_i_Eq(kind, "string"))
      return "character(len=100)";
   else
      return kind;
   }
std::string indent(int indentAtStartOfLine, int indentLevel)
   {
   return string(indentAtStartOfLine, ' ') + "\"" + string(indentLevel*3, ' ');
   }
// ------------------------------------------------------------------
// convert a DDML string to a C formatted string
// ------------------------------------------------------------------
std::string ddmlToCPP(const ApsimDataTypeData& dataType,
                      bool isType = true,
                      int indentLevel = 0,
                      int indentAtStartOfLine = 3,
                      string lineDelimiter = "\\")
   {
   string st;
   if (isType)
      st = indent(indentAtStartOfLine, indentLevel) + "<type";
   else
      st = indent(indentAtStartOfLine, indentLevel) + "<field";
   if (dataType.isStructure())
      st += " name = \\\"" + dataType.getName() + "\\\"";

   if (dataType.isArray())
      st += " array=\\\"T\\\"";
   if (dataType.getNumFields() > 0)
      st += ">\" " + lineDelimiter + "\n";
   else
      {
      if (dataType.getKind() != "")
         st += " kind=\\\"" + dataType.getKind() + "\\\"";
      st += "/>\"\n";
      return st;
      }

   ApsimDataTypeData thisNode = dataType;
   if (dataType.isArray())
      {
      indentLevel++;
      st += indent(indentAtStartOfLine, indentLevel) + "<element>\" " + lineDelimiter + "\n";
      thisNode = *dataType.begin();
      }

   for (ApsimDataTypeData::iterator field = thisNode.begin();
                                    field != thisNode.end();
                                    field++)
      {
      if (field->isStructure())
         st += ddmlToCPP(*field, false, indentLevel+1) + " \\\n";
      else
         {
         st += indent(indentAtStartOfLine, indentLevel+1) + "<field name=\\\"" + field->getName()
             + "\\\" kind=\\\"" + field->getKind() + "\\\"";
         if (field->isArray())
            st += " array=\\\"T\\\"";
         st += "/>\" " + lineDelimiter + "\n";
         }
      }
   if (dataType.isArray())
      {
      st += indent(indentAtStartOfLine, indentLevel) + "</element>\" " + lineDelimiter + "\n";
      indentLevel--;
      }
   if (isType)
      st += indent(indentAtStartOfLine, indentLevel) + "</type>\"";
   else
      st += indent(indentAtStartOfLine, indentLevel) + "</field>\"";
   return st;
   }
// ------------------------------------------------------------------
// convert a DDML string to a FORTRAN formatted string
// ------------------------------------------------------------------
std::string ddmlToFOR(const ApsimDataTypeData& dataType)
   {
   string st;
   st = "      '<type";
   if (dataType.isStructure())
      st += " name=\"" + dataType.getName() + "\"";

   if (dataType.isArray())
      st += " array=\"T\"";
   if (dataType.getNumFields() > 0)
      st += ">' // &\n";
   else
      {
      st += " kind=\"" + dataType.getKind() + "\"";
      st += "/>'\n";
      return st;
      }

   ApsimDataTypeData thisNode = dataType;
   if (dataType.isArray())
      {
      st += "      '   <element>' // &\n";
      thisNode = *dataType.begin();
      }

   for (ApsimDataTypeData::iterator field = thisNode.begin();
                                    field != thisNode.end();
                                    field++)
      {
      st += "      '   <field name=\"" + field->getName()
          + "\" kind=\"" + field->getKind() + "\"";
      if (field->isArray())
         st += " array=\"T\"";

      st += "/>' // &\n";
      }
   if (dataType.isArray())
      st += "      '   </element>' // &\n";
   st += "      '</type>'";
   return st;
   }


class Field
   {
   public:
      Field(const string& parentName,
            const ApsimDataTypeData& field)
         {
         parentStructName = parentName;
         if (field.isStructure())
            dataTypeName = field.getName();
         else
            dataTypeName = field.getKind();
         name = field.getName();
         isArray = field.isArray();
         isStructure = field.isStructure();
         }
      void addFieldToMacro(XMLNode n)
         {
         XMLNode fieldNode = n.appendChild("field", true);
         fieldNode.setAttribute("name", name);
         string t = dataTypeName;
         if (t.length() > 0)
            {
            t.erase(1);
            To_upper(t);
            t = t + dataTypeName.substr(1);
            }

         fieldNode.setAttribute("KKind", t);
         fieldNode.setAttribute("ctype", ddmlKindToCPP(dataTypeName));
         fieldNode.setAttribute("fortype", ddmlKindToFOR(dataTypeName));
         if (isArray)
            fieldNode.setAttribute("array", "T");
         else
            fieldNode.setAttribute("array", "F");
         if (isStructure)
            fieldNode.setAttribute("isStructure", "T");
         else
            fieldNode.setAttribute("isStructure", "F");
         if (isStructure && isArray)
            fieldNode.setAttribute("isArrayAndStructure", "T");
         else
            fieldNode.setAttribute("isArrayAndStructure", "F");
         }
   private:
      string parentStructName;
      string dataTypeName;
      string name;
      bool isArray;
      bool isStructure;
   };

//---------------------------------------------------------------------------
// Process a structure.
//---------------------------------------------------------------------------
void processStructure(const ApsimDataTypeData& dataType, XMLNode& node)
   {
   static set<string> structuresAlreadyDone;
   string lowerName = dataType.getName();
   To_lower(lowerName);
   if (structuresAlreadyDone.find(lowerName) == structuresAlreadyDone.end())
      {
      structuresAlreadyDone.insert(lowerName);
      vector<Field> fields;
      ApsimDataTypeData thisNode = dataType;
      if (dataType.isArray())
         thisNode = *dataType.begin();
      bool hasAnArrayField = false;
      for (ApsimDataTypeData::iterator field = thisNode.begin();
                                       field != thisNode.end();
                                       field++)
         {
         if (field->isArray())
            hasAnArrayField = true;
         if (field->isStructure())
            processStructure(*field, node);
         fields.push_back(Field(dataType.getName(), *field));
         }
      // add structure to a new macro.
      XMLNode child = node.appendChild("structure", true);
      child.setAttribute("name", dataType.getName());
      if (hasAnArrayField)
         child.setAttribute("hasAnArrayField", "T");
      else
         child.setAttribute("hasAnArrayField", "F");
      if (dataType.isArray())
         child.setAttribute("array", "T");
      else
         child.setAttribute("array", "F");
      if (dataType.isMessage())
         child.setAttribute("message", "T");
      else
         child.setAttribute("message", "F");
      XMLNode cddmlNode = child.appendChild("cddml");
      cddmlNode.setValue(ddmlToCPP(dataType));
      XMLNode forddmlNode = child.appendChild("forddml");
      forddmlNode.setValue(ddmlToFOR(dataType));
      XMLNode dotnetddmlNode = child.appendChild("dotnetddml");
      dotnetddmlNode.setValue(ddmlToCPP(dataType, true, 0, 9, ""));

      for_each(fields.begin(), fields.end(),
               bind2nd(mem_fun_ref(&Field::addFieldToMacro), child));
      }
   }
//---------------------------------------------------------------------------
// Process a field.
//---------------------------------------------------------------------------
void processField(const ApsimDataTypeData& dataType, XMLNode& node)
   {
   static set<string> structuresAlreadyDone;
   string lowerName = dataType.getName();
   if (lowerName == "")
      {
      lowerName = dataType.getKind();
      if (dataType.isArray())
         lowerName = lowerName + "Array";
      }
   To_lower(lowerName);
   if (structuresAlreadyDone.find(lowerName) == structuresAlreadyDone.end())
      {
      structuresAlreadyDone.insert(lowerName);

      string t = dataType.getKind();
      if (t.length() > 0)
         {
         t.erase(1);
         To_upper(t);
         t = t + dataType.getKind().substr(1);
         }

      // add field to a new macro.
      XMLNode child = node.appendChild("type", true);
      child.setAttribute("name", lowerName);
      if (dataType.getKind() != "")
         {
         child.setAttribute("kind", dataType.getKind());
         child.setAttribute("KKind", t);
         child.setAttribute("ckind", ddmlKindToCPP(dataType.getKind()));
         child.setAttribute("dotnetkind", ddmlKindToDotNet(dataType.getKind()));
         }
      if (dataType.isArray())
         {
         child.setAttribute("array", "T");
         child.setAttribute("dotnetarraybits", "__gc[]");
         }
      else
         {
         child.setAttribute("array", "F");
         child.setAttribute("dotnetarraybits", " ");
         }
      XMLNode cddmlNode = child.appendChild("cddml");
      cddmlNode.setValue(ddmlToCPP(dataType));
      XMLNode forddmlNode = child.appendChild("forddml");
      forddmlNode.setValue(ddmlToFOR(dataType));
      }
   }
//---------------------------------------------------------------------------
// Process an event
//---------------------------------------------------------------------------
void processEvent(const ApsimDataTypeData& dataType, XMLNode& node)
   {
   // add field to a new macro.
   XMLNode child = node.appendChild("event", true);
   child.setAttribute("name", dataType.getName());
   child.setAttribute("type", dataType.getType());
   }

//---------------------------------------------------------------------------
// Performs the conversion using the specified ddml and the specified
// macro file.
//---------------------------------------------------------------------------
void CreateSource::go(const std::string& ddml,
                      const std::string& contents,
                      bool writeXML)
   {
   try
      {
      ApsimDataTypesFile dataTypes(ddml);

      XMLDocument xml("Data", XMLDocument::rootName);
      XMLNode rootNode = xml.documentElement();
      for (ApsimDataTypesFile::iterator dataType = dataTypes.begin();
                                        dataType != dataTypes.end();
                                        dataType++)
         {
         if (dataType->isStructure())
            processStructure(*dataType, rootNode);
         else if (dataType->isBuiltIn())
            processField(*dataType, rootNode);
         else if (dataType->isEvent())
            processEvent(*dataType, rootNode);
         }
      if (writeXML)
         xml.write("macro.xml");

      vector<string> filesGenerated;
      Macro macro;
      macro.go(rootNode, contents, filesGenerated);

      cout << "Wrote:";
      for (vector<string>::iterator f = filesGenerated.begin();
                                    f != filesGenerated.end();
                                    f++)
         cout << *f << " ";
      cout << endl;
      }
   catch (const runtime_error& err)
      {
      cerr << err.what() << endl;
      }
   }

