//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include <ApsimShared\ApsimDataTypesFile.h>
#include <general\Macro.h>
#include <general\xml.h>
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
std::string indent(int indentLevel)
   {
   return "   \"" + string(indentLevel*3, ' ');
   }
// ------------------------------------------------------------------
// convert a DDML string to a C formatted string
// ------------------------------------------------------------------
std::string ddmlToCPP(const ApsimDataTypeData& dataType,
                      bool isType = true,
                      int indentLevel = 0)
   {
   string st;
   if (isType)
      st = indent(indentLevel) + "<type";
   else
      st = indent(indentLevel) + "<field";
   if (dataType.isStructure())
      st += " name = \\\"" + dataType.getName() + "\\\"";

   if (dataType.isArray())
      st += " array=\\\"T\\\"";
   if (dataType.getNumFields() > 0)
      st += ">\" \\\n";
   else
      {
      st += " kind=\\\"" + dataType.getKind() + "\\\"";
      st += "/>\"\n";
      return st;
      }

   ApsimDataTypeData thisNode = dataType;
   if (dataType.isArray())
      {
      indentLevel++;
      st += indent(indentLevel) + "<element>\" \\\n";
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
         st += indent(indentLevel+1) + "<field name=\\\"" + field->getName()
             + "\\\" kind=\\\"" + field->getKind() + "\\\"";
         if (field->isArray())
            st += " array=\\\"T\\\"";
         st += "/>\" \\\n";
         }
      }
   if (dataType.isArray())
      {
      st += indent(indentLevel) + "</element>\" \\\n";
      indentLevel--;
      }
   if (isType)
      st += indent(indentLevel) + "</type>\"";
   else
      st += indent(indentLevel) + "</field>\"";
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
      XMLNode cddmlNode = child.appendChild("cddml");
      cddmlNode.setValue(ddmlToCPP(dataType));
      XMLNode forddmlNode = child.appendChild("forddml");
      forddmlNode.setValue(ddmlToFOR(dataType));

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
   To_lower(lowerName);
   if (structuresAlreadyDone.find(lowerName) == structuresAlreadyDone.end())
      {
      structuresAlreadyDone.insert(lowerName);

      // add field to a new macro.
      XMLNode child = node.appendChild("type", true);
      child.setAttribute("name", dataType.getName());
      child.setAttribute("kind", dataType.getKind());
      if (dataType.isArray())
         child.setAttribute("array", "T");
      else
         child.setAttribute("array", "F");
      XMLNode cddmlNode = child.appendChild("cddml");
      cddmlNode.setValue(ddmlToCPP(dataType));
      XMLNode forddmlNode = child.appendChild("forddml");
      forddmlNode.setValue(ddmlToFOR(dataType));
      }
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

