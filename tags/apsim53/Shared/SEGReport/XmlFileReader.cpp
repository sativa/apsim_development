//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "XmlFileReader.h"
#include <vector>
#include <string>
#include <fstream>
#include <general\inifile.h>
#include <general\db_functions.h>
#include <general\string_functions.h>
#include <general\path.h>
#include <general\xml.h>
#include <general\io_functions.h>

using namespace std;
//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void XmlFileReader::createFields(TDataSet* source, TDataSet* result)
   {
   addDBField(result, "name", "xx");
   addDBField(result, "value", "xx");
   }

//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void XmlFileReader::process(TDataSet* pred, TDataSet* result)
   {
   std::string fileName = getProperty("filename");
   readXmlFile(fileName, fieldNames, fieldValues);
   for (unsigned i = 0; i != fieldNames.size(); i++)
      {
      result->Append();
      result->FieldValues["name"] = fieldNames[i].c_str();
      result->FieldValues["value"] = fieldValues[i].c_str();
      result->Post();
      }
   }

//---------------------------------------------------------------------------
// read in the contents of our XML file into fieldNames and fieldValues
//---------------------------------------------------------------------------
void XmlFileReader::readXmlFile(const std::string& fileName,
                                vector<string>& fieldNames,
                                vector<string>& fieldValues)
   {
   if (!FileExists(fileName))
      throw runtime_error("Cannot find XML file: " + fileName);

   fieldNames.erase(fieldNames.begin(), fieldNames.end());
   fieldValues.erase(fieldValues.begin(), fieldValues.end());
   XMLDocument xml(fileName.c_str());
   readXmlNode(xml.documentElement(), "", fieldNames, fieldValues);
   }

//---------------------------------------------------------------------------
// read the specified XmlNode and add to fieldNames and fieldValues - recursive.
//---------------------------------------------------------------------------
void XmlFileReader::readXmlNode(const XMLNode& node, const string& name,
                                vector<string>& fieldNames,
                                vector<string>& fieldValues)
   {
   if (node.begin() == node.end())
      {
      // no children.
      // make sure the name doesn't clash with an existing name.
      string fieldName = name;
      int index = 1;
      while (find(fieldNames.begin(), fieldNames.end(), fieldName) != fieldNames.end())
         {
         fieldName = name;
         unsigned posPeriod = fieldName.rfind('.');
         if (posPeriod == string::npos)
            posPeriod = fieldName.length();
         index++;
         fieldName.insert(posPeriod, itoa(index));
         }

      fieldNames.push_back(fieldName);
      string value = node.getValue();
      if (value == "")
         value = "?";
      fieldValues.push_back(value);
      }
   else
      {
      if (node.getAttribute("name") != "")
         {
         fieldNames.push_back(name + ".name");
         fieldValues.push_back(node.getAttribute("name"));
         }
      for (XMLNode::iterator child = node.begin();
                             child != node.end();
                             child++)
         {
         string childName = makeNameUnique(node, *child);

         if (name  != "")
            childName = name + "." + childName;
         readXmlNode(*child, childName, fieldNames, fieldValues);
         }
      }
   }

//---------------------------------------------------------------------------
// Return a unique name for the specified node unique amongst the children of
// the specified parent node
//---------------------------------------------------------------------------
string XmlFileReader::makeNameUnique(const XMLNode& parentNode, const XMLNode& node)
   {
   string name = node.getName();
   int numMatches = 0;
   int nodeIndex = 0;
   for (XMLNode::iterator child = parentNode.begin();
                          child != parentNode.end();
                          child++)
      {
      if (name == child->getName())
         numMatches++;
      if (node == *child)
         nodeIndex = numMatches;
      }
   if (nodeIndex == 0)
      throw runtime_error("Internal failure in XmlFileReader::makeNameUnique");
   if (nodeIndex == 1)
      return name;
   else
      return name + itoa(nodeIndex);
   }

