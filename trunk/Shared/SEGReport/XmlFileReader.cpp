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
   std::string fileName = getProperty("filename");

   readXmlFile(fileName, fieldNames, fieldValues);
   addDBFields(result, fieldNames, fieldValues);
   }

//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void XmlFileReader::process(TDataSet* pred, TDataSet* result)
   {
   appendDBRecord(result, fieldNames, fieldValues);
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
      for (XMLNode::iterator child = node.begin();
                             child != node.end();
                             child++)
         {
         string childName = child->getAttribute("name");
         if (Str_i_Eq(child->getName(), "YPPaddock"))
            {
            fieldNames.push_back("paddockname");
            fieldValues.push_back(childName);
            childName = "";
            }
         else if (childName == "")
            childName = child->getName();
         if (name  != "")
            childName = name + "." + childName;
         readXmlNode(*child, childName, fieldNames, fieldValues);
         }
      }
   }


