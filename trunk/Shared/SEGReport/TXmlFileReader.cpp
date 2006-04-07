//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TXmlFileReader.h"
#include <vector>
#include <string>
#include <fstream>
#include <general\inifile.h>
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\path.h>
#include <general\xml.h>
#include <general\io_functions.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TXmlFileReader::TXmlFileReader(TComponent* owner)
   : TSEGTable(owner)
   {
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TXmlFileReader::~TXmlFileReader()
   {
   }
//---------------------------------------------------------------------------
// Component has loaded - assume the current dir is the reportDirectory.
//---------------------------------------------------------------------------
void __fastcall TXmlFileReader::Loaded(void)
   {
   TSEGTable::Loaded();
   reportDirectory = GetCurrentDir();
   }
//---------------------------------------------------------------------------
// set the 'fileName' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TXmlFileReader::setFileName(AnsiString file)
   {
   fileName = file;
   absoluteToRelativeFile();
   if (file != "")
      forceRefresh();
   }
//---------------------------------------------------------------------------
// If we already have a report directory then the current file will be
// relative to that dir.  Convert to absolute.
//---------------------------------------------------------------------------
void TXmlFileReader::relativeToAbsoluteFile(void)
   {
   if (reportDirectory != "")
      {
      SetCurrentDir(reportDirectory);
      fileName = ExpandFileName(fileName);
      }
   }
//---------------------------------------------------------------------------
// If we already have a report directory then the current file will be
// relative to that dir.  Convert to absolute.
//---------------------------------------------------------------------------
void TXmlFileReader::absoluteToRelativeFile(void)
   {
   if (reportDirectory != "")
      fileName = ExtractRelativePath(reportDirectory + "\\", fileName);
   }
//---------------------------------------------------------------------------
// Called by SEGReport to give components a chance to know the current
// report directory.  Used by ApsimFileReader to use relative paths.
//---------------------------------------------------------------------------
void TXmlFileReader::setReportDirectory(AnsiString reportDir)
   {
   relativeToAbsoluteFile();
   reportDirectory = reportDir;
   absoluteToRelativeFile();
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TXmlFileReader::createFields(void) throw(runtime_error)
   {
   FieldDefs->Clear();
   readXmlFile();

   try
      {
      relativeToAbsoluteFile();
      addDBFields(this, fieldNames, fieldValues);
      absoluteToRelativeFile();
      }
   catch (const runtime_error& error)
      {
      string msg = string(error.what()) + " File: " + fileName.c_str();
      throw runtime_error(msg);
      }
   return (true);
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TXmlFileReader::storeRecords(void) throw(runtime_error)
   {
   try
      {
      appendDBRecord(this, fieldNames, fieldValues);
      }
   catch (const runtime_error& error)
      {
      string msg = string(error.what()) + " File: " + fileName.c_str();
      throw runtime_error(msg);
      }
   }
//---------------------------------------------------------------------------
// read in the contents of our XML file into fieldNames and fieldValues
//---------------------------------------------------------------------------
void TXmlFileReader::readXmlFile()
   {
   fieldNames.erase(fieldNames.begin(), fieldNames.end());
   fieldValues.erase(fieldValues.begin(), fieldValues.end());
   if (FileExists(fileName))
      {
      XMLDocument xml(fileName.c_str());

      readXmlNode(xml.documentElement(), "");
      }
   }

//---------------------------------------------------------------------------
// read the specified XmlNode and add to fieldNames and fieldValues - recursive.
//---------------------------------------------------------------------------
void TXmlFileReader::readXmlNode(const XMLNode& node, const string& name)
   {
   if (node.begin() == node.end())
      {
      // no children.
      fieldNames.push_back(name);
      fieldValues.push_back(node.getValue());
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
         readXmlNode(*child, childName);
         }
      }
   }

// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void TXmlFileReader::setProperty(const std::string& propertyName,
                                   const std::string& propertyValue)
   {
   if (Str_i_Eq(propertyName, "filename"))
      setFileName(propertyValue.c_str());
   }

