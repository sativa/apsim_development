#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimDataTypesFile.h"
#include "ApsimDirectories.h"
#include <general\xml.h>

#pragma package(smart_init)
//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
ApsimDataTypesFile::ApsimDataTypesFile(void)
   : xmlDoc(getApsimDirectory() + "\\apsim\\infra\\datatypes.interface")
   {
   fileName = getApsimDirectory() + "\\apsim\\infra\\datatypes.interface";
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot find file: " + fileName);
   }
//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
ApsimDataTypesFile::ApsimDataTypesFile(const string& ddml)
   : xmlDoc(ddml, XMLDocument::xmlContents)
   {
   }
//---------------------------------------------------------------------------
// Return a specific data type to caller.  Will throw if that type doesn't
// exist.
//---------------------------------------------------------------------------
ApsimDataTypeData ApsimDataTypesFile::getDataType(const string& name)
   {
   XMLNode::iterator i = find_if(xmlDoc.documentElement().begin(),
                                 xmlDoc.documentElement().end(),
                                 NodeEquals<XMLNode>("type", name));
   if (i != xmlDoc.documentElement().end())
      return ApsimDataTypeData(*i);

   throw runtime_error("Cannot find a data type: " + name
                       + " in file: " + fileName);
   }
//---------------------------------------------------------------------------
// return iterator to first data type.
//---------------------------------------------------------------------------
ApsimDataTypesFile::iterator ApsimDataTypesFile::begin() const
   {
   return xmlDoc.documentElement().begin();
   }
//---------------------------------------------------------------------------
// return iterator to last data type.
//---------------------------------------------------------------------------
ApsimDataTypesFile::iterator ApsimDataTypesFile::end() const
   {
   return xmlDoc.documentElement().end();
   }

