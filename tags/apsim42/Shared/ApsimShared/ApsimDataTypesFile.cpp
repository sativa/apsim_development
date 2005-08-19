#include <string>
#include <vector>
#include <algorithm>

#include <stdexcept>

#include <boost/filesystem/operations.hpp>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>

#include "ApsimDataTypeData.h"
#include "ApsimDataTypesFile.h"
#include "ApsimDirectories.h"

using namespace std;

//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
ApsimDataTypesFile::ApsimDataTypesFile(void)
   : fileName(getApsimDirectory() + "/apsim/infra/datatypes.interface"), xmlDoc(fileName)
   {
   if (!xmlDoc.isValid())
      throw std::runtime_error("Cannot find file: " + fileName);
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

   throw std::runtime_error("Cannot find a data type: " + name
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

