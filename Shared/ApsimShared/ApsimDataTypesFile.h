//---------------------------------------------------------------------------

#ifndef ApsimDataTypesFileH
#define ApsimDataTypesFileH

#include <vector>
#include <ApsimShared\ApsimDataTypeData.h>
#include <general\xml.h>

//---------------------------------------------------------------------------
// This class encapsulates the apsim types database ie. (apsim\datatypes.interface)
//---------------------------------------------------------------------------
class __declspec(dllexport) ApsimDataTypesFile
   {
   public:
      ApsimDataTypesFile(void) throw(std::runtime_error);
      ApsimDataTypesFile(const std::string& ddml) throw(std::runtime_error);

      // Return a specific data type to caller.  Will throw if that type doesn't
      // exist.
      ApsimDataTypeData getDataType(const std::string& name) throw(std::runtime_error);

      typedef TreeNodeAliasIterator< TreeNodeIterator<XMLNode>, ApsimDataTypeData> iterator;
      iterator begin() const;
      iterator end() const;

   private:
      std::string fileName;
      XMLDocument xmlDoc;
   };
#endif
