//---------------------------------------------------------------------------

#ifndef ApsimDataTypeDataH
#define ApsimDataTypeDataH
#include <general\xml.h>
#include <general\stristr.h>
// ------------------------------------------------------------------
//  Short description:
//     This class encapsulates a single data type.  The
//     class exposes different methods to extract information
//     about the data type.

//  Changes:
//    DPH 19/11/2001
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimDataTypeData
   {
   public:
      ApsimDataTypeData(XMLNode n) : node(n) { }

      bool isStructure(void) const;
      bool isArray(void) const;
      bool isBuiltIn(void) const;
      bool isArrayElement(void) const;
      std::string getName(void) const;
      std::string getKind(void) const;
      std::string getDescription(void) const;
      std::string getUnit(void) const;
      std::string getLowerBound(void) const;
      std::string getUpperBound(void) const;
      unsigned getNumFields(void) const;
      typedef TreeNodeAliasIterator< TreeNodeIterator<XMLNode>, ApsimDataTypeData> iterator;
      iterator begin() const {return iterator(TreeNodeIterator<XMLNode>(node.begin()));}
      iterator end() const   {return iterator(TreeNodeIterator<XMLNode>(node.end()));}
      std::string getTypeString(void) const;
   private:
      XMLNode node;
   };

#endif
