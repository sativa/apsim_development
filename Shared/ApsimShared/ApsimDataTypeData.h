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

      bool isStructure(void) const
         {return (begin() != end());}
      bool isArray(void) const
         {return (strcmpi(node.getAttribute("array").c_str(), "T") == 0);}
      bool isBuiltIn(void) const
         {return (strcmpi(node.getAttribute("builtin").c_str(), "T") == 0);}
      bool isArrayElement(void) const
         {return (strcmpi(node.getName().c_str(), "element") == 0);}
      std::string getName(void) const
         {return node.getAttribute("name");}

      std::string getKind(void) const
         {return node.getAttribute("kind");}
      std::string getDescription(void) const
         {return node.getAttribute("description");}
      std::string getUnit(void) const
         {return node.getAttribute("unit");}
      std::string getLowerBound(void) const
         {return node.getAttribute("lower_bound");}
      std::string getUpperBound(void) const
         {return node.getAttribute("upper_bound");}
      unsigned getNumFields(void) const;
      typedef TreeNodeAliasIterator< TreeNodeIterator<XMLNode>, ApsimDataTypeData> iterator;
      iterator begin() const {return iterator(TreeNodeIterator<XMLNode>(node.begin()));}
      iterator end() const   {return iterator(TreeNodeIterator<XMLNode>(node.end()));}

   private:
      XMLNode node;
   };

#endif
