//---------------------------------------------------------------------------
#ifndef ApsimRegistrationDataH
#define ApsimRegistrationDataH
#include <general\xml.h>
#include <general\string_functions.h>
// ------------------------------------------------------------------
// This class encapsulates a single component registration from an
// interface file.
// ------------------------------------------------------------------
class ApsimRegistrationData
   {
   public:
      ApsimRegistrationData(XMLNode n) : node(n) { }

      bool doAutoRegister(void) const {return !Str_i_Eq(node.getAttribute("autoregister"), "F");}
      std::string getName(void) const {return node.getAttribute("name");}
      std::string getType(void) const {return node.getName();}
      std::string getDataTypeName(void) const {return node.getAttribute("type");}

   private:
      XMLNode node;
   };
#endif

 