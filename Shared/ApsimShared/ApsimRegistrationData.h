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

      bool doAutoRegister(void) const;
      std::string getName(void) const;
      std::string getType(void) const;
      std::string getDataTypeName(void) const;

   private:
      XMLNode node;
   };
#endif

 