//---------------------------------------------------------------------------
#ifndef LoaderH
#define LoaderH
#include "basetypes.h"
// ------------------------------------------------------------------
//  Short description:
//    Encapsulates a component DLL loader.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class PROTOCOLLoader
   {
   public:
      PROTOCOLLoader(void) { };

      void loadComponent(const std::string& aDLLName, PROTOCOLDLLInfo& dllInfo);
      void unloadComponent(const PROTOCOLDLLInfo& dllInfo);


   };
#endif
