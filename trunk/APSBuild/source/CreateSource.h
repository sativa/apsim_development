//---------------------------------------------------------------------------

#ifndef CreateSourceH
#define CreateSourceH
#include <iostream>
//---------------------------------------------------------------------------
// Converts a data types interface file to c++ and FORTRAN source code
// that can be linked to APSIM components.
//---------------------------------------------------------------------------
class CreateSource
   {
   public:

      //---------------------------------------------------------------------------
      // Performs the conversion using the specified ddml.  Writes source to
      // cppSource, hppSource (in c++) and forDataTypes and forDataTypesInterface
      // (in FORTRAN)
      //---------------------------------------------------------------------------
      void go(const std::string& ddml,
              std::ostream& cpp,
              std::ostream& hpp,
              std::ostream& forDataTypes,
              std::ostream& forDataTypesInterface);

   private:

   };
#endif
