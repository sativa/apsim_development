//---------------------------------------------------------------------------

#ifndef CreateDataTypesF90H
#define CreateDataTypesF90H
#include <string>
#include <iostream>
// ------------------------------------------------------------------
//  Short description:
//     This class creates a datatypes source code in FORTRAN 90

//  Changes:
//    DPH 19/11/2001
// ------------------------------------------------------------------
class CreateDataTypesF90
   {
   public:
      CreateDataTypesF90(void) { }

      void convert(const std::string& filename, std::ostream& console);

   private:
      void doConvert(const std::string& sourceFilename,
                     const std::string& destFilename,
                     std::ostream& console);
   };
#endif
