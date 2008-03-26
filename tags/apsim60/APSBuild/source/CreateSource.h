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
      // Performs the conversion using the specified ddml and the specified
      // macro file.
      //---------------------------------------------------------------------------
      void go(const std::string& ddml, const std::string& contents, bool writeXML);

   };
#endif
