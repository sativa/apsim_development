//---------------------------------------------------------------------------
#include <iostream.h>
#include "ComponentInterfaceGenerator.h"
//---------------------------------------------------------------------------
int main(int argc, char* argv[])
   {
   if (argc == 2)
      GenerateComponentInterface(argv[1]);
   else
      {
      cerr << "Usage: " << argv[0] << " <interface file>";
      return 1;
      }
   return 0;
   }
//---------------------------------------------------------------------------


