//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include "ComponentInterfaceGenerator.h"
#include <ApsimShared\ApsimProject.h>
#include <general\path.h>
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
