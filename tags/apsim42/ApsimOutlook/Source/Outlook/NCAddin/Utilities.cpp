//---------------------------------------------------------------------------


#pragma hdrstop

#include "Utilities.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------
//Description: Finds the index of a String in a TSL
//Notes:
//Changes:
//    AJD 10/8/2003 created
//---------------------------------------------------------------------------
int __fastcall FindStringInTSL(TStringList *TSL, String Var)
   {
   for(int i = 0; i < TSL->Count; i++)
      {
      if(TSL->Strings[i].UpperCase() == Var.UpperCase())
         {
         return i;
         }
      }

   return -1;
   }