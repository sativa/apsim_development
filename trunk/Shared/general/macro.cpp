//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Macro.h"
#include <general\string_functions.h>

//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------
Macro::Macro(string Name):                                    //  CONSTRUCTOR
   MacroName(Name)
//---------------------------------------------------------------------------
// Description:
//    Constructor
// Notes:
//
   {
      //Do nothing else
   }
//----------------------------------------------------------------------------
Macro::~Macro()                                               //   DESTRUCTOR
//----------------------------------------------------------------------------
// Description:
//    Destructor
// Notes:
//
   {
   // Do Nothing
   }
//----------------------------------------------------------------------------
void Macro::SetValues(list<string> Values)
//----------------------------------------------------------------------------
// Description:
//    Set the internal list of macro values
// Notes:
//
   {
   MacroValues = Values;
   }
//----------------------------------------------------------------------------
void Macro::GetValues(list<string> Values)
//----------------------------------------------------------------------------
// Description:
//    Provide the list of macro values
// Notes:
//
   {
   Values = MacroValues;
   }
//----------------------------------------------------------------------------
void Macro::Translate(string& text)
//----------------------------------------------------------------------------
// Description:
//    Translate the given text for the values of this macro.
//    This requires appending a copy of the original text with each
// Notes:
//
   {
    string original = text;   // Keep a copy of the original text provided

    text = "";                // Now clear the text and incrementally rebuild it

    for (list<string>::iterator I = MacroValues.begin();
                                I != MacroValues.end();
                                I++)
      {
         // Now, add a copy of the original with macro substituted
         string workstr = original;
         Replace_all(workstr,MacroName.c_str(),(*I).c_str());
         text+=workstr;
      }
   }