//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Macro.h"
#include <general\string_functions.h>

//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------
Macro::Macro(string Contents)                                 //  CONSTRUCTOR
//---------------------------------------------------------------------------
// Description:
//    Constructor
// Notes:
//
   {
   ParseContents(Contents);

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
void Macro::GetValues(list<string>& Values)
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
         Replace_all(workstr,MacroKey.c_str(),(*I).c_str());
         text+=workstr;
      }
   }

//----------------------------------------------------------------------------
void Macro::ParseContents(const string& Contents)
//----------------------------------------------------------------------------
// Description:
//     Break up the contents string into description and values.
// Notes:
//
   {
   unsigned int posName = Contents.find("#name");
   if (posName != string::npos)
      {
      int posEOL = Contents.find("\n", posName);
      MacroName = Contents.substr(posName+strlen("#name"),
                                  posEOL-posName-strlen("#name"));
      Strip(MacroName, " ");
      }
   else
      throw string("Cannot find macro name");

   unsigned int posKey = Contents.find("#key");
   if (posKey != string::npos)
      {
      int posEOL = Contents.find("\n", posKey);
      MacroKey = Contents.substr(posKey+strlen("#key"),
                                         posEOL-posKey-strlen("#key"));
      Strip(MacroKey, " ");
      }
   else
      throw string("Cannot find macro key");

   unsigned int posValues = Contents.find("#values");
   if (posValues != string::npos)
      {
      int posEOL = Contents.find("\n", posValues);
      string MacroValuesString = Contents.substr(posValues+strlen("#values"),
                                                 posEOL-posValues-strlen("#values"));
      Split_string(MacroValuesString, " ", MacroValues);
      }
   else
      MacroValues.erase(MacroValues.begin(), MacroValues.end());
   }

