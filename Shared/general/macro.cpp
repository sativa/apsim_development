//---------------------------------------------------------------------------
#pragma hdrstop

#include "Macro.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <sstream>
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
Macro::Macro(const std::string& contents)
   {
   parseString(contents);
   }
// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
Macro::~Macro()
   {
   // Do Nothing
   }
// ------------------------------------------------------------------
//  Short description:
//    return a list of macrovalue names to caller.

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
void Macro::getValueNames(vector<string>& values)
   {
   for_each(values.begin(), values.end(), GetNameFunction<vector<string>, MacroValue>(values));
   }
// ------------------------------------------------------------------
//  Short description:
//    return a specific macro value or NULL if not found.

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
MacroValue* Macro::getValue(const string& name)
   {
   vector<MacroValue>::iterator v = find(values.begin(), values.end(), name);
   if (v != values.end())
      return v;
   else
      return NULL;
   }
// ------------------------------------------------------------------
//  Short description:
//    Parse the specified string removing this macros' name and
//    replacing with 1 or more values.  Original text is left
//    unchanged.

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
string Macro::performMacroReplacement(const string& text)
   {
   string newText;
   for (vector<MacroValue>::iterator v = values.begin();
                                     v != values.end();
                                     v++)
      newText += v->performMacroReplacement(text, name);
   return newText;
   }

// ------------------------------------------------------------------
//  Short description:
//    Parse the specified string looking for macro specifications.

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
void Macro::parseString(const string& contents)
   {
   istringstream in(contents.c_str());
   string line;
   getline(in, line);
   name = getAttributeFromLine("name", line);

   // go looking for lines like:
   //    #value name="year" type="(y)"
   //    #value name="day"  type="(d)"
   // create a new MacroValue object for each and store in our values array.
   while (getline(in, line))
      {
      if (line.find("#value") != string::npos)
         values.push_back(MacroValue(line));
      }
   }

