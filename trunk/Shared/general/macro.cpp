//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
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
//    return a specific attribute or blank if not found.

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
string Macro::getAttribute(const string& name) const
   {
   Attributes::const_iterator attributeI = attributes.find(name);
   if (attributeI == attributes.end())
      return "";
   else
      return attributeI->second;
   }

// ------------------------------------------------------------------
//  Short description:
//    Parse the specified string removing this macros' name and
//    replacing with 1 or more values.

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
void Macro::performMacroReplacement(string& text)
   {
   string original = text;   // Keep a copy of the original text provided

   text = "";                // Now clear the text and incrementally rebuild it

   for (vector<MacroValue>::iterator v = values.begin();
                                     v != values.end();
                                     v++)
      {
      // Now, add a copy of the original with macro substituted
      string workstr = original;
      v->performMacroReplacement(workstr, getName());
      text += workstr;
      }
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
   vector<string> names, vals;
   getAttributesFromLine(line, names, vals);
   for (unsigned int n = 0; n < names.size(); n++)
      attributes.insert(Attributes::value_type(names[n], vals[n]));

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
// ------------------------------------------------------------------
// Set a macro value.
// ------------------------------------------------------------------
void Macro::setAttribute(const string& name, const string& value)
   {
   attributes.insert(Attributes::value_type(name, value));
   }
// ------------------------------------------------------------------
//  Short description:
//    Parse the specified string replacing any attributes found with
//    their values.
// ------------------------------------------------------------------
void Macro::performMacroAttributeReplacement(string& text)
   {
   // replace all attributes in text.
   for (Attributes::iterator a = attributes.begin();
                             a != attributes.end();
                             a++)
      {
      string stringToFind = getName() + "." + a->first;
      Replace_all(text, stringToFind.c_str(), a->second.c_str());
      }
   }

