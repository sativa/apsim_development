//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ReportMacros.h"
#include <general\stringtokenizer.h>
#include <general\string_functions.h>
#include <general\vcl_functions.h>
#pragma package(smart_init)

using namespace std;

//---------------------------------------------------------------------------
// Evaluate the specified macro.
//---------------------------------------------------------------------------
string ReportMacros::evaluateMacro(TComponent* owner, const string& macro, const string& argumentString)
   {
   string returnValue;

   vector<string> arguments;
   Split_string(argumentString, ",", arguments);

   if (macro == "$property")
      {
      if (arguments.size() == 1)
         {
         returnValue = resolveComponentPropertyMacro(owner, arguments[0].c_str()).c_str();

         // get the referenced component name.
         unsigned posPeriod = arguments[0].find('.');
         componentNames.push_back(arguments[0].substr(0, posPeriod));
         }
      }
   else if (macro == "$precision")
      {
      if (arguments.size() == 2)
         {
         returnValue = resolveComponentPropertyMacro(owner, arguments[0].c_str()).c_str();
         try
            {
            double value = StrToFloat(returnValue.c_str());
            returnValue = ftoa(value, StrToInt(arguments[1].c_str()));
            }
         catch (Exception& err)
            {
            returnValue = "";
            }
         // get the referenced component name.
         unsigned posPeriod = arguments[0].find('.');
         componentNames.push_back(arguments[0].substr(0, posPeriod));
         }
      }
   return returnValue;
   }
//---------------------------------------------------------------------------
// Do all macro replacements. Does not modify the input text.
//---------------------------------------------------------------------------
AnsiString ReportMacros::doReplacement(TComponent* owner, AnsiString text)
   {
   componentNames.erase(componentNames.begin(), componentNames.end());

   string st = text.c_str();

   unsigned posMacro = st.find('$');
   while (posMacro != string::npos)
      {
      unsigned posOpenBracket = st.find('(', posMacro);
      unsigned posCloseBracket = st.find(')', posOpenBracket);
      if (posOpenBracket != string::npos && posCloseBracket != string::npos)
         {
         string macroName = st.substr(posMacro, posOpenBracket-posMacro);
         string arguments = st.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1);
         string value = evaluateMacro(owner, macroName, arguments);
         if (value != "")
            st.replace(posMacro, posCloseBracket-posMacro+1, value);
         }

      posMacro = st.find('$', posMacro+1);
      }
   return st.c_str();
   }
