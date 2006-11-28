#pragma hdrstop

#include "CMPData.h"

void getKindAndArray(const std::string& ddml,
                     std::string& kind, bool& isArray)
   {
   isArray = (ddml.find("array=\"T\"") != string::npos);
   int posKind = ddml.find("kind=\"");
   if (posKind != string::npos)
      {
      kind = ddml.substr(posKind + strlen("kind=\""));
      unsigned posQuote = kind.find('\"');
      if (posQuote == string::npos)
         throw runtime_error("Invalid data type string: " + ddml);
      kind.erase(posQuote);
      }
   }
