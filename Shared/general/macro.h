//---------------------------------------------------------------------------

#ifndef MacroH
#define MacroH
#include <vector>
#include <string>
#include <general\general.h>
#include "MacroValue.h"
// ------------------------------------------------------------------
//  Short description:
//    Encapsulates a specific macro complete with 0 or more values.

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
class GENERAL_EXPORT Macro
   {
   public:
      // constructor and destructor
      Macro(const std::string& contents);
      ~Macro();

      // != operator so that other objects can do a find on name.
      bool operator!=(const std::string& rhsName)
         {
         return !Str_i_Eq(getName(), rhsName);
         }

      // return a macro name to caller.
      std::string getName(void) {return name;}

      // return a complete list of macro value names to caller.
      void getValueNames(std::vector<std::string>& names);

      // return a specific macro value or NULL if not found.
      MacroValue* getValue(const std::string& name);

      // add a macro value to our list of macros.
      void addValue(const MacroValue& value) {values.push_back(value);}

      // Parse the specified string removing this macros' name and
      // replacing with 1 or more values.  Original text is left
      // unchanged.
      std::string performMacroReplacement(const std::string& text);
   private:
      std::string name;
      std::vector<MacroValue> values;

      void parseString(const std::string& contents);
   };
#endif
