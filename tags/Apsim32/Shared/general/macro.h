//---------------------------------------------------------------------------

#ifndef MacroH
#define MacroH
#include <vector>
#include <string>
#include "MacroValue.h"
// ------------------------------------------------------------------
//  Short description:
//    Encapsulates a specific macro complete with 0 or more values.

//  Changes:
//    DPH 22/8/2001 reworked from NIH original
// ------------------------------------------------------------------
class Macro
   {
   public:
      // constructor and destructor
      Macro(const std::string& contents);
      ~Macro();

      // != operator so that other objects can do a find on name.
      bool operator==(const std::string& rhsName)
         {
         return Str_i_Eq(getName(), rhsName);
         }

      // return a macro name to caller.
      std::string getName(void) const {return getAttribute("name");}

      // return a macro attribute to caller.
      std::string getAttribute(const std::string& name) const;

      // set a macro attribute.
      void setAttribute(const std::string& name, const std::string& value);

      // return the number of values to caller.
      unsigned int getNumValues(void) const {return values.size();}

      // return a complete list of macro value names to caller.
      void getValueNames(std::vector<std::string>& names);

      // return a specific macro value or NULL if not found.
      MacroValue* getValue(const std::string& name);

      // return a specific macro value or NULL if not found.
      MacroValue* getValue(unsigned int valueIndex)
         {
         return &values[valueIndex];
         }

      // add a macro value to our list of macros.
      void addValue(const MacroValue& value) {values.push_back(value);}

      // clear all macro values.
      void clearValues(void)
         {
         values.erase(values.begin(), values.end());
         }

      // Parse the specified string removing this macros' name and
      // replacing with 1 or more values.
      void performMacroReplacement(std::string& text);

      void Macro::performMacroAttributeReplacement(std::string& text);

   private:
      typedef std::map<std::string, std::string, std::less<std::string> > Attributes;
      Attributes attributes;
      std::vector<MacroValue> values;

      void parseString(const std::string& contents);
   };
#endif
