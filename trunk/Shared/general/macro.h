//---------------------------------------------------------------------------

#ifndef MacroH
#define MacroH
#include <list>
#include <string>
#include <general\general.h>
using namespace std;
//---------------------------------------------------------------------------
class GENERAL_EXPORT Macro
   {
      public:
         Macro(string Contents);                 //Constructor
         ~Macro();                               //Destructor
         void SetValues(list<string> Values);    //Directive to set macro values
         void GetValues(list<string>& Values);    //Return macro values
         void Translate(string& text);           //Directive to translate the given
         string MacroName;                       // Macro name
         string MacroKey;                        // RAW Key e.g. $$nrate
      private:
         list<string> MacroValues;               //The list of macro values

         void ParseContents(const string& Contents);

   };
#endif
