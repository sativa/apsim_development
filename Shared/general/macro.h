//---------------------------------------------------------------------------

#ifndef MacroH
#define MacroH
#include <list>
#include <string>
using namespace std;
//---------------------------------------------------------------------------
class Macro
   {
      public:
         Macro(string Name);                     //Constructor
         ~Macro();                               //Destructor
         void SetValues(list<string> Values);    //Directive to set macro values
         void GetValues(list<string> Values);    //Return macro values
         string MacroName;                       //The name of the macro
         void Translate(string& text);           //Directive to translate the given
                                                 //text for the macro values
      private:
         list<string> MacroValues;               //The list of macro values
   };
#endif
