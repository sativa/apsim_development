//---------------------------------------------------------------------------
#ifndef ReportMacrosH
#define ReportMacrosH

#include <vector>
#include <string>
//---------------------------------------------------------------------------
// This class is responsible for all macro replacements on an SEG report.
//---------------------------------------------------------------------------
class ReportMacros
   {
   public:
      //---------------------------------------------------------------------------
      // Do all macro replacements. Does not modify the input text.
      //---------------------------------------------------------------------------
      AnsiString doReplacement(TComponent* owner, AnsiString text);

      void getReferencedComponents(std::vector<std::string>& names)
         {
         names = componentNames;
         }

   private:
      std::vector<std::string> componentNames;
      std::string resolve(TComponent* owner, const std::string& text);
   };


#endif