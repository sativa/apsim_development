//---------------------------------------------------------------------------

#ifndef ClipboardtesterH
#define ClipboardtesterH
#include <iostream>
class __declspec(dllexport) Clipboardtester
   {
   public:
      Clipboardtester(const std::string& xml);
      ~Clipboardtester(void);
      Clipboardtester(const XMLNode& xml);  //added
      std::string RootName(){return doc->documentElement().getName();}
      bool isSoil(){return Str_i_Eq(RootName(),Soil());}
      bool isSample(){return Str_i_Eq(RootName(),Sample());}
      bool IsSoilOrSample(){return isSoil() || isSample();};
      std::string Soil(){return "Soil";}
      std::string Sample(){return "Sample";}
   private:
      XMLDocument* doc;
   };
//---------------------------------------------------------------------------
#endif
