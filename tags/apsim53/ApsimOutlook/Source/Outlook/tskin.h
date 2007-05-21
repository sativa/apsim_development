//---------------------------------------------------------------------------
#ifndef TSkinH
#define TSkinH
#include <string>
#include <ApsimShared\ApsimSettings.h>
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a "skin" for APSIM Outlook.

//  Notes:

//  Changes:
//    DPH 9/12/99

// ------------------------------------------------------------------
class TSkin
   {
   public:
      TSkin(void);
      ~TSkin(void);

      void DisplaySplashScreen(void);
      void InitApplication(void);
      void displayEvaluation(void);
      void displayHelp(void);

   private:
      TIcon* Icon;
      bool showBackdrop;
      std::string BitmapName;
      std::string evaluation;
      std::string helpFile;
      ApsimSettings settings;
   };
extern PACKAGE TSkin *Skin;

#endif
