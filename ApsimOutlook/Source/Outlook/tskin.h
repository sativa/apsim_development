//---------------------------------------------------------------------------
#ifndef TSkinH
#define TSkinH
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

   private:
      TIcon* Icon;
   };
extern PACKAGE TSkin *Skin;

#endif
