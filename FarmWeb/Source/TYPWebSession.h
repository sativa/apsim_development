//---------------------------------------------------------------------------

#ifndef TYPWebSessionH
#define TYPWebSessionH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TWebSession.h"
#include <ImgList.hpp>
#include "IWImageList.hpp"
class TYPPaddockForm;
class TYPSetupForm;
class TSoilsForm;
class TYPNitrogenReportForm;
class TMetStationForm;
//---------------------------------------------------------------------------
class TYPWebSession : public TWebSession
   {
   __published:
   private:
      TYPPaddockForm* paddockForm;
      TYPSetupForm* ypsetupForm;
      TSoilsForm* soilsForm;
      TYPNitrogenReportForm* nitrogenReportForm;
      TMetStationForm* metStationForm;

      //---------------------------------------------------------------------------
      // setup the specified form.
      //---------------------------------------------------------------------------
      virtual int setupForm(TIWAppForm* form);

      //---------------------------------------------------------------------------
      // Process the bar item click event.
      //---------------------------------------------------------------------------
      virtual void processClickItem(AnsiString caption);

      //---------------------------------------------------------------------------
      // Show the soils form.
      //---------------------------------------------------------------------------
      void showSoilsForm();

      //---------------------------------------------------------------------------
      // Show the met stations form.
      //---------------------------------------------------------------------------
      void showMetStationsForm();


   public:
      __fastcall TYPWebSession(TComponent* Owner);
      __fastcall ~TYPWebSession(void);

      //---------------------------------------------------------------------------
      // Get the application name
      //---------------------------------------------------------------------------
      virtual std::string getApplicationName(void) {return "YieldProphet";}

      //---------------------------------------------------------------------------
      // Get the application url
      //---------------------------------------------------------------------------
      virtual std::string getApplicationUrl(void) {return "www.yieldprophet.com.au";}

      //---------------------------------------------------------------------------
      // Show the paddock form.
      //---------------------------------------------------------------------------
      void showPaddockForm(const std::string& userName,
                           const string& paddockName,
                           bool readOnly,
                           bool fromGrowerManagement);
      //---------------------------------------------------------------------------
      // Show the paddock setup form.
      //---------------------------------------------------------------------------
      void showSetupForm(const std::string& userName,
                         const string& paddockName,
                         bool readOnly,
                         bool fromGrowerManagement);

      //---------------------------------------------------------------------------
      // Show the nitrogen report form.
      //---------------------------------------------------------------------------
      void showNitrogenReportForm(const std::string& userName,
                                  const std::string& paddockName,
                                  const std::string& emailAddress);

   };
//---------------------------------------------------------------------------
extern PACKAGE TYPWebSession *YPWebSession;
//---------------------------------------------------------------------------
#endif
