//---------------------------------------------------------------------------

#ifndef TAfloWebSessionH
#define TAfloWebSessionH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TWebSession.h"
#include <ImgList.hpp>
class TAfloPaddockForm;
//---------------------------------------------------------------------------
class TAfloWebSession : public TWebSession
   {
   __published:	// IDE-managed Components
   private:	// User declarations
      TAfloPaddockForm* paddockForm;

   public:		// User declarations
      __fastcall TAfloWebSession(TComponent* Owner);

      //---------------------------------------------------------------------------
      // Get the application name
      //---------------------------------------------------------------------------
      virtual std::string getApplicationName(void) {return "AfloMan";}

      //---------------------------------------------------------------------------
      // Get the application url
      //---------------------------------------------------------------------------
      virtual std::string getApplicationUrl(void) {return "www.apsim.info/apsim/afloman/default.htm";}

      //---------------------------------------------------------------------------
      // Show the paddock form.
      //---------------------------------------------------------------------------
      virtual void showPaddockForm(const std::string& userName,
                                   const std::string& paddockName,
                                   bool readOnly,
                                   bool fromGrowerManagement);

   };
//---------------------------------------------------------------------------
extern PACKAGE TAfloWebSession *AfloWebSession;
//---------------------------------------------------------------------------
#endif
