//---------------------------------------------------------------------------

#ifndef TViewReportFormH
#define TViewReportFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompRectangle.hpp"
#include "IWControl.hpp"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"
#include "IWVCLBaseControl.hpp"

class TWebSession;

//---------------------------------------------------------------------------
class TViewReportForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWRectangle *IWRectangle1;
      TIWImageFile *IWImageFile2;
      TIWLink *BackButton;
      TIWImageFile *Image;
      void __fastcall BackButtonClick(TObject *Sender);
   private:	// User declarations
      TWebSession* webSession;
      std::string userName;

   public:		// User declarations
      __fastcall TViewReportForm(TComponent* Owner);
      void setup(TWebSession* webSession, const std::string& fileName, const std::string& userName);


   };
//---------------------------------------------------------------------------
#endif
