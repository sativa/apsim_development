//---------------------------------------------------------------------------
#ifndef TUserDetailsFormH
#define TUserDetailsFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWCompLabel.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
#include "IWOutlookBar.hpp"
#include "IWCompEdit.hpp"
#include "IWCompListbox.hpp"
#include "IWTMSCal.hpp"
#include "IWCompButton.hpp"
#include <ImgList.hpp>
#include "IWBaseHTMLControl.hpp"
#include "IWCompRectangle.hpp"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"
class Data;
class TWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TUserDetailsForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWLabel *IWLabel5;
      TIWEdit *NameEdit;
      TIWEdit *EmailEdit;
      TIWLabel *IWLabel1;
      TIWButton *PasswordButton;
   TIWRectangle *IWRectangle1;
   TIWLink *SaveButton;
   TIWImageFile *IWImageFile1;
   TIWImageFile *HelpImage;
   TIWLink *HelpButton;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall PasswordButtonClick(TObject *Sender);
   void __fastcall HelpButtonClick(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;
      std::string userName;

   public:
      __fastcall TUserDetailsForm(TComponent* Owner);

      void setup(TWebSession* webSession, Data* data, const string& userName);
      //---------------------------------------------------------------------------
      // Change password callback.
      //---------------------------------------------------------------------------
      void __fastcall changePasswordCallback(bool okClicked,
                                             AnsiString text1,
                                             AnsiString text2,
                                             AnsiString text3,
                                             AnsiString text4);

   };
//---------------------------------------------------------------------------
#endif
