//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TUserDetailsForm.h"
#include "Data.h"
#include "TWebSession.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWCompLabel"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma link "IWOutlookBar"
#pragma link "IWCompEdit"
#pragma link "IWCompListbox"
#pragma link "IWTMSCal"
#pragma link "IWCompButton"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompRectangle"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TUserDetailsForm::TUserDetailsForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
void TUserDetailsForm::setup(TWebSession* session,
                             Data* d,
                             const string& userN)
   {
   webSession = session;
   userName = userN;
   data = d;

   NameEdit->Text = data->getNameOfUser(userName).c_str();
   EmailEdit->Text = data->getUserEmail(userName).c_str();
   SaveButton->Enabled = (webSession->isSaveAllowed());
   PasswordButton->Enabled = (webSession->isSaveAllowed());
   }
//---------------------------------------------------------------------------
// Save the user name and email.
//---------------------------------------------------------------------------
void __fastcall TUserDetailsForm::SaveButtonClick(TObject *Sender)
   {
   if (webSession->isSaveAllowed())
      {
      try
         {
         data->setNameOfUser(userName, NameEdit->Text.c_str());
         data->setUserEmail(userName, EmailEdit->Text.c_str());
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TUserDetailsForm::PasswordButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Enter a new password:",
                            "Reenter password:", "", "", changePasswordCallback);

   }
//---------------------------------------------------------------------------
// Change password callback.
//---------------------------------------------------------------------------
void __fastcall TUserDetailsForm::changePasswordCallback(bool okClicked,
                                                        AnsiString text1,
                                                        AnsiString text2,
                                                        AnsiString text3,
                                                        AnsiString text4)
   {
   if (okClicked && text1 != "" && text2 != "")
      {
      if (text1 != text2)
         webSession->showMessage("Passwords don't match. Password not changed.");
      else
         {
         data->changeUserPassword(userName, text1.c_str());
         webSession->show(this);
         }
      }
   else
      webSession->show(this);
   }
void __fastcall TUserDetailsForm::HelpButtonClick(TObject *Sender)
   {
   webSession->showHelp();
   }
//---------------------------------------------------------------------------

