//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMainForm.h"
#include "Data.h"
#include "TWebSession.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWCompLabel"
#pragma link "IWControl"
#pragma link "IWExtCtrls"
#pragma link "IWCompEdit"
#pragma link "IWCompButton"
#pragma link "IWVCLBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// required by web server?
//---------------------------------------------------------------------------
void setAsMainForm()
   {
   TMainForm::SetAsMainForm(__classid(TMainForm));
   }
#pragma startup setAsMainForm
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// automatically redirect to user details form.
//---------------------------------------------------------------------------
void __fastcall TMainForm::IWAppFormResize(TObject *Sender)
   {
   TWebSession* webSession = (TWebSession*) WebApplication->Data;
   webSession->setup(WebApplication);
   AnsiString userName = WebApplication->RunParams->Values["username"];
   AnsiString password = WebApplication->RunParams->Values["password"];
   if (webSession->loginOk(userName.c_str(), password.c_str()))
      webSession->showDefaultForm(userName.c_str());
   else
      {
      AnsiString db = WebApplication->RunParams->Values["db"];

      Label1->Visible = true;
      Label2->Visible = true;
      Label3->Visible = true;
      Label4->Visible = true;
      Label2->Caption = "User name: " + userName;
      Label3->Caption = "Password: " + password;
      Label4->Caption = "Database: " + db;
      }
   }
//---------------------------------------------------------------------------

