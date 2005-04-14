//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAfloWebSession.h"
#include "TAfloPaddockForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TWebSession"
#pragma resource "*.dfm"
TAfloWebSession *AfloWebSession;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TAfloWebSession::TAfloWebSession(TComponent* Owner)
   : TWebSession(Owner)
   {
   paddockForm = NULL;
   }
//---------------------------------------------------------------------------
// Show the paddock form.
//---------------------------------------------------------------------------
void TAfloWebSession::showPaddockForm(const std::string& userName,
                                      const string& paddockName,
                                      bool readOnly,
                                      bool fromGrowerManagement)
   {
   try
      {
      if (paddockForm == NULL)
         {
         paddockForm = new TAfloPaddockForm(WebApplication);
         setupForm(paddockForm);
         }

      paddockForm->setup(this, data, userName, paddockName, readOnly);
      show(paddockForm);
      }
   catch (const exception& err)
      {
      WebApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
