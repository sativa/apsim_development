//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ServerController.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
__fastcall TIWServerController::TIWServerController(TComponent* Owner)
        : TIWServerControllerBase(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TIWServerController::IWServerControllerBaseNewSession(
      TIWApplication *ASession, TIWBaseForm *&VMainForm)
{
  ASession->Data = new TIWUserSession(NULL);
}
//---------------------------------------------------------------------------


void setServerController() {
  TIWServerController::SetServerControllerClass(__classid(TIWServerController));
}

#pragma startup setServerController
 