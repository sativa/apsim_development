//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TYPServerController.h"
#include "TYPWebSession.h"
using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TServerController::TServerController(TComponent* Owner)
        : TIWServerControllerBase(Owner)
   {
   }
//---------------------------------------------------------------------------
// Called whenever a new session needs to be created
//---------------------------------------------------------------------------
void __fastcall TServerController::OnCreateNewSession(
      TIWApplication *ASession, TIWBaseForm *&VMainForm)
   {
   ASession->Data = new TYPWebSession(ASession);
   }
//---------------------------------------------------------------------------
void setServerController()
   {
   TServerController::SetServerControllerClass(__classid(TServerController));
   }

#pragma startup setServerController

