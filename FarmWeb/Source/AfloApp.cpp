//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include "TMainForm.h"
USEFORM("TAfloServerController.cpp", ServerController); /* TIWServerControllerBase: File Type */
USEFORM("TAfloWebSession.cpp", AfloWebSession); /* TIWUserSessionBase: File Type */
USEFORM("TAfloPaddockForm.cpp", AfloPaddockForm); /* TIWAppForm: File Type */
//---------------------------------------------------------------------------
#include <IWMain.hpp>
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
   {
   try
      {
      Forms::Application->Initialize();
      Forms::Application->CreateForm(__classid(TFormIWMain), &FormIWMain);
       Forms::Application->Run();
      }
   catch (Exception &exception)
      {}

   return 0;
   }
//---------------------------------------------------------------------------
// required by web server?
//---------------------------------------------------------------------------
void setAsMainForm()
   {
   TMainForm::SetAsMainForm(__classid(TMainForm));
   }
#pragma link "IWIndy_70_60.lib"
#pragma link "IWLicenseKey.obj"

