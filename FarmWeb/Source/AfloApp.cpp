//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
USEFORM("TAfloServerController.cpp", ServerController); /* TIWServerControllerBase: File Type */
USEFORM("TAfloWebSession.cpp", AfloWebSession); /* TIWUserSessionBase: File Type */
USEFORM("TAfloMainForm.cpp", AfloMainForm); /* TIWAppForm: File Type */
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

#pragma link "IWIndy_70_60.lib"