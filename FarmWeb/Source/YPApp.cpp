//---------------------------------------------------------------------------
//#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
USEFORM("TYPServerController.cpp", ServerController); /* TIWServerControllerBase: File Type */
USEFORM("TYPWebSession.cpp", YPWebSession); /* TIWUserSessionBase: File Type */
USEFORM("TYPSetupForm.cpp", YPSetupForm); /* TIWAppForm: File Type */
USEFORM("TYPPaddockForm.cpp", YPPaddockForm); /* TIWAppForm: File Type */
USEFORM("TMetStationForm.cpp", MetStationForm); /* TIWAppForm: File Type */
USEFORM("TYPNitrogenReportForm.cpp", YPNitrogenReportForm); /* TIWAppForm: File Type */
USEFORM("TSoilsForm.cpp", SoilsForm); /* TIWAppForm: File Type */
USEFORM("TMainForm.cpp", MainForm); /* TIWAppForm: File Type */
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