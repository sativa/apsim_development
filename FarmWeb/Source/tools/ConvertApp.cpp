//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include <IWMain.hpp>
//---------------------------------------------------------------------------
USEFORM("MainForm.cpp", IWForm1); /* TIWAppForm: File Type */
USEFORM("ServerController.cpp", IWServerController); /* TIWServerControllerBase: File Type */
USEFORM("UserSessionUnit.cpp", IWUserSession); /* TIWUserSessionBase: File Type */
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
        {

        }
        return 0;
}
//---------------------------------------------------------------------------

#pragma link "IWIndy_70_60.lib"