//---------------------------------------------------------------------------
#include <ActiveX.hpp>
#include <ComObj.hpp>
#include <WebBroker.hpp>
#include <ISAPIApp.hpp>
#include <Isapi2.hpp>
#include <IWInitISAPI.hpp>

#pragma hdrstop

USEFORM("MainForm.cpp", IWForm1); /* TIWAppForm: File Type */
USEFORM("ServerController.cpp", IWServerController); /* TIWServerControllerBase: File Type */
USEFORM("UserSessionUnit.cpp", IWUserSession); /* TIWUserSessionBase: File Type */
//---------------------------------------------------------------------------
#define Application Webbroker::Application

#pragma link "isapiapp.obj"
#pragma link "webbroker.obj"
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  try {
    switch (reason) {
      case DLL_PROCESS_ATTACH: {
        CoInitFlags = COINIT_MULTITHREADED;
        IWRun();
        break;
      }
    }

  }
  catch (Exception &exception)
  {
  }
  return 1;
}
//---------------------------------------------------------------------------
extern "C"
{
  BOOL __declspec(dllexport) WINAPI GetExtensionVersion(Isapi2::THSE_VERSION_INFO &Ver)
  {
    return Isapiapp::GetExtensionVersion(Ver);
  }
  //---------------------------------------------------------------------------
  unsigned __declspec(dllexport) WINAPI HttpExtensionProc(Isapi2::TEXTENSION_CONTROL_BLOCK &ECB)
  {
    return Isapiapp::HttpExtensionProc(ECB);
  }
  //---------------------------------------------------------------------------
  BOOL __declspec(dllexport) WINAPI TerminateExtension(int dwFlags)
  {
    return Isapiapp::TerminateExtension(dwFlags);
  }
}
//---------------------------------------------------------------------------
#undef Application
//---------------------------------------------------------------------------

#pragma link "IWIndy_70_60.lib"