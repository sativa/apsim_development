//---------------------------------------------------------------------------
#include <ActiveX.hpp>
#include <ComObj.hpp>
#include <WebBroker.hpp>
#include <ISAPIApp.hpp>
#include <Isapi2.hpp>
#include <IWInitISAPI.hpp>

#pragma hdrstop

USEFORM("TYPServerController.cpp", ServerController); /* TIWServerControllerBase: File Type */
USEFORM("TYPWebSession.cpp", YPWebSession); /* TIWUserSessionBase: File Type */
USEFORM("TMainForm.cpp", YPMainForm); /* TIWAppForm: File Type */
USEFORM("TYPSetupForm.cpp", YPSetupForm); /* TIWAppForm: File Type */
USEFORM("TYPPaddockForm.cpp", YPPaddockForm); /* TIWAppForm: File Type */
USEFORM("TYPNitrogenReportForm.cpp", YPNitrogenReportForm); /* TIWAppForm: File Type */
USEFORM("TMetStationForm.cpp", MetStationForm); /* TIWAppForm: File Type */
USEFORM("TSoilsForm.cpp", SoilsForm); /* TIWAppForm: File Type */
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