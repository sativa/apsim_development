//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("TMain_form.cpp", Main_form);
USERES("ContFileGen.res");
USEUNIT("k:\general\path.cpp");
USEUNIT("k:\general\string_functions.cpp");
USEUNIT("k:\general\stream_functions.cpp");
USEUNIT("k:\general\vcl_functions.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
   try
   {
       Application->Initialize();
       Application->CreateForm(__classid(TMain_form), &Main_form);
      Application->Run();
   }
   catch (Exception &exception)
   {
       Application->ShowException(&exception);
   }
   return 0;
}
//---------------------------------------------------------------------------
