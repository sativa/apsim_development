//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORM("TMain_form.cpp", Main_form);
USEFORM("TFilespec_form.cpp", Filespec_import_form);
USEFORM("TBatch_import_form.cpp", Batch_import_form);
USELIB("general.lib");
USELIB("C:\Program Files\Borland\CBuilder5\Lib\memmgr.lib");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
   try
   {
      Application->Initialize();
      Application->CreateHandle();  // DAH: 3/8/2000

      Application->CreateForm(__classid(TMain_form), &Main_form);
       Application->CreateForm(__classid(TFilespec_import_form), &Filespec_import_form);
       Application->CreateForm(__classid(TBatch_import_form), &Batch_import_form);
       Application->Run();
   }
   catch (Exception &exception)
   {
      Application->ShowException(&exception);
   }
   return 0;
}
//---------------------------------------------------------------------------
