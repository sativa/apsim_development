//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TFilespec_form.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ToolEdit"
#pragma resource "*.dfm"
TFilespec_import_form *Filespec_import_form;
//---------------------------------------------------------------------------
__fastcall TFilespec_import_form::TFilespec_import_form(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
