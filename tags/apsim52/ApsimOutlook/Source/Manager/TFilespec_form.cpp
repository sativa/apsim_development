//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFilespec_form.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvDirectoryEdit"
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma resource "*.dfm"
TFilespec_import_form *Filespec_import_form;
//---------------------------------------------------------------------------
__fastcall TFilespec_import_form::TFilespec_import_form(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
