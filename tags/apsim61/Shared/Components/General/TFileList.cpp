//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "TFileList.h"
#include <general\vcl_functions.h>
#pragma package(smart_init)
//---------------------------------------------------------------------------
namespace Tfilelist
   {
   void __fastcall PACKAGE Register()
      {
      RegisterPropertyEditor(__typeinfo(TFileList),
                             NULL,
                             "",
                             __classid(TFileListEditor));
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TFileList::TFileList(void)
   : TPersistent ()
   {
   FFile_names = new TStringList;
   FExtensions = new TStringList;
   FExtensions->Add("*.*");
   FMulti_select = true;
   }

// ------------------------------------------------------------------
//  Short description:
//      destructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TFileList::~TFileList(void)
   {
   delete FFile_names;
   delete FExtensions;
   }

// ------------------------------------------------------------------
//  Short description:
//      allow user to edit the file names list

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TFileList::Edit()
   {
   TOpenDialog* Dlg = new TOpenDialog(Application);
   Dlg->Options.Clear();
   if (FMulti_select)
      Dlg->Options << ofAllowMultiSelect;

   Dlg->Filter = Create_filter_string();
   string defaultext = FExtensions->Strings[0].c_str();
   defaultext.erase(0, 1);
   Dlg->DefaultExt = defaultext.c_str();
//   Give_files_to_open_dialog(Dlg, FFile_names);

   if (Dlg->Execute())
      {
      Get_files_from_open_dialog(Dlg, FFile_names);
      }
   delete Dlg;
   }

// ------------------------------------------------------------------
//  Short description:
//      create an opendialog filter string from extensions.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
AnsiString TFileList::Create_filter_string(void)
   {
   // loop through all extensions.
   string filter;
   for (int i = 0; i < FExtensions->Count; i++)
      {
      string Extension = FExtensions->Strings[0].c_str();
      if (filter.length() > 0)
         filter += "|";
      filter += Extension.substr(1) + " files|*" + Extension;
      }
   return filter.c_str();
   }

// ------------------------------------------------------------------
//  Short description:
//      allow user to edit the string list

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TFileListEditor::Edit()
   {
   TFileList *obj = (TFileList*) GetOrdValue();
   obj->Edit();
   SetOrdValue((long) obj);
   }

