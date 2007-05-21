//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMultiStringList.h"
#include "TMultiStringListForm.h"
#pragma package(smart_init)
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TMultiStringList::TMultiStringList(void)
   : TPersistent ()
   {
   FItems = new TStringList;
   FPossibleItems = new TStringList;
   }

// ------------------------------------------------------------------
//  Short description:
//      destructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TMultiStringList::~TMultiStringList(void)
   {
   delete FItems;
   delete FPossibleItems;
   }

// ------------------------------------------------------------------
//  Short description:
//      allow user to edit the string list

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TMultiStringList::Edit()
   {
   TMultiStringListForm* Dlg = new TMultiStringListForm(Application);
   Dlg->Multi_string_list_ptr = this;
   Dlg->ShowModal();
   delete Dlg;
   }

