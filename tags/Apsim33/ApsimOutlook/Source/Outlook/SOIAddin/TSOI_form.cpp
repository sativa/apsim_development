//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSOI_form.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TSOI_form *SOI_form;
//---------------------------------------------------------------------------
__fastcall TSOI_form::TSOI_form(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TSOI_form::FormShow(TObject *Sender)
   {
   if (SOI_ptr != NULL) SOI_listbox->ItemIndex = SOI_ptr->FPhase_month - 1;
   SOI_toggleClick(Sender);
   }
//---------------------------------------------------------------------------
void __fastcall TSOI_form::FormClose(TObject *Sender, TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      SOI_ptr->FPhase_month = SOI_listbox->ItemIndex + 1;
      if (SOI_toggle->Checked)
         SOI_ptr->SOI_enabled = true;
      else
         SOI_ptr->SOI_enabled = false;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TSOI_form::SOI_toggleClick(TObject *Sender)
{
   if (SOI_toggle->Checked)
      SOI_listbox->Enabled = true;
   else
      SOI_listbox->Enabled = false;
}
//---------------------------------------------------------------------------

