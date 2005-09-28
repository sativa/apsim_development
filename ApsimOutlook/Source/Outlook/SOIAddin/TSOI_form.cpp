//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSOI_form.h"
#include <general\vcl_functions.h>
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

   Stl_2_tstrings(SOI_ptr->FPhase_names, PhaseCheckBox->Items);

   for (unsigned i = 0; i != SOI_ptr->phasesToInclude.size(); i++)
      {
      int Index = SOI_ptr->phasesToInclude[i]-1;
      if (Index < PhaseCheckBox->Items->Count)
         PhaseCheckBox->Checked[Index] = true;
      }

   AllYears->Checked = SOI_ptr->allYears;
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
   SOI_ptr->phasesToInclude.erase(SOI_ptr->phasesToInclude.begin(),
                                  SOI_ptr->phasesToInclude.end());

   for (int i = 0; i != PhaseCheckBox->Items->Count; i++)
      {
      if (PhaseCheckBox->Checked[i])
         SOI_ptr->phasesToInclude.push_back(i+1);
      }

   SOI_ptr->allYears = AllYears->Checked;
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

