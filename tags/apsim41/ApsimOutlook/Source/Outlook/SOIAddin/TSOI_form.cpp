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
   CheckBox1->Checked = (find(SOI_ptr->phasesToInclude.begin(),
                              SOI_ptr->phasesToInclude.end(),
                              1) != SOI_ptr->phasesToInclude.end());
   CheckBox2->Checked = (find(SOI_ptr->phasesToInclude.begin(),
                              SOI_ptr->phasesToInclude.end(),
                              2) != SOI_ptr->phasesToInclude.end());
   CheckBox3->Checked = (find(SOI_ptr->phasesToInclude.begin(),
                              SOI_ptr->phasesToInclude.end(),
                              3) != SOI_ptr->phasesToInclude.end());
   CheckBox4->Checked = (find(SOI_ptr->phasesToInclude.begin(),
                              SOI_ptr->phasesToInclude.end(),
                              4) != SOI_ptr->phasesToInclude.end());
   CheckBox5->Checked = (find(SOI_ptr->phasesToInclude.begin(),
                              SOI_ptr->phasesToInclude.end(),
                              5) != SOI_ptr->phasesToInclude.end());

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
   if (CheckBox1->Checked)
      SOI_ptr->phasesToInclude.push_back(1);
   if (CheckBox2->Checked)
      SOI_ptr->phasesToInclude.push_back(2);
   if (CheckBox3->Checked)
      SOI_ptr->phasesToInclude.push_back(3);
   if (CheckBox4->Checked)
      SOI_ptr->phasesToInclude.push_back(4);
   if (CheckBox5->Checked)
      SOI_ptr->phasesToInclude.push_back(5);

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

