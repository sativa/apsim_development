#pragma link "DBAdvGrd"
//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPredObsForm.h"
#include <general\vcl_functions.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TPredObsForm *PredObsForm;
//---------------------------------------------------------------------------
__fastcall TPredObsForm::TPredObsForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TPredObsForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   predobs = dynamic_cast<TPredObs*>(component);

   getComponentNames<TSEGTable>(component->Owner, PredCombo->Items);
   ObsCombo->Items->AddStrings(PredCombo->Items);

   if (predobs->predData != NULL)
      PredCombo->Text = predobs->predData->Name;

   if (predobs->obsData != NULL)
      ObsCombo->Text = predobs->obsData->Name;

   SetupKeyFieldNameList();
   }
//---------------------------------------------------------------------------
// User has changed the predicted dataset property
//---------------------------------------------------------------------------
void __fastcall TPredObsForm::PredComboChange(TObject *Sender)
   {
   predobs->predData = getComponent<TSEGTable>(predobs->Owner, PredCombo->Text);
   }
//---------------------------------------------------------------------------
// User has changed the observed dataset property
//---------------------------------------------------------------------------
void __fastcall TPredObsForm::ObsComboChange(TObject *Sender)
   {
   predobs->obsData = getComponent<TSEGTable>(predobs->Owner, ObsCombo->Text);
   SetupKeyFieldNameList();
   }
//---------------------------------------------------------------------------
// Setup the key field name listbox.
//---------------------------------------------------------------------------
void TPredObsForm::SetupKeyFieldNameList()
   {
   if (predobs->obsData != NULL)
      {
      FieldNameList->Items->Assign(predobs->obsData->FieldList);
      for (int i = 0; i != FieldNameList->Count; i++)
         FieldNameList->Selected[i] = (predobs->keyFields->IndexOf(FieldNameList->Items->Strings[i]) >= 0);
      }
   }
//---------------------------------------------------------------------------
// User has changed a selection in the field name list.
//---------------------------------------------------------------------------
void __fastcall TPredObsForm::FieldNameListClick(TObject *Sender)
   {
   TStringList* KeyFields = new TStringList;
   for (int i = 0; i != FieldNameList->Count; i++)
      {
      if (FieldNameList->Selected[i])
         KeyFields->Add(FieldNameList->Items->Strings[i]);
      }
   predobs->keyFields = KeyFields;
   delete KeyFields;
   }
//---------------------------------------------------------------------------

