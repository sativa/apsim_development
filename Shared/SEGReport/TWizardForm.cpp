//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "TWizardForm.h"
#include <algorithm>
using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TWizardForm *WizardForm;
//---------------------------------------------------------------------------
__fastcall TWizardForm::TWizardForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void TWizardForm::addComponentForm(TForm* componentForm)
   {
   componentForms.push_back(componentForm);
   }
//---------------------------------------------------------------------------
void __fastcall TWizardForm::FormShow(TObject *Sender)
   {
   currentForm = 0;
   showForm();
   }
//---------------------------------------------------------------------------
void TWizardForm::showForm()
   {
   if (currentForm <= componentForms.size())
      {
      componentForms[currentForm]->Parent = WizardPanel;
      componentForms[currentForm]->Align = alClient;
      componentForms[currentForm]->Show();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TWizardForm::BackButtonClick(TObject *Sender)
   {
   currentForm = max(currentForm-1, 0);
   showForm();
   }
//---------------------------------------------------------------------------
void __fastcall TWizardForm::NextButtonClick(TObject *Sender)
   {
   currentForm = min(currentForm+1, componentForms.size()-1);
   showForm();
   }
//---------------------------------------------------------------------------
void __fastcall TWizardForm::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   for (unsigned componentI = 0; componentForms.size(); componentI++)
      delete componentForms[componentI];
   }
//---------------------------------------------------------------------------
