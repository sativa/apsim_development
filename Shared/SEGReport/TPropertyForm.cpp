//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPropertyForm.h"
#include <general\vcl_functions.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "DBAdvGrd"
#pragma resource "*.dfm"
TPropertyForm *PropertyForm;
//---------------------------------------------------------------------------
__fastcall TPropertyForm::TPropertyForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
__fastcall TPropertyForm::~TPropertyForm()
   {
   }
//---------------------------------------------------------------------------
void TPropertyForm::setComponent(TComponent* comp)
   {
   component = comp;
   NameEdit->Text = component->Name;

   TSEGTable* segTable = dynamic_cast<TSEGTable*>(component);
   if (segTable != NULL)
      {
      ToolbarCheckBox->Checked = segTable->addToToolBar;

      getComponentNames<TSEGTable>(segTable->Owner, SourceCombo->Items);
      if (segTable->source != NULL)
         SourceCombo->Text = segTable->source->Name;
      SortFieldsEdit->Text = segTable->sortFields;
      }
   else
      {
      ToolbarLabel->Visible = false;
      ToolbarCheckBox->Visible = false;
      SourceLabel->Visible = false;
      SourceCombo->Visible = false;
      SortFieldsLabel->Visible = false;
      SortFieldsEdit->Visible = false;

      }
   }
//---------------------------------------------------------------------------
void __fastcall TPropertyForm::NameEditExit(TObject *Sender)
   {
   component->Name = NameEdit->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TPropertyForm::SourceComboChange(TObject *Sender)
   {
   TSEGTable* segTable = dynamic_cast<TSEGTable*>(component);
   segTable->source = getComponent<TSEGTable>(segTable->Owner, SourceCombo->Text);
   sourceHasChanged(segTable);
   }
//---------------------------------------------------------------------------
void __fastcall TPropertyForm::ToolbarCheckBoxClick(TObject *Sender)
   {
   TSEGTable* segTable = dynamic_cast<TSEGTable*>(component);
   segTable->addToToolBar = ToolbarCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TPropertyForm::SortFieldsEditChange(TObject *Sender)
   {
   TSEGTable* segTable = dynamic_cast<TSEGTable*>(component);
   segTable->sortFields = SortFieldsEdit->Text;
   }
//---------------------------------------------------------------------------

