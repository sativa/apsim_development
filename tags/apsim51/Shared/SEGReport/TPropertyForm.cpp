//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPropertyForm.h"
#include <general\vcl_functions.h>
#include "TXYGraph.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "DBAdvGrd"
#pragma link "AdvPanel"
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
void __fastcall TPropertyForm::FormShow(TObject *Sender)
   {
   AdvancedPanel->Visible = showAdvanced;


   NameEdit->Text = component->Name;

   TSEGTable* segTable = dynamic_cast<TSEGTable*>(component);
   TXYGraph* xyGraph = dynamic_cast<TXYGraph*>(component);
   if (segTable != NULL)
      {
      getComponentNames<TSEGTable>(segTable->Owner, SourceCombo->Items);
      if (segTable->source != NULL)
         SourceCombo->Text = segTable->source->Name;
      }
   else if (xyGraph != NULL)
      {
      TForm* data = getComponent<TForm>(xyGraph->Owner, "data");
      getComponentNames<TSEGTable>(data, SourceCombo->Items);
      if (xyGraph->source != NULL)
         SourceCombo->Text = xyGraph->source->Name;
      }
   if (segTable != NULL)
      {
      ToolbarCheckBox->Checked = segTable->addToToolBar;
      WizardCheckBox->Checked = segTable->addToWizard;
      SortFieldsEdit->Text = segTable->sortFields;
      GroupByEdit->Text = segTable->groupByFields;
      }
   if (segTable != NULL || xyGraph != NULL)
      {
      ToolbarCheckBox->Visible = false;
      SourceLabel->Visible = false;
      SourceCombo->Visible = false;
      SortFieldsLabel->Visible = false;
      SortFieldsEdit->Visible = false;
      GroupByEdit->Visible = false;
      }

   setComponent(component);
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
   if (segTable != NULL)
      segTable->source = getComponent<TSEGTable>(segTable->Owner, SourceCombo->Text);
   else
      {
      TXYGraph* xyGraph = dynamic_cast<TXYGraph*>(component);
      if (xyGraph != NULL)
         {
         TForm* data = getComponent<TForm>(xyGraph->Owner, "data");
         xyGraph->source = getComponent<TSEGTable>(data, SourceCombo->Text);
         }
      }
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
void __fastcall TPropertyForm::GroupByEditExit(TObject *Sender)
   {
   TSEGTable* segTable = dynamic_cast<TSEGTable*>(component);
   segTable->groupByFields = GroupByEdit->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TPropertyForm::WizardCheckBoxClick(TObject *Sender)
   {
   TSEGTable* segTable = dynamic_cast<TSEGTable*>(component);
   segTable->addToWizard = WizardCheckBox->Checked;
   }

