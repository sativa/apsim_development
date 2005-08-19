#pragma link "DBAdvGrd"
//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TStatsForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TStatsForm *StatsForm;
//---------------------------------------------------------------------------
__fastcall TStatsForm::TStatsForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TStatsForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   stats = dynamic_cast<TStats*>(component);

   if (stats->source != NULL)
      {
      FieldNameCombo->Items->Assign(stats->source->FieldList);
      FieldNameCombo->Text = stats->fieldName;
      }
   StatSet allStats = stats->stats;
   if (allStats.Contains(statMean))
      MeanCheckBox->Checked = true;
   if (allStats.Contains(statCount))
      CountCheckBox->Checked = true;
   if (allStats.Contains(statMin))
      MinCheckBox->Checked = true;
   if (allStats.Contains(statMax))
      MaxCheckBox->Checked = true;
   if (allStats.Contains(statSum))
      SumCheckBox->Checked = true;
   if (allStats.Contains(stat10))
      Decile10CheckBox->Checked = true;
   if (allStats.Contains(stat20))
      Decile20CheckBox->Checked = true;
   if (allStats.Contains(stat30))
      Decile30CheckBox->Checked = true;
   if (allStats.Contains(stat40))
      Decile40CheckBox->Checked = true;
   if (allStats.Contains(stat50))
      Decile50CheckBox->Checked = true;
   if (allStats.Contains(stat60))
      Decile60CheckBox->Checked = true;
   if (allStats.Contains(stat70))
      Decile70CheckBox->Checked = true;
   if (allStats.Contains(stat80))
      Decile80CheckBox->Checked = true;
   if (allStats.Contains(stat90))
      Decile90CheckBox->Checked = true;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::FieldNameComboChange(TObject *Sender)
   {
   stats->fieldName = FieldNameCombo->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::CheckBoxClick(TObject *Sender)
   {
   StatSet allStats;
   if (MeanCheckBox->Checked)
      allStats << statMean;
   if (MinCheckBox->Checked)
      allStats << statMin;
   if (MaxCheckBox->Checked)
      allStats << statMax;
   if (CountCheckBox->Checked)
      allStats << statCount;
   if (SumCheckBox->Checked)
      allStats << statSum;
   if (Decile10CheckBox->Checked)
      allStats << stat10;
   if (Decile20CheckBox->Checked)
      allStats << stat20;
   if (Decile30CheckBox->Checked)
      allStats << stat30;
   if (Decile40CheckBox->Checked)
      allStats << stat40;
   if (Decile50CheckBox->Checked)
      allStats << stat50;
   if (Decile60CheckBox->Checked)
      allStats << stat60;
   if (Decile70CheckBox->Checked)
      allStats << stat70;
   if (Decile80CheckBox->Checked)
      allStats << stat80;
   if (Decile90CheckBox->Checked)
      allStats << stat90;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
// User has changed the source property - update field list.
//---------------------------------------------------------------------------
void TStatsForm::sourceHasChanged(TSEGTable* segTable)
   {
   if (stats->source != NULL)
      {
      FieldNameCombo->Items->Assign(stats->source->FieldList);
      FieldNameCombo->Text = stats->fieldName;
      }
   }
