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
#pragma link "TSEGTableForm"
#pragma resource "*.dfm"
TStatsForm *StatsForm;
//---------------------------------------------------------------------------
__fastcall TStatsForm::TStatsForm(TComponent* Owner)
   : TSEGTableForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TStatsForm::setComponent(TStats* s)
   {
   stats = s;
   TSEGTableForm::setComponent(stats);
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::PropertiesSheetShow(TObject *Sender)
   {
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
void __fastcall TStatsForm::MeanCheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << statMean;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::MinCheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << statMin;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::MaxCheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << statMax;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::CountCheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << statCount;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile10CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat10;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile20CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat20;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile30CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat30;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile40CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat40;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile50CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat50;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile60CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat60;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile70CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat70;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile80CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat80;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------
void __fastcall TStatsForm::Decile90CheckBoxClick(TObject *Sender)
   {
   StatSet allStats = stats->stats;
   allStats << stat90;
   stats->stats = allStats;
   }
//---------------------------------------------------------------------------

