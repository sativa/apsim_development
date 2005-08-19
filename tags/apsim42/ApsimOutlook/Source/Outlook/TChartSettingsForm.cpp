//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TChartSettingsForm.h"
#include "ChildWin.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TChartSettingsForm *ChartSettingsForm;
//---------------------------------------------------------------------------
__fastcall TChartSettingsForm::TChartSettingsForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TChartSettingsForm::OnMinimize(TMessage &Msg)
   {
   TForm::Dispatch(&Msg);
   unsigned int uCmdType = Msg.WParam & 0xFFF0;
   if (uCmdType == SC_MINIMIZE)
      Show(); // without this the chart settings form disappears behind analysis_panel D330
   }

