//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <general\vcl_functions.h>
#include "AddCostsBenefits.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

using namespace std;

TAddCostsBenefitsForm *AddCostsBenefitsForm;
//---------------------------------------------------------------------------
__fastcall TAddCostsBenefitsForm::TAddCostsBenefitsForm(TComponent* Owner)
   : TForm(Owner)
{
   analysisType = NO_NPV_CALCS;
   baseCase = "";
   investmentPeriod = 15;
   salvageRate = 0;
}
//---------------------------------------------------------------------------

void TAddCostsBenefitsForm::EnableControls(bool activate)
{
   BaseCaseComboBox->Enabled = activate;
   InvestmentPeriodBox->Enabled = activate;
   SalvageRateBox->Enabled = activate;
}

void __fastcall TAddCostsBenefitsForm::AnalysisTypeRadioGroupClick(
      TObject *Sender)
{
   if (AnalysisTypeRadioGroup->ItemIndex == 0)
      EnableControls(false);
   else
      EnableControls(true);
}
//---------------------------------------------------------------------------

void __fastcall TAddCostsBenefitsForm::FormShow(TObject *Sender)
{
   AnalysisTypeRadioGroup->ItemIndex = analysisType;
   Stl_2_tstrings(scenario_names, BaseCaseComboBox->Items);
   BaseCaseComboBox->ItemIndex =
                BaseCaseComboBox->Items->IndexOf(AnsiString(baseCase.c_str()));
   if (BaseCaseComboBox->ItemIndex < 0)
      BaseCaseComboBox->ItemIndex = 0;
   InvestmentPeriodBox->Text = IntToStr(investmentPeriod);
   SalvageRateBox->Text = FloatToStr(salvageRate);

}
//---------------------------------------------------------------------------

AnalysisType TAddCostsBenefitsForm::getAnalysisType()
{  return analysisType; }

string TAddCostsBenefitsForm::getBaseCase()
{  return baseCase;  }

int TAddCostsBenefitsForm::getInvestmentPeriod()
{  return investmentPeriod;   }

double TAddCostsBenefitsForm::getSalvageRate()
{  return salvageRate;  }

void __fastcall TAddCostsBenefitsForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
   if (ModalResult == mrOk)
   {
      analysisType = AnalysisTypeRadioGroup->ItemIndex;
      baseCase = BaseCaseComboBox->Items->Strings[BaseCaseComboBox->ItemIndex].c_str();
      investmentPeriod = StrToFloat(InvestmentPeriodBox->Text);
      salvageRate = StrToInt(SalvageRateBox->Text);
   }
}
//---------------------------------------------------------------------------

void __fastcall TAddCostsBenefitsForm::InvestmentPeriodBoxExit(
      TObject *Sender)
{
   InvestmentPeriodBoxCheck();
}
//---------------------------------------------------------------------------

void __fastcall TAddCostsBenefitsForm::FormCloseQuery(TObject *Sender,
      bool &CanClose)
{
   if (InvestmentPeriodBoxCheck())
      CanClose = true;
}
//---------------------------------------------------------------------------


bool TAddCostsBenefitsForm::InvestmentPeriodBoxCheck()
{
   if (StrToInt(InvestmentPeriodBox->Text) < 2)
   {
      ShowMessage("The investment period must be 2 or more years");
      InvestmentPeriodBox->SetFocus();
      return false;
   }
   else
      return true;
}
//---------------------------------------------------------------------------

