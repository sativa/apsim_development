//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TClimateForecastForm.h"
#include "TWebSession.h"
#include "Data.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompEdit"
#pragma link "IWCompLabel"
#pragma link "IWCompRectangle"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma link "IWCompListbox"
#pragma link "IWOutlookBar"
#pragma link "IWCompMemo"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
__fastcall TClimateForecastForm::TClimateForecastForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {

   }
//---------------------------------------------------------------------------
// Setup the form
//---------------------------------------------------------------------------
void TClimateForecastForm::setup(TWebSession* sess, Data* d)
   {
   session = sess;
   data = d;
   try
      {
      unsigned soiPhase, soiMonth;
      unsigned davidSYear1, davidSYear2, davidSYear3, davidSYear4, davidSYear5;
      string soiDescription, davidSDescription;
      data->getClimateForecast(soiPhase, soiMonth,
                               davidSYear1, davidSYear2, davidSYear3,
                               davidSYear4, davidSYear5,
                               soiDescription, davidSDescription);
      MonthCombo->ItemIndex = soiMonth;
      PhaseCombo->ItemIndex = soiPhase;
      Year1Edit->Text = IntToStr(davidSYear1);
      Year2Edit->Text = IntToStr(davidSYear2);
      Year3Edit->Text = IntToStr(davidSYear3);
      Year4Edit->Text = IntToStr(davidSYear4);
      Year5Edit->Text = IntToStr(davidSYear5);
      SOIDescriptionMemo->Lines->Text = soiDescription.c_str();
      DavidSDescriptionMemo->Lines->Text = davidSDescription.c_str();
      }
   catch (const exception& err)
      {
      }
   }
//---------------------------------------------------------------------------
// User has clicked save
//---------------------------------------------------------------------------
void __fastcall TClimateForecastForm::SaveButtonClick(TObject *Sender)
   {
   try
      {
      unsigned soiPhase, soiMonth;
      unsigned davidSYear1, davidSYear2, davidSYear3, davidSYear4, davidSYear5;
      soiMonth = MonthCombo->ItemIndex;
      soiPhase = PhaseCombo->ItemIndex;
      davidSYear1 = StrToInt(Year1Edit->Text);
      davidSYear2 = StrToInt(Year2Edit->Text);
      davidSYear3 = StrToInt(Year3Edit->Text);
      davidSYear4 = StrToInt(Year4Edit->Text);
      davidSYear5 = StrToInt(Year5Edit->Text);
      data->setClimateForecast(soiPhase, soiMonth,
                               davidSYear1, davidSYear2, davidSYear3,
                               davidSYear4, davidSYear5,
                               SOIDescriptionMemo->Lines->Text.c_str(),
                               DavidSDescriptionMemo->Lines->Text.c_str());
      }
   catch (const exception& err)
      {
      session->showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------

