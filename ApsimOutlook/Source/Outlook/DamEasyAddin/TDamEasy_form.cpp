//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TDamEasy_form.h"
#include <general\string_functions.h>
#include <general\vcl_functions.h>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

using namespace std;

TDamEasy_form *DamEasy_form;
//---------------------------------------------------------------------------
__fastcall TDamEasy_form::TDamEasy_form(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------

__fastcall TDamEasy_form::~TDamEasy_form()
{
   //  TDamEasy_form does not create any Config instances, so does not delete Config
}

void __fastcall TDamEasy_form::FormShow(TObject *Sender)
   {
   ConfigNameBox->Text = Config.getName().c_str();
   Sugar_price_box->Text = ftoa(Config.Sugar_price, 2).c_str();
   CCS_box->Text = ftoa(Config.CCS, 2).c_str();
   Crop_cash_box->Text = ftoa(Config.Cash_cost, 2).c_str();
   Overhead_box->Text = ftoa(Config.Overhead_cost, 2).c_str();
   Allocation_box->Text = ftoa(Config.Allocation_price, 2).c_str();
   OOA_price_box->Text = ftoa(Config.OOA_price, 2).c_str();
   Storage_pumping_box->Text = ftoa(Config.Storage_pumping_cost, 2).c_str();
   Irrigation_operating_box->Text = ftoa(Config.Irrigation_operating_cost, 2).c_str();
   Harvesting_and_levies_box->Text = ftoa(Config.Harvesting_and_levies, 2).c_str();
   Interest_rate_box->Text = ftoa(Config.Interest_rate, 2).c_str();
   Investment_rate_box->Text = ftoa(Config.Investment_rate, 2).c_str();
   Inflation_rate_input_box->Text = ftoa(Config.Inflation_input_rate, 2).c_str();
   Inflation_rate_cane_box->Text = ftoa(Config.Inflation_cane_rate, 2).c_str();
   Repayment_time_box->Text = ftoa(Config.Repayment_time, 0).c_str();
   Reticulation_box->Text = ftoa(Config.Reticulation_cost, 2).c_str();
   Number_partners_box->Text = ftoa(Config.Num_partners, 2).c_str();
   OFWS_pump_cost_box->Text = ftoa(Config.OFWS_pump_cost,2).c_str();
   OFWS_construction_box->Text = ftoa(Config.OFWS_construction_cost,2).c_str();

   if (Config.Upfront == true)
      OFWS_payment_method->ItemIndex = 0;
   else
      OFWS_payment_method->ItemIndex = 1;

   if (Config.Calc_tax == true)
      Tax_yes_no->Checked = true;
   else
      Tax_yes_no->Checked = false;

   Repayment_time_boxExit(this);
   StartYearCombo->ItemIndex = StartYearCombo->Items->IndexOf(IntToStr(Config.Starting_year));
   Repayment_time_boxExit(this);

   OFWS_payment_methodClick(this);
   }

//---------------------------------------------------------------------------
void __fastcall TDamEasy_form::FormClose(TObject *Sender, TCloseAction &Action)
{
   if (ModalResult == mrOk)
   {
      vector<string>::iterator find_pos =
            find(Illegal_names.begin(),Illegal_names.end(),ConfigNameBox->Text.c_str());
      if (find_pos != Illegal_names.end()) {
         ShowMessage("The configuration name you have chosen already exists. Please choose another.");
         ConfigNameBox->SetFocus();
         Action = caNone;
         return;
      }
      Config.setName(ConfigNameBox->Text.c_str());
      Config.Sugar_price = atof(Sugar_price_box->Text.c_str());
      Config.CCS = atof(CCS_box->Text.c_str());
      Config.Cash_cost = atof(Crop_cash_box->Text.c_str());
      Config.Overhead_cost = atof(Overhead_box->Text.c_str());
      Config.Allocation_price = atof(Allocation_box->Text.c_str());
      Config.OOA_price = atof(OOA_price_box->Text.c_str());
      Config.Storage_pumping_cost = atof(Storage_pumping_box->Text.c_str());
      Config.Irrigation_operating_cost = atof(Irrigation_operating_box->Text.c_str());
      Config.Harvesting_and_levies = atof(Harvesting_and_levies_box->Text.c_str());
      Config.Interest_rate = atof(Interest_rate_box->Text.c_str());
      Config.Investment_rate = atof(Investment_rate_box->Text.c_str());
      Config.Inflation_input_rate = atof(Inflation_rate_input_box->Text.c_str());
      Config.Inflation_cane_rate = atof(Inflation_rate_cane_box->Text.c_str());
      Config.Repayment_time = atof(Repayment_time_box->Text.c_str());
      Config.Reticulation_cost = atof(Reticulation_box->Text.c_str());
      Config.Num_partners = atof(Number_partners_box->Text.c_str());
      Config.OFWS_pump_cost = atof(OFWS_pump_cost_box->Text.c_str());
      Config.OFWS_construction_cost = atof(OFWS_construction_box->Text.c_str());

      if (OFWS_payment_method->ItemIndex == 0)
         Config.Upfront = true;
      else
         Config.Upfront = false;

      Config.Starting_year = atoi(StartYearCombo->Text.c_str());

      if (Tax_yes_no->Checked)
         Config.Calc_tax = true;
      else
         Config.Calc_tax = false;
   }
}
//---------------------------------------------------------------------------


void __fastcall TDamEasy_form::Repayment_time_boxExit(TObject *Sender)
{
   // need to check that the number is viable, and that the StartYearCombo only
   // allows sensible years with respect to the investment period
   int period = Repayment_time_box->Text.ToDouble();
   int max_period = end_year - begin_year + 1;

   if ( period > max_period ) {
      Repayment_time_box->Text = max_period;
      ShowMessage("The number you entered exceeds the maximum Investment Period ("
                   + IntToStr(max_period) + " years).");
      Repayment_time_box->SetFocus();
      return;
   }

   vector<string> years_vector;
   for (int yr = begin_year; yr < end_year - period + 1; yr++)  {
      years_vector.push_back(ftoa(yr,0));
   }

   int save_item_index = StartYearCombo->ItemIndex;
   StartYearCombo->Clear();
   Stl_2_tstrings(years_vector, StartYearCombo->Items);
   if (save_item_index > StartYearCombo->Items->Count)
      StartYearCombo->ItemIndex = StartYearCombo->Items->IndexOf(begin_year);
   else
      StartYearCombo->ItemIndex = save_item_index;
   if (StartYearCombo->ItemIndex == -1)
      StartYearCombo->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void TDamEasy_form::setMyConfig(const DEEconConfig& new_config)
{
   Config = new_config;
}

const DEEconConfig TDamEasy_form::getMyConfig()
{
   return Config;
}

void TDamEasy_form::setIllegalNames(const vector<string>& names)
{
   Illegal_names = names;
}

void __fastcall TDamEasy_form::OFWS_payment_methodClick(TObject *Sender)
{
   if (OFWS_payment_method->ItemIndex == 0) {
      Repayment_time_box->Enabled = false;
      Repayment_time_box->ParentColor = true;
      Interest_rate_box->Enabled = false;
      Interest_rate_box->ParentColor = true;

   }
   else {
      Repayment_time_box->Enabled = true;
      Repayment_time_box->Color = clWindow;
      Interest_rate_box->Enabled = true;
      Interest_rate_box->Color = clWindow;
   }
}
//---------------------------------------------------------------------------

void __fastcall TDamEasy_form::RestoreButtonClick(TObject *Sender)
{
   FormShow(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TDamEasy_form::Number_partners_boxExit(TObject *Sender)
{
   // number must be at least one and must be an int
   int num;
   try
   {
      num = StrToInt(Number_partners_box->Text);
   }
   catch(...)
   {
      ShowMessage("Number of Partners must be an integer value");
      Number_partners_box->Text = "1";
      Number_partners_box->SetFocus();
      return;
   }

   if (num < 1)
   {
      ShowMessage("Number of Partners must be at least 1");
      Number_partners_box->SetFocus();
      return;
   }
}
//---------------------------------------------------------------------------

