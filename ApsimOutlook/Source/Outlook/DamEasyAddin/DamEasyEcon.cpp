//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "DamEasyEcon.h"
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\math_functions.h>
#include <general\path.h>
#include <general\ini_file.h>
#include <functional>

#define DE_ECON_FACTOR_NAME "Econ Config"
#define DAMEASY_SECTION     "DamEa$y Economics"
#define ECON_CONFIGS_KEY    "econconfigs"
#define BITMAP_NAME_KEY     "bitmap"
#define SIMULATION_FACTOR_NAME "Simulation"

#pragma package(smart_init)
using namespace std;

// static member variable declarations:
int DamEasyEcon::numObjects;

// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DAH 20/4/01

// ------------------------------------------------------------------
extern "C" AddInBase* _export __stdcall createAddIn(const string& parameters)
   {
   // will be called with begin
   // and end years from the database
   return new DamEasyEcon(parameters);
   }


// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 15/12/99
//    DAH 31/10/00   added the concept of upfront costs as an
//                   option (vs. loan option)
//    DAH 29/04/01   changed to Addin framework (vs. APSTable derived)

// ------------------------------------------------------------------
DamEasyEcon::DamEasyEcon(const string& parameters)
   {
   if (numObjects == 0)
   {
      DEValueSelectionForm = new TDEValueSelectionForm(Application->MainForm);
      DamEasy_form = new TDamEasy_form(Application->MainForm);
   //   AddCostsBenefitsForm = new TAddCostsBenefitsForm(Application->MainForm);
   }

   // handle parameter string
   vector<string> begin_and_end_years;
   Split_string(parameters, " ", begin_and_end_years);
   Begin_year = atoi(begin_and_end_years[0].c_str());
   End_year = atoi(begin_and_end_years[1].c_str());
   DamEasy_form->end_year = End_year;
   DamEasy_form->begin_year = Begin_year;

   Read_inifile_settings();

   Path bitmap_path(Application->ExeName.c_str());
   bitmap_path.Set_name (Econ_bitmap_name.c_str());
   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   bitmap->LoadFromFile(bitmap_path.Get_path().c_str());

   string default_config_name = Econ_configs[0].getName();
   Factor econ(bitmap,DE_ECON_FACTOR_NAME,default_config_name.c_str(),this);
   factors.push_back(econ);
   numObjects++;
   }

DamEasyEcon::~DamEasyEcon(void) {
//   delete AddCostsBenefitsForm;
   if (numObjects == 1)
   {
      delete DamEasy_form;
      delete DEValueSelectionForm;
   }
   numObjects--; 
}


void DamEasyEcon::makeScenarioValid(Scenario& scenario) const {}

Scenario DamEasyEcon::getDefaultScenario(void) const {
   // check the following to see if "" is the right thing to pass in.
   return Scenario("",factors);
}


void DamEasyEcon::getFactorValues(const std::string& factorName,
                                   std::vector<std::string>& factorValues) const
{
   if ( DEValueSelectionForm != NULL)
   {
      for_each(Econ_configs.begin(), Econ_configs.end(),
            GetNameFunction< vector<string>, DEEconConfig>(factorValues));
   }
}


TValueSelectionForm*  DamEasyEcon::getUIForm(const string& factorName,
                                                             TComponent* Owner) const{
   if ( DEValueSelectionForm == NULL)
      DEValueSelectionForm = new TDEValueSelectionForm(Owner);

   DEValueSelectionForm->SetEconConfigs(Econ_configs);
   DEValueSelectionForm->OkButton->OnClick = updateConfigs;
   return  DEValueSelectionForm;
}


void __fastcall DamEasyEcon::updateConfigs(TObject *Sender)
{
   Econ_configs = DEValueSelectionForm->GetEconConfigs();

}


void DamEasyEcon::saveConfigs()
{
   // update the ini file
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file ini;
   ini.Set_file_name (p.Get_path().c_str());
   vector<string> config_names;
   getFactorValues("", config_names);
   string config_names_comma_list;
   Build_string(config_names, ",", config_names_comma_list);
   ini.Write(DAMEASY_SECTION, ECON_CONFIGS_KEY, config_names_comma_list.c_str());

   // remove old Econconfig sections, and put in only the ones remaining in Econ_configs
   list<string> Section_names;
   ini.Read_section_names(Section_names);
   // find those section names that are EconConfig entries
   list<string>::iterator sect_name = Section_names.begin();
   while (sect_name != Section_names.end()) {
      string prefix = SECTION_PREFIX;
      string::iterator where =
           search((*sect_name).begin(),(*sect_name).end(),prefix.begin(),prefix.end());
      if (where == (*sect_name).begin())  // then this section needs deleting
         ini.Delete_section((*sect_name).c_str());
      sect_name++;
   }

   // now write to file the current list of EconConfigs
   vector<DEEconConfig>::iterator config;
   for (config = Econ_configs.begin(); config!= Econ_configs.end(); config++)
      (*config).writeToFile();
}


// ------------------------------------------------------------------
//  Short description:
//      read all defaults from .ini file.

//  Notes:

//  Changes:
//    DPH 15/12/99
//    DAH 31/10/00:   Getting economics up to new specs.
//    DAH 20/4/01 :   econ as addin
//    DAH 30/4/01 :   Changed to read in multiple configurations (of type DEEconConfig)

// ------------------------------------------------------------------
void DamEasyEcon::Read_inifile_settings (void)
{
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file ini;
   ini.Set_file_name (p.Get_path().c_str());

   // read all defaults.
   string st;
   ini.Read (DAMEASY_SECTION, BITMAP_NAME_KEY, st);
   Econ_bitmap_name = st;
   ini.Read (DAMEASY_SECTION, ECON_CONFIGS_KEY, st);
   vector<string> words;
   Split_string(st, ",", words);

   DEEconConfig::setIOFile(p.Get_path());

   // loop through econ configs present in ini file
   for (vector<string>::iterator e = words.begin(); e!=words.end(); e++)
   {
      DEEconConfig config;
      if (config.readFromFile(*e))
         Econ_configs.push_back(config);
   }

   // read tax table
   ini.Read (DAMEASY_SECTION, "Tax_brackets", st);
   Split_string(st, ",", words);
   String2double< vector<string>, vector<double> >(words, Tax_brackets);
   ini.Read (DAMEASY_SECTION, "Tax_rates", st);
   Split_string(st, ",", words);
   String2double< vector<string>, vector<double> > (words, Tax_rates);

}

// ------------------------------------------------------------------
//  Short description:
//      locate and return a particular part of a descriptor.

//  Notes:

//  Changes:
//    DPH 15/12/99

// ------------------------------------------------------------------
string DamEasyEcon::Get_descriptor_value(string Descriptor, string Item)
   {
   To_lower(Descriptor);
   To_lower(Item);
   unsigned int Pos = Descriptor.find(Item);
   if (Pos == string::npos)
      {
      string msg = "Cannot find " + Item + " in descriptor inside of DamEa$y economics";
      ShowMessage(msg.c_str());
      return "";
      }
   Pos = Descriptor.find("=", Pos);
   if (Pos == string::npos)
      {
      string msg = "Cannot find equals sign after " + Item + " in descriptor inside of DamEa$y economics";
      ShowMessage(msg.c_str());
      return "";
      }
   unsigned int EndPos = Descriptor.find(";", Pos);
   if (EndPos == string::npos)
      EndPos = Descriptor.length();
   Pos++;
   return Descriptor.substr(Pos, EndPos-Pos);
   }



// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 15/12/99
//    DAH 31/10/00:   Getting economics up to new specs.
//    DAH 8/5/01:     Modified for Addin, multiple econ configs. Removed
//                    Base case dependent stuff.
// ------------------------------------------------------------------
void DamEasyEcon::doCalculations(TAPSTable& data,
                                  const std::vector<Scenario*>& selectedScenarios)
{
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   saveConfigs();

//   data.addField(DE_ECON_FACTOR_NAME);
   data.markFieldAsAPivot(DE_ECON_FACTOR_NAME);

   vector<string> econ_config_names;
   // for each record in data, find the corresponding Scenario to find which
   // econConfig is being used
   bool ok = data.first();
   while (ok)
   {
      for (vector<TAPSRecord>::const_iterator record = data.begin();
                     record != data.end(); record++)
      {
         string rec_name = record->getFieldValue(SIMULATION_FACTOR_NAME);
         vector<Scenario*>::const_iterator find_pos =
               find_if(selectedScenarios.begin(),selectedScenarios.end(),
                       PEqualToName<Scenario>(rec_name));
         Graphics::TBitmap* temp;
         string econ_config_name;
         (*find_pos)->getFactorAttributes(DE_ECON_FACTOR_NAME, econ_config_name, temp);
         econ_config_names.push_back(econ_config_name);
      }

      ok = data.next();
   }

   // add the config names to the data table.
   data.first();
   data.storeStringArray(DE_ECON_FACTOR_NAME, econ_config_names);
   data.endStoringData();   // this sorts the data so we can sensibly access it
                            // for the next section of code.


   // result vectors:
   vector<double> vNet_Cash_Flow, vNet_Cash_Flow_BIT, vCumulative_Cash_Flow,
                  vCumulative_Cash_Flow_BIT, vCane_Income, vCosts, vInterest, vTax;

   // process data block by block:
   ok = data.first();
   while (ok)
   {
      // do calculations/operations that are common to the whole block:
      vector<TAPSRecord>::const_iterator record = data.begin();
      string config_name = record->getFieldValue(DE_ECON_FACTOR_NAME);
      vector<DEEconConfig>::const_iterator find_pos =
         find_if(Econ_configs.begin(),Econ_configs.end(),
                          EqualToName<DEEconConfig>(config_name));
      if (find_pos == Econ_configs.end()) return; // should throw an error here

      const DEEconConfig* config = find_pos;

      double Irrig_area =
               StrToFloat(record->getFieldValue("Irrig Area (ha)").c_str());

      double Upfront_installation_costs = config->OFWS_construction_cost +
                           config->OFWS_pump_cost + config->Reticulation_cost;

      double Debt_repayment;
      if (config->Interest_rate == 0)
         Debt_repayment = Upfront_installation_costs / config->Repayment_time;
      else
         Debt_repayment =
               Upfront_installation_costs /
               ( 1/(config->Interest_rate/100) - 1/(config->Interest_rate/100)
               * pow(1 + (config->Interest_rate/100), - config->Repayment_time) );

      double Cum_net_cash_flow = 0.0;
      double Cum_net_cash_flow_BIT = 0.0;

      int first_yr = StrToInt( record->getFieldValue("Year").c_str() );


      int start_yr = config->Starting_year - first_yr;
      // finished doing calculations/operations that are common to the whole block



      // loop through the rows before the repayment starts, giving zero
      // for all econ variables.
      for (int time = 0; time < start_yr; time++) {
         // appending values to result vectors:
         vNet_Cash_Flow.push_back(0);
         vNet_Cash_Flow_BIT.push_back(0);
         vCumulative_Cash_Flow.push_back(0);
         vCumulative_Cash_Flow_BIT.push_back(0);
         vCane_Income.push_back(0);
         vCosts.push_back(0);
         vInterest.push_back(0);
         vTax.push_back(0);
      }

      // do the calculations that need doing for every TAPSRecord in the block
      int time = 0;
      for (vector<TAPSRecord>::const_iterator record = data.begin() + start_yr;
                     record != data.end(); record++)
      {
      // reading database variables:
         double Cane_fresh_weight = StrToFloat(record->getFieldValue("Cane fresh wt (t per ha)").c_str());
         double Irrigation_from_allocation = StrToFloat(record->getFieldValue("Irrig from allocation (ML)").c_str());
         double Allocation_into_OFWS = StrToFloat(record->getFieldValue("Allocation into OFWS (ML)").c_str());
         double Irrigation_from_OOA = StrToFloat(record->getFieldValue("Irrig from OOA (ML)").c_str());
         double OOA_into_OFWS = StrToFloat(record->getFieldValue("OOA into OFWS (ML)").c_str());
         double Applied_irrigation  = StrToFloat(record->getFieldValue("Applied irrigation (ML)").c_str());

      // Cane Income section:
/*         double Salvage = Upfront_installation_costs * config->Salvage_rate/100;
         // only count salvage in the last year of invest.
         if (time == config->Repayment_time - 1)
            Salvage = Salvage;
         else
            Salvage = 0;
*/
         double Cane_income = ((Cane_fresh_weight * ((config->Sugar_price*0.009*(config->CCS-4)+0.6455))
                              * Irrig_area) /* + Salvage */)
                              * pow((1.0 + config->Inflation_cane_rate / 100.0), time);


      // Costs section:
         double Costs = 0.0;
         double Yearly_cost = (Irrigation_from_allocation + Allocation_into_OFWS) * config->Allocation_price
                              + (Irrigation_from_OOA + OOA_into_OFWS) * config->OOA_price
                              + (Allocation_into_OFWS + OOA_into_OFWS) * config->Storage_pumping_cost
                              + Applied_irrigation * config->Irrigation_operating_cost
                              + config->Cash_cost * Irrig_area + config->Overhead_cost
                              + config->Harvesting_and_levies * Irrig_area;

         if (config->Upfront) {  // upfront section
            double Upfront_costs;
            // only count upfront costs in first year
            if (time != 0)    Upfront_costs = 0.0;
            else            Upfront_costs = Upfront_installation_costs;

            Costs    = ( Upfront_costs + Yearly_cost )
                              * pow(1 + config->Inflation_input_rate/100, time);
         }
         else {                 // debt repayment option selected
            double Repayment;
            // only count debt repay from time 0 to Repayment_time
            if (time < config->Repayment_time)
                      Repayment = Debt_repayment;
            else      Repayment = 0;

            Costs    = ( Repayment + Yearly_cost )
                              * pow(1 + config->Inflation_input_rate/100, time);
         }


      // Interest Section:
         double Interest = Cum_net_cash_flow * config->Investment_rate / 100;

      // Tax Section:
         double Tax = 0.0;
         double Deduction = 0.0;
         if (time < 3) {   // ie. if we are in the first 3 years
            Deduction = Upfront_installation_costs / 3;
         }

         for (int p = 0; p < config->Num_partners; p++)
         {
            double Partners_income =
                (Cane_income + Interest - Costs - Deduction)/(config->Num_partners);
            double Marginal_tax_rate;
            vector<double>::iterator rate_pos =
                      find_if(Tax_brackets.begin(), Tax_brackets.end(),
                              bind1st(less_equal<double>(),Partners_income));
            // if rate_pos == Tax_brackets.end(), everything is fine since Tax_rates
            // should have one extra element ->  the last tax rate repeated.
            Marginal_tax_rate = Tax_rates[rate_pos - Tax_brackets.begin()];

            Tax += Partners_income * Marginal_tax_rate/100;
         }

         // if tax -ve, remove altogether:
         if (Tax < 0) Tax = 0;

         if (config->Calc_tax == false)
            Tax = 0;

      // Calculating summary variables:
         double Net_cash_flow = Cane_income - Costs + Interest - Tax;
         double Net_cash_flow_BIT = Cane_income - Costs;

         Cum_net_cash_flow += Net_cash_flow;
         Cum_net_cash_flow_BIT += Net_cash_flow_BIT;

      // Writing data to table:
         vNet_Cash_Flow.push_back(Net_cash_flow);
         vNet_Cash_Flow_BIT.push_back(Net_cash_flow_BIT);
         vCumulative_Cash_Flow.push_back(Cum_net_cash_flow);
         vCumulative_Cash_Flow_BIT.push_back(Cum_net_cash_flow_BIT);
         vCane_Income.push_back(Cane_income);
         vCosts.push_back(Costs);
         vInterest.push_back(Interest);
         vTax.push_back(Tax);

         time++;
      }

      ok = data.next();
   }
   // Append the result vectors to 'data':
   data.first();
   data.storeNumericArray("Net Cash Flow ($)", vNet_Cash_Flow);
   data.storeNumericArray("Net Cash Flow BIT($)", vNet_Cash_Flow_BIT);
   data.storeNumericArray("Cumulative Cash Flow ($)", vCumulative_Cash_Flow);
   data.storeNumericArray("Cumulative Cash Flow BIT($)", vCumulative_Cash_Flow_BIT);
   data.storeNumericArray("Cane Income ($)", vCane_Income);
   data.storeNumericArray("Costs ($)", vCosts);
   data.storeNumericArray("Interest ($)", vInterest);
   data.storeNumericArray("Tax ($)", vTax);


   Screen->Cursor = savedCursor;
}


// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 15/12/99
//    DAH 31/10/00:   Getting economics up to new specs.

// ------------------------------------------------------------------
/*void DamEasyEcon::Calc_and_store_records()
   {
   // tell base object to get ready to store data.
   Begin_storing_data();

   // loop through all datasets.
   vector<TAPSRecord> Data;

   bool ok = Source_dataset->First(Pivot_values->Items);
   while (ok) {
      Source_dataset->Get(Data);

      unsigned int Num_records = Data.size();
      vector<TAPSRecord> All_years;

      // Get values from descriptor.
      double Irrig_area;
      double OFS_capacity;
      if (Num_records > 0)
         {
         string Descriptor = Data[0].Get_title();
         Irrig_area = StrToFloat(Get_descriptor_value(Descriptor, "Irrig Area (ha)").c_str());
         OFS_capacity = StrToFloat(Get_descriptor_value(Descriptor, "OFS Capacity (ML)").c_str());
         }

      // interpolate to get OFS construction cost from OFS capacity.
      bool Did_interpolate;
      double Upfront_installation_costs =
                              linear_interp_real (OFS_capacity, OFWS_construction_capacity,
                              OFWS_construction_cost, Did_interpolate) * OFS_capacity;
      if (Upfront_installation_costs > 0)
         Upfront_installation_costs += OFWS_pump_cost;

      double Debt_repayment;
      if (Interest_rate == 0)
         Debt_repayment = Upfront_installation_costs / Investment_period;
      else
         Debt_repayment = Upfront_installation_costs
                              / ( 1/(Interest_rate/100) - 1/(Interest_rate/100)
                              * pow(1 + (Interest_rate/100), -Investment_period) );

      double Cum_net_cash_flow = 0.0;

      // work out what the first year is:
//      Source_dataset->First(NULL);
      TAPSRecord first_rec;
      Source_dataset->Get_first(first_rec);
      int first_yr = atoi( first_rec.Get_field_value("Year").c_str() );

      int start_row = Start_year - first_yr;

      // loop through the rows before the investment period starts:
      for (int row = 0; row < start_row; row++) {
         // Writing data to table:
         Data[row].Set_field_value("Net Cash Flow ($)", FloatToStr(0).c_str());
         Data[row].Set_field_value("Cumulative Cash Flow ($)", FloatToStr(0).c_str());
         Data[row].Set_field_value("Cane Income ($)", FloatToStr(0).c_str());
         Data[row].Set_field_value("Costs ($)", FloatToStr(0).c_str());
         Data[row].Set_field_value("Interest ($)", FloatToStr(0).c_str());
         Data[row].Set_field_value("Tax ($)", FloatToStr(0).c_str());

         All_years.push_back (Data[row]);
      }

      // loop through all rows including the investment period and following:
      for (int row = start_row; row < Num_records; row++) {

         // reading database variables:
         double Cane_fresh_weight = StrToFloat(Data[row].Get_field_value("Cane fresh wt (t per ha)").c_str());
         double Irrigation_from_allocation = StrToFloat(Data[row].Get_field_value("Irrig from allocation (ML)").c_str());
         double Allocation_into_OFS = StrToFloat(Data[row].Get_field_value("Allocation into OFS (ML)").c_str());
         double Irrigation_from_OOA = StrToFloat(Data[row].Get_field_value("Irrig from OOA (ML)").c_str());
         double OOA_into_OFS = StrToFloat(Data[row].Get_field_value("OOA into OFS (ML)").c_str());
         double Applied_irrigation  = StrToFloat(Data[row].Get_field_value("Applied Irrigation (ML)").c_str());

         // Cane Income section:
         double Salvage;
         if (row == start_row + Investment_period-1)    // only count salvage in the last year of invest.
            Salvage = Salvage;
         else
            Salvage = 0;

         double Cane_income = ((Cane_fresh_weight * ((Sugar_price*0.009*(CCS-4)+0.608)
                              - Harvesting_and_levies) * Irrig_area) + Salvage )
                              * pow((1.0 + Inflation_cane_rate / 100.0), row - start_row);


         // Costs section:
         double Costs = 0.0;
         double Yearly_cost = (Irrigation_from_allocation + Allocation_into_OFS) * Allocation_price
                              + (Irrigation_from_OOA + OOA_into_OFS) * OOA_price
                              + (Allocation_into_OFS + OOA_into_OFS) * Storage_pumping_cost
                              + Applied_irrigation * Irrigation_operating_cost
                              + Crop_cash_cost * Irrig_area + Overhead_cost;

         if (Upfront) {  // upfront section
            double Upfront_costs;
            // only count upfront costs in first year
            if (row > start_row)    Upfront_costs = 0.0;
            else            Upfront_costs = Upfront_installation_costs;

            Costs    = ( Upfront_costs + Yearly_cost )
                              * pow(1 + Inflation_input_rate/100, row - start_row);
         }
         else {           // debt repayment option selected
            double Repayment;
            // only count debt repay from year 2 to year (Investment_Period+1)
            if (row > start_row + Investment_period || row == start_row)
                      Repayment = 0;
            else      Repayment = Debt_repayment;

            Costs    = ( Repayment + Yearly_cost )
                              * pow(1 + Inflation_input_rate/100, row - start_row);
         }

         // Interest Section:
         double Interest = Cum_net_cash_flow * Interest_rate / 100;

         // Tax Section:
         double Tax = 0.0;
         for (unsigned int i = 0; i < Number_of_partners; i++){  // sum tax over all partners

            double Extra_deduction = 0.0;
            if (row <= start_row + 2) {   // ie. if we are in the first 3 years
               Extra_deduction = Upfront_installation_costs / 3;
            }

            Tax += ((Cane_income + Interest - Costs - Annual_deductions - Extra_deduction)
                   / Number_of_partners )  *  Marginal_tax_rate/100;
         }

         double Net_cash_flow = Cane_income - Costs + Interest - Tax;

         Cum_net_cash_flow += Net_cash_flow;

         // Writing data to table:
         Data[row].Set_field_value("Net Cash Flow ($)", FloatToStr(Net_cash_flow).c_str());
         Data[row].Set_field_value("Cumulative Cash Flow ($)", FloatToStr(Cum_net_cash_flow).c_str());
         Data[row].Set_field_value("Cane Income ($)", FloatToStr(Cane_income).c_str());
         Data[row].Set_field_value("Costs ($)", FloatToStr(Costs).c_str());
         Data[row].Set_field_value("Interest ($)", FloatToStr(Interest).c_str());
         Data[row].Set_field_value("Tax ($)", FloatToStr(Tax).c_str());

         All_years.push_back (Data[row]);
      }

      // store all years records.
      Store_data (All_years);

      // goto next dataset.
      Data.erase(Data.begin(), Data.end());
      ok = Source_dataset->Next();
   }

   // work out which of the sims is the base case:
   ok = this->First(Pivot_values->Items);
   bool found = false;
   while (ok && !found) {
      Data.erase(Data.begin(), Data.end());
      this->Get(Data);
      found = Data.begin()->Get_pivot_value() == Base_case.c_str();
      ok = this->Next();
   }
   vector<TAPSRecord> Base;
   if (found) {
      Base = Data;
   } else {
      this->First(Pivot_values->Items);
      this->Get(Data);
      Base = Data;
      Base_case = AnsiString( Data.begin()->Get_pivot_value().c_str() );
   }
   Data.erase(Data.begin(), Data.end());

   vector<TAPSRecord> All_sims;


   // calculate 'additional' values
   ok = this->First(Pivot_values->Items);
   while (ok) {
      this->Get(Data);
      unsigned int Num_records = Data.size();
      vector<TAPSRecord> All_years;

      double Cum_add_cash_flow = 0.0;
      // loop through all rows in data array and make calculations
      for (unsigned int row = 0; row < Num_records; row++) {

         // reading database variables:
         double Net_cash_flow = StrToFloat(Data[row].Get_field_value("Net Cash Flow ($)").c_str());
         double Base_case_cash_flow = StrToFloat(Base[row].Get_field_value("Net Cash Flow ($)").c_str());

         double Add_net_cash_flow = Net_cash_flow - Base_case_cash_flow;
         Cum_add_cash_flow += Add_net_cash_flow;

         // Writing data to table:
         Data[row].Set_field_value("Additional Net Cash Flow ($)", FloatToStr(Add_net_cash_flow).c_str());
         Data[row].Set_field_value("Cum Additional Cash Flow ($)", FloatToStr(Cum_add_cash_flow).c_str());

         //All_years.push_back (Data[row]);
         All_sims.push_back (Data[row]);
      }

      // store all years records.
      // Store_data (All_years);


      // goto next dataset.
      Data.erase(Data.begin(), Data.end());
      ok = this->Next();

   }

   Begin_storing_data();

   Store_data(All_sims);

   // tell base object were finished storing data.
   End_storing_data();
   }
*/

// ------------------------------------------------------------------
//  Short description:
//      allow user to edit the component.

//  Notes:

//  Changes:
//    DPH 15/12/99
//    DAH 2/5/01  Disabled

// ------------------------------------------------------------------
/*bool DamEasyEcon::Edit(void)
   {
   TDamEasy_form* Form = new TDamEasy_form(Application->MainForm);
   Form->DamEasy_ptr = this;
   if (Form->ShowModal() == mrOk)
      return true;
   else
      return false;
   }
*/
