//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DEToolBar.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\path.h>
#include <general\ini_file.h>
#include <general\math_functions.h>
#include "AddCostsBenefits.h"
#include <Math.hpp>

//---------------------------------------------------------------------------

#pragma package(smart_init)

#define DAMEASY_SECTION     "DamEa$y Economics"
#define DE_ECON_FACTOR_NAME "Econ Config"
#define ECON_CONFIGS_KEY    "econconfigs"

using namespace std;

// static member variable declarations:
int DEToolBar::numObjects;
TToolButton* DEToolBar::Costs_benefits_button;
Graphics::TBitmap* DEToolBar::glyph;
int DEToolBar::glyph_position;
TToolBar* DEToolBar::Toolbar;


// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DAH 20/4/01

// ------------------------------------------------------------------
extern "C" ToolBarAddInBase* _export __stdcall createToolBarAddIn(const string& parameters)
{
   return new DEToolBar(parameters);
}

DEToolBar::DEToolBar(const string& parameters)
{
   if (numObjects == 0)
      AddCostsBenefitsForm = new TAddCostsBenefitsForm(Application->MainForm);

   vector<string> begin_and_end_years;
   Split_string(parameters, " ", begin_and_end_years);
   Begin_year = atoi(begin_and_end_years[0].c_str());
   End_year = atoi(begin_and_end_years[1].c_str());

   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file ini;
   ini.Set_file_name (p.Get_path().c_str());

   string bitmap_name;
   ini.Read (DAMEASY_SECTION, "toolbitmap", bitmap_name);

   // get stuff from ini file like image name
   if (numObjects == 0)
   {
      Path bitmap_path(Application->ExeName.c_str());
      bitmap_path.Set_name (bitmap_name.c_str());
      glyph = new Graphics::TBitmap;
      glyph->LoadFromFile(bitmap_path.Get_path().c_str());
   }
   NPV_flag = NO_NPV_CALCS;
   needs_update = true;

   // read tax table
   string st;
   vector<string> words;
   ini.Read (DAMEASY_SECTION, "Tax_brackets", st);
   Split_string(st, ",", words);
   String2double< vector<string>, vector<double> >(words, Tax_brackets);
   ini.Read (DAMEASY_SECTION, "Tax_rates", st);
   Split_string(st, ",", words);
   String2double< vector<string>, vector<double> > (words, Tax_rates);

   numObjects++;
}

DEToolBar::~DEToolBar()
{
//   delete glyph;
//   delete Costs_benefits_button;
   delete AddCostsBenefitsForm;
   Toolbar->RemoveControl(Costs_benefits_button);
   delete Costs_benefits_button;
   Toolbar->Images->Delete(glyph_position);
   delete glyph;
   
   numObjects--;
}

void DEToolBar::decorateToolBar(TToolBar* toolbar)
{
   Toolbar = toolbar;
   Costs_benefits_button = new TToolButton(toolbar);
   Costs_benefits_button->Left = toolbar->Width; // ensures button goes at right end of row
   Costs_benefits_button->Parent = toolbar;
   int pos = toolbar->Images->Add(glyph, NULL);
   Costs_benefits_button->ImageIndex = pos;
   Costs_benefits_button->Hint = "Additional Costs/Benefits Analysis";
   Costs_benefits_button->OnClick = buttonClick;
   glyph_position = pos;
}

void __fastcall DEToolBar::buttonClick(TObject* Sender)
{
   vector<string> names;
   scenarios->getScenarioNames(names);
   AddCostsBenefitsForm->setScenarios(names);
   if (AddCostsBenefitsForm->ShowModal() == mrOk)
   {
      NPV_flag = AddCostsBenefitsForm->getAnalysisType();
      Salvage_rate = AddCostsBenefitsForm->getSalvageRate();
      Base_case = AddCostsBenefitsForm->getBaseCase();
      Investment_period = AddCostsBenefitsForm->getInvestmentPeriod();
      needs_update = true;
   }
   else
   {
      needs_update = false;
   }
}



bool DEToolBar::needsUpdate()
{
   return needs_update;
}


void DEToolBar::youNeedUpdating()
{
   needs_update = true;
}


void DEToolBar::getConfigs()
{
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file ini;
   ini.Set_file_name (p.Get_path().c_str());

   string st;
   ini.Read (DAMEASY_SECTION, ECON_CONFIGS_KEY, st);
   vector<string> words;
   Split_string(st, ",", words);

   Econ_configs.clear();
   // loop through econ configs present in ini file
   for (vector<string>::iterator e = words.begin(); e!=words.end(); e++)
   {
      DEEconConfig config;
      if (config.readFromFile(*e))
         Econ_configs.push_back(config);
   }
}


void DEToolBar::doCalculations(TAPSTable& data)
{
   //if base case is not found, invalidate the NPV calcs and reset the Base_Case
   bool found = false;
   bool ok = data.first();
   while (!found && ok)
   {
      string name = data.getDataBlockName();
      found = (Base_case == name);
      if (!found)
         ok = data.next();
   }
   if (!found)
   {
      return;
   }

   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   // do a switch on NPV_flag:
   switch (NPV_flag)
   {
      case NO_NPV_CALCS:
         // do nothing
         break;
      case ADDITIONAL_CALCS:
      {
         // calculate the additional costs and benefits, adding them to the current
         // working table
         vector<double> Base_cash_flow, Base_cash_flow_BIT, Base_cum_cash_flow,
                        Base_cum_cash_flow_BIT;

         // loop through blocks and find base case:
         bool found = false;
         while (!found)
         {
            string name = data.getDataBlockName();
            found = (Base_case == name);

            if (!found)
               data.next();
         }

         // get the base case values for required fields
         data.fieldAsNumericArray("Net Cash Flow ($)", Base_cash_flow);
         data.fieldAsNumericArray("Net Cash Flow BIT($)", Base_cash_flow_BIT);
         data.fieldAsNumericArray("Cumulative Cash Flow ($)", Base_cum_cash_flow);
         data.fieldAsNumericArray("Cumulative Cash Flow BIT($)", Base_cum_cash_flow_BIT);


         vector<double> Add_cash_flow, Add_cash_flow_BIT, Add_cum_cash_flow,
                        Add_cum_cash_flow_BIT;

         vector<double> cash_flow, cash_flow_BIT, cum_cash_flow,
                        cum_cash_flow_BIT;

         // loop through data calculating additional values and storing them.
         bool ok = data.first();
         while (ok)
         {
            data.fieldAsNumericArray("Net Cash Flow ($)", cash_flow);
            data.fieldAsNumericArray("Net Cash Flow BIT($)", cash_flow_BIT);
            data.fieldAsNumericArray("Cumulative Cash Flow ($)", cum_cash_flow);
            data.fieldAsNumericArray("Cumulative Cash Flow BIT($)", cum_cash_flow_BIT);

            Add_cash_flow = subtract(cash_flow, Base_cash_flow);
            Add_cash_flow_BIT = subtract(cash_flow_BIT, Base_cash_flow_BIT);
            Add_cum_cash_flow = subtract(cum_cash_flow, Base_cum_cash_flow);
            Add_cum_cash_flow_BIT = subtract(cum_cash_flow_BIT, Base_cum_cash_flow_BIT);

            data.storeNumericArray("Add Net Cash Flow ($)", Add_cash_flow);
            data.storeNumericArray("Add Net Cash Flow BIT($)", Add_cash_flow_BIT);
            data.storeNumericArray("Add Cum Cash Flow ($)", Add_cum_cash_flow);
            data.storeNumericArray("Add Cum Cash Flow BIT($)", Add_cum_cash_flow_BIT);

            ok = data.next();
         }
         break;
      }
      case NPV_CALCS:
      {
         getConfigs();  //obtains latest econconfigs from the ini file

         TAPSTable* scribble = new TAPSTable(NULL);

         scribble->copyFieldNamesFrom(data);
         scribble->copyPivotsFrom(data);
         scribble->copySortFieldsFrom(data);
         scribble->addField("Investment Start Year");
         scribble->markFieldAsAPivot("Investment Start Year");

         int years = End_year - Begin_year + 1;
         int numblocks = years - Investment_period + 1;

         // copy data into scribble placing each investment period length subsequence
         // in scribble as a block (ie add new factor "Investment Start Year")
         bool ok = data.first();
         while (ok)
         {
            for (int i = 0; i < numblocks; i++)
            {
               // copy out a sub-block to scribble
               vector<TAPSRecord>::const_iterator cursor = data.begin();

               cursor += i;
               vector<TAPSRecord>::const_iterator endsubblock =
                                                cursor + Investment_period;
               int startyr = atoi((cursor->getFieldValue("Year")).c_str());
               for (    ; cursor < endsubblock; cursor++)
               {
                  TAPSRecord curr_record = *cursor;
                  curr_record.setFieldValue("Investment Start Year",IntToStr(startyr).c_str());
                  scribble->storeRecord(curr_record);
               }
            }
            ok = data.next();
         }
         scribble->endStoringData();

         // now all the blocks in scribble are Investment_period long. For each block
         // calculate cane income, costs and tax. Also store base case entries if found

         // result vectors
         vector<double> vUpfront, vNet_Cash_Flow, vNet_Cash_Flow_BIT, vCumulative_Cash_Flow,
                        vCumulative_Cash_Flow_BIT, vCane_Income, vCosts, vInterest, vTax;



         ok = scribble->first();
         while (ok)
         {
            // do calculations/operations that are common to the whole block:
            vector<TAPSRecord>::const_iterator record = scribble->begin();
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

            // do the calculations that need doing for every TAPSRecord in the block
            int time = 0;
            for (vector<TAPSRecord>::const_iterator record = scribble->begin();
                           record != scribble->end(); record++)
            {
            // reading database variables:
               double Cane_fresh_weight = StrToFloat(record->getFieldValue("Cane fresh wt (t per ha)").c_str());
               double Irrigation_from_allocation = StrToFloat(record->getFieldValue("Irrig from allocation (ML)").c_str());
               double Allocation_into_OFWS = StrToFloat(record->getFieldValue("Allocation into OFWS (ML)").c_str());
               double Irrigation_from_OOA = StrToFloat(record->getFieldValue("Irrig from OOA (ML)").c_str());
               double OOA_into_OFWS = StrToFloat(record->getFieldValue("OOA into OFWS (ML)").c_str());
               double Applied_irrigation  = StrToFloat(record->getFieldValue("Applied irrigation (ML)").c_str());

            // Cane Income section:
               double Salvage = Upfront_installation_costs * Salvage_rate/100;
               // only count salvage in the last year of invest.
               if (time == Investment_period - 1)
                  Salvage = Salvage;
               else
                  Salvage = 0;

               double Cane_income = ((Cane_fresh_weight * ((config->Sugar_price*0.009*(config->CCS-4)+config->Payment_constant))
                                    * Irrig_area) + Salvage )
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
                  if (time != 0)  Upfront_costs = 0.0;
                  else
                  {
                     Upfront_costs = Upfront_installation_costs;
                     Cum_net_cash_flow = Cum_net_cash_flow_BIT = -Upfront_installation_costs;
                  }


                  vUpfront.push_back(Upfront_costs);

                  Costs    = ( /*Upfront_costs + */ Yearly_cost )
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
                  vUpfront.push_back(0.0);
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
//               vCumulative_Cash_Flow.push_back(Cum_net_cash_flow);
//               vCumulative_Cash_Flow_BIT.push_back(Cum_net_cash_flow_BIT);
               vCane_Income.push_back(Cane_income);
               vCosts.push_back(Costs);
//               vInterest.push_back(Interest);
               vTax.push_back(Tax);

               time++;
            }

            ok = scribble->next();
         }
         scribble->first();
         scribble->storeNumericArray("NetCF",vNet_Cash_Flow);
         scribble->storeNumericArray("NetCFBIT",vNet_Cash_Flow_BIT);
//         scribble->storeNumericArray("CumCF",vCumulative_Cash_Flow);
//         scribble->storeNumericArray("CumCFBIT",vCumulative_Cash_Flow_BIT);
         scribble->storeNumericArray("CIncome",vCane_Income);
         scribble->storeNumericArray("Csts",vCosts);
//         scribble->storeNumericArray("Int",vInterest);
         scribble->storeNumericArray("Tx",vTax);
         scribble->storeNumericArray("Upfront",vUpfront);

         scribble->endStoringData();

         // isolate and store the base case values
         vector< vector<double> > vBase_CIncome, vBase_Costs, vBase_Tax, vBase_Upfront;
         ok = scribble->first();
         while (ok)
         {
            string block_name = scribble->getDataBlockName();
            if (block_name == Base_case)
            {
               vector<double> temp;
               scribble->fieldAsNumericArray("CIncome", temp);
               vBase_CIncome.push_back(temp);
               scribble->fieldAsNumericArray("Csts", temp);
               vBase_Costs.push_back(temp);
               scribble->fieldAsNumericArray("Tx", temp);
               vBase_Tax.push_back(temp);
               scribble->fieldAsNumericArray("Upfront", temp);
               vBase_Upfront.push_back(temp);
            }
            ok = scribble->next();
         }

         // go through table and calculate the additional values
         vector<double> vAdd_CIncome, vAdd_Costs, vAdd_Tax,
                        vAdd_Flow, vAdd_Flow_BIT, vAdd_Upfront;
         ok = scribble->first();
         int base_counter = 0;
         while (ok)
         {
            vector<double> temp;
            scribble->fieldAsNumericArray("CIncome",temp);
            vAdd_CIncome = subtract(temp,vBase_CIncome[base_counter]);
            scribble->storeNumericArray("Add_CIncome",vAdd_CIncome);

            scribble->fieldAsNumericArray("Csts",temp);
            vAdd_Costs = subtract(temp,vBase_Costs[base_counter]);
            scribble->storeNumericArray("Add_Costs",vAdd_Costs);

            scribble->fieldAsNumericArray("Tx",temp);
            vAdd_Tax = subtract(temp,vBase_Tax[base_counter]);
            scribble->storeNumericArray("Add_Tax",vAdd_Tax);

            scribble->fieldAsNumericArray("Upfront",temp);
            vAdd_Upfront = subtract(temp,vBase_Upfront[base_counter]);
            scribble->storeNumericArray("Add_Upfront",vAdd_Upfront);

            vAdd_Flow_BIT = subtract(vAdd_CIncome, vAdd_Costs);
            vAdd_Flow = subtract(vAdd_Flow_BIT, vAdd_Tax);
            scribble->storeNumericArray("Add_Flow",vAdd_Flow);
            scribble->storeNumericArray("Add_Flow_BIT",vAdd_Flow_BIT);


            base_counter++;
            if (base_counter == vBase_CIncome.size())
               base_counter = 0;

            ok = scribble->next();
         }


      // now build the NPVtable from information in scribble:
         TAPSTable* NPVtable = new TAPSTable(NULL);

         vector<string> sim_names;
         scribble->getDataBlockNames(sim_names);
         NPVtable->first();
         NPVtable->storeStringArray("Simulation", sim_names);

         vector<string> vYear;
         vector< vector<string> > vPivot;
         vector<double> vNPV, vIRR, vNPV_BIT, vIRR_BIT;

         vector<string> pivot_names;
         scribble->getPivotNames(pivot_names);
         // remove Investment Start Year from the the pivots
         vector<string>::iterator find_pos =
            find(pivot_names.begin(),pivot_names.end(), "Investment Start Year");
         if (find_pos != pivot_names.end())
            pivot_names.erase(find_pos);

         for (int p = 0; p < pivot_names.size(); p++)
         {
            vector<string> pivot_values;
            vPivot.push_back(pivot_values);
         }

         ok = scribble->first();
         while (ok)
         {
            // find the EconConfig for the current block:
            vector<TAPSRecord>::const_iterator record = scribble->begin();
            string config_name = record->getFieldValue(DE_ECON_FACTOR_NAME);
            vector<DEEconConfig>::const_iterator find_pos =
               find_if(Econ_configs.begin(),Econ_configs.end(),
                                EqualToName<DEEconConfig>(config_name));
            if (find_pos == Econ_configs.end()) return; // should throw an error here

            const DEEconConfig* config = find_pos;

            string sim = scribble->getDataBlockName();
            string year = scribble->begin()->getFieldValue("Investment Start Year");

            for (int p = 0; p < pivot_names.size(); p++)
            {
               string pivot_val = scribble->begin()->getFieldValue(pivot_names[p]);
               vPivot[p].push_back(pivot_val);
            }

            double npv, irr, npvBIT, irrBIT;
            if (sim == Base_case) {
               npv = 0;
               irr = 0;
               npvBIT = 0;
               irrBIT = 0;
            }
            else {
               vector<double> cf, cfBIT, upfront;
               scribble->fieldAsNumericArray("Add_Flow", cf);
               scribble->fieldAsNumericArray("Add_Upfront", upfront);
               scribble->fieldAsNumericArray("Add_Flow_BIT", cfBIT);

               npv = NetPresentValue(config->Investment_rate/100,
                                            cf.begin(),
                                            cf.size()-1,
                                            ptEndOfPeriod) - upfront[0];

               npvBIT = NetPresentValue(config->Investment_rate/100,
                                               cfBIT.begin(),
                                               cfBIT.size()-1,
                                               ptEndOfPeriod) - upfront[0];

               //if (upfront[0] == 0.0) upfront[0] = 1.0;

               cf.insert(cf.begin(), -upfront[0]);
               try
               {
                  irr = InternalRateOfReturn(0.1, cf.begin(),
                                                          cf.size()-1);
               }
               catch (EInvalidArgument& e)
               {
                  irr = 0;
               }
               catch (Exception& e)
               {
                  irr = -1;
               }

               cfBIT.insert(cfBIT.begin(), -upfront[0]);
               try
               {
                  irrBIT = InternalRateOfReturn(0.1, cfBIT.begin(),
                                                          cfBIT.size()-1);
               }
               catch (EInvalidArgument& e)
               {
                  irrBIT = 0;
               }
               catch (Exception& e)
               {
                  irrBIT = -1;
               }
            }
            vYear.push_back(year);
            vNPV.push_back(npv);
            vIRR.push_back(irr);
            vNPV_BIT.push_back(npvBIT);
            vIRR_BIT.push_back(irrBIT);

            ok = scribble->next();
         }

         multiply_value(vIRR, 100);
         multiply_value(vIRR_BIT, 100);

//         NPVtable->copyPivotsFrom(*scribble);

         NPVtable->first();

         for (int p = 0; p < pivot_names.size(); p++)
         {
            NPVtable->markFieldAsAPivot(pivot_names[p]);
            NPVtable->storeStringArray(pivot_names[p], vPivot[p]);
         }

         NPVtable->storeStringArray("Year", vYear);
         NPVtable->storeNumericArray("NPV with Tax ($)", vNPV);
         NPVtable->storeNumericArray("IRR with Tax (%)", vIRR);
         NPVtable->storeNumericArray("NPV without Tax ($)", vNPV_BIT);
         NPVtable->storeNumericArray("IRR without Tax (%)", vIRR_BIT);

         //NPVtable->markFieldAsAPivot("Simulation");
         NPVtable->addSortField("Year");
         NPVtable->endStoringData();


         data.storeData(*NPVtable);
         delete scribble;
         delete NPVtable;
         break;
      }
   }
   // indicate that no updates are necessary unless the base table changes:
   needs_update = false;

   Screen->Cursor = savedCursor;
}
