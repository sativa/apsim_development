//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "DamEasyEcon.h"
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\math_functions.h>
#include <general\path.h>
#include <functional>
#include <ApsimShared\ApsimDirectories.h>
#include "TDamEasy_form.h"

#define DE_ECON_FACTOR_NAME "Econ Config"
#define ECON_CONFIGS_KEY    "DamEa$y Economics|econconfigs"
#define BITMAP_NAME_KEY     "DamEa$y Economics|bitmap"
#define TAX_BRACKETS_KEY    "DamEa$y Economics|tax_brackets"
#define TAX_RATES_KEY    "DamEa$y Economics|tax_rates"
#define SIMULATION_FACTOR_NAME "Simulation"

#pragma package(smart_init)
using namespace std;

// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DAH 20/4/01

// ------------------------------------------------------------------
extern "C" AddInBase* _export __stdcall createAddIn()
   {
   return new DamEasyEcon();
   }
// ------------------------------------------------------------------
// Exported function to delete the specified addin.
// ------------------------------------------------------------------
extern "C" void _export __stdcall deleteAddIn(AddInBase* addin)
   {
   delete addin;
   }


// ------------------------------------------------------------------
// set any startup parameters.
// ------------------------------------------------------------------
void DamEasyEcon::setStartupParameters(const std::string& parameters)
   {
   // handle parameter string
   vector<string> begin_and_end_years;
   Split_string(parameters, " ", begin_and_end_years);
   Begin_year = atoi(begin_and_end_years[0].c_str());
   End_year = atoi(begin_and_end_years[1].c_str());
   Read_inifile_settings();

   string app_dir = getAppHomeDirectory();
   string econ_bmp_name = app_dir + string("\\") + Econ_bitmap_name;

   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   bitmap->LoadFromFile(econ_bmp_name.c_str());

   string default_config_name = Econ_configs[0].getName();
   Factor econ(DE_ECON_FACTOR_NAME,default_config_name.c_str());
   factors.push_back(econ);
   }

DamEasyEcon::~DamEasyEcon(void) { }

// return true if the simulation is valid.  False otherwise.
bool DamEasyEcon::isScenarioValid(Scenario& scenario) const
   {
   string econName = scenario.getFactorValue(DE_ECON_FACTOR_NAME);

   vector<string> econConfigs;
   getAllFactorValues("", econConfigs);
   return (find(econConfigs.begin(), econConfigs.end(),
                econName) != econConfigs.end());
   }

void DamEasyEcon::makeScenarioValid(Scenario& scenario,
                                    const std::string factor_name) const
   {
   string econName = scenario.getFactorValue(DE_ECON_FACTOR_NAME);

   vector<string> econConfigs;
   getAllFactorValues("", econConfigs);
   if (find(econConfigs.begin(), econConfigs.end(), econName) == econConfigs.end())
      {
      // not valid - make it valid.
      scenario.setFactorValue(DE_ECON_FACTOR_NAME, econConfigs[0]);
      }
   }

Scenario DamEasyEcon::getDefaultScenario(void) const {
   // check the following to see if "" is the right thing to pass in.
   return Scenario("",factors);
}


void DamEasyEcon::getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const
{
   if (factorName == DE_ECON_FACTOR_NAME)
   {
      Read_inifile_settings();  //slow but necessary
      getAllFactorValues(factorName, factorValues);
   }
}


void DamEasyEcon::getAllFactorValues(const std::string& factorName,
                                   std::vector<std::string>& factorValues) const
{
   for_each(Econ_configs.begin(), Econ_configs.end(),
         GetNameFunction< vector<string>, DEEconConfig>(factorValues));
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
   // read all defaults.
   string st;
   settings.read(BITMAP_NAME_KEY, st);
   Econ_bitmap_name = st;

   getConfigs();

   vector<string> words;

   // read tax table
   settings.read ("TAX_BRACKETS_KEY", st);
   Split_string(st, ",", words);
   String2double< vector<string>, vector<double> >(words, Tax_brackets);
   settings.read (TAX_RATES_KEY, st);
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
                                  const Scenario& selectedScenarios)
{  }


// ------------------------------------------------------------------
// display our form.
// ------------------------------------------------------------------
void DamEasyEcon::showUI(void)
   {
   DamEasy_form->setMyConfigs(Econ_configs);
   if (DamEasy_form->ShowModal() == mrOk)
      saveConfigs();

   else
      getConfigs();
   }
void DamEasyEcon::getConfigs()
{
   string st;
   settings.read(ECON_CONFIGS_KEY, st);
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
void DamEasyEcon::saveConfigs()
{
   // update the ini file
   vector<string> config_names;
   getAllFactorValues("", config_names);
   string config_names_comma_list;
   Build_string(config_names, ",", config_names_comma_list);
   settings.write(ECON_CONFIGS_KEY, config_names_comma_list.c_str());

   // remove old Econconfig sections, and put in only the ones remaining in Econ_configs
   vector<string> Section_names;
   settings.getSectionNames(Section_names);
   // find those section names that are EconConfig entries
   vector<string>::iterator sect_name = Section_names.begin();
   while (sect_name != Section_names.end()) {
      string prefix = SECTION_PREFIX;
      string::iterator where =
           search((*sect_name).begin(),(*sect_name).end(),prefix.begin(),prefix.end());
      if (where == (*sect_name).begin())  // then this section needs deleting
         settings.deleteSection((*sect_name).c_str());
      sect_name++;
   }

   // now write to file the current list of EconConfigs
   vector<DEEconConfig>::iterator config;
   for (config = Econ_configs.begin(); config!= Econ_configs.end(); config++)
      (*config).writeToFile();
}

