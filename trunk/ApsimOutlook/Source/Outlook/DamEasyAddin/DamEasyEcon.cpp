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

// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DAH 20/4/01

// ------------------------------------------------------------------
extern "C" AddInBase* _export __stdcall createAddIn(const string& parameters, bool& success)
   {
   // will be called with begin
   // and end years from the database
   success = true;
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
   // handle parameter string
   vector<string> begin_and_end_years;
   Split_string(parameters, " ", begin_and_end_years);
   Begin_year = atoi(begin_and_end_years[0].c_str());
   End_year = atoi(begin_and_end_years[1].c_str());
   Read_inifile_settings();

   Path bitmap_path(Application->ExeName.c_str());
   bitmap_path.Set_name (Econ_bitmap_name.c_str());
   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   bitmap->LoadFromFile(bitmap_path.Get_path().c_str());

   string default_config_name = Econ_configs[0].getName();
   Factor econ(bitmap,DE_ECON_FACTOR_NAME,default_config_name.c_str(),this);
   factors.push_back(econ);
   }

DamEasyEcon::~DamEasyEcon(void) { }


void DamEasyEcon::makeScenarioValid(Scenario& scenario,
                                        const std::string factor_name) const {}

Scenario DamEasyEcon::getDefaultScenario(void) const {
   // check the following to see if "" is the right thing to pass in.
   return Scenario("",factors);
}


void DamEasyEcon::getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const
{
   Read_inifile_settings();  //slow but necessary
   getAllFactorValues(factorName, factorValues);
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
   Econ_configs.clear();
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
{  }



