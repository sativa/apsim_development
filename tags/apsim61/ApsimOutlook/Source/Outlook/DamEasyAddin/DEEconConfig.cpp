//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "DEEconConfig.h"
#include <general\path.h>

//---------------------------------------------------------------------------

#pragma package(smart_init)

using namespace std;

DEEconConfig::DEEconConfig() {
   OFWS_construction_cost = 0;
   OFWS_pump_cost = 0;
   Reticulation_cost = 0;
   Storage_pumping_cost = 0;
   Allocation_price = 0;
   OOA_price = 0;
   Sugar_price = 0;
   CCS = 0;
   Irrigation_operating_cost = 0;
   Cash_cost = 0;
   Overhead_cost = 0;
   Harvesting_and_levies = 0;
   Interest_rate = 0;
   Investment_rate = 0;
   Inflation_input_rate = 0;
   Inflation_cane_rate = 0;
   Payment_constant = 0.6455;
   Repayment_time = 0;
   Num_partners = 0;
   Upfront = false;
   Calc_tax = true;
   Starting_year = 0;
   Name = "Name not set";
}

DEEconConfig::DEEconConfig(const DEEconConfig& from)
{
   *this = from;
}

DEEconConfig::~DEEconConfig()
{
}

DEEconConfig& DEEconConfig::operator=(const DEEconConfig& rhs)
{
   // check for assignment to self
   if (this == &rhs)
      return *this;

   Name =                        rhs.Name;
   OFWS_construction_cost =      rhs.OFWS_construction_cost;
   OFWS_pump_cost =              rhs.OFWS_pump_cost;
   Reticulation_cost =           rhs.Reticulation_cost;
   Storage_pumping_cost =        rhs.Storage_pumping_cost;
   Allocation_price =            rhs.Allocation_price;
   OOA_price =                   rhs.OOA_price;
   Sugar_price =                 rhs.Sugar_price;
   CCS =                         rhs.CCS;
   Irrigation_operating_cost =   rhs.Irrigation_operating_cost;
   Cash_cost =                   rhs.Cash_cost;
   Overhead_cost =               rhs.Overhead_cost;
   Harvesting_and_levies =       rhs.Harvesting_and_levies;
   Interest_rate =               rhs.Interest_rate;
   Investment_rate =             rhs.Investment_rate;
   Inflation_input_rate =        rhs.Inflation_input_rate;
   Inflation_cane_rate =         rhs.Inflation_cane_rate;
   Payment_constant =            rhs.Payment_constant;
   Repayment_time =              rhs.Repayment_time;
   Num_partners =                rhs.Num_partners;
   Upfront =                     rhs.Upfront;
   Calc_tax =                    rhs.Calc_tax;
   Starting_year =               rhs.Starting_year;

   return *this;
}

string DEEconConfig::getName() const
{
   return Name;
}


void DEEconConfig::writeToFile() const
{
   string section_name = SECTION_PREFIX + this->Name;
   // delete any duplicate entry
//   ini.Delete_section(section_name.c_str());

   // create a new blank section (must do this to get around a win95 bug)
//   ini.Write_section_contents(section_name.c_str(), "");

   // fill out the new section
   string section = "";
   section += "OFWS_construction_cost       = "; section += FloatToStr(OFWS_construction_cost).c_str(); section += "\n";
   section += "OFWS_pump_cost               = "; section += FloatToStr(OFWS_pump_cost).c_str(); section += "\n";
   section += "Reticulation_cost            = "; section += FloatToStr(Reticulation_cost).c_str(); section += "\n";
   section += "Storage_pumping_cost         = "; section += FloatToStr(Storage_pumping_cost).c_str(); section += "\n";
   section += "Allocation_price             = "; section += FloatToStr(Allocation_price).c_str(); section += "\n";
   section += "OOA_price                    = "; section += FloatToStr(OOA_price).c_str(); section += "\n";
   section += "Sugar_price                  = "; section += FloatToStr(Sugar_price).c_str(); section += "\n";
   section += "CCS                          = "; section += FloatToStr(CCS).c_str(); section += "\n";
   section += "Irrigation_operating_cost    = "; section += FloatToStr(Irrigation_operating_cost).c_str(); section += "\n";
   section += "Cash_cost                    = "; section += FloatToStr(Cash_cost).c_str(); section += "\n";
   section += "Overhead_cost                = "; section += FloatToStr(Overhead_cost).c_str(); section += "\n";
   section += "Harvesting_and_levies        = "; section += FloatToStr(Harvesting_and_levies).c_str(); section += "\n";
   section += "Inflation_input_rate         = "; section += FloatToStr(Inflation_input_rate).c_str(); section += "\n";
   section += "Inflation_cane_rate          = "; section += FloatToStr(Inflation_cane_rate).c_str(); section += "\n";
   section += "Payment_constant             = "; section += FloatToStr(Payment_constant).c_str(); section += "\n";
   section += "Repayment_time               = "; section += FloatToStr(Repayment_time).c_str(); section += "\n";
   section += "Interest_rate                = "; section += FloatToStr(Interest_rate).c_str(); section += "\n";
   section += "Investment_rate              = "; section += FloatToStr(Investment_rate).c_str(); section += "\n";
   section += "Num_partners                 = "; section += FloatToStr(Num_partners).c_str(); section += "\n";
   section += "Upfront                      = "; section += FloatToStr(Upfront).c_str(); section += "\n";
   section += "Calc_tax                     = "; section += FloatToStr(Calc_tax).c_str(); section += "\n";
   section += "Starting_year                = "; section += FloatToStr(Starting_year).c_str(); section += "\n";

   settings.writeSection(section_name, section);

   /*
   ini.Write(section_name.c_str(), "OFWS_construction_cost       ", FloatToStr(OFWS_construction_cost).c_str());
   ini.Write(section_name.c_str(), "OFWS_pump_cost               ", FloatToStr(OFWS_pump_cost).c_str());
   ini.Write(section_name.c_str(), "Reticulation_cost            ", FloatToStr(Reticulation_cost).c_str());
   ini.Write(section_name.c_str(), "Storage_pumping_cost         ", FloatToStr(Storage_pumping_cost).c_str());
   ini.Write(section_name.c_str(), "Allocation_price             ", FloatToStr(Allocation_price).c_str());
   ini.Write(section_name.c_str(), "OOA_price                    ", FloatToStr(OOA_price).c_str());
   ini.Write(section_name.c_str(), "Sugar_price                  ", FloatToStr(Sugar_price).c_str());
   ini.Write(section_name.c_str(), "CCS                          ", FloatToStr(CCS).c_str());
   ini.Write(section_name.c_str(), "Irrigation_operating_cost    ", FloatToStr(Irrigation_operating_cost).c_str());
   ini.Write(section_name.c_str(), "Cash_cost                    ", FloatToStr(Cash_cost).c_str());
   ini.Write(section_name.c_str(), "Overhead_cost                ", FloatToStr(Overhead_cost).c_str());
   ini.Write(section_name.c_str(), "Harvesting_and_levies        ", FloatToStr(Harvesting_and_levies).c_str());
   ini.Write(section_name.c_str(), "Inflation_input_rate         ", FloatToStr(Inflation_input_rate).c_str());
   ini.Write(section_name.c_str(), "Inflation_cane_rate          ", FloatToStr(Inflation_cane_rate).c_str());
   ini.Write(section_name.c_str(), "Repayment_time               ", FloatToStr(Repayment_time).c_str());
   ini.Write(section_name.c_str(), "Interest_rate                ", FloatToStr(Interest_rate).c_str());
   ini.Write(section_name.c_str(), "Investment_rate              ", FloatToStr(Investment_rate).c_str());
   ini.Write(section_name.c_str(), "Num_partners                 ", FloatToStr(Num_partners).c_str());
   ini.Write(section_name.c_str(), "Upfront                      ", FloatToStr(Upfront).c_str());
   ini.Write(section_name.c_str(), "Calc_tax                     ", FloatToStr(Calc_tax).c_str());
   ini.Write(section_name.c_str(), "Starting_year                ", FloatToStr(Starting_year).c_str());
*/

}

bool DEEconConfig::readFromFile(const string& name)
{
   // find the relevant file section - if not found return false
   string section_name = string(SECTION_PREFIX) + name;
   string value;
   settings.read(section_name + "|" + "OFWS_construction_cost", value);
   if (value == "")  // then the section does not exist
      return false;

   // read variable values and assign
   OFWS_construction_cost = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "OFWS_pump_cost", value);
   OFWS_pump_cost = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Reticulation_cost", value);
   Reticulation_cost = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Storage_pumping_cost", value);
   Storage_pumping_cost = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Allocation_price", value);
   Allocation_price = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "OOA_price", value);
   OOA_price = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Sugar_price", value);
   Sugar_price = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "CCS", value);
   CCS = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Irrigation_operating_cost", value);
   Irrigation_operating_cost = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Cash_cost", value);
   Cash_cost = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Overhead_cost", value);
   Overhead_cost = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Harvesting_and_levies", value);
   Harvesting_and_levies = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Inflation_input_rate", value);
   Inflation_input_rate = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Inflation_cane_rate", value);
   Inflation_cane_rate = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Payment_constant", value);
   Payment_constant = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Repayment_time", value);
   Repayment_time = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Interest_rate", value);
   Interest_rate = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Investment_rate", value);
   Investment_rate = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Num_partners", value);
   Num_partners = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Upfront", value);
   Upfront = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Calc_tax", value);
   Calc_tax = StrToFloat(value.c_str());
   settings.read(section_name + "|" + "Starting_year", value);
   Starting_year = StrToFloat(value.c_str());

   Name = name;
   return true;
}

void DEEconConfig::deleteFromFile() const
{
   string section_name = string(SECTION_PREFIX) + Name;
   settings.deleteSection(section_name);
}

bool DEEconConfig::editName(const std::string& new_name)
{
return true;
/*
   string new_section_name = string(SECTION_PREFIX) + new_name;
   // check to see if new_name already exists - if so, return false
   list<string> section_names;
   Ini_file ini(File_name.c_str());
   settings.Read_section_names(section_names);
   list<string>::iterator found_pos = find(section_names.begin(),
                                           section_names.end(),new_section_name);
   if (found_pos != section_names.end())
      return false;

   // else change name to new_name
   string old_section_name = string(SECTION_PREFIX) + Name;
   settings.Delete_section(old_section_name.c_str());
   Name = new_name;
   this->writeToFile();
   return true;
*/
}

