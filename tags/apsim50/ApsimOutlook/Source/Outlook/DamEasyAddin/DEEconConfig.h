//---------------------------------------------------------------------------

#ifndef DEEconConfigH
#define DEEconConfigH

#include <string>
#include <ApsimShared\ApsimSettings.h>

#define SECTION_PREFIX "EconConfig "

class DEEconConfig {
   public:
      DEEconConfig();
      DEEconConfig(const DEEconConfig& from);

      ~DEEconConfig();

      DEEconConfig& operator=(const DEEconConfig& rhs);

      static bool setIOFile(const std::string& file_name);

      std::string getName() const;

      double OFWS_construction_cost;
      double OFWS_pump_cost;
      double Reticulation_cost;
      double Storage_pumping_cost;
      double Allocation_price;
      double OOA_price;
      double Sugar_price;
      double CCS;
      double Irrigation_operating_cost;
      double Cash_cost;
      double Overhead_cost;
      double Harvesting_and_levies;
      double Interest_rate;
      double Investment_rate;
      double Inflation_input_rate;
      double Inflation_cane_rate;
      double Payment_constant;
      int Repayment_time;
      int Num_partners;
      bool   Upfront;
      bool   Calc_tax;
      int Starting_year;

      void writeToFile() const;
      bool readFromFile(const std::string& name);
      void deleteFromFile() const;
      bool editName(const std::string& new_name);
      void setName(const std::string& new_name) { Name = new_name;};
      static std::string File_name;

   private:
      std::string Name;
      ApsimSettings settings;
};


//---------------------------------------------------------------------------
#endif
