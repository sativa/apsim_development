//---------------------------------------------------------------------------
#ifndef DamEasyEconH
#define DamEasyEconH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TAPSTable.h"
#include <Db.hpp>
#include <DBTables.hpp>
#include <istream.h>
#include <vector>
#include "..\Addin.h"
#include "AddCostsBenefits.h"
#include "..\Scenario.h"
#include "DEEconConfig.h"
#include <ApsimShared\ApsimSettings.h>

using std::vector;
// ------------------------------------------------------------------
//  Short description:
//      this component allows the use to add Dam Ea$y economics fields to the memory table.

//  Notes:

//  Changes:
//    DPH 15/12/99
//    DAH 27/10/00
//    DAH 20/04/01 changed to an Addin derived class.

// ------------------------------------------------------------------
class DamEasyEcon : public AddInBase
   {
   public:

      DamEasyEcon() { };
      ~DamEasyEcon(void);

      // set any startup parameters.
      virtual void setStartupParameters(const std::string& parameters);

      // return a default scenario to caller.
      virtual Scenario getDefaultScenario(void) const;

      // return true if the simulation is valid.  False otherwise.
      virtual bool isScenarioValid(Scenario& scenario) const;

      // make the scenario passed in a valid one.  This may mean adding
      // extra factors to the scenario or changing existing factors.
      virtual void makeScenarioValid(Scenario& scenario,
                                          const std::string factor_name) const;

		// the following method returns a
      // list of values for the specified factor in the specified scenario.
		virtual void getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const;

      // given the data object, and the list of user selected
      // scenarios, perform all calculations and store all new data
      // in the returned TAPSTable.
      virtual void doCalculations(TAPSTable& data, const Scenario& selectedScenarios);

      // if your add-in wants to display a properties form then implement
      // the following 2 methods.
      virtual std::string getUIButtonCaption(void) const {return "Edit economic config.";}
      virtual void showUI(void);

   private:
      ApsimSettings settings;
      void Read_inifile_settings (void);
      string Get_descriptor_value(string Descriptor, string Item);
		void getAllFactorValues(const std::string& factorName,
                                     std::vector<std::string>& factorValues) const;
      void getConfigs(void);
      void saveConfigs(void);
      
      vector<Factor> factors;
      std::string Econ_bitmap_name;

      int Begin_year;   // the year simulations start
      int End_year;     //  "               "   end

      vector<DEEconConfig> Econ_configs;
      vector<double> Tax_brackets, Tax_rates;


};
//---------------------------------------------------------------------------
#endif
