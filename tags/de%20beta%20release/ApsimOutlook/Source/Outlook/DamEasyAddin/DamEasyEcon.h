//---------------------------------------------------------------------------
#ifndef DAMEASYH
#define DAMEASYH
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
#include "TDEValueSelectionForm.h"
#include "TDamEasy_form.h"
#include "AddCostsBenefits.h"
#include "..\Scenario.h"
#include "DEEconConfig.h"

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

      DamEasyEcon(const std::string& parameters);
      ~DamEasyEcon(void);

      // return a default scenario to caller.
      virtual Scenario getDefaultScenario(void) const;

      // make the scenario passed in a valid one.  This may mean adding
      // extra factors to the scenario or changing existing factors.
      virtual void makeScenarioValid(Scenario& scenario) const;

		// the following method returns a
      // list of values for the specified factor in the specified scenario.
		virtual void getFactorValues(const std::string& factorName,
                                   std::vector<std::string>& factorValues) const;

      virtual TValueSelectionForm*  getUIForm(const std::string& factorName,
                                                             TComponent* Owner)const;

      void __fastcall updateConfigs(TObject *Sender);

      // given the data object, and the list of user selected
      // scenarios, perform all calculations and store all new data
      // in the returned TAPSTable.
      virtual void doCalculations(TAPSTable& data,
                                  const std::vector<Scenario*>& selectedScenarios);

   private:
      static int numObjects;
      void Read_inifile_settings (void);
      string Get_descriptor_value(string Descriptor, string Item);
      void saveConfigs();

      vector<Factor> factors;
      std::string Econ_bitmap_name;

      int Begin_year;   // the year simulations start
      int End_year;     //  "               "   end

      vector<DEEconConfig> Econ_configs;
      vector<double> Tax_brackets, Tax_rates;


};
//---------------------------------------------------------------------------
#endif
