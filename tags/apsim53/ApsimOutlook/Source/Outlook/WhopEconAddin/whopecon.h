//---------------------------------------------------------------------------

#ifndef WhopEconH
#define WhopEconH
//---------------------------------------------------------------------------
#include "..\Addin.h"
#include "..\Scenario.h"
#include <set>
#include <ApsimShared\ApsimSettings.h>
#include <GrossMarginCalculator\GMCalculator.h>
// ------------------------------------------------------------------
//  Short description:
//      Defines the WhopEcon class which implements economics for Whopper Cropper
//  Notes:

//  Changes:
//    DAH 29/08/01 created

// ------------------------------------------------------------------
class WhopEcon : public AddInBase
   {
   public:

      WhopEcon(void) { };
      ~WhopEcon(void);

      // set any startup parameters.
      virtual void setStartupParameters(const std::string& parameters);

      // return a default scenario to caller.
      virtual Scenario getDefaultScenario(void) const;

      // return true if the simulation is valid.  False otherwise.
      virtual bool isScenarioValid(Scenario& scenario) const;

      // make the scenario passed in a valid one.  This may mean adding
      // extra factors to the scenario or changing existing factors.
      virtual void makeScenarioValid(Scenario& scenario, const std::string factor_name) const;

		// the following method returns a
      // list of values for the specified factor in the specified scenario.
		virtual void getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const;

      // if your add-in wants to display a properties form then implement
      // the following 2 methods.
      virtual std::string getUIButtonCaption(void) const {return "Edit economic config.";}
      virtual void showUI(void);

      void __fastcall updateConfigs(TObject *Sender);

      // given the data object, and the list of user selected
      // scenarios, perform all calculations and store all new data
      // in the returned TAPSTable.
      virtual void doCalculations(TAPSTable& data, const Scenario& scenario);

      // gives the add-in the chance to return any information to the
      // 'displaySettings' window e.g. warnings, errors etc.
      virtual std::string getDisplaySettings(void);
      
   private:
      string econMDB;
      GMCalculator gm;
      ApsimSettings settings;
      static int numObjects;
      std::set<std::string, std::less<std::string> > warnings;
      string Get_descriptor_value(string Descriptor, std::string Item);
      bool getFloatFromRecord(const TAPSRecord* record,
                              std::vector<std::string>& IDs,
                              float& value);
      std::string getStringFromRecord(const TAPSRecord* record,
                               std::vector< std::string >& IDs);
      void addWarning(const std::string& msg);

      std::vector<Factor> factors;
      std::string Econ_DB_name;

};
//---------------------------------------------------------------------------
#endif

