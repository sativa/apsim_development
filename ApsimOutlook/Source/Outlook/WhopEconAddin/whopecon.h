//---------------------------------------------------------------------------

#ifndef WhopEconH
#define WhopEconH
//---------------------------------------------------------------------------
#include "..\Addin.h"
#include "..\Scenario.h"

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

      WhopEcon(const std::string& parameters);
      ~WhopEcon(void);

      // return a default scenario to caller.
      virtual Scenario getDefaultScenario(void) const;

      // make the scenario passed in a valid one.  This may mean adding
      // extra factors to the scenario or changing existing factors.
      virtual void makeScenarioValid(Scenario& scenario, const std::string factor_name) const;

		// the following method returns a
      // list of values for the specified factor in the specified scenario.
		virtual void getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
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

      vector<Factor> factors;
      std::string Econ_bitmap_name;
      std::string Econ_DB_name;

};
//---------------------------------------------------------------------------
#endif

