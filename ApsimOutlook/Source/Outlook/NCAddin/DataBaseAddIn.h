//---------------------------------------------------------------------------
#ifndef DataBaseAddInH
#define DataBaseAddInH

#include "AddIn.h"
#include "DBSimulation.h"
#include <map>
// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a database addin

//  Changes:
//    DPH 18/6/98

// ------------------------------------------------------------------
class DatabaseAddIn : public AddInBase
   {
   public:
      DatabaseAddIn(void) { };
      ~DatabaseAddIn(void);

      // set any startup parameters.
      virtual void setStartupParameters(const std::string& parameters);

      // return a default scenario to caller.
      virtual Scenario getDefaultScenario(void) const;

      // return true if the simulation is valid.  False otherwise.
      virtual bool isScenarioValid(Scenario& scenario) const;

      // make the scenario passed in a valid one.  This may mean adding
      // extra factors to the scenario or changing existing factors.
      virtual void makeScenarioValid(Scenario& scenario, const std::string factor_name) const;

		// if your add-in wants to just supply values for the "Value Selection" form
		// then the following method needs to be implemented.  It returns a
      // list of values for the specified factor in the specified scenario.
		virtual void getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const;

      // given the source data object, and the list of user selected
      // scenarios, perform all calculations and store all new data
      // in the returned TAPSTable.
      virtual void doCalculations(TAPSTable& data, const Scenario&);
   private:
      typedef std::vector<DBSimulation*> SimulationContainer;
      SimulationContainer simulations;

      void askUserForNCs(void);
      void askUserForDataSets(void);
      void readAllNCs(std::vector<std::string>& NCFilenames);
      void readNCFile(const std::string& NCFileName);
      Scenario convertSimulationToScenario(const DBSimulation& simulation,
                                           const std::string& scenarioName,
                                           const std::string& econFactorValue) const;
   };

#endif
