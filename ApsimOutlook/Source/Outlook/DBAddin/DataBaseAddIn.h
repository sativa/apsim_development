//---------------------------------------------------------------------------
#ifndef DataBaseAddInH
#define DataBaseAddInH

#include "AddIn.h"
#include "DBSimulation.h"
#include <map>
#include <ApsimShared\ApsimSettings.h>
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
      typedef std::map<std::string, Graphics::TBitmap*> ImageMap;
      SimulationContainer simulations;
      ImageMap images;
      ApsimSettings settings;

      void askUserForMDBs(void);
      void askUserForDataSets(void);
      void readAllDatabases(std::vector<std::string>& databaseFilenames);
      void readDatabase(const std::string& databaseFileName);
      Scenario convertSimulationToScenario(const DBSimulation& simulation,
                                           const std::string& scenarioName,
                                           const std::string& econFactorValue) const;
      void readAllImages(void);
      const Graphics::TBitmap* getImageForFactor(const std::string& factorName) const;
   };

#endif
