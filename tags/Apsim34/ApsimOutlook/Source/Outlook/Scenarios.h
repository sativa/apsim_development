//---------------------------------------------------------------------------

#ifndef ScenariosH
#define ScenariosH

#include <graphics.hpp>
#include <TAPSTable.h>
#include <ApsimShared\ApsimSettings.h>
#include <general\AddInManager.h>
#include "AddIn.h"
class Scenario;

// ------------------------------------------------------------------
// This class encapsulates a collection of scenarios.
// It also manages a collection
// of add-in's which supply the possible scenario objects.
// ------------------------------------------------------------------
class Scenarios
   {
   public:
      Scenarios(void);

      // return the number of scenarios.
      unsigned int count(void) {return scenarios.size();}

      // create multiple scenarios, based on the specified scenario, given
      // the factor name and 1 or more factor values.
      void createScenariosFrom(const std::string& scenarioName,
                               const std::string& factorName,
                               const std::vector<std::string>& factorValues);

      // Create a permutation of scenarios based on the current selected
      // scenario, the factor name passed in and 1 or more values for
      // that factor.
      void createScenarioPermutation(const std::string& scenarioName,
                                     const std::string& factorName,
                                     const std::vector<std::string>& factorValues);

      // return a list of scenario names to caller.
      void getScenarioNames(std::vector<std::string>& scenarioNames) const;

      // return a list of factor names for specified scenario.
      void getFactorNames(const std::string& scenarioName,
                          std::vector<std::string>& factorNames) const;
      // Return a factor value to caller.
      std::string getFactorValue(const std::string& scenarioName,
                                 const std::string& factorName) const;

      // Return a list of possible factor values for the given
      // scenario and factor name.
      void getFactorValues(const std::string& scenarioName,
                           const std::string& factor_name,
                           std::vector<std::string>& factorValues) const;

      // Rename the specified scenario.
      void renameScenario(const std::string& oldName, const std::string& newName);

      // Delete the specified scenario.
      void deleteScenario(const std::string& scenarioName);

      // Move a scenario and insert it before the specified dest scenario.
      void moveScenario(const std::string& sourceName, const std::string& destName);

      // Delete all scenarios - leaving a default scenario.
      void deleteAllScenarios();

      void getAllData(TAPSTable* table);

      // Save and restore scenarios.
      void save(const std::string& name);
      void restore(const std::string& name);

      // Allow the add-ins to display information in the settings window.
      std::string getDisplaySettings(void);

      // Allow the add-ins to display a UI form if they want to.
      std::string getUIButtonCaption(void) const;
      void showUI(void);

      void makeScenariosValid(void);


   private:
      AddInManager<AddInBase> addIns;

      typedef std::vector<Scenario> ScenarioContainer;
      ScenarioContainer scenarios;
      ApsimSettings settings;

      // prevent the following functions from being auto-generated:
      Scenarios(const Scenarios& rhs);
      Scenarios& operator=(const Scenarios& rhs);

      // create a default scenario.
      void makeDefaultScenario(void);

      // create multiple scenarios, based on the specified scenario, given
      // the factor name and 1 or more factor values.
      void createScenariosFrom(ScenarioContainer::iterator posn,
                               const std::string& factorName,
                               const std::vector<std::string>& factorValues);

      // load all add-in DLLs
      void loadAllAddIns(void);

      // Make sure all scenario names is valid and reflects the factor values.
      void makeScenarioNamesValid(void);

      // ensure scenario is completely valid.
      void makeScenarioValid(Scenario& scenario, const string& factorName);
      bool isScenarioValid(Scenario& scenario);

};



//---------------------------------------------------------------------------
#endif
