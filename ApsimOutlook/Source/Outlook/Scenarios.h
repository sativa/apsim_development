//---------------------------------------------------------------------------

#ifndef ScenariosH
#define ScenariosH

#include <graphics.hpp>
#include <TAPSTable.h>
class Scenario;
class AddInBase;

// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a collection of scenarios, keeping
//      track of a current scenario.  It also manages a collection
//      of add-in's which supply the possible scenario objects.

//  Notes:
//      We should probably create an add-in manager class which
//      encapsulates a collection of add-in's.  An instance of
//      this class could then be passed into this constructor.

//  Changes:
//    DPH 4/4/01 - modified slightly from Dene Hughes.

// ------------------------------------------------------------------
class Scenarios {
   public:
      Scenarios(void);

      ~Scenarios();

      // return the number of scenarios.
      unsigned int count(void) {return scenarios.size();}

      // create multiple scenarios, based on the current scenario, given
      // the factor name and 1 or more factor values.
      void createScenariosFromCurrent(const std::string& factorName,
                                      const std::vector<std::string>& factorValues);

      // Create a permutation of scenarios based on the current selected
      // scenario, the factor name passed in and 1 or more values for
      // that factor.
      void createScenarioPermutation(const std::string& factorName,
                                     const std::vector<std::string>& factorValues);

      // return a list of scenario names to caller.
      void getScenarioNames(std::vector<std::string>& scenarioNames) const;

      // return a list of factor names for current scenario.
      void getFactorNames(std::vector<std::string>& factorNames) const;

      // return attributes for a given factor for the current scenario.
      void getFactorAttributes(const std::string&  factorName,
                               std::string&        factorValue,
                               Graphics::TBitmap*& factorBitmap);

      // Get a list of factor values for the current scenario and the
      // specified factor.
      void getFactorValues(const std::string& factor_name, std::vector<std::string>& factorValues) const;

      // Get and set the current scenario.
      void        setCurrentScenario(const std::string& name);
      std::string getCurrentScenario(void);

      // Rename the current scenario.
      void renameCurrentScenario(const std::string& newName);

      // Delete the current scenario.
      void deleteCurrentScenario();

      // Delete all scenarios.
      void deleteAllScenarios();

      void getAllData(TAPSTable* table);

   private:
      typedef std::vector<Scenario*>            ScenarioContainer;
      typedef std::vector<AddInBase*>           AddInContainer;
      typedef std::vector<HINSTANCE>            HINSTANCEContainer;

      ScenarioContainer scenarios;
      AddInContainer addIns;
      HINSTANCEContainer dllHandles;
      Scenario* currentScenario;

      // prevent the following functions from being auto-generated:
      Scenarios(const Scenarios& rhs);
      Scenarios& operator=(const Scenarios& rhs);

      // create a default scenario.
      void makeDefaultScenario(void);

      // create a series of scenarios based on the specified scenario,
      // the specified factor and 1 or more factor values.
      void createMultipleScenariosFrom(const Scenario& scenario,
                                       const std::string& factorName,
                                       const std::vector<std::string>& factorValues);

      // load all add-in DLLs
      void loadAllAddIns(void);
};



//---------------------------------------------------------------------------
#endif
