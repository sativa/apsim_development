//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Scenarios.h"
#include "Scenario.h"
#include "AddIn.h"
#include <general\path.h>
#include <general\stl_functions.h>
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDirectories.h>
#include <list>
using namespace std;
static const char* SCENARIOS_KEY = "Saved Scenarios";

#pragma package(smart_init)

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Scenarios::Scenarios(void)
   {
   loadAllAddIns();
   makeDefaultScenario();
   }
// ------------------------------------------------------------------
// load all add-ins
// ------------------------------------------------------------------
void Scenarios::loadAllAddIns(void)
   {
   // get a list of add-in filenames from the .ini file.
   ApsimSettings settings;
   vector<string> fileNames;
   settings.read("Outlook Addins|addin", fileNames);

   // Loop through all filenames, add strip off any parameters.
   vector<string> parameters;
   for (vector<string>::iterator fileName = fileNames.begin();
                                 fileName != fileNames.end();
                                 fileName++)
      {
      // look for add in parameters after a space.
      unsigned int posSpace = fileName->find(' ');
      if (posSpace == string::npos)
         parameters.push_back("");
      else
         {
         parameters.push_back(fileName->substr(posSpace+1));
         fileName->erase(posSpace);
         }
      }
   try
   {
      addIns.load(fileNames);
   }
   catch (const exception& e)
   {
      MessageDlg(e.what(), mtWarning, TMsgDlgButtons() << mbOK, 0);
   }
   // loop through all addins and pass any add-in parameters.
   for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
      {
      AddInBase* addin = addIns.getAddIn(addinIndex);
      addin->setStartupParameters(parameters[addinIndex]);
      }
   }
// ------------------------------------------------------------------
// make a default scenario.
// ------------------------------------------------------------------
void Scenarios::makeDefaultScenario(void)
   {
   Scenario defaultScenario;

   // Loop through all add-ins, and build up a default scenario.
   for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
      defaultScenario = defaultScenario + addIns.getAddIn(addinIndex)->getDefaultScenario();

   // if there is a factor called "simulation" then use that as the default name.
   defaultScenario.setName(defaultScenario.getFactorValue("simulation"));
   if (defaultScenario.getName() == "")
      defaultScenario.setName("default");
   scenarios.push_back(defaultScenario);
   }
// ------------------------------------------------------------------
// Return a list of possible factor values for the given
// scenario and factor name.
// ------------------------------------------------------------------
void Scenarios::getFactorValues(const string& scenarioName,
                                const string& factorName,
                                vector<string>& factorValues) const
   {
   ScenarioContainer::const_iterator scenario = find(scenarios.begin(),
                                                     scenarios.end(),
                                                     scenarioName);
   if (scenario != scenarios.end())
      {
      // Let each addin add factorValues for the given factor name.
      // In reality one 1 addin will respond.
      for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
         addIns.getAddIn(addinIndex)->getFactorValues(*scenario, factorName, factorValues);
      }
   }
// ------------------------------------------------------------------
// Return a list of factor names for the specified scenario.
// ------------------------------------------------------------------
void Scenarios::getFactorNames(const string& scenarioName,
                               vector<string>& names) const
   {
   ScenarioContainer::const_iterator scenario = find(scenarios.begin(),
                                                     scenarios.end(),
                                                     scenarioName);
   if (scenario != scenarios.end())
      scenario->getFactorNames(names);
   }
// ------------------------------------------------------------------
// Return a factor value to caller.
// ------------------------------------------------------------------
string Scenarios::getFactorValue(const string& scenarioName,
                                 const string& factorName) const
   {
   ScenarioContainer::const_iterator scenario = find(scenarios.begin(),
                                                     scenarios.end(),
                                                     scenarioName);
   if (scenario != scenarios.end())
      return scenario->getFactorValue(factorName);
   else
      return "";
   }
// ------------------------------------------------------------------
// Rename the specified scenario.
// ------------------------------------------------------------------
void Scenarios::renameScenario(const string& oldName, const string& newName)
   {
   ScenarioContainer::iterator scenario = find(scenarios.begin(),
                                               scenarios.end(),
                                               oldName);
   if (scenario != scenarios.end())
      scenario->setName(newName);
   }
// ------------------------------------------------------------------
// Delete a scenario.
// ------------------------------------------------------------------
void Scenarios::deleteScenario(const string& scenarioName)
   {
   ScenarioContainer::iterator scenario = find(scenarios.begin(),
                                               scenarios.end(),
                                               scenarioName);
   if (scenario != scenarios.end())
      scenarios.erase(scenario);

   if (scenarios.size() == 0)
      makeDefaultScenario();
   }

// ------------------------------------------------------------------
// Move a scenario and insert it before the specified dest scenario.
// ------------------------------------------------------------------
void Scenarios::moveScenario(const string& sourceName, const string& destName)
   {
   ScenarioContainer::iterator i = find(scenarios.begin(), scenarios.end(),
                                        sourceName);
   if (i != scenarios.end())
      {
      Scenario scenarioToMove = *i;
      scenarios.erase(i);

      i = find(scenarios.begin(), scenarios.end(), destName);
      if (i != scenarios.end())
         {
         scenarios.insert(i, scenarioToMove);
         }
      }
   }
// ------------------------------------------------------------------
// Clear all scenarios.
// ------------------------------------------------------------------
void Scenarios::deleteAllScenarios(void)
   {
   scenarios.erase(scenarios.begin(), scenarios.end());
   makeDefaultScenario();
   }
// ------------------------------------------------------------------
// return a list of scenario names to caller.
// ------------------------------------------------------------------
void Scenarios::getScenarioNames(vector<string>& scenarioNames) const
   {
   for_each(scenarios.begin(), scenarios.end(),
            GetName<Scenario>(scenarioNames));
   }
// ------------------------------------------------------------------
// create multiple scenarios, based on the given scenario, given
// the factor name and 1 or more factor values.
// ------------------------------------------------------------------
void Scenarios::createScenariosFrom(const string& scenarioName,
                                    const string& factorName,
                                    const vector<string>& factorValues)
   {
   ScenarioContainer::iterator scenario = find(scenarios.begin(),
                                               scenarios.end(),
                                               scenarioName);
   if (scenario != scenarios.end())
      createScenariosFrom(scenario, factorName, factorValues);
   makeScenarioNamesValid();
   }
// ------------------------------------------------------------------
// create multiple scenarios, based on the given scenario, given
// the factor name and 1 or more factor values.
// ------------------------------------------------------------------
void Scenarios::createScenariosFrom(ScenarioContainer::iterator posn,
                                    const string& factorName,
                                    const vector<string>& factorValues)
   {
   Scenario copiedScen(*posn);
   if (factorValues.size() >= 1)
      {
      posn->setFactorValue(factorName, factorValues[0]);
      for (unsigned factorI = 1; factorI != factorValues.size(); factorI++)
         {
         Scenario newScenario(copiedScen);
         newScenario.setFactorValue(factorName, factorValues[factorI]);
         newScenario.setName("");
         makeScenarioValid(newScenario, factorName);
         posn++;
         posn = scenarios.insert(posn, newScenario);
         }
      }
   }
// ------------------------------------------------------------------
// Create a permutation of scenarios based on the given
// scenario, the factor name passed in and 1 or more values for
// that factor.
// ------------------------------------------------------------------
void Scenarios::createScenarioPermutation(const std::string& scenarioName,
                                          const string& factorName,
                                          const vector<string>& factorValues)
   {
   ScenarioContainer::iterator scenario = find(scenarios.begin(),
                                               scenarios.end(),
                                               scenarioName);
   if (scenario != scenarios.end())
      {
      // get a list of the current scenario names.
      vector<string> scenarioNames;
      getScenarioNames(scenarioNames);

      // loop through all our current scenario names and create scenarios from it.
      for (vector<string>::iterator name = scenarioNames.begin();
                                    name != scenarioNames.end();
                                    name++)
         {
         ScenarioContainer::iterator scenarioToCopy = find(scenarios.begin(),
                                                           scenarios.end(),
                                                           *name);
         if (scenarioToCopy != scenarios.end())
             createScenariosFrom(scenarioToCopy, factorName, factorValues);
         }
      makeScenarioNamesValid();
      }
   }
// ------------------------------------------------------------------
// return all data.
// ------------------------------------------------------------------
void Scenarios::getAllData(TAPSTable* data)
   {
   data->beginStoringData();
   for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
      {
      for (ScenarioContainer::iterator scenario = scenarios.begin();
                                       scenario != scenarios.end();
                                       scenario++)
         addIns.getAddIn(addinIndex)->doCalculations(*data, *scenario);
      }
   data->endStoringData();
   }
// ------------------------------------------------------------------
// Small struct using in makeScenarioNamesValid method.
// ------------------------------------------------------------------
struct FactorInfo
   {
   FactorInfo(const string& n, const string& v)
      : name(n), value(v), isDifferent(false) { }
   void setValue(const string& v)
      {
      if (value != v)
         isDifferent = true;
      }
   bool operator==(const string& n)
      {return name == n;}
   string name;
   string value;
   bool isDifferent;
   };

// ------------------------------------------------------------------
// Make sure all scenario names are valid and reflect the factor values.
// ------------------------------------------------------------------
void Scenarios::makeScenarioNamesValid(void)
   {
   typedef vector<FactorInfo> FactorInfos;
   FactorInfos allFactors;

   // Create a container of factor names and values and whether they are
   // different between more than one scenario.
   for (ScenarioContainer::iterator scenario = scenarios.begin();
                                    scenario != scenarios.end();
                                    scenario++)
      {
      vector<string> factorNames;
      scenario->getFactorNames(factorNames);
      for (vector<string>::iterator factorName = factorNames.begin();
                                    factorName != factorNames.end();
                                    factorName++)
         {
         string factorValue = scenario->getFactorValue(*factorName);

         FactorInfos::iterator i = find(allFactors.begin(),
                                        allFactors.end(),
                                        *factorName);
         if (i == allFactors.end())
            allFactors.push_back(FactorInfo(*factorName, factorValue));
         else
            i->setValue(factorValue);
         }
      }

   // Loop through all scenarios and change name to reflect all factors in
   // our container of factors.
   for (ScenarioContainer::iterator scenario = scenarios.begin();
                                    scenario != scenarios.end();
                                    scenario++)
      {
      string newName;
      for (FactorInfos::iterator factor = allFactors.begin();
                                 factor != allFactors.end();
                                 factor++)
         {
         if (factor->isDifferent)
            {
            if (newName != "")
               newName += ";";
            newName += factor->name + "=" + scenario->getFactorValue(factor->name);
            }
         }
      if (newName != "")
         scenario->setName(newName);
      }
   }
//---------------------------------------------------------------------------
// save the current state to the .ini file.
//---------------------------------------------------------------------------
void Scenarios::save(const string& name)
   {
   settings.refresh();
   // loop through all scenarios and tell each to write its state to the
   // specified string.
   vector<string> scenarioStates;
   for (ScenarioContainer::const_iterator scenario = scenarios.begin();
                                          scenario != scenarios.end();
                                          scenario++)
      scenarioStates.push_back(scenario->getState());
   settings.write("Outlook Scenario " + name + "|scenario", scenarioStates);
   }
//---------------------------------------------------------------------------
// restore the previously saved state from the .ini file.
//---------------------------------------------------------------------------
void Scenarios::restore(const string& name)
   {
   settings.refresh();
   vector<string> scenarioStates;
   settings.read("Outlook Scenario " + name + "|scenario", scenarioStates);
   if (scenarioStates.size() > 0)
      scenarios.erase(scenarios.begin(), scenarios.end());


   for (vector<string>::iterator state = scenarioStates.begin();
                                 state != scenarioStates.end();
                                 state++)
      {
      try
         {
         Scenario newScenario(*state);
         if (isScenarioValid(newScenario))
            scenarios.push_back(newScenario);
         }
      catch (const runtime_error& error)
         {
         }
      }
   if (scenarios.size() == 0)
      makeDefaultScenario();
   }
//---------------------------------------------------------------------------
// ensure the specified scenario is completely valid.
//---------------------------------------------------------------------------
void Scenarios::makeScenarioValid(Scenario& scenario,
                                  const string& factorName)
   {
   for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
      addIns.getAddIn(addinIndex)->makeScenarioValid(scenario, factorName);
   }
//---------------------------------------------------------------------------
// ensure all scenarios are completely valid.
//---------------------------------------------------------------------------
void Scenarios::makeScenariosValid(void)
   {
   for (ScenarioContainer::iterator s = scenarios.begin(); s != scenarios.end(); s++)
      {
      for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
         addIns.getAddIn(addinIndex)->makeScenarioValid(*s, "");
      }
   }
//---------------------------------------------------------------------------
// return true if the specified scenario is valid.
//---------------------------------------------------------------------------
bool Scenarios::isScenarioValid(Scenario& scenario)
   {
   for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
      {
      if (!addIns.getAddIn(addinIndex)->isScenarioValid(scenario))
         return false;
      }
   return true;
   }
//---------------------------------------------------------------------------
// Allow the add-ins to display information in the settings window.
//---------------------------------------------------------------------------
std::string Scenarios::getDisplaySettings(void)
   {
   string settingsString;
   for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
      settingsString += addIns.getAddIn(addinIndex)->getDisplaySettings();
   return settingsString;
   }
//---------------------------------------------------------------------------
// Allow the add-ins to display a UI form if they want to.
//---------------------------------------------------------------------------
std::string Scenarios::getUIButtonCaption(void) const
   {
   string captionString;
   for (unsigned addinIndex = 0;
                 addinIndex != addIns.getNumAddIns() && captionString == "";
                 addinIndex++)
      captionString = addIns.getAddIn(addinIndex)->getUIButtonCaption();
   return captionString;
   }
//---------------------------------------------------------------------------
// Allow the add-ins to display information in the settings window.
//---------------------------------------------------------------------------
void Scenarios::showUI(void)
   {
   for (unsigned addinIndex = 0; addinIndex != addIns.getNumAddIns(); addinIndex++)
      addIns.getAddIn(addinIndex)->showUI();
   }

