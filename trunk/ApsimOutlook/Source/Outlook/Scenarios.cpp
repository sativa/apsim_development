//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Scenarios.h"
#include "Scenario.h"
#include "AddIn.h"
#include <general\path.h>
#include <general\stl_functions.h>
#include <general\ini_file.h>
#include <list>
using namespace std;

//---------------------------------------------------------------------------

#pragma package(smart_init)

Scenarios::Scenarios(void) {
   loadAllAddIns();
   makeDefaultScenario();
}


Scenarios::~Scenarios() {
   bool leaveDefault = false;
   deleteAllScenarios(leaveDefault);
   while (!addIns.empty()) {
      AddInBase* ptr = addIns.back();
      addIns.pop_back();
      delete ptr;
      }
   while (!dllHandles.empty()) {
      HINSTANCE dllHandle = dllHandles.back();
//      FreeLibrary(dllHandle);
      dllHandles.pop_back();
      }
}


void Scenarios::makeDefaultScenario(void) {
   vector<Factor> allFactors;

   // Loop through all add-ins, get a default scenario from each
   // and then take a copy of the factors.
   for (AddInContainer::iterator a = addIns.begin();
                                 a != addIns.end();
                                 a++){
      Scenario scenario = (*a)->getDefaultScenario();
      scenario.getFactors(allFactors);
   }

   // create a new scenario that contains all the factors, add it to our
   // list of scenarios and make it the current scenario.
   Scenario* defaultScenario = new Scenario("default", allFactors);
   scenarios.push_back(defaultScenario);
   currentScenario = defaultScenario;
}

void Scenarios::getFactorValues(const string& factorName,
                                vector<string>& factorValues) const
{
   currentScenario->getFactorValues(factorName, factorValues);
}

void Scenarios::setCurrentScenario(const string& scenarioName) {
   // find the 'name' in the list of names, get that Scenario and set it to current
   ScenarioContainer::iterator s = find_if(scenarios.begin(),
                                           scenarios.end(),
                                           PEqualToName<Scenario>(scenarioName));
   if (s != scenarios.end())
      currentScenario = *s;
}
string Scenarios::getCurrentScenario(void) {
   return currentScenario->getName();
}

void Scenarios::getFactorNames(vector<string>& names) const {
   currentScenario->getFactorNames(names);
}

void Scenarios::renameCurrentScenario(const string& newName) {
   currentScenario->setName(newName);
}


void Scenarios::deleteCurrentScenario() {
   // Locate the current scenario, remove it from our list of scenarios,
   // delete the object and assign the currentScenario to the next one
   // in the list.
   ScenarioContainer::iterator current_s
                  = find(scenarios.begin(), scenarios.end(), currentScenario);
   ScenarioContainer::iterator next = scenarios.erase(current_s);
   delete currentScenario;

   if (next == scenarios.end())  // then we were are past the end and should set Current
                                 // scenario to point to a valid Scenario
      currentScenario = scenarios.back();
   else if (scenarios.size() == 0)   // then Scens is empty and we need to make a default
                                 // Scenario and set Current_scenario to point to it
      makeDefaultScenario();
   else
      currentScenario = *next;
}


void Scenarios::deleteAllScenarios(bool leaveDefaultScenario) {
   while (!scenarios.empty()) {
      Scenario* ptr = scenarios.back();
      scenarios.pop_back();
      delete ptr;
   }
   if (leaveDefaultScenario)
      makeDefaultScenario();
}

// ------------------------------------------------------------------
//  Short description:
//    return a list of scenario names to caller.

//  Notes:

//  Changes:
//    dph 4/4/01

// ------------------------------------------------------------------
void Scenarios::getScenarioNames(vector<string>& scenarioNames) const
   {
   for_each(scenarios.begin(), scenarios.end(),
            PGetNameFunction< vector<string>, Scenario>(scenarioNames));
   }

// ------------------------------------------------------------------
//  Short description:
//    return attributes for a given factor.

//  Notes:

//  Changes:
//    dph 4/4/01

// ------------------------------------------------------------------
void Scenarios::getFactorAttributes(const std::string& factorName,
                                    std::string& factorValue,
                                    Graphics::TBitmap*& factorBitmap)
   {
   currentScenario->getFactorAttributes(factorName, factorValue, factorBitmap);
   }

// ------------------------------------------------------------------
//  Short description:
//    create multiple scenarios, based on the current scenario, given
//    the factor name and 1 or more factor values.

//  Notes:

//  Changes:
//    DPH 29/6/98
//    dph 4/4/01 moved from drilldownform to Scenarios.

// ------------------------------------------------------------------
void Scenarios::createScenariosFromCurrent(const string& factorName,
                                           const vector<string>& factorValues)
   {
   if (factorValues.size() == 1)
      currentScenario->setFactorValue(factorName, *(factorValues.begin()));
   else
      {
      createMultipleScenariosFrom(*currentScenario, factorName, factorValues);
      deleteCurrentScenario();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Create a permutation of scenarios based on the current selected
//    scenario, the factor name passed in and 1 or more values for
//    that factor.

//  Notes:

//  Changes:
//    DPH 29/6/98
//    dph 4/4/01 moved from drilldownform to Scenarios.

// ------------------------------------------------------------------
void Scenarios::createScenarioPermutation(const string& factorName,
                                          const vector<string>& factorValues)
   {
   // get a list of the current scenario names.
   vector<string> scenarioNames;
   getScenarioNames(scenarioNames);

   // loop through all our current scenario names, make it the current
   // scenario and create scenarios from it.
   for (vector<string>::iterator n = scenarioNames.begin();
                                 n != scenarioNames.end();
                                 n++)
      {
      setCurrentScenario(*n);
      createScenariosFromCurrent(factorName, factorValues);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    create a series of scenarios based on the specified scenario,
//    the specified factor and 1 or more factor values.

//  Notes:

//  Changes:
//    DPH 29/6/98
//    dph 8/12/99 added identifier to simulation name - c227,d308
//    dph 4/4/01 moved from drilldownform to Scenarios.

// ------------------------------------------------------------------
void Scenarios::createMultipleScenariosFrom(const Scenario& scenario,
                                            const string& factorName,
                                            const vector<string>& factorValues)
   {
   // loop through all factor values and create a new scenario
   // for each.
   for (vector<string>::const_iterator v = factorValues.begin();
                                       v != factorValues.end();
                                       v++)
      {
      Scenario* newScenario = new Scenario(*currentScenario);
      newScenario->setFactorValue(factorName, *v);

      if (factorValues.size() > 1)
         {
         string newName = newScenario->getName();
         if (Str_i_Eq(newName, "default"))
            newName = "";
         else
            newName = newName + ";";
         newName += string(factorName) + "=" + *v;
         newScenario->setName(newName.c_str());
         }
      scenarios.push_back(newScenario);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    load all add-in DLLs

//  Changes:
//    dph 4/4/01

// ------------------------------------------------------------------
void Scenarios::loadAllAddIns(void)
   {
   // get a list of add-in filenames from the .ini file.
   Path iniPath(Application->ExeName.c_str());
   iniPath.Set_extension(".ini");

   Ini_file ini;
   ini.Set_file_name(iniPath.Get_path().c_str());
   list<string> addInFileNames;
   ini.Read_list("Addins", "addin", addInFileNames);

   // Loop through all filenames, load the DLL, call the DLL to create an
   // instance of an AddInBase and store in our list of addins.
   for (list<string>::iterator a = addInFileNames.begin();
                               a != addInFileNames.end();
                               a++)
      {
      // look for add in parameters after a space.
      unsigned int posSpace = (*a).find(" ");
      string addInParameters;
      if (posSpace != string::npos)
         {
         addInParameters = (*a).substr(posSpace+1);
         (*a).erase(posSpace);
         }
      HINSTANCE dllHandle = LoadLibrary( (*a).c_str() );
      if (dllHandle != NULL)
         {
         dllHandles.push_back(dllHandle);

         AddInBase* __stdcall (*createAddInProc) (const string& addInParameters);
         (FARPROC) createAddInProc = GetProcAddress(dllHandle, "createAddIn");
         if (createAddInProc != NULL)
            addIns.push_back( (*createAddInProc)(addInParameters) );
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    return all data.

//  Changes:
//    dph 4/4/01

// ------------------------------------------------------------------
void Scenarios::getAllData(TAPSTable* data)
   {
   for (AddInContainer::iterator a = addIns.begin();
                                 a != addIns.end();
                                 a++)
      {
      (*a)->doCalculations(*data, scenarios);
      }
   }


// ------------------------------------------------------------------
//  Short description:
//    Get a value selection form relevant to the requested factor_name

//  Changes:
//    dph 4/4/01

// ------------------------------------------------------------------

TValueSelectionForm*  Scenarios::getUIForm(const string& factor_name, TComponent* Owner)
   {
      return currentScenario->getUIForm(factor_name, Owner);
   }
