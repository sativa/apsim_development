//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "DataBaseAddIn.h"
#include <general\stl_functions.h>
#include <general\inifile.h>
#include <general\path.h>
#include <dialogs.hpp>
#include <assert.h>
#include <sstream>
#include "TDirectory_select_form.h"

#define ECON_FACTOR_NAME "Econ Config"

using namespace std;
// ------------------------------------------------------------------
// Exported function for created an instance of this add-in
// ------------------------------------------------------------------
extern "C" AddInBase* _export __stdcall createAddIn(void)
   {
   return new DatabaseAddIn();
   }
// ------------------------------------------------------------------
// Exported function to delete the specified addin.
// ------------------------------------------------------------------
extern "C" void _export __stdcall deleteAddIn(AddInBase* addin)
   {
   delete addin;
   }

// ------------------------------------------------------------------
//  Short description:
//    create a lookup partial title that is based on the scenario
//    passed in and the specified factor name.  This is used to
//    limit the search for factorValues (getFactorValues) and to
//    make a scenario valid (makeScenarioValid).

//  Notes:
//    The upToButNotIncluding flag controls whether the returned partial
//    title contains all factors up to and including the factor name
//    passed in.

//  Changes:
//    DPH 5/2/98
//    dph 4/4/01 reworked for new Add-in.

// ------------------------------------------------------------------
string createPartialTitle(const Scenario& scenario,
                          const string& factorName,
                          bool upToButNotIncluding = true)
   {
   string partialTitle;
   vector<string> factorNames;
   scenario.getFactorNames(factorNames);
   for (vector<string>::iterator f = factorNames.begin();
                                 f != factorNames.end();
                                 f++)
      {
      if (upToButNotIncluding &&  (*f) == factorName)
         return partialTitle;
      string factorValue = scenario.getFactorValue(*f);
      if (partialTitle.length() > 0)
         partialTitle += ";";
      if (*f == "Simulation")
         partialTitle += factorValue;
      else
         partialTitle += *f + "=" + factorValue;
      if (!upToButNotIncluding &&  (*f) == factorName)
         return partialTitle;
      }
   return partialTitle;
   }

// ------------------------------------------------------------------
//  Short description:
//    A function class to call the getFactorValue method of each
//    simulation passed in.

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
class GetFactorValue
   {
   private:
      vector<string>& Container;
      string factorName;
      const char* partialTitle;

   public:
      GetFactorValue(vector<string>& container,
                     string factorname,
                     const string& partialtitle)
         : Container (container), factorName(factorname), partialTitle(partialtitle.c_str())
         { }

      void operator () (const DBSimulation* simulation) const
         {
         if (simulation->partialTitleCompare(partialTitle))
            {
            string factorValue = simulation->getFactorValue(factorName);
            if (factorValue != "" &&
                find(Container.begin(), Container.end(), factorValue) == Container.end())
               Container.push_back(factorValue);
            }
         };
   };
// ------------------------------------------------------------------
//  Short description:
// set any startup parameters.

//  Notes:

//  Changes:
//    DPH 24/9/98

// ------------------------------------------------------------------
void DatabaseAddIn::setStartupParameters(const std::string& parameters)
   {
   if (parameters == "")
      askUserForMDBs();
   else if (parameters.find("datasetsON") != string::npos)
      askUserForDataSets();
   else
      {
      vector<string> filenames;
      Split_string(parameters, " ", filenames);
      readAllDatabases(filenames);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
DatabaseAddIn::~DatabaseAddIn(void)
   {
   delete_container(simulations);
   }

// ------------------------------------------------------------------
//  Short description:
//    get a list of MDB's from user.

//  Notes:

//  Changes:
//    DPH 24/9/98

// ------------------------------------------------------------------
void DatabaseAddIn::askUserForMDBs()
   {
   TOpenDialog* OpenDialog = new TOpenDialog(NULL);
   OpenDialog->Filter = "MDB files (*.mdb)|*.MDB";
   OpenDialog->DefaultExt = "mdb";
   OpenDialog->Options << ofAllowMultiSelect;
   if (OpenDialog->Execute())
      {
      vector<string> databaseFilenames;
      TStrings_2_stl(OpenDialog->Files, databaseFilenames);
      readAllDatabases(databaseFilenames);
      }
   else
      throw runtime_error("User hit cancel.");
   }



void DatabaseAddIn::askUserForDataSets(void)
   {
   Directory_select_form = new TDirectory_select_form(Application->MainForm);
   if (Directory_select_form->ShowModal() == mrOk)
      {
      vector<string> databaseFilenames;
      TStrings_2_stl(Directory_select_form->SelectedMDBs, databaseFilenames);
      readAllDatabases(databaseFilenames);
      }
   else
      {
      delete Directory_select_form;
      throw runtime_error("User hit cancel.");
      }
   delete Directory_select_form;
   }



// ------------------------------------------------------------------
//  Short description:
//    read in all simulations for the specified databases.

//  Notes:

//  Changes:
//    DPH 24/9/98
//    DPH 4/4/2001 - moved from TSimulations_from_mdb.cpp

// ------------------------------------------------------------------
void DatabaseAddIn::readAllDatabases(vector<string>& databaseFilenames)
   {
   delete_container(simulations);

   for (vector<string>::iterator db = databaseFilenames.begin();
                                 db != databaseFilenames.end();
                                 db++)
      readDatabase(*db);
   }

// ------------------------------------------------------------------
//  Short description:
//    read contents of a single database

//  Notes:

//  Changes:
//    DPH 24/9/98
//    DAH 10/8/2000 - saved and restored cursor rather than setting it to crArrow
//    DPH 4/4/2001 - moved from TSimulations_from_mdb.cpp

// ------------------------------------------------------------------
void DatabaseAddIn::readDatabase(const string& databaseFileName)
   {
   // show hourglass.
   TCursor Saved_cursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   // open the database.
   TADOConnection* db = new TADOConnection(NULL);
   string connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;"
                             "Data Source=####;"
                             "Persist Security Info=False";
   Replace_all(connectionString, "####", databaseFileName.c_str());
   db->ConnectionString = connectionString.c_str();
   db->LoginPrompt = false;
   db->Mode = cmShareExclusive;
   db->Connected = true;

   // Open the index table.
   TADOTable* indexTable = new TADOTable(NULL);
   indexTable->Connection = db;
   indexTable->TableName = "[Index]";
   indexTable->CursorLocation = clUseServer;
   indexTable->Open();

   // loop through all records in the index table.  For each, create a
   // simulation object, get it to read itself in and then store in list.
   DBSimulation* newSimulation;
   while (!indexTable->Eof)
      {
      newSimulation = new DBSimulation();
      newSimulation->readFromIndex(databaseFileName, indexTable);
      simulations.push_back(newSimulation);
      indexTable->Next();
      }

   // cleanup
   delete indexTable;
   delete db;

   // show hourglass.
   Screen->Cursor = Saved_cursor;
   }

// ------------------------------------------------------------------
//  Short description:
//    return a default scenario to caller.

//  Notes:

//  Changes:
//    DPH 4/4/01

// ------------------------------------------------------------------
Scenario DatabaseAddIn::getDefaultScenario(void) const
   {
   if (simulations.size() > 0)
      return convertSimulationToScenario(*(simulations[0]), "", "");

   else
      {
      vector<Factor> dummy;
      return Scenario("", dummy);
      }
   }
// ------------------------------------------------------------------
// return true if the simulation is valid.  False otherwise.
// ------------------------------------------------------------------
bool DatabaseAddIn::isScenarioValid(Scenario& scenario) const
   {
   string titleToFind = createPartialTitle(scenario, ECON_FACTOR_NAME, true);

   SimulationContainer::const_iterator sim = Pfind(simulations.begin(),
                                                   simulations.end(),
                                                   titleToFind);
   return (sim != simulations.end());
   }
// ------------------------------------------------------------------
//  Short description:
//    make the scenario passed in a valid one.  This may mean adding
//    extra factors to the scenario or changing existing factors.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    dph 4/4/01 reworked for new Add-in.

// ------------------------------------------------------------------
void DatabaseAddIn::makeScenarioValid(Scenario& scenario, const std::string factorName) const
   {
   string econFactorValue = scenario.getFactorValue(ECON_FACTOR_NAME);

   string titleToFind = createPartialTitle(scenario, factorName, false);
   SimulationContainer::const_iterator closestSimulation = simulations.begin();
   unsigned int closestRank = 0;
   for (SimulationContainer::const_iterator  s = simulations.begin();
                                             s != simulations.end() && closestRank != 100000;
                                             s++)
      {
      unsigned int rank = (*s)->calculateRank(titleToFind);
      if (rank > closestRank)
         {
         closestRank = rank;
         closestSimulation = s;
         }
      }
   // try and make this simulation even closer by substituting factor values
   // AFTER the factorName passed in, from the scenario passed in
   // into this closestSimulation.
   vector<string> factorsNamesToFind, closestFactorNames;
   scenario.getFactorNames(factorsNamesToFind);

   (*closestSimulation)->getFactorNames(closestFactorNames);
   vector<string>::const_iterator f = find(closestFactorNames.begin(),
                                           closestFactorNames.end(),
                                           factorName);
   if (f != closestFactorNames.end())
      f++;
   while (f != closestFactorNames.end())
      {
      // see if this factor is one we're supposed to find.
      vector<string>::const_iterator factorToFind = find(factorsNamesToFind.begin(),
                                                         factorsNamesToFind.end(),
                                                         *f);
      bool closerSimulationWasFound = false;
      if (factorToFind != factorsNamesToFind.end())
         {
         string titleToTry = titleToFind + ";" + *f + "=" + scenario.getFactorValue(*f);
         SimulationContainer::const_iterator closerSimulation = find_if
            (simulations.begin(), simulations.end(), PartialTitleFindIf(titleToTry));
         if (closerSimulation != simulations.end())
            {
            closestSimulation = closerSimulation;
            closerSimulationWasFound = true;
            titleToFind = titleToTry;
            }
         }
      if (!closerSimulationWasFound)
         titleToFind += ";" + *f + "=" + scenario.getFactorValue(*f);
      f++;
      }
   scenario = convertSimulationToScenario(**closestSimulation,
                                          scenario.getName(),
                                          econFactorValue);
   }
// ------------------------------------------------------------------
//  Short description:
//      return a list of possible legal values for the specified identifier

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void DatabaseAddIn::getFactorValues(const Scenario& scenario,
                                    const string& factorName,
                                    vector<string>& factorValues) const
   {
   for_each(simulations.begin(), simulations.end(),
            GetFactorValue(factorValues, factorName,
                           createPartialTitle(scenario, factorName)));
   }

// ------------------------------------------------------------------
//  Short description:
//    given the source data object, and the list of user selected
//    scenarios, perform all calculations and store all new data
//    in the returned TAPSTable.

//  Changes:
//    DPH 5/2/98
//    dph 8/12/1999 replaced call to get_desciptor_field_names with a
//                  call to get_name.  We no longer want specially created
//                  descriptor names. c227,d308
//    DAH 10/8/2000 saved and restored cursor
//    dph 5/4/2001 pulled from the old TSelected_simulations.cpp and modified

// ------------------------------------------------------------------
void DatabaseAddIn::doCalculations(TAPSTable& data, const Scenario& scenario)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   string title = createPartialTitle(scenario, ECON_FACTOR_NAME);
   vector<DBSimulation*>::iterator s = Pfind(simulations.begin(),
                                             simulations.end(),
                                             title);
   assert (s != simulations.end());
   (*s)->readData(data, scenario.getName());

   Screen->Cursor = savedCursor;
   }

// ------------------------------------------------------------------
//  Short description:
//    convert the specified simulation into a scenario.

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
Scenario DatabaseAddIn::convertSimulationToScenario
   (const DBSimulation& simulation,
    const std::string& scenarioName,
    const std::string& econFactorValue) const
   {
   vector<string> names;
   vector<Factor> factors;
   simulation.getFactorNames(names);
   for (unsigned i = 0; i != names.size(); i++)
      factors.push_back(Factor(names[i], simulation.getFactorValue(names[i])));
   if (econFactorValue != "")
      factors.push_back(Factor(ECON_FACTOR_NAME, econFactorValue));

   return Scenario(scenarioName, factors);
   }

