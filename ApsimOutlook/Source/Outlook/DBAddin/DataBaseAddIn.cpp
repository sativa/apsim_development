//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "DataBaseAddIn.h"
#include <general\stl_functions.h>
#include <general\ini_file.h>
#include <general\path.h>
#include <dialogs.hpp>
#include <assert.h>
#include <sstream>
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
extern "C" AddInBase* _export __stdcall createAddIn(const string& parameters)
   {
   return new DatabaseAddIn(parameters);
   }

// ------------------------------------------------------------------
//  Short description:
//    A function class to call the getRank method of all
//    each simulation and then keep track of the highest rank.

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
class FindClosestScenario
   {
   private:
      vector<string> factorNames;
      vector<string> factorValues;
      const DBSimulation* closestSimulation;
      unsigned int closestRank;

   public:
      FindClosestScenario(const Scenario& scenarioToMatch)
         : closestRank(0),
           closestSimulation(NULL)
           {
           scenarioToMatch.getFactorNames(factorNames);
           for (vector<string>::iterator f = factorNames.begin();
                                         f != factorNames.end();
                                         f++)
              {
              string value;
              Graphics::TBitmap* bitmap;
              scenarioToMatch.getFactorAttributes(*f, value, bitmap);
              factorValues.push_back(value);
              }
           }

      void operator () (const DBSimulation& simulation)
         {
         unsigned int rank = simulation.calculateRank(factorNames, factorValues);
         if (rank > closestRank)
            {
            closestRank = rank;
            closestSimulation = &simulation;
            }
         };

      const DBSimulation* getClosestSimulation(void)
         {
         assert(closestSimulation != NULL);
         return closestSimulation;
         }
   };

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
   public:
      GetFactorValue(vector<string>& container, string factorname)
         : Container (container), factorName(factorname)
         { }

      void operator () (const DBSimulation& simulation) const
         {
         string factorValue = simulation.getFactorValue(factorName);
         if (find(Container.begin(), Container.end(), factorValue)
             == Container.end())
            Container.push_back(factorValue);
         };
   };

// ------------------------------------------------------------------
//  Short description:
//    A function class to read in all data for each scenario.
//    passed in.

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
class ReadData
   {
   private:
      TAPSTable& data;
      TCursor savedCursor;
      vector<DBSimulation>& simulations;
   public:
      ReadData(vector<DBSimulation>& sims, TAPSTable& d)
         : simulations(sims),
           data(d)
         {
         }
      void operator() (const Scenario* scenario)
         {
         vector<DBSimulation>::iterator s = find(simulations.begin(),
                                                 simulations.end(),
                                                 *scenario);
         assert (s != simulations.end());
         (*s).readData(data, scenario->getName());
         }
   };

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 24/9/98

// ------------------------------------------------------------------
DatabaseAddIn::DatabaseAddIn(const string& parameters)
   {
   if (parameters == "")
      askUserForMDBs();
   else
      {
      vector<string> filenames;
      Split_string(parameters, " ", filenames);
      readAllDatabases(filenames);
      }
   readAllImages();
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
   for (ImageMap::iterator i = images.begin();
                           i != images.end();
                           i++)
      {
      Graphics::TBitmap* bitmap = (*i).second;
      delete bitmap;
      }
   images.erase(images.begin(), images.end());
   }

// ------------------------------------------------------------------
//  Short description:
//    get a list of MDB's from user.

//  Notes:

//  Changes:
//    DPH 24/9/98

// ------------------------------------------------------------------
void DatabaseAddIn::askUserForMDBs(void)
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
   simulations.erase(simulations.begin(), simulations.end());

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
   indexTable->Open();

   // loop through all records in the index table.  For each, create a
   // simulation object, get it to read itself in and then store in list.
   DBSimulation newSimulation;
   while (!indexTable->Eof)
      {
      newSimulation.readFromIndex(databaseFileName, indexTable);
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
      return convertSimulationToScenario(simulations[0], "");

   else
      {
      vector<Factor> dummy;
      return Scenario("", dummy);
      }
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
void DatabaseAddIn::makeScenarioValid(Scenario& scenario) const
   {
   // Go find the closest matching simulation to the one passed in.
   FindClosestScenario findClosest(scenario);
   findClosest = for_each(simulations.begin(), simulations.end(), findClosest);

   // return the closest matching scenario.
   const DBSimulation* closestSimulation = findClosest.getClosestSimulation();
   scenario = convertSimulationToScenario(*closestSimulation, scenario.getName());
   }

// ------------------------------------------------------------------
//  Short description:
//      return a list of possible legal values for the specified identifier

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void DatabaseAddIn::getFactorValues(const string& factorName,
                                    vector<string>& factorValues) const
   {
   for_each(simulations.begin(), simulations.end(),
            GetFactorValue(factorValues, factorName));
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
void DatabaseAddIn::doCalculations(TAPSTable& data,
                                   const vector<Scenario*>& selectedScenarios)
   {
   data.beginStoringData();
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   ReadData readData(simulations, data);
   for_each(selectedScenarios.begin(), selectedScenarios.end(), readData);

   data.addSortField(data.getYearFieldName());

   data.endStoringData();
   Screen->Cursor = savedCursor;
   }

// ------------------------------------------------------------------
//  Short description:
//    convert the specified simulation into a scenario.

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
Scenario DatabaseAddIn::convertSimulationToScenario
   (const DBSimulation& simulation, const std::string& scenarioName) const
   {
   vector<Factor> factors;
   simulation.getFactors(factors);

   for (vector<Factor>::iterator f = factors.begin();
                                 f != factors.end();
                                 f++)
      {
      (*f).setAddIn(this);
      (*f).setImage(getImageForFactor( (*f).getName() ));
      }
   return Scenario(scenarioName, factors);
   }

// ------------------------------------------------------------------
//  Short description:
//    read all images from .ini file.

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
void DatabaseAddIn::readAllImages(void)
   {
   static const char* BITMAPS_SECTION = "bitmaps";

   // locate and open our ini file.
   Path iniPath (Application->ExeName.c_str());
   iniPath.Set_extension (".ini");
   Ini_file ini;
   ini.Set_file_name (iniPath.Get_path().c_str());

   // read in all bitmaps.
   string bitmapSectionContents;
   ini.Read_section_contents (BITMAPS_SECTION, bitmapSectionContents);

   // loop through all lines in section.
   istringstream in (bitmapSectionContents.c_str());
   string line;
   getline(in, line);
   while (!in.eof())
      {
      string factorName, bitmapName;
      Get_keyname_and_value (line.c_str(), factorName, bitmapName);
      Path bitmapPath(Application->ExeName.c_str());
      bitmapPath.Set_name (bitmapName.c_str());
      To_lower(factorName);

      if (bitmapPath.Exists())
         {
         Graphics::TBitmap* bitmap = new Graphics::TBitmap;
         bitmap->LoadFromFile(bitmapPath.Get_path().c_str());
         images.insert(ImageMap::value_type(factorName, bitmap));
         }
      getline(in, line);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return an image for a specified factor.

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
const Graphics::TBitmap* DatabaseAddIn::getImageForFactor(const string& factorName) const
   {
   string lowerFactorName = factorName;
   To_lower(lowerFactorName);
   ImageMap::const_iterator i = images.find(lowerFactorName);
   if (i != images.end())
      return (*i).second;
   else
      return NULL;
   }

