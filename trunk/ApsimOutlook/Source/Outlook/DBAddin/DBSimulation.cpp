//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "DBSimulation.h"
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <sstream>
#include <assert.h>
#pragma package(smart_init)
using namespace std;

#define SIMULATION_FACTOR_NAME "Simulation"
#define SIMULATION_FIELD_NAME  "Simulation"
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 13/1/98

// ------------------------------------------------------------------
DBSimulation::DBSimulation (void)
   {
   dataset = NULL;
   db = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 13/1/98

// ------------------------------------------------------------------
DBSimulation::~DBSimulation (void)
   {
   close();
   }

// ------------------------------------------------------------------
//  Short description:
//    read in all necessary info for this simulation from the
//    current record on the specified open index table.

//  Notes:

//  Changes:
//    DPH 13/1/98

// ------------------------------------------------------------------
void DBSimulation::readFromIndex (const string& dbFilename,
                                  TDataSet* indexTable)
   {
   factorNames.erase(factorNames.begin(), factorNames.end());
   factorValues.erase(factorValues.begin(), factorValues.end());

   databaseFilename = dbFilename;
   simulationId = indexTable->FieldValues["SimulationID"];
   string title = AnsiString(indexTable->FieldValues["Name"]).c_str();
   vector<string> factorAndValues;
   Split_string(title, ";", factorAndValues);
   string factor, value;
   for (unsigned int i = 0; i < factorAndValues.size(); i++)
      {
      Get_keyname_and_value(factorAndValues[i].c_str(), factor, value);
      if (factor == "")
         {
         factor = SIMULATION_FACTOR_NAME;
         value = factorAndValues[i];
         }

      factorNames.push_back(factor);
      factorValues.push_back(value);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      return the name of the sow year field name.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
string DBSimulation::getOrderFieldName(vector<string>& fieldNames)
   {
   string yearFieldName;

   for (vector<string>::iterator i = fieldNames.begin();
                                 i != fieldNames.end();
                                 i++)
      {
      string fieldName (*i);
      To_lower(fieldName);
      if (fieldName.find("year") != string::npos)
         {
         if (fieldName.find("sow") != string::npos)
            return *i;
         else
            yearFieldName = *i;
         }
      }

   return yearFieldName;
   }

// ------------------------------------------------------------------
//  Short description:
//    open the simulation dataset

//  Notes:

//  Changes:
//    DPH 13/1/98
//    dph  8/12/99 changed to use the simulation name in the descriptor field
//                 instead of a special descriptor field c227,d308

// ------------------------------------------------------------------
void DBSimulation::open (void)
   {
   db = new TADOConnection(Application);
   string connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;"
                             "Data Source=####;"
                             "Persist Security Info=False";
   Replace_all(connectionString, "####", databaseFilename.c_str());
   db->ConnectionString = connectionString.c_str();
   db->LoginPrompt = false;
   db->Mode = cmShareExclusive;
   db->Connected = true;

   // get a list of field names for final query.
   vector<string> fieldNames;
   getFieldNames (db, fieldNames);

   // create a query object for this simulation.
   dataset = new TADOQuery (db);
   dataset->Connection = db;

   // create an SQL query statement to extract the necessary data.
   ostringstream SQL;
   SQL << "SELECT [";
   for (vector<string>::iterator field = fieldNames.begin();
                                 field != fieldNames.end();
                                 field++)
      {
      if (field != fieldNames.begin())
         SQL << ",[";
      SQL << *field << ']';
      }
   SQL << " FROM Data WHERE SimulationID = " << simulationId;

   string orderByField = getOrderFieldName(fieldNames);
   if (orderByField.length() > 0)
      SQL << " ORDER BY [" << orderByField << ']';
   SQL << ends;
   dataset->Close();
   dataset->SQL->Text = SQL.str().c_str();
   dataset->Open();
   }

// ------------------------------------------------------------------
//  Short description:
//    close the simulation dataset

//  Notes:

//  Changes:
//    DPH 13/1/98

// ------------------------------------------------------------------
void DBSimulation::close (void)
   {
   delete dataset;
   dataset = NULL;

   if (db != NULL)
      {
      delete db;
      db = NULL;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Get a list of field names that the simulation dataset should contain.

//  Notes:

//  Changes:
//    DPH 13/1/98

// ------------------------------------------------------------------
void DBSimulation::getFieldNames (TADOConnection* db, vector<string>& fieldNames)
   {
   TADOTable* dataTable = new TADOTable(NULL);
   dataTable->Connection = db;
   dataTable->TableName = "Data";
   dataTable->ReadOnly = true;
   dataTable->CursorLocation = clUseServer;
   dataTable->Open();

   TStringList* TFieldNames = new TStringList;
   Get_field_list (dataTable, TFieldNames);

   // remove the simulation_id field from the list.  Assume it is first field.
   TFieldNames->Delete(0);
   TStrings_2_stl (TFieldNames, fieldNames);
   delete TFieldNames;
   dataTable->Close();
   delete dataTable;
   }

// ------------------------------------------------------------------
//  Short description:
//     read in all data records from our dataset and store in
//     specified TAPSTable.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void DBSimulation::readData(TAPSTable& data, const string& simulationName)
   {
   open();

   // get a list of field names from our open dataset.
   TStringList* TFieldNames = new TStringList;
   Get_field_list (dataset, TFieldNames);
   vector<string> fieldNames, dummy;
   TStrings_2_stl (TFieldNames, fieldNames);
   delete TFieldNames;

   // give all field names to the data object.
   data.addField(SIMULATION_FIELD_NAME);
   for (unsigned int f = 0; f < fieldNames.size(); f++)
      data.addField(fieldNames[f]);
   for (unsigned int f = 0; f < factorNames.size(); f++)
      {
//      data.addField(factorNames[f]);
      data.markFieldAsAPivot(factorNames[f]);
      }


   // loop through all records and store in our return list of records.
   dataset->First();
   while (!dataset->Eof)
      {
      TAPSRecord record;

      // Loop through all also store the simulation name as a field.
      record.setFieldValue(SIMULATION_FIELD_NAME, simulationName.c_str());
      record.read(dataset, fieldNames);

      // store all factors as a separate column.
      for (unsigned int f = 0; f < factorNames.size(); f++)
         record.setFieldValue(factorNames[f], factorValues[f]);

      data.storeRecord(record);
      dataset->Next();
      }

   close();
   }
// ------------------------------------------------------------------
//  Short description:
//    return a container of factors that represents this simulation.

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
void DBSimulation::getFactors(std::vector<Factor>& factors) const
   {
   for (unsigned int i = 0; i < factorNames.size(); i++)
      factors.push_back(Factor(NULL, factorNames[i], factorValues[i], NULL));
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if this simulation equals the specified scenario.

//  Notes:

//  Changes:
//    DPH 5/4/01
//    DAH 2/5/01: changed to look for a subsequence rather than the equality of
//                the factor sequences

// ------------------------------------------------------------------
bool DBSimulation::operator!= (const Scenario& rhs) const
   {
   vector<string> rhsFactorNames;
   rhs.getFactorNames(rhsFactorNames);
   vector<string>::iterator place =
          search(rhsFactorNames.begin(),rhsFactorNames.end(),
                 factorNames.begin(),factorNames.end());
   if (place != rhsFactorNames.end())
   // factorNames is contained within rhsFactorNames
   {
      vector<string> rhsFactorValues;
      for (vector<string>::iterator n = place;
                                    n != place + factorNames.size();
                                    n++)
         {
         string value;
         Graphics::TBitmap* bitmap;
         rhs.getFactorAttributes(*n, value, bitmap);
         rhsFactorValues.push_back(value);
         }
      return (rhsFactorValues != factorValues);
      }
   else
      return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    calculate a rank based on how close the factors and values
//    passed in are to our factors and values.

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
unsigned int DBSimulation::calculateRank
   (const vector<string>& rhsFactorNames, const vector<string>& rhsFactorValues) const
   {
   unsigned int count = 0;
   for (unsigned int f = 0; f < factorNames.size(); f++)
      {
      if (factorNames[f] == rhsFactorNames[f] &&
          factorValues[f] == rhsFactorValues[f])
         count++;
      else
         break;
      }
   return count;
   }

// ------------------------------------------------------------------
//  Short description:
//    return a value for a specified factor.

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
string DBSimulation::getFactorValue(const string& factorName) const
   {
   if (factorName == SIMULATION_FACTOR_NAME)
      return factorValues[0];
   else
      {
      vector<string>::const_iterator f = find(factorNames.begin(),
                                              factorNames.end(),
                                              factorName);
      if (f == factorNames.end())
         return "";
      else
         return factorValues[f-factorNames.begin()];
      }
   }

