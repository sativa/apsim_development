//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "DBSimulation.h"
#include "apsimnc.h"
#include "utilities.h"
#include <general\vcl_functions.h>
#include <general\db_functions.h>
#include <general\string_functions.h>
#include <general\stristr.h>
#include <general\inifile.h>
#include <sstream>
#include <assert.h>
#pragma package(smart_init)
using namespace std;

#define SIMULATION_FACTOR_NAME "Simulation"
#define SIMULATION_FIELD_NAME  "Simulation"

#define WHOPECON_FACTOR_NAME "Econ Config"
#define NO_ECONOMICS_NAME "(no economics)"
#define DEFAULT_ECONOMICS_NAME "Example Configuration"


// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 13/1/98

// ------------------------------------------------------------------
DBSimulation::DBSimulation (void)
   {
   Factors = new TStringList;
   Levels = new TStringList;
   //dataset = NULL;
   //db = NULL;
   title = NULL;
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
   delete Factors;
   delete Levels;

  // delete [] title;
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
   /*
   strcpy(databaseFilename, dbFilename.c_str());
   simulationId = indexTable->FieldValues["SimulationID"];

   string titleSt = AnsiString(indexTable->FieldValues["Name"]).c_str();
   Replace_all(titleSt, "; ", ";");
   Replace_all(titleSt, " ;", ";");
   Replace_all(titleSt, "= ", "=");
   Replace_all(titleSt, " =", "=");
   if (titleSt[titleSt.length()-1] == ';')
      titleSt.erase(titleSt.length()-1);

   title = new char[titleSt.length() + 1];
   strcpy(title, titleSt.c_str());
   */
   }
void DBSimulation::AssignValues(TStringList *_Factors, TStringList *_Levels, ApsimNC *_NC)
   {
   Factors->Assign(_Factors);
   Levels->Assign(_Levels);
   ApsNC = _NC;
   String _title;
   int i;
   for(i = 0; i < (Factors->Count - 1); i++)
      {
      _title += Factors->Strings[i] + "=" + Levels->Strings[i] + ";";
      }
   //Add the last one
   _title += Factors->Strings[i] + "=" + Levels->Strings[i];

   title = new char[_title.Length() + 1];
   strcpy(title, _title.c_str());

   //Add economics
   //Factors->Add(WHOPECON_FACTOR_NAME);
   //Levels->Add("");

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
  /* string yearFieldName;

   for (vector<string>::iterator i = fieldNames.begin();
                                 i != fieldNames.end();
                                 i++)
      {
      string fieldName (*i);
      To_lower(fieldName);
      if (Str_i_Eq(fieldName, "date"))
         return *i;
      if (fieldName.find("year") != string::npos)
         {
         if (fieldName.find("sow") != string::npos)
            return *i;
         else
            yearFieldName = *i;
         }
      }

   return yearFieldName; */
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
   { /*
   db = new TADOConnection(Application);
   string connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;"
                             "Data Source=####;"
                             "Persist Security Info=False";
   Replace_all(connectionString, "####", databaseFilename);
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
   dataset->CursorLocation = clUseServer;

   // create an SQL query statement to extract the necessary data.
   ostringstream SQL;
   if (fieldNames.size() == 0)
      SQL << "SELECT *";
   else
      {
      SQL << "SELECT [";
      for (vector<string>::iterator field = fieldNames.begin();
                                    field != fieldNames.end();
                                    field++)
         {
         if (field != fieldNames.begin())
            SQL << ",[";
         SQL << *field << ']';
         }
      }
   SQL << " FROM Data WHERE SimulationID = " << simulationId;

   string orderByField = getOrderFieldName(fieldNames);
   if (orderByField.length() > 0)
      SQL << " ORDER BY [" << orderByField << ']';
   SQL << ends;
   dataset->Close();
   dataset->SQL->Text = SQL.str().c_str();
   dataset->Open();
   */
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
  /* delete dataset;
   dataset = NULL;

   if (db != NULL)
      {
      delete db;
      db = NULL;
      } */
   }

// ------------------------------------------------------------------
//  Short description:
//    Get a list of field names that the simulation dataset should contain.

//  Notes:

//  Changes:
//    DPH 13/1/98
//    DAH 11/4/02: Replaced Get_field_list call with new method from db_functions.h

// ------------------------------------------------------------------
void DBSimulation::getFieldNames (vector<string>& fieldNames)
   {
   //Get Traits from the NC file
   TStringList *Traits = new TStringList;

   ApsNC->GetTraits(Traits);

   for(int i = 0; i < Traits->Count; i++)
      {
      fieldNames.push_back(Traits->Strings[i].c_str());
      }

   delete Traits;
   /*
   TADOTable* dataTable = new TADOTable(NULL);
   dataTable->Connection = db;
   dataTable->TableName = "Data";
   dataTable->ReadOnly = true;
   dataTable->CursorLocation = clUseServer;
   dataTable->Open();

   getDBFieldNames(dataTable, fieldNames);

   // remove the simulation_id field from the list.  Assume it is first field.
   fieldNames.erase(fieldNames.begin());
   dataTable->Close();
   delete dataTable;  */
   }

// ------------------------------------------------------------------
//  Short description:
//     read in all data records from our dataset and store in
//     specified TAPSTable.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 13/4/02: replaced the call Get_field_list to the new equivalent from
//                 db_functions.h

// ------------------------------------------------------------------
void DBSimulation::readData(TAPSTable& data, const string& simulationName)
   {
  // vector<string> factorNames, factorValues;
  // getFactors(factorNames, factorValues);

   //open();

   // get a list of field names from our open dataset.
   vector<string> fieldNames, dummy;
   getFieldNames(fieldNames);

   // give all field names to the data object.
   data.addField(SIMULATION_FIELD_NAME);
   for (unsigned int f = 0; f < fieldNames.size(); f++)
      data.addField(fieldNames[f]);
   for (unsigned int f = 0; f < Factors->Count; f++)
      {
//      data.addField(factorNames[f]);
      data.markFieldAsAPivot(Factors->Strings[f].c_str());
      }


   //Create the data matrix
   float **Data = new float*[fieldNames.size()];
   for(int i = 0; i < fieldNames.size(); i++)
      {
      Data[i] = new float[ApsNC->NumYears()];
      }

   data.addField("Year");


   // Get all data from the NCfile
   for(int i = 0; i < fieldNames.size(); i++)
      {
      //Get the data
      //TStringList *NoEconLevs = new TStringList;
      //NoEconLevs->Assign(Levels);
      //NoEconLevs->Delete(Levels->Count - 1);

      //ApsNC->GetSiteData(NoEconLevs, fieldNames[i].c_str(), Data[i]);
      ApsNC->GetSiteData(Levels, fieldNames[i].c_str(), Data[i]);

      //delete NoEconLevs;
      }

   //Load all info into the TAPSTable
   for(int i = 0; i < ApsNC->NumYears(); i++)
      {
      TAPSRecord record;

      // Loop through all also store the simulation name as a field.
      record.setFieldValue(SIMULATION_FIELD_NAME, simulationName.c_str());
      record.setFieldValue("Year", String(ApsNC->GetStartYear() + i).c_str());
      // store all traits as a separate column.
      for (int f = 0; f < fieldNames.size(); f++)
         {
         if(Data[f][i] < -998.5)
            {
            Data[f][i] = 0.00;
            }
         record.setFieldValue(fieldNames[f].c_str(), String(Data[f][i]).c_str());
         }
      for(int j = 0; j < Factors->Count; j++)
         {
         record.setFieldValue(Factors->Strings[j].c_str(), Levels->Strings[j].c_str());
         }
      data.storeRecord(record);
      }


   //free memory
   for(int i = 0; i < fieldNames.size(); i++)
      {
      delete []Data[i];
      }
   delete []Data;

   }


// ------------------------------------------------------------------
//  Short description:
//    return a container of factors that represents this simulation.

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
void DBSimulation::getFactorNames(std::vector<string>& factorNames) const
   {
   for(int i = 0; i < Factors->Count; i++)
      {
      factorNames.push_back(Factors->Strings[i].c_str());
      }
   /*
   vector<string> factorValues;
   getFactors(factorNames, factorValues);
   */
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
   (const string& titleToMatch) const
   {
   for(int i = 0; i < Levels->Count; i++)
      {
      if(Levels->Strings[i].SubString(1,1) == "~")
         {
         return 1;
         }
      }

   const char* pos1 = title;
   const char* pos2 = titleToMatch.c_str();
   while (*pos1 != 0 && *pos2 != 0 && *pos1 == *pos2)
      {
      pos1++;
      pos2++;
      }
   if(((pos1 - title) > 25) && String(title).Pos("Wheat"))
      {
      int i =pos1 - title;
      }


   if (*pos1 == 0 && *pos2 == 0)
      {
      return 100000;
      }
   else
      {
      return pos1 - title;
      }
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
   int FactorIndex = FindStringInTSL(Factors,factorName.c_str());
   if(FactorIndex == -1)
      {
      return "";
      }
   else
      {
      return Levels->Strings[FactorIndex].c_str();
      }
   /*if (factorName == "Simulation")
      return title;
   else
      {
      string searchString = factorName + "=";
      char* posFactor = stristr(title, searchString.c_str());
      if (posFactor == NULL)
         return "";
      posFactor += searchString.length();
      char* posSemiColon = strchr(posFactor, ';');
      if (posSemiColon == NULL)
         return posFactor;
      else
         return string(posFactor, posSemiColon - posFactor);
      } */
   }
// ------------------------------------------------------------------
//  Short description:
//    return all factor names and values to caller.

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
void DBSimulation::getFactors(vector<string>& names, vector<string>& values) const
   {
   for(int i = 0; i < Levels->Count; i++)
      {
      values.push_back(Levels->Strings[i].c_str());
      }

  /* names.erase(names.begin(), names.end());
   values.erase(values.begin(), values.end());

   vector<string> titleBits;
   string titleString(title);
   Split_string (titleString, ";", titleBits);

   for (unsigned int i = 0; i < titleBits.size(); i++)
      {
      string name, value;
      getKeyNameAndValue(titleBits[i], name, value);
      if (name == "")
         {
         name = "Simulation";
         value = titleBits[i];
         }
      names.push_back (name);
      values.push_back (value);
      } */
   }
// ------------------------------------------------------------------
//  Short description:
//    returns true if all factor names are legitimate.

//  Notes:

//  Changes:
//    AJD 5/4/01

// ------------------------------------------------------------------
bool DBSimulation::IsLegit()
   {
   for(int i = 0; i < Levels->Count; i++)
      {
      if(Levels->Strings[i].SubString(1,1) == "~")
         {
         return false;
         }
      }
   return true;
   }


