//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDiff.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <numeric>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"      

static const char* PROBABILITY_FIELD_NAME = "Probability";
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TDiff::TDiff(TComponent* owner)
   : TSEGTable(owner)
   {
   mySecondDataSet = NULL;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TDiff::~TDiff()
   {
   }
//---------------------------------------------------------------------------
// Set the fieldName property.
//---------------------------------------------------------------------------
void __fastcall TDiff::setFieldName(AnsiString fieldName)
   {
   if (fieldNameToAnalyse != fieldName)
      {
      fieldNameToAnalyse = fieldName;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// set the 'seconddataset' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TDiff::setSecondDataSet(TSEGTable* second)
   {
   if (mySecondDataSet != second)
      {
      mySecondDataSet = second;
      mySecondDataSet->addDataChangeSubscription(Name + ".onSourceDataChanged");
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TDiff::createFields(void) throw(runtime_error)
   {
   if (source != NULL && fieldNameToAnalyse != "" && mySecondDataSet != NULL)
      {
      AnsiString NewFieldName = "Diff_" + fieldNameToAnalyse;

      FieldDefs->Clear();
      addDBField(this, NewFieldName.c_str(), "1.0");
      string yearFieldName = source->getYearFieldName();
      if (yearFieldName != "")
         addDBField(this, yearFieldName, "1.0");

      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TDiff::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL && fieldNameToAnalyse != "" && mySecondDataSet != NULL)
      {
      AnsiString yearFieldName = source->getYearFieldName().c_str();
      AnsiString NewFieldName = "Diff_" + fieldNameToAnalyse;
      // Loop through all series blocks and all records within that series.
      bool ok1 = source->firstSeries();
      bool ok2 = mySecondDataSet->firstSeries();
      while (ok1 && ok2)
         {
         addGroupByValuesFromSource();

         vector<double> year1, year2, values1, values2;
         getDBFieldValues(source, yearFieldName.c_str(), year1);
         getDBFieldValues(mySecondDataSet, yearFieldName.c_str(), year2);
         getDBFieldValues(source, fieldNameToAnalyse.c_str(), values1);
         getDBFieldValues(mySecondDataSet, fieldNameToAnalyse.c_str(), values2);
         if (year1.size() == year2.size() && values1.size() == values2.size())
            {
            vector<double> newValues = subtract(values1, values2);

            // Now loop through all values and append a record for each.
            for (unsigned recordNum = 0; recordNum < newValues.size(); recordNum++)
               {
               if (year1[recordNum] != year2[recordNum])
                  throw runtime_error("Years dont match in TDiff");

               Append();
               FieldValues[yearFieldName] = year1[recordNum];
               FieldValues[NewFieldName] = newValues[recordNum];
               Post();
               }
            }
         ok1 = source->nextSeries();
         ok2 = source->nextSeries();
         }
      source->cancelSeries();
      }
   }

