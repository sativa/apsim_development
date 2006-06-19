//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TProbability.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"      

static const char* PROBABILITY_FIELD_NAME = "Probability";
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TProbability::TProbability(TComponent* owner)
   : TSEGTable(owner)
   {
   isExceedence = false;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TProbability::~TProbability()
   {
   }
//---------------------------------------------------------------------------
// set the 'exceedence' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TProbability::setExceedence(bool exceedence)
   {
   if (isExceedence != exceedence)
      {
      isExceedence = exceedence;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the fieldName property.
//---------------------------------------------------------------------------
void __fastcall TProbability::setFieldName(AnsiString fieldName)
   {
   if (fieldNameToAnalyse != fieldName)
      {
      fieldNameToAnalyse = fieldName;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TProbability::createFields(void) throw(runtime_error)
   {
   if (source != NULL && fieldNameToAnalyse != "")
      {
      FieldDefs->Clear();
      addDBField(this, fieldNameToAnalyse.c_str(), "1.0");
      addDBField(this, PROBABILITY_FIELD_NAME, "1.0");
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TProbability::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL && fieldNameToAnalyse != "")
      {
      // Loop through all series blocks and all records within that series.
      bool ok = source->firstSeries();
      while (ok)
         {
         vector<double> values;
         vector<double> probValues;
         getDBFieldValues(source, fieldNameToAnalyse.c_str(), values);
         Calculate_prob_dist(values, isExceedence, probValues);

         // Now loop through all values and append a record for each.
         for (unsigned recordNum = 0; recordNum < probValues.size(); recordNum++)
            {
            Append();
            FieldValues[fieldNameToAnalyse] = values[recordNum];
            FieldValues[PROBABILITY_FIELD_NAME] = probValues[recordNum];
            Post();
            }
         ok = source->nextSeries();
         }
      source->cancelSeries();
      sortFields = PROBABILITY_FIELD_NAME;
      }
   }
// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void TProbability::setProperty(const std::string& propertyName,
                               const std::string& propertyValue)
   {
   if (Str_i_Eq(propertyName, "fieldName"))
      fieldName = propertyValue.c_str();
   else if (Str_i_Eq(propertyName, "exceedence"))
      exceedence = Str_i_Eq(propertyValue, "true");
   }

