//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TProbability.h"
#include <general\db_functions.h>
#include <general\math_functions.h>

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
      Active = false;
      Active = true;
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
      Active = false;
      Active = true;
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
void TProbability::createFields(void) throw(runtime_error)
   {
   if (source != NULL && fieldNameToAnalyse != "")
      {
      addDBField(this, fieldNameToAnalyse.c_str(), "1.0");
      addDBField(this, PROBABILITY_FIELD_NAME, "1.0");
      }
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
      source->firstSeries();
      while (!source->Eof)
         {
         vector<double> values;
         vector<double> probValues;
         string seriesName = source->getSeriesName();

         getDBFieldValues(source, fieldNameToAnalyse.c_str(), values);
         Calculate_prob_dist(values, isExceedence, probValues);

         // Now loop through all values and append a record for each.
         for (unsigned recordNum = 0; recordNum < probValues.size(); recordNum++)
            {
            Append();
            FieldValues[fieldNameToAnalyse] = values[recordNum];
            FieldValues[PROBABILITY_FIELD_NAME] = probValues[recordNum];
            setSeriesName(seriesName);   // this does a post as well.
            }
         source->nextSeries();
         }
      source->cancelSeries();
      SortFields = PROBABILITY_FIELD_NAME;
      }
   }

