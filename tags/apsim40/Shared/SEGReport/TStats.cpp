//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TStats.h"
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
__fastcall TStats::TStats(TComponent* owner)
   : TSEGTable(owner)
   {
   statsToCalc << statMean;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TStats::~TStats()
   {
   }
//---------------------------------------------------------------------------
// Set the fieldName property.
//---------------------------------------------------------------------------
void __fastcall TStats::setFieldName(AnsiString fieldName)
   {
   if (fieldNameToAnalyse != fieldName)
      {
      fieldNameToAnalyse = fieldName;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// set the 'exceedence' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TStats::setStats(StatSet stats)
   {
   if (statsToCalc != stats)
      {
      statsToCalc = stats;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TStats::createFields(void) throw(runtime_error)
   {
   if (source != NULL && fieldNameToAnalyse != "")
      {
      FieldDefs->Clear();
      addGroupByFieldDefsFromSource();

      if (stats.Contains(statMean))
         addDBField(this, "Mean", "1.0");
      if (stats.Contains(statCount))
         addDBField(this, "Count", "1");
      if (stats.Contains(statMin))
         addDBField(this, "Minimum", "1.0");
      if (stats.Contains(statMax))
         addDBField(this, "Maximum", "1.0");
      if (stats.Contains(statSum))
         addDBField(this, "Sum", "1.0");
      if (stats.Contains(stat10))
         addDBField(this, "Decile10", "1.0");
      if (stats.Contains(stat20))
         addDBField(this, "Decile20", "1.0");
      if (stats.Contains(stat30))
         addDBField(this, "Decile30", "1.0");
      if (stats.Contains(stat40))
         addDBField(this, "Decile40", "1.0");
      if (stats.Contains(stat50))
         addDBField(this, "Decile50", "1.0");
      if (stats.Contains(stat60))
         addDBField(this, "Decile60", "1.0");
      if (stats.Contains(stat70))
         addDBField(this, "Decile70", "1.0");
      if (stats.Contains(stat80))
         addDBField(this, "Decile80", "1.0");
      if (stats.Contains(stat90))
         addDBField(this, "Decile90", "1.0");
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TStats::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL && fieldNameToAnalyse != "")
      {
      // Loop through all series blocks and all records within that series.
      bool ok = source->firstSeries();
      while (ok)
         {
         Append();
         addGroupByValuesFromSource();

         vector<double> values;
         while (!source->Eof)
            {
            try
               {
               values.push_back( StrToFloat(source->FieldValues[fieldNameToAnalyse.c_str()]) );
               }
            catch (const Exception& err)
               { }
            source->Next();
            }


         if (stats.Contains(statMean))
            FieldValues["Mean"] = Calculate_mean(values);
         if (stats.Contains(statCount))
            FieldValues["Count"] = values.size();
         if (stats.Contains(statMin))
            FieldValues["Minimum"] = min_element(values.begin(), values.end(),
                                                 less<double>());
         if (stats.Contains(statMax))
            FieldValues["Maximum"] = max_element(values.begin(), values.end(),
                                                 less<double>());
         if (stats.Contains(statSum))
            FieldValues["Sum"] = accumulate(values.begin(), values.end(), 0.0);
         if (stats.Contains(stat10))
            FieldValues["Decile10"] = Calculate_percentile(values, false, 10);
         if (stats.Contains(stat20))
            FieldValues["Decile20"] = Calculate_percentile(values, false, 20);
         if (stats.Contains(stat30))
            FieldValues["Decile30"] = Calculate_percentile(values, false, 30);
         if (stats.Contains(stat40))
            FieldValues["Decile40"] = Calculate_percentile(values, false, 40);
         if (stats.Contains(stat50))
            FieldValues["Decile50"] = Calculate_percentile(values, false, 50);
         if (stats.Contains(stat60))
            FieldValues["Decile60"] = Calculate_percentile(values, false, 60);
         if (stats.Contains(stat70))
            FieldValues["Decile70"] = Calculate_percentile(values, false, 70);
         if (stats.Contains(stat80))
            FieldValues["Decile80"] = Calculate_percentile(values, false, 80);
         if (stats.Contains(stat90))
            FieldValues["Decile90"] = Calculate_percentile(values, false, 90);
         Post();
         ok = source->nextSeries();
         }
      source->cancelSeries();
      }
   }
// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void TStats::setProperty(const std::string& propertyName,
                         const std::string& propertyValue)
   {
   if (Str_i_Eq(propertyName, "fieldname"))
      fieldName = propertyValue.c_str();
   }

