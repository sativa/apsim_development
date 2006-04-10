//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSummary_analysis.h"
#include "TSummary_form.h"
#include <general\math_functions.h>
#include <general\vcl_functions.h>
#include <ApsimShared\ApsimSettings.h>
#include <strstream>
#include <iomanip>

#pragma link "TAnalysis"
#pragma link "TAPSTable"
#pragma package(smart_init)
using namespace std;
static const char* STAT_FIELD_NAME = "Statistic";
static const char* VALUE_FIELD_NAME = "Value";
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TSummary_analysis *)
{
   new TSummary_analysis(NULL);
}
//---------------------------------------------------------------------------
namespace Tsummary_analysis
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TSummary_analysis)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:
//

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
__fastcall TSummary_analysis::TSummary_analysis(TComponent* Owner)
   : TAnalysis(Owner)
   {
   FPercentiles = new TStringList;
   }
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:
//

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
__fastcall TSummary_analysis::~TSummary_analysis(void)
   {
   delete FPercentiles;
   }
// ------------------------------------------------------------------
// Load settings.
// ------------------------------------------------------------------
void TSummary_analysis::load()
   {
   TAnalysis::load();
   ApsimSettings settings;

   try
      {
      vector<string> percentiles;
      settings.read(CHART_SETTINGS_KEY + "|Percentiles", percentiles);
      Stl_2_tstrings(percentiles, FPercentiles);

      int seriesType;
      settings.read(CHART_SETTINGS_KEY + "|SeriesType", seriesType);
      FSeriesType = (SeriesTypes) seriesType;
      settings.read(CHART_SETTINGS_KEY + "|Mean", FMean);
      settings.read(CHART_SETTINGS_KEY + "|Minimum", FMinimum);
      settings.read(CHART_SETTINGS_KEY + "|Maximum", FMaximum);
      settings.read(CHART_SETTINGS_KEY + "|Count", FCount);
      settings.read(CHART_SETTINGS_KEY + "|TableOnly", FTableOnly);
      }
   catch (const exception& err)
      {
      FSeriesType = box;
      FMean = true;
      }
   }

// ------------------------------------------------------------------
// Save settings.
// ------------------------------------------------------------------
void TSummary_analysis::save()
   {
   TAnalysis::save();
   ApsimSettings settings;

   vector<string> percentiles;
   TStrings_2_stl(FPercentiles, percentiles);
   settings.write(CHART_SETTINGS_KEY + "|Percentiles", percentiles);

   settings.write(CHART_SETTINGS_KEY + "|SeriesType", (int) FSeriesType);
   settings.write(CHART_SETTINGS_KEY + "|Mean", FMean);
   settings.write(CHART_SETTINGS_KEY + "|Minimum", FMinimum);
   settings.write(CHART_SETTINGS_KEY + "|Maximum", FMaximum);
   settings.write(CHART_SETTINGS_KEY + "|Count", FCount);
   settings.write(CHART_SETTINGS_KEY + "|TableOnly", FTableOnly);
   }
// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
string TSummary_analysis::Double_2_str(double value, int precision)
   {
   ostrstream out;
   out.setf(ios::fixed, ios::floatfield);
   out << setprecision(precision) << value << ends;
   string st(out.str());
   delete out.str();
   return st;
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void TSummary_analysis::calcAndStoreRecords()
   {
   if (Field_names_to_analyse->Items->Count > 0)
      {
      beginStoringData();

      // create a list of field names to keep.
      string fieldName = Field_names_to_analyse->Items->Strings[0].c_str();

      // tell the destination apstable (us) to get ready for first data block.
      first();

      // loop through all pivot series.
      bool ok = sourceDataset->first();
      while (ok)
         {
         // get an array of numbers from our array of records.
         vector<double> values;
         sourceDataset->fieldAsNumericArray(fieldName, values);
         string blockName = sourceDataset->getDataBlockName();

         // loop through all percentiles and store each as a separate record
         for (int i = 0; i < FPercentiles->Count; i++)
            {
            TAPSRecord newRecord;

            int percentile = StrToInt(FPercentiles->Strings[i]);
            double value = Calculate_percentile(values, false, percentile);

            string percentileString = IntToStr(percentile).c_str();
                   percentileString += "%";
            newRecord.setFieldValue(fieldName, FloatToStr(value).c_str());
            newRecord.setFieldValue("Statistic", percentileString);
            newRecord.setFieldValue("Simulation", blockName);
            storeRecord(newRecord);
            }
         if (FMean)
            {
            // calculate and store mean
            TAPSRecord meanRecord;
            double Mean = Calculate_mean(values);
            meanRecord.setFieldValue(fieldName, FloatToStr(Mean).c_str());
            meanRecord.setFieldValue("Statistic", "Mean");
            meanRecord.setFieldValue("Simulation", blockName);
            storeRecord(meanRecord);
            }
         if (FMinimum)
            {
            // calculate and store minimum
            TAPSRecord minimumRecord;
            double Minimum = values[0];
            minimumRecord.setFieldValue (fieldName, FloatToStr(Minimum).c_str());
            minimumRecord.setFieldValue("Statistic", "Minimum");
            minimumRecord.setFieldValue("Simulation", blockName);
            storeRecord(minimumRecord);
            }
         if (FMaximum)
            {
            // calculate and store maximum
            TAPSRecord maximumRecord;
            double Maximum = values[values.size()-1];
            maximumRecord.setFieldValue (fieldName, FloatToStr(Maximum).c_str());
            maximumRecord.setFieldValue("Statistic", "Maximum");
            maximumRecord.setFieldValue("Simulation", blockName);
            storeRecord(maximumRecord);
            }
         if (FCount)
            {
            // calculate and store count
            TAPSRecord countRecord;
            double Count = values.size();
            countRecord.setFieldValue (fieldName, Double_2_str(Count, 0).c_str());
            countRecord.setFieldValue("Statistic", "Count");
            countRecord.setFieldValue("Simulation", blockName);
            storeRecord(countRecord);
            }

         // add all the necessary field names to our destination table.
         addField("Statistic");
         addField("Simulation");
         if (SeriesType < box && !TableOnly)
            {
            clearPivots();
            markFieldAsAPivot("Statistic");
            sortRecords("Statistic");
            }
         else
            copyPivotsFrom(*sourceDataset);
         addField(fieldName);


         // goto next pivot series in source dataset.
         ok = sourceDataset->next();

         // tell the destination apstable to get ready for next data.
         next();
         }

      endStoringData();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      create and return a pointer to an analysis form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSTable_form* TSummary_analysis::createPropertiesForm()
   {
   return new TSummary_form(Application);
   }

