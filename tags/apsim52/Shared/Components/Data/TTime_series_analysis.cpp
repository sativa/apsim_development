//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTime_series_analysis.h"
#include <general\math_functions.h>
#include <general\string_functions.h>
#include "TTime_series_form.h"
#include <ApsimShared\ApsimSettings.h>
#include <strstream>

#pragma link "TAPSTable"

#pragma package(smart_init)

//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TTime_series_analysis *)
{
   new TTime_series_analysis(NULL);
}
//---------------------------------------------------------------------------
namespace Ttime_series_analysis
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TTime_series_analysis)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
__fastcall TTime_series_analysis::TTime_series_analysis(TComponent* Owner)
   : TAnalysis(Owner)
   {
   }

// ------------------------------------------------------------------
// Load settings.
// ------------------------------------------------------------------
void TTime_series_analysis::load()
   {
   TAnalysis::load();
   ApsimSettings settings;
   string st;

   FOrigin = 0.0;

   try
      {
      settings.read(CHART_SETTINGS_KEY + "|WithMeanLine", FWith_mean_line);
      settings.read(CHART_SETTINGS_KEY + "|WithPercentLine", FWith_percentile_line);
      settings.read(CHART_SETTINGS_KEY + "|DiffFromMean", FDiff_from_mean);
      settings.read(CHART_SETTINGS_KEY + "|DiffFromPercentile", FDiff_from_percentile);
      settings.read(CHART_SETTINGS_KEY + "|ProbExceed", FProb_exceed);
      settings.read(CHART_SETTINGS_KEY + "|Percentile", FPercentile);
      settings.read(CHART_SETTINGS_KEY + "|DifferentAboveBelowColours", FDifferent_above_below_colours);
      settings.read(CHART_SETTINGS_KEY + "|BasePivotValue", st);
      FBase_pivot_value = st.c_str();
      }
   catch (const exception& err)
      {
      FWith_mean_line = false;
      FWith_percentile_line = false;
      FDiff_from_mean = false;
      FDiff_from_percentile = false;
      FProb_exceed = false;
      FDifferent_above_below_colours = false;
      FPercentile = 50;
      }
   }

// ------------------------------------------------------------------
// Save settings.
// ------------------------------------------------------------------
void TTime_series_analysis::save()
   {
   TAnalysis::save();
   ApsimSettings settings;
   settings.write(CHART_SETTINGS_KEY + "|WithMeanLine", FWith_mean_line);
   settings.write(CHART_SETTINGS_KEY + "|WithPercentLine", FWith_percentile_line);
   settings.write(CHART_SETTINGS_KEY + "|DiffFromMean", FDiff_from_mean);
   settings.write(CHART_SETTINGS_KEY + "|DiffFromPercentile", FDiff_from_percentile);
   settings.write(CHART_SETTINGS_KEY + "|ProbExceed", FProb_exceed);
   settings.write(CHART_SETTINGS_KEY + "|Percentile", FPercentile);
   settings.write(CHART_SETTINGS_KEY + "|DifferentAboveBelowColours", FDifferent_above_below_colours);
   settings.write(CHART_SETTINGS_KEY + "|BasePivotValue", string(FBase_pivot_value.c_str()));
   }

// ------------------------------------------------------------------
//  Short description:
//      set the difference from mean value.

//  Notes:

//  Changes:
//    DPH 14/7/98

// ------------------------------------------------------------------
void __fastcall TTime_series_analysis::Set_diff_from_mean (bool Diff_from_mean)
   {
   FDiff_from_mean = Diff_from_mean;
   if (Diff_from_mean)
      {
      FWith_mean_line = false;
      FWith_percentile_line = false;
      FDiff_from_percentile = false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      set the difference from percentile

//  Notes:

//  Changes:
//    DPH 14/7/98

// ------------------------------------------------------------------
void __fastcall TTime_series_analysis::Set_diff_from_percentile (bool Diff_from_percentile)
   {
   FDiff_from_percentile = Diff_from_percentile;
   if (Diff_from_percentile)
      {
      FWith_mean_line = false;
      FWith_percentile_line = false;
      FDiff_from_mean = false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      set the with mean line property

//  Notes:

//  Changes:
//    DPH 14/7/98

// ------------------------------------------------------------------
void __fastcall TTime_series_analysis::Set_with_mean_line (bool With_mean_line)
   {
   FWith_mean_line = With_mean_line;
   if (With_mean_line)
      {
      FWith_percentile_line = false;
      FDiff_from_mean = false;
      FDiff_from_percentile = false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      set the difference from percentile

//  Notes:

//  Changes:
//    DPH 14/7/98

// ------------------------------------------------------------------
void __fastcall TTime_series_analysis::Set_with_percentile_line (bool With_percentile_line)
   {
   FWith_percentile_line = With_percentile_line;
   if (With_percentile_line)
      {
      FWith_mean_line = false;
      FDiff_from_mean = false;
      FDiff_from_percentile = false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 14/7/98
//    DAH 31/10/00   d380 used new Get_year_field_name() method from APSTable

// ------------------------------------------------------------------
void TTime_series_analysis::calcAndStoreRecords()
   {
   beginStoringData();
   for (int field = 0; field < Field_names_to_analyse->Items->Count; field++)
      {

      // create a list of field names to keep.
      string fieldName = Field_names_to_analyse->Items->Strings[field].c_str();
      string yearFieldName = sourceDataset->getYearFieldName();

      // get the base dataset from which to calculate the origin.
      calcOriginValue(fieldName);

      // tell the destination apstable (us) to get ready for first data block.
      first();

      // get all data.
      bool ok = sourceDataset->first();
      while (ok)
         {
         // get the year values and our field values.
         vector<string> yearValues, fieldValues;
         sourceDataset->fieldAsStringArray(yearFieldName, yearValues);
         sourceDataset->fieldAsStringArray(fieldName, fieldValues);

         // subtract the origin value we calculated earlier from all our
         // field values.
         if (FDiff_from_mean || FDiff_from_percentile)
            subtractOrigin(fieldValues, FOrigin);

         // if the user wants different colours above and below the mean line
         // then we need to create 2 data blocks.
         if (Different_above_below_colours)
            {
            // Split the arrays of numbers into 2 separate arrays for
            // above and below the FOrigin.
            vector<double> aboveYears, aboveValues, belowYears, belowValues;
            for (unsigned int i = 0; i < fieldValues.size(); i++)
               {
               double fieldValue = atof(fieldValues[i].c_str());
               double yearValue = atof(yearValues[i].c_str());
               if (fieldValue > FOrigin)
                  {
                  aboveYears.push_back(yearValue);
                  aboveValues.push_back(fieldValue);
                  }
               else
                  {
                  belowYears.push_back(yearValue);
                  belowValues.push_back(fieldValue);
                  }
               }
            // Write out above and below values into separate data blocks.
            storeValues(aboveYears, aboveValues, fieldName);
            createNewPivotField("(above)");
            next();
            storeValues(belowYears, belowValues, fieldName);
            createNewPivotField("(below)");
            }
         else
            storeValues(yearValues, fieldValues, fieldName);

         // goto next pivot series in source dataset.
         ok = sourceDataset->next();

         // tell the destination apstable to get ready for next data.
         next();
         }
      }
   endStoringData();
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate a base line value.

//  Notes:

//  Changes:
//    DPH 14/7/98

// ------------------------------------------------------------------
void TTime_series_analysis::calcOriginValue(const string& fieldName)
   {
   if (FDiff_from_mean || FDiff_from_percentile || FWith_mean_line || FWith_percentile_line)
      {
      // go find the data block for our simulation.
      bool ok = sourceDataset->first();
      while (ok &&
             (*sourceDataset->begin()).getFieldValue("Simulation") != FBase_pivot_value.c_str())
         ok = sourceDataset->next();

      vector<double> values;
      sourceDataset->fieldAsNumericArray(fieldName, values);

      if (FDiff_from_mean || FWith_mean_line)
         FOrigin = Calculate_mean(values);
      else
         FOrigin = Calculate_percentile(values, FProb_exceed, FPercentile);
      }
   else
      FOrigin = 0.0;
   }

// ------------------------------------------------------------------
//  Short description:
//    Store the statistic fields.

//  Notes:

//  Changes:
//    DPH 14/7/98
//    DAH 6/2/02:    D-496 temporary fix. This module and associated panel expect
//                   a field called "Statistic" which interfered with the field
//                   "StatisticName" in the macro substitution

// ------------------------------------------------------------------
void TTime_series_analysis::storeStatisticFields(void)
   {
   // store statistic values column
   vector<double> statisticValues(end() - begin(), FOrigin);
   storeNumericArray("Statistic", statisticValues);

   string statisticName;
   if (FDiff_from_mean || FWith_mean_line)
      statisticName = "Mean of ";
   else
      {
      statisticName = IntToStr(FPercentile).c_str();
      statisticName += "% of ";
      }
   statisticName += "\'";
   statisticName += FBase_pivot_value.c_str();
   statisticName += "\' = ";
   statisticName += ftoa(FOrigin, 3);
   vector<string> statisticNameValues(end() - begin(), statisticName);
   //storeStringArray("StatisticName", statisticNameValues);
   storeStringArray("StatName", statisticNameValues);
   }

// ------------------------------------------------------------------
//  Short description:
//      store the specified arrays into the destination data table.

//  Notes:

//  Changes:
//    DPH 12/4/01

// ------------------------------------------------------------------
void TTime_series_analysis::storeValues(vector<double>& yearValues,
                                        vector<double>& values,
                                        const string& fieldName)
   {
   storeNumericArray("Year", yearValues);
   storeNumericArray(fieldName, values);
   storeStatisticFields();
   copyPivotsFrom(*sourceDataset);
   }

// ------------------------------------------------------------------
//  Short description:
//      store the specified arrays into the destination data table.

//  Notes:

//  Changes:
//    DPH 12/4/01

// ------------------------------------------------------------------
void TTime_series_analysis::storeValues(vector<string>& yearValues,
                                        vector<string>& values,
                                        const string& fieldName)
   {
   storeStringArray("Year", yearValues);
   storeStringArray(fieldName, values);
   storeStatisticFields();
   copyPivotsFrom(*sourceDataset);
   }

// ------------------------------------------------------------------
//  Short description:
//      store the specified arrays into the destination data table.

//  Notes:

//  Changes:
//    DPH 12/4/01

// ------------------------------------------------------------------
void TTime_series_analysis::createNewPivotField(const string& fieldValue)
   {
   vector<string> fieldValues(end() - begin(), fieldValue);
   storeStringArray("above_below", fieldValues);
   setDataBlockName(getDataBlockName() + fieldValue);
   markFieldAsAPivot("above_below");
   }
// ------------------------------------------------------------------
// Subtract the specified origin from the field values passed in.
// ------------------------------------------------------------------
void TTime_series_analysis::subtractOrigin(vector<string>& fieldValues,
                                           double FOrigin)
   {
   for (unsigned i = 0; i != fieldValues.size(); i++)
      {
      if (fieldValues[i] != "")
         {
         double value = atof(fieldValues[i].c_str());
         value = value - FOrigin;
         fieldValues[i] = ftoa(value, 6);
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      create and return a pointer to an analysis form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSTable_form* TTime_series_analysis::createPropertiesForm()
   {
   TTime_series_form* form = new TTime_series_form(Application);
   //form->allowMultipleVariables();
   return form;
   }


