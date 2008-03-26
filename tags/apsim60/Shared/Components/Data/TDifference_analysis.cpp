//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDifference_analysis.h"
#include "TDifference_form.h"
#include <general\string_functions.h>
#include <general\math_functions.h>
#include <general\vcl_functions.h>
#include <ApsimShared\ApsimSettings.h>

using namespace std;

#pragma link "TAnalysis"
#pragma link "TAPSTable"

#pragma package(smart_init)

//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TDifference_analysis *)
{
   new TDifference_analysis(NULL);
}
//---------------------------------------------------------------------------
namespace Tdifference_analysis
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TDifference_analysis)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 21/7/98

// ------------------------------------------------------------------
__fastcall TDifference_analysis::TDifference_analysis(TComponent* Owner)
   : TAnalysis(Owner)
   {
   FLHS_pair = new TStringList;
   FRHS_pair = new TStringList;
   }

// ------------------------------------------------------------------
//  Short description:
//      destructor

//  Notes:

//  Changes:
//    DPH 21/7/98

// ------------------------------------------------------------------
__fastcall TDifference_analysis::~TDifference_analysis()
   {
   delete FLHS_pair;
   delete FRHS_pair;
   }
// ------------------------------------------------------------------
// Load settings.
// ------------------------------------------------------------------
void TDifference_analysis::load()
   {
   TAnalysis::load();
   ApsimSettings settings;

   string st;
   settings.read(CHART_SETTINGS_KEY + "|BasePivotValue", st);
   FLHS_pair->Add(st.c_str());
   settings.read(CHART_SETTINGS_KEY + "|SecondPivotValue", st);
   FRHS_pair->Add(st.c_str());
   }

// ------------------------------------------------------------------
// Save settings.
// ------------------------------------------------------------------
void TDifference_analysis::save()
   {
   TAnalysis::save();
   ApsimSettings settings;

   settings.write(CHART_SETTINGS_KEY + "|BasePivotValue", string(FLHS_pair->Strings[0].c_str()));
   settings.write(CHART_SETTINGS_KEY + "|SecondPivotValue", string(FRHS_pair->Strings[0].c_str()));
   }
// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TDifference_analysis::calcAndStoreRecords()
   {
   if (Field_names_to_analyse->Items->Count == 1)
      {
      beginStoringData();

      // get the field name we're interested in.
      const string fieldName = Field_names_to_analyse->Items->Strings[0].c_str();
      const string yearFieldName = sourceDataset->getYearFieldName();

      vector<double> lhs1Years, lhs1Values, rhs1Years, rhs1Values;
      vector<double> lhs2Years, lhs2Values, rhs2Years, rhs2Values;

      // get all data.
      bool ok = sourceDataset->first();
      while (ok)
         {
         // get the data block name for this data block.
         string dataBlockName = sourceDataset->getDataBlockName();

         if (FLHS_pair->Count >= 1 &&
             dataBlockName == FLHS_pair->Strings[0].c_str())
            {
            sourceDataset->fieldAsNumericArray(yearFieldName, lhs1Years);
            sourceDataset->fieldAsNumericArray(fieldName, lhs1Values);
            }
         else if (FRHS_pair->Count >= 1 &&
             dataBlockName == FRHS_pair->Strings[0].c_str())
            {
            sourceDataset->fieldAsNumericArray(yearFieldName, rhs1Years);
            sourceDataset->fieldAsNumericArray(fieldName, rhs1Values);
            }
         else if (FRHS_pair->Count >= 2 &&
             dataBlockName == FRHS_pair->Strings[0].c_str())
            {
            sourceDataset->fieldAsNumericArray(yearFieldName, lhs2Years);
            sourceDataset->fieldAsNumericArray(fieldName, lhs2Values);
            }
         else if (FRHS_pair->Count >= 2 &&
             dataBlockName == FRHS_pair->Strings[0].c_str())
            {
            sourceDataset->fieldAsNumericArray(yearFieldName, rhs2Years);
            sourceDataset->fieldAsNumericArray(fieldName, rhs2Values);
            }

         ok = sourceDataset->next();
         }

      // tell the destination apstable (us) to get ready for first data block.
      first();

      // ok - SUBTRACT RHS1 from LHS1
      if (lhs1Years.size() > 0 && lhs1Values.size() > 0 &&
          rhs1Years.size() > 0 && rhs1Values.size() > 0)
         {
         vector<double> years, values;
         subtractData(lhs1Values, lhs1Years, rhs1Values, rhs1Years, years, values);
         storeNumericArray("Year", years);
         storeNumericArray(fieldName, values);
         AnsiString dataBlockName = FLHS_pair->Strings[0] + " - " + FRHS_pair->Strings[0];
         setDataBlockName(dataBlockName.c_str());
         markFieldAsAPivot("Simulation");

         next();
         }
      // ok - SUBTRACT RHS2 from LHS2
      if (lhs2Years.size() > 0 && lhs2Values.size() > 0 &&
          rhs2Years.size() > 0 && rhs2Values.size() > 0)
         {
         vector<double> years, values;
         subtractData(lhs2Values, lhs2Years, rhs2Values, rhs2Years, years, values);
         storeNumericArray("Year", years);
         storeNumericArray(fieldName, values);
         AnsiString dataBlockName = FLHS_pair->Strings[1] + " - " + FRHS_pair->Strings[1];
         setDataBlockName(dataBlockName.c_str());
         markFieldAsAPivot("Simulation");
         }

      endStoringData();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      subtract 2 columns of numbers making sure that the years
//      match up.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TDifference_analysis::subtractData(vector<double>& LHS_values,
                                        vector<double>& LHS_years,
                                        vector<double>& RHS_values,
                                        vector<double>& RHS_years,
                                        vector<double>& Years,
                                        vector<double>& Values)
   {
   if (LHS_years.size() > 0 && LHS_values.size() > 0 &&
       RHS_years.size() > 0 && RHS_values.size() > 0)
      {
      int Minimum_year = max(LHS_years[0], RHS_years[0]);
      int Maximum_year = min(LHS_years[LHS_values.size()-1], RHS_years[RHS_years.size()-1]);

      // loop through all years.
      for (int year = Minimum_year; year <= Maximum_year; year++)
         {
         // try and find the year on the lhs
         vector<double>::iterator i = std::find(LHS_years.begin(), LHS_years.end(), year);
         if (i != LHS_years.end())
            {
           // found year - get the lhs value
            double lhs_value = LHS_values[i - LHS_years.begin()];

            // try and find the year on the rhs
            i = std::find(RHS_years.begin(), RHS_years.end(), year);
            if (i != RHS_years.end())
               {
               // found year - get the rhs value
               double rhs_value = RHS_values[i - RHS_years.begin()];

               // store the year and value in return arrays.
               Years.push_back (year);
               Values.push_back (lhs_value - rhs_value);
               }
            }
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
TAPSTable_form* TDifference_analysis::createPropertiesForm()
   {
   return new TDifference_form(Application);
   }

