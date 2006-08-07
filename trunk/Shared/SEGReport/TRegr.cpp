//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRegr.h"
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
__fastcall TRegr::TRegr(TComponent* owner)
   : TSEGTable(owner)
   {
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TRegr::~TRegr()
   {
   }
//---------------------------------------------------------------------------
// Set the xFieldName property.
//---------------------------------------------------------------------------
void __fastcall TRegr::setXFieldName(AnsiString fieldName)
   {
   if (XFieldName != fieldName)
      {
      XFieldName = fieldName;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the xFieldName property.
//---------------------------------------------------------------------------
void __fastcall TRegr::setYFieldName(AnsiString fieldName)
   {
   if (YFieldName != fieldName)
      {
      YFieldName = fieldName;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TRegr::createFields(void) throw(runtime_error)
   {
   if (source != NULL && XFieldName != "" && YFieldName != "")
      {
      FieldDefs->Clear();
      addDBField(this, "RegrX", "1.0");
      addDBField(this, "RegrY", "1.0");
      addDBField(this, "1:1X", "1.0");
      addDBField(this, "1:1Y", "1.0");
      addDBField(this, "Equation", "x");
      addDBField(this, "m", "1.0");
      addDBField(this, "c", "1.0");
      addDBField(this, "r2", "1.0");
      addDBField(this, "n", "1.0");
      addDBField(this, "StdErr(m)", "1.0");
      addDBField(this, "StdErr(c)", "1.0");
      addDBField(this, "RMSD", "1.0");
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TRegr::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL && XFieldName != "" && YFieldName != "")
      {
      // Loop through all series blocks and all records within that series.
      bool ok = source->firstSeries();
      while (ok)
         {
         vector<double> x, y;
         source->First();
         while (!source->Eof)
            {
            try
               {
               double xValue = source->FieldValues[XFieldName];
               double yValue = source->FieldValues[YFieldName];
               x.push_back(xValue);
               y.push_back(yValue);
               }
            catch (...)
               {
               }
            source->Next();
            }

         Regr_stats stats;
         calcRegressionStats(x, y, stats);

         double minX = *min_element(x.begin(), x.end());
         double maxX = *max_element(x.begin(), x.end());
         //double minY = *min_element(y.begin(), y.end());
         //double maxY = *max_element(y.begin(), y.end());

         string equation;
         equation = " y = " + ftoa(stats.m, 2) + " x + " + ftoa(stats.c, 2);
         equation += " (r2 = " + ftoa(stats.R2, 2) + ")";

         Append();
         FieldValues["RegrX"] = minX;
         FieldValues["RegrY"] = stats.m * minX + stats.c;
         FieldValues["1:1X"] = minX;
         FieldValues["1:1Y"] = minX;
         FieldValues["Equation"] = equation.c_str();
         FieldValues["m"] = stats.m;
         FieldValues["c"] = stats.c;
         FieldValues["r2"] = stats.R2;
         FieldValues["n"] = x.size();
         FieldValues["StdErr(m)"] = stats.SEslope;
         FieldValues["StdErr(c)"] = stats.SEcoeff;
         FieldValues["RMSD"] = stats.RMSD;
         Post();
         Append();
         FieldValues["RegrX"] = maxX;
         FieldValues["RegrY"] = stats.m * maxX + stats.c;
         FieldValues["1:1X"] = maxX;
         FieldValues["1:1Y"] = maxX;
         FieldValues["Equation"] = equation.c_str();
         FieldValues["m"] = stats.m;
         FieldValues["c"] = stats.c;
         FieldValues["r2"] = stats.R2;
         FieldValues["n"] = x.size();
         FieldValues["StdErr(m)"] = stats.SEslope;
         FieldValues["StdErr(c)"] = stats.SEcoeff;
         FieldValues["RMSD"] = stats.RMSD;
         Post();

         ok = source->nextSeries();
         }
      source->cancelSeries();
      }
   }

